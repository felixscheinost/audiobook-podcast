module PictureTools (
    pictureCollage
)where

import           Codec.Picture       (DynamicImage (..), Image, PixelRGB8 (..))
import qualified Codec.Picture       as Picture
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NEL
import qualified Data.Text           as T
import           Import.NoFoundation

data ImageDesc = ImageDesc
    { idGetPixel :: !(Int -> Int -> PixelRGB8)
    , idWidth    :: !Int
    , idHeight   :: !Int
    }

image :: Image PixelRGB8 -> ImageDesc
image img = ImageDesc
    { idGetPixel = Picture.pixelAt img
    , idWidth = Picture.imageWidth img
    , idHeight = Picture.imageHeight img
    }

upTo :: Int -> (Int -> a) -> (Int -> a) -> (Int -> a)
upTo maxX fBelow fAbove x
    | x < maxX = fBelow x
    | otherwise = fAbove (x - maxX)
{-# INLINE upTo #-}

beside :: ImageDesc -> ImageDesc -> ImageDesc
beside a b = ImageDesc
    { idGetPixel = \x y -> upTo (idWidth a) (flip (idGetPixel a) y) (flip (idGetPixel b) y) x
    , idWidth = idWidth a + idWidth b
    , idHeight = min (idHeight a) (idHeight b)
    }

below :: ImageDesc -> ImageDesc -> ImageDesc
below a b = ImageDesc
    { idGetPixel = \x y -> upTo (idHeight a) (idGetPixel a x) (idGetPixel b x) y
    , idWidth = min (idWidth a) (idWidth b)
    , idHeight = idHeight a + idHeight b
    }

mulp :: PixelRGB8 -> Float -> PixelRGB8
mulp pixel x = Picture.colorMap (floor . (* x) . fromIntegral) pixel
{-# INLINE mulp #-}

addp :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
addp = Picture.mixWith (const f)
  where
    f x y = 255 `min` (x + y)
{-# INLINE addp #-}

scaleBilinear :: Int -> Int -> ImageDesc -> ImageDesc
scaleBilinear width height img = ImageDesc
    { idGetPixel = scale
    , idWidth = width
    , idHeight = height
    }
    where
        sx = fromIntegral (idWidth img)  / fromIntegral width
        sy = fromIntegral (idHeight img) / fromIntegral height
        pixelAt x y =
            if x >= idWidth img || y >= idHeight img
                then idGetPixel img (idWidth img - 1) (idHeight img - 1)
                else idGetPixel img x y

        scale x y =
            mulp (pixelAt x0 y0) ((1 - dx) * (1 - dy)) `addp`
            mulp (pixelAt (x0 + 1) y0) (dx * (1 - dy)) `addp`
            mulp (pixelAt x0 (y0 + 1)) ((1 - dx) * dy) `addp`
            mulp (pixelAt (x0 + 1) (y0 + 1)) (dx * dy)
            where
                x' = sx * fromIntegral x
                y' = sy * fromIntegral y
                x0 = floor x'
                y0 = floor y'
                dx = x' - fromIntegral x0
                dy = y' - fromIntegral y0

-- | Render a ImageDesc to a Image PixelRGB8
idRender :: ImageDesc -> Image PixelRGB8
idRender img = Picture.generateImage (idGetPixel img) (idWidth img) (idHeight img)

-- | Loads a picture and tries to convert its pixels to RGB8
loadImage :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe (Image PixelRGB8))
loadImage filepath = do
    res <- liftIO $ Picture.readImage filepath
    case res of
        Left err      -> do
            logErrorN ("Error opening picture at " <> T.pack filepath <> ": " <> T.pack err)
            return Nothing
        Right picture ->
            return (Just $ Picture.convertRGB8 picture)

_pictureCollage :: Int -> Int -> NonEmpty (Image PixelRGB8) -> ImageDesc
_pictureCollage width height pics =
    case pics of
        a :| b:c:d:_ -> arrange (scale a) (scale b) (scale c) (scale d)
        a :| _       -> scaleBilinear width height (image a)
    where
        partWidth = width `quot` 2
        partHeight = width `quot` 2
        scale = scaleBilinear partWidth partHeight . image
        arrange a b c d = below (beside a b) (beside c d)

-- | Return a grid of 2x2 if 4 or more picture are available or a scaled version of the first picture if less than 4 are available
pictureCollage :: (MonadIO m, MonadLogger m) => Int -> Int -> NonEmpty FilePath -> m (Maybe DynamicImage)
pictureCollage w h paths= do
    images <- catMaybes . NEL.toList <$> mapM loadImage paths
    case images of
        x:xs -> return $ Just $ ImageRGB8 $ idRender $ _pictureCollage w h (x :| xs)
        []   -> return Nothing
