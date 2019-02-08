module PictureTools where

import           Codec.Picture       (Image, PixelRGBA8 (..), pixelAt, Pixel, PixelBaseComponent)
import qualified Codec.Picture       as Picture
import qualified Codec.Picture.Extra as Extra
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NonEmpty
import qualified Data.Text           as T
import           Import.NoFoundation
import qualified System.Directory    as Directory

data ImageDesc = ImageDesc
    { idGetPixel :: !(Int -> Int -> PixelRGBA8)
    , idWidth :: !Int
    , idHeight :: !Int
    }

image :: Image PixelRGBA8 -> ImageDesc
image img = ImageDesc 
    { idGetPixel = Picture.pixelAt img
    , idWidth = Picture.imageWidth img
    , idHeight = Picture.imageHeight img 
    }

upTo :: Int -> (Int -> a) -> (Int -> a) -> (Int -> a)
upTo maxX below above x
    | x < maxX = below x
    | otherwise = above (x - maxX)

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

mulp :: PixelRGBA8 -> Float -> PixelRGBA8
mulp pixel x = Picture.colorMap (floor . (* x) . fromIntegral) pixel
{-# INLINE mulp #-}

addp :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
addp = Picture.mixWith (const f)
  where
    f x y = fromIntegral (255 `min` (fromIntegral x + fromIntegral y))
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
                then PixelRGBA8 0 0 0 0
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

-- | Render a ImageDesc to a Image PixelRGBA8
idRender :: ImageDesc -> Image PixelRGBA8
idRender img = Picture.generateImage (idGetPixel img) (idWidth img) (idHeight img)

-- | Load a picture and tries to convert its pixels to RGBA8
loadImage :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe (Image PixelRGBA8))
loadImage filepath = do
    res <- liftIO $ Picture.readImage filepath
    case res of
        Left err      -> do
            logErrorN ("Error opening picture at " <> T.pack filepath <> ": " <> T.pack err)
            return Nothing
        Right picture ->
            return (Just $ Picture.convertRGBA8 picture)

_pictureCollage :: Int -> Int -> [Image PixelRGBA8] -> ImageDesc
_pictureCollage width height nonEmptyPics =
    case nonEmptyPics of
        []       -> empty width height
        [x]      -> scaleBilinear width height (image x)
        x:[y]    -> arrange (scale x) (scale y) emptyPart emptyPart
        x:y:[z]  -> arrange (scale x) (scale y) (scale z) emptyPart
        x:y:z:rs -> arrange (scale x) (scale y) (scale z) (_pictureCollage partWidth partHeight (take 4 rs))
    where
        partWidth = width `quot` 2
        partHeight = width `quot` 2
        empty w h = ImageDesc { idGetPixel = \_ _ -> PixelRGBA8 0 0 0 0, idWidth = w, idHeight = h }
        scale = scaleBilinear partWidth partHeight . image
        arrange a b c d = below (beside a b) (beside c d)
        emptyPart = empty partWidth partHeight

pictureCollage :: Int -> Int -> [Image PixelRGBA8] -> Image PixelRGBA8
pictureCollage w h imgs = idRender $ _pictureCollage w h imgs