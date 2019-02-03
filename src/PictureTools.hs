module PictureTools (
    loadImage,
    padImageToBeQuadratic,
    pictureCollage
) where

import           Codec.Picture       (Image, PixelRGBA8 (..))
import qualified Codec.Picture       as Picture
import qualified Codec.Picture.Extra as Extra
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NonEmpty
import qualified Data.Text           as T
import           Import.NoFoundation
import qualified System.Directory    as Directory

-- Load a picture and tries to convert its pixels to RGBA8
loadImage :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe (Image PixelRGBA8))
loadImage filepath = do
    res <- liftIO $ Picture.readImage filepath
    case res of
        Left err      -> do
            logErrorN ("Error opening picture at " <> T.pack filepath <> ": " <> T.pack err)
            return Nothing
        Right picture ->
            return (Just $ Picture.convertRGBA8 picture)

-- Pads a image with transparent pixels to be transparent
padImageToBeQuadratic :: Image PixelRGBA8 -> Image PixelRGBA8
padImageToBeQuadratic img
    | width < height =
        Extra.beside
            [ img
            , Picture.generateImage (\_ _ -> PixelRGBA8 0 0 0 0) (height - width) height
            ]
    | height < width =
        Extra.below
            [ img
            , Picture.generateImage (\_ _ -> PixelRGBA8 0 0 0 0) width (width - height)
            ]
    | otherwise = img
        where
            width = Picture.imageWidth img
            height = Picture.imageHeight img

pictureCollage :: Int -> Int -> [Image PixelRGBA8] -> Image PixelRGBA8
pictureCollage width height nonEmptyPics =
    case nonEmptyPics of
        []       -> arrange empty empty empty empty
        [x]      -> arrange x empty empty empty
        x:[y]    -> arrange x y empty empty
        x:y:[z]  -> arrange x y z empty
        x:y:z:rs -> arrange x y z (pictureCollage partWidth partHeight rs)
    where
        partWidth = width `quot` 2
        partHeight = width `quot` 2
        empty = Picture.generateImage (\_ _ -> PixelRGBA8 0 0 0 0) partWidth partHeight
        arrange a b c d = Extra.below [Extra.beside [a, b], Extra.beside [c, d]]
