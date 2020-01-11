{-# LANGUAGE RecordWildCards #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad
import           Control.Monad.ST
import           Debug.Trace
import           Lib

instance Show DynamicImage where
  show (ImageY8 _)     = "ImageY8"
  show (ImageY16 _)    = "ImageY16"
  show (ImageY32 _)    = "ImageY32"
  show (ImageYF _)     = "ImageYF"
  show (ImageYA8 _)    = "ImageYA8"
  show (ImageYA16 _)   = "ImageYA16"
  show (ImageRGB8 _)   = "ImageRGB8"
  show (ImageRGB16 _)  = "ImageRGB16"
  show (ImageRGBF _)   = "ImageRGBF"
  show (ImageRGBA8 _)  = "ImageRGBA8"
  show (ImageRGBA16 _) = "ImageRGBA16"
  show (ImageYCbCr8 _) = "ImageYCbCr8"
  show (ImageCMYK8 _)  = "ImageCMYK8"
  show (ImageCMYK16 _) = "ImageCMYK16"

rgba8To16 :: PixelRGBA8 -> PixelRGBA16
rgba8To16 (PixelRGBA8 r g b a) = PixelRGBA16 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

rgba16To8 :: PixelRGBA16 -> PixelRGBA8
rgba16To8 (PixelRGBA16 r g b a) = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

addPixelRGBA16 :: PixelRGBA16 -> PixelRGBA16 -> PixelRGBA16
addPixelRGBA16 (PixelRGBA16 r1 g1 b1 a1) (PixelRGBA16 r2 g2 b2 a2) = PixelRGBA16 (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)

divPixelRGBA16 :: PixelRGBA16 -> Pixel16 -> PixelRGBA16
divPixelRGBA16 (PixelRGBA16 r g b a) factor =
  PixelRGBA16 (r `div` factor) (g `div` factor) (b `div` factor) (a `div` factor)

main :: IO ()
main = do
  image <- readImage "test2.png"
  case image of
    Left error -> print $ "Error! message: " ++ error
    Right (ImageRGBA8 image) -> (savePngImage "result.png" . ImageRGBA8 . resizeImage 3) image
    Right image -> print image

resizeImage :: Int -> Image PixelRGBA8 -> Image PixelRGBA8
resizeImage samplingFactor img@Image {..} =
  runST $ do
    let targetWidth = imageWidth `div` samplingFactor
        targetHeight = imageHeight `div` samplingFactor
    targetImg <- newMutableImage targetWidth targetHeight
    let go x y
          | x >= targetWidth = go 0 (y + 1)
          | y >= targetHeight = unsafeFreezeImage targetImg
          | otherwise = do
            let xRange = [x * samplingFactor .. x * samplingFactor + samplingFactor - 1]
                yRange = [y * samplingFactor .. y * samplingFactor + samplingFactor - 1]
                targetPixelList = [pixelAt img i j | i <- xRange, j <- yRange]
                sum = foldr (addPixelRGBA16 . rgba8To16) (PixelRGBA16 0 0 0 0) targetPixelList
                resultPixel = sum `divPixelRGBA16` fromIntegral (samplingFactor ^ 2)
            writePixel targetImg x y $ rgba16To8 resultPixel
            go (x + 1) y
    go 0 0
