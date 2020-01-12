{-# LANGUAGE RecordWildCards #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad
import           Control.Monad.ST
import           Debug.Trace
import           Lib

data RGBA =
  RGBA Int Int Int Int

instance Num RGBA where
  (RGBA a b c d) + (RGBA e f g h) = RGBA (a + e) (b + f) (c + g) (d + h)
  (RGBA a b c d) - (RGBA e f g h) = RGBA (a - e) (b - f) (c - g) (d - h)
  (RGBA a b c d) * (RGBA e f g h) = RGBA (a * e) (b * f) (c * g) (d * h)
  abs (RGBA a b c d) = RGBA (abs a) (abs b) (abs c) (abs d)
  signum (RGBA a b c d) = RGBA (signum a) (signum b) (signum c) (signum d)
  fromInteger x = RGBA (fromInteger x) (fromInteger x) (fromInteger x) (fromInteger x)
  negate (RGBA a b c d) = RGBA (negate a) (negate b) (negate c) (negate d)

divRGBA :: RGBA -> RGBA -> RGBA
divRGBA (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) = RGBA (r1 `div` r2) (g1 `div` g2) (b1 `div` b2) (a1 `div` a2)

class RGBAConversion a where
  toRGBA :: a -> RGBA
  fromRGBA :: RGBA -> a

instance RGBAConversion PixelRGB8 where
  toRGBA (PixelRGB8 r g b) = RGBA (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
  fromRGBA (RGBA r g b _) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)

instance RGBAConversion PixelRGBA8 where
  toRGBA (PixelRGBA8 r g b a) = RGBA (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
  fromRGBA (RGBA r g b a) = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

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
                sum = foldr ((+) . toRGBA) (RGBA 0 0 0 0) targetPixelList
                resultPixel = sum `divRGBA` fromIntegral (samplingFactor ^ 2)
            writePixel targetImg x y $ fromRGBA resultPixel
            go (x + 1) y
    go 0 0
