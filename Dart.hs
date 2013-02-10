module Main where

import Data.List
import Data.Binary
import Data.Bits
import Control.Monad

type Pos   = (Double,Double)
data Coord = Pos { angle :: Double, radius :: Double } deriving (Show,Eq)

π :: Double
π = pi

gauss :: Double -> Double -> Double -> Double
gauss μ σ x = scale * exp (-0.5 * ( (x - μ) / σ ) ^ (2::Int)) 
    where scale = 1 / (σ * sqrt (2 * π)) 


-- (-1,1) .... (1, 1)
-- (-1,-1).....(1,-1)

len :: Pos -> Double
len (x,y) = sqrt (x*x + y*y)

distance :: Pos -> Pos -> Double
distance (x,y) (z,w) = len (z - x,w - y)

toPolar :: Pos -> Coord
toPolar pos@(x,y) = Pos { angle = arg, radius = r }  
    where r = len pos
          arg    | y > 0     =   acos $ x / r
                 | otherwise = -(acos $ x / r)

-- | rates position according to standard dart gameboard
rate :: Coord -> Int
rate pos | r < 0.04 = 50
         | r < 0.08 = 25
         | r > 1 = 0
         | otherwise = dupl (searchFromMin a)
    where (Pos { angle = a,radius = r} ) = pos
          dupl = duplicator r

rateBoard size stddev = map (sampleRaster stddev raster) raster
    where raster = [(x/size * 2 - 1,y/size * 2 - 1) | x <- [0..size-1], y <- [0..size-1]]

sampleRaster :: Double -> [Pos] -> Pos -> Double
sampleRaster stddev raster center = foldl' rateAccum 0 raster
  where polar = toPolar center
        rateAccum accum sample = let d = distance center sample
                                     weight = gauss 0 stddev d
                                     rating = rate (toPolar sample)
                                 in fromIntegral rating * weight + accum

duplicator r | r > 0.42 && r < 0.50 = (* 3)
             | r > 0.92 = (* 2)
             | r <= 1.0 = id
             | r > 1.0 = const 0 

searchFromMin angle | null candidates = 0-- error $ "could not rate " ++ show angle
                    | otherwise = last candidates
   where candidates = [pts | (minAngle,pts) <- gameboard, mkPositive angle >= minAngle] 

mkPositive x | x < -0.157   = pi + (-x)
             | otherwise = x

gameboard = zip [-0.157,0.157 .. 2*π] [6,13,4,18,1,20,5,12,9,14,11,8,16,7,19,3,17,2,15,10,6]


makeScoreImage :: Double -> Int -> TIFF
makeScoreImage stddev size = TIFF (fromIntegral size) (fromIntegral size) image
    where board = rateBoard (fromIntegral size) stddev
          image = normalize $ board

writeImage' :: String -> Double -> Int -> IO ()
writeImage' filename stddev size = encodeFile filename $ makeScoreImage stddev size

main = test0

test0 = writeImage' "/home/hs/dart/result2.tga" 0.15 128

greyscale :: [Double] -> [RGB]
greyscale = map (mkRgb . scale) 
  where scale x = floor $ x * 255.0
        mkRgb x = RGB (fromIntegral x) (fromIntegral x) (fromIntegral x)

normalize :: [Double] -> [RGB]
normalize xs = map rate xs
  where max = maximum xs
        rate val = if val > max * 0.8 
                     then RGB 255 0 0
                     else let lum = floor $ (val / max) * 255
                          in RGB lum lum lum

data RGB = RGB Word8 Word8 Word8
  deriving Show

type THeight = Word16
type TWidth = Word16

data TIFF = TIFF THeight TWidth [RGB] deriving Show

instance Binary RGB where
  put (RGB r g b) = do put r; put g; put b
  get = undefined

instance Binary TIFF where
  put (TIFF width height pixels) = do
    -- header
    put (0 :: Word8); put (0 :: Word8); put (2 :: Word8)
    mapM_ (\_ -> put (0 :: Word8)) [4..10]
    put (0xe0 :: Word8); put (0xe0 :: Word8) -- wtf?
    putCoor width; putCoor height
    put (0x18 :: Word8); put (0x20 :: Word8)
    -- payload
    mapM_ put pixels
  get = undefined

putCoor w16 = do
  put (fromIntegral (w16 .&. 0xff) :: Word8)
  put (fromIntegral ((shift w16 (-8)) .&. 0xff) :: Word8) 
