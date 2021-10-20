{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Reanimate
import Reanimate.Builtin.Documentation

import Graphics.SvgTree.Types

import Control.Lens.Operators ((.~), (?~))
import Reanimate.Animation (staticFrame)

-- | Type representing orientations (horizontal or vertical)
data Orientation = H | V deriving (Eq, Show)

main :: IO ()
main = reanimate drawGraph

-- | Animation of axes and grid, and then the curves, and then a pause
drawGraph :: Animation
drawGraph = addStatic (mkBackground "white") $
  drawAxes `parA` pauseAtBeginning 1.0 (drawGrid 4)
  `andThen` drawCurves
  `andThen` pause 2.0

-- | Animation of the horizontal axis and the vertical axis, at the same time
drawAxes :: Animation
drawAxes = drawAxis H `parA` drawAxis V

-- | Animation of an axis, and then its label
drawAxis :: Orientation -> Animation
drawAxis o = mapA (gridAxisStyle . gridColor)
  (drawLine (-7.5, -4.0) e) `andThen`
  mapA (axisLabelStyle . axisLabelColor) (drawAxisLabel o)
 where
  e = case o of
    H -> (7.5, -4.0)
    V -> (-7.5, 4.0)

-- | Animation of a line between two points, using 'curveS'
drawLine :: (Double, Double) -> (Double, Double) -> Animation
drawLine s e = mkAnimation 2 animation
 where
  animation t = partialSvg (curveS 2 t) $ pathify $ mkLine s e

-- | Animation of an axis label
drawAxisLabel :: Orientation -> Animation
drawAxisLabel o = staticFrame 0.25 $ case o of
  H -> translate 7.0 (-4.25) $ smallText "space"
  V -> translate (-7.5) 4.1 $ smallText "time"
 where
  smallText = (scale 0.2) . mkText

-- | Animation of horizontal grid lines and vertical grid lines, at the same
-- time
drawGrid :: Int -> Animation
drawGrid n = parAs hGridLines `parA` parAs vGridLines
 where
  n' = fromIntegral n
  hIndices = [ 1 .. 8 * n]
  vIndices = [ 1 .. 15 * n]
  hGridLines = map (\i -> drawGridLine' H (fromIntegral i) (8.0 * n')) hIndices
  vGridLines = map (\i -> drawGridLine' V (fromIntegral i) (15.0 * n')) vIndices

-- | Animation of a grid line, with an initial pause
drawGridLine' :: Orientation -> Double -> Double -> Animation
drawGridLine' o i n = pause d `seqA` drawGridLine o p
 where
  p = i/n
  d = i/gridSpeed

-- | Value representing the speed at which the grid lines appear
gridSpeed :: Double
gridSpeed = 10

-- | Animation of a grid line
drawGridLine :: Orientation -> Double -> Animation
drawGridLine o p = mapA (gridLineStyle . gridColor) $ drawLine s e
 where
  x = case o of
    H -> -4.0 + p * 8.0
    V -> -7.5 + p * 15.0
  s = case o of
    H -> (-7.5, x)
    V -> (x, -4.0)
  e = case o of
    H -> (7.5, x)
    V -> (x, 4.0)

-- | Animation of the 'iso time line' and curves (at the same time)
drawCurves :: Animation
drawCurves = parAs [ drawIsoTime
                   , drawCurve "darkturquoise" curve1
                   , drawCurve "blueviolet" curve2 ]

-- | Animation of the 'iso time line'
drawIsoTime :: Animation
drawIsoTime = mkAnimation 5.0 isoTime

-- | Draw an 'iso time' line
isoTime :: Time -> Tree
isoTime t
  | t > 0.99 = None  -- No line at the end
  | otherwise = (gridAxisStyle . gridColor) $ mkLine (-7.5, y) (7.5, y)
 where
  y = -4.0 + t * 8.0

-- | Animation of a curve, given its colour and definition
drawCurve :: String -> (Time -> (Double, Double)) -> Animation
drawCurve col c = mkAnimation 5.0 animation
 where
  animation t =
    let ts = [0.0, 0.01 .. t]
        -- Note `withFillColor "none"` and `withStrokeOpacity` are not yet on
        -- Hackage
        style = withFillColor "none" . withStrokeOpacity 0.5 .
                withStrokeColor col . withStrokeWidth 0.1
        line = (withClipPathRef (Ref "cp") . style) $ mkLinePath (map c ts)
        marker = if t > 0.99
          then None  -- No marker at the end
          else uncurry translate (c t) $ withFillColor col (mkCircle 0.1)
    in  mkGroup [cp, line, marker]
   where
    cp = mkClipPath "cp" [mkRect 15.0 8.0]

-- | Helper function to animate a list of animations at the same time
parAs :: [Animation] -> Animation
parAs [] = pause 0.0  -- Empty animation, for completeness
parAs [a] = a
parAs (a:as) = a `parA` parAs as

-- Styles used to determine the colours etc of key elements

gridColor :: Tree -> Tree
gridColor = withStrokeColor "blue"

gridAxisStyle :: Tree -> Tree
gridAxisStyle = withStrokeOpacity 0.75 . withStrokeWidth 0.04


axisLabelColor :: Tree -> Tree
axisLabelColor = withFillColor "blue"

axisLabelStyle :: Tree -> Tree
axisLabelStyle =  withFillOpacity 0.75

gridLineStyle :: Tree -> Tree
gridLineStyle = withStrokeOpacity 0.25 . withStrokeWidth 0.02

-- | Define the first curve, in terms of canvas coordinates
curve1 :: Time -> (Double, Double)
curve1 t = (x, y)
 where
  y = -4.0 + t * 8.0
  t' = t - 0.6
  x = t' * t' * 7.5 * 4.0

-- | Define the second curve, in terms of canvas coordinates
curve2 :: Time -> (Double, Double)
curve2 t = (x, y)
 where
  y = -4.0 + t * 8.0
  t' = t - 0.5
  x = - t' * t' * 7.5 * 4.0
