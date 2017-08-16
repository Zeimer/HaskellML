{-# LANGUAGE FlexibleContexts #-}
module GradientDescent where

import Numeric.LinearAlgebra

import Graphics.Rendering.Chart.Easy ((.=), points, line, plot, setColors, def, layout_title, opaque, blue, red)
import Graphics.Rendering.Chart.Backend.Cairo

norm :: Vector R -> R
norm v = sqrt (dot v v)

diff :: (R -> R) -> (R -> R)
diff f x = (f (x + 1e-10) - f x) / 1e-10

partialv :: (Vector R -> R) -> Int -> (Vector R -> R)
partialv f i = \v ->
	let	h = vector $ map (\k -> if k == i then 1e-10 else 0) [0..size v - 1]
	in 	(f (v + h) - f v) / norm h

diffv :: (Vector R -> R) -> (Vector R -> Vector R)
diffv f v = vector $ map (\i -> partialv f i v) [0..size v - 1]

descent :: R -> Int -> R -> (R -> R) -> R
descent alpha numOfIter start f
	| numOfIter <= 0 = start
	| otherwise = descent alpha (numOfIter - 1) (start - alpha * diff f start) f

autodescent :: R -> (R -> R) -> R
autodescent p = aux 1000 0 where

	aux alpha start f
		| f (start - alpha * diff f start) > f start = aux (alpha / 2) start f
		| abs (diff f start) < p = start
		| otherwise = aux (2 * alpha) (start - alpha * diff f start) f

descentGrad :: R -> Int -> R -> (R -> R) -> (R -> R) -> R
descentGrad alpha numOfIter start f f'
	| numOfIter <= 0 = start
	| otherwise = descentGrad alpha (numOfIter - 1) (start - alpha * f' start) f f'

descentv :: R -> Int -> Vector R -> (Vector R -> R) -> Vector R
descentv alpha numOfIter start f
	| numOfIter <= 0 = start
	| otherwise = descentv alpha (numOfIter - 1) (start - scale alpha (diffv f start)) f

autodescentv :: R -> Vector R -> (Vector R -> R) -> Vector R
autodescentv p start = aux 1000 start where

	aux alpha start f
		| f (start - scale alpha (diffv f start)) > f start = aux (alpha / 2) start f
		| norm (diffv f start) < p = start
		| otherwise = aux (2 * alpha) (start - scale alpha (diffv f start)) f

descentHist :: R -> Int -> R -> (R -> R) -> [R]
descentHist alpha numOfIter start f
	| numOfIter <= 0 = [start]
	| otherwise = start : descentHist alpha (numOfIter - 1) (start - alpha * diff f start) f

descentvHist :: R -> Int -> Vector R -> (Vector R -> R) -> [Vector R]
descentvHist alpha numOfIter start f
	| numOfIter <= 0 = [start]
	| otherwise = start : descentvHist alpha (numOfIter - 1) (start - scale alpha (diffv f start)) f

plotDescentHist :: String -> R -> Int -> R -> (R -> R) -> IO ()
plotDescentHist name α numOfIter start f = do
	let g = zip [0..numOfIter] $ map f (descentHist α numOfIter start f)

	toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque blue]
		plot (line "Gradient descent's error" [g])

plotDescentvHist :: String -> R -> Int -> Vector R -> (Vector R -> R) -> IO ()
plotDescentvHist name α numOfIter start f = do
	let g = zip [0..numOfIter] $ map f (descentvHist α numOfIter start f)

	toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque blue]
		plot (line "Gradient descent's error" [g])

data Config = Config {α :: R, iters :: Int, start :: Vector R}

cfg :: Config
cfg = Config {α = 0.1, iters = 50}

superDescent :: Config -> (Vector R -> R) -> Vector R
superDescent cfg f
	| iters cfg <= 0 = start cfg
	| otherwise = superDescent (cfg {iters = iters cfg - 1, start = start cfg - scale (α cfg) (diffv f (start cfg))}) f
