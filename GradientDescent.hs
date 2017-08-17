{-# LANGUAGE FlexibleContexts #-}
module GradientDescent where

import Numeric.LinearAlgebra

import Graphics.Rendering.Chart.Easy ((.=), points, line, plot, setColors, def, layout_title, opaque, blue, red)
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Number.CReal

-- Compute the normal equation
normalEq :: Matrix R -> Vector R -> Vector R
normalEq x y = (inv (tr x <> x) <> tr x) #> y

-- Vector norm.
norm :: Vector R -> R
norm v = sqrt (dot v v)

-- Differentiate a function of one variable.
diff :: (R -> R) -> (R -> R)
--diff f x = (f (x + 1e-10) - f x) / 1e-10
diff = diffeps 1e-4

diffeps :: R -> (R -> R) -> (R -> R)
diffeps ε f x = (f (x + ε) - f (x - ε)) / (2 * ε)

-- A partial derivative.
partialv :: (Vector R -> R) -> Int -> (Vector R -> R)
partialv = partialveps 1e-4

partialveps :: R -> (Vector R -> R) -> Int -> (Vector R -> R)
partialveps ε f i v =
	let
		h = vector $ map (\k -> if k == i then ε else 0) [0..size v - 1]
	in
		(f (v + h) - f (v - h)) / (2 * ε)

-- A total derivative.
diffv :: (Vector R -> R) -> (Vector R -> Vector R)
diffv = diffveps 1e-4

diffveps :: R -> (Vector R -> R) -> (Vector R -> Vector R)
diffveps ε f v = vector $ map (\i -> partialveps ε f i v) [0..size v - 1]

-- Various variants of gradient descent, including ones with history and code for plotting their error.
-- Analytically computed derivative speeds up the algorithm 2x.
descentGrad :: R -> Int -> R -> (R -> R) -> (R -> R) -> R
descentGrad alpha numOfIter start f f'
	| numOfIter <= 0 = start
	| otherwise = descentGrad alpha (numOfIter - 1) (start - alpha * f' start) f f'

{-descent :: R -> Int -> R -> (R -> R) -> R
descent alpha numOfIter start f
	| numOfIter <= 0 = start
	| otherwise = descent alpha (numOfIter - 1) (start - alpha * diff f start) f-}

descent a n x f = descentGrad a n x f (diff f) 

autodescent :: R -> (R -> R) -> R
autodescent p = aux 1e-100 0 where

	aux alpha start f
		| f (start - alpha * diff f start) > f start = aux (alpha / 2) start f
		| abs (diff f start) < p = start
		| otherwise = aux (2 * alpha) (start - alpha * diff f start) f


descentv :: R -> Int -> Vector R -> (Vector R -> R) -> Vector R
descentv alpha numOfIter start f
	| numOfIter <= 0 = start
	| otherwise = descentv alpha (numOfIter - 1) (start - scale alpha (diffv f start)) f

autodescentv :: R -> Vector R -> (Vector R -> R) -> Vector R
autodescentv p start = aux 1e-3 start where

	aux alpha start f
		| f (start - scale alpha (diffv f start)) > f start = aux (alpha / 2) start f
		| norm (diffv f start) < p = start
		| otherwise = aux (2 * alpha) (start - scale alpha (diffv f start)) f

descentHist :: R -> Int -> R -> (R -> R) -> [R]
descentHist alpha numOfIter start f
	| numOfIter <= 0 = [start]
	| otherwise = start : descentHist alpha (numOfIter - 1) (start - alpha * diff f start) f

autodescentHist :: R -> (R -> R) -> [R]
autodescentHist p = aux 1 0 where

	aux alpha start f
		| f (start - alpha * diff f start) > f start = aux (alpha / 2) start f
		| abs (diff f start) < p = [start]
		| otherwise = start : aux (2 * alpha) (start - alpha * diff f start) f

descentvHist :: R -> Int -> Vector R -> (Vector R -> R) -> [Vector R]
descentvHist alpha numOfIter start f
	| numOfIter <= 0 = [start]
	| otherwise = start : descentvHist alpha (numOfIter - 1) (start - scale alpha (diffv f start)) f

autodescentvHist :: R -> Vector R -> (Vector R -> R) -> [Vector R]
autodescentvHist p start = aux 1e-3 start where

	aux alpha start f
		| f (start - scale alpha (diffv f start)) > f start = aux (alpha / 2) start f
		| norm (diffv f start) < p = [start]
		| otherwise = start : aux (2 * alpha) (start - scale alpha (diffv f start)) f

plotDescentHist :: String -> R -> Int -> R -> (R -> R) -> IO ()
plotDescentHist name α numOfIter start f = do
	let g = zip ([0..] :: [Int]) $ map f (descentHist α numOfIter start f)

	toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque blue]
		plot (line "Gradient descent's error" [g])

plotAutodescentHist :: String -> R -> (R -> R) -> IO ()
plotAutodescentHist name p f = do
	let g = zip ([0..] :: [Int]) $ map f (autodescentHist p f)

	toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque blue]
		plot (line ("Error for autodescent " ++ show p) [g])

plotDescentvHist :: String -> R -> Int -> Vector R -> (Vector R -> R) -> IO ()
plotDescentvHist name α numOfIter start f = do
	let g = zip ([0..] :: [Int]) $ map f (descentvHist α numOfIter start f)

	toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque blue]
		plot (line "Gradient descent's error" [g])

plotAutodescentvHist :: String -> R -> Vector R -> (Vector R -> R) -> IO ()
plotAutodescentvHist name p start f = do
	let g = zip ([0..] :: [Int]) $ map f (autodescentvHist p start f)

	toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque blue]
		plot (line ("Error for autodescentv " ++ show p ++ " " ++ show start) [g])

data Config = Config {α :: R, iters :: Int, start :: Vector R}

cfg :: Config
cfg = Config {α = 0.1, iters = 50}

superDescent :: Config -> (Vector R -> R) -> Vector R
superDescent cfg f
	| iters cfg <= 0 = start cfg
	| otherwise = superDescent (cfg {iters = iters cfg - 1, start = start cfg - scale (α cfg) (diffv f (start cfg))}) f
