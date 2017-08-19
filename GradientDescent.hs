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

descent a n x f = descentGrad a n x f (diff f) 

autodescentGrad :: R -> (R -> R) -> (R -> R) -> R
autodescentGrad p f f' = aux 1e-100 0 f f' where

	aux alpha start f f'
		| f (start - alpha * f' start) > f start = aux (alpha / 2) start f f'
		| abs (f' start) < p = start
		| otherwise = aux (2 * alpha) (start - alpha * f' start) f f'

autodescent p f = autodescentGrad p f (diff f)

descentvGrad :: R -> Int -> Vector R -> (Vector R -> R) -> (Vector R -> Vector R) -> Vector R
descentvGrad alpha numOfIter start f f'
	| numOfIter <= 0 = start
	| otherwise = descentvGrad alpha (numOfIter - 1) (start - scale alpha (f' start)) f f'

descentv a n x f = descentvGrad a n x f (diffv f)

autodescentvGrad :: R -> Vector R -> (Vector R -> R) -> (Vector R -> Vector R) -> Vector R
autodescentvGrad p start f f' = aux 1e-100 start f f' where

	aux alpha start f f'
		| f (start - scale alpha (f' start)) > f start = aux (alpha / 2) start f f'
		| norm (f' start) < p = start
		| otherwise = aux (2 * alpha) (start - scale alpha (f' start)) f f'

autodescentv p x f = autodescentvGrad p x f (diffv f)

agh :: Vector R -> (Vector R -> R) -> (Vector R -> Vector R) -> [Vector R]
agh start f f' = aux [start] 1 f f' where

	aux hist@(h1:h2:_) alpha f f'
		| h1 == h2 = hist
	aux hist@(h:_) alpha f f'
		| f (h - scale alpha (f' h)) > f h = aux hist (alpha / 2) f f'
	--	| f (h - scale alpha (f' h)) == f h = hist
		| otherwise = aux (h - scale alpha (f' h) : hist) (2 * alpha) f f'
	{-aux hist@(h:_) alpha f f' =
		let
			h' = h - scale alpha (f' h)
		in
			case compare (f h') (f h) of
				GT -> aux (h': hist) (alpha / 2) f f'
				EQ -> h' : hist
				LT -> aux (h' : hist) (2 * alpha) f f'-}

descentHist :: R -> Int -> R -> (R -> R) -> [R]
descentHist alpha numOfIter start f
	| numOfIter <= 0 = [start]
	| otherwise = start : descentHist alpha (numOfIter - 1) (start - alpha * diff f start) f


autodescentvGradHist :: R -> Vector R -> (Vector R -> R) -> (Vector R -> Vector R) -> [Vector R]
autodescentvGradHist p start f f' = aux 1e-100 start f f' where

	aux alpha start f f'
		| f (start - scale alpha (f' start)) > f start = aux (alpha / 2) start f f'
		| norm (f' start) < p = [start]
		| otherwise = start : aux (2 * alpha) (start - scale alpha (f' start)) f f'


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

plotHist :: String -> [R] -> IO ()
plotHist name points = do
	let g = zip ([0..] :: [Int]) points

	toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque blue]
		plot (line "Gradient descent's error" [g])


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
