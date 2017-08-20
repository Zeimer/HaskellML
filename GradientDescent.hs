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

-- The naming is as follows:
-- 1. The standard version is unprefixed.
-- 2. 'A' means that the parameter alpha is chosen automatically. No 'A' means it has to be given.
-- 3. 'G' means it takes the derivative as an argument. No 'G' means that an approximation is used.
-- 4. 'H' means the algorithm returns a list of consecutive approximations instead of the final result only.
-- 5. 'V' means the domain of the function is a vector.

descentG :: R -> Int -> R -> (R -> R) -> (R -> R) -> R
descentG alpha numOfIter start f f'
	| numOfIter <= 0 = start
	| otherwise = descentG alpha (numOfIter - 1) (start - alpha * f' start) f f'

descent a n x f = descentG a n x f (diff f) 

descentAG :: R -> (R -> R) -> (R -> R) -> R
descentAG p f f' = aux 1e-3 0 f f' where

	aux alpha start f f'
		| f (start - alpha * f' start) > f start = aux (alpha / 2) start f f'
		| abs (f' start) < p = start
		| otherwise = aux (2 * alpha) (start - alpha * f' start) f f'

descentA p f = descentAG p f (diff f)

descentGV :: R -> Int -> Vector R -> (Vector R -> R) -> (Vector R -> Vector R) -> Vector R
descentGV alpha numOfIter start f f'
	| numOfIter <= 0 = start
	| otherwise = descentGV alpha (numOfIter - 1) (start - scale alpha (f' start)) f f'

descentV a n x f = descentGV a n x f (diffv f)

descentAGV :: R -> Vector R -> (Vector R -> R) -> (Vector R -> Vector R) -> Vector R
descentAGV p start f f' = aux 1e-3 start f f' where

	aux alpha start f f'
		| f (start - scale alpha (f' start)) > f start = aux (alpha / 2) start f f'
		| norm (f' start) < p = start
		| otherwise = aux (2 * alpha) (start - scale alpha (f' start)) f f'

descentAV p x f = descentAGV p x f (diffv f)

descentGH :: R -> Int -> R -> (R -> R) -> (R -> R) -> [R]
descentGH a n x f f'
	| n <= 0 = [x]
	| otherwise = x : descentGH a (n - 1) (x - a * f' x) f f'

descentH a n x f = descentGH a n x f (diff f)

descentAGHV :: R -> Vector R -> (Vector R -> R) -> (Vector R -> Vector R) -> [Vector R]
descentAGHV p start f f' = aux 1e-3 start f f' where

	aux alpha start f f'
		| f (start - scale alpha (f' start)) > f start = aux (alpha / 2) start f f'
		| norm (f' start) < p = [start]
		| otherwise = start : aux (2 * alpha) (start - scale alpha (f' start)) f f'


descentAH :: R -> (R -> R) -> [R]
descentAH p = aux 1 0 where

	aux alpha start f
		| f (start - alpha * diff f start) > f start = aux (alpha / 2) start f
		| abs (diff f start) < p = [start]
		| otherwise = start : aux (2 * alpha) (start - alpha * diff f start) f

descentAHV :: R -> Vector R -> (Vector R -> R) -> [Vector R]
descentAHV p start = aux 1e-3 start where

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

agh :: R -> Vector R -> (Vector R -> R) -> (Vector R -> Vector R) -> [Vector R]
agh a start f f' = aux [start] a f f' where

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

agh' :: R -> Vector R -> (Vector R -> R) -> (Vector R -> Vector R) -> [Vector R]
agh' a start f f' = aux a start f f' where

	aux :: R -> Vector R -> (Vector R -> R) -> (Vector R -> Vector R) -> [Vector R]
	aux alpha x f f' =
		let
			x' = x - scale alpha (f' x)
		in
			case compare (f x') (f x) of
				GT -> x : aux (alpha / 2) x' f f'
				EQ -> [x]
				LT -> x : aux (2 * alpha) x' f f'

