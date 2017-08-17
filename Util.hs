module Util where

import Data.List
import Data.List.Split
import Control.Monad

import Numeric.LinearAlgebra
import Numeric.GSL.Statistics

-- Graph of a function.
graph :: (a -> b) -> [a] -> [(a, b)]
graph f dom = zip dom $ map f dom

-- Read and preprocess a matrix.
readMatr :: FilePath -> IO (Matrix R)
readMatr path = liftM (fromLists . map (map read) . map (splitOn ",") . lines) $ readFile path

-- Feature normalization. Beware: possible division by zero.
normalizeFeatures' :: Matrix R -> (Matrix R, [R], [R])
normalizeFeatures' m =
	let	f l = let v = vector l in (l, mean v, stddev v)
		ls = toLists . tr $ m
		ls' = map f ls
		ls'' = map (\(l, μ, σ) -> map (\x -> (x - μ) / σ) l) ls'
		m' = tr . fromLists $ ls''
		means = map (\(_, μ, _) -> μ) ls'
		stddevs = map (\(_, _, σ) -> σ) ls'
	in (m', means, stddevs)

-- Extraction of datapoints and features from a single matrix.
getXY :: Matrix R -> (Matrix R, Vector R)
getXY m =
	let	ls = map toList . toColumns $ m
		x' = init ls
		y' = last ls
		x = adjoinOnes . tr . fromLists $ x'
		y = fromList y'
	in (x, y)

getNormalizedXY :: Matrix R -> (Matrix R, Vector R, Vector R, Vector R)
getNormalizedXY m = 
	let	ls = map toList . toColumns $ m
		x'' = init ls
		y' = last ls
		(x', means, stddevs) = normalizeFeatures' (tr . fromLists $ x'')
		x = adjoinOnes x'
		y = fromList y'
	in (x, y, vector means, vector stddevs)

-- Adjoin a column of 1's to a mtrix/vector.
adjoinOnes :: Matrix R -> Matrix R
adjoinOnes = fromLists . map (1:) . toLists

adjoinOne :: Vector R -> Vector R
adjoinOne v = fromList $ 1 : toList v

-- Cost regularization.
reg :: R -> (R -> R) -> (Vector R -> R) -> (Vector R -> R)
reg λ r f v = f v + λ * sum (map r . tail . toList $ v)
