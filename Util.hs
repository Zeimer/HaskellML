module Util where

import Data.List
import Data.List.Split
import Control.Monad

import System.Random.Shuffle

import Numeric.LinearAlgebra
import Numeric.GSL.Statistics

-- Graph of a function.
graph :: (a -> b) -> [a] -> [(a, b)]
graph f dom = zip dom $ map f dom

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

getXY' :: Matrix R -> (Matrix R, Vector R)
getXY' m =
	let	ls = map toList . toColumns $ m
		x' = init ls
		y' = last ls
		x = tr . fromLists $ x'
		y = fromList y'
	in (x, y)

getNormalizedXY' :: Matrix R -> (Matrix R, Vector R, Vector R, Vector R)
getNormalizedXY' m =
	let	ls = map toList . toColumns $ m
		x'' = init ls
		y' = last ls
		(x, means, stddevs) = normalizeFeatures' (tr . fromLists $ x'')
		y = fromList y'
	in (x, y, vector means, vector stddevs)


-- Utilities for reading a matrix from fie, shuffling it and splitting into train and test sets.
readMatr :: FilePath -> IO (Matrix R)
readMatr path = liftM (fromLists . map (map read) . map (splitOn ",") . lines) $ readFile path

splitMatr :: Matrix R -> (Matrix R, Matrix R)
splitMatr m =
	let
		n = floor $ 0.7 * fromIntegral (rows m)
		train = fromLists . take n. toLists $ m
		test = fromLists . drop n . toLists $ m
	in
		(train, test)

shuffleMatr :: Matrix R -> IO (Matrix R)
shuffleMatr m = do
	m' <- shuffleM (toLists m)
	return $ fromLists m'

readData :: FilePath -> IO (Matrix R, Vector R, Matrix R, Vector R)
readData path = do
	m <- readMatr path
	m' <- shuffleMatr m
	
	let
		(train, test) = splitMatr m'
		(x_train, y_train) = getXY' train
		(x_test, y_test) = getXY' test

	return (x_train, y_train, x_test, y_test)

readDataN :: FilePath -> IO (Matrix R, Vector R, Vector R, Vector R, Matrix R, Vector R, Vector R, Vector R)
readDataN path = do
	m <- readMatr path
	m' <- shuffleMatr m
	
	let
		(train, test) = splitMatr m'
		(x_train, y_train, μ_train, σ_train) = getNormalizedXY' train
		(x_test, y_test, μ_test, σ_test) = getNormalizedXY' test

	return (x_train, y_train, μ_train, σ_train, x_test, y_test, μ_test, σ_test)

-- Adjoin a column of 1's to a matrix/vector.
adjoinOnes :: Matrix R -> Matrix R
adjoinOnes = fromLists . map (1:) . toLists

adjoinOne :: Vector R -> Vector R
adjoinOne v = fromList $ 1 : toList v

-- Cost regularization.
reg' :: R -> (R -> R) -> (Vector R -> R) -> (Vector R -> R)
reg' λ r f v = f v + λ * sum (map r . tail . toList $ v)

reg :: R -> (Vector R -> R) -> (Vector R -> R)
reg λ f v = reg' λ (^2) f v

-- Sigmoid.
sigmoid :: R -> R
sigmoid x = 1 / (1 + exp (-x))

-- Mapping over vectors and matrices.
mmap :: (R -> R) -> Matrix R -> Matrix R
mmap f = fromLists . map (map f) . toLists

vmap :: (R -> R) -> Vector R -> Vector R
vmap f = fromList . map f . toList

vtom :: Vector R -> Matrix R
vtom v = fromLists $ [toList v]

-- Accuracy for classification problems.
accuracy :: Matrix R -> Vector R -> (Vector R -> R) -> R
accuracy x y h =
	let
		y' = vector $ map h (toRows x)
		errors = sum . toList $ vmap abs (y' - y)
		all = fromIntegral $ size y
	in
		errors / all
