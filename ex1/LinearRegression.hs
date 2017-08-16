module LinearRegression where

import Data.List
import Data.List.Split
import Control.Monad

import Numeric.LinearAlgebra
import Numeric.GSL.Statistics

import Graphics.Rendering.Chart.Easy ((.=), points, line, plot, setColors, def, layout_title, opaque, blue, red)
import Graphics.Rendering.Chart.Backend.Cairo

import GradientDescent

-- Graph of a function.
graph :: (a -> b) -> [a] -> [(a, b)]
graph f dom = zip dom $ map f dom

-- Read and preprocess a matrix.
readMatr :: FilePath -> IO (Matrix R)
readMatr path = liftM (fromLists . map (map read) . map (splitOn ",") . lines) $ readFile path

-- Beware: division by zero!
normalizeFeatures :: Matrix R -> Matrix R
normalizeFeatures m = tr . fromLists $ map (\l -> let μ = mean (vector l) in let σ = stddev (vector l) in map (\x -> (x - μ) / σ) l) (toLists $ tr m)

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

getDatapoints :: Matrix R -> [(R, R)]
getDatapoints m = map (\[a, b] -> (a, b)) . map toList . toRows $ m

getXY :: Matrix R -> (Matrix R, Vector R)
getXY m =
	let	ls = map toList . toColumns $ m
		x' = init ls
		y' = last ls
		x = adjoinOnes . tr . fromLists $ x'
		y = fromList y'
	in (x, y)

getNormalizedXY :: Matrix R -> (Matrix R, Vector R, [R], [R])
getNormalizedXY m =
	let	ls = map toList . toColumns $ m
		x'' = init ls
		y' = last ls
		(x', means, stddevs) = normalizeFeatures' (tr . fromLists $ x'')
		x = adjoinOnes x'
		y = fromList y'
	in (x, y, means, stddevs)

getNormalizedXY' :: Matrix R -> (Matrix R, Vector R, Vector R, Vector R)
getNormalizedXY' m = (x, y, vector means, vector stddevs)
	
	where	(x, y, means, stddevs) = getNormalizedXY m

adjoinOnes :: Matrix R -> Matrix R
adjoinOnes = fromLists . map (1:) . toLists

draw name datapoints graph = toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque red, opaque blue]
		plot (points "data, obviously" datapoints)
		plot (line "fit" [graph])

-- Prediction using normal equation
normalEq :: Matrix R -> Vector R -> Vector R
normalEq x y = (inv (tr x <> x) <> tr x) #> y

predictNormalEq :: Matrix R -> Vector R -> (R -> R)
predictNormalEq x y = \arg -> t0 + arg * t1
	
	where [t0, t1] = toList $ normalEq x y

plotNormalEq :: FilePath -> IO ()
plotNormalEq path = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y) = getXY m

		-- Fit the regressor and draw its graph.
		f = predictNormalEq x y
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ "_normal") xy g

-- Prediction using gradient descent
cost :: Matrix R -> Vector R -> Vector R -> R
cost x y t = 1 / (2 * (fromIntegral $ size y)) * sum (toList $ ((x #> t - y) ^ 2))

predictDescent :: Matrix R -> Vector R -> (R -> R)
predictDescent x y = \arg -> t0 + arg * t1

	where [t0, t1] = toList $ descentv 0.02 1000 (vector $ [0, 0]) (cost x y)

plotDescent :: FilePath -> IO ()
plotDescent path = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y) = getXY m

		-- Fit the regressor and draw its graph.
		f = predictDescent x y
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ "_descent") xy g

-- Gradient descent with regularization.
regCost :: R -> Matrix R -> Vector R -> Vector R -> R
regCost λ x y t = 1 / (2 * (fromIntegral $ size y)) * sum (toList $ ((x #> t - y) ^ 2)) + λ * sum (map (^2) . tail . toList $ t)

predictRegDescent :: R -> Matrix R -> Vector R -> (R -> R)
predictRegDescent λ x y = \arg -> t0 + arg * t1

	where [t0, t1] = toList $ descentv 0.02 1000 (vector $ [0, 0]) (regCost λ x y)

plotRegDescent :: FilePath -> IO ()
plotRegDescent path = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y) = getXY m

		-- Fit the regressor and draw its graph.
		f = predictRegDescent 0.1 x y
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ "reg_descent") xy g

-- Prediction using gradient descent with feature normalization.
predictNormDescent :: R -> R -> Matrix R -> Vector R -> (R -> R)
predictNormDescent μ σ x y = \arg -> t0 + (arg - μ) / σ * t1

	where [t0, t1] = toList $ descentv 0.1 50 (vector $ [0, 0]) (cost x y)

plotNormDescent :: FilePath -> IO ()
plotNormDescent path = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y, [μ], [σ]) = getNormalizedXY m

		-- Fit the regressor and draw its graph.
		f = predictNormDescent μ σ x y
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ "_norm_descent") xy g

-- Prediction using regularized gradient descent with feature normalization.
predictRegNormDescent :: R -> R -> R -> Matrix R -> Vector R -> (R -> R)
predictRegNormDescent λ μ σ x y = \arg -> t0 + (arg - μ) / σ * t1

	where [t0, t1] = toList $ descentv 0.1 50 (vector $ [0, 0]) (regCost λ x y)

plotRegNormDescent :: FilePath -> IO ()
plotRegNormDescent path = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y, [μ], [σ]) = getNormalizedXY m

		-- Fit the regressor and draw its graph.
		f = predictRegNormDescent 1e-10 μ σ x y
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ "_reg_norm_descent") xy g

-- Predict using autodescent.
{-predictRNAutoDescent :: R -> R -> R -> Matrix R -> Vector R -> (R -> R)
predictRegNormDescent λ μ σ x y = \arg -> t0 + (arg - μ) / σ * t1

	where [t0, t1] = toList $ descentv 0.1 50 (vector $ [0, 0]) (regCost λ x y)-}

-- Perform all experiments.
plotAll :: FilePath -> IO ()
plotAll path = do
	plotNormalEq path
	plotDescent path
	plotNormDescent path
	plotRegDescent path
	plotRegNormDescent path

plotEx1 :: IO ()
plotEx1 = plotAll "ex1data1.txt"

-- Exercise 2
predictRegNormDescentv :: Int -> R -> R -> Vector R -> Vector R -> Matrix R -> Vector R -> (Vector R -> R)
predictRegNormDescentv numOfIter α λ μ σ x y arg = arg'' <.> t

	where	arg' = (arg - μ) / σ
		arg'' = fromList (1 : toList arg')
		t = descentv α numOfIter (vector $ [0.0, 1.0..(fromIntegral (cols x) - 1)]) (regCost λ x y)

plotEx2 :: IO ()
plotEx2 = do
	m <- readMatr "ex1data2.txt"
	
	let
		-- Extract the feature vector x and output vector y.
		(x, y, μ, σ) = getNormalizedXY' m

		-- Fit the regressor and draw its graph.
		f = predictRegNormDescentv 500 0.1 1e-10 μ σ x y

	plotDescentvHist "descent" 0.125 50 (vector $ [0.0, 1.0..(fromIntegral (cols x) - 1)]) (regCost 0 x y)

predictNormalEq' :: Matrix R -> Vector R -> (Vector R -> R)
predictNormalEq' x y arg = arg' <.> t
	
	where	arg' = fromList (1 : toList arg)
		t = normalEq x y

reg :: R -> (Vector R -> R) -> (Vector R -> R)
reg λ f v = f v + λ * sum (map (^2) . tail . toList $ v)
