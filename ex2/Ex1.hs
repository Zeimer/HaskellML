module Ex1 where

import Data.List
import Data.List.Split
import Control.Monad

import Numeric.LinearAlgebra
import Numeric.GSL.Statistics

import Graphics.Rendering.Chart.Easy ((.=), points, line, plot, setColors, def, layout_title, opaque, blue, red)
import Graphics.Rendering.Chart.Backend.Cairo

import GradientDescent
import Util

-- Visualization.
getDatapoints :: Matrix R -> [(R, R, R)]
getDatapoints m = map (\[a, b, c] -> (a, b, c)) . map toList . toRows $ m

draw name pos neg = toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque red, opaque blue]
		plot (points "1" (map (\(a, b, _) -> (a, b)) pos))
		plot (points "0" (map (\(a, b, _) -> (a, b)) neg))

plotData :: FilePath -> IO ()
plotData path = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m
		(pos, neg) = partition (\(_, _, c) -> c == 1.0) xy

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ "_data") pos neg

-- Cost function for logistic regression.
cost :: Matrix R -> Vector R -> Vector R -> R
cost x y θ =
	let
		m = fromIntegral (size y)
		y1 = scale (-1) y
		y2 = vmap (\x -> 1 - x) y
		h = vmap sigmoid (x #> θ)
		h1 = y1 <.> vmap log h
		h2 = y2 <.> vmap (\x -> log (1 - x)) h
	in
		1 / m * (h1 - h2)

cost' :: Matrix R -> Vector R -> Vector R -> Vector R
cost' x y t =
	let
		m = fromIntegral (size y)
		h = vmap sigmoid (x #> t)
	in
		1 / m * ((h - y) <# x)

-- Prediction for logistic regression. 'N' means normalized.
predict :: Vector R -> (Vector R -> R)
predict θ arg = sigmoid $ adjoinOne arg <.> θ

predictN :: Vector R -> Vector R -> Vector R -> (Vector R -> R)
predictN μ σ θ arg = sigmoid $ adjoinOne ((arg - μ) / σ) <.> θ

predict' :: Vector R -> (Vector R -> R)
predict' θ arg = sigmoid $ arg <.> θ

roundR :: R -> R
roundR = fromIntegral . round


plotDescentN :: FilePath -> R -> IO ()
plotDescentN path p = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y, μ, σ) = getNormalizedXY m

		-- Fit the regressor and draw its graph.
		f = predictN μ σ $ descentAV p (vector $ take (cols x) [0.0,0.0..]) (cost x y)

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	putStrLn $ "Prediction for a student with scores 45 and 85 (should be 0.776): " ++ show (f $ vector [45, 85])

	-- Plot datapoints and the predicted line.
	plotHist (name ++ "_norm_descentAHV") $ map (cost x y) $ descentAHV p (vector $ take (cols x) [0.0,0.0..]) (cost x y)

plotDescentAGHVN :: FilePath -> R -> IO ()
plotDescentAGHVN path p = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x', y, μ, σ) = getNormalizedXY' m
		x = adjoinOnes x'

		-- Fit the regressor and draw its graph.
		f = predictN μ σ $ descentAGV p (vector $ take (cols x) [0.0,0.0..]) (cost x y) (cost' x y)

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	putStrLn $ "Prediction for a student with scores 45 and 85 (should be 0.776): " ++ show (f $ vector [45, 85])
	putStrLn $ "Accuracy: " ++ show (accuracy x' y $ roundR . f)

	-- Plot datapoints and the predicted line.
	plotHist (name ++ "_norm_descentAGHV") $ map (cost x y) $ descentAGHV p (vector $ take (cols x) [0.0,0.0..]) (cost x y) (cost' x y)


ex1 :: R -> IO ()
ex1 p = do
	let path = "ex2/ex2data1.txt"
	plotData path
	plotDescentN path p
	plotDescentAGHVN path p

measureAcc :: FilePath -> IO R
measureAcc path = do
	(x_train, y_train, x_test, y_test) <- readData "ex2/ex2data1.txt"

	let
		g = predict $ descentAV 1e-1 (vector $ take (1 + cols x_train) [0.0,0.0..]) (cost (adjoinOnes x_train) y_train)
		h = roundR . g :: Vector R -> R
	
	return $ accuracy x_test y_test h

measureAccN :: FilePath -> IO R
measureAccN path = do
	(x_train, y_train, _, _, x_test, y_test, μ_test, σ_test) <- readDataN "ex2/ex2data1.txt"

	let
		g = predictN μ_test σ_test $ descentAV 1e-1 (vector $ take (1 + cols x_train) [0.0,0.0..]) (cost (adjoinOnes x_train) y_train)
		h = fromIntegral . round . g :: Vector R -> R
	
	return $ accuracy x_test y_test h

-- Exercise 2.
mapFeature :: Vector R -> Vector R
mapFeature v =
	let
		l = [(x, y) | x <- [0..6], y <- [0..6], x + y <= 6]
	in
		vector $ map (\(p1, p2) -> (v!0)^p1 * (v!1)^p2) l

-- Regularized cost and its derivative.
sumv :: Vector R -> R
sumv = sum . toList

regCost :: R -> Matrix R -> Vector R -> Vector R -> R
regCost λ x y θ =
	let
		m = fromIntegral (size y)
		y1 = scale (-1) y
		y2 = vmap (\x -> 1 - x) y
		h = vmap sigmoid (x #> θ)
		h1 = y1 <.> vmap log h
		h2 = y2 <.> vmap (\x -> log (1 - x)) h

		t = vector $ 0 : (drop 1 . toList $ θ)
	in
		1 / m * (h1 - h2) + λ / (2 * m) * t <.> t


regCost' :: R -> Matrix R -> Vector R -> Vector R -> Vector R
regCost' λ x y θ =
	let
		m = fromIntegral (size y)
		h = vmap sigmoid (x #> θ)
		t = vector $ 0 : (drop 1 . toList $ θ)
	in
		scale (1 / m) ((h - y) <# x) + scale (λ / m) t


-- TODO
ex2' :: FilePath -> R -> R -> IO ()
ex2' path p λ = do
	m <- readMatr path
	
	let
		-- Extract the feature vector x and output vector y.
		(d, y) = getXY' m
		x' = fromLists . map (toList . mapFeature . vector) . toLists $ d
		--x' = d
		x = adjoinOnes x'

		-- Fit the regressor and draw its graph.
		f = predict' $ descentAGV p (vector $ take (cols x') [0.0,0.0..]) (regCost λ x' y) (regCost' λ x' y)

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	putStrLn $ "Accuracy: " ++ show (accuracy x' y $ roundR . f)

	-- Plot datapoints and the predicted line.
	plotHist (name ++ "_descentAGHV") $ map (regCost λ x' y) $ descentAGHV p (vector $ take (cols x') [0.0,0.0..]) (regCost λ x' y) (regCost' λ x y)

ex2 = ex2' "ex2/ex2data2.txt"
ex2N = ex2' "ex2/ex2data2.txt"
