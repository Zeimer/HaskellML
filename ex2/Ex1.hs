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
sigmoid :: R -> R
sigmoid x = 1 / (1 + exp (-x))

mmap :: (R -> R) -> Matrix R -> Matrix R
mmap f = fromLists . map (map f) . toLists

vmap :: (R -> R) -> Vector R -> Vector R
vmap f = fromList . map f . toList

vtom :: Vector R -> Matrix R
vtom v = fromLists $ [toList v]

cost :: Matrix R -> Vector R -> Vector R -> R
cost x y θ =

	let
		m = fromIntegral (size y)
		y1 = scale (-1) y
		y2 = vmap (\x -> 1 - x) y
		h = mmap sigmoid (vtom θ <> tr x)
		h1 = y1 <# tr (mmap log h)
		h2 = y2 <# tr (mmap (\x -> log (1 - x)) h)
	in
		1 / m * (sum . toList $ h1 - h2)

-- Prediction for logistic regression.
predictLogistic :: Vector R -> (Vector R -> R)
predictLogistic θ arg = sigmoid $ adjoinOne arg <.> θ

predictLogisticNorm :: Vector R -> Vector R -> Vector R -> (Vector R -> R)
predictLogisticNorm μ σ θ arg = sigmoid $ adjoinOne ((arg - μ) / σ) <.> θ

plotNormDescent :: FilePath -> R -> IO ()
plotNormDescent path p = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y, μ, σ) = getNormalizedXY m

		-- Fit the regressor and draw its graph.
		f = predictLogisticNorm  μ σ $ autodescentv p (vector $ take (cols x) [0.0,0.0..]) (cost x y)

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	putStrLn $ "Prediction for a student with scores 45 and 85 (should be 0.776): " ++ show (f $ vector [45, 85])

	-- Plot datapoints and the predicted line.
	plotAutodescentvHist (name ++ "_norm_autodescent") p (vector $ take (cols x) [0.0,0.0..]) (cost x y)

ex1 :: IO ()
ex1 = do
	let path = "ex2/ex2data1.txt"
	plotData path
	plotNormDescent path 1e-8
