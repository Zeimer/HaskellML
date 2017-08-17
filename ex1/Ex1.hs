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
getDatapoints :: Matrix R -> [(R, R)]
getDatapoints m = map (\[a, b] -> (a, b)) . map toList . toRows $ m

draw name datapoints graph = toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque red, opaque blue]
		plot (points "data, obviously" datapoints)
		plot (line "fit" [graph])

-- Cost function for linear regression.
cost :: Matrix R -> Vector R -> Vector R -> R
cost x y t = 1 / (2 * (fromIntegral $ size y)) * sum (toList $ ((x #> t - y) ^ 2))

-- Prediction for linear regression.
predict :: Vector R -> (R -> R)
predict θ = \arg -> adjoinOne (vector [arg]) <.> θ

predictNorm :: Vector R -> Vector R -> Vector R -> (R -> R)
predictNorm μ σ θ = \arg -> adjoinOne ((vector [arg] - μ) / σ) <.> θ

predictv :: Vector R -> (Vector R -> R)
predictv θ = \arg -> adjoinOne arg <.> θ

predictvNorm :: Vector R -> Vector R -> Vector R -> (Vector R -> R)
predictvNorm μ σ θ = \arg -> adjoinOne ((arg - μ) / σ) <.> θ

-- Use normal equation.
plotNormalEq :: FilePath -> IO ()
plotNormalEq path = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y) = getXY m

		-- Fit the regressor and draw its graph.
		f = predict $ normalEq x y
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ "_normal_eq") xy g

-- Prediction using gradient descent
plotDescent :: FilePath -> IO ()
plotDescent path = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y) = getXY m

		-- Fit the regressor and draw its graph.
		--f = predictDescent x y
		f = predict $ descentv 0.02 1000 (vector $ [0, 0]) (cost x y)
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ "_descent") xy g

-- Gradient descent with regularization.
plotRegDescent :: FilePath -> IO ()
plotRegDescent path = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y) = getXY m

		-- Fit the regressor and draw its graph.
		f = predict $ descentv 0.02 1000 (vector $ [0, 0]) (reg 0.1 (^2) (cost x y))
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ "reg_descent") xy g

-- Prediction using gradient descent with feature normalization.
plotNormDescent :: FilePath -> IO ()
plotNormDescent path = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y, μ, σ) = getNormalizedXY m

		-- Fit the regressor and draw its graph.
		f = predictNorm μ σ (descentv 0.1 50 (vector $ [0, 0]) (cost x y))
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ "_norm_descent") xy g

-- Prediction using regularized gradient descent with feature normalization.
plotRegNormDescent :: FilePath -> IO ()
plotRegNormDescent path = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y, μ, σ) = getNormalizedXY m

		-- Fit the regressor and draw its graph.
		f = predictNorm μ σ (descentv 0.1 50 (vector $ [0, 0]) (reg 1e-10 (^2) (cost x y)))
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ "_reg_norm_descent") xy g

-- Predict using autodescent.
plotRNAutoDescent :: FilePath -> IO ()
plotRNAutoDescent path = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y, μ, σ) = getNormalizedXY m

		-- Fit the regressor and draw its graph.
		f = predictNorm μ σ $ autodescentv 1e-5 (vector $ [0, 0]) (reg 0 (^2) (cost x y))
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ "_rn_autodescent") xy g

-- Perform all experiments.
plotEx1 :: FilePath -> IO ()
plotEx1 path = do
	plotNormalEq path
	plotDescent path
	plotNormDescent path
	plotRegDescent path
	plotRegNormDescent path
	plotRNAutoDescent path
