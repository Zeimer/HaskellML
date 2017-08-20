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
cost x y θ = 1 / (2 * (fromIntegral $ size y)) * sum (toList $ ((x #> θ - y) ^ 2))

cost' :: Matrix R -> Vector R -> Vector R -> Vector R
cost' x y θ = 1 / (fromIntegral $ size y) * ((x #> θ - y) <# x) 

-- Prediction for linear regression. 'N' means normalized, 'V' means vector version.
predict :: Vector R -> (R -> R)
predict θ = \arg -> adjoinOne (vector [arg]) <.> θ

predictN :: Vector R -> Vector R -> Vector R -> (R -> R)
predictN μ σ θ = \arg -> adjoinOne ((vector [arg] - μ) / σ) <.> θ

predictV :: Vector R -> (Vector R -> R)
predictV θ = \arg -> adjoinOne arg <.> θ

predictVN :: Vector R -> Vector R -> Vector R -> (Vector R -> R)
predictVN μ σ θ = \arg -> adjoinOne ((arg - μ) / σ) <.> θ

-- Plot a single experiment. 'N' means normalized.
plotEx1 :: FilePath -> String -> (Matrix R -> Vector R -> Vector R) -> IO ()
plotEx1 path prefix θ = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y) = getXY m

		-- Fit the regressor and draw its graph.
		f = predict (θ x y)
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ prefix) xy g

plotEx1N :: FilePath -> String -> (Matrix R -> Vector R -> Vector R) -> IO ()
plotEx1N path prefix θ = do
	m <- readMatr path
	
	let
		-- Extract data points to be plotted.
		xy = getDatapoints m

		-- Extract the feature vector x and output vector y.
		(x, y, μ, σ) = getNormalizedXY m

		-- Fit the regressor and draw its graph.
		f = predictN μ σ (θ x y)
		g = graph f [0, 0.05..25]

		-- Drop the extension from the filename.
		name = join . intersperse "." . init . splitOn "." $ path

	-- Plot datapoints and the predicted line.
	draw (name ++ prefix) xy g

-- Plot all experiments.
ex1' :: FilePath -> IO ()
ex1' path = do
	plotEx1 path "_normal_eq'" normalEq
	plotEx1 path "_descentV" $ \x y -> descentV 0.02 1000 (vector $ [0, 0]) (cost x y)
	plotEx1 path "_reg_descentV" $ \x y -> descentV 0.02 1000 (vector $ [0, 0]) (reg 0.1 (cost x y))
	plotEx1N path "_norm_descentV" $ \x y -> descentV 0.1 50 (vector $ [0, 0]) (cost x y)
	plotEx1N path "_reg_norm_descentV" $ \x y -> descentV 0.1 50 (vector $ [0, 0]) (reg 1e-10 (cost x y))
	plotEx1N path "_norm_descentAV" $ \x y -> descentAV 1e-5 (vector $ [0, 0]) (reg 0 (cost x y))
	plotEx1N path "_norm_descentAGV" $ \x y -> descentAGV 1e-5 (vector $ [0, 0]) (cost x y) (cost' x y)

ex1 = ex1' "ex1/ex1data1.txt"
