module Ex2 where

import Data.List
import Data.List.Split
import Control.Monad

import Numeric.LinearAlgebra
import Numeric.GSL.Statistics

import Graphics.Rendering.Chart.Easy ((.=), points, line, plot, setColors, def, layout_title, opaque, blue, red)
import Graphics.Rendering.Chart.Backend.Cairo

import GradientDescent
import Util

-- Cost function for linear regression.
cost :: Matrix R -> Vector R -> Vector R -> R
cost x y t = 1 / (2 * (fromIntegral $ size y)) * sum (toList $ ((x #> t - y) ^ 2))

plotEx2 :: FilePath -> IO ()
plotEx2 path = do
	(x, y, μ, σ) <- liftM getNormalizedXY $ readMatr path
	
	let
		-- Fit the regressors using normal equation and gradient descent.
		θ_descent = autodescentv 1e-10 (vector $ take (cols x) [0.0,0.0..]) (cost x y)
		θ_normal_eq = normalEq x y

		c_descent = cost x y θ_descent
		c_normal_eq = cost x y θ_normal_eq

		-- Extract filename from the path.
		name = join . intersperse "." . init . splitOn "." $ path

	plotAutodescentvHist (name ++ "_norm_autodescent") 1e-10 (vector $ take (cols x) [0.0,0.0..]) (cost x y)

	putStrLn $ "Cost using gradient descent: " ++ show c_descent
	putStrLn $ "Cost using normal equation: " ++ show c_normal_eq
	putStrLn $ "Difference: " ++ show (c_descent - c_normal_eq)
	putStrLn $ "Ratio: " ++ show (c_descent / c_normal_eq)
