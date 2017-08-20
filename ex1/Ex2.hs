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

cost' :: Matrix R -> Vector R -> Vector R -> Vector R
cost' x y θ = 1 / (fromIntegral $ size y) * ((x #> θ - y) <# x) 

ex2' :: FilePath -> IO ()
ex2' path = do
	(x, y, μ, σ) <- liftM getNormalizedXY $ readMatr path
	
	let
		-- Fit the regressors using normal equation and gradient descent.
		θ_normal_eq = normalEq x y
		θ_descentAV = descentAV 1e-2 (vector $ take (cols x) [0.0,0.0..]) (cost x y)
		t_descentAGV = descentAGV 1e-2 (vector $ take (cols x) [0.0,0.0..]) (cost x y) (cost' x y)

		-- Compute the costs.
		c_normal_eq = cost x y θ_normal_eq
		c_descentAV = cost x y θ_descentAV
		c_descentAGV = cost x y t_descentAGV

		-- Extract filename from the path.
		name = join . intersperse "." . init . splitOn "." $ path

	plotHist (name ++ "_norm_descentAHV") $ map (cost x y) $ descentAHV 1e-2 (vector $ take (cols x) [0.0,0.0..]) (cost x y)
	plotHist (name ++ "_norm_descent_AGHV") $ map (cost x y) $ descentAGHV 1e-2 (vector $ take (cols x) [0.0,0.0..]) (cost x y) (cost' x y)

	putStrLn $ "Cost using normal equation: " ++ show c_normal_eq
	putStrLn $ "Cost using descentAV: " ++ show c_descentAV
	putStrLn $ "Cost using descentAGV:" ++ show c_descentAGV

	putStrLn $ "Difference (equation - descentAV): " ++ show (c_descentAV - c_normal_eq)
	putStrLn $ "Ratio: " ++ show (c_descentAV / c_normal_eq)

ex2 :: IO ()
ex2 = ex2' "ex1/ex1data2.txt"
