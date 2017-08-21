module Ex1 where

import Data.List
import Data.List.Split
import Control.Monad

import Numeric.LinearAlgebra
import Numeric.GSL.Statistics

import Graphics.Rendering.Chart.Easy ((.=), points, line, plot, setColors, def, layout_title, opaque, blue, red, green)
import Graphics.Rendering.Chart.Backend.Cairo

import GradientDescent
import Util
import LinearRegression

readMatrG :: String -> FilePath -> IO (Matrix R)
readMatrG sep path = liftM (fromLists . map (map read . splitOn sep . tail) . lines) $ readFile path

draw name datapoints = toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque red]
		plot (points "data, obviously" datapoints)

plotData :: IO ()
plotData = do
	m <- readMatrG " " "ex5/train.txt"
	draw "ex5/data" $ map (\[x, y] -> (x, y)) (toLists m)

-- Step through the whole exercise.
ex5 :: IO ()
ex5 = do
	m_train <- readMatrG " " "ex5/train.txt"
	m_val <- readMatrG " " "ex5/val.txt"
	m_test <- readMatrG " " "ex5/test.txt"

	--draw "ex5/data" $ map (\[a, b] -> (a, b)) (toLists m_train)
	
	let
		(x_train, y_train) = getXY m_train
		(x_val, y_val) = getXY m_val
		(x_test, y_test) = getXY m_test

		datapoints = map (\[a, b] -> (a, b)) (toLists m_train)

		h = predict $ descentAGV 4e-1 (vector [0, 0]) (regCost 1 x_train y_train) (regCost' 1 x_train y_train)
		g = graph h [-50, 0.05..50]

		name = "ex5/wut"

	toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque red, opaque blue]
		plot (points "data, obviously" datapoints)
		plot (line "fit" [g])

	-- Training curve.
	let
		xs_train = map (\n -> takeRows n x_train) [1..(rows x_train)]
		ys_train = map (\n -> fromList . take n $ toList y_train) [1..(rows x_train)]

		θs = map (\(x, y) -> descentAGV 4e-1 (vector [0, 0]) (cost x y) (cost' x y)) (zip xs_train ys_train)

		train_curve = map (\(x, y, θ) -> cost x y θ) $ zip3 xs_train ys_train θs
		val_curve = map (cost x_val y_val) θs

		g_train = zip ([1..] :: [Int]) train_curve
		g_val = zip ([1..] :: [Int]) val_curve

	toFile def ("ex5/train_curve.png") $ do
		layout_title .= name
		setColors [opaque blue, opaque green]
		plot (line "train curve" [g_train])
		plot (line "val curve" [g_val])
	
