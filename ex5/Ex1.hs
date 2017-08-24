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

read3 :: FilePath -> FilePath -> FilePath -> IO (Matrix R, Vector R, Matrix R, Vector R, Matrix R, Vector R)
read3 p1 p2 p3 = do
	(x1, y1) <- liftM getXY $ readMatrG " " p1
	(x2, y2) <- liftM getXY $ readMatrG " " p2
	(x3, y3) <- liftM getXY $ readMatrG " " p3

	return (x1, y1, x2, y2, x3, y3)

-- Plot training and validation curves.
plotCurves ::
	String ->					-- Name under which the file is to be saved.
	Matrix R -> Vector R ->				-- Train data.
	Matrix R -> Vector R ->				-- Validation data.
	(Matrix R -> Vector R -> Vector R -> R) ->	-- Cost function.
	((Matrix R, Vector R) -> Vector R) ->		-- Parameter learning function.
	IO ()

plotCurves name x_train y_train x_val y_val cost f = do
	let
		xs_train = map (\n -> takeRows n x_train) [1..(rows x_train)]
		ys_train = map (\n -> fromList . take n $ toList y_train) [1..(rows x_train)]

		θs = map f (zip xs_train ys_train)

		training_curve = zip ([1..] :: [Int]) $ map (\(x, y, θ) -> cost x y θ) $ zip3 xs_train ys_train θs
		validation_curve = zip ([1..] :: [Int]) $ map (cost x_val y_val) θs

	toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque blue, opaque green]
		plot (line "training curve" [training_curve])
		plot (line "validation curve" [validation_curve])

poly :: Int -> Matrix R -> Matrix R
poly p m
	| p < 1 = error "WUT?"
	| otherwise =
		let
			cols = replicate p . head . transpose . toLists $ m
			cols' = zip [1..] cols
		in
			fromLists . transpose . map (\(i, col) -> map (^i) col) $ cols'
		

-- Step through the whole exercise.
ex5 :: IO ()
ex5 = do
	m_train <- readMatrG " " "ex5/train.txt"
	m_val <- readMatrG " " "ex5/val.txt"
	m_test <- readMatrG " " "ex5/test.txt"
	
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

	plotCurves "ex5/curves" x_train y_train x_val y_val cost $
		\(x, y) -> descentAGV 4e-1 (vector [0, 0]) (cost x y) (cost' x y)


prepare :: Matrix R -> (Matrix R, Vector R, Vector R)
prepare x''' =
	let
		x'' = poly 8 x'''
		(x', μ, σ) = normalizeFeatures' x''
		x = adjoinOnes x'
	in (x, μ, σ)


-- Step through the whole exercise.
ex5' :: IO ()
ex5' = do
	m_train <- readMatrG " " "ex5/train.txt"
	m_val <- readMatrG " " "ex5/val.txt"
	m_test <- readMatrG " " "ex5/test.txt"
	
	let
		(x_train''', y_train) = getXY' m_train
		(x_val', y_val) = getXY' m_val
		(x_test', y_test, _, _) = getNormalizedXY m_test

		{-x_train'' = poly 8 x_train'''
		(x_train', μ, σ) = normalizeFeatures' x_train''
		x_train = adjoinOnes x_train'-}

		(x_train, μ, σ) = prepare x_train'''
		(x_val, _, _) = prepare x_val'

		datapoints = map (\[a, b] -> (a, b)) (toLists m_train)

		h = predictVN μ σ $ descentAGV 1e-3 (vector $ replicate 9 0) (regCost 0 x_train y_train) (regCost' 0 x_train y_train)
		h' = \x -> h . fromList . take 8 . map (\(i, x) -> x^i) $ zip [1..] [x,x..]
		g = graph h' [-50, 0.05..50]

		name = "ex5/ex5_norm_ling_reg"

	toFile def (name ++ ".png") $ do
		layout_title .= name
		setColors [opaque red, opaque blue]
		plot (points "data, obviously" datapoints)
		plot (line "fit" [g])

	plotCurves "ex5/curves2" x_train y_train x_val y_val cost $
		\(x, y) -> descentAGV 1e-3 (vector $ replicate 9 0) (cost x y) (cost' x y)
