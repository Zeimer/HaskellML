module LinearRegression where

import Numeric.LinearAlgebra
import Numeric.GSL.Statistics

import Util

-- Cost function for linear regression.
cost :: Matrix R -> Vector R -> Vector R -> R
cost x y θ = 1 / (2 * (fromIntegral $ size y)) * sum (toList $ ((x #> θ - y) ^ 2))

cost' :: Matrix R -> Vector R -> Vector R -> Vector R
cost' x y θ = 1 / (fromIntegral $ size y) * ((x #> θ - y) <# x)

regCost :: R -> Matrix R -> Vector R -> Vector R -> R
regCost λ x y = reg λ (cost x y)

regCost' :: R -> Matrix R -> Vector R -> Vector R -> Vector R
regCost' λ x y θ =
	let
		m = fromIntegral $ size y
		t = vector $ 0 : (drop 1 . toList $ θ)
	in
		scale (1 / m) ((x #> θ - y) <# x) + scale (λ / m) t

-- Prediction for linear regression. 'N' means normalized, 'V' means vector version.
predict :: Vector R -> (R -> R)
predict θ = \arg -> adjoinOne (vector [arg]) <.> θ

predictN :: Vector R -> Vector R -> Vector R -> (R -> R)
predictN μ σ θ = \arg -> adjoinOne ((vector [arg] - μ) / σ) <.> θ

predictV :: Vector R -> (Vector R -> R)
predictV θ = \arg -> adjoinOne arg <.> θ

predictVN :: Vector R -> Vector R -> Vector R -> (Vector R -> R)
predictVN μ σ θ = \arg -> adjoinOne ((arg - μ) / σ) <.> θ
