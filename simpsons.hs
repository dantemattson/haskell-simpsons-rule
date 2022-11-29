-- Run with ./simpsons

-- `n` is the stop (# intervals), acc is for internal stop, h should be getH
xVal :: Double -> Double -> Double -> Int -> [Double]
xVal ll n h acc
  | fromIntegral acc == n = [ll + fromIntegral acc * h]
  | otherwise = (ll + fromIntegral acc * h) : xVal ll n h (acc + 1)

-- This "iterates" over [xVal] like this:
-- for (int i = 0; i <= n; i++) { fx[i] = func(x[i]); }
funcVal :: [Double] -> Double -> Double -> [Double]
funcVal [] _ _ = []
funcVal [x] _ _ = [func x]
funcVal (x:xs) n acc = func x : funcVal xs n acc

-- This defines the width
-- Takes lower, upper, n of interval/s
getH :: Double -> Double -> Double -> Double
getH ll ul n = (ul-ll) / n

-- Returns accumulated, unadjusted ans
-- Takes funcV, iterates over and returns Double with the unadjusted result
calcResult :: Double -> Int -> [Double] -> Double
calcResult _ _ [] = undefined -- should never be executed
calcResult _ _ [x] = x
calcResult n acc (x:xs)
  | acc == 0 = x + calcResult n (acc+1) xs
  | (acc `mod` 2) /= 0 = (4 * x) + calcResult n (acc+1) xs
  | otherwise = (2 * x) + calcResult n (acc+1) xs

-- After final iteration ans = ans * (h/3)
adjustAns :: Double -> Double -> Double
adjustAns v h = v * (h / 3.0)

-- This defines the mathematical function for use in the integration
-- Can be changed
-- f(x) = sin x
func :: Double -> Double
func = sin

main :: IO ()
main = do
  -- Define needed values for the equation
  -- `func` is defined above
  let lowerLimit = 4.0
      upperLimit = 5.2
      intervalN :: Int
      intervalN = 6
  -- Ensure n is even
  if even intervalN then putStr "" else error "n must be even"
  -- Process
  let doubleN = fromIntegral intervalN
      h = getH lowerLimit upperLimit doubleN
      xV = xVal lowerLimit doubleN h 0
      funcV = funcVal xV doubleN 0
      unadjustedAnswer = calcResult doubleN 0 funcV
      answer = adjustAns unadjustedAnswer h
  -- Print
  print answer
