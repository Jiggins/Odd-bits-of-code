trapesiumRule :: (Double -> Double) -> [Double] -> Double
trapesiumRule f xs = h * (sum $ f a : f b : map (*2) [f x | x <- init $ tail xs])
  where a = head xs
        b = last xs
        n = fromIntegral $ length xs
        h = (b - a) / (2 * n)

simpsonsRule :: (Double -> Double) -> [Double] -> Double
simpsonsRule f xs = (h / 3) * (sum $ f a : f b : (map (*2) [f x | x <- seconds $ init xs] ++ (map (*4) [f x | x <- seconds $init $ tail xs])))
 where a = head xs
       b = last xs
       n = fromIntegral $ length xs
<<<<<<< HEAD
       h = (b - a) / (2 * n)
       seconds []	= []
       seconds (_:[])	= []
       seconds (x:y:xs) = y : seconds xs

main = do
	let f x = x^2 + x + 1
	print $ simpsonsRule f [1..5]
=======
       h = (b - a) / (n)
       seconds []       = []
       seconds (_:[])   = []
       seconds (x:y:xs) = y : seconds xs

main = do
  let f x = x^2 + x + 1
  print $ simpsonsRule f [0, 0.1..5]
>>>>>>> Fixed Simpson's rule, nt n/2
