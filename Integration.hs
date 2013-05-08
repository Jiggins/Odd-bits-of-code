trapesiumRule f xs = h * (sum $ f a : f b : map (*2) [f x | x <- init $ tail xs])
  where a = head xs
        b = last xs
        n = fromIntegral $ length xs
        h = (b - a) / (2 * n)

<<<<<<< HEAD
simpsonsRule f xs = (h / 3) * (sum $ f a : f b : (map (*2) [x | x <- filter odd $ slice 2 (length xs - 1) xs] ++ (map (*4) [x | x <- filter odd $ slice 2 (length xs - 1) xs])))
=======

simpsonsRule f xs = (h / 3) * (sum $ f a : f b : (map (*2) [x | x <- filter odd $ init $ tail xs] ++ (map (*2) [x | x <- filter odd $ init $ tail xs])))
>>>>>>> Added Conor's suggestion
 where a = head xs
       b = last xs
       n = fromIntegral $ length xs
       h = (b - a) / (2 * n)

main = do
	let f x = x^2 + x + 1
	print $ trapesiumRule f [1..5]
