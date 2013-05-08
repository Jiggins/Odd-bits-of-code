trapesiumRule f xs = h * (sum $ f a : f b : map (*2) [f x | x <- slice 2 (length xs - 1) xs])
  where a = head xs
        b = last xs
        n = fromIntegral $ length xs
        h = (b - a) / (2 * n)


simpsonsRule f xs = (h / 3) * (sum $ f a : f b : (map (*2) [x | x <- filter odd $ slice 2 (length xs - 1) xs] ++ (map (*2) [x | x <- filter odd $ slice 2 (length xs - 1) xs])))
 where a = head xs
       b = last xs
       n = fromIntegral $ length xs
       h = (b - a) / (2 * n)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop (from - 1) xs)

main = do
	let f x = x^2 + x + 1
	print $ trapesiumRule f [1..5]