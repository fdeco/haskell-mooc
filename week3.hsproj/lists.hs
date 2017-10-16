module Lists where
  
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs
  | null xs = []
  | otherwise = let (x:xs') = xs in
      if p x then x:filter' p xs'
      else filter' p xs'
