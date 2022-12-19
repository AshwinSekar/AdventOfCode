import Utils

main :: IO ()
main = do
  let go d (p1, p2) (s1, s2)
        | s2 >= 1000 = (s2, s1, d * 3)
        | otherwise = go (d + 1) (p2, p1') (s2, s1 + p1')
        where
          i = (1 + 3 * d) `mod` 10
          p1' = modCeil (p1 + 3 * i + 3) 10
      (w, l, d) = go 0 (2, 1) (0, 0)
  putStrLn $ "Part 1: " ++ show (l * d)

-- putStrLn $ "Part 2: " ++ show p2

dirac = [2, 3, 3, 4, 4, 4, 5, 5, 6]
