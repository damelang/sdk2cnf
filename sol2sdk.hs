import Data.Char

formatValue n v
  | n < 4     = chr $ v - 1 + (ord '1')
  | otherwise = chr $ v - 1 + (ord 'A')

concatValue :: Int -> [Int] -> Int -> [Int]
concatValue n vals var = if var <= 0 then vals else val : vals
    where val = (var-1) `mod` (n^2) + 1

printRows :: Int -> [Int] -> IO ()
printRows n []   = return ()
printRows n vals = do let (row, vals') = splitAt (n^2) vals
                      putStrLn $ map (formatValue n) row
                      printRows n vals'

main = do c <- getContents
          let f  = \s -> read s::Int
          let vars = map f $ concat $ map (drop 1) $ map words $
                     filter ((== "v") . take 1) $ lines c
          let n = floor $ (**(1/6)) $ (fromInteger (toInteger $ (length vars) - 1)::Float)
          let vals = foldl (concatValue n) [] vars
          printRows n $ reverse vals
