import Data.Char
import Data.Bits

formatValue n v
  | n < 4     = chr $ v - 1 + (ord '1')
  | otherwise = chr $ v - 1 + (ord 'A')

printRows :: Int -> [Int] -> IO ()
printRows n []   = return ()
printRows n vals = do let (row, vals') = splitAt (n^2) vals
                      putStrLn $ map (formatValue n) row
                      printRows n vals'

extractValues :: Int -> [Int] -> [Int]
extractValues m [_] = []
extractValues m vars = (foldr f 0 vs) : (extractValues m vars')
  where (vs, vars') = splitAt m vars
        f var val   = val*2 + (if var > 0 then 1 else 0)

getN nvars
  | (nvars ==   32) = 2
  | (nvars ==  324) = 3
  | (nvars == 1024) = 4
  | (nvars == 3125) = 5
  | otherwise       = error ("Can't handle this number of vars: " ++ (show nvars))

main = do c <- getContents
          let f  = \s -> read s::Int
          let vars = map f $ concat $ map (drop 1) $ map words $
                     filter ((== "v") . take 1) $ lines c
          let n = getN $ (length vars) - 1
          let m = ceiling $ logBase 2 (fromInteger (toInteger (n^2))::Float)
          let vals = map (+1) $ extractValues m vars
          printRows n vals
