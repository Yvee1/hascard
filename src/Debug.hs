module Debug where
import System.IO.Unsafe (unsafePerformIO)

debugToFile :: String -> a -> a
debugToFile s expr = unsafePerformIO $ do
  appendFile "log.txt" s
  return expr