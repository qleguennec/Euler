import qualified Problems.Current as Problem
import Test.QuickCheck
import Control.Monad (unless)
import System.Environment

test = quickCheck Problem.test
res = putStrLn $ show Problem.res

parseArgs :: [String] -> IO ()
parseArgs ["test"] = test
parseArgs ["res"] = res
parseArgs [] = putStrLn "Usage: test/res"
parseArgs args = putStrLn "Usage: test/res"

main = getArgs >>= parseArgs
