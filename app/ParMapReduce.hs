import Options.Applicative
import Data.Semigroup ((<>))

import Text.Pretty.Simple (pPrint)
import qualified Data.ByteString.Char8 as BS

import Control.Concurrent.Async.Pool
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Applicative
import Control.Concurrent
import Data.Monoid

main :: IO ()
main = do
  p' <- createPool
  p  <- createTaskGroup p' 20000
  h <- atomically $ mapReduce p $ map (return . Sum) [1..10]
  --g <- atomically $ readTVar (tasks (pool p))
  Async.withAsync (runTaskGroup p) $ const $ do
    x <- wait h
    putStrLn $ show x
    --x `shouldBe` Sum 55


  putStrLn "a"

