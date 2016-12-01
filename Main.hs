module Main where

import Prelude hiding (catch)
import Control.Exception ( AsyncException(..), catch )
import Control.Monad.Error
import qualified Control.Monad.Parallel as MP

import Data.Version
import Data.List

import System.IO
import System.Environment
import System.Directory (getHomeDirectory)
import System.Console.Haskeline hiding (handle, catch, throwTo)
import System.Console.GetOpt
import System.Exit (ExitCode (..), exitWith, exitFailure)

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

main :: IO ()
main = do
--  mapM (\n -> putStrLn (show (fib n))) (take 10 (repeat 35))
  MP.mapM (\n -> putStrLn (show (fib n))) (take 10 (repeat 35))
  return ()

