module Main where

import System.Environment ( getArgs )
import Control.Monad ( void )
import Data.Maybe ( isNothing )
import System.IO (print)

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  content <- readFile file
  -- TODO: parse file
  return ()