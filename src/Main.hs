module Main where

import AST (AST (..))
import Control.Arrow ((>>>))
import Control.Monad (void)
import Data.Map (empty, fold, fromList)
import Data.Maybe (isNothing)
import Parser (Parser (..), parseTopLevel)
import Runtime (Context, execute)
import System.Environment (getArgs)
import System.IO (print)

makeContext :: [AST] -> Context
makeContext = map declare >>> Prelude.foldr (<>) Data.Map.empty
  where
    declare (Declaration identifier _ value) = Data.Map.fromList [(identifier, value)]
    declare _ = Data.Map.empty

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  content <- readFile file

  let context = runParser parseTopLevel content

  case context of
    Just (rest, declarations) -> do
      let context = makeContext declarations
      let result = execute context
      print context
    Nothing -> do
      putStrLn "Error parsing program"