module Runtime where

import Data.Map( Map, lookup )
import AST ( AST(..), Identifier (Identifier) )

type Context = Map Identifier AST

newtype RuntimeAction = RuntimeAction (Context -> Context)

run :: AST -> RuntimeAction
run (FunctionCall (FunctionLiteral arguments body) parameters) = run body
run _ = RuntimeAction id

execute :: Context -> Maybe ()
execute context = do
  mainFunction <- Data.Map.lookup (Identifier "main") context
  runtimeAction <- Just $ run $ FunctionCall mainFunction []
  return ()