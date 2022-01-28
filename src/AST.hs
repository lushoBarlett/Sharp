module AST where

newtype Identifier = Identifier String
  deriving (Eq, Show, Ord)

newtype Type = Type String
  deriving (Eq, Show, Ord)

data AST

  = Declaration
    { identifierOf :: Identifier
    , typeOf :: Type
    , valueOf :: AST
    }

  | Argument
    { identifierOf :: Identifier
    , typeOf :: Type
    }

  | IntLiteral Int

  | FunctionLiteral
    { argumentsOf :: [AST]
    , bodyOf :: AST
    }

  | FunctionCall
    { functionOf :: AST
    , parametersOf :: [AST]
    }

  | Block [AST]
  
  deriving (Eq, Show)
