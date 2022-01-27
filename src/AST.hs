module AST where

newtype Identifier = Identifier String
  deriving (Eq, Show)

newtype Type = Type String
  deriving (Eq, Show)

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

  | Block [AST]
  
  deriving (Eq, Show)
