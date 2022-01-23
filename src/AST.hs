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

  | IntLiteral Int
  deriving (Eq, Show)