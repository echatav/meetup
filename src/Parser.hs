module Parser where

import Expression

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String (Parser)

expressionParser :: Parser Expression
expressionParser = makeExprParser term table where

  term = try (parenthesized expressionParser)
    <|> try (Literal <$> valueParser)

  table =
    [ [ Prefix (UnOp <$> unOpParser) ]
    , [ InfixR (BinOp <$> binOpParser And "and")
      , InfixR (BinOp <$> binOpParser Or "or")
      ]
    ]
    where
      unOpParser = try (Not <$ string "not") <|> (Abs <$ string "abs")
      binOpParser :: BinOp -> String -> Parser BinOp
      binOpParser op str = op <$ (space *> string str <* space)

  parenthesized = between (char '(' <* space) (space *> char ')')

valueParser :: Parser Value
valueParser = try bool <|> try double <|> int
  where
    bool = fmap VBool $
      try (True <$ string "true")
      <|> (False <$ string "false")
    double = VDouble <$> L.signed space L.float
    int = VInt . fromIntegral <$> L.signed space L.integer
