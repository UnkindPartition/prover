module Lexer (Token(..), Lexer.lex) where

import Language.Lexer.Applicative
import Text.Regex.Applicative
import Data.Foldable
import Data.Char
import Data.Loc
import Data.Monoid
import qualified Data.HashSet as HS

data Token
  = Forall
  | Var String
  | Sym Char
  deriving (Eq, Show)

lToken :: Lexer Token
lToken = token . longest $ asum
  [ Forall <$  string "forall"
  , Var    <$> (some (psym (\c -> isAlphaNum c && c /= 'λ' || c == '_')))
  , Sym    <$> psym (flip HS.member $ HS.fromList ['\\', 'λ', '.', '(', ')', ':', '=', ';'])
  ]

lSpace :: Lexer Token
lSpace = fold
  [ whitespace $ longest $ some $ psym isSpace
  , whitespace $ longestShortest (string "--") (const $ many anySym *> sym nl)
  ]
  where nl = '\n'

lex :: String -> String -> [L Token]
lex n s = streamToList $ runLexer (lToken <> lSpace) n s
