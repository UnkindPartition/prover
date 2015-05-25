module Lexer (Token(..), Lexer.lex) where

import Language.Lexer.Applicative
import Text.Regex.Applicative
import Data.Foldable
import Data.Char
import Data.Loc
import qualified Data.HashSet as HS

data Token
  = Forall
  | Var String
  | Sym Char
  deriving (Eq, Show)

pToken :: RE Char Token
pToken = asum
  [ Forall <$  string "forall"
  , Var    <$> (some (psym (\c -> isAlphaNum c || c == '_')))
  , Sym    <$> psym (flip HS.member $ HS.fromList ['\\', '.', '(', ')', ':', '=', ';'])
  ]

pSpace :: RE Char ()
pSpace = () <$ (some $ psym isSpace)

lex :: String -> String -> [L Token]
lex = tokens pToken pSpace
