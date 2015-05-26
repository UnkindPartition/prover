module Lexer (Token(..), Lexer.lex) where

import Language.Lexer.Applicative
import Text.Regex.Applicative
import Data.Foldable
import Data.Char
import Data.Loc
import qualified Data.HashSet as HS
import Control.Monad (void)

data Token
  = Forall
  | Var String
  | Sym Char
  deriving (Eq, Show)

pToken :: RE Char Token
pToken = asum
  [ Forall <$  string "forall"
  , Var    <$> (some (psym (\c -> isAlphaNum c && c /= 'λ' || c == '_')))
  , Sym    <$> psym (flip HS.member $ HS.fromList ['\\', 'λ', '.', '(', ')', ':', '=', ';'])
  ]

pSpace :: RE Char ()
pSpace = asum
  [ void $ some $ psym isSpace
  , void $ string "--" *> many (psym (not . (== nl))) *> sym nl
  ]
  where nl = '\n'

lex :: String -> String -> [L Token]
lex = tokens pToken pSpace
