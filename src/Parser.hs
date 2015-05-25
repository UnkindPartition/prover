{-# LANGUAGE LambdaCase #-}
module Parser (parseExpr, parseDecl, parseDeclsFile) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import Text.Parsec.Pos
import qualified Data.Loc as Loc
import Data.Foldable (asum)
import Control.Applicative hiding ((<|>))
import Bound
import Lexer
import Ast

----------------------------------------------------------------------
--                           Entry points
----------------------------------------------------------------------

parseExpr :: String -> String -> Either ParseError (Term String)
parseExpr fname = P.parse (pTerm <* eof) fname . Lexer.lex fname

parseDecl :: String -> String -> Either ParseError Decl
parseDecl fname = P.parse (pDecl <* eof) fname . Lexer.lex fname

parseDeclsFile :: FilePath -> IO (Either ParseError [Decl])
parseDeclsFile fname = P.parse (P.many pDecl <* eof) fname . Lexer.lex fname <$> readFile fname

----------------------------------------------------------------------
--                           Declarations
----------------------------------------------------------------------

pDecl :: P Decl
pDecl =
  Decl <$> pName <* the (Sym '=') <*> pTerm <* the (Sym ';')

----------------------------------------------------------------------
--                           Expressions
----------------------------------------------------------------------

pName :: P String
pName = tok $ \case
  Lexer.Var n -> Just n 
  _ -> Nothing

the :: Token -> P ()
the t = tok $ \case
  t' | t == t' -> Just ()
  _ -> Nothing

paren :: P a -> P a
paren p = the (Sym '(') *> p <* the (Sym ')')

optParen :: P a -> P a
optParen p = paren p <|> p

pLambda :: P (Term String)
pLambda = do
  the (Sym '\\')
  vars <- P.many pName
  the (Sym '.')
  body <- pTerm
  return $ foldr (\name body_ -> Lam $ abstract1 name body_) body vars

pTerm1 :: P (Term String)
pTerm1 = asum
  [ Ast.Var <$> pName
  , pLambda
  , paren pTerm
  ]

pTerm :: P (Term String)
pTerm = Prelude.foldl1 App <$> some pTerm1

----------------------------------------------------------------------
--                           Aux definitions
----------------------------------------------------------------------
type P = Parsec [Loc.L Token] ()

locToSourcePos :: Loc.Loc -> SourcePos
locToSourcePos loc =
  let Loc.Loc (Loc.Pos name line col _) _ = Loc.locStart loc
  in newPos name line col

tok :: (Token -> Maybe a) -> P a
tok f = token show (locToSourcePos . Loc.locOf) (f . Loc.unLoc)
