module Main (main) where

import qualified Control.Arrow as A
import Control.Monad
import Control.Exception
import Options.Applicative
import Parser
import Reduce
import Equality
import Ast

data Options = Options
  { oIncludes :: [FilePath]
  , oMode :: Mode
  }

data Mode
  = Reduce (Term String)

termReader :: ReadM (Term String)
termReader = eitherReader $ A.left show . parseExpr "<cmdline>"

main :: IO ()
main = work =<< execParser opts
  where
    opts = info (helper <*> optsParser)
      ( fullDesc <> header "Lambda equivalence prover" )
    optsParser =
      Options
        <$> many (strOption (long "include" <> short 'i' <> metavar "FILE"))
        <*> modeParser
    modeParser =
      Reduce <$> option termReader (long "reduce" <> metavar "TERM")

work :: Options -> IO ()
work opts = do
  decls <- liftM concat $ mapM (either (throwIO . ErrorCall . show) return <=< parseDeclsFile) (oIncludes opts)
  case oMode opts of
    Reduce term ->
      case nf (lookupFromDecls decls) 10 term of
        Just term' -> print term'
        Nothing -> putStrLn "No reduced form found"
