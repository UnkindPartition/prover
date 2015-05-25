module Main (main) where

import qualified Control.Arrow as A
import Control.Monad
import Options.Applicative
import Parser
import Reduce
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
work opts =
  case oMode opts of
    Reduce term ->
      case nf (const Nothing) 10 term of
        Just term' -> print term'
        Nothing -> putStrLn "No reduced form found"
