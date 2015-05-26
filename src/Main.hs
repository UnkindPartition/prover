module Main (main) where

import Prelude hiding (mapM, concat)
import qualified Control.Arrow as A
import Control.Monad hiding (mapM)
import Control.Exception
import Options.Applicative
import Data.Tuple.Homogenous
import Data.Foldable
import Data.Traversable
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
  | Equal (Tuple2 (Term String))

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
    modeParser = asum
      [ Reduce <$> option termReader (long "reduce" <> metavar "TERM")
      , Equal <$ flag' () (long "equal")
        <*> (sequenceA . pure $ argument termReader (metavar "TERM"))
      ]

work :: Options -> IO ()
work opts = do
  decls <- liftM concat $ mapM (either (throwIO . ErrorCall . show) return <=< parseDeclsFile) (oIncludes opts)
  let lkp = lookupFromDecls decls
  case oMode opts of
    Reduce term ->
      case nf lkp 10 term of
        Just term' -> print term'
        Nothing -> putStrLn "No reduced form found"
    Equal terms -> print $ equal lkp 10 terms
