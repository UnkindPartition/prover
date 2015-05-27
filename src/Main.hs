module Main (main) where

import Prelude hiding (mapM, concat)
import qualified Control.Arrow as A
import Control.Monad hiding (mapM)
import Control.Exception
import Options.Applicative
import Data.Tuple.Homogenous
import Data.Foldable
import Data.Traversable
import Text.PrettyPrint.Mainland as PP
import Parser
import Reduce
import Equality
import Ast

data Options = Options
  { oIncludes :: [FilePath]
  , oFuel :: Int
  , oMode :: Mode
  }

data Mode
  = Reduce (Term String)
  | Equal (Tuple2 (Term String)) (Maybe Int)

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
        <*> option auto (long "fuel" <> value 10 <> metavar "NUM" <> help "Reduction limit (default: 10)")
        <*> modeParser
    modeParser = asum
      [ Reduce <$> option termReader (long "reduce" <> metavar "TERM")
      , Equal <$ flag' () (long "equal")
        <*> (sequenceA . pure $ argument termReader (metavar "TERM"))
        <*> optional (option auto (long "size-limit" <> metavar "NUM" <> help "Do not consider terms that are larger than NUM"))
      ]

work :: Options -> IO ()
work Options{..} = do
  decls <- liftM concat $ mapM (either (throwIO . ErrorCall . show) return <=< parseDeclsFile) oIncludes
  let lkp = lookupFromDecls decls
  case oMode of
    Reduce term ->
      case nf lkp oFuel term of
        Just term' -> print term'
        Nothing -> putStrLn "No reduced form found"
    Equal terms mbLimit ->
      case equal lkp oFuel mbLimit terms of
        Nothing -> putStrLn "Could not prove equality"
        Just reds ->
          let
            Tuple2 (doc1, doc2) =
              (</>) <$>
                (ppr <$> terms) <*>
                (ppr <$> reds)
          in
            case untuple2 reds of
              ([], []) -> putStrLn "Terms are α-equivalent"
              (_,  []) -> print doc1
              ([], _ ) -> print doc2
              _ -> print $ doc1 </> line <> doc2
