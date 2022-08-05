module Main where

import           Compiler
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Python.PyRender
import           REPL


data CmdFlags = CmdFlags
  { interactive      :: Bool }

cmdConfig :: Parser CmdFlags
cmdConfig = CmdFlags
      <$> switch
          ( long "interactive"
         <> short 'i'
         <> help "Start an interpreter instance" )


main :: IO ()
main = work =<< execParser opts
  where
    opts = info (cmdConfig <**> helper)
      ( fullDesc
     <> progDesc "A compiler from \\lambda\\rho to Qiskit"
     <> header "compiler" )

work :: CmdFlags -> IO ()
work (CmdFlags True) = repl
work _               = do
  inp <- getContents
  print $ (pyRenderStr.snd) <$> compile inp
