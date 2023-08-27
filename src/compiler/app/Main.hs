module Main where

import           Compiler
import           Control.Monad.Except
import           Data.Semigroup       ((<>))
import           Options.Applicative
import           Python.PyRender
import           REPL
import           System.IO            (hPutStrLn, stderr)

data CmdFlags = Interactive
  | NonInteractive {
    optInput    :: Maybe String
    , optOutput :: Maybe String }


interactiveFlag :: Parser CmdFlags
interactiveFlag = flag' Interactive
    ( long "interactive"
     <> short 'i'
     <> help "Start an interpreter instance" )

fileOutput :: Parser (Maybe String)
fileOutput = optional $ strOption
  (  long "output"
  <> short 'o'
  <> metavar "FILENAME"
  <> help "Output Python file" )

fileInput :: Parser (Maybe String)
fileInput = optional $ strOption
  (  long "input"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input Lambda Rho file" )

nonInteractive :: Parser CmdFlags
nonInteractive = NonInteractive <$> fileInput <*> fileOutput

cmdConfig :: Parser CmdFlags
cmdConfig = interactiveFlag <|> nonInteractive

main :: IO ()
main = work =<< execParser opts
  where
    opts = info (cmdConfig <**> helper)
      ( fullDesc
     <> progDesc "A compiler from \\lambda\\rho to Qiskit"
     <> header "compiler" )

getInput :: Maybe String -> IO String
getInput Nothing     = getContents
getInput (Just file) = readFile file

writeOutput :: Maybe String -> String -> IO ()
writeOutput Nothing     s = putStrLn s
writeOutput (Just file) s = writeFile file (s++"\n")

work :: CmdFlags -> IO ()
work Interactive = repl
work (NonInteractive {optInput=optInput, optOutput=optOutput}) = do
  inp <- getInput optInput
  let result = (pyRenderStr.snd) <$> compile inp
  let showError e = hPutStrLn stderr $ "Error while compiling:\n" ++ e
  either showError (writeOutput optOutput) (runExcept result)
