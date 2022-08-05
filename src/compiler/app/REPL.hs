module REPL (repl) where

import           Compiler
import           Control.Monad.Except
import           Control.Monad.Trans
import           Data.List              (isPrefixOf)
import           Python.PyRender
import           System.Console.Repline
import           System.Process         (callCommand)
import           Typing.TypeChecker

type Repl a = HaskelineT IO a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd src = liftIO $ putStrLn $ showResult $ pretty <$> compile src
  where
    pretty (typ, trans) = "Type: " ++ show typ ++ "\nTranslation:\n" ++ (pyRenderStr trans)


showResult :: Except String String -> String
showResult = (either ("Error: " ++) id).runExcept

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = ["letcase", "in"]
  return $ filter (isPrefixOf n) names

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

typeExp :: String -> Repl ()
typeExp exp = liftIO $ print $ showResult $ show <$> fst <$> compile exp

opts :: [(String, String -> Repl ())]
opts =
  [ ("help", help . words), -- :help
    ("type", typeExp) -- :type
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome! Write \\lambda\\rho terms below"

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

repl :: IO ()
repl = evalReplOpts $ ReplOpts
  { banner           = customBanner
  , command          = cmd
  , options          = opts
  , prefix           = Just ':'
  , multilineCommand = Just "paste"
  , tabComplete      = (Word0 completer)
  , initialiser      = ini
  , finaliser        = final
  }

customBanner :: MultiLine -> Repl String
customBanner SingleLine = pure ">>> "
customBanner MultiLine  = pure "| "
