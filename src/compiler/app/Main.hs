module Main where
import           Grammar

main :: IO ()
main = do
    s <- getLine
    let ast = parseLambdaRho s
    print ast
    main
