module Main where
import           Grammar
import           Tokens

main :: IO ()
main = do
    s <- getLine
    let ast = parseLambdaRho (scanTokens s)
    print ast
    main
