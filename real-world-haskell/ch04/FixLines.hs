import System.Environment (getArgs)
import SplitLines (splitLines)

-- InteractWith.hs からcopyしてきたものに手を加える.
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        myFunction = fixLines -- 元id.

fixLines :: String -> String
fixLines input = unlines (splitLines input)
