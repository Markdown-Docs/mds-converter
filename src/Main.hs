module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Markdown
import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-c"] -> do
      content <- TIO.getContents
      let html = markdownToHtml content
      TIO.putStrLn html
    [input, output] -> do
      content <- TIO.readFile input
      let html = markdownToHtml content
      TIO.writeFile output html
    _ -> putStrLn "Usage: program <input> <output> OR program -c"
