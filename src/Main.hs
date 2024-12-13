module Main where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Markdown
import Parser
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

main :: IO ()
main = do
  -- Set UTF-8 encoding for stdout
  hSetEncoding stdout utf8

  args <- getArgs
  case args of
    ["-c"] -> do
      content <- TIO.getContents
      let html = T.concat [T.pack "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"UTF-8\">\n</head>\n<body>\n", markdownToHtml content, T.pack "\n</body>\n</html>"]
      TIO.putStrLn html
    [input, output] -> do
      content <- TIO.readFile input
      let html = T.concat [T.pack "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"UTF-8\">\n</head>\n<body>\n", markdownToHtml content, T.pack "\n</body>\n</html>"]
      TIO.writeFile output html
    _ -> putStrLn "Usage: program <input> <output> OR program -c"
