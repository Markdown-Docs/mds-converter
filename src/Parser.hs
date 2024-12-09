module Parser (parseMarkdown) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Types

parseMarkdown :: [Text] -> [MDElement]
parseMarkdown = parseLines [] . skipEmptyLines

parseLines :: [Text] -> [Text] -> [MDElement]
parseLines acc [] = processBlock (reverse acc)
parseLines acc (line : lines)
  | T.null line = processBlock (reverse acc) ++ parseLines [] (skipEmptyLines lines)
  | isHeaderLine line = processBlock (reverse acc) ++ [parseHeader line] ++ parseLines [] lines
  | isHorizontalRule line = processBlock (reverse acc) ++ [HorizontalRule] ++ parseLines [] lines
  | isCodeBlock line = parseCodeBlock lines
  | otherwise = case parseUnderlineHeader (line : lines) of
      Just (header, rest) -> processBlock (reverse acc) ++ [header] ++ parseLines [] rest
      Nothing -> parseLines (line : acc) lines

skipEmptyLines :: [Text] -> [Text]
skipEmptyLines = dropWhile T.null

isHeaderLine :: Text -> Bool
isHeaderLine line = not (T.null line) && T.head line == '#'

isHorizontalRule :: Text -> Bool
isHorizontalRule line =
  let trimmed = T.strip line
   in T.length trimmed >= 3 && (T.all (== '*') trimmed || T.all (== '-') trimmed)

isCodeBlock :: Text -> Bool
isCodeBlock line = T.isPrefixOf (T.pack "```") line || T.isPrefixOf (T.pack "    ") line

parseHeader :: Text -> MDElement
parseHeader line =
  let level = min 6 $ T.length $ T.takeWhile (== '#') line
      text = T.strip $ T.dropWhile (== '#') line
      cleanText = T.strip $ T.takeWhile (/= '#') text
      headerId = makeHeaderId cleanText
   in Header level cleanText headerId

parseUnderlineHeader :: [Text] -> Maybe (MDElement, [Text])
parseUnderlineHeader (line1 : line2 : rest)
  | not (T.null line1) && not (T.null line2) && T.all (== '=') (T.strip line2) =
      let cleanText = T.strip line1
          headerId = makeHeaderId cleanText
       in Just (Header 1 cleanText headerId, rest)
  | not (T.null line1) && not (T.null line2) && T.all (== '-') (T.strip line2) =
      let cleanText = T.strip line1
          headerId = makeHeaderId cleanText
       in Just (Header 2 cleanText headerId, rest)
  | otherwise = Nothing
parseUnderlineHeader _ = Nothing

makeHeaderId :: Text -> Text
makeHeaderId text =
  let normalized = T.toLower $ T.strip text
      words = T.words normalized
   in T.intercalate (T.pack "-") words

processBlock :: [Text] -> [MDElement]
processBlock [] = []
processBlock lines = [Paragraph (concatMap processLine lines)]

processLines :: Text -> [MDElement]
processLines text = concatMap processLine $ T.splitOn (T.pack "\n") text

processLine :: Text -> [MDElement]
processLine line
  | T.null line = []
  | T.isSuffixOf (T.pack "  ") line = [PlainText (T.dropEnd 2 line), LineBreak]
  | T.isSuffixOf (T.pack "\\") line = [PlainText (T.dropEnd 1 line), LineBreak]
  | otherwise = case T.breakOn (T.pack "<br>") line of
      (before, after)
        | T.null after -> [PlainText line]
        | otherwise -> [PlainText before, LineBreak] ++ processLine (T.drop 4 after)

parseInline :: Text -> MDElement
parseInline text
  | T.isPrefixOf (T.pack "**") rest && T.isSuffixOf (T.pack "**") content = Bold (T.dropEnd 2 content)
  | T.isPrefixOf (T.pack "*") rest && T.isSuffixOf (T.pack "*") content = Italic (T.dropEnd 1 content)
  | otherwise = Paragraph [PlainText text]
  where
    rest = T.drop 2 text
    content = T.drop 2 text

parseCodeBlock :: [Text] -> [MDElement]
parseCodeBlock [] = []
parseCodeBlock (line : lines)
  | T.isPrefixOf (T.pack "```") line =
      let (codeLines, rest) = break (T.isPrefixOf (T.pack "```")) lines
       in CodeBlock (T.unlines codeLines) : parseLines [] (drop 1 $ skipEmptyLines rest)
  | T.isPrefixOf (T.pack "    ") line =
      let (codeLines, rest) = span (T.isPrefixOf (T.pack "    ")) (line : lines)
          code = T.unlines $ map (T.drop 4) codeLines
       in CodeBlock code : parseLines [] (skipEmptyLines rest)
  | otherwise = parseLines [] lines
