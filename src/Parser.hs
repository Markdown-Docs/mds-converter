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
processBlock lines = [Paragraph (concatMap parseInline lines)]

processLine :: Text -> [MDElement]
processLine line
  | T.null line = []
  | T.isSuffixOf (T.pack "  ") line = [PlainText (T.dropEnd 2 line), LineBreak]
  | T.isSuffixOf (T.pack "\\") line = [PlainText (T.dropEnd 1 line), LineBreak]
  | otherwise = case T.breakOn (T.pack "<br>") line of
      (before, after)
        | T.null after -> [PlainText line]
        | otherwise -> [PlainText before, LineBreak] ++ processLine (T.drop 4 after)

-- Parse inline text with nested decorations
parseInline :: Text -> [MDElement]
parseInline text
  | T.null text = []
  | otherwise =
      case T.uncons text of
        Just ('*', _) -> parseDecoration '*' text
        Just ('_', _) -> parseDecoration '_' text
        Just ('~', _) -> parseStrikethrough text
        Just ('<', _) -> parseHtmlTags text
        _ -> parsePlainText text

-- Handle decorations like *, **, *** or _, __, ___
parseDecoration :: Char -> Text -> [MDElement]
parseDecoration char text
  | T.isPrefixOf (T.pack [char, char, char]) text =
      let (content, rest) = T.breakOn (T.pack [char, char, char]) (T.drop 3 text)
       in if T.isPrefixOf (T.pack [char, char, char]) rest
            then BoldItalic (T.strip content) : parseInline (T.drop 3 rest)
            else PlainText (T.pack [char, char, char]) : parseInline (T.drop 3 text)
  | T.isPrefixOf (T.pack [char, char]) text =
      let (content, rest) = T.breakOn (T.pack [char, char]) (T.drop 2 text)
       in if T.isPrefixOf (T.pack [char, char]) rest
            then Bold (T.strip content) : parseInline (T.drop 2 rest)
            else PlainText (T.pack [char, char]) : parseInline (T.drop 2 text)
  | T.isPrefixOf (T.pack [char]) text =
      let (content, rest) = T.breakOn (T.pack [char]) (T.drop 1 text)
       in if T.isPrefixOf (T.pack [char]) rest
            then Italic (T.strip content) : parseInline (T.drop 1 rest)
            else PlainText (T.pack [char]) : parseInline (T.drop 1 text)
  | otherwise = [PlainText text]

-- Handle ~~strikethrough~~
parseStrikethrough :: Text -> [MDElement]
parseStrikethrough text
  | T.isPrefixOf (T.pack "~~") text =
      let (content, rest) = T.breakOn (T.pack "~~") (T.drop 2 text)
       in if T.isPrefixOf (T.pack "~~") rest
            then Strikethrough (T.strip content) : parseInline (T.drop 2 rest)
            else PlainText (T.pack "~~") : parseInline (T.drop 2 text)
  | otherwise = [PlainText text]

-- Parse <u>...</u>
parseHtmlTags :: Text -> [MDElement]
parseHtmlTags text
  | T.isPrefixOf (T.pack "<u>") text =
      let (content, rest) = T.breakOn (T.pack "</u>") (T.drop 3 text)
       in if T.isPrefixOf (T.pack "</u>") rest
            then Underlined (T.strip content) : parseInline (T.drop 4 rest)
            else PlainText (T.pack "<u>") : parseInline (T.drop 3 text)
  | otherwise = [PlainText text]

-- Handle plain text until a special character
parsePlainText :: Text -> [MDElement]
parsePlainText text =
  let (content, rest) = T.break (`elem` ['*', '_', '~', '<']) text
   in PlainText content : parseInline rest
