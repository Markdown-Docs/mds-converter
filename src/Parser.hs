module Parser (parseMarkdown, isTableLine) where

import Crypto.Hash (Digest, MD5, hash)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (groupBy)
import Data.Text (Text, break)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Types
  ( MDElement (..),
    TableAlignment (..),
  )

data ListContext = ListContext
  { indentLevel :: Int,
    content :: Text,
    listType :: ListType,
    children :: [MDElement]
  }
  deriving (Show, Eq)

data ListType = Ordered | Unordered deriving (Show, Eq)

isTableLine :: Text -> Bool
isTableLine line =
  let trimmed = T.strip line
   in T.isPrefixOf (T.pack "|") trimmed
        && T.isSuffixOf (T.pack "|") trimmed
        && T.count (T.pack "|") trimmed > 1

identifyTable :: [Text] -> Maybe ([Text], [Text])
identifyTable lines@(_ : _ : _) =
  let (potentialTableLines, rest) = span isTableLine lines
   in if length potentialTableLines >= 3
        && isTableHeaderSeparator (potentialTableLines !! 1)
        then
          Just
            ( take 3 potentialTableLines
                ++ takeWhile isTableLine (drop 3 potentialTableLines),
              rest
            )
        else Nothing
  where
    isTableHeaderSeparator line =
      let trimmed = T.strip line
       in T.all (\c -> c `elem` ['-', ':', '|']) trimmed
            && T.count (T.pack "|") trimmed > 1

parseTableAlignmentLine :: Text -> [TableAlignment]
parseTableAlignmentLine line =
  let cells = T.splitOn (T.pack "|") line
      cleanCells = filter (not . T.null) $ map T.strip cells
   in map parseAlignment cleanCells
  where
    parseAlignment cell
      | T.isPrefixOf (T.pack ":-:") cell = AlignCenter
      | T.isSuffixOf (T.pack "-:") cell = AlignRight
      | T.isPrefixOf (T.pack ":-") cell = AlignLeft
      | otherwise = AlignDefault

extractTableRows :: [Text] -> Maybe ([Text], [Text])
extractTableRows lines@(_ : _ : _) =
  let headerLine = head lines
      alignmentLine = lines !! 1
      dataLines = drop 2 lines
      isValidTable =
        isTableLine headerLine
          && isTableLine alignmentLine
          && all isTableLine (take (length dataLines) dataLines)
   in if isValidTable
        then Just (lines, [])
        else Nothing
extractTableRows _ = Nothing

parseTable :: [Text] -> (MDElement, [Text])
parseTable lines =
  let headerLine = head lines
      alignmentLine = lines !! 1
      dataLines = drop 2 lines

      headers =
        map parseInlineCellContent $
          filter (not . T.null) $
            tail $
              init $
                T.splitOn (T.pack "|") (T.strip headerLine)
      alignments = parseTableAlignmentLine alignmentLine

      rows = map parseTableRow $ take (length dataLines) $ dropWhile (not . isTableLine) dataLines

      remainingLines = drop (length rows + 2) lines
   in (Table headers alignments rows, remainingLines)
  where
    parseTableRow line =
      map parseInlineCellContent $
        filter (not . T.null) $
          tail $
            init $
              T.splitOn (T.pack "|") (T.strip line)

    parseInlineCellContent cell =
      let trimmedCell = T.strip cell
       in if T.null trimmedCell
            then PlainText T.empty
            else case parseInline trimmedCell of
              [] -> PlainText T.empty
              [x] -> x -- If only one element, return it directly
              xs -> Paragraph xs -- If multiple elements, wrap in Paragraph

parseMarkdown :: [Text] -> [MDElement]
parseMarkdown = parseLines [] . skipEmptyLines

parseLines :: [Text] -> [Text] -> [MDElement]
parseLines acc [] = processBlock (reverse acc)
parseLines acc (line : lines)
  | isTableLine line =
      case identifyTable (line : lines) of
        Just (tableLines, rest) ->
          let (parsedTable, _) = parseTable tableLines
           in processBlock (reverse acc) ++ [parsedTable] ++ parseLines [] rest
        Nothing -> parseLines (line : acc) lines
  | T.null line = processBlock (reverse acc) ++ parseLines [] (skipEmptyLines lines)
  | isListLine line =
      let (listItems, rest) = extractListItems (line : lines)
          parsedLists = parseNestedLists listItems
       in processBlock (reverse acc) ++ parsedLists ++ parseLines [] rest
  | isHeaderLine line = processBlock (reverse acc) ++ [parseHeader line] ++ parseLines [] lines
  | isHorizontalRule line = processBlock (reverse acc) ++ [HorizontalRule] ++ parseLines [] lines
  | otherwise = case parseUnderlineHeader (line : lines) of
      Just (header, rest) -> processBlock (reverse acc) ++ [header] ++ parseLines [] rest
      Nothing -> parseLines (line : acc) lines

-- Helper function to get level of nesting based on indentation
getIndentLevel :: Text -> Int
getIndentLevel line =
  let spaces = T.length $ T.takeWhile isSpace line
   in spaces `div` 4 + T.length (T.takeWhile (== '\t') line)

-- Check if the line is a list item (ordered or unordered)
isListLine :: Text -> Bool
isListLine line =
  let stripped = T.stripStart line
   in isOrderedListLine stripped || isUnorderedListLine stripped

-- Check if the line is an ordered list item
isOrderedListLine :: Text -> Bool
isOrderedListLine line =
  case T.uncons line of
    Just (c, rest) | isDigit c ->
      case T.stripPrefix (T.takeWhile isDigit line) line of
        Just rest' -> T.stripPrefix (T.pack ". ") rest' /= Nothing
        Nothing -> False
    _ -> False

-- Check if the line is an unordered list item
isUnorderedListLine :: Text -> Bool
isUnorderedListLine line =
  case T.uncons line of
    Just (c, rest)
      | c `elem` ['*', '-', '+'] ->
          T.stripPrefix (T.singleton c) line == Just (T.cons ' ' (T.stripStart rest))
    _ -> False

-- Extract list items, considering indentation for nested lists
extractListItems :: [Text] -> ([Text], [Text])
extractListItems = span (\line -> isListLine line || (not (T.null line) && getIndentLevel line > 0))

parseListContext :: Text -> ListContext
parseListContext line =
  let level = getIndentLevel line
      cleaned = T.stripStart line
      listType = if isOrderedListLine cleaned then Ordered else Unordered
      -- Extract content after the list marker
      content = T.strip $ T.dropWhile (\c -> isDigit c || c `elem` ['*', '-', '+', '.', ' ']) cleaned
      -- Parse the content for inline markdown
      parsedContent = parseInline content
   in ListContext level content listType []

groupByTopLevel :: [ListContext] -> [[ListContext]]
groupByTopLevel [] = []
groupByTopLevel (x : xs) =
  let (group, rest) = span (\item -> indentLevel item >= indentLevel x) xs
      nestedGroups = groupListsByLevel (x : group)
   in nestedGroups : groupByTopLevel rest

groupListsByLevel :: [ListContext] -> [ListContext]
groupListsByLevel [] = []
groupListsByLevel (x : xs) =
  let currentLevel = indentLevel x
      (children, rest) = span (\item -> indentLevel item > currentLevel) xs
      nestedChildren = ([constructListElement (groupListsByLevel children) | not (null children)])
      updatedContext = x {children = nestedChildren}
   in updatedContext : groupListsByLevel rest

constructListElement :: [ListContext] -> MDElement
constructListElement [] = UnorderedList [] -- Fallback
constructListElement contexts@(x : _) =
  let listItems = map makeListItem contexts
   in case listType x of
        Ordered -> OrderedList listItems
        Unordered -> UnorderedList listItems
  where
    makeListItem ctx =
      -- Parse the content for inline markdown instead of wrapping in PlainText
      ListItem (processInlineContent $ content ctx) (children ctx)

-- Helper function to process inline content for list items
processInlineContent :: Text -> MDElement
processInlineContent text =
  case parseInline text of
    [single] -> single
    multiple -> Paragraph multiple

-- Parse nested lists with different markers
parseNestedLists :: [Text] -> [MDElement]
parseNestedLists items =
  let contexts = map parseListContext items
      grouped = groupByTopLevel contexts
   in map constructListElement grouped
  where
    parseListContext line =
      let level = getIndentLevel line
          cleaned = T.stripStart line
          listType = if isOrderedListLine cleaned then Ordered else Unordered
          content = T.strip $ T.dropWhile (\c -> isDigit c || c `elem` ['*', '-', '+', '.', ' ']) cleaned
       in ListContext level content listType []

-- Group and parse list items
parseListGroups :: [Text] -> ([MDElement], [MDElement])
parseListGroups items =
  let groupedLists = groupListItems items
      parsedOrdered = map parseOrderedList (filter isOrderedListGroup groupedLists)
      parsedUnordered = map parseUnorderedList (filter isUnorderedListGroup groupedLists)
   in (parsedOrdered, parsedUnordered)

-- Group list items by their indentation and marker type
groupListItems :: [Text] -> [[Text]]
groupListItems [] = []
groupListItems (x : xs) =
  let (currentGroup, rest) = span (isListWithSameMarker x) (x : xs)
   in currentGroup : groupListItems rest

isListWithSameMarker :: Text -> Text -> Bool
isListWithSameMarker firstItem item =
  (isOrderedListLine firstItem && isOrderedListLine item)
    || (isUnorderedListLine firstItem && isUnorderedListLine item)

-- Get the indentation level of a list item
getListIndent :: Text -> Int
getListIndent line = T.length (T.takeWhile isSpace line)

-- Check if a group is an ordered list group
isOrderedListGroup :: [Text] -> Bool
isOrderedListGroup (x : _) = isOrderedListLine x
isOrderedListGroup _ = False

-- Check if a group is an unordered list group
isUnorderedListGroup :: [Text] -> Bool
isUnorderedListGroup (x : _) = isUnorderedListLine x
isUnorderedListGroup _ = False

-- Parse an ordered list, supporting nested structures
parseOrderedList :: [Text] -> MDElement
parseOrderedList items =
  OrderedList $ map parseListItem items

-- Parse an unordered list, supporting nested structures
parseUnorderedList :: [Text] -> MDElement
parseUnorderedList items =
  UnorderedList $ map parseListItem items

-- Parse a single list item, handling nested content
parseListItem :: Text -> MDElement
parseListItem line =
  let cleanedLine = T.strip $ T.dropWhile (\c -> isDigit c || c `elem` ['*', '-', '+'] || isSpace c) line
   in ListItem (PlainText cleanedLine) []

skipEmptyLines :: [Text] -> [Text]
skipEmptyLines = dropWhile T.null

isHeaderLine :: Text -> Bool
isHeaderLine line = not (T.null line) && T.head line == '#'

isHorizontalRule :: Text -> Bool
isHorizontalRule line =
  let trimmed = T.strip line
   in T.length trimmed >= 3
        && ( T.all (== '*') trimmed
               || T.all (== '-') trimmed
               || T.all (== '_') trimmed
           )

parseHeader :: Text -> MDElement
parseHeader line =
  let level = min 6 $ T.length $ T.takeWhile (== '#') line
      text = T.strip $ T.dropWhile (== '#') line
      cleanText = T.strip $ T.takeWhile (/= '#') text
      headerId = makeHeaderId cleanText
      mdElementsText = parseInline cleanText
   in Header level mdElementsText headerId

parseUnderlineHeader :: [Text] -> Maybe (MDElement, [Text])
parseUnderlineHeader (line1 : line2 : rest)
  | not (T.null line1) && not (T.null line2) && T.all (== '=') (T.strip line2) =
      let cleanText = T.strip line1
          headerId = makeHeaderId cleanText
          mdElementsText = parseInline cleanText
       in Just (Header 1 mdElementsText headerId, rest)
  | not (T.null line1) && not (T.null line2) && T.all (== '-') (T.strip line2) =
      let cleanText = T.strip line1
          headerId = makeHeaderId cleanText
          mdElementsText = parseInline cleanText
       in Just (Header 2 mdElementsText headerId, rest)
  | otherwise = Nothing
parseUnderlineHeader _ = Nothing

makeHeaderId :: Text -> Text
makeHeaderId = generateHashId

generateHashId :: Text -> Text
generateHashId text =
  let hashed = hash (encodeUtf8 text) :: Digest MD5
   in T.pack (show hashed)

processBlock :: [Text] -> [MDElement]
processBlock [] = []
processBlock lines =
  [Paragraph (concatMap processLineForParagraph $ addSpaceBetweenLines lines)]
  where
    addSpaceBetweenLines :: [Text] -> [Text]
    addSpaceBetweenLines [] = []
    addSpaceBetweenLines [x] = [x]
    addSpaceBetweenLines (x : xs) = x : map (T.append (T.singleton ' ')) xs

processLineForParagraph :: Text -> [MDElement]
processLineForParagraph line = case processLine line of
  [] -> []
  elems -> concatMap processElement elems

processElement :: MDElement -> [MDElement]
processElement (PlainText t) = parseInline t
processElement LineBreak = [LineBreak]
processElement other = [other]

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
        Just ('[', _) -> parseLinkOrCheckbox text
        Just ('*', _) -> parseDecoration '*' text
        Just ('_', _) -> parseDecoration '_' text
        Just ('~', _) -> parseStrikethrough text
        Just ('<', _) ->
          if T.isPrefixOf (T.pack "<u>") text || T.isPrefixOf (T.pack "</u>") text
            then parseHtmlTags text
            else parseAngleBracketLink text
        _ -> parsePlainText text

parseLinkOrCheckbox :: Text -> [MDElement]
parseLinkOrCheckbox text
  | T.isPrefixOf (T.pack "[x]") text || T.isPrefixOf (T.pack "[ ]") text =
      parseCheckbox text
  | T.isPrefixOf (T.pack "[") text =
      parseBracketLink text
  | otherwise = parsePlainText text

parseBracketLink :: Text -> [MDElement]
parseBracketLink text =
  let (linkText, afterText) = parseLinkText text
      (url, title, remaining) = parseLinkUrlAndTitle afterText
   in case parseInline linkText of
        [] -> Link url url Nothing : parseInline remaining
        [PlainText t] -> Link t url title : parseInline remaining
        multipleElems ->
          let renderedText = T.concat $ map renderPlainText multipleElems
           in Link renderedText url title : parseInline remaining
  where
    -- Parse the link text between square brackets
    parseLinkText :: Text -> (Text, Text)
    parseLinkText t =
      let linkTextStart = T.tail $ T.takeWhile (/= ']') t
          remaining = T.drop (T.length linkTextStart + 2) t
       in (linkTextStart, remaining)

    -- Parse URL and optional title
    parseLinkUrlAndTitle :: Text -> (Text, Maybe Text, Text)
    parseLinkUrlAndTitle t
      | T.isPrefixOf (T.pack "(") t =
          let urlAndRest = T.tail t
              (rawUrl, afterUrl) = T.break (\c -> c == ')' || c == '"') urlAndRest
              cleanUrl = T.strip rawUrl
           in case T.uncons afterUrl of
                Just ('"', titleRest) ->
                  let (rawTitle, afterTitle) = T.break (== '"') (T.tail titleRest)
                      cleanTitle = unescapeTitle rawTitle
                      finalRest = T.drop (T.length rawTitle + 2) afterTitle
                   in (cleanUrl, Just cleanTitle, finalRest)
                Just (')', rest) -> (cleanUrl, Nothing, rest)
                _ -> (cleanUrl, Nothing, T.empty)
      | otherwise = (T.pack "", Nothing, t)

    -- Unescape title, particularly handling &quot;
    unescapeTitle :: Text -> Text
    unescapeTitle =
      T.replace (T.pack "&quot;") (T.pack "\"")
        . T.replace (T.pack "&amp;") (T.pack "&")
        . T.replace (T.pack "&lt;") (T.pack "<")
        . T.replace (T.pack "&gt;") (T.pack ">")

parseAngleBracketLink :: Text -> [MDElement]
parseAngleBracketLink text
  | T.isPrefixOf (T.pack "<") text && T.isSuffixOf (T.pack ">") text =
      let link = T.init $ T.tail text
          cleanLink = T.strip link
       in Link cleanLink cleanLink Nothing : parseInline (T.drop (T.length link + 2) text)
  | otherwise = [PlainText text]

renderPlainText :: MDElement -> Text
renderPlainText (PlainText t) = t
renderPlainText (Bold t) = t
renderPlainText (Italic t) = t
renderPlainText (BoldItalic t) = t
renderPlainText (Paragraph elems) = T.concat $ map renderPlainText elems
renderPlainText _ = T.empty

parseCheckbox :: Text -> [MDElement]
parseCheckbox text
  | T.isPrefixOf (T.pack "[x]") text || T.isPrefixOf (T.pack "[X]") text =
      let content = T.strip $ T.drop 3 text
          parsedContent = parseInline content
       in [Checkbox True parsedContent]
  | T.isPrefixOf (T.pack "[ ]") text =
      let content = T.strip $ T.drop 3 text
          parsedContent = parseInline content
       in [Checkbox True parsedContent]
  | otherwise = parsePlainText text

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
