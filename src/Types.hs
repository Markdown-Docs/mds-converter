module Types
  ( MDElement (..),
    TableAlignment (..),
    LabReportInfo(..),
  )
where

import Data.Text (Text)

data LabReportInfo = LabReportInfo
  { university :: Text
  , labName :: Text
  , discipline :: Text
  , groupName :: Text
  , students :: [Text]
  , teacher :: Text
  , city :: Text
  , year :: Text
  } deriving (Show, Eq)

data TableAlignment
  = AlignLeft
  | AlignCenter
  | AlignRight
  | AlignDefault
  deriving (Show, Eq)

data MDElement
  = Paragraph [MDElement]
  | PlainText Text
  | Header Int [MDElement] Text
  | Bold Text
  | Italic Text
  | BoldItalic Text
  | Strikethrough Text
  | Underlined Text
  | LineBreak
  | LabReport LabReportInfo
  | HorizontalRule
  | Link
      { linkText :: Text,
        linkUrl :: Text,
        linkTitle :: Maybe Text
      }
  | Image Text Text Text
  | UnorderedList [MDElement]
  | OrderedList [MDElement]
  | ListItem MDElement [MDElement]
  | CodeBlock Text
  | InlineCode Text
  | Checkbox Bool [MDElement]
  | Table
      { headers :: [MDElement],
        alignments :: [TableAlignment],
        rows :: [[MDElement]]
      }
  deriving (Show, Eq)
