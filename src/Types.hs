module Types
  ( MDElement (..),
    TableAlignment (..),
  )
where

import Data.Text (Text)

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
  | HorizontalRule
  | Link Text Text
  | Image Text Text Text
  | UnorderedList [MDElement]
  | OrderedList [MDElement]
  | ListItem MDElement [MDElement] -- New type to support nested lists
  | CodeBlock Text
  | InlineCode Text
  | Checkbox Bool [MDElement]
  | Table
      { headers :: [MDElement],
        alignments :: [TableAlignment],
        rows :: [[MDElement]]
      }
  deriving (Show, Eq)
