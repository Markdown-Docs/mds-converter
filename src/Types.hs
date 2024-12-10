module Types
  ( MDElement (..),
  )
where

import Data.Text (Text)

data MDElement
  = Paragraph [MDElement]
  | PlainText Text
  | Header Int Text Text
  | Bold Text
  | Italic Text
  | BoldItalic Text
  | Strikethrough Text
  | Underlined Text
  | LineBreak
  | HorizontalRule
  | Link Text Text
  | Image Text Text Text
  | UnorderedList [Text]
  | OrderedList [Text]
  | BlockQuote Text
  | CodeBlock Text
  | InlineCode Text
  deriving (Show, Eq)
