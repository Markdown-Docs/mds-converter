module Markdown
  ( markdownToHtml,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Parser (parseMarkdown)
import Types

markdownToHtml :: Text -> Text
markdownToHtml input = T.concat $ map renderElement $ Parser.parseMarkdown $ T.lines input

renderElement :: MDElement -> Text
renderElement element = case element of
  Image alt url title ->
    let titleAttr = if T.null title 
          then T.pack "" 
          else T.concat [T.pack " title=\"", escapeHtml title, T.pack "\""]
     in T.concat
          [ T.pack "<img src=\"",
            escapeHtml url,
            T.pack "\" alt=\"",
            escapeHtml alt,
            titleAttr,
            T.pack "\" />"
          ]
  Link text url title ->
    let titleAttr = case title of
          Just t -> T.concat [T.pack " title=\"", escapeHtml t, T.pack "\""]
          Nothing -> T.pack ""
     in T.concat
          [ T.pack "<a href=\"",
            escapeHtml url,
            T.pack "\"",
            titleAttr,
            T.pack ">",
            escapeHtml text,
            T.pack "</a>"
          ]
  Paragraph elements -> T.concat [T.pack "<p>", T.concat (map renderElement elements), T.pack "</p>\n"]
  LineBreak -> T.pack "<br />\n"
  Header level text id ->
    T.concat
      [T.pack "<h", T.pack (show level), T.pack " id=\"", id, T.pack "\">", T.concat (map renderListItem text), T.pack "</h", T.pack (show level), T.pack ">\n"]
  Bold text -> T.concat [T.pack "<strong>", escapeHtml text, T.pack "</strong>"]
  Italic text -> T.concat [T.pack "<em>", escapeHtml text, T.pack "</em>"]
  BoldItalic text -> T.concat [T.pack "<strong><em>", escapeHtml text, T.pack "</em></strong>"]
  Strikethrough text -> T.concat [T.pack "<s>", escapeHtml text, T.pack "</s>"]
  Underlined text -> T.concat [T.pack "<u>", escapeHtml text, T.pack "</u>"]
  HorizontalRule -> T.pack "<hr />\n"
  UnorderedList items -> T.concat [T.pack "<ul>\n", T.concat (map renderListItem items), T.pack "</ul>\n"]
  OrderedList items -> T.concat [T.pack "<ol>\n", T.concat (map renderListItem items), T.pack "</ol>\n"]
  ListItem mainContent children ->
    let mainContentHtml = renderElement mainContent
        childrenHtml = case children of
          [] -> T.pack ""
          _ -> T.concat [T.pack "<ul>", T.concat (map renderElement children), T.pack "</ul>"]
     in T.concat [T.pack "<li>", mainContentHtml, childrenHtml, T.pack "</li>\n"]
  CodeBlock text -> T.concat [T.pack "<pre><code>", escapeHtml text, T.pack "</code></pre>\n"]
  InlineCode text -> T.concat [T.pack "<code>", escapeHtml text, T.pack "</code>"]
  PlainText text -> escapeHtml text
  Checkbox checked content ->
    T.concat
      [ T.pack "<input class=\"task-list-item-checkbox\" ",
        if checked then T.pack "checked=\"\" " else T.pack "",
        T.pack "type=\"checkbox\" />",
        T.pack " ",
        T.concat (map renderListItem content)
      ]
  Table headers alignments rows ->
    T.concat
      [ T.pack "<table>\n",
        T.pack "  <thead>\n",
        T.pack "    <tr>\n",
        T.concat (zipWith renderTableHeader headers alignments),
        T.pack "    </tr>\n",
        T.pack "  </thead>\n",
        T.pack "  <tbody>\n",
        T.concat (map renderTableRow rows),
        T.pack "  </tbody>\n",
        T.pack "</table>\n"
      ]

renderListItem :: MDElement -> Text
renderListItem = renderElement

titleAttr :: Text -> Text
titleAttr title = if T.null title then T.pack "" else T.concat [T.pack " title=\"", title, T.pack "\""]

escapeHtml :: Text -> Text
escapeHtml =
  T.replace (T.pack "&") (T.pack "&amp;")
    . T.replace (T.pack "<") (T.pack "&lt;")
    . T.replace (T.pack ">") (T.pack "&gt;")
    . T.replace (T.pack "\"") (T.pack "&quot;")

renderTableHeader :: MDElement -> TableAlignment -> Text
renderTableHeader header alignment =
  let alignStyle = case alignment of
        AlignLeft -> " style=\"text-align: left\""
        AlignCenter -> " style=\"text-align: center\""
        AlignRight -> " style=\"text-align: right\""
        AlignDefault -> ""
   in T.concat
        [ T.pack "      <th",
          T.pack alignStyle,
          T.pack ">",
          renderElement header,
          T.pack "</th>\n"
        ]

renderTableRow :: [MDElement] -> Text
renderTableRow cells =
  T.concat
    [ T.pack "    <tr>\n",
      T.concat (map renderTableCell cells),
      T.pack "    </tr>\n"
    ]

renderTableCell :: MDElement -> Text
renderTableCell cell =
  T.concat
    [ T.pack "      <td>",
      renderElement cell,
      T.pack "</td>\n"
    ]
