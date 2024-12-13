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
  LabReport info ->
    T.concat
      [ T.pack "<div align=\"center\">\n",
        T.pack "<h3>",
        escapeHtml (university info),
        T.pack "</h3>\n",
        T.pack "<div style=\"margin-top: 50px;\">\n",
        T.pack "<h2><b>",
        escapeHtml (labName info),
        T.pack "</b></h2>\n",
        T.pack "</div>\n",
        T.pack "<h3>по дисциплине\n\"",
        escapeHtml (discipline info),
        T.pack "\"\n</h3>\n",
        T.pack "</div>\n\n",
        T.pack "<div align=\"right\" style=\"margin-top: 50px;\">\n",
        T.pack "    <b>Выполнили студенты группы ",
        escapeHtml (groupName info),
        T.pack ":</b>\n",
        T.pack "    <ul style=\"list-style-type: none; padding-left: 0; margin: 0;\">\n",
        renderStudentsList (students info),
        T.pack "    </ul>\n",
        T.pack "    <div style=\"margin-top: 20px;\">\n",
        T.pack "    <b>Преподаватель:</b>\n",
        T.pack "    ",
        escapeHtml (teacher info),
        T.pack "\n",
        T.pack "    </div>\n",
        T.pack "</div>\n\n",
        T.pack "<div align=\"center\" style=\"margin-top: 200px; text: center\">\n",
        T.pack "г. ",
        escapeHtml (city info),
        T.pack "\n\n",
        escapeHtml (year info),
        T.pack " г.\n",
        T.pack "</div>\n\n",
        T.pack "<div style=\"page-break-after: always;\"></div>\n"
      ]
    where
      renderStudentsList :: [Text] -> Text
      renderStudentsList =
        T.concat
          . map
            ( \student ->
                T.concat [T.pack "        <li>", escapeHtml student, T.pack "</li>\n"]
            )
  Image alt url title ->
    let titleAttr =
          if T.null title
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
        T.concat (map (renderTableRow alignments) rows),
        T.pack "  </tbody>\n",
        T.pack "</table>\n"
      ]

renderListItem :: MDElement -> Text
renderListItem = renderElement

titleAttr :: Text -> Text
titleAttr title = if T.null title then T.pack "" else T.concat [T.pack " title=\"", title, T.pack "\""]

escapeHtml :: Text -> Text
escapeHtml =
  T.replace (T.pack "<") (T.pack "&lt;")
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

renderTableRow :: [TableAlignment] -> [MDElement] -> Text
renderTableRow alignments cells =
  T.concat
    [ T.pack "    <tr>\n",
      T.concat (zipWith renderTableCell alignments cells),
      T.pack "    </tr>\n"
    ]

renderTableCell :: TableAlignment -> MDElement -> Text
renderTableCell alignment cell =
  let alignStyle = case alignment of
        AlignLeft -> " style=\"text-align: left\""
        AlignCenter -> " style=\"text-align: center\""
        AlignRight -> " style=\"text-align: right\""
        AlignDefault -> ""
   in T.concat
        [ T.pack "      <td",
          T.pack alignStyle,
          T.pack ">",
          renderElement cell,
          T.pack "</td>\n"
        ]
