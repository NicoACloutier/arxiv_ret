module Parser where

import qualified Text.HTML.TagSoup as TagSoup

-- |A paper in the arXiv system.
--  Fields:
--      `title :: String`: The paper's title.
--      `summary :: String`: The paper's abstract.
--      `authors :: [String]`: A list of authors for the paper.
--      `pdfLink :: String`: A link to a PDF of the paper.
--      `link :: String`: A link to the paper itself of arXiv's system.
data Entry = Entry { title :: String,
                     summary :: String,
                     authors :: [String],
                     pdfLink :: String, 
                     link :: String }
    deriving ( Read, Show, Eq )

-- |Remove all newlines from a string.
--  Arguments:
--      `String`: The string to remove newlines from.
--  Returns:
--      `String`: The string without newlines.
removeNewline :: String -> String
removeNewline ( '\n':xs ) = removeNewline ( tail xs )
removeNewline ( x:xs ) = [x] ++ removeNewline xs
removeNewline _ = ""

-- |Return everything in a list of XML tags up until the closing tab of a particular type.
--  Arguments:
--      `String`: The closing tag to terminate on.
--      `[TagSoup.Tag String]`: The XML tags to parse through.
--  Returns:
--      `[TagSoup.Tag String]`: The XML tags up until the terminal tag.
filterTag :: String -> [TagSoup.Tag String] -> [TagSoup.Tag String]
filterTag _ [] = []
filterTag tag ( current:xs ) = if current == TagSoup.TagClose tag
    then []
    else [current] ++ ( filterTag tag xs )

-- |Find the first instance of a tag's contents within an XML structure.
--      NOTE: Does not return opening or closing tag, only contents.
--  Arguments:
--      `String`: The tag to find the contents of.
--      `[TagSoup.Tag String]`: The XML tags to parse through.
--  Returns:
--      `[TagSoup.Tag String]`: The content of the first instance of these tags.
findTag :: String -> [TagSoup.Tag String] -> [TagSoup.Tag String]
findTag _ [] = []
findTag tag ( current:xs ) = if current == TagSoup.TagOpen tag []
    then filterTag tag xs 
    else findTag tag xs

-- |Find the text of all instances of a tag in an XML structure.
--      NOTE: Only works when this tag has no nesting inside of it.
--  Arguments:
--      `String`: The tag to find the contents of.
--      `[TagSoup.Tag String]`: The XML tags to parse through.
--  Returns:
--      `[String]`: An array of the texts within this tag in the XML structure.
findAllTags :: String -> [TagSoup.Tag String] -> [String]
findAllTags _ [] = []
findAllTags tag entry = current ++ findAllTags tag ( tail entry )
    where current = if head entry == TagSoup.TagOpen tag []
                        then map TagSoup.fromTagText ( findTag ( tag ) entry )
                        else []

-- |Find the contents of every <entry>-tagged item in an XML structure.
--  Arguments:
--      `[TagSoup.Tag String]`: The XML tags to parse through.
--  Returns:
--      `[[TagSoup.Tag String]]`: The XML structure split into entries.
findAllEntries :: [TagSoup.Tag String] -> [[TagSoup.Tag String]]
findAllEntries [] = [[]]
findAllEntries entry = [current] ++ findAllEntries ( tail entry )
    where current = if head entry == TagSoup.TagOpen "entry" []
                        then findTag "entry" entry
                        else []

-- |Find a link with a particular type within an XML structure.
--  Arguments:
--      `String`: The link type.
--      `[TagSoup.Tag String]`: The XML tags to parse through.
--  Returns:
--      `String`: The first link that matches this type.
findLink :: String -> [TagSoup.Tag String] -> String
findLink linkType ( x:xs ) = if ( TagSoup.isTagOpen x && 
                                  possible /= "" && 
                                  TagSoup.fromAttrib "type" x == linkType )
                                then possible
                                else findLink linkType xs
                                    where possible = TagSoup.fromAttrib "href" x
findLink _ _ = ""

-- |Make an entry given only its internal structure.
--  Arguments:
--      `[TagSoup.Tag String]`: The XML tags within this entry.
--  Returns:
--      `Entry`: The parsed entry with paper information.
makeEntry :: [TagSoup.Tag String] -> Entry
makeEntry entryXml = Entry entryTitle entrySummary entryAuthors entryPdfLink entryLink
    where
        entryTitle = removeNewline ( TagSoup.fromTagText ( head ( findTag "title" entryXml ) ) )
        entrySummary = removeNewline ( TagSoup.fromTagText ( head ( findTag "summary" entryXml ) ) )
        entryAuthors = map removeNewline ( findAllTags "name" entryXml )
        entryPdfLink = removeNewline ( findLink "application/pdf" entryXml )
        entryLink = removeNewline ( findLink "text/html" entryXml )

-- |Parse an XML file, returning all of its entries.
--  Arguments:
--      `String`: The XML file to parse.
--  Returns:
--      `[Entry]`: An array of all present entries.
parseFile :: String -> [Entry]
parseFile xml = [ makeEntry potentialEntry | potentialEntry <- findAllEntries ( TagSoup.parseTags xml ), length potentialEntry > 0 ]