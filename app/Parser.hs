module Parser where

import qualified Text.HTML.TagSoup as TagSoup

data Entry = Entry { title :: String,
                     summary :: String,
                     authors :: [String],
                     pdfLink :: String, 
                     link :: String }
    deriving ( Read, Show, Eq )

filterTag :: String -> [TagSoup.Tag String] -> [TagSoup.Tag String]
filterTag _ [] = []
filterTag tag ( current:xs ) = if ( current == TagSoup.TagClose tag ) then [] else [current] ++ ( filterTag tag xs )

findTag :: String -> [TagSoup.Tag String] -> [TagSoup.Tag String]
findTag _ [] = []
findTag tag ( current:xs ) = if ( current == TagSoup.TagOpen tag [] ) then filterTag tag xs else findTag tag xs

findAllTags :: String -> [TagSoup.Tag String] -> [String]
findAllTags _ [] = []
findAllTags tag entry = ( if ( head entry == TagSoup.TagOpen tag [] ) then map TagSoup.fromTagText ( findTag ( tag ) entry ) else [] ) ++ findAllTags tag ( tail entry )

findAllEntries :: [TagSoup.Tag String] -> [[TagSoup.Tag String]]
findAllEntries [] = [[]]
findAllEntries entry = [( if ( head entry == TagSoup.TagOpen "entry" [] ) then findTag "entry" entry else [] )] ++ findAllEntries ( tail entry )

findLink :: String -> [TagSoup.Tag String] -> String
findLink linkType ( x:xs ) = if ( possibleLink /= "" && TagSoup.fromAttrib "type" x == linkType ) then possibleLink else findLink linkType xs
    where possibleLink = TagSoup.fromAttrib "href" x
findLink _ _ = ""

makeEntry :: [TagSoup.Tag String] -> Entry
makeEntry entryXml = Entry entryTitle entrySummary entryAuthors entryPdfLink entryLink
    where
        entryTitle = TagSoup.fromTagText ( head ( findTag "title" entryXml ) )
        entrySummary = TagSoup.fromTagText ( head ( findTag "summary" entryXml ) )
        entryAuthors = findAllTags "author" entryXml
        entryPdfLink = findLink "application/pdf" entryXml
        entryLink = findLink "text/html" entryXml
        
parseFile :: String -> [Entry]
parseFile xml = map ( makeEntry ) ( findAllEntries ( TagSoup.parseTags xml ) )