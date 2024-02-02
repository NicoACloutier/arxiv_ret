module Wrapper ( split, createUrl ) where

baseUrl :: String
baseUrl = "https://export.arxiv.org/api/query?"

-- |Split a string on a delimiter character.
--  Arguments:
--      `(Char -> Bool)`: An equality check for the delimiter.
--      `String`: The string to parse.
--  Returns:
--      `[String]`: A split String list.
split :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
    "" -> []
    s' -> w : split p s''
        where (w, s'') = break p s'

-- |Parse a search category and add a properly formed addition to query.
--      This is the field that the API will search in.
--      Options: 'ti' (title), 'au' (author), 'abs' (abstract), 'co' (comment),
--               'jr' (journal reference), 'cat' (subject category), 'rn' (report number),
--               'id' (id), 'all' (all of the above).
--      See arXiv API User Manual for more details.
--  Arguments:
--      `String`: The field to search in. 
--  Returns:
--      `String`: The properly formed addition to the API URL request with the field.
parseSearchField :: String -> String
parseSearchField x = "search_query=" ++ x

-- |Parse a search query. This is the actual search itself.
--  Arguments:
--      `String`: The search query.
--  Returns:
--      `String`: The properly formed addition to API URL request with query.
parseSearchQuery :: String -> String
parseSearchQuery x = ":" ++ x

-- |Parse the start value for an API request.
--      This is used to tell API which in the returned order you want to begin with.
--  Arguments:
--      `String`: A zero-indexed index of the beginning value.
--  Returns:
--      `String`: The properly formed addition to API URL request with start value.
parseBegin :: String -> String
parseBegin x = "&start=" ++ x

-- |Parse the maximum returned values for an API request.
--  Arguments:
--      `String`: The number of maximum results to return.
--  Returns:
--      `String`: The properly formed addition to API URL request with maximum returned.
parseMaxResults :: String -> String
parseMaxResults x = "&max_results=" ++ x

-- |Parse the sort key for the API request.
--  Arguments:
--      `String`: The sort key.
--          Options: "relevance", "lastUpdatedDate", "submittedDate".
--          See arXiv API User Manual for more details.
--  Returns:
--      `String`: The properly formed addition to API URL request with sort key.
parseSort :: String -> String
parseSort x = "&sortBy=" ++ x

-- |Parse the sort order.
--  Arguments:
--      `String`: The sort order.
--          Options: "asc" (ascending), "desc" (descending).
--  Returns:
--      `String`: The properly formed addition to API URL request with sort order.
parseOrder :: String -> String
parseOrder x = "&sortOrder=" ++ x ++ "cending"

-- |Parse an argument and apply its parsing function, returning its URL addition.
--  Arguments:
--      `String`: The query to be parsed.
--          Has an initial character giving parse type information as well as specific query.
--          Initial character options: 'f' (search field), 'q' (search query),
--          'b' (starting index), 'm' (maximum results), 's' (sort key), and 'o' (sort order).
--  Returns:
--      `String`: The addition to the complete URL request.
parseArgument :: String -> String
parseArgument ( x:' ':xs ) = ( parser . init ) xs
    where 
        parser = case x of
            'f' -> parseSearchField
            'q' -> parseSearchQuery
            'b' -> parseBegin
            'm' -> parseMaxResults
            's' -> parseSort
            'o' -> parseOrder
            _   -> \_ -> ""
parseArgument _ = ""

-- |Convert a query to an array of URL additions, split by dashes.
--  Arguments:
--      `String`: The query string.
--          Example: "-f cat -q cs.cl -b 0 -m 10 -s lastUpdatedDate -o desc"
--  Returns:
--      `[String]`: The array of query additions.
--          Example: ["search_query=cat", ":cs.cl", "&start=0", "&max_results=10", 
--                    "&sortBy=lastUpdatedDate", "&sortOrder=descending"]
takeQuery :: String -> [String]
takeQuery query = map parseArgument ( split ( == ( '-' ) ) query )

-- |Convert a query to a complete URL request URL.
--  Arguments:
--      `String`: The query string.
--          Example: "-f cat -q cs.cl -b 0 -m 10 -s lastUpdatedDate -o desc"
--  Returns:
--      `String`: The arXiv API request URL.
createUrl :: String -> String
createUrl query = baseUrl ++ ( foldl (++) "" ( takeQuery query ) )