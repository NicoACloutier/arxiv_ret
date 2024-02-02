module Query ( makeEntries ) where

import qualified Parser
import qualified Wrapper

searchFieldOptions :: [String]
searchFieldOptions = ["ti", "au", "abs", "co", "jr", "cat", "rn", "id", "all"]
beginOptions :: [String]
beginOptions = ["b"]
maxResultsOptions :: [String]
maxResultsOptions = ["m"]
sortOptions :: [String]
sortOptions = ["s"]
orderOptions :: [String]
orderOptions = ["asc", "desc"]

-- |Find an option after a dash, returning the option if present, otherwise the default value.
--      NOTE: This is meant for argumentless options, such as `-desc` and `-cat`.
--  Arguments:
--      `[String]`: The full query, split by spaces.
--      `[String]`: The options for this argument.
--          NOTE: e.g. `["asc", "desc"]` for `orderOptions`
--      `String`: The default value of this option.
--  Returns:
--      `String`: The first option encountered.
findOption :: [String] -> [String] -> String -> String
findOption ( x:xs ) options d = if x `elem` options then tail x else findOption xs options d
findOption [] _ d = d

-- |Find an option argument, returning the argument if present, otherwise the default value.
--      NOTE: This is meant for options with arguments, such as `-b` and `-m`.
--  Arguments:
--      `[String]`: The full query, split by spaces.
--      `[String]`: The options for the option that takes this argument.
--          NOTE: e.g. `["s"]` for `sortOptions`
--      `String`: The default value of this argument.
--  Returns:
--      `String`: The first option argument encountered.
findSearch :: [String] -> [String] -> String -> String
findSearch ( x:y:xs ) options d = if x `elem` options then y else findSearch ( y:xs ) options d
findSearch _ _ d = d

-- |Convert a sorting option to its proper form.
--  Arguments:
--      `String`: The userland option for sorting key.
--          NOTE: `rel` for relevance, `updated` for last updated, `submitted` for last submitted.
--  Returns:
--      `String`: The proper, converted form for this key to be passed to the API wrapper.
convertSort :: String -> String
convertSort "rel" = "relevance"
convertSort "updated" = "lastUpdatedDate"
convertSort "sumbitted" = "submittedDate"
convertSort "relevance" = "relevance"
convertSort "lastUpdatedDate" = "lastUpdatedDate"
convertSort "sumbittedDate" = "submittedDate"
convertSort _ = ""

-- |Convert an improper query to its proper form.
--  Arguments:
--      `[String]`: The improper query, split by spaces.
--      `[String]`: The default values for all arguments.
--          NOTE: must be in order: field, query, start #, max results, sort key, order.
--  Returns:
--      `String`: The proper, converted form for this query to be passed to the API wrapper.
makeProper :: [String] -> [String] -> String
makeProper improper defaults = foldl (++) "" [field, search, begin, maxResults, sort, order]
    where
        field = "-f " ++ ( findOption improper searchFieldOptions ( defaults !! 0 ) )
        search = "-q " ++ ( findSearch improper searchFieldOptions ( defaults !! 1 ) )
        begin = "-b " ++ ( findSearch improper beginOptions ( defaults !! 2 ) )
        maxResults = "-m " ++ ( findSearch improper maxResultsOptions ( defaults !! 3 ) )
        sort = "-s " ++ convertSort ( findSearch improper sortOptions ( defaults !! 4 ) )
        order = "-o " ++ ( findOption improper orderOptions ( defaults !! 5 ) )

-- |Convert an improper query to a URL, passing it to the API Wrapper.
--  Arguments:
--      `String`: The improper query.
--      `[String]`: A list of the default options.
--  Returns:
--      `String`: The resulting URL.
toUrl :: String -> [String] -> String
toUrl improperQuery d = Wrapper.createUrl ( makeProper splitQuery d )
    where splitQuery = ( Wrapper.split ( == ( ' ' ) ) improperQuery )

-- |Find a line for a particular option in the defaults .config file.
--  Arguments:
--      `String`: The option to search for.
--      `[String]`: The file text, split by newlines.
--      `String`: The default value of this option.
--  Returns:
--      `String`: The found value of this option in the file, otherwise the given default.
findLine :: String -> [String] -> String -> String
findLine m ( x:xs ) d = if m == ( take ( length m ) x ) then ( drop ( ( length m ) + 1 ) x ) else findLine m xs d
findLine _ [] d = d

-- |Get a list of defaults in the .config file.
--  Arguments:
--      `[String]`: The .config file text, split by newlines.
--  Returns:
--      `[String]`: The default values.
--          NOTE: will be in order: field, query, start #, max results, sort key, order.
getDefaults :: [String] -> [String]
getDefaults d = [defField, defSearch, defBegin, defMaxResults, defSort, defOrder]
    where
        defField = findLine "f" d "cat"
        defSearch = findLine "s" d "cs.CL"
        defBegin = findLine "b" d "0"
        defMaxResults = findLine "m" d "10"
        defSort = findLine "s" d "submitted"
        defOrder = findLine "o" d "desc"

-- |Make a userland query, returning the URL for arXiv retrieval.
--  Arguments:
--      `String`: The userland query to parse.
--      `String`: The .config file text, for default values.
--  Returns:
--      `String`: The URL for XML retrieval.
query :: String -> String -> String
query x d = toUrl x defs
    where defs = getDefaults ( Wrapper.split ( == ( '\n' ) ) d )

-- |Make a userland query and return a list of entries in the result.
--  Arguments:
--      `String`: The userland query to parse.
--      `String`: The .config file text, for default values.
--  Returns:
--      `[Parser.Entry]`: A list of entries with parsed information from the XML.
makeEntries :: String -> String -> [Parser.Entry]
makeEntries x d = Parser.parseFile ( query x d )