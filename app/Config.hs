module Config where

-- EDIT THIS FILE to change default values.
-- Make sure all values are left inside of quotes.

-- First argument: search field.
-- This is the field that the search will be performed in.
-- The options are as folows:
--  "ti": search within titles of articles.
--  "au": search within authors of articles.
--  "abs": search within article summaries.
--  "co": search within article comments.
--  "jr": search within journal reference.
--  "cat": search for a category.
--  "rn": search for a reference number.
--  "id": search for paper ID.
--  "all": search in all fields.
-- "cat" by default.
searchField :: String
searchField = "cat" -- EDIT HERE

-- The search query, searched within the search field.
-- "cs.CL" by default.
searchQuery :: String
searchQuery = "cs.CL" -- EDIT HERE

-- The beginning value to return (0-indexed).
-- "0" by default.
beginning :: String
beginning = "0" -- EDIT HERE

-- The maximum number of papers to return.
-- "10" by default.
maximum :: String
maximum = "10" -- EDIT HERE

-- The key to sort by.
-- The options are as follows:
--  "rel": relevance.
--  "submitted": last submitted date.
--  "updated": last updated date.
-- "submitted" by default.
sortKey :: String
sortKey = "submitted" -- EDIT HERE

-- The order to sort in, either ascending or descending according to the above key.
-- The options are as follows:
--  "desc": descending order.
--  "asc": ascending order.
-- "desc" by default.
sortOrder :: String
sortOrder = "desc"