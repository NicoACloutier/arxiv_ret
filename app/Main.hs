module Main where

import qualified Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified System.Environment
import qualified Query
import qualified Parser

import qualified Brick.Main as M
import Brick.Types ( Widget )
import Brick.Widgets.Core ( (<=>), vBox, str )

ui :: [String] -> Widget ()
ui titleInput = vBox [ foldl (<=>) ( head titleArray ) ( tail titleArray ) ]
    where titleArray = map str titleInput

main :: IO ()
main = do
    arguments <- System.Environment.getArgs
    defaults <- readFile "./arxivret.config"
    
    let url = Query.query arguments defaults
    response <- Network.HTTP.Conduit.simpleHttp url
    let responseString = LazyChar8.unpack ( response )
    let entries = Parser.parseFile responseString
    
    let titles = map Parser.title entries

    M.simpleMain ( ui titles )