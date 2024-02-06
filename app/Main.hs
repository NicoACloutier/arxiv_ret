module Main where

import qualified Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified System.Environment
import qualified Query
import qualified Parser
import qualified Config

import qualified Brick.Main as M
import Brick.Types ( Widget )
import Brick.Widgets.Core 
    ( (<+>), (<=>), vBox, str, strWrap, emptyWidget )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B

interAdd :: [Widget ()] -> Widget () -> [Widget ()]
interAdd ( x:xs ) intermediate = [x, intermediate] ++ interAdd xs intermediate
interAdd _ _ = []

titles :: [Parser.Entry] -> Widget ()
titles x = foldl ( <=> ) ( head titles ) ( tail titles )
    where titles = interAdd ( map ( strWrap . Parser.title ) x ) ( B.hBorder )

displayAuths :: [String] -> String
displayAuths ( x:[] ) = x
displayAuths ( x:xs ) = x ++ ", " ++ displayAuths xs

display :: Parser.Entry -> Widget ()
display entry = foldl ( <=> ) ( head elements ) ( tail elements )
    where elements = interAdd ( [ strWrap ( Parser.title entry ),
                                  strWrap ( "Authors: " ++ displayAuths ( Parser.authors entry ) ),
                                  strWrap ( Parser.summary entry ),
                                  str ( "Link: " ++ Parser.link entry ),
                                  str ( "PDF link: " ++ Parser.pdfLink entry )
                                ] ) ( B.hBorder )

ui :: [Parser.Entry] -> Widget ()
ui entryInputs =
    vBox [ B.hBorderWithLabel (str "arXiv retriever"),
           ( titles entryInputs
           <+> B.vBorder
           <+> display ( head entryInputs ) )
         ]

main :: IO ()
main = do
    arguments <- System.Environment.getArgs
    defaults = [Config.searchField, Config.searchQuery, Config.beginning, 
                Config.maximum, Config.sortKey, Config.sortOrder]
    
    let url = Query.query arguments defaults
    response <- Network.HTTP.Conduit.simpleHttp url
    let responseString = LazyChar8.unpack ( response )
    let entries = Parser.parseFile responseString
    
    M.simpleMain ( ui entries )