module Main where

import qualified Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified System.Environment
import qualified Query
import qualified Parser

import qualified Brick.Main as M
import Brick.Types ( Widget )
import Brick.Widgets.Core 
    ( (<+>), (<=>), vBox, str, strWrap )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B

addIntermediates :: [Widget ()] -> Widget () -> [Widget ()]
addIntermediates ( x:xs ) intermediate = [x, intermediate] ++ addIntermediates xs intermediate
addIntermediates _ _ = []

titles :: [Parser.Entry] -> Widget ()
titles x = foldl ( <=> ) ( head titles ) ( tail titles )
    where titles = addIntermediates ( map ( strWrap . Parser.title ) x ) ( B.hBorder )

ui :: [Parser.Entry] -> Widget ()
ui entryInputs =
    vBox [ B.hBorderWithLabel (str "arXiv retriever"),
           ( titles entryInputs
           <+> C.center ( str "" )
           <+> B.vBorder
           <+> (str ""))
         ]

main :: IO ()
main = do
    arguments <- System.Environment.getArgs
    defaults <- readFile "./arxivret.config"
    
    let url = Query.query arguments defaults
    response <- Network.HTTP.Conduit.simpleHttp url
    let responseString = LazyChar8.unpack ( response )
    let entries = Parser.parseFile responseString
    
    M.simpleMain ( ui entries )