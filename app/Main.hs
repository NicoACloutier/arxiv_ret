{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V

import Brick.BChan
import Brick.Main
  ( App(..)
  , showFirstCursor
  , customMainWithDefaultVty
  , halt
  )
import Brick.AttrMap
  ( attrMap
  )
import Brick.Types
  ( Widget
  , EventM
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( (<=>)
  , str
  )

import qualified Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified System.Environment
import qualified Query
import qualified Parser
import qualified Config

import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.TH ( makeLenses )
import Lens.Micro.Mtl
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)
import Data.Monoid

import qualified Brick.Main as M
import Brick.Types ( Widget )
import Brick ( App (..), BrickEvent (..), EventM, )
import Brick.Widgets.Core 
    ( (<+>), (<=>), vBox, str, strWrap, emptyWidget )
import Brick.AttrMap ( attrMap )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B

data Screen = Screen { entryArray :: [Parser.Entry],
                       index :: Integer}

data St = St { _screen :: Screen }

makeLenses ''St

data Name = ArXivRet
    deriving ( Ord, Eq, Show )

interAdd :: [Widget Name] -> Widget Name -> [Widget Name]
interAdd ( x:xs ) intermediate = [x, intermediate] ++ interAdd xs intermediate
interAdd _ _ = []

titles :: [Parser.Entry] -> Widget Name
titles x = foldl ( <=> ) ( head titles ) ( tail titles )
    where titles = interAdd ( map ( strWrap . Parser.title ) x ) ( B.hBorder )

displayAuths :: [String] -> String
displayAuths ( x:[] ) = x
displayAuths ( x:xs ) = x ++ ", " ++ displayAuths xs

display :: Parser.Entry -> Widget Name
display entry = foldl ( <=> ) ( head elements ) ( tail elements )
    where elements = interAdd ( [ strWrap ( Parser.title entry ),
                                  strWrap ( "Authors: " ++ displayAuths ( Parser.authors entry ) ),
                                  strWrap ( Parser.summary entry ),
                                  str ( "Link: " ++ Parser.link entry ),
                                  str ( "PDF link: " ++ Parser.pdfLink entry )
                                ] ) ( B.hBorder )

ui :: St -> [Widget Name]
ui st = [
              vBox [ B.hBorderWithLabel (str "arXiv retriever"),
                     ( titles ( entryInputs )
                     <+> B.vBorder
                     <+> display ( entryInputs !! fromInteger ind ) )
                   ]
            ]
                where 
                    entryInputs = entryArray ( st^.screen )
                    ind = index ( st^.screen )

decrease :: Screen -> Screen
decrease s = case index s of
    0 -> s
    _ -> Screen ( entryArray s ) ( ( index s ) - 1 )

increase :: Screen -> Screen
increase s = if maximumIndex then s else increasedS
    where
        maximumIndex = index s == ( toInteger $ length $ entryArray s ) - 1
        increasedS = Screen ( entryArray s ) ( ( index s ) + 1 )

appEvent :: BrickEvent Name () -> EventM Name St ()
appEvent ( VtyEvent ( V.EvKey V.KEsc [] ) ) = M.halt
appEvent ( VtyEvent ( V.EvKey ( V.KChar 'q' ) [] ) ) = M.halt
appEvent ( VtyEvent ( V.EvKey V.KUp [] ) ) = screen %= decrease
appEvent ( VtyEvent ( V.EvKey V.KDown [] ) ) = screen %= increase
appEvent _ = return ()

app :: App St () Name
app = App { M.appDraw = ui,
            M.appChooseCursor = M.showFirstCursor,
            M.appHandleEvent = appEvent,
            M.appStartEvent = return (),
            M.appAttrMap = const $ attrMap V.defAttr []
          }

main :: IO ()
main = do
    arguments <- System.Environment.getArgs
    let defaults = [Config.searchField, Config.searchQuery, Config.beginning, 
                    Config.maximum, Config.sortKey, Config.sortOrder]
    
    let url = Query.query arguments defaults
    response <- Network.HTTP.Conduit.simpleHttp url
    let responseString = LazyChar8.unpack ( response )
        entries = Parser.parseFile responseString
        screen = Screen { entryArray = entries,
                          index = 0
                        } 
        st = St { _screen = screen }
    
    void $ M.defaultMain app $ st