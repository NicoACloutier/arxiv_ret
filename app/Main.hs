{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified System.Environment
import qualified System.Hclip
import qualified Query
import qualified Parser
import qualified Config

import qualified Graphics.Vty as V
import Lens.Micro ( (^.) )
import Lens.Micro.TH ( makeLenses )
import Brick.AttrMap ( attrMap )
import Lens.Micro.Mtl
import Control.Monad ( void )

import qualified Brick.Main as M
import Brick.Main ( App(..), showFirstCursor, halt )
import Brick.Types ( Widget, EventM, BrickEvent(..) )
import Brick.Widgets.Core  ( (<+>), (<=>), vBox, str, strWrap, emptyWidget )
import qualified Brick.Widgets.Border as B

-- |A screen state.
--  Fields:
--      `entryArray :: [Parser.Entry]`: An array of entries to display.
--      `index :: Integer`: The current index of the entry to display.
--      `copy :: String`: The link to copy to clipboard.
data Screen = Screen { entryArray :: [Parser.Entry],
                       index :: Integer,
                       copy :: String 
                     }

information :: String
information = "To close the application, press Q. Navigate with arrow keys.\
              \\nYou may press C to copy a file's link to the\
              \ clipboard, and D to copy a link to its PDF.\nThe link will not be copied until \
              \the program is closed.\nBy default, nothing is copied. To cancel a copy, press X\
              \."

-- |The application state.
--      NOTE: This wraps the Screen type to allow it to be passed in its entirety to functions,
--            which is useful for event handling.
--  Fields:
--      `_screen :: Screen`: The screen state.
data St = St { _screen :: Screen }

makeLenses ''St

-- |The application name.
data Name = ArXivRet
    deriving ( Ord, Eq, Show )

-- |Add an intermediate widget within every two adjacent elements of a wdiget array.
--  Arguments:
--      `[Widget Name]`: The initial array of widgets.
--      `Widget Name`: The widget to add in between all of them.
--  Returns:
--      `[Widget Name]`: A new array with intermediate values.
interAdd :: [Widget Name] -> Widget Name -> [Widget Name]
interAdd ( x:xs ) intermediate = [x, intermediate] ++ interAdd xs intermediate
interAdd _ _ = []

-- |Display the titles of an array of entries in a vBox widget.
--  Arguments:
--      `[Parser.Entry]`: The array of entries to display the titles of.
--  Returns:
--      `Widget Name`: The vBox with names.
titles :: [Parser.Entry] -> Widget Name
titles x = foldl ( <=> ) ( head titles ) ( tail titles )
    where titles = interAdd ( map ( strWrap . Parser.title ) x ) ( B.hBorder )

-- |Display an array of authors as a single comma-delimited string.
--  Arguments:
--      `[String]`: An array of author names.
--  Returns:
--      `String`: Those same names in a single comma-delimited string.
displayAuths :: [String] -> String
displayAuths ( x:[] ) = x
displayAuths ( x:xs ) = x ++ ", " ++ displayAuths xs

-- |Display a single entry as the primary view entry in the application.
--  Arguments:
--      `Parser.Entry`: The entry to display.
--  Returns:
--      `Widget Name`: The widget for displaying.
display :: Parser.Entry -> Widget Name
display entry = foldl ( <=> ) ( head elements ) ( tail elements )
    where elements = interAdd ( [ strWrap ( Parser.title entry ),
                                  strWrap ( "Authors: " ++ displayAuths ( Parser.authors entry ) ),
                                  strWrap ( Parser.summary entry ),
                                  str ( "Link: " ++ Parser.link entry ),
                                  str ( "PDF link: " ++ Parser.pdfLink entry )
                                ] ) ( B.hBorder )

-- |Create a user interface given the application state.
--  Arguments:
--      `St`: The application state to use.
--  Returns:
--      `[Widget Name]`: The array of widgets used to generate UI.
ui :: St -> [Widget Name]
ui st = [
            vBox [ B.hBorderWithLabel (str "arXiv retriever"),
                   ( titles ( entryInputs )
                   <+> B.vBorder
                   <+> display ( entryInputs !! fromInteger ind ) ),
                   B.hBorderWithLabel (str "Information"),
                   (str information),
                   (str ( "Current copied value: `" ++ copyVal ++ "`.") ),
                   B.hBorder
                 ]
        ]
                where 
                    entryInputs = entryArray ( st^.screen )
                    ind = index ( st^.screen )
                    copyVal = if copy ( st^.screen ) /= "" then copy ( st^.screen ) else "NONE"

-- |Decrease the present index of a screen state.
--  Arguments:
--      `Screen`: The screen state to lower the index of.
--  Returns:
--      `Screen`: The same screen state with a lower index.
decrease :: Screen -> Screen
decrease s = case index s of
    0 -> s
    _ -> Screen ( entryArray s ) ( ( index s ) - 1 ) ( copy s )

-- |Increase the present index of a screen state.
--  Arguments:
--      `Screen`: The screen state to increase the index of.
--  Returns:
--      `Screen`: The same screen state with a higher index.
increase :: Screen -> Screen
increase s = if maximumIndex then s else increasedS
    where
        maximumIndex = index s == ( toInteger $ length $ entryArray s ) - 1
        increasedS = Screen ( entryArray s ) ( ( index s ) + 1 ) ( copy s )

-- |Set the current copy link value to the current article link.
--  Arguments:
--      `Screen`: The initial screen.
--  Returns:
--      `Screen`: The screen with the current article's link in the copy field.
copyVal :: Screen -> Screen
copyVal s = Screen ( entryArray s ) ( index s ) currentLink
    where currentLink = Parser.link ( entryArray s !! fromInteger ( index s ) )

-- |Set the current copy link value to the current article PDF link.
--  Arguments:
--      `Screen`: The initial screen.
--  Returns:
--      `Screen`: The screen with the current article's PDF link in the copy field.
copyPdf :: Screen -> Screen
copyPdf s = Screen ( entryArray s ) ( index s ) currentLink
    where currentLink = Parser.pdfLink ( entryArray s !! fromInteger ( index s ) )

-- |Set the current copy link value to the empty string.
--  Arguments:
--      `Screen`: The initial screen.
--  Returns:
--      `Screen`: The screen with the empty string in the copy field.
removeCopy :: Screen -> Screen
removeCopy s = Screen ( entryArray s ) ( index s ) ( "" )

-- |Handle an application event.
--  Arguments:
--      `BrickEvent Name ()`: The event to handle.
--          NOTE: Currently, supported events are as follows:
--              `esc`: exits program.
--              `q`: exits program.
--              `up`: decrease screen state index.
--              `down`: increase screen state index.
--  Returns:
--      `EventM Name St ()`: The returned action.
appEvent :: BrickEvent Name () -> EventM Name St ()
appEvent ( VtyEvent ( V.EvKey V.KEsc [] ) ) = M.halt
appEvent ( VtyEvent ( V.EvKey ( V.KChar 'q' ) [] ) ) = M.halt
appEvent ( VtyEvent ( V.EvKey V.KUp [] ) ) = screen %= decrease
appEvent ( VtyEvent ( V.EvKey V.KDown [] ) ) = screen %= increase
appEvent ( VtyEvent ( V.EvKey ( V.KChar 'c' ) [] ) ) = screen %= copyVal
appEvent ( VtyEvent ( V.EvKey ( V.KChar 'd' ) [] ) ) = screen %= copyPdf
appEvent ( VtyEvent ( V.EvKey ( V.KChar 'x' ) [] ) ) = screen %= removeCopy
appEvent _ = return ()

-- |The arXiv retriever application.
--  Fields:
--      `M.appDraw`: The drawing function; `ui`.
--      `M.appChooseCursor`: The cursor behavior; `M.showFirstCursor`.
--      `M.appHandleEvent`: The event handling function; `appEvent`.
--      `M.appStartEvent`: The start event; `return ()`.
--      `M.appAttrMap`: The attribute map; `const $ attrMap V.defAttr []`.
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
                          index = 0,
                          copy = ""
                        } 
        st = St { _screen = screen }
    
    finalSt <- M.defaultMain app $ st
    let finalScreen = _screen finalSt
    if copy finalScreen /= "" then System.Hclip.setClipboard ( copy finalScreen ) else return ()