module View (view, theMap) where

import Brick
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Dialog ( renderDialog, buttonSelectedAttr )
import Text.Printf (printf)

import Model
import Model.Board
import Graphics.Vty hiding (dim)
import qualified Graphics.Vty as V

-------------------------------------------------------------------------------
view :: State -> [Widget String]
-------------------------------------------------------------------------------
view s = viewHelper (gameState s)

viewHelper :: GameState -> [Widget String]
viewHelper (Intro SelectSinglePlayer) = [borderWithLabel (str "Welcome to Gomoku!") $ center $ vBox [drawButton "SinglePlayer" True, drawButton "MultiPlayer - Server" False, drawButton "MultiPlayer - Client" False]]
viewHelper (Intro SelectMultiPlayerServer) = [borderWithLabel (str "Welcome to Gomoku!") $ center $ vBox [drawButton "SinglePlayer" False, drawButton "MultiPlayer - Server" True, drawButton "MultiPlayer - Client" False]]
viewHelper (Intro SelectMultiPlayerClient) = [borderWithLabel (str "Welcome to Gomoku!") $ center $ vBox [drawButton "SinglePlayer" False, drawButton "MultiPlayer - Server" False, drawButton "MultiPlayer - Client" True]]
viewHelper (Outro s outro_dialog) = [renderDialog outro_dialog (view' s)]
viewHelper (Play s) = [view' s]

drawButton :: String -> Bool -> Widget String
drawButton info True = withAttr buttonOnAttr
  $ withBorderStyle BS.unicodeBold
  $ B.border
  $ hCenter
  $ padAll 1
  $ str info
drawButton info False = withAttr buttonOffAttr
  $ withBorderStyle BS.unicodeBold
  $ B.border
  $ hCenter
  $ padAll 1
  $ str info

view' :: PlayState -> Widget String
view' s = 
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      vBox [ mkRow s row | row <- [1..dim] ]

header :: PlayState -> String
header s = printf "Gomoku Turn = %s, row = %d, col = %d" (show turn) (pRow p) (pCol p)
  where 
    p    = psPos s
    turn = if useNet (net_data s) && not (isServ (net_data s)) then flipXO (psTurn s) else psTurn s

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hBox [ mkCell s row i | i <- [1..dim] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c 
  | isCurr s r c = withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c = center (whichMkXO xoMb)
  where 
    xoMb      = psBoard s ! Pos r c
    whichMkXO = if useNet (net_data s) && not (isServ (net_data s)) then reverseMkXO else mkXO
    -- xoMb 
    --   | r == c    = Just X 
    --   | r > c     = Just O 
    --   | otherwise = Nothing

mkXO :: Maybe XO -> Widget n
mkXO Nothing  = blockB
mkXO (Just X) = blockX
mkXO (Just O) = blockO

reverseMkXO :: Maybe XO -> Widget n
reverseMkXO Nothing  = blockB
reverseMkXO (Just X) = blockO
reverseMkXO (Just O) = blockX

blockB, blockX, blockO :: Widget n
blockB = vBox [ str "   "
              , str " . "
              , str "   "]
blockX = vBox [ str "   "
              , withAttr xAttr (str " X ")
              , str "   "]
blockO = vBox [ str "   "
              , withAttr oAttr (str " O ")
              , str "   "]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget

singlePlayerButtonAttr :: AttrName
singlePlayerButtonAttr = attrName "singlePlayerButtonAttr"

multiPlayerButtonAttr :: AttrName
multiPlayerButtonAttr = attrName "multiPlayerButtonAttr"

buttonOnAttr :: AttrName
buttonOnAttr = attrName "buttonOnAttr"

buttonOffAttr :: AttrName
buttonOffAttr = attrName "buttonOffAttr"

xAttr :: AttrName
xAttr = attrName "xAttr"

oAttr :: AttrName
oAttr = attrName "oAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [
    (singlePlayerButtonAttr, V.blue `on` V.red)
  , (multiPlayerButtonAttr, fg V.red `V.withStyle` V.bold)
  , (buttonOnAttr, bg V.white `V.withStyle` V.bold)
  , (buttonOffAttr, fg V.white `V.withStyle` V.bold)
  , (xAttr, fg V.red `V.withStyle` V.bold)
  , (oAttr, fg V.green `V.withStyle` V.bold)
  , (buttonSelectedAttr, bg V.white `V.withStyle` V.bold)
  ]