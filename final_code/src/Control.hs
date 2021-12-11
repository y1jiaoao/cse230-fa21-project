module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Brick.Widgets.Dialog (Dialog, dialog, dialogSelection, handleDialogEvent)

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString as B
import System.IO.Unsafe

-------------------------------------------------------------------------------

control :: State -> BrickEvent n Tick -> EventM n (Next State)
control (MkState (Intro intro_state) isServer)  ev = case ev of
  T.VtyEvent (V.EvKey V.KEsc _) -> Brick.halt (MkState (Intro intro_state) isServer)
  T.VtyEvent (V.EvKey V.KUp _) -> Brick.continue (MkState (Intro (Model.chgIntroStateUp intro_state)) isServer)
  T.VtyEvent (V.EvKey V.KDown _) -> Brick.continue (MkState (Intro (Model.chgIntroStateDown intro_state)) isServer)
  T.VtyEvent (V.EvKey V.KEnter _) -> case intro_state of
    SelectSinglePlayer -> Brick.continue (MkState (Play (Model.initSinglePlayer 0)) False)
    SelectMultiPlayerServer -> Brick.continue =<< liftIO (handshake True)
    SelectMultiPlayerClient -> Brick.continue =<< liftIO (handshake False)
  _ -> Brick.continue (MkState (Intro intro_state) isServer)
control (MkState (Outro game_state outro_dialog) isServer) (T.VtyEvent (V.EvKey V.KEnter _)) =
  if dialogSelection outro_dialog == Just 1
    then
      Brick.continue (MkState (Intro SelectSinglePlayer) isServer)
    else
      Brick.halt (MkState (Intro SelectSinglePlayer) isServer)
control (MkState (Outro game_state outro_dialog) isServer) (T.VtyEvent ev) = do
  new_dialog <- handleDialogEvent ev outro_dialog
  Brick.continue (MkState (Outro game_state new_dialog) isServer)
control (MkState (Outro game_state outro_dialog) isServer) (AppEvent Tick) = Brick.continue (MkState (Outro game_state outro_dialog) isServer)
control (MkState (Outro game_state outro_dialog) isServer) _ = error "outro other event"
control (MkState (Play s) isServer) ev = case ev of 
  AppEvent Tick                   -> nextS s isServer =<< liftIO (play O s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextS s isServer =<< liftIO (play X s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (MkState (Play (move up    s)) isServer)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (MkState (Play (move down  s)) isServer)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (MkState (Play (move left  s)) isServer)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (MkState (Play (move right s)) isServer)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt (MkState (Play s) isServer)
  _                               -> Brick.continue (MkState (Play s) isServer) -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psPos = f (psPos s) }

-------------------------------------------------------------------------------
play :: XO -> PlayState -> IO (Result Board)
-------------------------------------------------------------------------------
play xo s
  | psTurn s == xo = put (psBoard s) xo <$> getPos xo s 
  | otherwise      = return Retry

handshake :: Bool -> IO State
-- is server
handshake True = do
  let new_s = Model.initMultiPlayer 0 True
  _ <- send (must (Model.socket (net_data new_s))) (C.pack "handshake")
  return (MkState (Play new_s) True)
-- is client
handshake False = do
  let new_s = Model.initMultiPlayer 0 False
  _ <- B.recv (must (Model.socket (net_data new_s))) 1024
  return (MkState (Play new_s) False)

getPos :: XO -> PlayState -> IO Pos
getPos xo s = if useNet (net_data s)
  then
    if xo == X then
      do
        _ <- send (must (Model.socket (net_data s))) (C.pack (posToString (psPos s)))
        -- putStrLn "finish send"
        return (psPos s)
      else
        return (stringToPos (C.unpack $ unsafeDupablePerformIO $ B.recv (must (Model.socket (net_data s))) 1024 ))
  else
    getStrategy xo s (psPos s) (psBoard s) xo

getStrategy :: XO -> PlayState -> Strategy 
getStrategy X s = plStrat (psX s)
getStrategy O s = plStrat (psO s)

posToString :: Pos -> String
posToString (Pos r c) = "(" ++ show r ++ "," ++ show c ++ ")"

stringToPos :: String -> Pos
stringToPos s = Pos r c
  where (r,c) = read s::(Int, Int)

must :: Maybe a -> a
must (Just a) = a

-------------------------------------------------------------------------------
nextS :: PlayState -> Bool -> Result Board -> EventM n (Next State)
-------------------------------------------------------------------------------
nextS s is_server b = case next s b of
  Right s' -> continue (MkState (Play s') is_server)
  Left (Win xo b) -> continue (MkState (Outro (s{ psBoard = b}) (initOutroDialog (Win xo b))) is_server)
  Left res -> continue (MkState (Outro s (initOutroDialog res)) is_server)

initOutroDialog :: Result Board -> Dialog Int
initOutroDialog res = dialog (Just game_over_info) (Just (0, [("Play Again", 1), ("Exit", 2)])) 1024
  where game_over_info = case res of
          Win X _-> "You Win! Congratulations!"
          Win O _-> "You Lose! Better Luck Next Time"
          Draw -> "Draw!"
          Retry -> error "should not enter initOutroDialog when result=Retry"
          Continue map -> error "should not enter initOutroDialog when result=Continue"

