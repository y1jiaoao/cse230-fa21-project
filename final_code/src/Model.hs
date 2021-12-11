{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO.Unsafe
import Brick.Widgets.Dialog
import System.Random

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State = MkState
  { gameState :: GameState
  , isServer :: Bool
  }

data GameState
  = Intro IntroState
  | Play PlayState
  | Outro PlayState (Dialog Int)

data IntroState
  = SelectSinglePlayer
  | SelectMultiPlayerServer
  | SelectMultiPlayerClient
  deriving (Eq)

chgIntroStateUp :: IntroState -> IntroState
chgIntroStateUp SelectSinglePlayer = SelectMultiPlayerClient
chgIntroStateUp SelectMultiPlayerClient = SelectMultiPlayerServer
chgIntroStateUp SelectMultiPlayerServer = SelectSinglePlayer

chgIntroStateDown :: IntroState -> IntroState
chgIntroStateDown SelectSinglePlayer = SelectMultiPlayerServer
chgIntroStateDown SelectMultiPlayerServer = SelectMultiPlayerClient
chgIntroStateDown SelectMultiPlayerClient = SelectSinglePlayer
  
data PlayState = PS
  { psX      :: Player.Player   -- ^ player X info
  , psO      :: Player.Player   -- ^ player O info
  , psBoard  :: Board.Board     -- ^ current board
  , psTurn   :: Board.XO        -- ^ whose turn 
  , psPos    :: Board.Pos       -- ^ current cursor
  , psResult :: Board.Result () -- ^ result  
  , net_data :: NetData    
  } 

data NetData = NetData { useNet :: Bool,          -- if the network should be used
                         isServ :: Bool,          -- if this instance of the game is the server or client
                         socket :: Maybe Socket,  -- connected socket to use when sending and recv msgs
                         addr   :: String,        -- the ip address to connect to
                         port   :: String         -- the port to use/connect to
                       }

initSinglePlayer :: Int -> PlayState
initSinglePlayer n = PS 
  { psX      = Player.human
  , psO      = Player.rando
  , psBoard  = Board.init
  , psTurn   = if randomm==1 then Board.X else Board.O
  , psPos    = head Board.positions 
  , psResult = Board.Continue ()
  , net_data = NetData False False Nothing "" ""
  }

randomm :: Int
randomm = unsafeDupablePerformIO (randomRIO (0, 1))

initMultiPlayer :: Int -> Bool -> PlayState
initMultiPlayer n is_server = PS 
  { psX      = Player.human
  , psO      = Player.human
  , psBoard  = Board.init
  , psTurn   = if is_server then Board.X else Board.O
  , psPos    = head Board.positions 
  , psResult = Board.Continue ()
  , net_data = initNet is_server
  }

initNet :: Bool -> NetData
--- is server
initNet True = NetData True True (Just socket) "127.0.0.1" "5555"
  where socket = unsafeDupablePerformIO (serverSetup "5555")
initNet False = NetData True False (Just socket) "127.0.0.1" "5555"
  where socket = unsafeDupablePerformIO (clientSetup "127.0.0.1" "5555")

isCurr :: PlayState -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

next :: PlayState -> Board.Result Board.Board -> Either (Board.Result Board.Board) PlayState
next s Board.Retry     = Right s
next s (Board.Continue b') = Right (s { psBoard = b'
                                  , psTurn  = Board.flipXO (psTurn s) })
next s (Board.Win xo b)    = nextBoard (s{ psBoard = b}) (Board.Win xo b) 
next s res                 = nextBoard s res

nextBoard :: PlayState -> Board.Result a -> Either (Board.Result a) PlayState
nextBoard s res = case res of
                    Board.Win _ ss -> Left res
                    Board.Draw  -> Left res
                    _           -> Right s 
  -- where 
  --   sc'  = Score.add (Score.init 0) (Board.boardWinner res) 
  --   res' = Score.winner sc'
  --   s'   = s { psBoard = mempty                -- clear the board
  --            , psTurn  = Score.startPlayer sc' -- toggle start player
  --            } 

serverSetup :: String -> IO Socket
serverSetup prt = do
               addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just prt)              -- set up serveraddr with specified port
               let serveraddr = head addrinfos
               sock <- Network.Socket.socket (addrFamily serveraddr) Stream defaultProtocol
                                                  -- iniitalises socket
               bind sock (addrAddress serveraddr)
               listen sock 1                      -- only allow 1 simultanious connection
               putStrLn "Please sit still and wait for a client...\n(I won't tell you server can go first)"
               (conn, _) <- accept sock           -- accept connections
               return conn

clientSetup :: String -> String -> IO Socket
clientSetup addr prt = withSocketsDo $
     do addrinfos <- getAddrInfo Nothing (Just addr) (Just prt) 
        let serveraddr = head addrinfos
        sock <- Network.Socket.socket (addrFamily serveraddr) Stream defaultProtocol -- initialise socket
        connect sock (addrAddress serveraddr) -- connect to socket
        return sock