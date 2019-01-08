{-# LANGUAGE DeriveDataTypeable #-}

module UI where

import Text.JSON.Generic
import Control.Exception as E
import System.IO

import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.State
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)

data GridConfig = GridConfig { grid::GridData
                             , robo::[(Robo,Pos)]
                             } deriving (Show,Eq,Typeable,Data)

data GridData = Grid { rows::Int
                     , cols::Int
                     , cellmem::[[(Int,Bool)]]
                     , blocks::[Pos]
                     } deriving (Show,Eq,Typeable,Data)

data Robo = Robo { roboname::String
                 , allowBrick::Bool
                 , robomem::Int
                 } deriving (Show,Eq,Typeable,Data)

type Pos = (Int,Int)
---------------------------------------------------------------------------------------------------------------
tryTillsuccess :: (FilePath -> IOMode -> IO Handle) -> FilePath -> IOMode -> IO Handle
tryTillsuccess opr path mode = do
  x <- E.try (opr path mode) :: IO (Either E.IOException Handle)
  case x of
    Left _ -> threadDelay 1000 >> tryTillsuccess opr path mode
    Right hdl -> return hdl

tryGetLine :: (Handle -> IO String) -> Handle -> IO String -- exception handling for getline
tryGetLine opr hdl = do
  x <- E.try (opr hdl) :: IO (Either E.IOException String)
  case x of
    Left _ -> threadDelay 1000 >> tryGetLine opr hdl
    Right st -> return st

----------------------------------------------------------------------------------------------------------------
wait :: IO GridConfig
wait = do
  hdl <- tryTillsuccess openFile "result.gi" ReadMode
  g <- tryGetLine hGetLine hdl
  hClose hdl 
  return (decodeJSON g :: GridConfig)

initGame :: IO GridConfig
initGame = do
  hdl <- tryTillsuccess openFile "request.gi" WriteMode
  hPutStr hdl "YES"
  hClose hdl
  g <- wait
  return g

gridB :: IO () -- IO monad is used here 
gridB = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 500000 -- decides how fast your game moves
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g

data Cell = RoboT |Block | Empty
data Tick = Tick
type Name = ()

app :: App GridConfig Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appStartEvent = return
          , appHandleEvent = handleEvent
          , appAttrMap = const theMap
          }

handleEvent :: GridConfig -> BrickEvent Name Tick -> EventM Name (Next GridConfig)
handleEvent g (AppEvent Tick)                       = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = halt g

drawUI :: GridConfig -> [Widget Name]
drawUI g =  [ C.center $ drawCellmem g <+> drawGrid g]


drawCellmem :: GridConfig -> Widget Name
drawCellmem g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Cell Memory")
  $ vBox rowss
  where
    height = (cols $ grid g)
    width = (rows $ grid g)
    rowss         = [hBox $ cellsInRow r | r <- [0..height]]
    cellsInRow y = [drawCoord (x,y) | x <- [0..width-1]]
    drawCoord    = drawval.cellp
    cellp c = c_look!!(fst c)!!(snd c)
    c_look = cellmem (grid g)

drawval :: (Int,Bool) -> Widget Name
drawval n = withAttr cellAttr (str $ " " ++ show n ++ " ")

cellAttr :: AttrName
cellAttr = (A.attrName  "cellAttr")

drawGrid :: GridConfig -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "My Robo")
  $ vBox rowss
  where
    height = cols $ grid g
    width = rows $ grid g
    rowss         = [hBox $ cellsInRow r | r <- [0..height]]
    cellsInRow y = [drawCoord (x,y) | x <- [0..width-1]]
    drawCoord    = drawCell.cellAt
    cellAt c
      |c `elem` (map (\x -> snd x) (robo g)) = RoboT
      |c `elem` (blocks (grid g)) = Block
      |otherwise           = Empty

drawCell :: Cell -> Widget Name
drawCell RoboT  = withAttr roboAttr cw
drawCell Empty = withAttr emptyAttr emptyy
drawCell Block = withAttr blockAttr block

cw :: Widget Name
cw = str " ☺ "

block :: Widget Name
block = str " ∰ "

emptyy :: Widget Name
emptyy = str "  ⃞ "

theMap :: AttrMap
theMap = attrMap V.defAttr  [(emptyAttr, V.black `on` V.blue)  , (blockAttr, V.red `on` V.blue) , (roboAttr, V.black `on` V.blue) ,(cellAttr, V.red `on` V.blue)]

roboAttr :: AttrName
emptyAttr :: AttrName
blockAttr :: AttrName
roboAttr = (A.attrName  "roboAttr")
emptyAttr = (A.attrName "emptyAttr")
blockAttr = (A.attrName "blockAttr")


