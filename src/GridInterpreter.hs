{-# LANGUAGE DeriveDataTypeable #-}

module GridInterpreter where

import Text.JSON.Generic
import Control.Exception as E
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.IO.Class
import System.IO 

data GridConfig = GridConfig { grid::GridData
                             , robo::[(Robo,Pos)]
                             } deriving (Show,Eq,Typeable,Data)

data GridData = Grid { rows::Int
                     , cols::Int
                     , cellmem::[[(Int,Bool)]]
                     , blocks::[Pos]
                     } deriving (Show,Eq,Typeable,Data)

data Robo = Robo { roboname::String
                 , allowBrick::Bool  --New Feature in Grid that Robot allow to pass through brick or not 
                 , robomem::Int
                 } deriving (Show,Eq,Typeable,Data)

data CellObject = ROBOT | BRICK | SPACE | WALL deriving (Show,Eq,Typeable,Data)

data Input = LEFT | RIGHT | UP | DOWN | READ | WRITE | NONE | END deriving (Eq,Show,Typeable,Data,Read)
type Pos = (Int,Int)
type RoboConf = ((Int,Int,Int),Bool)
------------------------------------ Pure Game Logic , how the grid is changing -------------------------------------------------

checkPos (x,y) (GridConfig (Grid row col cellm bricks) robos) | or(map (\z -> (x,y)==snd(z)) robos) = ROBOT
                                                        | elem (x,y) bricks = BRICK
                                                        | and[(y > 0),(y<=row),(x> 0),(x<=col)] = SPACE
                                                        | otherwise = WALL

----------------------------------------
changeGrid :: StateT (GridConfig,[(String,Input)]) IO GridConfig -- changing the grid according to robot commands
changeGrid = do 
          (g,xs) <- get 
          case xs of
                [] -> return g
                (inp:inpls) -> do
                           case (snd inp) of
                              END -> do 
                                     liftIO $ removeRobName (fst inp)
                                     put ((g {robo = removeRobo (robo g) (fst inp)}),inpls)
                                     changeGrid
                              NONE -> put (g,inpls) >> changeGrid
                              READ -> do
                                      put ((g {robo = updateRoboMem (grid g) (robo g) (fst inp)}),inpls)
                                      hdl <- liftIO $ runReaderT tryTillsuccess (openFile,((fst inp) ++ ".mem"),WriteMode)
                                      liftIO $ hPutStr hdl (show $ newRobomem (grid g) (robo g) (fst inp))
                                      liftIO $ hClose hdl
                                      changeGrid 
                              WRITE -> put((g {grid = updateCellMem (grid g) (findRobo (robo g) (fst inp))}),inpls) >> changeGrid
                              _ -> case isValidMove g inp of
                                     Right (SPACE,newPos) -> put ((g {robo = updateRobo newPos (robo g) (fst inp) }),inpls) >> changeGrid
                                     Right (BRICK,newPos) -> put ((g {robo = chkChange newPos (robo g) (fst inp)}),inpls)  >> changeGrid
                                     Left _ -> put (g,inpls) >> changeGrid

chkChange:: Pos -> [(Robo,Pos)] -> String -> [(Robo,Pos)] -- function that check that robo is allowed to brick or not than change the position of robot
chkChange newPos robos rn = map(\((Robo n t m),x) -> if (n == rn && t == True) then ((Robo n t m),newPos) else ((Robo n t m),x)) robos

updateRobo :: Pos -> [(Robo,Pos)] -> String -> [(Robo,Pos)] -- updating robot position if there is space on new pos
updateRobo newPos robos rn = map (\((Robo n t m),x) -> if n == rn then ((Robo n t m),newPos) else ((Robo n t m),x)) robos

updateRoboMem :: GridData -> [(Robo,Pos)] -> String -> [(Robo,Pos)] -- reading from cell memory
updateRoboMem (Grid a b c d) robos rn = map (\((Robo n t m),x) -> if n == rn then ((Robo n t (fst(c!!(fst(x))!!(snd(x))))),x) else ((Robo n t m),x)) robos

newRobomem :: GridData -> [(Robo,Pos)] -> String -> Int
newRobomem (Grid a b c d) robos rn | [] == rls = 0
                                   | otherwise = fst(c!!(fst(snd(head(rls))))!!(snd(snd(head(rls)))))
 where 
  rls = (filter (\((Robo n t m),x) -> n == rn) robos) 
 
findRobo :: [(Robo,Pos)] -> String -> (Robo,Pos) 
findRobo robos rn = head $ filter (\((Robo n t m),x) -> n == rn) robos

updateCellMem :: GridData -> (Robo,Pos) -> GridData -- writing to cell memory
updateCellMem (Grid a b c d) ((Robo n t y),x) = Grid a b (zipWith (\p q-> if fst(x) /= q then p else (zipWith (\r s -> if s /= snd(x) then r else (y,(snd r))) p [0,1..]) ) c [0,1..]) d

removeRobo :: [(Robo,Pos)] -> String -> [(Robo,Pos)] -- removing robot from grid
removeRobo robos rn = filter (\((Robo n t m),x) -> n /= rn) robos

--------------------------------------- checking validation and than giving positions -------------------------------------------------------------------

isValidMove::GridConfig -> (String,Input) -> Either String (CellObject,Pos)
isValidMove g inp = case isValidPos.checkPos newPos$g of
                       (False,BRICK) -> Right (BRICK,newPos)
                       (True,SPACE) -> Right (SPACE,newPos)
                       _             -> Left "not Valid move"
 where
  newPos = modifyPos (snd inp) oldPos
  oldPos = snd $ head $ filter (\x -> (roboname (fst x))== (fst inp)) (robo g)

isValidPos::CellObject->(Bool,CellObject) 
isValidPos x = (x==SPACE,x)

modifyPos::Input -> Pos -> Pos -- modifing position according to direction
modifyPos move (x,y) = case move of
                        LEFT  -> (x-1,y) ; RIGHT -> (x+1,y)
                        DOWN  -> (x,y+1) ; UP    -> (x,y-1)
                        NONE -> (x,y)

--------------------------------------------Two types of exceptions are handled here-------------------------------------------------

tryTillsuccess :: ReaderT ((FilePath -> IOMode -> IO Handle),FilePath,IOMode) IO Handle
tryTillsuccess = do
  (opr,path,mode) <- ask
  x <- liftIO $ (E.try (opr path mode) :: IO (Either E.IOException Handle))
  case x of
    Left _ -> (liftIO $ threadDelay 1000 ) >> tryTillsuccess 
    Right hdl -> (liftIO $ putStrLn ("opened " ++ path)) >> return hdl

tryGetLine :: ReaderT ((Handle -> IO String),Handle) IO String -- exception handling for getline
tryGetLine = do
  (opr,hdl) <- ask
  let x = E.try (opr hdl) :: IO (Either E.IOException String)
  y <- liftIO x
  case y of
    Left _ -> (liftIO $ threadDelay 1000) >> tryGetLine
    Right st -> return st
 
------------------------------------------- Bool functions ----------------------------------------------

isWall :: Input -> GridConfig -> Pos -> Bool -- checking that there is wall or brick on that position
isWall ip g p = case (checkPos (modifyPos ip p) g) of
                  ROBOT -> False
                  BRICK -> True
                  WALL -> True
                  SPACE -> False

isLock :: GridConfig -> Pos -> Bool -- checking that there is (x,Bool) at position so returning the Bool vale
isLock g p = snd ((cellmem (grid g))!!(fst p)!!(snd p))

----------------------------------------------------------------------------------------------------------

--sendRequest :: [(Robo,Pos)] -> IO Bool -- sending request to all robots
sendRequest :: StateT [(Robo,Pos)] IO Bool
sendRequest = do
  robos <- get
  case robos of
    [] -> return True
    _ -> do
      hdl <- liftIO $ runReaderT tryTillsuccess (openFile,((roboname (fst $ head robos)) ++ ".rqst"),WriteMode)
      liftIO $ hPutStr hdl "YES"
      liftIO $ hClose hdl
      put (tail robos)
      sendRequest

pickcmd :: GridConfig -> (Robo,Pos) -> IO Input
pickcmd g r = do
  hdl <- runReaderT tryTillsuccess (openFile,((roboname (fst r)) ++ ".cmd"),ReadMode) -- if command is there than recive it 
  res <- runReaderT tryGetLine (hGetLine,hdl)
  putStrLn ("hello \n" ++ res)
  hClose hdl
  case res of
    "NO" -> do
      hdl <- runReaderT tryTillsuccess (openFile,((roboname (fst r)) ++ ".cbc"),ReadMode) -- else checking for callback 
      cb <- runReaderT tryGetLine (hGetLine,hdl)
      hClose hdl
      case cb of -- callback things for whitch robot asking to grid
        "IsLock" -> cbfn (show $ isLock g (snd r)) r 
        "IsWall LEFT" -> cbfn (show $ isWall LEFT g (snd r)) r
        "IsWall RIGHT" -> cbfn (show $ isWall RIGHT g (snd r)) r
        "IsWall UP" -> cbfn (show $ isWall UP g (snd r)) r
        "IsWall DOWN" -> cbfn (show $ isWall DOWN g (snd r)) r
        _ -> return NONE -- if robot not asked than do nothing
    x -> return (myread x) -- if command is there than return it

cbfn :: String -> (Robo,Pos) -> IO Input
cbfn s r = do
  hdl <- runReaderT tryTillsuccess (openFile,((roboname (fst r)) ++ ".cbr"),WriteMode) -- checking for call back result
  hPutStr hdl s
  hClose hdl
  threadDelay 3000
  hdl <- runReaderT tryTillsuccess (openFile,((roboname (fst r)) ++ ".cmd"),ReadMode) -- if yes than check what to do with cmd
  rs <- runReaderT tryGetLine (hGetLine,hdl)
  hClose hdl
  case rs of
    "NO" -> return NONE -- if not sended command untill than do nothing
    x -> putStrLn x >> return (myread x) -- returning command

myread x = do 
  case x of
    "LEFT" -> LEFT
    "RIGHT" -> RIGHT
    "UP" -> UP
    "DOWN" -> DOWN
    "READ" -> READ
    "WRITE" -> WRITE 
    "END" -> NONE
    "NONE" -> NONE

------------------------------------
receiveCmds :: GridConfig -> [(Robo,Pos)] -> WriterT [(String,Input)] IO Int -- reciving all robots command
receiveCmds g [] = return 1
receiveCmds g (r:robos) = do
  cmd <- liftIO $ pickcmd g r -- reciving command from a perticuler robot
  tell [((roboname (fst r)),cmd)] -- storing values of inputs
  receiveCmds g robos  

takecommands :: GridConfig -> IO [(String,Input)] -- take all robot commands with name [(name,command)]
takecommands g = do
  runStateT sendRequest (robo g) -- requesting every robot to send next command
  threadDelay 3000
  a <-  (runWriterT (receiveCmds g (robo g))) -- after some time reciving commands from robots
  return (snd a)
------------------------------------------------ robo registration -----------------------------------------------------

chkPresence:: [(Robo,Pos)] -> String -> Bool
chkPresence [] r = False
chkPresence ((s,p):robos) r | (roboname s) == r = True
                            | otherwise = chkPresence robos r

putrobo :: GridConfig -> String -> IO GridConfig -- asking the user Where he/she wants to put Robot in Grid and Other Things about Robot
putrobo g r = do
{-  putStrLn ("Where you want to put robot " ++ r ++ " \n Enter (x coord)::Integer value")
  x <- getLine
  putStrLn "Enter (y coord)::Integer value"
  y <- getLine
  putStrLn "Enetr Robot Memory (robomem)::Integer value"
  rm <- getLine
  putStrLn "Enter That Robot Allowed to Pass Througt Brick or Not (True/False)::Bool Value"
  t <- getLine 
-}
  hdl <- runReaderT tryTillsuccess (openFile,r ++ ".conf",ReadMode) 
  inp <- runReaderT tryGetLine (hGetLine,hdl)
  hClose hdl
  ((x,y,rm),t) <- return (decodeJSON (inp :: String) :: RoboConf)
  return (g {robo = (((Robo r (t::Bool) (rm::Int)),(x::Int,y::Int)):(robo g)) })

placerobos :: StateT (GridConfig,[String]) IO GridConfig 
placerobos = do
  (g,robos) <- get 
  case robos of 
    [] -> return g
    _ -> do
      case (chkPresence (robo g) (head robos)) of
        True -> do
          put (g,(tail robos))
          placerobos
        False -> do
          h <- liftIO $ putrobo g (head robos)
          put (h,(tail robos))
          placerobos

removeRobName :: String -> IO () -- Removing Robot name from "Robonames" file
removeRobName rn = do
  hdl <- runReaderT tryTillsuccess (openFile,"RoboNames",ReadMode)
  ros <- hGetContents hdl
  robos <- return ros
  hClose hdl
  hdl <- runReaderT tryTillsuccess (openFile,"RoboNames",WriteMode)
  hPutStr hdl (unlines (filter (\x-> x/=rn) (lines robos)))
  hClose hdl

-----------------------------------------------------------------------------------------------------------

gridChanger :: GridConfig -> IO GridConfig  -- after one command of every Robot changing the grid
gridChanger g = do
  hdl <- runReaderT tryTillsuccess (openFile,"RoboNames",ReadMode) 
  robos <- hGetContents hdl
  print (lines robos)
  h <- runStateT placerobos (g,(lines robos)) -- if new robot comes (register) than palce the robo in Grid 
  hClose hdl
  cmds <- takecommands (fst h) -- to take Commands for all robots to execute
  gh <- runStateT changeGrid ((fst h),cmds) -- changing the GridConfig according to all robots
  return $ fst gh

gridInterpreter :: GridConfig -> IO () -- communicating with UI
gridInterpreter g = do
  hdl <- runReaderT tryTillsuccess (openFile,"request.gi",ReadMode) 
  req <- runReaderT tryGetLine (hGetLine,hdl) 
  putStrLn req
  hClose hdl
  hdl <- runReaderT tryTillsuccess (openFile,"request.gi",WriteMode) -- if UI requested then Remove the request and do Work for UI
  hPutStr hdl "NO"
  hClose hdl
  case req of
    "YES" -> do
      h <- gridChanger g
      hdl <- runReaderT tryTillsuccess (openFile,"result.gi",WriteMode) 
      hPutStr hdl (encodeJSON (h :: GridConfig))
      hClose hdl
      gridInterpreter h
    _ -> putStr "hello " >> threadDelay 30000 >> gridInterpreter g

mygame :: IO () -- run this function for GridInterpreter 
mygame = do
  hSetEcho stdin False
  hdl <- runReaderT tryTillsuccess (openFile,"GridConfig.JSON",ReadMode) -- taking Starting Configuration of World(Grid)
  inpg <- runReaderT tryGetLine (hGetLine,hdl)
  hClose hdl
  (putStrLn.show) (decodeJSON (inpg :: String) :: GridConfig)
  gridInterpreter (decodeJSON (inpg :: String) :: GridConfig) -- it will tackle with further changes in grid

