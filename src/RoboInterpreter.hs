module RoboInterpreter where

import System.IO
import Text.JSON.Generic
import Control.Exception as E
import Control.Monad
import Control.Concurrent
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Monad.State
import Control.Monad.IO.Class

data Cond = BoolConst Bool
          | IsWall Direction
          | IsLock
            deriving (Eq,Show)

data Direction = LEFT | RIGHT | UP | DOWN deriving(Eq,Show)

data Opration = READ | WRITE | NONE deriving(Eq,Show)

data FlatLine = If Cond JumpLineNo   |
                Else  JumpLineNo     |
                Move Direction       |
                Opr Opration         |
                Fcall FunctionName   |
                Ret                  |
                BlockStart           |
                BlockEnd             |
                ReturnEnd            |
                Fstart FunctionName deriving (Show,Eq)

type FunctionName = String
type JumpLineNo = Int
type LineNo = Int
type InstPointer = Int
type Fstart = LineNo
type Fend = LineNo
type FuncDetails = [(FunctionName,Fstart,Fend)]
type FlatList = [(LineNo,FlatLine)]
type Stack = [LineNo]
type RoboMem = Int
type RoboConf = ((Int,Int,Int),Bool)
--------------------------------------------------------------------------------------------------------------------------
funcInfo :: FlatList -> FuncDetails -> IO FuncDetails
funcInfo [] st = return st
funcInfo ((x,y):prog) st = case y of
                                Fstart fname -> funcInfo prog (st ++ [(fname,(findBlockStart prog),(findBlockEnd prog 0 ))])
                                _ -> funcInfo prog st

findBlockStart :: FlatList -> Fstart
findBlockStart  [] = 0
findBlockStart  ((x,y):prog) = case y of
                                  BlockStart -> x
                                  _ -> findBlockStart prog

findBlockEnd :: FlatList -> Int ->  Fend
findBlockEnd  []  i = 0
findBlockEnd  ((x,y):prog) i = case y of
                                  BlockStart -> findBlockEnd prog (i+1)
                                  BlockEnd -> case i of
                                                1 -> x
                                                _-> findBlockEnd prog (i-1)
                                  _ -> findBlockEnd prog i

findjumps :: FlatList -> FlatList -> IO FlatList 
findjumps [] st = return st
findjumps ((x,y):prog) st = case y of
                                If c n -> findjumps prog (st ++ [(x,If c ((findBlockEnd prog 0)+2))])
                                Else n -> findjumps prog (st ++ [(x,Else ((findBlockEnd prog 0)+1))])
                                _ -> findjumps prog (st ++ [(x,y)])

--------------------------------------------------------------------------------------------------------------------------

languageDef =
  emptyDef { Token.reservedNames   = ["move","Left","Right","Up","Down","Read","Write","if","else","True","False","IsLock","IsWall","return","mReturn"]}

lexer = Token.makeTokenParser languageDef

reserved   = Token.reserved   lexer -- parses a reserved name
parens     = Token.parens     lexer -- parses surrounding parenthesis
braces     = Token.braces     lexer -- parses surrounding braces
iden       = Token.identifier lexer -- for function names
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

-----------------------------------------------------------------------------------------
rbtParser :: [(Int,String)] -> FlatList -> IO FlatList
rbtParser [] fl = return fl
rbtParser ((x,y):zs) fl = do 
  case fltLineParse y of 
    Left e -> fail ("Error : Line no " ++ (show x))
    Right r -> rbtParser zs (fl++[(x,r)])

fltLineParse :: String -> Either ParseError FlatLine
fltLineParse y | "" == y = return (Opr NONE)
               | elem '-' y = return (Opr NONE)
               | elem '{' y = return BlockStart
               | elem '}' y = return BlockEnd
               | elem ';' y = parse fltLineParse1 "" (filter (\x -> x /= ';') y)
               | otherwise = parse fltLineParse2 "" y

fltLineParse1 = whiteSpace >> statement'
fltLineParse2 = whiteSpace >> statement

statement :: Parser FlatLine
statement = funcStartStmt
          <|> statement'

statement' :: Parser FlatLine
statement' =  moveStmt
           <|> ifStmt
           <|> (reserved "mReturn" >> return ReturnEnd)
           <|> (reserved "return" >> return Ret)
           <|> (reserved "else" >> return (Else (-1))) 
           <|> (reserved "Read" >> return (Opr READ))
           <|> (reserved "Write" >> return (Opr WRITE))
           <|> funcCallStmt

funcStartStmt :: Parser FlatLine
funcStartStmt = do
   n <- iden
   p <- parens pr
   return $ Fstart n

funcCallStmt = do 
   n <- iden
   p <- parens pr
   return $ Fcall n

pr :: Parser Opration 
pr = return NONE

dirStmt :: Parser Direction
dirStmt = (reserved "Left" >> return LEFT)
        <|> (reserved "Right" >> return RIGHT)
        <|> (reserved "Up">> return UP)
        <|> (reserved "Down" >> return DOWN)

condStmt :: Parser Cond
condStmt = iswallStmt
         <|> (reserved "IsLock" >> return IsLock)
         <|> (reserved "True" >> return (BoolConst True))
         <|> (reserved "False" >> return (BoolConst False))

moveStmt :: Parser FlatLine
moveStmt =
  do reserved "move"
     x <- dirStmt
     return $ Move x

ifStmt :: Parser FlatLine 
ifStmt =
  do reserved "if"
     cond  <- parens condStmt
     return $ If cond (-1)

iswallStmt:: Parser Cond
iswallStmt =
  do reserved "IsWall"
     dir <- dirStmt
     return $ IsWall dir

---------------------------------------------------------------------------------------------------
myfst :: (a,b,c) -> a
myfst (x,y,z) = x

mysnd :: (a,b,c) -> b
mysnd (x,y,z) = y

mythrd :: (a,b,c) -> c
mythrd (x,y,z) = z
---------------------------------------------------------------------------------------------------
parseFile :: String -> IO ()
parseFile rbt_program = do
  p <- readFile rbt_program
  hdl <- tryTillsuccess openFile "RoboNames" AppendMode
  hPutStr hdl (rbt_program ++ "\n")
  hClose hdl
  hdl <- emptyFile (rbt_program ++ ".cmd") 
  hdl <- emptyFile (rbt_program ++ ".cbc") 
  hdl <- emptyFile (rbt_program ++ ".cbr") 
  hdl <- emptyFile (rbt_program ++ ".rqst")
  hdl <- emptyFile (rbt_program ++ ".mem")
  r <- rbtParser (zip [1..] (lines p)) [] 
  fl <- findjumps r []
  fd <- funcInfo fl []
  print fl 
  print fd
  crrI <- return $ (mysnd.head) $ filter (\(x,y,z)-> x=="main") fd
  print crrI 
  hdl <- tryTillsuccess openFile (rbt_program ++ ".conf") ReadMode
  inp <- tryGetLine hGetLine hdl
  hClose hdl
  ((x,y,rm),t) <- return (decodeJSON (inp :: String) :: RoboConf)  
  runStateT myInterpreter (([],crrI),fl,fd,rbt_program,rm)  -- interpreter function
  putStrLn "i have done"

---------------------------------- Exception handled here ----------------------------------------

tryTillsuccess :: (FilePath -> IOMode -> IO Handle) -> FilePath -> IOMode -> IO Handle
tryTillsuccess opr path mode = do
  x <- E.try (opr path mode) :: IO (Either E.IOException Handle)
  case x of
    Left _ -> threadDelay 1000 >> tryTillsuccess opr path mode
    Right hdl -> putStrLn ("opened " ++ path) >> return hdl

tryGetLine :: (Handle -> IO String) -> Handle -> IO String
tryGetLine opr hdl = do
  x <- E.try (opr hdl) :: IO (Either E.IOException String)
  case x of
    Left _ -> threadDelay 1000 >> tryGetLine opr hdl
    Right st -> return st
--------------------------------------------------------------------------------------------------
emptyFile :: FilePath -> IO ()
emptyFile path = do
  hdl <- tryTillsuccess openFile path WriteMode
  hPutStrLn hdl "NO"
  hClose hdl
-------------------------------------------------------------------------------------------------
sendCommand :: String -> String -> IO ()
sendCommand x filename = do
  hdl <- tryTillsuccess openFile filename WriteMode
  putStrLn x
  hPutStr hdl x
  hClose hdl

wait :: String -> IO ()
wait filename = do
  hdl <- tryTillsuccess openFile filename ReadMode
  s <- tryGetLine hGetLine hdl
  hClose hdl
  print s
  case s of
    "YES" -> do
      hdl <- tryTillsuccess openFile filename WriteMode
      hPutStr hdl "NO"
      hClose hdl
    _ -> threadDelay 1000 >> wait filename

cbChk :: String -> String -> IO Bool
cbChk x rbt_name = do
  hdl <- tryTillsuccess openFile (rbt_name ++ ".cbc") WriteMode
  hPutStr hdl x
  hClose hdl
  chkCbRes rbt_name

chkCbRes :: String -> IO Bool
chkCbRes rbt_name = do
  hdl <- tryTillsuccess openFile (rbt_name ++ ".cbr") ReadMode
  s <- tryGetLine hGetLine hdl
  hClose hdl
  case s of
    "True" -> do
      hdl <- tryTillsuccess openFile (rbt_name ++ ".cbr") WriteMode
      hPutStr hdl "NO"
      hClose hdl
      return True
    "False" -> do
      hdl <- tryTillsuccess openFile (rbt_name ++ ".cbr") WriteMode
      hPutStr hdl "NO"
      hClose hdl
      return False
    _ -> threadDelay 1000 >> chkCbRes rbt_name

----------------------------------------------------------------------------------------------------------------------

--myInterpreter :: ((Stack,InstPointer),FlatList,FuncDetails) -> String -> IO ()
myInterpreter :: StateT ((Stack,InstPointer),FlatList,FuncDetails,String,RoboMem) IO ()
myInterpreter = do
  ((st,ip),fl,fd,rp,rm) <- get
  case fl!!(ip-1) of
    (l,Move dir) -> do 
          liftIO $ wait (rp ++ ".rqst")
          liftIO $ sendCommand (show dir) (rp ++ ".cmd")
          put ((st,(ip+1)),fl,fd,rp,rm) >> myInterpreter
    (l,Opr WRITE) -> do
          liftIO $ wait (rp ++ ".rqst")
          liftIO $ sendCommand "WRITE" (rp ++ ".cmd")
          put ((st,(ip+1)),fl,fd,rp,rm) >> myInterpreter
    (l,Opr READ) -> do
          liftIO $ wait (rp ++ ".rqst")
          liftIO $ sendCommand "READ" (rp ++ ".cmd")
          liftIO $ threadDelay 1000 
          hdl <- liftIO $ tryTillsuccess openFile (rp ++ ".mem") ReadMode
          rm' <- liftIO $ tryGetLine hGetLine hdl
          liftIO $ hClose hdl
          liftIO $ putStrLn rm'
          put ((st,(ip+1)),fl,fd,rp,(read rm':: Int)) >> myInterpreter          
    (l,Fcall fname) -> put ((st',ip'),fl,fd,rp,rm) >> myInterpreter
      where
        st' = st ++ [(ip+1)]
        ip' = (mysnd.head) $ filter (\(x,y,z)-> x==fname) fd
    (l,Ret) -> put ((st',ip'),fl,fd,rp,rm) >> myInterpreter
      where
        st' = if st == [] then [] else (init st)
        ip' = last st
    (l,ReturnEnd) -> do
          liftIO $ wait (rp ++ ".rqst")
          liftIO $ sendCommand "END" (rp ++ ".cmd")
          liftIO $ putStrLn "END"
    (l,If c n) -> do
       case c of
         BoolConst True -> put ((st,(ip+1)),fl,fd,rp,rm) >> myInterpreter
         BoolConst False -> put ((st,n),fl,fd,rp,rm) >> myInterpreter
         _ -> do
           cbres <- liftIO $ cbChk (show c) rp
           case cbres of 
             True -> put ((st,ip+1),fl,fd,rp,rm) >> myInterpreter
             False -> put ((st,n),fl,fd,rp,rm) >> myInterpreter
    (l,Else n) -> put ((st,n),fl,fd,rp,rm) >> myInterpreter
    _ -> put ((st,ip+1),fl,fd,rp,rm) >> myInterpreter 



