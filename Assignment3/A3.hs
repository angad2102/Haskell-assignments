import System.IO
import Data.Char (chr)
import Parsing

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Sqr Expr
          | Neg Expr
          | Val Int deriving (Show)

-- Problem 1: TODO
-- 0 cannot be the divisor in Div or Mod.
evalExpr :: Expr -> Maybe Int

evalExpr (Add e1 e2) = case (evalExpr e1, evalExpr e2) of
                         (Just x, Just y) -> Just (x + y)
                         _                -> Nothing
evalExpr (Sub e1 e2) = case (evalExpr e1, evalExpr e2) of
                        (Just x, Just y) -> Just (x-y)
                        _                -> Nothing
evalExpr (Mul e1 e2) = case (evalExpr e1, evalExpr e2) of
                        (Just x, Just y) -> Just (x*y)
                        _                -> Nothing

evalExpr (Div e1 e2) = case (evalExpr e1, evalExpr e2) of
                        (Just x, Just y) -> if y==0 then Nothing else Just (x`div`y)
                        _                -> Nothing

evalExpr (Mod e1 e2) = case (evalExpr e1, evalExpr e2) of
                        (Just x, Just y) -> if y==0 then Nothing else Just (x`mod`y)
                        _                -> Nothing
evalExpr (Sqr e1) = case (evalExpr e1) of
                        (Just x) ->Just (x*x)
                        _        -> Nothing
evalExpr (Neg e1) = case (evalExpr e1) of
                        (Just x) -> Just (-x)
                        _        -> Nothing
evalExpr (Val e1) = Just e1


-- Problem 2: TODO
expr :: Parser Expr
expr = do x <- term
          (do y <- op_term x
              return y) +++ return x
          

op_term :: Expr -> Parser Expr
op_term ex = (do char '+'
                 y <- term 
                 (do z <- op_term (Add ex y)
                     return z) +++ return (Add ex y)) +++ (do char '-'
                                                              y <- term 
                                                              (do z <- op_term (Sub ex y)
                                                                  return z) +++ return (Sub ex y))

term :: Parser Expr
term = do x <- factor
          (do y <- op_factor x
              return y) +++ return x


op_factor :: Expr -> Parser Expr 
op_factor ex = (do char '*'
                   y <- factor
                   (do z <- op_factor (Mul ex y)
                       return z) +++ return (Mul ex y)) +++ (do char '/'
                                                                y <- factor 
                                                                (do z <- op_factor (Div ex y) 
                                                                    return z) +++ return (Div ex y)) +++ (do char '%'
                                                                                                             y <- factor
                                                                                                             (do z <- op_factor (Mod ex y)
                                                                                                                 return z) +++ return (Mod ex y))
factor :: Parser Expr
factor = (do string "sqr("
             e <- expr
             char ')'
             return (Sqr e)) +++ (do string "neg("
                                     e' <- expr
                                     char ')'
                                     return (Neg e')) +++ (do char '('
                                                              e'' <- expr
                                                              char ')'
                                                              return e'')  +++ (do n <- natural
                                                                                   return (Val n)) 

type Pos = (Int, Int)

-- Set the cursor at position (x, y).
goto :: Pos -> IO ()
goto (x, y) = putStr $ "\ESC[" ++ show x ++ ";" ++ show y ++ "H"

-- Write text xs at position p.
writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

-- Sequencing.
seqn :: [IO a] -> IO ()
seqn []     = return ()
seqn (a:as) = do a
                 seqn as

-- Erase the screen.
cls :: IO ()
cls = putStr "\ESC[2J"

-- Read a single character from keyboard, without echoing to the screen.
getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

box :: [String]
box = ["+-DEC-----------+",
       "|               |",
       "|               |",
       "+---+---+---+---+",
       "| Q | C | D | M |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+",
       "| % |sqr|neg| = |",
       "+---+---+---+---+"]

boxWidth :: Int
boxWidth = 17

boxHeight :: Int
boxHeight = 16 

buttons :: String
buttons = "+-*/%=()QCD" ++ map chr ([48..57] ++ [97..122])

showBox :: IO ()
showBox = seqn [writeAt (y, 1) xs | (y, xs) <- zip [1..boxHeight] box]

main = do cls
          showBox
          calc ""

calc :: String -> IO ()
calc xs = do displayExpr xs
             c <- getCh
             process c xs


-- Problem 3 Part I: TODO
displayExpr :: String -> IO () 
displayExpr xs = do goto (2,1)
                    putStr "                "
                    if(length xs > 14) then do goto (2,1)
                                               putStr ">" 
                                               putStr (reverse (take 14 (reverse xs))) else do goto(2,17-(length xs)) 
                                                                                               putStr xs


-- Problem 3 Part II: TODO
process :: Char -> String -> IO ()
process c xs
  | c == 'Q' = goto (boxHeight + 1, 1)
  | c == 'D' = if (length xs >0) then calc (init xs) else calc ""
  | c == 'C' = calc ""
  | c == '=' =  eval xs
  | elem c buttons = calc (xs++[c])
  | otherwise = calc xs


-- Problem 3 Part III: TODO
eval :: String -> IO ()
eval xs = do if (build xs == "") then case (evalExpr (buildfirst xs)) of 
                                        (Just e) ->  displayExpr2 e
                                        _ -> writeAt (3,14) "NaN"
                                else 
                                   writeAt (3,12) "ERROR"
             calc xs



displayExpr2 :: Int -> IO ()
displayExpr2 xs = do goto (3,1)
                     putStr "                "
                     if(length (show xs) > 14) then do goto (3,1)
                                                       putStr ">" 
                                                       putStr (reverse (take 14 (reverse (show xs)))) else do goto(3,17-(length (show xs))) 
                                                                                                              putStr (show xs)
                     

build :: String -> String
build = snd . head . parse expr

buildfirst :: String -> Expr
buildfirst = fst . head . parse expr

-- Problem 4 Part I: TODO
decToHex :: Int -> String
decToHex = error "TODO!"

hexToDec :: String -> Int
hexToDec = error "TODO!"

-- Problem 4 Part II & III: TODO
-- M for switching modes: "DEC" and "HEX".
-- Needs modification on existing code.
-- Hint: add an argument for mode and pass it throughout the interaction and calculation.

