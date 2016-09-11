import System.IO
import Data.Char (chr)
import Parsing
import Data.Char

--Definition of the data Expr 
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Sqr Expr
          | Neg Expr
          | Val Int deriving (Show)

-- Problem 1
-- 0 cannot be the divisor in Div or Mod.
-- Evaluates Expr to Maybe Int 
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


-- Problem 2
-- Parsing a string to an Expr 
-- Grammar 
-- expr -> term op term
expr :: Int -> Parser Expr
expr a = do x <- term a
            (do y <- op_term a x
                return y) +++ return x

-- Grammar
-- op term -> ε | “+” term op term | “-” term op term
op_term :: Int -> Expr -> Parser Expr
op_term a ex = (do char '+'
                   y <- term a
                   (do z <- op_term a (Add ex y)
                       return z) +++ return (Add ex y)) +++ (do char '-'
                                                                y <- term a
                                                                (do z <- op_term a (Sub ex y)
                                                                    return z) +++ return (Sub ex y))

-- Grammar
-- term -> factor op factor
term :: Int -> Parser Expr
term a = do x <- factor a
            (do y <- op_factor a x
                return y) +++ return x

-- Grammar
-- op factor -> ε | “*” factor op factor | “/” factor op factor | “%” factor op factor
op_factor :: Int -> Expr -> Parser Expr 
op_factor a ex = (do char '*'
                     y <- factor a
                     (do z <- op_factor a (Mul ex y)
                         return z) +++ return (Mul ex y)) +++ (do char '/'
                                                                  y <- factor a
                                                                  (do z <- op_factor a (Div ex y) 
                                                                      return z) +++ return (Div ex y)) +++ (do char '%'
                                                                                                               y <- factor a
                                                                                                               (do z <- op_factor a (Mod ex y)
                                                                                                                   return z) +++ return (Mod ex y))
-- Grammar
-- factor -> “sqr(” expr “)” | “neg(” expr “)” | “(” expr “)” | nat                                                                                                                  
factor :: Int -> Parser Expr
factor a = (do string "sqr("
               e <- expr a
               char ')'
               return (Sqr e)) +++ (do string "neg("
                                       e' <- expr a
                                       char ')'
                                       return (Neg e')) +++ (do char '('
                                                                e'' <- expr a
                                                                char ')'
                                                                return e'')  +++ (if (a==0) then 
                                                                                        (do n <- natural
                                                                                            return (Val n))
                                                                                  else (do n <- hex 
                                                                                           return (Val (hexToDec n)))) 

--Parsers to Parse for a Hexadecimal String 
hex :: Parser String
hex =  token hexString

--Parsers to Parse for a Hexadecimal String 
hexString :: Parser String
hexString =  do xs <- many1 hexchar
                return (xs)

--Parsers to Parse for a Hexadecimal String 
hexchar :: Parser Char 
hexchar = (do char 'a'
              return 'a') +++ (do char 'b'
                                  return 'b') +++ (do char 'c'
                                                      return 'c') +++ (do char 'd'
                                                                          return 'd') +++ (do char 'e'
                                                                                              return 'e') +++ (do char '1'
                                                                                                                  return '1') +++ (do char '2'
                                                                                                                                      return '2') +++ (do char '3'
                                                                                                                                                          return '3') +++ (do char '4'
                                                                                                                                                                              return '4')+++(do char '5'
                                                                                                                                                                                                return '5') +++ (do char '6'
                                                                                                                                                                                                                    return '6') +++ (do char '7'
                                                                                                                                                                                                                                        return '7') +++ (do char '8'
                                                                                                                                                                                                                                                            return '8') +++ (do char '9'
                                                                                                                                                                                                                                                                                return '9') +++ (do char '0'
                                                                                                                                                                                                                                                                                                    return '0') +++ failure 

-- Type Definition of Pos 
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

-- Creating the Box calculator 
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
          calc 0 ""

-- Listen for input
calc :: Int -> String -> IO ()
calc a xs = do displayExpr a xs
               c <- getCh
               process a c xs


-- Problem 3 Part I
-- Displays the expression being typed out 
displayExpr :: Int -> String -> IO () 
displayExpr a xs = do goto (2,1)
                      putStr "                "
                      if(length xs > 14) then do goto (2,1)
                                                 putStr ">" 
                                                 putStr (reverse (take 14 (reverse xs))) else do goto(2,17-(length xs)) 
                                                                                                 putStr xs


-- Problem 3 Part II
-- Processing the input  
process :: Int -> Char -> String -> IO ()
process a c xs
  | c == 'Q' = goto (boxHeight + 1, 1)
  | c == 'D' = if (length xs >0) then calc a (init xs) else calc a ""
  | c == 'C' = calc a ""
  | c == '=' =  eval a xs
  | c == 'M' = changeMode a 
  | elem c buttons = calc a (xs++[c])
  | otherwise = calc a xs

-- Change mode 0 -> DEC 1 -> HEX  
changeMode :: Int -> IO ()
changeMode a = if (a==0) then do writeAt (0,3) "HEX"
                                 writeAt (3,1) "                "
                                 calc 1 ""
               else do writeAt (0,3) "DEC"
                       writeAt (3,1) "                "
                       calc 0 ""

-- Problem 3 Part III
-- Evaluate the expression entered 
eval :: Int -> String -> IO ()
eval a xs = do if (check (parse (expr a) xs)) then do goto (3,1) 
                                                      putStr "                "
                                                      writeAt (3,12) "ERROR"
               else 
                  if (build a xs == "") then case (evalExpr (buildfirst a xs)) of 
                                               (Just e) ->  if (a==0) then displayExpr2 a (show e) else displayExpr2 a (decToHex e)
                                               _ -> do goto (3,1)
                                                       putStr "                "
                                                       writeAt (3,14) "NaN"
                                else 
                                   do goto (3,1) 
                                      putStr "                "
                                      writeAt (3,12) "ERROR"
               calc a xs

-- Check for empty list 
check :: [(Expr,String)] -> Bool
check [] = True
check _ = False

-- Display the result 
displayExpr2 :: Int -> String -> IO ()
displayExpr2 a xs = do goto (3,1)
                       putStr "                "
                       if(length (xs) > 14) then do goto (3,1)
                                                    putStr ">" 
                                                    putStr (reverse (take 14 (reverse (xs)))) else do goto(3,17-(length (xs))) 
                                                                                                      putStr (xs)
                     
-- Check for String in [(Expr,String)]
build :: Int -> String -> String
build a = snd . head . parse (expr a)

-- Check for Expr in [(Expr,String)]
buildfirst :: Int -> String -> Expr
buildfirst a = fst . head . parse (expr a)

-- Problem 4 Part I
-- Convert Decimal to Hexadecimal 
decToHex :: Int -> String
decToHex x = ((if((x `div` 16)>0) then (decToHex (x `div` 16)) else [])++[(decChar (x `mod` 16 ))])

-- Convert Hexadecimal to Decimal 
hexToDec :: String -> Int
hexToDec [] = 0
hexToDec hxStr = hexChar (last hxStr) + 16 * (hexToDec (init hxStr))

--Converts char to int 
hexChar :: Char -> Int 
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'a' = 10
    | ch == 'B' = 11
    | ch == 'b' = 11
    | ch == 'C' = 12
    | ch == 'c' = 12
    | ch == 'D' = 13
    | ch == 'd' = 13
    | ch == 'E' = 14
    | ch == 'e' = 14
    | ch == 'F' = 15
    | ch == 'f' = 15
    | otherwise  = 0

-- Converts int to char 
decChar :: Int -> Char 
decChar ch
    | ch == 0 = '0'
    | ch == 1 = '1'
    | ch == 2 = '2'
    | ch == 3 = '3'
    | ch == 4 = '4'
    | ch == 5 = '5'
    | ch == 6 = '6'
    | ch == 7 = '7'
    | ch == 8 = '8'
    | ch == 9 = '9'
    | ch == 10 = 'a'
    | ch == 11 = 'b'
    | ch == 12 = 'c'
    | ch == 13 = 'd'
    | ch == 14 = 'e'
    | ch == 15 = 'f'
    | otherwise = '0'
-- Problem 4 Part II & III: DONE 
-- M for switching modes: "DEC" and "HEX".
-- Needs modification on existing code.
-- Hint: add an argument for mode and pass it throughout the interaction and calculation.

