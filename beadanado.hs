
module NagyBead where

import Data.Either
import Data.Maybe
import Data.List (dropWhile, dropWhileEnd, partition)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Data.Char (isSpace)

basicInstances = 0 -- Mágikus tesztelőnek kell ez, NE TÖRÖLD!

type OperatorTable a = [(Char, (a -> a -> a, Int, Dir))]

tAdd, tMinus, tMul, tDiv, tPow :: (Floating a) => Tok a
tAdd = TokBinOp (+) '+' 6 InfixL
tMinus = TokBinOp (-) '-' 6 InfixL
tMul = TokBinOp (*) '*' 7 InfixL
tDiv = TokBinOp (/) '/' 7 InfixL
tPow = TokBinOp (**) '^' 8 InfixR

operatorTable :: (Floating a) => OperatorTable a
operatorTable =
    [ ('+', ((+), 6, InfixL))
    , ('-', ((-), 6, InfixL))
    , ('*', ((*), 7, InfixL))
    , ('/', ((/), 7, InfixL))
    , ('^', ((**), 8, InfixR))
    ]

data Dir = InfixL | InfixR deriving (Show, Eq, Ord)

data Tok a = BrckOpen | BrckClose | TokLit a | TokBinOp (a -> a -> a) Char Int Dir 

instance Show a => Show (Tok a) where 
    show BrckOpen = "BrckOpen"
    show BrckClose = "BrckClose"
    show (TokLit x) = "TokLit " ++ show x
    show (TokBinOp _ symbol prec dir) = "TokBinOp " ++ show symbol ++ " " ++ show prec ++ " " ++ show dir

instance Eq a => Eq (Tok a) where 
    BrckOpen == BrckOpen = True
    BrckClose == BrckClose = True 
    (TokLit x) == (TokLit y) = x == y
    (TokBinOp _ s1 p1 d1) == (TokBinOp _ s2 p2 d2) = s1 == s2 && p1 == p2 && d1 == d2
    _ == _ = False

operatorFromChar :: OperatorTable a -> Char -> Maybe (Tok a)
operatorFromChar [] _ = Nothing
operatorFromChar table symbol = case lookup symbol table of 
    Just (op, prec, dir) -> Just (TokBinOp op symbol prec dir)
    Nothing -> Nothing

getOp :: (Floating a) => Char -> Maybe (Tok a)
getOp = operatorFromChar operatorTable

parse :: String -> Maybe [Tok Double]
parse = parseTokens operatorTable

parseAndEval :: (String -> Maybe [Tok a]) -> ([Tok a] -> ([a], [Tok a])) -> String -> Maybe ([a], [Tok a])
parseAndEval parse eval input = maybe Nothing (Just . eval) (parse input)

parseTokens :: Read a => OperatorTable a -> String -> Maybe [Tok a]
parseTokens ops str = parse' ops (tokenize str)
  where
    tokenize :: String -> [String]
    tokenize [] = []
    tokenize ('\"':xs) = 
      let (s, rest) = span (/= '\"') xs
      in ("\"" ++ s ++ "\"") : tokenize (drop 1 rest)
    tokenize (x:xs) 
      | x == ' ' = tokenize xs
      | otherwise = (x:takeWhile (/= ' ') xs) : tokenize (dropWhile (/= ' ') xs)

    parse' :: Read a => OperatorTable a -> [String] -> Maybe [Tok a]
    parse' _ [] = Just []
    parse' ops (x:xs)
      | isParenthesesString x = 
          let tokens = map toParenToken x
          in (tokens ++) <$> parse' ops xs 
      | isOperator ops x = case operatorFromChar ops (head x) of
                              Just op -> (op :) <$> parse' ops xs
                              Nothing -> Nothing
      | isLiteral x = case readMaybe x of
                         Just lit -> (TokLit lit :) <$> parse' ops xs 
                         Nothing -> Nothing
      | otherwise = Nothing
    
    toParenToken :: Char -> Tok a
    toParenToken '(' = BrckOpen
    toParenToken ')' = BrckClose
    toParenToken _ = error "Invalid parenthesis"

    isParenthesesString :: String -> Bool
    isParenthesesString = all (`elem` "()") 

    isOperator :: OperatorTable a -> String -> Bool
    isOperator ops x = 
      length x == 1 && 
      case operatorFromChar ops (head x) of
        Just _ -> True
        Nothing -> False
    
    isLiteral :: String -> Bool
    isLiteral s
        | s == "True" || s == "False" = True
        | all isDigit s = True
        | head s == '.' && all isDigit (tail s) = True
        | head s == '-' && all isDigit (tail s) = True
        | isFloat s = True 
        | s == "EQ" || s == "LT" || s == "GT" = True 
        | head s == '"' && last s == '"' = True  
        | head s == '\'' && last s == '\'' = True 
        | otherwise = False

    isFloat :: String -> Bool
    isFloat s
      | null s = False
      | head s == '-' = isValidFloat (tail s)
      | otherwise = isValidFloat s

    isValidFloat :: String -> Bool
    isValidFloat s = 
        let (integer, dotAndFraction) = break (== '.') s
        in not (null integer) && (null dotAndFraction || isDigits (tail dotAndFraction))

    isDigits :: String -> Bool
    isDigits = all (`elem` "0123456789")

syNoEval :: String -> Maybe ([Double], [Tok Double])
syNoEval = parseAndEval parse shuntingYardBasic

syEvalBasic :: String -> Maybe ([Double], [Tok Double])
syEvalBasic = parseAndEval parse (\t -> shuntingYardBasic $ BrckOpen : (t ++ [BrckClose]))

shuntingYardBasic :: [Tok a] -> ([a], [Tok a])
shuntingYardBasic = shuntingYardBasic' [] []

shuntingYardBasic' :: [a] -> [Tok a] -> [Tok a] -> ([a], [Tok a])
shuntingYardBasic' literals ops [] = (literals, ops)
shuntingYardBasic' literals ops (BrckOpen : ts) = shuntingYardBasic' literals (BrckOpen : ops) ts
shuntingYardBasic' literals ops (BrckClose : ts) = let (newLiterals, newOps) = processClosingParen literals ops in shuntingYardBasic' newLiterals newOps ts
shuntingYardBasic' literals ops (TokLit l : ts) = shuntingYardBasic' (l : literals) ops ts
shuntingYardBasic' literals ops (o@(TokBinOp _ _ _ _) : ts) = shuntingYardBasic' literals (o : ops) ts

processClosingParen :: [a] -> [Tok a] -> ([a], [Tok a])
processClosingParen literals (BrckOpen : ops) = (literals, ops)
processClosingParen (x1:x2:literals) (o@(TokBinOp f _ _ _) : ops) = processClosingParen (f x2 x1 : literals) ops
processClosingParen literals _ = error "Mismatched parentheses"

syEvalPrecedence :: String -> Maybe ([Double], [Tok Double])
syEvalPrecedence = parseAndEval parse (\t -> shuntingYardPrecedence $ BrckOpen : (t ++ [BrckClose]))

shuntingYardPrecedence :: [Tok a] -> ([a], [Tok a])
shuntingYardPrecedence tokens = shuntingYardPrecedence' tokens [] []

shuntingYardPrecedence' :: [Tok a] -> [Tok a] -> [a] -> ([a], [Tok a])
shuntingYardPrecedence' [] ops vals = (vals, ops)
shuntingYardPrecedence' (t:ts) ops vals = case t of
  BrckOpen -> shuntingYardPrecedence' ts (BrckOpen : ops) vals
  BrckClose -> let (vals', ops') = evalUntilOpenBrck (vals, ops)
               in shuntingYardPrecedence' ts ops' vals'
  TokBinOp op sym prec dir -> let (vals', ops') = takeHigherPrecedenceOps (vals, ops) (TokBinOp op sym prec dir)
                              in shuntingYardPrecedence' ts (TokBinOp op sym prec dir : ops') vals'
  TokLit x -> shuntingYardPrecedence' ts ops (x : vals)

evalUntilOpenBrck :: ([a], [Tok a]) -> ([a], [Tok a])
evalUntilOpenBrck (vals, BrckOpen : ops) = (vals, ops)
evalUntilOpenBrck (vals, TokBinOp op sym prec dir : ops) = evalUntilOpenBrck (evalOp (TokBinOp op sym prec dir) vals, ops)
evalUntilOpenBrck _ = error "Mismatched parentheses"

takeHigherPrecedenceOps :: ([a], [Tok a]) -> Tok a -> ([a], [Tok a])
takeHigherPrecedenceOps (vals, TokBinOp o symO precO dirO : ops) (TokBinOp _ _ prec dir)
  | precO > prec || (precO == prec && (dirO == InfixL && dir == InfixL || dirO == InfixR && dir == InfixL)) =
    takeHigherPrecedenceOps (evalOp (TokBinOp o symO precO dirO) vals, ops) (TokBinOp undefined undefined prec dir)
  | otherwise = (vals, TokBinOp o symO precO dirO : ops)
takeHigherPrecedenceOps (vals, ops) _ = (vals, ops)

evalOp :: Tok a -> [a] -> [a]
evalOp (TokBinOp op _ _ _) (x:y:ys) = (op y x) : ys
evalOp _ _ = error "Invalid operation"

-shuntingYardPrecedence :: [Tok a] -> ([a], [Tok a])
