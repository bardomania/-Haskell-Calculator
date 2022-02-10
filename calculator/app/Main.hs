module Main where

import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitFailure), exitWith )
import Text.Printf ( printf )
import Data.Typeable
import GHC.Real

data Operator = Plus
              | Minus
              | Power
              | Times
              | Div
              | PLeft
              | PRight
              deriving (Show, Eq)

data Eval = Op Operator
        | Number Double
        deriving(Show, Eq)

data Expr = Val Double | Calc Operator Expr Expr

data Result = Result {str :: [String], double :: Double} deriving (Show)

noArgs :: IO ()
noArgs =
    print "No args" >>
    exitWith (ExitFailure 84)

badArgsNumber :: IO ()
badArgsNumber =
    print "More than 1 args" >>
    exitWith (ExitFailure 84)

badArgs :: IO()
badArgs = print "Bad args" >>
        exitWith (ExitFailure 84)

checkArgs :: String -> IO ()
checkArgs [] = return ()
checkArgs (x:xs)
    | isOperator x || isCharDouble x = checkArgs xs 
    | otherwise = badArgs

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
           | c == '^' = Power
           | c == '(' = PLeft
           | c == ')' = PRight

opToStr :: Operator -> String
opToStr Plus = "+"
opToStr Minus = "-"
opToStr Times = "*"
opToStr Power = "^"
opToStr Div = "/"
opToStr PRight = ")"
opToStr PLeft = "("

isCharDouble :: Char -> Bool
isCharDouble x = x `elem` ['0'..'9'] || x == '.'

concatEval:: [Eval] -> Expr
concatEval (Number nb1: Op ope1: Number nb2: xs) = (Calc ope1 (Val nb1) (Val nb2))

parser :: String -> [Eval]
parser [] = []
parser (x:xs) | elem x "+-*/^()" = Op (operator x) : parser xs
             | isCharDouble x = let (numbers, rest) = span isCharDouble (x:xs) in
                                Number (read numbers) : parser rest
             | otherwise = error ("Syntax not respected")

eval ::  Expr -> Double
eval (Val n) = n
eval (Calc Plus x y) = eval x + eval y
eval (Calc Minus x y) = eval x - eval y
eval (Calc Times x y) = eval x * eval y
eval (Calc Div x y) = eval x / eval y
eval (Calc Power x y) = eval x ** eval y

isStringOperator :: String -> Bool
isStringOperator [] = True
isStringOperator (x:xs)
    | isOperator x = isStringOperator xs
    | otherwise = False

isOperatorButParenthese :: Char -> Bool
isOperatorButParenthese operator = operator `elem` "+-*/^"

isStringOperatorButParenthese :: String -> Bool
isStringOperatorButParenthese [] = True
isStringOperatorButParenthese (x:xs)
    | isOperatorButParenthese x = isStringOperatorButParenthese xs
    | otherwise = False

isStringDouble2 :: String -> Bool
isStringDouble2 [] = True
isStringDouble2 (x:[])
    | x == '-' = False
isStringDouble2 (x:xs)
    | x == '-' = isStringDouble xs
    | otherwise = isStringDouble (x:xs)

tokenize :: String -> [String] -> [String]
tokenize [] token = token
tokenize (x:xs) token
    | token == [] && isOperator x == False = tokenize xs (token ++ [[x]] ++ [])
    | length token >= 1 && isCharDouble x && isStringDouble2 (last token) == True = 
    tokenize xs (init token ++ [(last token) ++ [x]])
    | (isCharDouble x && length token == 1 && ((head token)!!0) == '-') = tokenize xs 
    (init token ++ [(last token) ++ [x]])
    | (isCharDouble x && length token >= 2 && isStringOperatorButParenthese 
    (last (init token)) == True && (last token)!!0 == '-' && length 
    (last token) == 1) = tokenize xs (init token ++ [(last token) ++ [x]])
    | isOperator x = tokenize xs (token ++ [[x]] ++ [])
    | isOperator (head(last token)) = tokenize xs (token ++ [[x]] ++ [])
    | otherwise = tokenize xs (init token ++ [(last token) ++ [x]])

isOperator :: Char -> Bool
isOperator operator = operator `elem` "+-*/^()"

stripSpace :: String -> String -> String
stripSpace [] b = b
stripSpace (a:as) b
    | a == ' ' || a == '\t' = stripSpace as b
    | otherwise = stripSpace as (b ++ [a])

isStringDouble :: String -> Bool
isStringDouble [] = True
isStringDouble (x:xs)
    | isCharDouble x == True = isStringDouble xs
    | otherwise = False

hasPrior :: Char -> Char -> Bool
hasPrior x y
    | y == ')' = True
    | y == '(' = False
    | x == '(' = True
    | x == y = True
    | y `elem` "+-" = True
    | x == '^' = True
    | otherwise = False

tokenList :: [String] -> [String] -> [String]
tokenList [] result = result
tokenList (x:xs) result
    | isStringOperator x = tokenList xs (result ++ [x])
    | otherwise = tokenList xs result

numberList :: [String] -> [String] -> [String]
numberList [] result = result
numberList (x:xs) result
    | isStringOperator x == False = numberList xs (result ++ [x])
    | otherwise = numberList xs result

getDoubleFromResult :: Result -> Double
getDoubleFromResult (Result _ nbr) = nbr

getStringFromResult :: Result -> [String]
getStringFromResult (Result x _) = x

parseNumber :: [String] -> Result
parseNumber [] = (Result [] 0)
parseNumber (x:[])
    | isStringDouble2 x = (Result ([]) (read x::Double))
    | otherwise = (Result ([]) 0)
parseNumber (x:x1:x2)
    | x == "(" = parseSum (parseFactors (parsePow (parseNumber (x1:x2))))
    | otherwise = (Result (x1:x2) (read x::Double))

parsePow :: Result -> Result
parsePow (Result [] nbr) = (Result [] nbr)
parsePow (Result (x:xs) nbr)
    | x /= "^" = (Result (x:xs) nbr)
    | x == "^" && length xs >= 2 && xs!!1 == "^" = parsePow (Result 
    (getStringFromResult(parsePow (parseNumber xs))) (eval (Calc Power 
    (Val nbr) (Val (getDoubleFromResult(parsePow
    (parseNumber xs)))))))
    | x == "^" = parsePow (Result (getStringFromResult (parseNumber xs))
    (eval (Calc Power (Val nbr) (Val (getDoubleFromResult 
    (parseNumber xs))))))
    | otherwise = parseSum (parseFactors (parsePow (parseNumber (x:xs))))

parseFactors :: Result -> Result
parseFactors (Result [] nbr) = (Result [] nbr)
parseFactors (Result (x:xs) nbr)
    | x /= "*" && x /= "/" = (Result (x:xs) nbr)
    | x == "*" = parseFactors (Result (getStringFromResult (parsePow
    (parseNumber xs))) (eval (Calc Times (Val nbr) (Val 
    (getDoubleFromResult(parsePow (parseNumber xs)))))))
    | otherwise = parseFactors (Result (getStringFromResult (parsePow
    (parseNumber xs))) (eval (Calc Div (Val nbr) (Val 
    (getDoubleFromResult(parsePow (parseNumber xs)))))))

parseSum :: Result -> Result
parseSum (Result [] nbr) = (Result [] nbr)
parseSum (Result (x:xs) nbr)
    | x == ")" = (Result (xs) nbr)
    | x /= "+" && x /= "-" = (Result (x:xs) nbr)
    | x == "+" = parseSum (Result (getStringFromResult (parseFactors (parsePow
     (parseNumber xs)))) (eval (Calc Plus (Val nbr) (Val (getDoubleFromResult
    (parseFactors (parsePow (parseNumber xs))))))))
    | otherwise = parseSum (Result (getStringFromResult (parseFactors (parsePow
    (parseNumber xs)))) (eval (Calc Minus (Val nbr) (Val (getDoubleFromResult
    (parseFactors (parsePow (parseNumber xs))))))))

doMath :: [String] -> IO()
doMath x
    | checkIfTokenizeIsOk x == False = badArgs
    | otherwise = printDouble (getDoubleFromResult (parseSum 
    (parseFactors (parsePow
    (parseNumber x)))))

printResult :: Result -> IO ()
printResult ret = print $ ret

printDouble :: Double -> IO ()
printDouble x
    | x == (1/0) = badArgs
    | x == (-1/0) = badArgs
    | otherwise = printf "%.2f\n" x

remakeTokenizeForOperator :: [String] -> [String] -> String -> [String]
remakeTokenizeForOperator [] str _ = str
remakeTokenizeForOperator (x:[]) str _ = str ++ [x]
remakeTokenizeForOperator (x:xs) str before
    | before == "(" && x == "-" && isStringDouble (xs!!0) = 
    remakeTokenizeForOperator (tail xs) (str ++ [x ++ (head xs)]) x
    | before == "" && x == "-" && (xs!!0 == "(" || isStringDouble2 (xs!!0)) = 
    remakeTokenizeForOperator xs (str ++ ["0"] ++ [x]) x
    | before /= ")" && isStringDouble2 before == False && x == "-" && 
    xs!!0 == "(" = remakeTokenizeForOperator xs (str ++ ["0"] ++ [x]) x
    | otherwise = remakeTokenizeForOperator xs (str ++ [x]) x

errorGestionBeginEnd :: [String] -> Int -> String -> Bool
errorGestionBeginEnd [] index _ = True
errorGestionBeginEnd (x:[]) index before
    | isStringOperator x && x /= ")" = False
    | isStringDouble2 x && before /= "" && isStringDouble2 before = False
    | isStringDouble2 x && before /= "" && (before == "(" || before == ")") = False
    | otherwise = errorGestionBeginEnd [] (index + 1) x
errorGestionBeginEnd (x:xs) index before
    | index == 0 && isStringOperator x && x /= "(" && x/= "-" = False
    | otherwise = errorGestionBeginEnd xs (index + 1) x

verifyAllOperator :: [String] -> Int -> String -> Bool
verifyAllOperator [] index _ = True
verifyAllOperator (x:xs) index before
    | x == "(" && before /= "" && isStringDouble2 before = False
    | isStringOperatorButParenthese x && before /= "" && 
    isStringOperatorButParenthese before = False
    | isStringDouble2 x && before /= "" && isStringDouble2 before = False
    | isStringDouble2 x && before /= "" && before == ")" = False
    | x == ")" && isStringOperatorButParenthese before = False
    | isStringOperator x && x /= "-" && x /= "(" && before == "(" = False
    | otherwise = verifyAllOperator xs (index + 1) x

nbrOpenParent :: [String] -> Int -> Int
nbrOpenParent [] nbr = nbr
nbrOpenParent (x:xs) nbr
    | x == "(" = nbrOpenParent xs (nbr + 1)
    | otherwise = nbrOpenParent xs nbr

nbrCloseParent :: [String] -> Int -> Int
nbrCloseParent [] nbr = nbr
nbrCloseParent (x:xs) nbr
    | x == ")" = nbrCloseParent xs (nbr + 1)
    | otherwise = nbrCloseParent xs nbr

verifyParenthese :: [String] -> Int -> String -> Bool
verifyParenthese [] index _ = True
verifyParenthese (x:xs) index before
    | before == ")" && x == "(" = False
    | before == "(" && isStringOperatorButParenthese x = False
    | otherwise = verifyParenthese xs (index + 1) x

checkIfTokenizeIsOk :: [String] -> Bool
checkIfTokenizeIsOk str
    | strictCheckDoubleString str == False = False
    | (nbrCloseParent str 0) /= (nbrOpenParent str 0) = False
    | errorGestionBeginEnd str 0 "" == False = False
    | verifyAllOperator str 0 "" == False = False
    | verifyParenthese str 0 "" == False = False
    | otherwise = True

rectifyDouble :: String -> String -> Int -> String
rectifyDouble [] corrected _ = corrected
rectifyDouble (x:[]) corrected index
    | x == '.' = rectifyDouble [] (corrected ++ [x] ++ ['0']) index
    | otherwise = rectifyDouble [] (corrected ++ [x]) index
rectifyDouble (x:xs) corrected index
    | index == 0 && x == '.' = rectifyDouble xs (corrected ++ ['0'] ++ [x]) (index + 1)
    | otherwise = rectifyDouble xs (corrected ++ [x]) (index + 1)

strictCheckDouble :: String -> Bool -> Bool
strictCheckDouble [] _ = True
strictCheckDouble (x:xs) bool
    | bool == True && x == '.' = False
    | x == '.' = strictCheckDouble xs True
    | otherwise = strictCheckDouble xs bool

strictCheckDoubleString :: [String] -> Bool
strictCheckDoubleString [] = True
strictCheckDoubleString (x:xs)
    | x == "." = False
    | strictCheckDouble x False == False = False
    | otherwise = strictCheckDoubleString xs

doubleTokenize :: [String] -> [String] -> [String]
doubleTokenize [] str = str
doubleTokenize (x:xs) str
    | x == "." = doubleTokenize xs (str ++ [".."])
    | isStringDouble2 x = doubleTokenize xs (str ++ [(rectifyDouble x "" 0)])
    | otherwise = doubleTokenize xs (str ++ [x])

wrongString :: String -> Bool
wrongString [] = True
wrongString (x:xs)
    | x `elem` ['0'..'9'] || x `elem` ".^+-/*()" = wrongString xs
    | otherwise = False

evalExpr :: String -> IO()
evalExpr [] = badArgs
evalExpr (x:[])
    | isCharDouble x = putStrLn ([x])
    | otherwise = badArgs
evalExpr (x:xs)
    | length (x:xs) == 0 = badArgs
    | x == '+' = evalExpr xs
    | wrongString (x:xs) == False = badArgs
    | otherwise = doMath (doubleTokenize (remakeTokenizeForOperator
    (tokenize (x:xs) []) [] "") [])

main :: IO ()
main = do
    args <- getArgs
    let len = length ( args )
    if len /= 0 && len > 1 then
        badArgsNumber
    else
        case args of
            [] -> noArgs
            otherwise -> evalExpr (stripSpace (args!!0) "")