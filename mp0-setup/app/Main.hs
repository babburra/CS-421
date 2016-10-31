module Main where

import System.IO (hFlush, stdout)

import qualified Data.HashMap.Strict as H (HashMap, lookup, insert, fromList)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec ((<|>), many, many1, oneOf, string)
import Text.Parsec.Prim (parse, ParsecT)

main :: IO ()
main = do putStrLn "Welcome!"
          repl runtime


-- Data Structures/Types
-- =====================

-- Nice interface to parsing
type Parser = ParsecT String () Identity

-- Expressions in our language
data Exp  = IExp Integer
          | VExp String
          | SExp String

instance Show Exp where
    show (IExp i) = show i
    show (VExp s) = s
    show (SExp s) = show s

-- Statements in our language
data Stmt = Assign String Exp
          | Print Exp
          | Quit


-- Parsing
-- =======

-- clear whitespace
whitespace :: Parser String
whitespace = many $ oneOf " \n\t"

-- Expressions
-- -----------

-- naturals
aNum :: Parser Exp
aNum = do n <- many1 $ oneOf ['0'..'9']
          whitespace
          return $ IExp (read n)

-- variable names
aVar :: Parser Exp
aVar = do var <- many . oneOf $ ['a'..'z'] ++ ['A'..'Z']
          whitespace
          return $ VExp var

-- quoted strings
aStr :: Parser Exp
aStr = do string "\""
          str <- many . oneOf $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ " \n\t"
          string "\""
          whitespace
          return $ SExp str

-- Any expression
anExp :: Parser Exp
anExp = aNum <|> aStr <|> aVar

-- Statements
-- ----------

-- print statement
aPrint :: Parser Stmt
aPrint = do string "print"
            whitespace
            var <- anExp
            whitespace
            return $ Print var

-- assignment statement
anAssign :: Parser Stmt
anAssign = do VExp var <- aVar
              whitespace
              string ":="
              whitespace
              exp <- anExp
              whitespace
              return $ Assign var exp

-- quit statement
aQuit :: Parser Stmt
aQuit = do string "quit"
           return Quit

-- any statement
aStmt :: Parser Stmt
aStmt = aQuit <|> aPrint <|> anAssign


-- Environment
-- ===========

type Env = H.HashMap String Exp
runtime :: Env
runtime = H.fromList []


-- Evaluation
-- ==========

eval :: Env -> Exp -> String
eval env (VExp s) = case H.lookup s env of
                        Just res -> eval env res
                        Nothing  -> "ERROR (environment): No definition for " ++ s
eval env exp      = show exp


-- REPL
-- ====

repl :: Env -> IO ()
repl env
    = do putStr "> "
         hFlush stdout
         input <- getLine
         case parse aStmt "" input of
            Left msg    -> do putStrLn $ "ERROR (parsing): " ++ show msg
                              repl env
            Right stmt  -> case stmt of
                            Assign s e  -> repl (H.insert s e env)
                            Print exp   -> do putStrLn $ eval env exp
                                              repl env
                            Quit        -> putStrLn "Goodbye!"
