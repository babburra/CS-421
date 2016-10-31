--- Given Code
--- ==========

module Main where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         | IntExp Integer
         | VarExp String
         | LamExp String Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (AppExp f e)     = show f ++ " " ++ show e
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3 ++ ")"
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"

--- The Parser
--- ----------

-- Pretty parser type
type Parser = ParsecT String () Identity

--- ### Lexicals

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

int :: Parser Integer
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Integer)

var :: Parser String
var = let keywords = ["if", "then", "else"]
      in  try $ do v1 <- letter                  <?> "an identifier"
                   vs <- many (letter <|> digit) <?> "an identifier"
                   spaces
                   let v = v1:vs
                   if (any (== v) keywords)
                    then fail "keyword"
                    else return v

oper :: Parser String
oper = do op <- many1 (oneOf "+-*/<>=") <?> "an operator"
          spaces
          return op

parens :: Parser a -> Parser a
parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

--- ### Expressions

intExp :: Parser Exp
intExp = do i <- int
            return $ IntExp i

varExp :: Parser Exp
varExp = do v <- var
            return $ VarExp v

opExp :: String -> Parser (Exp -> Exp -> Exp)
opExp str = do symbol str
               return (OpExp str)

mulOp :: Parser (Exp -> Exp -> Exp)
mulOp = opExp "*" <|> opExp "/"

addOp :: Parser (Exp -> Exp -> Exp)
addOp = opExp "+" <|> opExp "-"

compOp :: Parser (Exp -> Exp -> Exp)
compOp =     opExp "<"  <|> opExp ">"
         <|> opExp "<=" <|> opExp ">="
         <|> opExp "/=" <|> opExp "=="

ifExp :: Parser Exp
ifExp = do try $ symbol "if"
           e1 <- expr
           symbol "then"
           e2 <- expr
           symbol "else"
           e3 <- expr
           return $ IfExp e1 e2 e3

lamExp :: Parser Exp
lamExp = do try $ symbol "\\"
            param <- var
            symbol "->"
            body <- expr
            return $ LamExp param body

appExp :: Parser Exp
appExp = do e1 <- expr
            e2 <- expr
            return $ AppExp e1 e2

atom :: Parser Exp
atom =     intExp
       <|> ifExp
       <|> lamExp
       <|> varExp
       <|> parens expr

expr :: Parser Exp
expr = let arith  = term `chainl1` addOp
           term   = factor `chainl1` mulOp
           factor = app
           app    = do f <- many1 atom
                       return $ foldl1 AppExp f
       in  arith `chainl1` compOp

parseExp :: String -> Either ParseError Exp
parseExp str = parse expr "stdin" str

--- ### Declarations

decl :: Parser Stmt
decl = do f <- var
          params <- many1 var
          symbol "="
          body <- expr
          return $ Decl f params body

parseDecl :: String -> Either ParseError Stmt
parseDecl str = parse decl "stdin" str

--- The REPL
--- --------

prompt :: String -> IO ()
prompt str = hPutStr stdout str >> hFlush stdout

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: IO ()
repl = do input <- prompt "> " >> getLine
          case input of
            "quit" -> return ()
            _      -> do case parseDecl input of
                            Left err    -> do printLn "Parse error!"
                                              printLn $ show err
                            Right decl  -> printLn . show $ cpsDecl decl
                         repl


main :: IO ()
main = do putStrLn "Welcome to the CPS Transformer!"
          repl
          putStrLn "GoodBye!"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n-1) (\v -> k $ v * n)

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] ke ko | even x    = ke x
                  | otherwise = ko x
evenoddk (x:xs) ke ko | even x    = evenoddk xs (\v -> ke $ v + x) ko
                     | otherwise = evenoddk xs ke (\v -> ko $ v + x)

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### `isSimple :: Exp -> Bool`

isSimple :: Exp -> Bool
isSimple (VarExp _) = True
isSimple (IntExp _) = True
isSimple (AppExp _ _) = False
isSimple (IfExp e1 e2 e3) = all isSimple [e1,e2,e3]
isSimple (OpExp op e1 e2) = isSimple e1 && isSimple e2
isSimple (LamExp _ _) = True

--- ### `cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)`

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp e@(IntExp _) k syms = (AppExp k e, syms)
cpsExp e@(VarExp _) k syms = (AppExp k e, syms)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f arg) k syms | isSimple arg = (AppExp (AppExp f arg) k, syms)
                             | otherwise = let (name,s2) = gensym syms
                                            in cpsExp arg (LamExp name (AppExp f (AppExp (VarExp name) k))) s2

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp c t e) k syms | isSimple c =
         let (t1,_) = cpsExp t k syms
             (e1,_) = cpsExp e k syms
          in (IfExp c t1 e1, syms)
        | otherwise = let (v, s) = gensym syms
                          (t2, _) = cpsExp t k s
                          (e2, _) = cpsExp e k s
                      in (cpsExp c (LamExp v (IfExp (VarExp v) t2 e2)) s)

--- #### Define `cpsExp` for Operator Expressions
cpsExp e@(OpExp op e1 e2) k syms =
  case (isSimple e1, isSimple e2) of
     (True,True) -> (AppExp k e,syms)
     (True,False) -> cpsExp e2 (LamExp name (AppExp k (OpExp op e1 (VarExp name)))) s2
           where (name,s2) = gensym syms
     (False,True) -> cpsExp e1 (LamExp name (AppExp k (OpExp op (VarExp name) e2))) s1
           where (name,s1) = gensym syms
     (False,False) ->
           let (v1,s1) = gensym syms
               (v2,s2) = gensym s2
               base = LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
               (ce2,s3) = cpsExp e2 base s2
            in cpsExp e1 (LamExp v1 ce2) s3

--- ### `cpsDecl :: Stmt -> Stmt`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f params body) = Decl (f ++ "k")
                                    (params ++ ["k"])
                                    (fst (cpsExp body (VarExp "k") 1))