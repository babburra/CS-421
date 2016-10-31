{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec
import Control.Monad
import System.IO

-- Our datatypes
-- -------------


type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

-- Primitives
-- ----------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

-- Parser, given to you this time.
-- -------------------------------

-- Lexicals

run p s =
   case parse p "<stdin>" s of
      Right x -> x
      Left x -> error $ show x

symbol s = do string s
              spaces
              return s

int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

var = do v <- many1 letter <?> "an identifier"
         spaces
         return v

parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

-- Expressions

intExp = do i <- int
            return $ IntExp i

boolExp = do { symbol "true" ; return $ BoolExp True }
      <|> do { symbol "false"; return $ BoolExp False}

varExp = do v <- var
            return $ VarExp v

mulOp =    do { symbol "*" ; return $ IntOpExp "*" }
       <|> do { symbol "/" ; return $ IntOpExp "/" }

addOp =    do { symbol "+" ; return $ IntOpExp "+" }
       <|> do { symbol "-" ; return $ IntOpExp "-" }

andOp = do try $ symbol "and"
           return $ BoolOpExp "and"

orOp = do try $ symbol "or"
          return $ BoolOpExp "or"

compOp =   do try $ do { symbol "<=" ; return $ CompOpExp "<=" }
       <|> do try $ do { symbol ">=" ; return $ CompOpExp ">=" }
       <|> do try $ do { symbol "/=" ; return $ CompOpExp "/=" }
       <|> do try $ do { symbol "==" ; return $ CompOpExp "==" }
       <|> do try $ do { symbol "<" ; return $ CompOpExp "<" }
       <|> do try $ do { symbol ">" ; return $ CompOpExp ">" }

ifExp = do try $ symbol "if"
           e1 <- expr
           symbol "then"
           e2 <- expr
           symbol "else"
           e3 <- expr
           symbol "fi"
           return $ IfExp e1 e2 e3

funExp = do try $ symbol "fn"
            symbol "["
            params <- var `sepBy` (symbol ",")
            symbol "]"
            body <- expr
            symbol "end"
            return $ FunExp params body

letExp = do try $ symbol "let"
            symbol "["
            params <- (do v <- var
                          symbol ":="
                          e <- expr
                          return (v,e)
                      )
                      `sepBy` (symbol ";")
            symbol "]"
            body <- expr
            symbol "end"
            return $ LetExp params body

appExp = do try $ symbol "apply"
            efn <- expr
            symbol "("
            exps <- expr `sepBy` (symbol ",")
            symbol ")"
            return $ AppExp efn exps

expr = disj `chainl1` orOp
disj = conj `chainl1` andOp
conj = arith `chainl1` compOp
arith = term `chainl1` addOp
term = factor `chainl1` mulOp
factor = atom

atom = intExp
   <|> funExp
   <|> ifExp
   <|> letExp
   <|> try boolExp
   <|> appExp
   <|> varExp
   <|> parens expr

-- Statements

quitStmt = do try $ symbol "quit"
              symbol ";"
              return QuitStmt

printStmt = do try $ symbol "print"
               e <- expr
               symbol ";"
               return $ PrintStmt e

setStmt = do v <- var
             symbol ":="
             e <- expr
             symbol ";"
             return $ SetStmt v e

ifStmt = do try $ symbol "if"
            e1 <- expr
            symbol "then"
            s2 <- stmt
            symbol "else"
            s3 <- stmt
            symbol "fi"
            return $ IfStmt e1 s2 s3

procStmt = do try $ symbol "procedure"
              name <- var
              symbol "("
              params <- var `sepBy` (symbol ",")
              symbol ")"
              body <- stmt
              symbol "endproc"
              return $ ProcedureStmt name params body

callStmt = do try $ symbol "call"
              name <- var
              symbol "("
              args <- expr `sepBy` (symbol ",")
              symbol ")"
              symbol ";"
              return $ CallStmt name args

seqStmt = do try $ symbol "do"
             stmts <- many1 stmt
             symbol "od"
             symbol ";"
             return $ SeqStmt stmts

stmt = quitStmt
   <|> printStmt
   <|> ifStmt
   <|> procStmt
   <|> callStmt
   <|> seqStmt
   <|> try setStmt

-- repl
-- ----

repl :: PEnv -> Env -> [String] -> String -> IO Result
repl penv env [] _ =
  do putStr "> "
     hFlush stdout
     input <- getLine
     case parse stmt "stdin" input of
        Right QuitStmt -> do putStrLn "Bye!"
                             return ("",penv,env)
        Right x -> let (nuresult,nupenv,nuenv) = exec x penv env
                   in do {
                     putStrLn nuresult;
                     repl nupenv nuenv [] "stdin"
                   }
        Left x -> do putStrLn $ show x
                     repl penv env [] "stdin"

main = do
  putStrLn "Welcome to your interpreter!"
  repl H.empty H.empty [] "stdin"

-- lifting functions
-- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp div _ (IntVal 0) = ExnVal "Division by 0"
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y)= BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

-- Expression evaluation
-- ---------------------

eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i
eval (BoolExp i) env = BoolVal i

eval (IntOpExp op e1 e2) env = 
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = H.lookup op intOps
  in liftIntOp f v1 v2

eval (BoolOpExp op e1 e2) env = 
  let b1 = eval e1 env
      b2 = eval e2 env
      Just f = H.lookup op boolOps
  in liftBoolOp f b1 b2

eval (CompOpExp op e1 e2) env = 
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = H.lookup op compOps
  in liftCompOp f v1 v2

eval (IfExp c t e) env = 
  case (eval c env) of 
    BoolVal True -> eval t env
    BoolVal False -> eval e env
    _             -> ExnVal "Condition is not a Bool"

eval (VarExp s) env =
  case H.lookup s env of
    Just v -> v
    Nothing -> ExnVal "No match in env"

eval (FunExp v e1) env = 
  CloVal v e1 env

eval (AppExp e1 args) env = 
    let v1 = eval e1 env
        v2 = Prelude.map (\x -> eval x env) args
        in case v1 of
          CloVal nup body nuenv -> eval body (Prelude.foldr (\x map -> H.insert (fst x) (snd x) map) nuenv (zipWith (\x y -> (x,y)) nup v2))
          otherwise -> ExnVal "Apply to non-closure"

eval (LetExp xx ef) env =
  let locals = Prelude.map (\(v,e) -> (v, eval e env)) xx
      lh = H.fromList locals
  in eval ef (H.union lh env) 
-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (x:xs)) penv env = (p1 ++ p2, penv2, env2)
    where (p1, penv1, env1) = exec x penv env
          (p2, penv2, env2) = exec (SeqStmt xs) penv1 env1

--exec (IfStmt b e1 e2) penv env = ExnVal ("Condition is not a Bool", penv, env)
exec (IfStmt e1 s1 s2) penv env = 
  let b1 = eval e1 env
    in case (eval e1 env) of
      BoolVal True -> (exec s1 penv env)
      BoolVal False -> (exec s2 penv env)
      _             -> (val, penv, env)
        where val = show $ ExnVal "Condition is not a Bool"

exec (SetStmt var e) penv env = ("", penv, (H.insert var val env))
   where val = eval e env

exec p@(ProcedureStmt name args body) penv env = ("", H.insert name p penv, env)

exec (CallStmt name args) penv env = do
  case H.lookup name penv of
    Nothing -> ("Procedure " ++ name ++ " undefined", penv,env)
    Just (ProcedureStmt _ params body) ->
          exec body
               penv 
              (Prelude.foldr (\ (k,v) e -> H.insert k (eval v env) e) env 
                  $ zip params args)

exec _ _ _ = undefined