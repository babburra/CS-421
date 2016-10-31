--- Given Code
--- ==========

module Main where

import Prelude hiding (lookup)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap, fromList, lookup, insert, union, empty)


--- Problems (Part 1)
--- =================

--- Datatypes
--- ---------

--- ### Environments

type Env = HashMap String Val

--- ### Expressions

data Exp = IntExp Integer
        | SymExp String
        | SExp [Exp]
         deriving (Show, Eq)

--- ### Values

data Val = IntVal Integer
         | SymVal String
         | ExnVal String
         | PrimVal ([Val] -> Val)
         | Closure [String] Exp Env
         | DefVal String Val
         | ConsVal Val Val
         | Macro [String] Exp Env

instance Show Val where
    show (IntVal i)         = show i
    show (SymVal s)         = s
    show (ExnVal s)         = "*** Scheme-Exception: " ++ s ++ " ***"
    show (PrimVal _)        = "*primitive*"
    show (Closure _ _ _)    = "*closure*"
    show (DefVal v _)       = v
    show l@(ConsVal _ _)    = "(" ++ showCons l ++ ")"
        where
            showCons (ConsVal car (SymVal "nil"))   = show car ++ " "
            showCons (ConsVal car l'@(ConsVal _ _)) = show car ++ " " ++ showCons l'
            showCons (ConsVal car cdr)              = show car ++ " . " ++ show cdr
    show (Macro _ _ _)      = "*macro*"

--- Parsing
--- -------

type Parser = ParsecT String () Identity

parseWith :: Parser a -> String -> Either ParseError a
parseWith parser input = parse parser "" input

--- ### Lexicals

adigit :: Parser Char
adigit = oneOf ['0'..'9']

digits :: Parser String
digits = many1 adigit

--- #### Whitespace parser

whitespace :: Parser String
whitespace = many $ oneOf " \t\n"

--- #### Identifier parser

identFirst :: Parser Char
identFirst = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "-*+/:'?><=!"

identRest :: Parser Char
identRest = identFirst <|> adigit

identifier :: Parser String
identifier = do f <- identFirst
                r <- many identRest
                return $ f:r

--- ### Grammaticals

anInt :: Parser Exp
anInt = do d <- digits
           return $ IntExp (read d)

--- #### Parsing symbols

aSym :: Parser Exp
aSym =   do symbol <- identifier
            return $ SymExp symbol

--- #### Parsing forms

aForm :: Parser Exp
aForm =  do oneOf "("
            whitespace
            exps <- many anExp
            whitespace
            oneOf ")"
            return $ SExp exps

--- #### Quotes, Quasi-Quotes, and UnQuotes

aQuote :: Parser Exp
aQuote = do oneOf "'"
            whitespace
            exp <- anExp
            return $ SExp [SymExp "quote", exp]

aQQuote :: Parser Exp
aQQuote = do oneOf "`"
             whitespace
             exp <- anExp
             return $ SExp [SymExp "quasiquote", exp]

anUnquote :: Parser Exp
anUnquote = do oneOf ","
               whitespace
               exp <- anExp
               return $ SExp [SymExp "unquote", exp]

--- optionally, the above can be defined in terms of `mkQuote`
mkQuote :: Char -> String -> Parser Exp
mkQuote = undefined

anyQuote :: Parser Exp
anyQuote = do prefix <- oneOf "'`,"
              whitespace
              exp <- anExp
              return $ case prefix of
                '`' -> SExp [SymExp "quasiquote", exp]
                ',' -> SExp [SymExp "unquote", exp]
                '\'' -> SExp [SymExp "quote", exp]

--- #### Expression Parser

anExp :: Parser Exp
anExp =  do whitespace
            exp <- aQuote <|> aQQuote <|> anUnquote <|> anInt <|> aSym <|> aForm
            whitespace
            return exp

--- Lifters/Lowerers
--- ----------------

liftbool :: Bool -> Val
liftbool False = SymVal "nil"
liftbool True  = SymVal "t"

lowerbool :: Val -> Bool
lowerbool (SymVal "nil") = False
lowerbool _              = True

liftint :: Integer -> Val
liftint = IntVal

lowerint :: Val -> Integer
lowerint (IntVal i) = i
lowerint _          = error "Cannot lower, not an IntVal!"

--- ### Boolean operations

liftBoolOp :: ([Bool] -> Bool) -> [Val] -> Val
liftBoolOp f = liftbool . f . map lowerbool

--- ### Integer operations

liftIntOp :: (Integer -> Integer -> Integer) -> Integer -> ([Val] -> Val)
liftIntOp _ z []    = liftint z
liftIntOp f z vs    = liftint . foldl1 f . map lowerint $ vs

--- ### Comparison operations

liftCompOp :: (Integer -> Integer -> Bool) -> ([Val] -> Val)
liftCompOp _ [] = liftbool True
liftCompOp _ [x] = liftbool True
liftCompOp f vs = liftbool . myfold f . map lowerint $ vs

myfold f [] = True
myfold f (x:y:xs) = case xs of
                      [] -> f x y
                      _ -> f x y && myfold f (y:xs)    

--- ### List operations

liftList :: [Val] -> Val
liftList = foldr ConsVal (liftbool False)

lowerList :: Val -> [Val]
lowerList (ConsVal car cdr) = car : lowerList cdr
lowerList (SymVal "nil")    = []


--- Problems (Part 2)
--- =================

withRuntime :: String -> String
withRuntime input
    = let (SExp (SymExp op : args)) = (\(Right r) -> r) . parseWith aForm $ input
      in  case lookup op runtime of
            Just (PrimVal f) -> show . f . map (flip eval runtime) $ args
            _                -> error $ "Failed lookup '" ++ show op ++ "' in 'runtime'."

--- Runtime
--- -------

runtime :: Env
runtime = foldl union empty [ runtimeArith
                            , runtimeComp
                            , runtimeBool
                            , runtimeUnary
                            , runtimeOther
                            ]

--- ### Arithmetic

runtimeArith :: Env
runtimeArith = fromList [ ("+", PrimVal $ liftIntOp (+) 0),
                          ("-", PrimVal $ liftIntOp (-) 0),
                          ("*", PrimVal $ liftIntOp (*) 1)
                        ]

--- ### Comparison

runtimeComp :: Env
runtimeComp = fromList [ (">", PrimVal $ liftCompOp (>)),
                         ("<", PrimVal $ liftCompOp (<)),
                         (">=", PrimVal $ liftCompOp (>=)),
                         ("<=", PrimVal $ liftCompOp (<=)),
                         ("=", PrimVal $ liftCompOp (==)),
                         ("!=", PrimVal $ liftCompOp (/=))
                       ]

--- ### Boolean Operators

runtimeBool :: Env
runtimeBool = fromList [ ("and", PrimVal $ liftBoolOp and),
                         ("or", PrimVal $ liftBoolOp or)
                       ]

--- ### Unary Operators

primNot :: Val -> Val
primNot x = liftbool . not . lowerbool $ x

primCar :: Val -> Val
primCar (ConsVal car cdr)   = car
primCar val                 = ExnVal $ "Not a cons cell: " ++ show val

primCdr :: Val -> Val
primCdr (ConsVal car cdr)   = cdr
primCdr val                 = ExnVal $ "Not a cons cell: " ++ show val

primUnary :: String -> (Val -> Val) -> [Val] -> Val
primUnary _ f [v]       = f v
primUnary opName _ _    = ExnVal $ "`" ++ opName ++ "` is a unary operator."

runtimeUnary :: Env
runtimeUnary = fromList [ ("not",   PrimVal $ primUnary "not" primNot),
                          ("car",   PrimVal $ primUnary "car" primCar),
                          ("cdr",   PrimVal $ primUnary "cdr" primCdr)
                        ]

--- ### Other operators

primEq :: [Val] -> Val
primEq []   = liftbool True
primEq vs   = liftbool . and $ zipWith eqVal vs (tail vs)
    where
        eqVal (IntVal i1)         (IntVal i2)           = i1 == i2
        eqVal (SymVal s1)         (SymVal s2)           = s1 == s2
        eqVal (ConsVal car1 cdr1) (ConsVal car2 cdr2)   = eqVal car1 car2 && eqVal cdr1 cdr2
        eqVal _                   _                     = False

primList :: [Val] -> Val
primList = liftList

runtimeOther :: Env
runtimeOther = fromList [ ("eq?",   PrimVal primEq),
                          ("list",  PrimVal primList)
                        ]

--- Evaluation
--- ----------

--- ### Check parameter names

paramStrs :: [Exp] -> Either String [String]
paramStrs = traverse paramStr
    where
        paramStr (SymExp p) = Right p
        paramStr _          = Left "Must use only `SymExp` for parameter names."

--- ### Quoting, Quasi-Quoting, and Unquoting

quote :: Exp -> Val
quote (IntExp i)    = IntVal i
quote (SymExp s)    = SymVal s
quote (SExp exps)   = liftList $ map quote exps

quasiquote :: Exp -> Env -> Integer -> Val
quasiquote (SExp [SymExp "unquote", exp]) env 1
    = eval exp env
quasiquote (SExp [SymExp "unquote", exp]) env d
    = liftList $ [SymVal "unquote", quasiquote exp env (d-1)]
quasiquote (SExp [SymExp "quasiquote", exp]) env d
    = liftList $ [SymVal "quasiquote", quasiquote exp env (d+1)]
quasiquote (SExp exps) env d
    = liftList $ map (\exp -> quasiquote exp env d) exps
quasiquote e env _
    = quote e

unquote :: Val -> Exp
unquote (IntVal i)      = IntExp i
unquote (SymVal s)      = SymExp s
unquote l@(ConsVal _ _) = SExp (map unquote $ lowerList l)

--- ### Evaluation - the function!

eval :: Exp -> Env -> Val

--- #### Integer, Symbol, and Empty Forms
eval (IntExp i) env = IntVal i
eval (SymExp s) env                                                    
    = case lookup s env of
        Just v  -> v
        Nothing -> ExnVal $ "Symbol " ++ s ++ " has no value."
eval (SExp []) env
    = SymVal "nil"

--- #### Variable Definition Forms
eval (SExp [SymExp "define", SymExp f, SExp params, exp]) env          
    = case paramStrs params of
        Right ps    -> let closure = Closure ps exp env'
                           env'    = insert f closure env
                       in  DefVal f closure
        Left err    -> ExnVal err

--- #### Function Definition and Lambda Function Forms
eval (SExp [SymExp "def", SymExp f, exp]) env                           
    = DefVal f $ eval exp env
eval (SExp [SymExp "lambda", SExp params, exp]) env                     
    = case paramStrs params of
        Right ps    -> Closure ps exp env
        Left err    -> ExnVal err

--- #### Quoting, Quasi-Quoting, and Unquoting Forms
eval (SExp [SymExp "quote", exp]) env                                   
    = quote exp
eval (SExp [SymExp "quasiquote", exp]) env                              
    = quasiquote exp env 1
eval (SExp [SymExp "unquote", exp]) env                                 
    = ExnVal "Cannot `unquote` more than `quasiquote`."
--- #### Conditional Form
eval (SExp [SymExp "cond", SExp body]) env                              
    = case body of
        []                  -> liftbool False
        [_]                 -> liftbool False
        (cond : th : rest)  -> if (lowerbool $ eval cond env)
                                then eval th env
                                else eval (SExp [SymExp "cond", SExp rest]) env
--- #### Let Form
eval (SExp [SymExp "let", SExp defs, body]) env                         
    = let defs' = map (\(SExp [SymExp var, val]) -> (var, eval val env)) defs
          env'  = union (fromList defs') env
      in  eval body env'
--- #### Cons Form
eval (SExp [SymExp "cons", car, cdr]) env                               
    = ConsVal (eval car env) (eval cdr env)
--- #### Eval Form
eval (SExp [SymExp "eval", exp]) env                                    
    = eval (unquote (eval exp env)) env
--- #### Macro Form
eval (SExp [SymExp "defmacro", SymExp f, SExp params, exp]) env         
    = case paramStrs params of
        Right ps    -> let macro = Macro ps exp env'
                           env'  = insert f macro env
                       in  DefVal f macro
        Left err    -> ExnVal err
--- #### Application Form
eval (SExp (f : as)) env                                                
    = let as' = map (\a -> eval a env) as
      in  case eval f env of
            PrimVal pf          -> pf as'                                   
            Closure ps exp cenv -> let env' = union (fromList $ zip ps as') cenv
                                   in  eval exp env'                        
            Macro ps exp menv   -> let env' = union (fromList $ zip ps (map quote as)) menv
                                   in  eval (unquote (eval exp env')) env   
            val                 -> val                                      
--- REPL
--- ----

--- ### Generating next environment

nextEnv :: Env -> Val -> Env
nextEnv env s 
   = case s of
    DefVal k v -> insert k v env
    _ -> env

--- ### Writing the REPL

prompt :: String -> IO String
prompt str = hPutStr stdout str >> hFlush stdout >> hGetLine stdin

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: Env -> IO ()
repl env =
    do  putStr "scheme> "
        l <- getLine                                                        
        case parse anExp "Expression" l of                                  
            Right exp -> case eval exp env of                               
                            (DefVal k v)    -> do   putStrLn k              
                                                    repl $ insert k v env 
                            val             -> putStrLn $ show val          
            Left pe   -> putStrLn (show pe)                                 
        repl env                                                            


--- ### Main function

main :: IO ()
main = do printLn "Welcome to your Scheme interpreter!"
          repl runtime
          printLn "Goodbye!"
