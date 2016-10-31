--- Getting Started
--- ===============

--- Testing Your Code
--- -----------------

module Tests where

import Main hiding (main)
import Data.HashMap.Strict (empty, fromList)
import Data.List (isInfixOf)

allTests :: [([Bool], String)]
allTests = let prefix str (ts, desc) = (ts, str ++ desc)
           in  map (prefix "Parser: ") parserTests ++
               map (prefix "Lifters: ") tests_lifting ++
               map (prefix "Runtime: ") tests_runtime ++
               map (prefix "Evaluator: ") evaluatorTests ++
               [ (tests_paramStrs, "paramStrs")
               , (tests_nextEnv, "NextEnv")
               ]

testBank :: [(String, [([String], [Exp], String)])]
testBank = [ basicIntsSyms
           , primitiveInts
           , primitiveBools
           , primitiveMisc
           , consListCarCdr
           , defDefineLambdaApply
           , allowsDefOverPrimForm
           , condForm
           , letForm
           , everythingElse
           ]

--- ([Input], [ParsedAs], Output)
type TestCase  = ([String], [Exp], String)
type TestSuite = (String, [TestCase])

boolTests :: (TestCase -> Bool) -> TestSuite -> ([Bool], String)
boolTests run (desc, tcs) = (map run tcs, desc)


--- Problems (Part 1)
--- =================

--- Datatypes
--- ---------

--- ### Values

tests_valShow :: [Bool]
tests_valShow = [ show (IntVal 3) == "3"
                , show (SymVal "noetuh") == "noetuh"
                , show (ExnVal "`not` is unary!") == "*** Scheme-Exception: `not` is unary! ***"
                , show (PrimVal (const (IntVal 3))) == "*primitive*"
                , show (Closure [] (IntExp 3) empty) == "*closure*"
                , show (DefVal "varName" (IntVal 5)) == "varName"
                , show (ConsVal (IntVal 3) (IntVal 5)) == "(3 . 5)"
                , show (ConsVal (IntVal 3) (SymVal "nil")) == "(3 )"
                , show (ConsVal (ConsVal (IntVal 3) (SymVal "hoenu")) (ConsVal (IntVal 3) (SymVal "nil"))) == "((3 . hoenu) 3 )"
                , show (Macro [] (IntExp 3) empty) == "*macro*"
                ]

--- Parsing
--- -------

parserTests :: [([Bool], String)]
parserTests = tests_parsing

testParse :: TestCase -> Bool
testParse (inputs, parsd, _)
    = let parseExp          = (\(Right e) -> e) . parseWith anExp
          parseSame inp out = parseExp inp == out
      in  and $ zipWith parseSame inputs parsd

parsesAs :: Eq a => Bool -> Parser a -> (String, a) -> Bool
parsesAs shouldWork p (input, output)
    = let onFail _   = not shouldWork
          onSucc out = not shouldWork || (out == output)
      in  either onFail onSucc $ parseWith p input

tests_parsing :: [([Bool], String)]
tests_parsing = [ (tests_lexicals, "Lexical Parsers")
                , (tests_symbols,  "Symbol Parser")
                , (tests_forms,    "Form Parser")
                , (tests_quotes,   "Quotes Parsers")
                , (tests_exps,     "Expression Parser")
                ]

--- ### Lexicals

parseLex :: Bool -> Parser String -> String -> Bool
parseLex sW p input = parsesAs sW p (input, input)

tests_lexicals :: [Bool]
tests_lexicals
    = concat [ map (parseLex True whitespace)  [ "\n\t", "", "  \n\n\t"
                                               , "\t\n", "\n   \t\n  \t"
                                               ]
             , map (parseLex False whitespace) [ "o\n\t", "eou", " \nteounn\n\t"
                                               , "\tnntt\n", "\n .Ceo8  \t\n  \t"
                                               ]
             , map (parseLex True identifier)  [ "oentauh", "oeu3209uh", "AAuenoth-*+"
                                               , "Zoenz??<o20enth!=/:otah'-*+oa2032"
                                               , "a", "Z", "?"
                                               ]
             , map (parseLex False identifier) [ ".3eooentauh", "3oeu3209uh", "AA|uenoth-*+"
                                               , "Zo.|e33enz??<o20enth!=/:otah'-*+oa2032"
                                               ]
             ]

--- #### Whitespace parser

--- #### Identifier parser

--- ### Grammaticals

--- #### Parsing symbols

symExpStr :: String -> (String, Exp)
symExpStr s = (s, SymExp s)

tests_symbols :: [Bool]
tests_symbols
    = concat [ map (parsesAs True aSym . symExpStr)  [ "aoenuth"
                                                     , "Aoe=<u3oe"
                                                     , "oeun*-'?=<33Toehlhh"
                                                     , "EU??=''!+:aoen32"
                                                     ]
             , map (parsesAs False aSym . symExpStr) [ ".oaentu"
                                                     , "??=oeunth"
                                                     , "832oenc.hu"
                                                     , "AAzzehu##"
                                                     ]
             ]

--- #### Parsing forms

tests_forms :: [Bool]
tests_forms
    = concat [ map (parsesAs True aForm)  [ ( "(f 10 30 x)"
                                            , SExp [SymExp "f", IntExp 10, IntExp 30, SymExp "x"]
                                            )
                                          , ( "(+ 15 10 (oent Aeonu??-* uh))"
                                            , SExp [ SymExp "+", IntExp 15, IntExp 10
                                                   , SExp [ SymExp "oent", SymExp "Aeonu??-*"
                                                          , SymExp "uh"
                                                          ]
                                                   ]
                                            )
                                          , ( "()"
                                            , SExp []
                                            )
                                          , ( "(    \n\t\n +   \n\t 10 aoentuh \n)"
                                            , SExp [SymExp "+", IntExp 10, SymExp "aoentuh"]
                                            )
                                          , ( "(  '??? '==' \n\t\t A200aeuheh =<=<  )"
                                            , SExp [ SExp [SymExp "quote", SymExp "???"]
                                                   , SExp [SymExp "quote", SymExp "=='"]
                                                   , SymExp "A200aeuheh", SymExp "=<=<"
                                                   ]
                                            )
                                          , ( "(  '?onetuh 5 a 2neotuah ++ taunh   )"
                                            , SExp [ SExp [SymExp "quote",SymExp "?onetuh"]
                                                   , IntExp 5, SymExp "a", IntExp 2
                                                   , SymExp "neotuah", SymExp "++", SymExp "taunh"
                                                   ]
                                            )
                                          ]
             , map (parsesAs False aForm) [ ( "(f 10 20"
                                            , SExp [SymExp "f", IntExp 10, IntExp 20]
                                            )
                                          , ( "10 20 340 . oeau"
                                            , SExp [IntExp 10, IntExp 20, IntExp 340, SymExp ".", SymExp "oeau"]
                                            )
                                          ]
             ]

--- #### Quotes, Quasi-Quotes, and UnQuotes

tests_quotes :: [Bool]
tests_quotes
    = map (parsesAs True anyQuote)  [ ( "'10"
                                      , SExp [SymExp "quote", IntExp 10]
                                      )
                                    , ( "'quote"
                                      , SExp [SymExp "quote", SymExp "quote"]
                                      )
                                    , ( "'(f 10)"
                                      , SExp [SymExp "quote", SExp [SymExp "f", IntExp 10]]
                                      )
                                    , ( "' '10"
                                      , SExp [SymExp "quote", SExp [SymExp "quote", IntExp 10]]
                                      )
                                    , ( "'('f 10)"
                                      , SExp [SymExp "quote", SExp [SExp [SymExp "quote", SymExp "f"], IntExp 10]]
                                      )
                                    , ( "` 10"
                                      , SExp [SymExp "quasiquote", IntExp 10]
                                      )
                                    , ( "`quote"
                                      , SExp [SymExp "quasiquote", SymExp "quote"]
                                      )
                                    , ( "`(f 10)"
                                      , SExp [SymExp "quasiquote", SExp [SymExp "f", IntExp 10]]
                                      )
                                    , ( "``10"
                                      , SExp [SymExp "quasiquote", SExp [SymExp "quasiquote", IntExp 10]]
                                      )
                                    , ( "`(` f 10)"
                                      , SExp [SymExp "quasiquote", SExp [SExp [SymExp "quasiquote", SymExp "f"], IntExp 10]]
                                      )
                                    , ( ",10"
                                      , SExp [SymExp "unquote", IntExp 10]
                                      )
                                    , ( ",quote"
                                      , SExp [SymExp "unquote", SymExp "quote"]
                                      )
                                    , ( ",( f 10)"
                                      , SExp [SymExp "unquote", SExp [SymExp "f", IntExp 10]]
                                      )
                                    , ( ",,10"
                                      , SExp [SymExp "unquote", SExp [SymExp "unquote", IntExp 10]]
                                      )
                                    , ( ",(,f 10)"
                                      , SExp [SymExp "unquote", SExp [SExp [SymExp "unquote", SymExp "f"], IntExp 10]]
                                      )
                                    , ( "' `,10"
                                      , SExp [SymExp "quote", SExp [SymExp "quasiquote", SExp [SymExp "unquote", IntExp 10]]]
                                      )
                                    ]

--- #### Expression Parser

tests_exps :: [Bool]
tests_exps
    = concat [ map (parsesAs True anExp)  [ ( "15"
                                            , IntExp 15 )
                                          , ( " \n\t15 \n\t"
                                            , IntExp 15 )
                                          , ( "aoenuth"
                                            , SymExp "aoenuth" )
                                          , ( " \n\taoenuth \n\t"
                                            , SymExp "aoenuth" )
                                          , ( "(f 10 30 x)"
                                            , SExp [SymExp "f", IntExp 10, IntExp 30, SymExp "x"]
                                            )
                                          , ( " \n\t(f 10 30 x) \n\t"
                                            , SExp [SymExp "f", IntExp 10, IntExp 30, SymExp "x"]
                                            )
                                          , ( "'quote"
                                            , SExp [SymExp "quote", SymExp "quote"]
                                            )
                                          , ( " \n\t'quote \n\t"
                                            , SExp [SymExp "quote", SymExp "quote"]
                                            )
                                          , ( "`quote"
                                            , SExp [SymExp "quasiquote", SymExp "quote"]
                                            )
                                          , ( " \n\t`quote \n\t"
                                            , SExp [SymExp "quasiquote", SymExp "quote"]
                                            )
                                          , ( ",quote"
                                            , SExp [SymExp "unquote", SymExp "quote"]
                                            )
                                          , ( " \n\t,quote \n\t"
                                            , SExp [SymExp "unquote", SymExp "quote"]
                                            )
                                          ]
             , map (parsesAs False anExp) [ ( "'a"
                                            , SymExp "'a"
                                            )
                                          ]
             ]


--- Problems (Part 2)
--- =================

--- Environment
--- -----------

tests_lifting :: [([Bool], String)]
tests_lifting = [ (tests_liftIntOp,     "liftIntOp")
                , (tests_liftBoolOp,    "liftBoolOp")
                , (tests_liftCompOp,    "liftCompOp")
                , (tests_liftLowerList, "Lift and Lower List")
                ]

tests_runtime :: [([Bool], String)]
tests_runtime = [ (tests_arithRuntime,  "Arithmetic Operators")
                , (tests_boolRuntime,   "Boolean Operators")
                , (tests_compRuntime,   "Comparison Operators")
                , (tests_unaryRuntime,  "Unary Operators")
                , (tests_otherRuntime,  "Other Operators")
                ]

--- ### Arithmetic Operators

tests_liftIntOp :: [Bool]
tests_liftIntOp = [ (show $ liftIntOp (+) 0 []) == "0"
                  , (show $ liftIntOp (+) 0 [IntVal 5, IntVal 7, IntVal 9]) == "21"
                  , (show $ liftIntOp (-) 0 []) == "0"
                  , (show $ liftIntOp (-) 0 [IntVal 10, IntVal 5, IntVal 3, IntVal 3]) == "-1"
                  , (show $ liftIntOp mod 10 []) == "10"
                  , (show $ liftIntOp mod 10 [IntVal 100, IntVal 77, IntVal 5]) == "3"
                  ]

tests_arithRuntime :: [Bool]
tests_arithRuntime = [ withRuntime "(+)" == "0"
                     , withRuntime "(-)" == "0"
                     , withRuntime "(*)" == "1"
                     , withRuntime "(+ 3 4 5)" == "12"
                     , withRuntime "(- 3 4 5)" == "-6"
                     , withRuntime "(* 7 8 9)" == "504"
                     ]

--- ### Boolean Operators

tests_liftBoolOp :: [Bool]
tests_liftBoolOp = [ (show $ liftBoolOp and []) == "t"
                   , (show $ liftBoolOp and [SymVal "t"]) == "t"
                   , (show $ liftBoolOp and [SymVal "t", IntVal 10]) == "t"
                   , (show $ liftBoolOp and [SymVal "t", IntVal 10, SymVal "nil"]) == "nil"
                   , (show $ liftBoolOp and [SymVal "t", IntVal 10, SymVal "nil", SymVal "t"]) == "nil"
                   ]

tests_boolRuntime :: [Bool]
tests_boolRuntime = [ withRuntime "(and 't 't 't 't)" == "t"
                    , withRuntime "(and t t t t)" == "t"
                    , withRuntime "(and t t nil)" == "t"
                    , withRuntime "(and t t 'nil)" == "nil"
                    , withRuntime "(and 3 5 2 t)" == "t"
                    , withRuntime "(or 't 't 't 't)" == "t"
                    , withRuntime "(or 't 't 't 'nil)" == "t"
                    , withRuntime "(and)" == "t"
                    , withRuntime "(or)" == "nil"
                    , withRuntime "(or 'nil nil 3 5 't)" == "t"
                    ]

--- ### Comparison Operators

tests_liftCompOp :: [Bool]
tests_liftCompOp = [ (show $ liftCompOp (<) []) == "t"
                   , (show $ liftCompOp (<) [IntVal 1]) == "t"
                   , (show $ liftCompOp (<) [IntVal 1, IntVal 2]) == "t"
                   , (show $ liftCompOp (<) [IntVal 1, IntVal 2, IntVal 3]) == "t"
                   , (show $ liftCompOp (<) [IntVal 1, IntVal 2, IntVal 3, IntVal 0]) == "nil"
                   , (show $ liftCompOp (<) [IntVal 1, IntVal 2, IntVal 0, IntVal 3]) == "nil"
                   , (show $ liftCompOp (\_ _ -> False) []) == "t"
                   , (show $ liftCompOp (\_ _ -> False) [IntVal 1]) == "t"
                   , (show $ liftCompOp (\_ _ -> False) [IntVal 1, IntVal 2]) == "nil"
                   ]

tests_compRuntime :: [Bool]
tests_compRuntime = [ withRuntime "(<)" == "t"
                    , withRuntime "(>)" == "t"
                    , withRuntime "(<=)" == "t"
                    , withRuntime "(>=)" == "t"
                    , withRuntime "(=)" == "t"
                    , withRuntime "(!=)" == "t"
                    , withRuntime "(< 3 4 5)" == "t"
                    , withRuntime "(< 3 3 5)" == "nil"
                    , withRuntime "(> 3 4 5)" == "nil"
                    , withRuntime "(> 3 2 1)" == "t"
                    , withRuntime "(= 3 3 3)" == "t"
                    , withRuntime "(= 3 3 1)" == "nil"
                    , withRuntime "(!= 3 3 3)" == "nil"
                    , withRuntime "(!= 3 3 1)" == "nil"
                    , withRuntime "(!= 3 2 1)" == "t"
                    ]

--- ### Unary Operators

tests_unaryRuntime :: [Bool]
tests_unaryRuntime = [ withRuntime "(not)" == "*** Scheme-Exception: `not` is a unary operator. ***"
                     , withRuntime "(car)" == "*** Scheme-Exception: `car` is a unary operator. ***"
                     , withRuntime "(cdr)" == "*** Scheme-Exception: `cdr` is a unary operator. ***"
                     , withRuntime "(cdr 7 2)" == "*** Scheme-Exception: `cdr` is a unary operator. ***"
                     , withRuntime "(car 7 2)" == "*** Scheme-Exception: `car` is a unary operator. ***"
                     , withRuntime "(not 7 2)" == "*** Scheme-Exception: `not` is a unary operator. ***"
                     , withRuntime "(not 't)" == "nil"
                     , withRuntime "(not t)" == "nil"
                     , withRuntime "(not 'nil)" == "t"
                     , withRuntime "(not nil)" == "nil"
                     , withRuntime "(not nil 't)" == "*** Scheme-Exception: `not` is a unary operator. ***"
                     , withRuntime "(not nil 't 3)" == "*** Scheme-Exception: `not` is a unary operator. ***"
                     , withRuntime "(not nil 3)" == "*** Scheme-Exception: `not` is a unary operator. ***"
                     , withRuntime "(not 3)" == "nil"
                     , withRuntime "(car '(3 5) )" == "3"
                     , withRuntime "(car 3 )" == "*** Scheme-Exception: Not a cons cell: 3 ***"
                     , withRuntime "(cdr '(3 7) )" == "(7 )"
                     ]

--- ### Other Operators

tests_liftLowerList :: [Bool]
tests_liftLowerList = [ (show $ liftList []) == "nil"
                      , (show $ liftList [IntVal 1]) == "(1 )"
                      , (show $ liftList [IntVal 1, SymVal "nil"]) == "(1 nil )"
                      , (show $ liftList [IntVal 1, SymVal "nil", ConsVal (IntVal 3) (SymVal "nil")]) == "(1 nil (3 ) )"
                      , (show $ lowerList (SymVal "nil")) == "[]"
                      , (show $ lowerList (ConsVal (IntVal 1) (SymVal "nil"))) == "[1]"
                      , (show $ lowerList (ConsVal (IntVal 3) (ConsVal (SymVal "nil") (SymVal "nil")))) == "[3,nil]"
                      , (show $ lowerList (ConsVal (IntVal 3) (ConsVal (SymVal "nil") (ConsVal (ConsVal (IntVal 3) (SymVal "nil")) (SymVal "nil"))))) == "[3,nil,(3 )]"
                      ]

tests_otherRuntime :: [Bool]
tests_otherRuntime = [ withRuntime "(eq? 3 't)" == "nil"
                     , withRuntime "(eq? 3 3 3)" == "t"
                     , withRuntime "(eq? 'x 'x 'x)" == "t"
                     , withRuntime "(eq?)" == "t"
                     , withRuntime "(eq? 6)" == "t"
                     , withRuntime "(list 3 5 2 5)" == "(3 5 2 5 )"
                     , withRuntime "(list)" == "nil"
                     , withRuntime "(list 'eq? 5 't 'nil)" == "(eq? 5 t nil )"
                     ]

--- Evaluation
--- ----------

evaluatorTests :: [([Bool], String)]
evaluatorTests = map (boolTests testEval) testBank

testEval :: TestCase -> Bool
testEval (_, exps, val) = let result = show (genVals exps runtime)
                                 in  (result == val || bothHaveExn val result)
    where
        bothHaveExn v r =    "Exception" `isInfixOf` v
                          && "Exception" `isInfixOf` r

genVals :: [Exp] -> Env -> [Val]
genVals []     env  = []
genVals (e:es) env  = let evald = eval e env
                      in  evald : (genVals es $ nextEnv env evald)

--- ### Check parameter names

tests_paramStrs :: [Bool]
tests_paramStrs = [ paramStrs [] == Right []
                  , paramStrs [SymExp "x", SymExp "oentuh", SymExp "yoeu"] == Right ["x","oentuh","yoeu"]
                  , paramStrs [SymExp "x", SymExp "oentuh", IntExp 6, SymExp "yoeu"] == Left "Must use only `SymExp` for parameter names."
                  , paramStrs [SymExp "x", SymExp "oentuh", SExp [], SymExp "yoeu"] == Left "Must use only `SymExp` for parameter names."
                  , paramStrs [SExp [SymExp "x", SymExp "y"]] == Left "Must use only `SymExp` for parameter names."
                  ]

--- ### Quoting, Quasi-Quoting, and Unquoting

--- ### Evaluation - the function!

--- #### Integer, Symbol, and Empty Forms

--- #### Variable Definition Forms

--- #### Function Definition and Lambda Function Forms

--- #### Conditional Form

--- #### Let Form

--- #### Cons Form

--- #### Quoting, Quasi-Quoting, and Unquoting Forms

--- #### Eval Form

--- #### Macro Form

--- #### Application Form

--- REPL
--- ----

--- ### Generating next environment

tests_nextEnv :: [Bool]
tests_nextEnv = [ (show $ nextEnv (fromList []) (IntVal 5)) == "fromList []"
                , (show $ nextEnv (fromList []) (SymVal "a")) == "fromList []"
                , (show $ nextEnv (fromList []) (DefVal "x" (IntVal 5))) == "fromList [(\"x\",5)]"
                , (show $ nextEnv (fromList []) (DefVal "x" (SymVal "x"))) == "fromList [(\"x\",x)]"
                , (show $ nextEnv (fromList [("x",IntVal 5)]) (DefVal "x" (SymVal "x"))) == "fromList [(\"x\",x)]"
                , (show $ nextEnv (fromList [("x",IntVal 5)]) (DefVal "y" (SymVal "x"))) == "fromList [(\"x\",5),(\"y\",x)]"
                , (show $ nextEnv (fromList []) (DefVal "x" (ConsVal (SymVal "x") (SymVal "nil")))) == "fromList [(\"x\",(x ))]"
                ]

--- ### Main function

basicIntsSyms :: TestSuite
basicIntsSyms = ("Basic Integers/Symbols", [(["435"],[IntExp 435],"[435]"),(["a"],[SymExp "a"],"[*** Scheme-Exception: Symbol a has no value. ***]"),(["a","555"],[SymExp "a",IntExp 555],"[*** Scheme-Exception: Symbol a has no value. ***,555]"),(["555","a"],[IntExp 555,SymExp "a"],"[555,*** Scheme-Exception: Symbol a has no value. ***]")])
primitiveInts :: TestSuite
primitiveInts = ("Primitive Integer Operators", [(["+","-","*"],[SymExp "+",SymExp "-",SymExp "*"],"[*primitive*,*primitive*,*primitive*]"),([">","<",">=","<=","=","!="],[SymExp ">",SymExp "<",SymExp ">=",SymExp "<=",SymExp "=",SymExp "!="],"[*primitive*,*primitive*,*primitive*,*primitive*,*primitive*,*primitive*]"),(["(+ 3 4 2 10)","(* 3 3 3 2)"],[SExp [SymExp "+",IntExp 3,IntExp 4,IntExp 2,IntExp 10],SExp [SymExp "*",IntExp 3,IntExp 3,IntExp 3,IntExp 2]],"[19,54]"),(["(+ 2 (* 3 4))","(- 20 1)","(- 10 5 2)","(- 10 (+ 4 5 (* 3 5)) (- 14 2) 22)"],[SExp [SymExp "+",IntExp 2,SExp [SymExp "*",IntExp 3,IntExp 4]],SExp [SymExp "-",IntExp 20,IntExp 1],SExp [SymExp "-",IntExp 10,IntExp 5,IntExp 2],SExp [SymExp "-",IntExp 10,SExp [SymExp "+",IntExp 4,IntExp 5,SExp [SymExp "*",IntExp 3,IntExp 5]],SExp [SymExp "-",IntExp 14,IntExp 2],IntExp 22]],"[14,19,3,-48]"),(["(> 5 3)","(> 6  4 2)","(> 6 4 2 6)"],[SExp [SymExp ">",IntExp 5,IntExp 3],SExp [SymExp ">",IntExp 6,IntExp 4,IntExp 2],SExp [SymExp ">",IntExp 6,IntExp 4,IntExp 2,IntExp 6]],"[t,t,nil]"),(["(>= 5 5 3)","(>= 5 6 3)","(<= 2 3 4)","(<= 2 2 3)","(<= 2 1 2)"],[SExp [SymExp ">=",IntExp 5,IntExp 5,IntExp 3],SExp [SymExp ">=",IntExp 5,IntExp 6,IntExp 3],SExp [SymExp "<=",IntExp 2,IntExp 3,IntExp 4],SExp [SymExp "<=",IntExp 2,IntExp 2,IntExp 3],SExp [SymExp "<=",IntExp 2,IntExp 1,IntExp 2]],"[t,nil,t,t,nil]"),(["(= 5 5 5 5)","(= 5 3 5 2)","(= 5 (+ 2 3) (- 8 3))","(= 4 2)","(= 4 (- 4 1) (+ 3 1))","(!= 1 2 1)","(!= 1 1 2)","(!= 22 22 22)","(!= (+ 1 22) 23 22)","(!= (+ 1 22) 24 26)"],[SExp [SymExp "=",IntExp 5,IntExp 5,IntExp 5,IntExp 5],SExp [SymExp "=",IntExp 5,IntExp 3,IntExp 5,IntExp 2],SExp [SymExp "=",IntExp 5,SExp [SymExp "+",IntExp 2,IntExp 3],SExp [SymExp "-",IntExp 8,IntExp 3]],SExp [SymExp "=",IntExp 4,IntExp 2],SExp [SymExp "=",IntExp 4,SExp [SymExp "-",IntExp 4,IntExp 1],SExp [SymExp "+",IntExp 3,IntExp 1]],SExp [SymExp "!=",IntExp 1,IntExp 2,IntExp 1],SExp [SymExp "!=",IntExp 1,IntExp 1,IntExp 2],SExp [SymExp "!=",IntExp 22,IntExp 22,IntExp 22],SExp [SymExp "!=",SExp [SymExp "+",IntExp 1,IntExp 22],IntExp 23,IntExp 22],SExp [SymExp "!=",SExp [SymExp "+",IntExp 1,IntExp 22],IntExp 24,IntExp 26]],"[t,nil,t,nil,nil,t,nil,nil,nil,t]")])
primitiveBools :: TestSuite
primitiveBools = ("Primitive Boolean Operators", [(["and","or","not"],[SymExp "and",SymExp "or",SymExp "not"],"[*primitive*,*primitive*,*primitive*]"),(["(and 't 't)","(and 't 'nil)","(and 't 't 't 't)","(and 't 'nil 6 4)","(and 't 5)","(and (> 4 2) (> 5 2))","(and (> 4 2) (> 2 5))"],[SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "nil"]],SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "nil"],IntExp 6,IntExp 4],SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"],IntExp 5],SExp [SymExp "and",SExp [SymExp ">",IntExp 4,IntExp 2],SExp [SymExp ">",IntExp 5,IntExp 2]],SExp [SymExp "and",SExp [SymExp ">",IntExp 4,IntExp 2],SExp [SymExp ">",IntExp 2,IntExp 5]]],"[t,nil,t,nil,t,t,nil]"),(["(or 't 't)","(or 't 'nil)","(or 'nil 'nil)","(or 5 2 6)","(or (and 't 't) 'nil)","(or (and 'nil 't) 6)"],[SExp [SymExp "or",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "or",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "nil"]],SExp [SymExp "or",SExp [SymExp "quote",SymExp "nil"],SExp [SymExp "quote",SymExp "nil"]],SExp [SymExp "or",IntExp 5,IntExp 2,IntExp 6],SExp [SymExp "or",SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "quote",SymExp "nil"]],SExp [SymExp "or",SExp [SymExp "and",SExp [SymExp "quote",SymExp "nil"],SExp [SymExp "quote",SymExp "t"]],IntExp 6]],"[t,t,nil,t,t,t]"),(["(not (> 5 3))","(not (< 5 3))","(not 't 't)","(not 'nil 't 't 't)"],[SExp [SymExp "not",SExp [SymExp ">",IntExp 5,IntExp 3]],SExp [SymExp "not",SExp [SymExp "<",IntExp 5,IntExp 3]],SExp [SymExp "not",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "not",SExp [SymExp "quote",SymExp "nil"],SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"]]],"[nil,t,*** Scheme-Exception: `not` is a unary operator. ***,*** Scheme-Exception: `not` is a unary operator. ***]")])
primitiveMisc :: TestSuite
primitiveMisc = ("Primitive Misc.", [(["()"],[SExp []],"[nil]"),(["eq?"],[SymExp "eq?"],"[*primitive*]"),(["(+)","(-)","(*)","(>)","(<)","(>=)","(<=)","(=)","(!=)","(eq?)","(and)","(or)","(not)","(list)","(car)","(cdr)"],[SExp [SymExp "+"],SExp [SymExp "-"],SExp [SymExp "*"],SExp [SymExp ">"],SExp [SymExp "<"],SExp [SymExp ">="],SExp [SymExp "<="],SExp [SymExp "="],SExp [SymExp "!="],SExp [SymExp "eq?"],SExp [SymExp "and"],SExp [SymExp "or"],SExp [SymExp "not"],SExp [SymExp "list"],SExp [SymExp "car"],SExp [SymExp "cdr"]],"[0,0,1,t,t,t,t,t,t,t,t,nil,*** Scheme-Exception: `not` is a unary operator. ***,nil,*** Scheme-Exception: `car` is a unary operator. ***,*** Scheme-Exception: `cdr` is a unary operator. ***]"),(["(+ 1)","(- 1)","(* 1)","(> 1)","(< 1)","(>= 1)","(<= 1)","(= 1)","(!= 1)","(eq? 'a)","(eq? 1)","(and 't)","(or 'nil)","(not 't)","(list 't)"],[SExp [SymExp "+",IntExp 1],SExp [SymExp "-",IntExp 1],SExp [SymExp "*",IntExp 1],SExp [SymExp ">",IntExp 1],SExp [SymExp "<",IntExp 1],SExp [SymExp ">=",IntExp 1],SExp [SymExp "<=",IntExp 1],SExp [SymExp "=",IntExp 1],SExp [SymExp "!=",IntExp 1],SExp [SymExp "eq?",SExp [SymExp "quote",SymExp "a"]],SExp [SymExp "eq?",IntExp 1],SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "or",SExp [SymExp "quote",SymExp "nil"]],SExp [SymExp "not",SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "list",SExp [SymExp "quote",SymExp "t"]]],"[1,1,1,t,t,t,t,t,t,t,t,t,nil,nil,(t )]"),(["(eq? 5 5)","(eq? 6 5 6)","(eq? (+ 3 3) 6)","(eq? 'a 5)","(eq? 'a 'b)","(eq? 'a 'a 'b)","(eq? 'a 'a 'a)"],[SExp [SymExp "eq?",IntExp 5,IntExp 5],SExp [SymExp "eq?",IntExp 6,IntExp 5,IntExp 6],SExp [SymExp "eq?",SExp [SymExp "+",IntExp 3,IntExp 3],IntExp 6],SExp [SymExp "eq?",SExp [SymExp "quote",SymExp "a"],IntExp 5],SExp [SymExp "eq?",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"]],SExp [SymExp "eq?",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"]],SExp [SymExp "eq?",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "a"]]],"[t,nil,t,nil,nil,nil,t]")])
consListCarCdr :: TestSuite
consListCarCdr = ("Cons, List, Car, Cdr", [(["cons","list","car","cdr"],[SymExp "cons",SymExp "list",SymExp "car",SymExp "cdr"],"[*** Scheme-Exception: Symbol cons has no value. ***,*primitive*,*primitive*,*primitive*]"),(["(list (> 3 4) 't 15 'nil (< 5 2 3 5))","(car (list 'a 'b))","(cdr (list 'a 'b))","(car (list 'a 'b 'c))","(cdr (list 'a 'b 'c))","(cdr (list 'a))","(cdr 'a)"],[SExp [SymExp "list",SExp [SymExp ">",IntExp 3,IntExp 4],SExp [SymExp "quote",SymExp "t"],IntExp 15,SExp [SymExp "quote",SymExp "nil"],SExp [SymExp "<",IntExp 5,IntExp 2,IntExp 3,IntExp 5]],SExp [SymExp "car",SExp [SymExp "list",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"]]],SExp [SymExp "cdr",SExp [SymExp "list",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"]]],SExp [SymExp "car",SExp [SymExp "list",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"],SExp [SymExp "quote",SymExp "c"]]],SExp [SymExp "cdr",SExp [SymExp "list",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"],SExp [SymExp "quote",SymExp "c"]]],SExp [SymExp "cdr",SExp [SymExp "list",SExp [SymExp "quote",SymExp "a"]]],SExp [SymExp "cdr",SExp [SymExp "quote",SymExp "a"]]],"[(nil t 15 nil nil ),a,(b ),a,(b c ),nil,*** Scheme-Exception: Not a cons cell: a ***]"),(["(cons 2 3)"],[SExp [SymExp "cons",IntExp 2,IntExp 3]],"[(2 . 3)]"),(["(cons 2 (cons 3 4))","(cons 2 (cons 3 (cons 4 'nil)))"],[SExp [SymExp "cons",IntExp 2,SExp [SymExp "cons",IntExp 3,IntExp 4]],SExp [SymExp "cons",IntExp 2,SExp [SymExp "cons",IntExp 3,SExp [SymExp "cons",IntExp 4,SExp [SymExp "quote",SymExp "nil"]]]]],"[(2 3 . 4),(2 3 4 )]"),(["(car (cons 2 (cons 3 (cons 4 'nil))))","(cdr (cons 2 (cons 3 (cons 4 'nil))))","(car (list 2 3 4 'a 'b))","(cdr (list 'b 'c 3 4 (+ 3 4)))"],[SExp [SymExp "car",SExp [SymExp "cons",IntExp 2,SExp [SymExp "cons",IntExp 3,SExp [SymExp "cons",IntExp 4,SExp [SymExp "quote",SymExp "nil"]]]]],SExp [SymExp "cdr",SExp [SymExp "cons",IntExp 2,SExp [SymExp "cons",IntExp 3,SExp [SymExp "cons",IntExp 4,SExp [SymExp "quote",SymExp "nil"]]]]],SExp [SymExp "car",SExp [SymExp "list",IntExp 2,IntExp 3,IntExp 4,SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"]]],SExp [SymExp "cdr",SExp [SymExp "list",SExp [SymExp "quote",SymExp "b"],SExp [SymExp "quote",SymExp "c"],IntExp 3,IntExp 4,SExp [SymExp "+",IntExp 3,IntExp 4]]]],"[2,(3 4 ),2,(c 3 4 7 )]"),(["(car (cons 1 2) (cons 2 3))","(cdr (cons 2 't) (list 4 2 4 5))"],[SExp [SymExp "car",SExp [SymExp "cons",IntExp 1,IntExp 2],SExp [SymExp "cons",IntExp 2,IntExp 3]],SExp [SymExp "cdr",SExp [SymExp "cons",IntExp 2,SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "list",IntExp 4,IntExp 2,IntExp 4,IntExp 5]]],"[*** Scheme-Exception: `car` is a unary operator. ***,*** Scheme-Exception: `cdr` is a unary operator. ***]")])
defDefineLambdaApply :: TestSuite
defDefineLambdaApply = ("Definition, Application", [(["(def x (+ 10 20))","x","y"],[SExp [SymExp "def",SymExp "x",SExp [SymExp "+",IntExp 10,IntExp 20]],SymExp "x",SymExp "y"],"[x,30,*** Scheme-Exception: Symbol y has no value. ***]"),(["(def x 1)","(define inc (y) (+ y x))","(inc 10)","(def x 2)","(inc 10)"],[SExp [SymExp "def",SymExp "x",IntExp 1],SExp [SymExp "define",SymExp "inc",SExp [SymExp "y"],SExp [SymExp "+",SymExp "y",SymExp "x"]],SExp [SymExp "inc",IntExp 10],SExp [SymExp "def",SymExp "x",IntExp 2],SExp [SymExp "inc",IntExp 10]],"[x,inc,11,x,11]"),(["(lambda (x) (+ x 10))","( (lambda (x) (+ x 10)) 20)","(define mkInc (x) (lambda (y) (+ x y)))","(def i2 (mkInc 2))","(i2 10)"],[SExp [SymExp "lambda",SExp [SymExp "x"],SExp [SymExp "+",SymExp "x",IntExp 10]],SExp [SExp [SymExp "lambda",SExp [SymExp "x"],SExp [SymExp "+",SymExp "x",IntExp 10]],IntExp 20],SExp [SymExp "define",SymExp "mkInc",SExp [SymExp "x"],SExp [SymExp "lambda",SExp [SymExp "y"],SExp [SymExp "+",SymExp "x",SymExp "y"]]],SExp [SymExp "def",SymExp "i2",SExp [SymExp "mkInc",IntExp 2]],SExp [SymExp "i2",IntExp 10]],"[*closure*,30,mkInc,i2,12]"),(["(define f ((+ 3 4) x y) (+ x y))","(def (+ 3 4) 5)"],[SExp [SymExp "define",SymExp "f",SExp [SExp [SymExp "+",IntExp 3,IntExp 4],SymExp "x",SymExp "y"],SExp [SymExp "+",SymExp "x",SymExp "y"]],SExp [SymExp "def",SExp [SymExp "+",IntExp 3,IntExp 4],IntExp 5]],"[*** Scheme-Exception: Must use only `SymExp` for parameter names. ***,*** Scheme-Exception: Symbol def has no value. ***]"),(["(define f (x y z) (+ (- x y) z))","(f 3 4 2 8 0)"],[SExp [SymExp "define",SymExp "f",SExp [SymExp "x",SymExp "y",SymExp "z"],SExp [SymExp "+",SExp [SymExp "-",SymExp "x",SymExp "y"],SymExp "z"]],SExp [SymExp "f",IntExp 3,IntExp 4,IntExp 2,IntExp 8,IntExp 0]],"[f,1]")])
allowsDefOverPrimForm :: TestSuite
allowsDefOverPrimForm = ("Overriding primitive/form Names", [(["(def define 3)","define"],[SExp [SymExp "def",SymExp "define",IntExp 3],SymExp "define"],"[define,3]"),(["(define let (x y z) (+ x y z))","(let 3 4 5)","(let ((x 5) (y 100) (z 3)) (+ x (- y z)))"],[SExp [SymExp "define",SymExp "let",SExp [SymExp "x",SymExp "y",SymExp "z"],SExp [SymExp "+",SymExp "x",SymExp "y",SymExp "z"]],SExp [SymExp "let",IntExp 3,IntExp 4,IntExp 5],SExp [SymExp "let",SExp [SExp [SymExp "x",IntExp 5],SExp [SymExp "y",IntExp 100],SExp [SymExp "z",IntExp 3]],SExp [SymExp "+",SymExp "x",SExp [SymExp "-",SymExp "y",SymExp "z"]]]],"[let,12,102]"),(["(def + 3)","(- + 2)","(* + 10)"],[SExp [SymExp "def",SymExp "+",IntExp 3],SExp [SymExp "-",SymExp "+",IntExp 2],SExp [SymExp "*",SymExp "+",IntExp 10]],"[+,1,30]"),(["(define quote (x y) (list x y))","(quote 'a 'b)","(quote 'a)"],[SExp [SymExp "define",SymExp "quote",SExp [SymExp "x",SymExp "y"],SExp [SymExp "list",SymExp "x",SymExp "y"]],SExp [SymExp "quote",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"]],SExp [SymExp "quote",SExp [SymExp "quote",SymExp "a"]]],"[quote,(a b ),(quote a )]")])
condForm :: TestSuite
condForm = ("`cond` form", [(["(cond ((> 4 3) 'a (> 4 2) 'b))","(cond ((< 4 3) 'a (> 4 2) 'b))","(cond ((< 4 3) 'a (< 4 2) 'b))"],[SExp [SymExp "cond",SExp [SExp [SymExp ">",IntExp 4,IntExp 3],SExp [SymExp "quote",SymExp "a"],SExp [SymExp ">",IntExp 4,IntExp 2],SExp [SymExp "quote",SymExp "b"]]],SExp [SymExp "cond",SExp [SExp [SymExp "<",IntExp 4,IntExp 3],SExp [SymExp "quote",SymExp "a"],SExp [SymExp ">",IntExp 4,IntExp 2],SExp [SymExp "quote",SymExp "b"]]],SExp [SymExp "cond",SExp [SExp [SymExp "<",IntExp 4,IntExp 3],SExp [SymExp "quote",SymExp "a"],SExp [SymExp "<",IntExp 4,IntExp 2],SExp [SymExp "quote",SymExp "b"]]]],"[a,b,nil]")])
letForm :: TestSuite
letForm = ("`let` form", [(["(let ((x 5) (y 10)) (+ x y))","(def x 20)","(def y 30)","(let ((x 11) (y 4)) (- (* x y) 2))","x","y"],[SExp [SymExp "let",SExp [SExp [SymExp "x",IntExp 5],SExp [SymExp "y",IntExp 10]],SExp [SymExp "+",SymExp "x",SymExp "y"]],SExp [SymExp "def",SymExp "x",IntExp 20],SExp [SymExp "def",SymExp "y",IntExp 30],SExp [SymExp "let",SExp [SExp [SymExp "x",IntExp 11],SExp [SymExp "y",IntExp 4]],SExp [SymExp "-",SExp [SymExp "*",SymExp "x",SymExp "y"],IntExp 2]],SymExp "x",SymExp "y"],"[15,x,y,42,20,30]")])
everythingElse :: TestSuite
everythingElse = ("Everything else", [(["435"],[IntExp 435],"[435]"),(["(def x 5)","x","y"],[SExp [SymExp "def",SymExp "x",IntExp 5],SymExp "x",SymExp "y"],"[x,5,*** Scheme-Exception: Symbol y has no value. ***]"),(["(f 10 30 x)"],[SExp [SymExp "f",IntExp 10,IntExp 30,SymExp "x"]],"[*** Scheme-Exception: Symbol f has no value. ***]"),(["+"],[SymExp "+"],"[*primitive*]"),(["()"],[SExp []],"[nil]"),(["'a","'5","(quote a)","'a","'asdf","'*first-val*"],[SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",IntExp 5],SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "asdf"],SExp [SymExp "quote",SymExp "*first-val*"]],"[a,5,a,a,asdf,*first-val*]"),(["(define fact (n) (cond ((< n 1) 1 't (* n (fact (- n 1))))))","(fact 5)"],[SExp [SymExp "define",SymExp "fact",SExp [SymExp "n"],SExp [SymExp "cond",SExp [SExp [SymExp "<",SymExp "n",IntExp 1],IntExp 1,SExp [SymExp "quote",SymExp "t"],SExp [SymExp "*",SymExp "n",SExp [SymExp "fact",SExp [SymExp "-",SymExp "n",IntExp 1]]]]]],SExp [SymExp "fact",IntExp 5]],"[fact,120]"),(["'a","''a","(car (quote (a b c)))","(car '(a b c))","(car ''(a b c))","'(2 3 4)","(list (+ 2 3))","'( (+ 2 3))","'(+ 2 3)"],[SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SExp [SymExp "quote",SymExp "a"]],SExp [SymExp "car",SExp [SymExp "quote",SExp [SymExp "a",SymExp "b",SymExp "c"]]],SExp [SymExp "car",SExp [SymExp "quote",SExp [SymExp "a",SymExp "b",SymExp "c"]]],SExp [SymExp "car",SExp [SymExp "quote",SExp [SymExp "quote",SExp [SymExp "a",SymExp "b",SymExp "c"]]]],SExp [SymExp "quote",SExp [IntExp 2,IntExp 3,IntExp 4]],SExp [SymExp "list",SExp [SymExp "+",IntExp 2,IntExp 3]],SExp [SymExp "quote",SExp [SExp [SymExp "+",IntExp 2,IntExp 3]]],SExp [SymExp "quote",SExp [SymExp "+",IntExp 2,IntExp 3]]],"[a,(quote a ),a,a,quote,(2 3 4 ),(5 ),((+ 2 3 ) ),(+ 2 3 )]"),(["'(+ 1 2)","(eval '(+ 1 2))","(eval ''(+ 1 2))","(eval (eval ''(+ 1 2)))","(def a '(+ x 1))","(def x 5)","(eval a)"],[SExp [SymExp "quote",SExp [SymExp "+",IntExp 1,IntExp 2]],SExp [SymExp "eval",SExp [SymExp "quote",SExp [SymExp "+",IntExp 1,IntExp 2]]],SExp [SymExp "eval",SExp [SymExp "quote",SExp [SymExp "quote",SExp [SymExp "+",IntExp 1,IntExp 2]]]],SExp [SymExp "eval",SExp [SymExp "eval",SExp [SymExp "quote",SExp [SymExp "quote",SExp [SymExp "+",IntExp 1,IntExp 2]]]]],SExp [SymExp "def",SymExp "a",SExp [SymExp "quote",SExp [SymExp "+",SymExp "x",IntExp 1]]],SExp [SymExp "def",SymExp "x",IntExp 5],SExp [SymExp "eval",SymExp "a"]],"[(+ 1 2 ),3,(+ 1 2 ),3,a,x,6]"),(["(def a 5)","`(+ a 1)","`(+ ,a 1)"],[SExp [SymExp "def",SymExp "a",IntExp 5],SExp [SymExp "quasiquote",SExp [SymExp "+",SymExp "a",IntExp 1]],SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SymExp "a"],IntExp 1]]],"[a,(+ a 1 ),(+ 5 1 )]"),(["(defmacro if (con then else) `(cond (,con ,then 't ,else)))","(def a 5)","(if (> a 2) 10 20)","(if (< a 2) 10 20)","(define fact (n) (if (< n 1) 1 (* n (fact (- n 1)))))","(fact 10)","(defmacro mkplus (e) (if (eq? (car e) '-) (cons '+ (cdr e)) e))","(mkplus (- 5 4))"],[SExp [SymExp "defmacro",SymExp "if",SExp [SymExp "con",SymExp "then",SymExp "else"],SExp [SymExp "quasiquote",SExp [SymExp "cond",SExp [SExp [SymExp "unquote",SymExp "con"],SExp [SymExp "unquote",SymExp "then"],SExp [SymExp "quote",SymExp "t"],SExp [SymExp "unquote",SymExp "else"]]]]],SExp [SymExp "def",SymExp "a",IntExp 5],SExp [SymExp "if",SExp [SymExp ">",SymExp "a",IntExp 2],IntExp 10,IntExp 20],SExp [SymExp "if",SExp [SymExp "<",SymExp "a",IntExp 2],IntExp 10,IntExp 20],SExp [SymExp "define",SymExp "fact",SExp [SymExp "n"],SExp [SymExp "if",SExp [SymExp "<",SymExp "n",IntExp 1],IntExp 1,SExp [SymExp "*",SymExp "n",SExp [SymExp "fact",SExp [SymExp "-",SymExp "n",IntExp 1]]]]],SExp [SymExp "fact",IntExp 10],SExp [SymExp "defmacro",SymExp "mkplus",SExp [SymExp "e"],SExp [SymExp "if",SExp [SymExp "eq?",SExp [SymExp "car",SymExp "e"],SExp [SymExp "quote",SymExp "-"]],SExp [SymExp "cons",SExp [SymExp "quote",SymExp "+"],SExp [SymExp "cdr",SymExp "e"]],SymExp "e"]],SExp [SymExp "mkplus",SExp [SymExp "-",IntExp 5,IntExp 4]]],"[if,a,10,20,fact,3628800,mkplus,9]")])
nestedQuasiQuotes :: TestSuite
nestedQuasiQuotes = ("Nested Quasiquoting", [(["(def a 5)","``(+ ,,a 1)","``(+ ,,a ,a)","`(+ a ,,a)","``(+ a ,,a)","(eval ``(+ ,,a 1))","(eval (eval ``(+ ,,a 1)))"],[SExp [SymExp "def",SymExp "a",IntExp 5],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]],IntExp 1]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]],SExp [SymExp "unquote",SymExp "a"]]]],SExp [SymExp "quasiquote",SExp [SymExp "+",SymExp "a",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SymExp "a",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]],SExp [SymExp "eval",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]],IntExp 1]]]],SExp [SymExp "eval",SExp [SymExp "eval",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]],IntExp 1]]]]]],"[a,(quasiquote (+ (unquote 5 ) 1 ) ),(quasiquote (+ (unquote 5 ) (unquote a ) ) ),(+ a *** Scheme-Exception: Cannot `unquote` more than `quasiquote`. *** ),(quasiquote (+ a (unquote 5 ) ) ),(+ 5 1 ),6]"),(["(def a 5)","```(+ ,,,a ,,a)","```(+ ,a ,,a)","```(+ `a `(+ ,,,,a a))"],[SExp [SymExp "def",SymExp "a",IntExp 5],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]],SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SymExp "a"],SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "quasiquote",SymExp "a"],SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]],SymExp "a"]]]]]]],"[a,(quasiquote (quasiquote (+ (unquote (unquote 5 ) ) (unquote (unquote a ) ) ) ) ),(quasiquote (quasiquote (+ (unquote a ) (unquote (unquote a ) ) ) ) ),(quasiquote (quasiquote (+ (quasiquote a ) (quasiquote (+ (unquote (unquote (unquote 5 ) ) ) a ) ) ) ) )]"),(["(def a 5)","```a","```,a","```,,a","```,,,a","(eval ```,,,a)","(eval (eval ```,,,a))"],[SExp [SymExp "def",SymExp "a",IntExp 5],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SymExp "a"]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "unquote",SymExp "a"]]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]]],SExp [SymExp "eval",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]]]],SExp [SymExp "eval",SExp [SymExp "eval",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]]]]]],"[a,(quasiquote (quasiquote a ) ),(quasiquote (quasiquote (unquote a ) ) ),(quasiquote (quasiquote (unquote (unquote a ) ) ) ),(quasiquote (quasiquote (unquote (unquote 5 ) ) ) ),(quasiquote (unquote 5 ) ),5]")])
