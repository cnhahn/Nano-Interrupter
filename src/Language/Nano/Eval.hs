{-# LANGUAGE OverloadedStrings #-}

module Language.Nano.Eval
  ( execFile, execString, execExpr
  , eval, lookupId, prelude
  , parse
  , env0
  )
  where

import Control.Exception (throw, catch)
import Language.Nano.Types
import Language.Nano.Parser

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (readFile f >>= execString) `catch` exitError

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) `catch` exitError

--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "True"
-- EBool True
--
-- >>> parse "False"
-- EBool False
--
-- >>> parse "123"
-- EInt 123
--
-- >>> parse "foo"
-- EVar "foo"
--
-- >>> parse "x + y"
-- EBin Plus (EVar "x") (EVar "y")
--
-- >>> parse "if x <= 4 then a || b else a && b"
-- EIf (EBin Le (EVar "x") (EInt 4)) (EBin Or (EVar "a") (EVar "b")) (EBin And (EVar "a") (EVar "b"))
--
-- >>> parse "if 4 <= z then 1 - z else 4 * z"
-- EIf (EBin Le (EInt 4) (EVar "z")) (EBin Minus (EInt 1) (EVar "z")) (EBin Mul (EInt 4) (EVar "z"))
--
-- >>> parse "let a = 6 * 2 in a /= 11"
-- ELet "a" (EBin Mul (EInt 6) (EInt 2)) (EBin Ne (EVar "a") (EInt 11))
--
-- >>> parseTokens "() (  )"
-- Right [LPAREN (AlexPn 0 1 1),RPAREN (AlexPn 1 1 2),LPAREN (AlexPn 3 1 4),RPAREN (AlexPn 6 1 7)]
--
-- >>> parse "f x"
-- EApp (EVar "f") (EVar "x")
--
-- >>> parse "(\\ x -> x + x) (3 * 3)"
-- EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EBin Mul (EInt 3) (EInt 3))
--
-- >>> parse "(((add3 (x)) y) z)"
-- EApp (EApp (EApp (EVar "add3") (EVar "x")) (EVar "y")) (EVar "z")
--
-- >>> parse <$> readFile "tests/input/t1.hs"
-- EBin Mul (EBin Plus (EInt 2) (EInt 3)) (EBin Plus (EInt 4) (EInt 5))
--
-- >>> parse <$> readFile "tests/input/t2.hs"
-- ELet "z" (EInt 3) (ELet "y" (EInt 2) (ELet "x" (EInt 1) (ELet "z1" (EInt 0) (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1"))))))
--
-- >>> parse "1-2-3"
-- EBin Minus (EBin Minus (EInt 1) (EInt 2)) (EInt 3)
-- >>> parse "1+a&&b||c+d*e-f-g x"
-- EBin Or (EBin And (EBin Plus (EInt 1) (EVar "a")) (EVar "b")) (EBin Minus (EBin Minus (EBin Plus (EVar "c") (EBin Mul (EVar "d") (EVar "e"))) (EVar "f")) (EApp (EVar "g") (EVar "x")))
--
-- >>> parse "1:3:5:[]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))
--
-- >>> parse "[1,3,5]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseExpr

exitError :: Error -> IO Value
exitError (Error msg) = return (VErr msg)

--------------------------------------------------------------------------------
-- | `eval env e` evaluates the Nano expression `e` in the environment `env`
--   (i.e. uses `env` for the values of the **free variables** in `e`),
--   and throws an `Error "unbound variable"` if the expression contains
--   a free variable that is **not bound** in `env`.
--
-- part (a)
--
-- >>> eval env0 (EBin Minus (EBin Plus "x" "y") (EBin Plus "z" "z1"))
-- 0
--
-- >>> eval env0 "p"
-- *** Exception: Error {errMsg = "unbound variable: p"}
--
-- part (b)
--
-- >>> eval []  (EBin Le (EInt 2) (EInt 3))
-- True
--
-- >>> eval []  (EBin Eq (EInt 2) (EInt 3))
-- False
--
-- >>> eval []  (EBin Eq (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> eval []  (EBin Lt (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> let e1 = EIf (EBin Lt "z1" "x") (EBin Ne "y" "z") (EBool False)
-- >>> eval env0 e1
-- True
--
-- >>> let e2 = EIf (EBin Eq "z1" "x") (EBin Le "y" "z") (EBin Le "z" "y")
-- >>> eval env0 e2
-- False
--
-- part (c)
--
-- >>> let e1 = EBin Plus "x" "y"
-- >>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
-- >>> eval [] e2
-- 3
--
-- part (d)
--
-- >>> eval [] (EApp (ELam "x" (EBin Plus "x" "x")) (EInt 3))
-- 6
--
-- >>> let e3 = ELet "h" (ELam "y" (EBin Plus "x" "y")) (EApp "f" "h")
-- >>> let e2 = ELet "x" (EInt 100) e3
-- >>> let e1 = ELet "f" (ELam "g" (ELet "x" (EInt 0) (EApp "g" (EInt 2)))) e2
-- >>> eval [] e1
-- 102
--
-- part (e)
-- |
-- >>> :{
-- eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq "n" (EInt 0))
--                                  (EInt 1)
--                                  (EBin Mul "n" (EApp "fac" (EBin Minus "n" (EInt 1))))))
--             (EApp "fac" (EInt 10)))
-- :}
-- 3628800
--
-- part (f)
--
-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr el
-- (1 : (2 : []))
-- >>> execExpr (EApp "head" el)
-- 1
-- >>> execExpr (EApp "tail" el)
-- (2 : [])
--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
--------------------------------------------------------------------------------
--eval = error "TBD:eval"
eval _ (EInt int)                 = (VInt int)
eval _ (EBool bool)               = (VBool bool) 
eval _ (ENil)                     = (VNil)
eval env (EVar id)                = (lookupId id env)

eval env (EBin Plus expr1 expr2)  = evalOp Plus (eval env expr1) (eval env expr2)
eval env (EBin Minus expr1 expr2) = evalOp Minus (eval env expr1) (eval env expr2)
eval env (EBin Mul expr1 expr2)   = evalOp Mul  (eval env expr1) (eval env expr2)
eval env (EBin Div expr1 expr2)   = evalOp Div  (eval env expr1) (eval env expr2)
eval env (EBin Eq expr1 expr2)    = evalOp Eq   (eval env expr1) (eval env expr2)
eval env (EBin Ne expr1 expr2)    = evalOp Ne   (eval env expr1) (eval env expr2)
eval env (EBin Lt expr1 expr2)    = evalOp Lt   (eval env expr1) (eval env expr2)
eval env (EBin Le expr1 expr2)    = evalOp Le   (eval env expr1) (eval env expr2)
eval env (EBin And expr1 expr2)   = evalOp And  (eval env expr1) (eval env expr2)
eval env (EBin Or expr1 expr2)    = evalOp Or   (eval env expr1) (eval env expr2)
eval env (EBin Cons expr1 expr2)  = evalOp Cons (eval env expr1) (eval env expr2)

eval env (EIf expr1 expr2 expr3) 
   | (((eval env expr1)) == ((eval env (EBool True)))) = (eval env expr2)
   | (((eval env expr1)) == ((eval env (EBool False)))) = (eval env expr3)
   | otherwise = VErr( throw (Error ("type error EIf")) )

------------------------------------------------------------------------------- 

eval env (ELet id expr1 expr2) = eval (env') expr2
     where
       env' = [(id, (eval env' expr1))] ++ env

--let blank = blank
--env dosent include 
--let env including id env and e2

--eval env (ELet id expr1 expr2) = let env' = ([(id, (eval env' expr1))] ++ env) in eval env' expr2
     
-------------------------------------------------------------------------------
eval env (ELam id expr)           = (VClos env id expr)

-------------------------------------------------------------------------------
--eval env (EApp a b) = eval v e
--     where 
--     VClos env' id e = eval env a
--     arg = eval env b
--     v = [(id , (arg + env'))] 
     
eval env (EApp a b) = case (eval env a) of
     VClos env' id e -> eval ( [(id, (eval env b))] ++ env' ) e
     VPrim f -> f (eval env b) 
     _ -> VErr( throw (Error ("type error EApp")) )


--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------
--evalOp a b c = error "TBD:evalOp"
-- = -- -- is the example stuff on how to handel Err _ honestly no clue how to handel that case atm.

evalOp Plus (VInt value1) (VInt value2)  = (VInt (value1 + value2))
evalOp Plus _ _                          = VErr( throw (Error ("type error: binop")) )
--evalOp Plus (VClos _ _ value1) (VClos _ _ value2)  = (VClos _ _ (value1 + value2))

evalOp Minus (VInt value1) (VInt value2) = (VInt (value1 - value2))
evalOp Minus _ _                         = VErr( throw (Error ("type error: binop")) )

evalOp Mul (VInt value1)  (VInt value2)  = (VInt (value1 * value2))
evalOp Mul _ _                           = VErr( throw (Error ("type error: binop")) )

evalOp Div (VInt value1) (VInt value2)   = (VInt (div value1 value2))
evalOp Div (VInt _) (VInt 0)             = VErr( throw (Error ("type error: binop")) )
evalOp Div _ _                           = VErr( throw (Error ("type error: binop")) )

evalOp Eq (VInt value1)  (VInt value2)   = (VBool (value1 == value2))
evalOp Eq (VBool value1) (VBool value2)  = (VBool (value1 == value2))
evalOp Eq (VNil) (VNil)                  = (VBool True)
evalOp Eq (VNil) _                  = (VBool False)
evalOp Eq  _ (VNil)                 = (VBool False)
evalOp Eq (VPair value1 value2) (VPair value3 value4)  
                          = case ((evalOp Eq value1 value3), (evalOp Eq value2 value4)) of  
                          (VBool True, VBool True) -> (VBool True)     
                          (VBool _, VBool _) -> (VBool False)
                          (VBool _, VNil) -> (VBool False)
                          (VNil, VBool _) -> (VBool False)
                          (VInt _, VNil) -> (VBool False)
                          (VNil, VInt _) -> (VBool False)
                          (VNil, VNil) -> (VBool True)
                          _ -> VErr( throw (Error ("type error: binop")) )
                                   
evalOp Eq _ _                            = VErr( throw (Error ("type error: binop")) )

--recurrsions if they are are the two remaining list equavlent.

evalOp Ne (VInt value1)  (VInt value2)   = (VBool (value1 /= value2))
evalOp Ne (VBool value1) (VBool value2)  = (VBool (value1 /= value2))
evalOp Ne (VPair value1 value2) (VPair value3 value4)  
                          = case ((evalOp Ne value1 value3), (evalOp Ne value2 value4)) of  
                          (VBool True, VBool True) -> (VBool False)     
                          (VBool _, VBool _) -> (VBool True)
                          (VBool _, VNil) -> (VBool True)
                          (VNil, VBool _) -> (VBool True)
                          (VInt _, VNil) -> (VBool True)
                          (VNil, VInt _) -> (VBool True)
                          (VNil, VNil) -> (VBool False)
                          _ -> VErr( throw (Error ("type error: binop")) )
evalOp Ne _ _                            = VErr( throw (Error ("type error: binop")) )

evalOp Lt (VInt value1) (VInt value2)    = (VBool (value1 < value2))
evalOp Lt _ _                            = VErr( throw (Error ("type error: binop")) )

evalOp Le (VInt value1) (VInt value2)    = (VBool (value1 <= value2))
evalOp Le _ _                            = VErr( throw (Error ("type error: binop")) )

evalOp And (VBool value1) (VBool value2) = (VBool (value1 && value2))
evalOp And _ _                           = VErr( throw (Error ("type error: binop")) )

evalOp Or (VBool value1) (VBool value2)  = (VBool (value1 || value2))
evalOp Or _ _                            = VErr( throw (Error ("type error: binop")) )



evalOp Cons (x) (VNil)                   = (VPair x (VNil))
evalOp Cons (x) (VPair a b)              = (VPair x (VPair a b))

evalOp Cons _ _                          = VErr( throw (Error ("type error: binop")) )


-- more than one case errors maybe thrown 

-- evalOp Cons value1 value2 = (VPair(value1 Cons value2)) more than one case errors maybe thrown

--------------------------------------------------------------------------------
-- | `lookupId x env` returns the most recent
--   binding for the variable `x` (i.e. the first
--   from the left) in the list representing the
--   environment, and throws an `Error` otherwise.
--
-- >>> lookupId "z1" env0
-- vint0
-- >>> lookupId "x" env0
-- vint1
-- >>> lookupId "y" env0
-- vint2
-- >>> lookupId "mickey" env0
-- *** Exception: Error {errMsg = "unbound variable: mickey"}
--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
--lookupId = error "TBD:lookupId"
--lookupId key [] = error ("unbound variable: " ++ key)
lookupId key [] = throw (Error ("unbound variable: " ++ key))
lookupId key ((id, val):t) = if key == id then val else lookupId key t 

prelude :: Env
prelude =
  [ -- HINT: you may extend this "built-in" environment
    -- with some "operators" that you find useful...
   ("head", VPrim(\x -> case x of
                        VPair z r -> z
                        _ -> VErr( throw (Error ("type error:prelude")) )
                  ) 
   ),
   
   ("tail", VPrim(\x -> case x of
                        VPair z r -> r
                        _ -> VErr( throw (Error ("type error:prelude")) )
                  ) 
   )
   
    
   
  ]

env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]

--------------------------------------------------------------------------------

