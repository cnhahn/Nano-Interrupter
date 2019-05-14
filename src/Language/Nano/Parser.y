{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Nano.Parser (
    parseExpr
  , parseTokens
  ) where

import Language.Nano.Lexer
import Language.Nano.Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    true  { TRUE _   }
    false { FALSE _  }
    in    { IN _     }
    if    { IF _     }
    then  { THEN _   }
    else  { ELSE _   }
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '='   { EQB _    }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '&&'  { AND _    }
    '||'  { OR  _    }
    '=='  { EQL _    }
    '/='  { NEQ _    }
    '<'   { LESS _   }
    '<='  { LEQ _    }
    ':'   { COLON _  }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }
    '['   { LBRAC _  }
    ']'   { RBRAC _  }
    ','   { COMMA _  }


-- Operators
%right in
%nonassoc '=' '==' '/=' '<' '<=' if then else
%right ':' '->'
%left '||' '&&'
%left '+' '-'
%left '*'
%%

Top  : ID '=' Expr                 { $3 }
     | Expr                        { $1 }

--Expr : let ID '=' Expr in Expr   { EApp (ELam $2 $6) $4 }
Expr : if Expr then Expr else Expr { EIf ($2) ($4) ($6) }
Expr : let ID '=' Expr in Expr     { ELet $2 ($4) ($6)}
Expr : Expr Expr                   { EApp ($1) ($2) }
Expr : '\\' ID '->' Expr           { ELam $2 ($4) }
--Expr : let Expr ID '=' Expr      { ELet $2 ($3) ($5) }

Expr : '(' Expr ')'                { $2 }
Expr : '[' Expr ']'                { $2 }
Expr : '[' ']'                     { ENil }

Expr : ID                          { EVar $1 }
Expr : TNUM                        { EInt $1 }
Expr : true                        { EBool True }
Expr : false                       { EBool False }

Expr : Expr '*' Expr               { EBin Mul $1 $3 }
Expr : Expr '+' Expr               { EBin Plus $1 $3 }
Expr : Expr '-' Expr               { EBin Minus $1 $3 }
Expr : Expr '==' Expr              { EBin Eq $1 $3 }
Expr : Expr '/=' Expr              { EBin Ne $1 $3 }
Expr : Expr '<' Expr               { EBin Lt $1 $3 }
Expr : Expr '<=' Expr              { EBin Le $1 $3 }
Expr : Expr '&&' Expr              { EBin And $1 $3 }
Expr : Expr '||' Expr              { EBin Or $1 $3 }

Expr : Expr ',' Expr              { EBin Cons ($1) ($3) }
--Expr2 : Expr ',' Expr           { EBin Cons ($1) ($3) }
--      | Expr                    { EBin Cons ($1) ENil }  

Expr : Expr ':' Expr               { EBin Cons ($1) $3 }
--Expr3 : Expr ':' Expr           { EBin Cons ($1) ($3) }
--      | Expr                       { EBin Cons ($1) ENil } 


{
mkLam :: [Id] -> Expr -> Expr
mkLam []     e = e
mkLam (x:xs) e = ELam x (mkLam xs e)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens


}
