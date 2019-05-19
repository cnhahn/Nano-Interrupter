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

Expr : let ID '=' Expr in Expr     { ELet $2 ($4) ($6) }
     | let ID MID '=' Expr in Expr { ELet $2 ($3 $5) ($7)}
     | '\\' ID '->' Expr           { ELam $2 ($4) }
     | '\\' ID LMID '->' Expr      { ELam $2 ($3 $5) }
     | Expr ':'  Expr              { EBin Cons ($1) $3 }
     | if Expr then Expr else Expr { EIf ($2) ($4) ($6) }
     | Ors                         { $1 }

Ors : Ors '||' Ors                 { EBin Or $1 $3 }
    | Ands                         { $1 }    

Ands : Ands '&&' Ands              { EBin And $1 $3 } 
     | Comp                        { $1 }

Comp : Comp '==' Comp              { EBin Eq $1 $3 }
     | Comp '/=' Comp              { EBin Ne $1 $3 }
     | Comp '<'  Comp              { EBin Lt $1 $3 }
     | Comp '<=' Comp              { EBin Le $1 $3 }
     | Form                        { $1 }

Form : Form '+' Form               { EBin Plus $1 $3 }
     | Form '-' Form               { EBin Minus $1 $3 }
     | '[' ']'                     { ENil }
     | Muli                        { $1 }

Muli : Muli '*'  Muli              { EBin Mul $1 $3 }
     | Fact                        { $1 }

Fact : Fact Unit                   { EApp ($1) ($2) }
     | Unit                        { $1 }

Unit : '[' Expr ']'                { $2 }
     | '[' Commas ']'              { $2 }
     | '(' Expr ')'                { $2 }
     | TNUM                        { EInt $1 }
     | ID                          { EVar $1 }
     | true                        { EBool True }
     | false                       { EBool False }


Commas : Expr ','  Commas          { EBin Cons ($1) ($3) }
       | Expr                      { EBin Cons ($1) ENil } 
     
MID   : ID  MID                    { \x -> ELam $1 ($2 x) }
      | ID                         { \x -> ELam $1 x } 
   
LMID  : ID MID                     { \x -> ELam $1 ($2 x) }
      | ID                         { \x -> ELam $1 x } 

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
