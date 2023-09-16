module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "repeat", "skip", "until", "end"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
arithOp :: Parser (Exp Int -> Exp Int -> Exp Int)
arithOp = do { reservedOp lis "+"; return Plus }
      <|> do { reservedOp lis "-"; return Minus}
      <|> do { reservedOp lis "*" ; return Times }
      <|> do { reservedOp lis "/" ; return Div}

intexp :: Parser (Exp Int)
intexp =
  do  {
      ; n <- natural lis
      ; return $ Const (fromIntegral n)
      }

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

cmpOp :: Parser (Exp Int -> Exp Int -> Exp Bool)
cmpOp = do { reservedOp lis "=="; return Eq  }
    <|> do { reservedOp lis "!="; return NEq }
    <|> do { reservedOp lis "<" ; return Lt  }
    <|> do { reservedOp lis ">" ; return Gt  }
    <|> unexpected "comparison operator"

boolOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
boolOp = do { reservedOp lis "&&"; return And }
     <|> do { reservedOp lis "||"; return Or  }  

boolexp :: Parser (Exp Bool)
boolexp = boolexp' `chainl1` boolOp

boolexp' :: Parser (Exp Bool)
boolexp' = try (do {
            ; e1 <- intexp
            ; op <- cmpOp
            ; e2 <- intexp
            ; return $ op e1 e2
          })
          <|>
          try (do {
            ; reservedOp lis "!"
            ; b <- boolexp
            ; return $ Not b
          })
          <|>
          try (parens lis boolexp)
          <|>
          try (do {
                  ; s <- reserved lis "true"
                  ; return BTrue
                  })
          <|>
          try (do {
                  ; s <- reserved lis "false"
                  ; return BFalse
                  })



-----------------------------------
--- Parser de comandos
-----------------------------------

seqOp :: Parser (Comm -> Comm -> Comm)
seqOp = do  { reservedOp lis ";"
            ; return Seq
            }

comm' :: Parser Comm
comm' = do { try $ reserved lis "skip"
          ; return Skip
          }
        <|>
        try (do {
          ; v <- identifier lis
          ; reservedOp lis "="
          ; e <- intexp
          ; return $ Let v e
          })
        <|>
        try (do {
          ; reserved lis "if"
          ; b <- boolexp
          ; c1 <- braces lis comm
          ; (do {
            ; reserved lis "else"
            ; c2 <- braces lis comm
            ; return $ IfThenElse b c1 c2
          }
          <|>
          (return $ IfThen b c1))
        })
        <|>
        try (do {
          ; reserved lis "repeat"
          ; c <- comm
          ; reserved lis "until"
          ; b <- boolexp
          ; reserved lis "end"
          ; return $ Repeat c b
        })


comm :: Parser Comm
comm = comm' `chainl1` seqOp

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
