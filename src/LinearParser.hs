{-# LANGUAGE OverloadedStrings #-}

module LinearParser where

import Text.Parsec

import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Types

langDef :: Token.LanguageDef st
langDef = emptyDef {
  commentStart = "{-",
  commentEnd = "-}",
  commentLine = "--",
  nestedComments = True,
  identStart = letter,
  identLetter = alphaNum,
  reservedOpNames = [":", "->", "\\", "$", "|", "="],
  reservedNames = ["case", "of", "data"],
  caseSensitive = True
}

tokenParser :: Token.TokenParser st
tokenParser = Token.makeTokenParser langDef

identifier :: Parsec String () String
identifier = Token.identifier tokenParser
whiteSpace :: Parsec String () ()
whiteSpace = Token.whiteSpace tokenParser

reservedOp, reserved :: String -> Parsec String () ()
reservedOp = Token.reservedOp tokenParser
reserved = Token.reserved tokenParser

parens, braces, lexeme :: Parsec String () a -> Parsec String () a
parens = Token.parens tokenParser
braces = Token.braces tokenParser
lexeme = Token.lexeme tokenParser

commaSep1 :: Parsec String () a -> Parsec String () [a]
commaSep1 = Token.commaSep1 tokenParser

upperIdentifier :: Parsec String () String
upperIdentifier = lookAhead upper *> identifier

lowerIdentifier :: Parsec String () String
lowerIdentifier = lookAhead lower *> identifier

parseTypeDecls :: Parsec String () [TypeDecl]
parseTypeDecls = many parseTypeDecl

-- data TYPE = CONS: Int -> Int -> Type | Type
parseTypeDecl :: Parsec String () TypeDecl
parseTypeDecl = do
  typeName <- reserved "data" *> upperIdentifier <* reservedOp "="
  consDecls <- parseConsDecl `sepBy` (reservedOp "|")
  return $ TypeDecl typeName consDecls

parseConsDecl :: Parsec String () ConsDecl
parseConsDecl = ConsDecl <$> upperIdentifier <*> (reservedOp ":" *> parseTyp)

parseConsName :: Parsec String () String
parseConsName = upperIdentifier

parseCons :: Parsec String () Expr
parseCons = Cons <$> parseConsName

-- case Just x of {
-- Just x -> x,
-- Nothing -> y
-- }
parseCase :: Parsec String () Expr
parseCase = do
  reserved "case" 
  e <- parens parseExpr 
  reserved "of"
  branches <- braces (commaSep1 parseCaseBranch)
  return $ Case e branches

parseCaseBranch :: Parsec String () (ConsElim, Expr)
parseCaseBranch = do
  elim <- parseConsElim
  reservedOp "->"
  e <- parseExpr
  return (elim, e)

parseConsElim :: Parsec String () ConsElim
parseConsElim = ConsElim <$> upperIdentifier <*> many parseBinding

parseBinding :: Parsec String () ConsElim
parseBinding = (Binding <$> lowerIdentifier) <|> parens parseConsElim

parseVar :: Parsec String () Expr
parseVar = Var <$> lowerIdentifier

parseTypBase :: Parsec String () Type
parseTypBase =  parens parseTyp
            <|> (Type <$> upperIdentifier)

parseTyp :: Parsec String () Type
parseTyp = chainr1 parseTypBase op
  where op = reservedOp "->" *> pure Func

-- \x:A -> x
parseLambda :: Parsec String () Expr
parseLambda = do
  arg <- reservedOp "\\" *> identifier
  reservedOp ":"
  typ <- parens parseTyp
  reservedOp "->"
  e <- parseExpr
  return $ Lam arg typ e

parseNonApp :: Parsec String () Expr
parseNonApp = parens parseExpr
            <|> parseLambda 
            <|> parseCase
            <|> parseCons              
            <|> parseVar

parseExpr :: Parsec String () Expr
parseExpr = chainl1 parseNonApp op
  where op = reservedOp "$" *> pure App

betweenParen :: Parsec String a b -> Parsec String a b
betweenParen = between (char '(') (char ')')

parseProg :: Parsec String () Prog
parseProg = Prog <$> (whiteSpace *> parseTypeDecls) <*> (parseExpr <* eof)
