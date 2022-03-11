{-# LANGUAGE OverloadedStrings #-}

module Parser (module Parser) where

import Adhoc (Qual ((:=>)))
import Ast (Alt (Alt), Decl (DImpl, DVal), Exp (EApp, ELet, ELit, EVar), Lit (LInt, LRat, LString, LUnit), Pat (PAs, PCon, PLit, PVar, PWildcard), Program (Program), ReplExp (REDecl, REExp))
import Control.Monad (void)
import Data.Functor (($>), (<&>))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Void (Void)
import Name (Name (Id))
import Scheme (Scheme (Forall))
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (MonadParsec (eof, label, lookAhead, try), ParseErrorBundle, Parsec, between, choice, many, manyTill, optional, runParser, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, punctuationChar, separatorChar, space1, string, symbolChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Types (Kind (KStar), TyCon (TyCon), TyVar (TyVar), Typ (TApp, TCon, TVar), tUnit, (->>))

type Parser = Parsec Void Text

keywords :: [Text]
keywords = ["let", "in", "class", "instance"]

parseLisy :: Text -> Either (ParseErrorBundle Text Void) Program
parseLisy = runParser program "stub"

program :: Parser Program
program = many pDecl <&> Program

pName :: Parser Name
pName = (pSymbolName <|> pAsciiName) <?> "id"

pSymbolName :: Parser Name
pSymbolName = do
  keyword "("
  str <- lexeme ((:) <$> unicodeSymbol <*> many unicodeSymbol <?> "symbol")
  keyword ")"

  pure $ Id str
  where
    unicodeSymbol :: Parser Char
    unicodeSymbol = choice [symbolChar, letterChar, char '-', char ':']

pBinName :: Parser Name
pBinName = Id <$> lexeme ((:) <$> unicodeSymbol <*> many unicodeSymbol <?> "binary id")
  where
    unicodeSymbol :: Parser Char
    unicodeSymbol = choice [symbolChar, letterChar, char '-', char ':']

pAsciiName :: Parser Name
pAsciiName = (lexeme . try) $ do
  s <- (:) <$> letterChar <*> many alphaNumChar <?> "ascii id"
  if pack s `elem` keywords
    then fail $ "keyword " ++ show s ++ " is a reseverd keyword"
    else pure $ Id s

pScheme :: Parser Scheme
pScheme = label "type scheme" $ do
  t <- pTyp
  pure $ Forall [] ([] :=> t)

pReplExp :: Parser ReplExp
pReplExp = choice [pDecl', pExp'] <?> "repl expression"
  where
    pDecl' :: Parser ReplExp
    pDecl' = pDecl <&> REDecl

    pExp' :: Parser ReplExp
    pExp' = pExp <&> REExp

pDecl :: Parser Decl
pDecl = choice [pImpl, pVal] <?> "declaration"
  where
    pVal :: Parser Decl
    pVal = (label "val declaration" . try) $ do
      n <- lexeme pName
      keyword ":"
      sc <- lexeme pScheme
      pure $ DVal n sc

    pImpl :: Parser Decl
    pImpl = (label "impl declaration" . try) $ do
      n <- lexeme pName
      alt <- lexeme pAlt

      pure $ DImpl n alt

pPat :: Parser Pat
pPat = choice [pCon, pPLit, pAs, pVar, pGroup, pWildcard] <?> "pattern"
  where
    pPLit :: Parser Pat
    pPLit = try (PLit <$> pLit) <?> "pattern literal"

    pGroup :: Parser Pat
    pGroup = try (between (keyword "(") (keyword ")") pPat) <?> "pattern group"

    pVar :: Parser Pat
    pVar = try (lexeme $ PVar <$> pName) <?> "pattern variable"

    pAs :: Parser Pat
    pAs = try (lexeme $ PAs <$> pName <* keyword "@" <*> pPat) <?> "pattern named"

    pWildcard :: Parser Pat
    pWildcard = try (keyword "_" $> PWildcard) <?> "pattern wildcard"

    pCon :: Parser Pat
    pCon = (label "pattern type constructor" . try) $ do
      n <- pName
      ps <- many pPat

      pure $ PCon n ps

pAlt :: Parser Alt
pAlt = label "alternative" $ do
  ps <- many pPat
  keyword "="

  Alt ps <$> pExp

pExp :: Parser Exp
pExp = choice [pLet, pApp] <?> "expression"
  where
    pLet :: Parser Exp
    pLet = lexeme $ do
      keyword "let"
      n <- pName
      keyword "="
      e <- pExp
      keyword "in"

      ELet [(n, Alt [] e)] <$> pExp

    pApp :: Parser Exp
    pApp = lexeme $ do
      e1 <- pPrimary
      e2 <- many pPrimary

      pure $ foldl EApp e1 e2

pLit :: Parser Lit
pLit = choice [pInt, pString, pUnit] <?> "literal"
  where
    pInt :: Parser Lit
    pInt = lexeme (LInt <$> L.decimal) <?> "integer"

    pString :: Parser Lit
    pString = lexeme (LString <$> (char '\'' *> manyTill L.charLiteral (char '\''))) <?> "string"

    pUnit :: Parser Lit
    pUnit = try ((keyword "(" *> keyword ")") $> LUnit) <?> "unit"

pPrimary :: Parser Exp
pPrimary = choice [pGroup, pELit, pVar] <?> "primary"
  where
    pVar :: Parser Exp
    pVar = lexeme (EVar <$> pName <?> "variable")

    pELit :: Parser Exp
    pELit = ELit <$> pLit <?> "expression literal"

    pGroup :: Parser Exp
    pGroup = try (between (keyword "(") (keyword ")") pExp) <?> "group"

pTyp :: Parser Typ
pTyp = pBinApp <?> "type"
  where
    pSimple :: Parser Typ
    pSimple = choice [pGroup, pUnit, pApp] <?> "type primary"

    pUnit :: Parser Typ
    pUnit = try ((keyword "(" *> keyword ")") $> tUnit) <?> "type unit"

    pGroup :: Parser Typ
    pGroup = try (between (keyword "(") (keyword ")") pTyp) <?> "type group"

    pApp :: Parser Typ
    pApp = label "type application" $ do
      t1 <- pCon
      t2 <- many pTyp
      pure $ foldl TApp t1 t2

    pCon :: Parser Typ
    pCon = label "type constructor" $ do
      n <- lexeme pName
      pure $ TCon $ TyCon n KStar

    -- TODO: fix right associative making IO () -> Float turn into IO (() -> Float)
    pBinApp :: Parser Typ
    pBinApp = label "type binary" $ do
      l <- pSimple
      n <- optional pBinName
      go l n
      where
        go :: Typ -> Maybe Name -> Parser Typ
        go l Nothing = pure l
        go l (Just n) = do
          rat <- many pBinApp

          pure $ foldl TApp (TApp (TCon $ TyCon n KStar) l) rat

lined :: Parser a -> Parser a
lined p = p <* (void (keyword "\n") <|> void (keyword ";") <|> eof)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keyword :: Text -> Parser Text
keyword = lexeme . string

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")
