{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import OntoLogic

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Main Parser Entry
parseRegistry :: Parser Registry
parseRegistry = do
    sc
    items <- many parseItem
    eof
    return $ foldr addItem emptyRegistry items

data Item = IConcept Name (Maybe Name)
          | IProperty Property
          | IAxiom FOL

parseItem :: Parser Item
parseItem = parens (
        (try parseDefConcept) <|>
        (try parseDefProperty) <|>
        (IAxiom <$> parseFOL)
    )

parseDefConcept :: Parser Item
parseDefConcept = do
    _ <- symbol "def-concept"
    name <- identifier
    parent <- optional identifier
    return $ IConcept name parent

parseDefProperty :: Parser Item
parseDefProperty = do
    _ <- symbol "def-property"
    name <- identifier
    domain <- identifier
    range <- identifier
    return $ IProperty (Property name domain range)

parseFOL :: Parser FOL
parseFOL = 
    parseForAll <|>
    parseExists <|>
    parseImplies <|>
    parseIff <|>
    parseAnd <|>
    parseOr <|>
    parseNot <|>
    parsePredicate

parseForAll :: Parser FOL
parseForAll = do
    _ <- try (symbol "forall")
    var <- parens identifier
    body <- parens parseFOL
    return $ ForAll var body

parseExists :: Parser FOL
parseExists = do
    _ <- try (symbol "exists")
    var <- parens identifier
    body <- parens parseFOL
    return $ Exists var body

parseImplies :: Parser FOL
parseImplies = do
    _ <- try (symbol "implies")
    p <- parens parseFOL
    q <- parens parseFOL
    return $ Implies p q

parseIff :: Parser FOL
parseIff = do
    _ <- try (symbol "iff")
    p <- parens parseFOL
    q <- parens parseFOL
    return $ Iff p q

parseAnd :: Parser FOL
parseAnd = do
    _ <- try (symbol "and")
    p <- parens parseFOL
    q <- parens parseFOL
    return $ And p q

parseOr :: Parser FOL
parseOr = do
    _ <- try (symbol "or")
    p <- parens parseFOL
    q <- parens parseFOL
    return $ Or p q

parseNot :: Parser FOL
parseNot = do
    _ <- try (symbol "not")
    p <- parens parseFOL
    return $ Not p

parsePredicate :: Parser FOL
parsePredicate = do
    op <- identifier
    args <- many parseTerm
    return $ Predicate op args

parseTerm :: Parser Term
parseTerm = (Constant <$> try stringLiteral) 
        <|> (try (Constant . show <$> L.float))   -- Parse float (must have . or exp)
        <|> (try (Constant . show <$> L.decimal)) -- Parse integer
        <|> (Var <$> identifier)

identifier :: Parser String
identifier = lexeme ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

emptyRegistry :: Registry
emptyRegistry = Registry [] [] []

addItem :: Item -> Registry -> Registry
addItem (IConcept n p) r = r { concepts = (n, p) : concepts r }
addItem (IProperty p) r = r { properties = p : properties r }
addItem (IAxiom a) r = r { axioms = a : axioms r }
