module Grammar.AST
( Stylesheet
, stylesheet
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos (initialPos)
import Grammar.Token

data Stylesheet = Stylesheet [RuleSet] deriving (Show)

data RuleSet = RuleSet [Selector] [Declaration] deriving (Show)

type Selector = [Char]  -- TODO
type Declaration = [Char]  -- TODO

stylesheet :: CharParser st Stylesheet
stylesheet = do
  many whitespace
  -- TODO other things than rulesets
  rulesets <- endBy ruleset (many whitespace)
  eof
  return $ Stylesheet rulesets

ruleset = do
  selectors <- sepBy1 selector $ do { char ','; many whitespace }
  many whitespace
  char '{'
  -- TODO actually read declarations
  many whitespace
  char '}'
  return $ RuleSet selectors []

selector = simple_selector -- TODO parse compound selectors

-- this stuff is horrible and breaks
{-selector = do-}
  {-parts <- try (sepBy1 (try simple_selector) (try combinator))-}
  {-return $ concat parts-}

{-combinator = do { char '+'; many whitespace; return '+' }-}
  {-<|> do { char '>'; many whitespace; return '>' }-}
  {-<|> do { whitespace; return ' ' }-}
  {-[><?> "combinator"<]-}

simple_selector = do
  (Ident i) <- ident
  return i
  <?> "simple selector"
