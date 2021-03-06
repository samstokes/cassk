module Grammar.Token
( CssToken(Ident)
, css_token
, ident
, whitespace
) where

import Text.ParserCombinators.Parsec

data CssToken = Ident [Char]
              | CssString [Char]
              | Hash [Char]
              | Number Float
              | Percentage Float
              | S
              | Comment [Char]
              | Delim Char
    deriving (Show)

css_token :: CharParser st CssToken
-- based on http://www.w3.org/TR/CSS21/syndata.html#tokenization
-- TODO implement parsers for all the tokens
css_token = (try ident)
    {-<|> atkeyword-}
    <|> (try css_string)
    {-<|> invalid-}
    <|> hash
    <|> (try percentage)
    <|> (try number)
    {-<|> dimension-}
    {-<|> uri-}
    {-<|> unicode_range-}
    {-<|> cdo-}
    {-<|> cdc-}
    {-<|> oneOf ";{}()[]"-}
    <|> (try whitespace)
    <|> (try comment)
    {-<|> function-}
    {-<|> includes-}
    {-<|> dashmatch-}
    <|> delim

nmchar = char '_' <|> letter <|> digit <|> char '-'
  <?> "name character"

ident = do
  hyphen <- option "" (string "-")
  first <- char '_' <|> letter
  rest <- many nmchar
  return (Ident (hyphen ++ (first : rest)))
  <?> "identifier"

hash = do
  char '#'
  name <- many nmchar
  return $ Hash name

whitespace = many1 space >> return S
  <?> "whitespace"

comment = do
  -- FIXME doesn't return contents of comment correctly
  string "/*"
  part1 <- many (noneOf "*")
  many1 $ char '*'
  part2 <- many $ do
    first <- noneOf "/*"
    second <- many $ noneOf "*"
    many1 $ char '*'
  string "/"
  return $ Comment (part1 ++ (concat part2))

-- TODO delim isn't supposed to be such a catch-all
-- (in fact in http://www.w3.org/TR/CSS21/grammar.html it's not even
-- mentioned, but in http://www.w3.org/TR/CSS21/syndata.html#tokenization
-- it is...)
delim = oneOf ",.*{}()[]<>:;=+!" >>= \c -> return $ Delim c

percentage = do
  num <- parse_number
  char '%'
  return $ Percentage num

number = do
  num <- parse_number
  return $ Number num

parse_number :: CharParser st Float
parse_number = do
    negation <- option "" $ string "-"
    ipart <- many1 digit -- FIXME doesn't allow numbers of form .3
    -- TODO figure out how to make this grammatical without being a one-liner
    (try (do { char '.'; fpart <- many1 digit; return (read (negation ++ ipart ++ "." ++ fpart) :: Float) })) <|> return (read (negation ++ ipart) :: Float)

css_string = do
  contents <- quoted_string '"' <|> quoted_string '\''
  return $ CssString contents

quoted_string :: Char -> CharParser st [Char]
quoted_string quote = between qp qp (many $ noneOf (quote : "\n\r\f\\"))
    -- TODO doesn't allow escaped newlines etc
    where qp = string [quote]
