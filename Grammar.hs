module Grammar
( CssToken
, css_token
) where

import Text.ParserCombinators.Parsec

data CssToken = Ident [Char]
              | Number Float
              | Percentage Float
              | S
              | Comment [Char]
              | Delim Char
    deriving (Show)

css_token :: CharParser st CssToken
css_token = (try ident)
    {-<|> atkeyword-}
    {-<|> string-}
    {-<|> invalid-}
    {-<|> hash-}
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

ident = do
    hyphen <- option "" (string "-")
    first <- char '_' <|> letter
    rest <- many (char '_' <|> letter <|> digit <|> char '-')
    return (Ident (hyphen ++ (first : rest)))

whitespace = many1 space >> return S

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

delim = do { c <- oneOf ",.{}():;"; return $ Delim c }

percentage = do
  num <- parse_number
  char '%'
  return $ Percentage num

number = do
  num <- parse_number
  return $ Number num

parse_number :: CharParser st Float
parse_number = do
    ipart <- many1 digit -- FIXME doesn't allow numbers of form .3
    -- TODO figure out how to make this grammatical without being a one-liner
    (try (do { char '.'; fpart <- many1 digit; return (read (ipart ++ "." ++ fpart) :: Float) })) <|> return (read ipart :: Float)
