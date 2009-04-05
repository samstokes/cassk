import Text.ParserCombinators.Parsec (CharParser, many, parse, eof)

import Grammar.Token

css_tokens :: CharParser st [CssToken]
css_tokens = do
  them <- many css_token
  eof
  return them

main = interact $ debug . (parse css_tokens "")
    where debug (Left error) = show error
          debug (Right tokens) = unlines $ map show tokens
