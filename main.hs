import Text.ParserCombinators.Parsec (parseTest)

import Grammar.AST

main = do
  contents <- getContents
  parseTest stylesheet contents
