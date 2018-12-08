import Text.ParserCombinators.Parsec as P
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok
import Data.Maybe
import SExp


lexer = Tok.makeTokenParser emptyDef

alpha = noneOf "() \n\t"

identifier :: Parser String
identifier = many1 alpha
  

lexS :: Parser [String]
lexS = many $ (spaces >>) $
  string "(" <|>
  string ")" <|>
  identifier

fact = ["(","define","fact","(","+","n","1",")",")"]
  
parseS :: GenParser String st (SExp String)
parseS = do
  tok <- anyToken
  case tok of
   "(" -> fmap SList (many parseS)
   ")" -> pzero
   ident -> return $ Atom ident 

{-
parseS :: Parser (SExp String)
parseS = spaces >> (
  (fmap SList $ char '(' *> many parseS <* char ')') <|> 
  (fmap Atom identifier))
-}


