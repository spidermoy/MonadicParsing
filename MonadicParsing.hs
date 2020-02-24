import Control.Monad
import GHC.Base

{- A type for parsers -}

newtype Parser a = Parser (String -> [(a, String)])


{- A monad of parsers -}

item::Parser Char
item = Parser $ \cs -> case cs of
                         ""     -> []
                         (c:cs) -> [(c, cs)]


parse::Parser a->String->[(a, String)]
parse (Parser p) cs = p cs


instance Functor Parser where
  fmap f p = Parser $ \cs -> [(f a, cs') | (a, cs') <- parse p cs]

instance Applicative Parser where
  pure a = Parser $ \cs -> [(a, cs)]
  parser_f <*> parser_p = Parser $ \cs -> [(f a, cs'') | (f, cs')  <- parse parser_f cs,
                                                         (a, cs'') <- parse parser_p cs']

instance Monad Parser where
  parser_p >>= f = Parser $ \cs -> concat [parse (f a) cs' | (a, cs') <- parse parser_p cs]

{- The do notation -}

p_example::Parser (Char, Char)
p_example = do
  c <- item
  item
  d <- item
  return (c, d)

{- Choice combinators -}

instance Alternative Parser where
  empty = Parser $ \_ -> []
  p <|> q = Parser $ \cs -> parse p cs ++ parse q cs

instance MonadPlus Parser


(+++)::Parser a->Parser a->Parser a
p +++ q = Parser $ \cs -> case parse (mplus p q) cs of
                            []  -> []
                            x:_ -> [x]


sat_example::(Char->Bool)->Parser Char
sat_example p = do
  c <- item
  if   p c
  then return c
  else mzero


parse_char::Char->Parser Char
parse_char c = sat_example (==c)

{- Recursion combinators -}

parse_string::String->Parser String
parse_string s = case s of
                   ""   -> return ""
                   c:cs -> do
                    parse_char c
                    parse_string cs
                    return (c:cs)


parse_many::Parser a->Parser [a]
parse_many p = parse_many1 p +++ return []


parse_many1::Parser a->Parser [a]
parse_many1 p = do
  a  <- p
  as <- parse_many p
  return (a:as)


sepby::Parser a->Parser b->Parser [a]
sepby p sep = (p `sepby1` sep) +++ return []


sepby1::Parser a->Parser b->Parser [a]
sepby1 p sep = do
  a  <- p
  as <- many (sep >> p)
  return (a:as)


chainl::Parser a->Parser (a->a->a)->a->Parser a
chainl p op a = (p `chainl1` op) +++ return a


chainl1::Parser a->Parser (a->a->a)->Parser a
chainl1 p op = do
  a <- p
  rest a
  where
    rest a = (do
      f <- op
      b <- p
      rest (f a b)) +++ return a


{- Lexical combinators -}

space::Parser String
space = many $ sat_example (==' ')


token::Parser a->Parser a
token p = do
  a <- p
  space
  return a


symb::String->Parser String
symb = token . parse_string


apply::Parser a->String->[(a, String)]
apply p = parse (space >> p)


{- Example -}

expr::Parser Int
expr = term `chainl1` addop


term::Parser Int
term = factor `chainl1` mulop


factor::Parser Int
factor = digit +++ (do
                  symb "("
                  n <- expr
                  symb ")"
                  return n)


digit::Parser Int
digit = do
  x <- token (sat_example (\c -> elem c ['0' .. '9']))
  return $ case x of
      '0' -> 0
      '1' -> 1
      '2' -> 2
      '3' -> 3
      '4' -> 4
      '5' -> 5
      '6' -> 6
      '7' -> 7
      '8' -> 8
      '9' -> 9


addop::Parser (Int->Int->Int)
addop = (symb "+" >> return (+)) +++ (symb "-" >> return (-))


mulop::Parser (Int->Int->Int)
mulop = (symb "*" >> return (*)) +++ (symb "/" >> return (div))

