module HRON where

import Text.Parsec(modifyState)
--import Text.Parsec.Pos
--import Text.Parsec.Indent
import Text.ParserCombinators.Parsec
import Control.Monad (replicateM)

{-
data Preprocessor = Preprocessor String
data ValueLine = NonEmptyLine String | CommentLine String | EmptyLine 
data Member = Value String [ValueLine] | Object String [Member] | Comment String | Empty
data SyntaxTree = SyntaxTree [Preprocessor] [Member]
-}

{-
members = many member
member =  value -- <|> object <|> comment <|> empty	
value = withBlock Value (char '=' >> tag) valueLine
	where tag = many (noneOf "\n")
valueLine = do { return (NonEmptyLine "daniel") }
-}

data NamedList = NamedList Name [Item] deriving (Show)
type Name = String
data Item = StringItem String | Comment String deriving (Show)

type IndentParser a = GenParser Char Int a

--parse :: String -> Either ParseError a
parse input = runParser (aNamedList) 0 "" input

manyx :: Int -> IndentParser a -> IndentParser [a]
manyx n p = replicateM n p

--eol :: Parser ()
--eol = void (char '\n') <|> eof
eol = char '\n'

indent :: IndentParser ()
indent = do
 			modifyState (1 +)
 --			i <- getState
--			setState (i+1)
			return ()

indention :: IndentParser ()
indention = do
				i <- getState
				manyx i (char '\t')
				return ()

aNamedList :: IndentParser NamedList
aNamedList = do
  name <- aName
  indent
  items <- many1 (anItem <|> aComment)   
  return (NamedList name items)

aComment :: IndentParser Item
aComment = do
	spaces
	char '#'
	x <- many1 alphaNum
	eol
	return (Comment x)

aName :: IndentParser Name
aName = do
  s <- many1 alphaNum
  _ <- char ':'
  eol
  return s

anItem :: IndentParser Item
anItem = do
  indention
  i <- many1 alphaNum
  eol
  return (StringItem i)

input_text :: String
input_text = unlines [
    "listName:",
    "\titem1",
    "\titem2",
    "#comment",
    "\titem3"
  ]
