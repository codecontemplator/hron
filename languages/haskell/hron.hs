module HRON where

import Text.Parsec(modifyState)
--import Text.Parsec.Pos
--import Text.Parsec.Indent
import Text.ParserCombinators.Parsec
import Control.Monad (replicateM)
import Debug.Trace

type IndentParser a = GenParser Char Int a

data Preprocessor = Preprocessor String deriving(Show)
data ValueLine = NonEmptyLine String | CommentLine String | EmptyLine  deriving(Show)
data Member = Value String [ValueLine] | Object String [Member] | Comment String | Empty deriving(Show)
data SyntaxTree = SyntaxTree [Preprocessor] [Member] deriving(Show)

traceM :: Monad m => String -> m ()
traceM msg = trace msg (return ())

void p = p >> (return ())

eol = void(oneOf "\n") --	<|> eof
any_indention = void(manyTill space eol)  -- fix this

indent = do
	modifyState (\i -> i + 1)
	return ()

dedent = do
	modifyState (\i -> i - 1)
	return ()

indention = do
	i <- getState
	count i (char '\t')
	return ()

empty_string = void $ manyTill space (try eol) 

hron_string = manyTill anyChar (try eol)

comment_string = do
	any_indention
	char '#'
	s <- hron_string
	return s

empty_line = do
	empty_string 
--	eol
	return $ EmptyLine

comment_line = do
	s <- comment_string
--	eol
	return $ CommentLine s

nonempty_line = do
	indention 
	s <- hron_string
--	eol
	return $ NonEmptyLine s

value_line = nonempty_line <|> comment_line <|> empty_line 

value_lines = many value_line

value = do
	indention
	char '='
	tag <- hron_string
	traceM $ "tag: " ++ show tag
	--eol
	indent
	lines <- value_lines
	dedent
	return $ Value tag lines

empty = do
	empty_string
--	eol
	return Empty

comment = do 
	cs <- comment_string 
--	eol
	return $ Comment cs

object = do
	indention
	char '@'
	tag <- hron_string
--	eol
	indent
	mbs <- members
	dedent
	return $ Object tag mbs

member =  value <|> object <|> comment <|> empty	
members = many member

hron_parse input = runParser members 0 "" input

input_text = unlines [
    "=Welcome message",
    "\tHello there",
    "\tThis is hron speaking",
    "=Svamp",
    "\t10",
    "\t20",
    "@ObjDef",
    "\t=V1",
    "\t\t999"
  ]
