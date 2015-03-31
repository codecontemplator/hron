module HRON where

import Text.Parsec(modifyState)
import Text.ParserCombinators.Parsec

type IndentParser a = GenParser Char Int a

data Preprocessor = Preprocessor String
data ValueLine = ContentLine String | CommentLine String | EmptyLine
data Member = Value String [ValueLine] | Object String [Member] | Comment String | Empty
data HRON = HRON [Preprocessor] [Member]

instance Show Preprocessor where
	show (Preprocessor s) = "Preprocessor:" ++ s

instance Show ValueLine where
	show (ContentLine s) = "ContentLine:" ++ s
	show (CommentLine s) = "CommentLine:" ++ s 
	show EmptyLine       = "EmptyLine:"

instance Show Member where
	show (Value tag value_lines) = 
		"Value_Begin:" ++ tag ++ "\n" ++
		unlines (map show value_lines) ++
		"Value_End:" ++ tag
	show (Object tag members) = 
		"Object_Begin" ++ tag ++ "\n" ++
		unlines (map show members) ++
		"Object_End:" ++ tag
	show (Comment s) = "Comment:" ++ s
	show Empty = "Empty"

instance Show HRON where
	show (HRON preprocessors members) =
		unlines (map show preprocessors) ++
		unlines (map show members)

--traceM :: Monad m => String -> m ()
--traceM msg = trace msg (return ())

void p = p >> (return ())

eol = void(oneOf "\n") -- <|> eof
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

preprocessor = do
	char '!'
	s <- hron_string
	return $ Preprocessor s

preprocessors = many preprocessor

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
	return $ ContentLine s

value_line = nonempty_line <|> comment_line <|> empty_line 

value_lines = many value_line

value = do
	indention
	char '='
	tag <- hron_string
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

hron = do
	pp <- preprocessors
	mbrs <- members
	return $ HRON pp mbrs

hron_parse input = runParser hron 0 "" input

input_text = unlines [
	"!abc",
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
