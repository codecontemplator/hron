-- http://stackoverflow.com/questions/6723208/trivial-parsec-example-produces-a-type-error
{-# LANGUAGE NoMonomorphismRestriction #-}

module HRON where

import Text.Parsec(modifyState)
import Text.ParserCombinators.Parsec
import Control.Monad(void)
--import Debug.Trace

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
		"Object_Begin:" ++ tag ++ "\n" ++
		unlines (map show members) ++
		"Object_End:" ++ tag
	show (Comment s) = "Comment:0," ++ s  -- todo: must count number of spaces
 	show Empty = "Empty"

instance Show HRON where
	show (HRON preprocessors members) =
		unlines (map show preprocessors) ++
		unlines (map show members)

--traceM :: Monad m => String -> m ()
--traceM msg = trace msg (return ())

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

eol = char '\n'

empty_string = many (oneOf " \t")

std_string = many $ noneOf "\n" 

comment_string = do
	empty_string
	char '#'
	s <- std_string
	return s

preprocessor = do
	char '!'
	s <- std_string
	eol
	return $ Preprocessor s

preprocessors = many preprocessor

empty_line = do
	empty_string
	eol
	return $ EmptyLine

comment_line = do
	s <- comment_string
	eol
	return $ CommentLine s

nonempty_line = do
	indention 
	s <- std_string
	eol
	return $ ContentLine s

value_line = 
	try(nonempty_line) <|> 
    try(comment_line)  <|> 
    try(empty_line)

value_lines = many value_line

value = do
	indention
	char '='
	tag <- std_string
	eol
	indent
	lines <- value_lines
	dedent
	return $ Value tag lines

empty = do
	empty_string
	eol
	return Empty

comment = do 
	cs <- comment_string
	eol
	return $ Comment cs

object = do
	indention
	char '@'
	tag <- std_string
	eol
	indent
	mbs <- members
	dedent
	return $ Object tag mbs

member =  try(value)   <|> 
		  try(object)  <|> 
		  try(comment) <|>
		  try(empty)	

members = many member

hron = do
	pp <- preprocessors
	mbrs <- members
	return $ HRON pp mbrs

hron_parse input = runParser hron 0 "" input

{-

input_text = unlines [
--	"!abc",
	"#comment @",
	"@x",
	"\t=y",
	"\t\t1010101",
    "=Welcome message",
    "\tHello there",
    "\tThis is hron speaking",
    "=Svamp",
    "\t10",
    "\t20",
    "@ObjDef",
    "\t=V1",
    "\t\t999",
    "=XX",
    "\tabcde"
  ]


input_text2 = unlines [
	"@Common",
	"\t=LogPath",
	"\t\tLogsCurrentDay",
	"\t=WelcomeMessage",
	"\t\tHello there!"
--	"",
--	"\t\tString values in hron is started with"
	]


input_text3 = unlines [
	"# object values are started with '@'",
	"@Common",
	"\t=LogPath",
	"\t\tLogs\\CurrentDay",
	"\t=WelcomeMessage",
	"\t\tHello there!",
	"",
	"\t\tString values in hron is started with '='",
	"",
	"\t\tJust as with Python in hron the indention is important",
	"",
	"\t\tIdention promotes readability but also allows hron string values ",
	"\t\tto be multi-line and hron has no special letters that requires escaping",
	"\t\t",
	"\t\tLetters like this causes hron no problems: &<>\\'@=",
	"",
	"\t\tThis helps readability",
	"",
	"",
	"@DataBaseConnection",
	"\t=Name",
	"\t\tCustomerDB",
	"\t=ConnectionString",
	"\t\tData Source=.\\SQLEXPRESS;Initial Catalog=Customers",
	"\t=TimeOut",
	"\t\t10",
	"\t@User",
	"\t\t=UserName",
	"\t\t\tATestUser",
	"\t\t=Password",
	"\t\t\t123",
	"@DataBaseConnection",
	"\t=Name",
	"\t\tPartnerDB",
	"\t=ConnectionString",
	"\t\tData Source=.\\SQLEXPRESS;Initial Catalog=Partners\t",
	""
	]
-}