module HRON where

import Text.Parsec(modifyState)
import Text.ParserCombinators.Parsec
import Debug.Trace
import Control.Monad(void)

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

traceM :: Monad m => String -> m ()
traceM msg = trace msg (return ())

--void p = p >> (return ())

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
	try $ spaces >> char '#'  -- todo: spaces might consume an entire row
	s <- hron_string
	return s

preprocessor = do
	char '!'
	s <- hron_string
	return $ Preprocessor s

preprocessors = many preprocessor

empty_line = do
	empty_string 
	return $ EmptyLine

comment_line = do
	s <- comment_string
	return $ CommentLine s

nonempty_line = do
	indention 
	s <- hron_string
	return $ ContentLine s

value_line = nonempty_line <|> comment_line <|> empty_line 

value_lines = many (try value_line)

value = do
	try $ indention >> char '='	
	tag <- hron_string
	indent
	lines <- value_lines
	dedent
	return $ Value tag lines

empty = do
	empty_string
	return Empty

comment = do 
	cs <- comment_string 
	return $ Comment cs

object = do
	try $ indention >> char '@'
	tag <- hron_string
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

{-
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

input_text2 = unlines [
	"# object values are started with @",
	"@Common",
	"\t=LogPath",
	"\t\tLogsCurrentDay",
	"\t=WelcomeMessage",
	"\t\tHello there!",
	"",
	"\t\tString values in hron is started with"
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