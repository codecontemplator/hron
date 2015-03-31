module HRON where

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Indent

data Preprocessor = Preprocessor String
data ValueLine = NonEmptyLine String | CommentLine String | EmptyLine 
data Member = Value String [ValueLine] | Object String [Member] | Comment String | Empty
data SyntaxTree = SyntaxTree [Preprocessor] [Member]

type IParser a = ParsecT String () (State SourcePos) a

parse :: string -> Either ParseError a
parse input = runIndent "" $ runParserT members () "" input

members = many member
member =  value -- <|> object <|> comment <|> empty	
value = withBlock Value (char '=' >> tag) valueLine
	where tag = many (noneOf "\n")
valueLine = do { return (NonEmptyLine "daniel") }

