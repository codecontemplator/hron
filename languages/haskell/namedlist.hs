import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State

type IParser a = ParsecT String Int (State SourcePos) a

iParse :: IParser a -> Int -> String -> Either ParseError a
iParse aParser i input =
  runIndent "" $ runParserT aParser () "" input

input_text :: String
input_text = unlines [
    "listName:",
    "  item1",
    "  item2",
    "#comment",
    "  item3"
  ]

main :: IO ()
main = do
  case iParse aNamedList 0 input_text of
    Left  err    -> print err
    Right result -> putStrLn $ "I parsed: " ++ show result

data NamedList = NamedList Name [Item]
  deriving (Show)

type Name = String
type Item = String

indent p = do
	i <- get 
	r <- put (i+1) >> p
	return r

aNamedList :: IParser NamedList
aNamedList = do
  name <- aName
  items <- indent $ many1 ((checkIndent >> anItem) <|> aComment) 
  --b <- withBlock2 NamedList aName anItem
  spaces
  return (NamedList name items)

aComment :: IParser String
aComment = do
	spaces
	char '#'
	x <- many1 alphaNum
	spaces
	return x

aName :: IParser Name
aName = do
  s <- many1 alphaNum
  _ <- char ':'
  spaces
  return s

anItem :: IParser Item
anItem = do
  i <- many1 alphaNum
  spaces
  return i

