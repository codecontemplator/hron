import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State

type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndent source_name $ runParserT aParser () source_name input

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
  case iParse aNamedList "indented_example" input_text of
    Left  err    -> print err
    Right result -> putStrLn $ "I parsed: " ++ show result

data NamedList = NamedList Name [Item]
  deriving (Show)

type Name = String
type Item = String

withBlock2 f a p = withPos $ do
    r1 <- a
    r2 <- block2 p
    return (f r1 r2)

block2 p = withPos $ do
    r <- many1 ((checkIndent >> p) <|> aComment)
    return r

aNamedList :: IParser NamedList
aNamedList = do
  b <- withBlock2 NamedList aName anItem
  spaces
  return b

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

