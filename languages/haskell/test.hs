import Test.HUnit
import HRON
import System.IO  
import Control.Monad

fromRight           :: Either a b -> b
fromRight (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'" -- yuck
fromRight (Right x) = x

testHelloWorld = TestCase $
	do
		hronFile <- openFile "../../reference-data/helloworld.hron" ReadMode
		hSetEncoding hronFile utf8_bom
		hron <- hGetContents hronFile
		putStrLn hron
		hronActionLogFile <- openFile "../../reference-data/helloworld.hron.actionlog" ReadMode
		hSetEncoding hronActionLogFile utf8_bom
		hronActionLog <- hGetContents hronActionLogFile
		let tree = hron_parse (hron ++ "\n") -- todo: handle missing newline at end of file properly in parser
		case tree of 
		  Left x -> putStrLn $ "failed " ++ show x
		  Right _ -> putStrLn "ok" 
		let log = show $ fromRight tree 
		putStrLn "------------------------"
		putStrLn log
		putStrLn "------------------------"
		assertEqual "action logs should be equal" hronActionLog log

tests = TestList [
	TestLabel "hello world" testHelloWorld
	]

rt = runTestTT tests