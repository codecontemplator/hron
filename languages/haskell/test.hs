import Test.HUnit
import HRON
import System.IO  
import Control.Monad

fromRight           :: Either a b -> b
fromRight (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'" -- yuck
fromRight (Right x) = x


mkTest id = TestCase $
	do
		hronFile <- openFile ("../../reference-data/" ++ id ++ ".hron") ReadMode
		hSetEncoding hronFile utf8_bom
		hron <- hGetContents hronFile
		
		hronActionLogFile <- openFile ("../../reference-data/" ++ id ++ ".hron.actionlog") ReadMode
		hSetEncoding hronActionLogFile utf8_bom
		hronActionLog <- hGetContents hronActionLogFile

		let tree = hron_parse hron
--		case tree of 
--			Left x -> putStrLn $ "failed " ++ show x
--		  	_ -> putStrLn ""
		let log = show $ fromRight tree 

		logFile <- openFile ("test-" ++ id ++ ".hron.actionlog") WriteMode
		hSetEncoding logFile utf8_bom
		hPutStr logFile log

		assertEqual "action logs should be equal" hronActionLog log

		hClose hronFile
		hClose hronActionLogFile
		hClose logFile


tests = TestList [
	TestLabel "hello world" (mkTest "helloworld"),
	TestLabel "simple" (mkTest "simple"),
	TestLabel "random" (mkTest "random"),
	TestLabel "large" (mkTest "large")
	]

rt = runTestTT tests