{-
Here you can find some unit tests.
Also I've implemented some property tests ^-^.
-}

import Test.Hspec

import HW5.Launcher
import HW5.Action

import Test.QuickCheck

import qualified Test.QuickCheck.Monadic as QM

import System.Directory         (removeDirectoryRecursive)
import Data.Functor             (($>))
import Control.Monad.Cont       (lift)
import System.Console.Haskeline (InputT, defaultSettings, runInputT)

-- | Entry point to all tests.
main :: IO ()
main = hspec $ do
    describe "simple tests" $ do
        describe "basic operations" $ do
            it "test 1" $ testCase "100" "100"
            it "test 2" $ testCase "-15" "-15"
            it "test 3" $ testCase "add(100, -15)" "85"
            it "test 4" $ testCase "add(3, div(14, 100))" "3.14"
            it "test 5" $ testCase "div(10, 3)" "3 + 1/3"
            it "test 6" $ testCase "sub(mul(201, 11), 0.33)" "2210.67"
            it "test 7" $ testCase "mul(2, 10)" "20"
            it "test 8" $ testCase "sub(1000, 7)" "993"
            it "test 9" $ testCase "div(3, 5)" "0.6"
        
        describe "error cases" $ do
            it "test 2" $ testCase "div(100,0)" "HiErrorDivideByZero"
            it "test 3" $ testCase "div(100)" "HiErrorArityMismatch"
            it "test 4" $ testCase "15(3)" "HiErrorInvalidFunction"
            it "test 5" $ testCase "sub(100, add)" "HiErrorInvalidArgument"
    
    describe "tests with booleans and conditions" $ do
        describe "basic operations" $ do
            it "test 1" $ testCase "false" "false"
            it "test 2" $ testCase "equals(add(2, 2), 4)" "true"
            it "test 3" $ testCase "less-than(mul(999, 99), 10000)" "false"
            it "test 4" $ testCase "if(greater-than(div(2, 5), div(3, 7)), 1, -1)" "-1"
            it "test 5" $ testCase "and(less-than(0, 1), less-than(1, 0))" "false"
            it "test 6" $ testCase "if(true, add, mul)" "add"
            it "test 7" $ testCase "if(true, add, mul)(10, 10)" "20"
            it "test 8" $ testCase "if(false, add, mul)(10, 10)" "100"
            it "test 9" $ testCase "equals(add, add)" "true"
            it "test 10" $ testCase "equals(add, mul)" "false"
            it "test 11" $ testCase "not (2==2)" "false"
            it "test 12" $ testCase "1>=2" "false"
        
        describe "test operators" $ do
            it "test 1" $ testCase "2 + 2" "4"
            it "test 2" $ testCase "2 + 2 * 3" "8"
            it "test 3" $ testCase "(2 + 2) * 3" "12"
            it "test 4" $ testCase "2 + 2 * 3 == (2 + 2) * 3" "false"
            it "test 5" $ testCase "10 == 2*5 && 143 == 11*13" "true"
            it "test 6" $ testCase "10 /= 11" "true"
            it "test 7" $ testCase "10 /= 10" "false"
            it "test 8" $ testCase "10 < 11" "true"
    
    describe "tests for strings and slices" $ do
        describe "basic operations" $ do
            it "test 1" $ testCase "length(\"Hello World\")" "11"
            it "test 2" $ testCase "to-upper(\"Hello World\")" "\"HELLO WORLD\""
            it "test 3" $ testCase "to-lower(\"Hello World\")" "\"hello world\""
            it "test 4" $ testCase "reverse(\"stressed\")" "\"desserts\""
            it "test 5" $ testCase "trim(\" Hello World \")" "\"Hello World\""
            it "test 6" $ testCase "\"Hello\" + \"World\"" "\"HelloWorld\""
            it "test 7" $ testCase "\"Cat\" * 5" "\"CatCatCatCatCat\""
            it "test 8" $ testCase "\"/home/user\" / \"hi\"" "\"/home/user/hi\""
            it "test 9" $ testCase "\"Hello World\"(0)" "\"H\""
            it "test 10" $ testCase "\"Hello World\"(7)" "\"o\""
            it "test 11" $ testCase "\"Hello World\"(-1)" "null"
            it "test 12" $ testCase "\"Hello World\"(99)" "null"
            it "test 13" $ testCase "\"Hello World\"(0, 5)" "\"Hello\""
            it "test 14" $ testCase "\"Hello World\"(0, -4)" "\"Hello W\""
            it "test 15" $ testCase "\"Hello World\"(-4, -1)" "\"orl\""
            it "test 16" $ testCase "to-upper(\"what a nice language\")(7, 11)" "\"NICE\""
            it "test 17" $ testCase "length(\"hehe\" * 5) / 3" "6 + 2/3"
            it "test 18" $ testCase "\"Hello, World\"(null, 5)" "\"Hello\""
    
    describe "lists and folds" $ do
        describe "basic operations" $ do
            it "test 1" $ testCase "list(1, 2, 3, 4, 5)" "[ 1, 2, 3, 4, 5 ]"
            it "test 2" $ testCase "fold(add, [2, 5] * 3)" "21"
            it "test 3" $ testCase "fold(mul, range(1, 10))" "3628800"
            it "test 4" $ testCase "[0, true, false, \"hello\", \"world\"](2, 4)" "[ false, \"hello\" ]"
            it "test 5" $ testCase "reverse(range(0.5, 70/8))" "[ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]"
            it "test 6" $ testCase "fold(div, [11, 22, 33])" "1/66"
            it "test 7" $ testCase "[1, 2] + [3, 4, 5]" "[ 1, 2, 3, 4, 5 ]"
            it "test 8" $ testCase "[0, \"x\"] * 3" "[ 0, \"x\", 0, \"x\", 0, \"x\" ]"
    
    describe "bytes" $ do
        describe "basic operations" $ do
            it "test 1" $ testCase "pack-bytes(range(30, 40))" "[# 1e 1f 20 21 22 23 24 25 26 27 28 #]"
            it "test 2" $ testCase "decode-utf8([# 68 69 #] * 5)" "\"hihihihihi\""
            it "test 3" $ testCase "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" "[# 01 02 03 #]"
            it "test 4" $ testCase "[# 00 ff 01 e3 #](1,3)" "[# ff 01 #]"
            it "test 5" $ testCase "[# 00 ff 01 e3 #](1)" "255"
            it "test 6" $ testCase "[# 00 ff #] + [# 01 e3 #]" "[# 00 ff 01 e3 #]"
            it "test 7" $ testCase "unpack-bytes([# 10 20 30 #])" "[ 16, 32, 48 ]"
            it "test 8" $ testCase "decode-utf8([# c3 28 #])" "null"
    
    describe "time" $ do
        describe "basic operations" $ do
            it "test 1" $ testCase "parse-time(\"2021-12-15 00:37:51.000890793 UTC\") - parse-time(\"2021-12-15 00:37:47.649047038 UTC\")" "3.351843755"
            it "test 2" $ testCase "parse-time(\"2021-01-01 00:00:00 UTC\") + 365 * 24 * 60 * 60" "parse-time(\"2022-01-01 00:00:00 UTC\")"
    
    describe "echo" $ do
        describe "basic operations" $ do
            it "test 1" $ testCase "echo" "echo"
            it "test 2" $ testCase "\"Hello\"(0) || \"Z\"" "\"H\""
            it "test 3" $ testCase "true || echo(\"Don't do this\")!" "true"
            it "test 4" $ testCase "false && echo(\"Don't do this\")!" "false"
    
    describe "dictionaries" $ do
        describe "basic operations" $ do
            it "test 1" $ testCase "count(\"Hello World\").o" "2"
            it "test 2" $ testCase "fold(add, values(count(\"Hello, World!\")))" "13"
            it "test 3" $ testCase "invert(count(\"big blue bag\"))" "{ 1: [ \"u\", \"l\", \"i\", \"e\", \"a\" ], 2: [ \"g\", \" \" ], 3: [ \"b\" ] }"
            it "test 4" $ testCase "{ \"width\": 120, \"height\": 80 }(\"width\")" "120"
            it "test 5" $ testCase "keys({ \"width\": 120, \"height\": 80 })" "[ \"height\", \"width\" ]"
            it "test 6" $ testCase "values({ \"width\": 120, \"height\": 80 })" "[ 80, 120 ]"
            it "test 7" $ testCase "count([# 58 58 58 4f 58 #])" "{ 79: 1, 88: 4 }"
            it "test 8" $ testCase "{ \"width\": 120, \"height\": 80 }.width" "120"

    describe "FAQ" $ do
        it "test 1" $ testCase "[# 00 ff #](1)" "255"
        it "test 2" $ testCase "if(true, 1, 1/0)" "1"
        it "test 3" $ testCase "length.hello-world" "11"
        it "test 4" $ testCase "\"abc\"(1, 2.2)" "HiErrorInvalidArgument"
        it "test 5" $ testCase "\"abcd\"(3, -3)" "\"\""
        it "test 6" $ testCase "[1,2,3](0)" "1"
        it "test 7" $ testCase "[# 00 ff #](1)" "255"
        it "test 8" $ testCase "or(true, 3)" "true"
        it "test 9" $ testCase "if(true, { \"width\" : 1 }, 1+1).width" "1"
        it "test 10" $ testCase "(add)(1,2)" "3"
        it "test 11" $ testCase "((div(1))(10))" "HiErrorArityMismatch"
        it "test 12" $ testCase "[1 + 2, 3 + 4]" "[ 3, 7 ]"
        it "test 13" $ testCase "reverse(\"to-upper\")(\"hello\")" "HiErrorInvalidArgument"
        it "test 14" $ testCase "{ \"A\" : 1 }.B" "null"
        it "test 15" $ testCase "serialise(1/0)" "HiErrorDivideByZero"
        it "test 16" $ testCase "fold(not,[true])" "HiErrorInvalidArgument"
    
    describe "no permission" $ do
        it "test 1" $ testCustomRun "now!" [AllowRead] `shouldThrow` anyException
        it "test 2" $ testCustomRun "read(\"tmp\")!" [AllowTime] `shouldThrow` anyException
        it "test 3" $ testCustomRun "write(\"tmp/hi.txt\", \"Hello\")!" [AllowRead] `shouldThrow` anyException

    describe "some more advanced cases" $ do
        it "test 1" $ testCase "div(add(mul(2, 5), 1), sub(11,6))" "2.2"
        it "test 2" $ testCase "+10" "10"
        it "test 3" $ testCase "div(1, sub(5, 5))" "HiErrorDivideByZero"
        it "test 4" $ testCase "sub(10, add)" "HiErrorInvalidArgument"
        it "test 5" $ testCase "[\"hello\", true, \"world\"](1,3)" "[ true, \"world\" ]"
        it "test 6" $ testCase "fold(div, [11, 22, 33])" "1/66"
        it "test 7" $ testCase "range(5, 10.3)" "[ 5, 6, 7, 8, 9, 10 ]"
        it "test 8" $ testCase "decode-utf8([# 48 65 6c 6c 6f #])" "\"Hello\""
        it "test 9" $ testCase "rand(1,1)!" "1"
        it "test 10" $ testCase "pack-bytes([100, 300])" "HiErrorInvalidArgument"
        it "test 11" $ testCase "reverse.hello" "\"olleh\""
        it "test 12" $ testCase "reverse.to-upper.hello" "HiErrorInvalidArgument"
        it "test 13" $ testCase "write(\"hi.txt\", \"Hi!\")" "write(\"hi.txt\", [# 48 69 21 #])"

    describe "complex cases" $ do
        it "test 1" $ runInputT defaultSettings testComplexCase

    describe "some property tests" $ do
        it "test 1" $ forAll genSafeString $ \gen -> runTestProp gen (\s -> "decode-utf8(unzip(zip(encode-utf8(\"" <> s <> "\"))))") (\s -> "\"" <> s <> "\"")
        it "test 2" $ forAll genSafeString $ \gen -> runTestProp gen (\s -> "deserialise(serialise(\"" <> s <> "\"))") (\s -> "\"" <> s <> "\"")
        it "test 3" $ testCase "unpack-bytes(pack-bytes([1,2,3]))" "[ 1, 2, 3 ]"
        it "test 4" $ testCase "read(\"app\")!" "[ \"Main.hs\" ]"
        it "test 5" $ forAll genSafeString $ \gen -> runTestProp gen (\s -> "reverse(reverse(\"" <> s <> "\"))") (\s -> "\"" <> s <> "\"")
    
    describe "dot property tests" $ do
        it "test 1" $ testCustomRun "{\"a\":\"b\"}." [] `shouldThrow` anyException
        it "test 2" $ testCase "(({\"a\":{\"b\":\"c\"}}).a).b" "\"c\""
        it "test 3" $ testCase "{\"a\":add}.a(1,2)" "3"
        it "test 4" $ testCase "{\"a\":rand}.a(1,1)!" "1"
        it "test 5" $ testCase "{\"a\":\"b\"}({\"b\":\"a\"}.b)" "\"b\""
        it "test 6" $ testCase "{\"a-a-b\":\"c\"}.a-a-b" "\"c\""
        it "test 7" $ testCase "{\"olleh\":\"239\"}((reverse).hello)" "\"239\""

-- | Function for running parser+eval and checking result.
test :: String -> String -> [HiPermission] -> InputT IO ()
test input expected perms = do
    result <- run input perms
    lift $ result `shouldBe` expected

-- | Returns result of parser+eval. (similar to test)
testProp :: String -> [HiPermission] -> InputT IO String
testProp input perms = do
    result <- run input perms
    lift $ pure result

-- | Runs some property test.
runTestProp :: String -> (String -> String) -> (String -> String) -> Property
runTestProp s f g = QM.monadicIO $ do
    res <- QM.run $ runInputT defaultSettings (testProp (f s) allPerms)
    QM.assert $ res == g s

-- | Generator for chars.
genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

-- | Generator for strings from small english letters.
genSafeString :: Gen String
genSafeString = listOf genSafeChar

-- | Basic test case.
testCase :: String -> String -> IO ()
testCase input expected = runInputT defaultSettings (test input expected allPerms)

-- | Run for checking that some case throws exception.
testCustomRun :: String -> [HiPermission] -> IO ()
testCustomRun input perms = runInputT defaultSettings (test input "error" perms)

-- | Just test case for read/write/cwd/cd operations.
testComplexCase :: InputT IO ()
testComplexCase = do
    run "mkdir(\"tmp\")!" allPerms $> ()
    run "read(\"tmp\")!" allPerms $> ()
    run "mkdir(\"tmp/a\")!" allPerms $> ()
    run "mkdir(\"tmp/b\")!" allPerms $> ()
    run "read(\"tmp\")!" allPerms >>= \res -> lift $ res `shouldBe` "[ \"b\", \"a\" ]" -- may differ depending on implementation
    run "write(\"tmp/hi.txt\", \"Hello\")!" allPerms $> ()
    run "cd(\"tmp\")!" allPerms $> ()
    run "read(\"hi.txt\")!" allPerms >>= \res -> lift $ res `shouldBe` "\"Hello\""
    run "cd(\"..\")!" allPerms $> ()
    lift $ removeDirectoryRecursive "tmp" -- clean up directory
