{-| This model contains unit-tests for 'GLL.Combinators.BinInterface'

= Included examples

  * Elementary parsers
  * Sequencing
  * Alternatives
  * Simple binding
  * Binding with alternatives
  * Recursion (non-left)

  * Higher-order patterns:

      * Optional
      * Kleene-closure / positive closure
      * Seperator
      * Inline choice

  * Ambiguities:

      * "aaa"
      * longambig
      * aho_s
      * EEE

  * Left recursion
  * Hidden left-recursion
-}
module GLL.Combinators.Test.BinInterface where

import Prelude hiding ((<*>), (<*), (<$>), (<$), (*>))

import Control.Compose
import Control.Monad
import Data.Char (ord)
import Data.List (sort)
import Data.IORef
import qualified Data.Map as M

import GLL.Combinators.BinInterface

-- | Defines and executes some unit-tests 
main = do
    count <- newIORef 1
    let test name p arg_pairs = do
            i <- readIORef count
            modifyIORef count succ
            subcount <- newIORef 'a'
            putStrLn (">> testing " ++ show i ++ " (" ++ name ++ ")")
            forM_ arg_pairs $ \(str,res) -> do
                j <- readIORef subcount
                modifyIORef subcount succ
                let parse_res   = parseString p str
                    norm        = sort . take 100
                    b           = norm parse_res == norm res
                putStrLn ("  >> " ++ [j,')',' '] ++ show b)
                unless b (putStrLn ("    >> " ++ show parse_res))

    -- Elementary parsers
    test "eps1" (satisfy 0) [("", [0])]
    test "eps2" (0 <$ epsilon) [("", [0]), ("111", [])]
    test "single" (char 'a') [("a", ['a'])
                    ,("abc", [])]
    test "semfun1" (1 <$ char 'a') [("a", [1])]

    -- Elementary combinators
    test "<*>" ((\b -> ['1',b]) <$> char 'a' *> char 'b')
         [("ab", ["1b"])
         ,("b", [])]
   
    -- Alternation
    test "<|>" (ord <$> char 'a' *> char 'b' <|> ord <$> char 'c')
         [("a", []), ("ab", [98]), ("c", [99]), ("cab", [])]

    --  Simple binding
    let pX = "X" <:=> ord <$> char 'a' <* char 'b'
    test "<:=>" pX [("ab",[97]),("a",[])]

    --  Simple binding
    let pX = "X" <::=> ord <$> char 'a' <* char 'b'
    test "<::=>" pX [("ab",[97]),("a",[])]

    let  pX = "X" <::=> uncurry (flip (:)) <$> pY <*> char 'a'
         pY = "Y" <::=> uncurry (\x y -> [x,y]) <$> char 'b' <*> char 'c'
    test "<::=> 2" pX [("bca", ["abc"]), ("cba", [])]

    -- Binding with alternatives
    let pX = "X" <::=> pY <* char 'c'
        pY = "Y" <::=> char 'a' <|> char 'b'
    test "<::=> <|>" pX [("ac", "a"), ("bc", "b")]

    -- (Right) Recursion
    let pX = "X" <::=> (+1) <$> char 'a' *> pX <|> 0 <$ epsilon
    test "rec1" pX [("", [0]), ("aa",[2]), (replicate 42 'a', [42]), ("bbb", [])]

    -- EBNF
    let pX = "X" <::=> id <$> char 'a' *> char 'b' *> optional (char 'z')
    test "optional" pX [("abz", [Just 'z']), ("abab", []), ("ab", [Nothing])]

    let pX = "X" <::=> (char 'a' <|> char 'b')
    test "<|> optional" (pX <* optional (char 'z'))
                [("az", "a"), ("bz", "b"), ("z", []), ("b", "b"), ("a", "a")]

    let pX = "X" <::=> (1 <$ optional (char 'a') <|> 2 <$ optional (char 'b'))
    test "optional-ambig" (pX <* optional (char 'z'))
                [("az", [1]), ("bz", [2]), ("z", [1,2]), ("b", [2]), ("a", [1])]

    let pX = "X" <::=> id <$> char 'a' *> (char 'b' <|> char 'c')
    test "inline choice (1)" pX
                [("ab", "b"), ("ac", "c"), ("a", []), ("b", [])]

    let pX = "X" <::=> length <$> many (char '1')
    test "many" pX [("", [0]), ("11", [2]), (replicate 12 '1', [12])]

    let pX = "X" <::=> length <$> some (char '1')
    test "some" pX [("", []), ("11", [2]), (replicate 12 '1', [12])]

    let pX = "X" <::=> 1 <$ many (char 'a') <|> 2 <$ many (char 'b')
    test "(many <|> many) <*> optional" (pX <* optional (char 'z'))
                [("az", [1]), ("bz", [2]), ("z", [1,2])
                ,("", [1,2]), ("b", [2]), ("a", [1])]

    let pX = "X" <::=> pY <* optional (char 'z')
         where pY = "Y" <::=> length <$> many (char 'a')
                          <|> length <$> some (char 'b') <* char 'e'
    test "many & some & optional" 
        pX  [("aaaz", [3]), ("bbbez", [3]), ("ez", []), ("z", [0])
            ,("aa", [2]), ("bbe", [2]) 
            ]

    -- many with nullable argument
    let pX = const 1 <$> char '1' <|> satisfy 0
    test "many (nullable arg)" 
        (many pX) [("11", [[1,1]]), ("",[[]]), ("e", [])]

    -- Simple ambiguities
    let pX = uncurry (++) <$> pA <*> pB
        pA = "a" <$ char 'a' <|> "aa" <$ char 'a' <* char 'a'
        pB = "b" <$ char 'a' <|> "bb" <$ char 'a' <* char 'a' 
    test "aaa" pX   [("aaa", ["aab", "abb"])
                    ,("aa", ["ab"])]

    let pX = (\(x,y) -> [x,y]) <$> char 'a' *> pL <*> pL <* char 'e'
        pL =    1 <$ char 'b'
            <|> 2 <$ char 'b' <* char 'c'
            <|> 3 <$ char 'c' <* char 'd'
            <|> 4 <$ char 'd'
    test "longambig" pX [("abcde", [[1,3],[2,4]]), ("abcdd", [])]

    let pX = "X" <::=> (1 <$ some (char 'a') <|> 2 <$ many (char 'b'))
        pY = "Y" <::=> uncurry (+) <$> pX <*> pY
                   <|> satisfy 0
    test "some & many & recursion + ambiguities" pY
        [("ab", [3]),("aa", [1,2]), (replicate 10 'a', [1..10])]

    let pX = "X" <::=>  1 <$ char 'a' <|> satisfy 0
        pY = "Y" <::=> uncurry (+) <$> pX <*> pY
    -- shouldn't this be 1 + infinite 0's?
    test "no parse infinite rec?" pY 
        [("a", [])]

    let pS = "S" <::=> ((\(x,y) -> x+y+1) <$> char '1' *> pS <*> pS) <|> satisfy 0    
    test "aho_S" pS [("", [0]), ("1", [1]), (replicate 5 '1', [5])]


    let pS = "S" <::=> ((\(x,y) -> '1':x++y) <$> char '1' *> pS <*> pS) <|> satisfy "0"
    test "aho_S" pS [("", ["0"]), ("1", ["100"]), ("11", ["10100", "11000"])
                    ,(replicate 5 '1', aho_S_5)]

    let pE = "E" <::=> (\((x,y),z) -> x+y+z) <$> pE <*> pE <*> pE 
                             <|> 1 <$ char '1'
                             <|> satisfy 0
    test "EEE" pE [("", [0]), ("1", [1]), ("11", [2])
                  ,(replicate 5 '1', [5]), ("112", [])]

    let pE = "E" <::=> (\((x,y),z) -> x++y++z) <$> pE <*> pE <*> pE 
                             <|> "1" <$ char '1'
                             <|> satisfy "0"
    test "EEE ambig" pE [("", ["0"]), ("1", ["1"])
                        ,("11", ["110", "011", "101"]), ("111", _EEE_3)]

    let pX = "X" <::=>  maybe 0 (const 1) <$> optional (char 'z') 
                    <|> (+1) <$> pX <* char '1'
    test "simple left-recursion" pX [("", [0]), ("z11", [3]), ("z", [1])
                                    ,(replicate 100 '1', [100])]

    let pX = "X" <::=> satisfy 0 
                    <|> (+1) <$> pB *> pX <* char '1'
        pB = maybe 0 (const 0) <$> optional (char 'z')
    test "hidden left-recursion" pX 
        [("", [0]), ("zz11", [2]), ("z11", [2]), ("11", [2])
        ,(replicate 100 '1', [100])]

    let pX = "X" <::=> uncurry (+) <$> pY <*> pA
        pA = 1 <$ char 'a' <* char 'b' <|> satisfy 0
        pY = "Y" <::=> satisfy 0 <|> pX 
    test "hidden left-recursion + infinite derivations" pX
        [("", [0]), ("ab", [1]), ("ababab", [3])]
 where
    aho_S_5 = ["10101010100","10101011000","10101100100","10101101000","10101110000","10110010100","10110011000","10110100100","10110101000","10110110000","10111000100","10111001000","10111010000","10111100000","11001010100","11001011000","11001100100","11001101000","11001110000","11010010100","11010011000","11010100100","11010101000","11010110000","11011000100","11011001000","11011010000","11011100000","11100010100","11100011000","11100100100","11100101000","11100110000","11101000100","11101001000","11101010000","11101100000","11110000100","11110001000","11110010000","11110100000","11111000000"]

    _EEE_3 = ["00111","01011","01101","01110","10011","10101","10110","11001","11010","111","11100"]
