-- | base on johnson1995
module GLL.Combinators.CPS where

import              Prelude         hiding ((<*>), (<*), (<$>), (<$))

import              Control.Monad
import              Data.IORef
import qualified    Data.IntMap     as IM

type Parser a = String -> Int -> Int -> (Int -> a -> IO ()) -> IO ()
type MemTable a = IM.IntMap ([(Int,a)], [Int -> a -> IO ()])

test :: (Show a) => Parser a -> String -> IO [a]
test p str = do
    tab <- newTable
    parse (tab <::=> p) str

parse :: Parser a -> String -> IO [a]
parse p str = do
    results <- newIORef []
    let filter_insert i a | i == m    = modifyIORef results (a:)
                          | otherwise = return ()
    p str m 0 filter_insert 
    readIORef results
 where  m = length str 

infix 3 <::=> 
(<::=>) :: (Show a) => IORef (MemTable a) -> Parser a -> Parser a
(ref <::=> p) str m i cont = do
    table <- readIORef ref
    case IM.lookup i table of       
        Nothing     -> do                           -- if first time here 
            modifyIORef ref (IM.insert i ([], []))  -- make the table entry
            modifyIORef ref (c_insert cont)   -- and insert given continuation
            let p_cont k a = do  -- whenever a result is discovered
                modifyIORef ref (a_insert (k,a)) -- insert it in the table-entry
                table <- readIORef ref 
                let (as,cs) = table IM.! i
                forM_ cs (flip ($) (k,a) . uncurry) -- and apply all conts to it
            p str m i p_cont -- and execute the parser with p_cont
        Just (as,_) -> do   modifyIORef ref (c_insert cont) 
                            forM_ as (uncurry cont) -- if not the first time, 
                                     -- post all the discovered values
 where  c_insert c = IM.adjust (ins c) i
         where ins c (as,cs) = (as, c:cs)
        a_insert a = IM.adjust (ins a) i
         where ins a (as,cs) = (a:as, cs)

newTable :: IO (IORef (MemTable a))
newTable = newIORef IM.empty

satisfy :: a -> Parser a 
satisfy a _ _ i cont = cont i a

char :: Char -> Parser Char
char c str m i cont | i < m && str !! i == c  = cont (i + 1) c
                    | otherwise               = return ()

infixl 4 <|>
infixl 4 <+>
(<|>) :: Parser a -> Parser a -> Parser a
(l <|> r) str m i cont = do
    l str m i cont
    r str m i cont
(<+>) = (<|>)

infixl 5 <*>
(<*>) :: Parser (b -> a) -> Parser b -> Parser a
(l <*> r) str m i lhs_cont = do 
    l str m i l_cont 
 where  l_cont k b2a = r str m k r_cont 
         where r_cont r b = lhs_cont r (b2a b)

-- | derived
infixl 5 <$>
(<$>) :: (b -> a) -> Parser b -> Parser a
f <$> p = satisfy f <*> p

infixl 5 <$
(<$) :: b -> Parser a -> Parser b
f <$  p = const f <$> p 

infixl 5 <*
(<*) :: Parser b -> Parser a -> Parser b
l <* r = const <$> l <*> r

-- | example1
pX :: Parser Int 
pX = (+1) <$ char 'a' <* char 'b' <*> pX <|> satisfy 0

pY_r = (+1) <$ char 'y' <*> pY_r <|> satisfy 0

testL str = do
    tab <- newTable 
    let pY_l = tab <::=> (+1) <$> pY_l <* char 'y' <|> satisfy 0
    parse pY_l str

testE str = do
    tab <- newTable
    let pE = tab <::=>  1 <$ char '1'
                    <|> (\x -> x) <$> pE
    parse pE str
