
-- | Parser Combinators for GLL parsing inspired by Tom Ridge's P3 OCaml library
module GLL.Combinators.BinInterface (
    Parser,
    parse, parseString,
    char, token, Token(..),
    epsilon, satisfy,
    many,some,optional,
    (<::=>),(<:=>),
    (<$>),
    (<$),
    (<*>),
    (*>),
    (<*),
    (<|>),
) where

import Prelude hiding ((<*>), (<*), (<$>), (<$), (*>))

import GLL.Combinators.Options
import GLL.Types.Abstract
import GLL.Types.Derivations
import GLL.Parser (gllSPPF,ParseResult(..))

import qualified    Data.Array  as A
import qualified    Data.IntMap as IM
import qualified    Data.Map    as M
import qualified    Data.Set    as S

type Visit1     = Symbol 
type Visit2     = M.Map Nt [Alt] -> M.Map Nt [Alt]
type Visit3 a   = PCOptions -> A.Array Int Token -> ParseContext -> SPPF 
                    -> Int -> Int -> Int -> S.Set a

type Parser a   = (Visit1, Visit2, Visit3 a)

type ParseContext = IM.IntMap (IM.IntMap Nt)

-- | Given a parser and a string of tokens, return:
--  * The grammar (GLL.Types.Abstract)
--  * a list of results, which are all semantic evaluations of 'good derivations'
--      - semantic evaluations are specified by using <$> and satisfy
--      - 'good derivations' as defined by by Tom Ridge
parse' :: PCOptions -> Parser a -> [Token] -> (Grammar, ParseResult, [a])
parse' opts (Nt start,rules,sem) str = 
    let cfg     = Grammar start [] [ Rule x alts  
                                   | (x, alts) <- M.assocs (rules M.empty) ]
        parse_r = gllSPPF cfg str
        sppf    = sppf_result parse_r
        as      = sem opts arr IM.empty sppf 0 m m
        m       = length str
        arr     = A.array (0,m) (zip [0..] str)
    in (cfg,parse_r,S.toList as)

-- | The grammar of a given parser
grammar :: Parser a -> Grammar
grammar p = (\(f,_,_) -> f) (parse' defaultOptions p [])

-- | The semantic results of a parser, given a token string
parse :: Parser a -> [Token] -> [a]
parse = parseWithOptions defaultOptions 

-- | The semantic results of a parser, given a token string 
--      and GLL.Combinator.Options
parseWithOptions :: PCOptions -> Parser a -> [Token] -> [a]
parseWithOptions opts p str = (\(_,_,t) -> t) (parse' opts  p str)

-- | Get the SPPF produced by parsing the given input with the given parser
sppf :: Parser a -> [Token] -> ParseResult
sppf p str =  (\(_,s,_) -> s) (parse' defaultOptions p str)

-- | Parse a given string of characters 
parseString :: Parser a -> [Char] -> [a]
parseString p = parse p . charS

-- | Parse a given string of characters and options 
parseStringWithOptions :: PCOptions -> Parser a -> [Char] -> [a]
parseStringWithOptions opts p = parseWithOptions opts p . charS
infixl 3 <::=>
-- | use <::=> to enforce using parse context (to handle left-recursion)
(<::=>) :: String -> Parser a -> Parser a
x <::=> _r = let (sym,_r_rules,_r_sem) = _r
                 alt     = Alt x [sym] -- TODO indirection (extra alt)
                 rules m = case M.lookup x m of
                            Nothing -> _r_rules (M.insert x [alt] m)
                            Just _  -> m

                 sem opts arr ctx sppf l r m
                    | (l,r,x) `inContext` ctx = S.empty
                    | otherwise = let ctx' = (l,r,x) `toContext` ctx
                                   in _r_sem opts arr ctx' sppf l r m
              in (Nt x,rules,sem)

-- | useful for non-recursive definitions (only internally)
infixl 3 <:=>
(<:=>) :: String -> Parser a -> Parser a
x <:=> _r = let (sym,_r_rules,_r_sem) = _r
                alt     = Alt x [sym] -- TODO indirection (extra alt)
                rules m = case M.lookup x m of
                          Nothing -> _r_rules (M.insert x [alt] m)
                          Just _  -> m
              in (Nt x,rules,_r_sem)

infixl 5 <$>
-- | Application of a semantic action. 
(<$>) :: (Ord b, Ord a) => (a -> b) -> Parser a -> Parser b
f <$> _r = let (sym,rules,_r_sem) = _r
               sem opts arr ctx sppf l r m = S.map f (_r_sem opts arr ctx sppf l r m)
            in (sym,rules,sem)

infixl 6 <*>
-- | Sequence two parsers, the results of the two parsers are tupled.
(<*>) :: (Ord a, Ord b) => Parser a -> Parser b -> Parser (a,b)
_l <*> _r = (Nt lhs_id,rules,sem)
 where  l_id    = id_ _l
        r_id    = id_ _r
        lhs_id  = concat [l_id, "*", r_id]
        
        -- ** one can bind this parser and recurse on it + other duplicate work
        alt     = Alt lhs_id [sym_ _l, sym_ _r]
        rules m = case M.lookup lhs_id m of -- necessary? **
                    Nothing -> rules_ _r (rules_ _l (M.insert lhs_id [alt] m))
                    Just _  -> m
        
        sem opts arr ctx sppf l r m = 
            let choices = maybe ks (\c -> maximumsWith c ks) $ 
                            pivot_select opts in S.fromList
            [ (a,b) | k <- choices 
                    , a <- S.toList (sem_ _l opts arr ctx sppf l k m)
                    , b <- S.toList (sem_ _r opts arr ctx sppf k r m) ]
         where ks = maybe [] id $ sppf `pNodeLookup` ((alt,2), l, r)

infixl 4 <|>
-- | A choice between two parsers, results of the two are concatenated
(<|>) :: (Ord a) => Parser a -> Parser a -> Parser a
_l <|> _r = (Nt lhs_id,rules,sem)
 where  l_id    = id_ _l
        r_id    = id_ _r
        lhs_id  = concat [l_id, "|", r_id]

        alts    = [Alt lhs_id [sym_ _l], Alt lhs_id [sym_ _r]]
        rules m = case M.lookup lhs_id m of
                    Nothing -> rules_ _r (rules_ _l (M.insert lhs_id alts m))
                    Just _  -> m

        sem opts arr ctx sppf l r m =  
            concatChoice opts (sem_ _l opts arr ctx sppf l r m)
                              (sem_ _r opts arr ctx sppf l r m)

-- derived combinators
infixl 6 <*
-- | Sequencing, ignoring the result to the right
(<*) :: (Ord a, Ord b) => Parser a -> Parser b -> Parser a
_l <* _r = (\(x,y) -> x) <$> _l <*> _r

infixl 6 *>
-- | Sequencing, ignoring the result to the left 
(*>) :: (Ord a, Ord b) => Parser a -> Parser b -> Parser b
_l *> _r = (\(x,y) -> y) <$> _l <*> _r

infixl 5 <$
-- | Ignore all results and just return the given value
(<$) :: (Ord a, Ord b) => a -> Parser b -> Parser a
f <$ _r = const f <$> _r 

-- elementary parsers
raw_parser :: String -> Token -> (Token -> a) -> Parser a
raw_parser str t f = (Nt str, rules, sem)
    where   alt     = Alt str [Term t]
            rules   = M.insert str [alt] 
            sem _ arr ctx sppf l r m 
                | l + 1 == r && l < m && arr A.! l == t = S.singleton (f t)
                | otherwise = S.empty

-- | A parser that recognises a given character
char :: Char -> Parser Char
char c = raw_parser ([c]) (Char c) (\(Char c) -> c)

-- | A parser that recognises a given token
token :: Token -> Parser Token
token t = raw_parser (show t) t id

-- | A parser that always succeeds (and returns unit)
epsilon :: Parser ()
epsilon = (Nt x, rules, sem)
    where   x       = "__eps"
            alt     = Alt x []
            rules   = M.insert x [alt]
            sem _ arr ctx sppf l r m  | l == r    = S.singleton ()
                                      | otherwise = S.empty

-- | A parser that always succeeds and returns a given value
satisfy :: (Ord a) => a -> Parser a
satisfy a = a <$ epsilon

-- helpers
sym_ :: Parser a -> Symbol
sym_ (f,_,_) = f

id_   :: Parser a -> Nt 
id_ (Nt x,_,_)   = x

rules_ :: Parser a -> Visit2
rules_ (_,f,_) = f

sem_   :: Parser a -> Visit3 a
sem_ (_,_,f)   = f

mkNt :: String -> Char -> Nt
mkNt x c = concat ["(",x,")",[c]]

inContext :: (Int, Int, Nt) -> ParseContext -> Bool
inContext (l,r,x) = maybe False inner . IM.lookup l 
    where inner = maybe False ((==) x) . IM.lookup r

toContext :: (Int, Int, Nt) -> ParseContext -> ParseContext
toContext (l,r,x) = IM.insertWith IM.union l (IM.singleton r x)

concatChoice :: (Ord a) => PCOptions -> S.Set a -> S.Set a -> S.Set a
concatChoice opts ls rs = if left_biased_choice opts
                            then firstRes
                            else ls `S.union` rs
 where  firstRes | S.null ls  = rs
                 | otherwise  = ls

-- higher level patterns

-- | Optionally use the given parser
optional :: (Ord a) => Parser a -> Parser (Maybe a)
optional p@(Nt x,_,_) = (mkNt x '?') <:=> satisfy Nothing <|> Just <$> p

-- | Apply the given parser many times, 0 or more times (Kleene closure)
many :: (Ord a) => Parser a -> Parser [a]
many p@(Nt x,_,_) = (mkNt x '^') <::=> satisfy [] 
                                   <|> uncurry (:) <$> p <*> many p

-- | Apply the given parser some times, 1 or more times (positive closure)
some :: (Ord a) => Parser a -> Parser [a]
some p@(Nt x,_,_) = let rec = (mkNt x '+') <::=> (:[]) <$> p
                                            <|> uncurry (:) <$> p <*> rec
                    in rec

