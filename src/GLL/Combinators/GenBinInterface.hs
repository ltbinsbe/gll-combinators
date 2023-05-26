
-- | Parser Combinators for GLL parsing inspired by Tom Ridge's P3 OCaml library
module GLL.Combinators.GenBinInterface (
    parse,
    parseString,
    grammar,
    (<::=>),
    (<$>),
    (<$),
    (<*>),
    (*>),
    (<*),
    (<|>),
    char,
    epsilon,
    satisfy,
    optional,
    many,
    some) where

import GLL.Types.Abstract
import GLL.Types.Derivations
import GLL.Common
import GLL.Machines.RGLL (gllSPPF)

import qualified    Data.Array  as A
import qualified    Data.Map    as M
import qualified    Data.Set    as S

type Visit1     = Symbol
type Visit2     = M.Map Nt [Alt] -> M.Map Nt [Alt]
type Visit3 a   = A.Array Int Token -> ParseContext -> SPPF 
                    -> Int -> Int -> Int -> S.Set a

type Parser a   = Seed -> SeedMap -> (Seed, SeedMap, Visit1, Visit2, Visit3 a)

type Seed       = Int
type SeedMap    = M.Map Nt Seed
type ParseContext = S.Set (Int, Int, Nt)

-- | Given a parser and a string of tokens, return:
--  * The grammar (GLL.Types.Abstract)
--  * a list of results, which are all semantic evaluations of 'good derivations'
--      - semantic evaluations are specified by using <$> and satisfy
--      - 'good derivations' as defined by by Tom Ridge
parse' :: Parser a -> [Token] -> (Grammar, [a])
parse' p str = 
    let (_,_,Nt start,rules,sem) = p 0 M.empty
        cfg     = Grammar start [] [ Rule x alts [] 
                                   | (x, alts) <- M.assocs (rules M.empty) ]
        sppf    = gllSPPF cfg str
        as      = sem arr S.empty sppf 0 m m
        m       = length str
        arr     = A.array (0,m) (zip [0..] str)
    in (cfg,S.toList as)

-- | The grammar of a given parser
grammar :: Parser a -> Grammar
grammar p = fst (parse' p [])

-- | The semantic results of a parser, given a token string
parse :: Parser a -> [Token] -> [a]
parse p str = snd (parse' p str)

-- | Parse a given string of characters 
parseString :: Parser a -> [Char] -> [a]
parseString p = parse p . charS

infixl 3 <::=>
-- | use <::=> to enforce using parse context (to handle left-recursion)
(<::=>) :: String -> Parser a -> Parser a
(x <::=> _r) i_seed i_seedM = 
    let m_seed  = M.lookup x i_seedM
        seed    = maybe i_seed id m_seed 
        p_seed  = maybe r_seed (const i_seed) m_seed
        seedM   = M.insert x seed i_seedM 

        (r_seed,r_seedM,sym,_r_rules,_r_sem) = _r seed seedM

        alt     = Alt x [sym] -- TODO indirection (extra alt)
        rules m = case M.lookup x m of
                   Nothing -> _r_rules (M.insert x [alt] m)
                   Just _  -> m

        sem arr ctx sppf l r m
            | (l,r,x) `S.member` ctx = S.empty
            | otherwise = let ctx' = S.insert (l,r,x) ctx
                           in _r_sem arr ctx' sppf l r m
     in (p_seed,r_seedM,Nt x,rules,sem)

-- | useful for non-recursive definitions (only internally)
infixl 3 <:==>
(<:==>) :: String -> Parser a -> Parser a
(x <:==> _r) seed seedM = 
    let (r_seed,r_seedM, sym,_r_rules,_r_sem) = _r seed seedM
        alt     = Alt x [sym] -- TODO indirection (extra alt)
        rules m = case M.lookup x m of
                  Nothing -> _r_rules (M.insert x [alt] m)
                  Just _  -> m
     in (r_seed,r_seedM,Nt x,rules,_r_sem)

infixl 5 <$>
-- | Application of a semantic action. 
(<$>) :: (Ord b, Ord a) => (a -> b) -> Parser a -> Parser b
(f <$> _r) seed seedM = 
    let   (seed',seedM',sym,rules,_r_sem) = _r seed seedM
          sem arr ctx sppf l r m = S.map f (_r_sem arr ctx sppf l r m)
     in (seed',seedM',sym,rules,sem)

infixl 6 <*>
-- | Sequence two parsers, the results of the two parsers are tupled.
(<*>) :: (Ord a, Ord b) => Parser a -> Parser b -> Parser (a,b)
(_l <*> _r) seed seedM = (r_seed,r_seedM,Nt lhs_id,rules,sem)
 where  lhs_id  = mkNt seed' '*'
        seed'   = succ seed

        (l_seed,l_seedM,l_sym,l_rules,l_sem) = _l seed' seedM
        (r_seed,r_seedM,r_sym,r_rules,r_sem) = _r l_seed l_seedM
        
        -- ** one can bind this parser and recurse on it + other duplicate work
        alt     = Alt lhs_id [l_sym, r_sym]
        rules m = case M.lookup lhs_id m of -- necessary? **
                    Nothing -> r_rules (l_rules (M.insert lhs_id [alt] m))
                    Just _  -> m
        
        sem arr ctx sppf l r m = S.fromList
            [ (a,b) | k <- ks
                    , a <- S.toList (l_sem arr ctx sppf l k m)
                    , b <- S.toList (r_sem arr ctx sppf k r m) ]
         where ks = maybe [] id $ sppf `pNodeLookup` ((alt,2), l, r)

infixl 4 <|>
-- | A choice between two parsers, results of the two are concatenated
(<|>) :: (Ord a) => Parser a -> Parser a -> Parser a
(_l <|> _r) seed seedM = (r_seed,r_seedM,Nt lhs_id,rules,sem)
 where  lhs_id  = mkNt seed' '|'
        seed'   = succ seed

        (l_seed,l_seedM,l_sym,l_rules,l_sem) = _l seed' seedM
        (r_seed,r_seedM,r_sym,r_rules,r_sem) = _r l_seed l_seedM

        alts    = [Alt lhs_id [l_sym], Alt lhs_id [r_sym]]
        rules m = case M.lookup lhs_id m of
                    Nothing -> r_rules (l_rules (M.insert lhs_id alts m))
                    Just _  -> m

        sem arr ctx sppf l r m =  l_sem arr ctx sppf l r m `S.union` 
                                  r_sem arr ctx sppf l r m 

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
raw_parser :: Token -> (Token -> a) -> Parser a
raw_parser t f seed seedM = (seed',seedM,Nt str, rules, sem)
    where   seed'   = succ seed
            str     = mkNt seed' '@'
            alt     = Alt str [Term t]
            rules   = M.insert str [alt] 
            sem arr ctx sppf l r m 
                | l + 1 == r && l < m && arr A.! l == t = S.singleton (f t)
                | otherwise = S.empty

-- | A parser that recognises a given character
char :: Char -> Parser Char
char c = raw_parser (Char c) (\(Char c) -> c)

-- | A parser that always succeeds (and returns unit)
epsilon :: Parser ()
epsilon seed seedM = (seed',seedM,Nt x, rules, sem)
    where   x       = mkNt seed' '#'
            seed'   = succ seed
            alt     = Alt x [Term Epsilon]
            rules   = M.insert x [alt]
            sem arr ctx sppf l r m  | l == r    = S.singleton ()
                                    | otherwise = S.empty

-- | A parser that always succeeds and returns a given value
satisfy :: (Ord a) => a -> Parser a
satisfy a = a <$ epsilon

-- helpers
sym_ :: (Visit1, Visit2, Visit3 a) -> Visit1
sym_ (f,_,_) = f

id_   :: (Visit1, Visit2, Visit3 a) -> Nt 
id_ (Nt x,_,_) = x

rules_ :: (Visit1, Visit2, Visit3 a) -> Visit2
rules_ (_,f,_) = f

sem_   :: (Visit1, Visit2, Visit3 a) -> Visit3 a
sem_ (_,_,f)   = f

mkNt :: Seed -> Char -> Nt
mkNt x c = concat ["__",show x,[c]]

-- higher level patterns

-- | Optionally use the given parser
optional :: (Ord a) => Parser a -> Parser (Maybe a)
optional p seed = let   x       = mkNt seed' '?'
                        seed'   = succ seed
                        rec     = x <:==> satisfy Nothing <|> Just <$> p
                    in rec seed'

-- | Apply the given parser many times, 0 or more times (Kleene closure)
many :: (Ord a) => Parser a -> Parser [a]
many p seed = let   x       = mkNt seed' '^'
                    seed'   = succ seed
                    rec     = x <::=> satisfy [] 
                                   <|> uncurry (:) <$> p <*> rec
                in rec seed'

-- | Apply the given parser some times, 1 or more times (positive closure)
some :: (Ord a) => Parser a -> Parser [a]
some p seed = let   x   = mkNt seed' '+'
                    seed' = succ seed
                    rec = x <::=> (:[]) <$> p <|> uncurry (:) <$> p <*> rec
              in rec seed'
