
import GLL.Combinators.Interface hiding(lexer, parens, within)

import Data.Char (isDigit)

lexer :: String -> [Token]
lexer [] = []
lexer (x:xs) 
    | isDigit x = IntLit (Just (read [x])) : lexer xs
    | otherwise = Char x                   : lexer xs


pExpr :: BNF Token Int
pExpr = "Expr" <::=> (-) <$$> pExpr <** keychar '-' <**> pExpr
                <||> (+) <$$> pExpr <** keychar '+' <**> pExpr
                <||> (*) <$$> pExpr <** keychar '*' <**> pExpr
                <||> div <$$> pExpr <** keychar '/' <**> pExpr
                <||> int_lit 
                <||> parens pExpr

within :: BNF Token a -> BNF Token b -> BNF Token a -> BNF Token b
within l p r = mkRule $ l **> p <** r

parens :: BNF Token a -> BNF Token a
parens p = within (keychar '(') p (keychar ')')

run1 = parse pExpr (lexer "1+2*2-5")            -- [0,1,0,-5,-9] 
run2 = parse pExpr (lexer "((1+(2*2))-3)-5")    -- [-3]

pExpr1 :: BNF Token Int
pExpr1 = "Expr" <::=  (      (-) <$$> pExpr1 <** keychar '-' <**>>> pExpr1
                        <||> (+) <$$> pExpr1 <** keychar '+' <**>>> pExpr1 )
                 <||> (      (*) <$$> pExpr1 <** keychar '*' <**>>> pExpr1
                        <||> div <$$> pExpr1 <** keychar '/' <**>>> pExpr1 )
                 <||> (      int_lit
                        <||> parens pExpr1 )

run3 = parseWithOptions [] pExpr1 (lexer "1+2*2-5")

chainl :: BNF Token a -> BNF Token (a -> a -> a) -> BNF Token a
chainl p s = mkRule $
    foldl (flip ($)) <$$> p <**> many (flip <$$> s <**> p)

pExpr2 :: BNF Token Int
pExpr2 = pE1
 where  pE1 = chainl pE2 ("E1" <::=> (+) <$$ keychar '+' <||> (-) <$$ keychar '-')
        pE2 = chainl pE3 ("E2" <::=> (*) <$$ keychar '*' <||> div <$$ keychar '/')
        pE3 = "E3" <::=> int_lit <||> parens pExpr2

run4 = parse pExpr2 (lexer "1+2*2-5")       -- [0]
