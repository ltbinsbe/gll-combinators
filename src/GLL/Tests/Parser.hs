
module GLL.Tests.Parser where

import GLL.Parser

grammar1 = ("X",    [Prod "X" [Nt "A", Nt "A"]
                    ,Prod "A" [Term 'a']
                    ,Prod "A" [Term 'a', Term 'a']
                    ] )

fail1       = "a"
success1    = "aa"
success2    = "aaa"
fail2       = "aaaaa"

instance Parseable Char where
    eos = '$'
    eps = '#'

run = parseWithOptions [] grammar1 
