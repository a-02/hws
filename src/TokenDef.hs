module TokenDef where

import StdTokenDef

tokenDef = emptyStyle                      
                { commentLine    = "#"
                , nestedComments = False
                , reservedOpNames= []
                , reservedNames  = []
                , caseSensitive  = False
                }         
