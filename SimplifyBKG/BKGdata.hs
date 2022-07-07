{-
    FLP - Simplify BKG
    Author: Jan Folenta (xfolen00)
    Date: 11.4.2021
-}

module BKGdata where

import Data.List

-- creation new types that we need to put context free grammar together
type Nonterminal = Char
type Terminal = Char
type TerminalsAndNonterminals = String
type GrammarRule = (Char, String)

-- definition of grammar that consist of Nonterminals, Terminals, initial Nonterminal and Grammar Rules
data Grammar = Grammar {
    nonterms :: [Nonterminal],
    terms :: [Terminal],
    initialNonterm :: Nonterminal,
    grammarRules :: [GrammarRule]
}

-- definition of how the grammar will be printed
instance Show Grammar where
    show (Grammar nonterms terms initialNonterm grammarRules) = 
        intersperse ',' nonterms ++ "\n" ++
        intersperse ',' terms ++ "\n"++
        [initialNonterm] ++ "\n" ++
        showRules grammarRules

-- showRules connect left and right side of rule with '->' and prints it
showRules :: [GrammarRule] -> String
showRules [] = ""
showRules [rule] = [fst rule] ++ "->" ++ snd rule
showRules (rule:rules) = [fst rule] ++ "->" ++ snd rule ++ "\n" ++ showRules rules

-- declaration of symbol epsilon
eps :: Char
eps = '#'