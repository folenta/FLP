{-
    FLP - Simplify BKG
    Author: Jan Folenta (xfolen00)
    Date: 11.4.2021
-}

module Simplify where

import Data.List
import Data.List.Split
import Data.String

import BKGdata
import ParseBKG


-- areTheSame checks if given lists of characters are the same
areTheSame :: String -> String -> Bool
areTheSame oldList newList
    | length oldList == length newList = True
    | otherwise = False

-- findNi gets list of grammar rules, previous set Ni (Ni-1) and list of terminals and returns
-- new set Ni, which is set of nonteriminals that generate terminals or epsilon
findNi :: [GrammarRule] -> String -> String -> String
findNi [] oldNi _ = nub oldNi
findNi ((leftSideOfRule, rightSideOfRule):rules) oldNi terminals 
    | (isInStr rightSideOfRule (oldNi ++ terminals ++ [eps])) = 
        findNi rules (oldNi ++ [leftSideOfRule]) terminals
    | otherwise = findNi rules oldNi terminals

-- findNt gets list of grammar rules, previous set Ni (Ni-1) and list of terminals 
-- and returns set Nt (which is Ni) when Ni and Ni-1 are the same 
findNt :: [GrammarRule] -> String -> String -> String
findNt rules oldNt terminals 
    | areTheSame oldNt (findNi rules oldNt terminals) = oldNt
    | otherwise = findNt rules (findNi rules oldNt terminals) terminals

-- deleteWrongRulesOfStep1 gets list of grammar rules, set Nt and set of terminals and returns
-- only grammar rules that meet the conditions
deleteWrongRulesOfStep1 :: [GrammarRule] -> String -> String -> [GrammarRule] 
deleteWrongRulesOfStep1 [] _ _ = []
deleteWrongRulesOfStep1 ((leftSideOfRule, rightSideOfRule):rules) correctNonterminals terminals
    | (leftSideOfRule `elem` correctNonterminals) 
    && (isInStr rightSideOfRule (correctNonterminals ++ terminals ++ [eps])) =
        (leftSideOfRule, rightSideOfRule) : (deleteWrongRulesOfStep1 rules correctNonterminals terminals)
    | otherwise = deleteWrongRulesOfStep1 rules correctNonterminals terminals


-- simplifyStep1 gets input grammar and simpifies grammar by step 1
simplifyStep1 :: Grammar -> Grammar
simplifyStep1 (Grammar nonterms terms initialNonterm grammarRules) =
    Grammar {
        nonterms = sort (nub $ (findNt grammarRules [] terms) ++ [initialNonterm]), 
        terms = sort terms, 
        initialNonterm = initialNonterm, 
        grammarRules = deleteWrongRulesOfStep1 grammarRules (findNt grammarRules [] terms) terms
    }

-- findVi gets list of grammar rules, previous set Vi (Vi-1) and returns new set Vi
findVi :: [GrammarRule] -> String -> String
findVi [] oldVi = nub oldVi
findVi ((leftSideOfRule, rightSideOfRule):rules) oldVi 
    | leftSideOfRule `elem` oldVi = findVi rules (rightSideOfRule ++ oldVi)
    | otherwise = findVi rules oldVi

-- findV gets list of grammar rules and set Vi-1 and returns set V (which is Vi) 
-- when Vi and Vi-1 are the same 
findV :: [GrammarRule] -> String -> String
findV rules oldV
    | areTheSame oldV (findVi rules oldV) = oldV
    | otherwise = findV rules (findVi rules oldV)

-- deleteWrongRulesOfStep2 gets list of grammar rules and set Vt and returns
-- only grammar rules that meet the conditions
deleteWrongRulesOfStep2 :: [GrammarRule] -> String -> [GrammarRule]
deleteWrongRulesOfStep2 [] _ = []
deleteWrongRulesOfStep2 ((leftSideOfRule, rightSideOfRule):rules) v
    | (leftSideOfRule `elem` v) && (isInStr rightSideOfRule v) = 
        (leftSideOfRule, rightSideOfRule) : (deleteWrongRulesOfStep2 rules v)
    | otherwise = deleteWrongRulesOfStep2 rules v

-- simplifyStep2 gets simplufied grammar from by 1 and simpifies grammar by step 2
simplifyStep2 :: Grammar -> Grammar
simplifyStep2 (Grammar nonterms terms initialNonterm grammarRules) =
    Grammar {
        nonterms = sort (intersect nonterms (findV grammarRules [initialNonterm])), 
        terms = sort (intersect terms (findV grammarRules [initialNonterm])), 
        initialNonterm = initialNonterm, 
        grammarRules =  deleteWrongRulesOfStep2 grammarRules (findV grammarRules [initialNonterm])
    }