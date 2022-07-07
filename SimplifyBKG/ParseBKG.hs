{-
    FLP - Simplify BKG
    Author: Jan Folenta (xfolen00)
    Date: 11.4.2021
-}

module ParseBKG where

import Data.List
import Data.List.Split
import Data.String

import BKGdata 

-- parseGrammar puts together context free grammar from input string
parseGrammar :: String -> Grammar
parseGrammar input 
    | (length (lines input) < 4) = error "Something in grammar is missing!"
    | otherwise = Grammar {
        nonterms = nonterms, 
        terms = terms, 
        initialNonterm = initialNonterm, 
        grammarRules = rules
      }
      where
          nonterms = checkNonterms (splitOn "," ((lines input) !! 0))
          terms = checkTerms (splitOn "," ((lines input) !! 1))
          initialNonterm = checkInitialNonterm ((lines input) !! 2) nonterms
          rules = checkRules (drop 3 (lines input)) nonterms (nonterms ++ terms ++ [eps])

-- checkRules checks correct format of grammar rules (nonterminal on the left side, '->' in middle
-- and combination of terminals and nonterminals on the right side)
checkRules :: [String] -> [Char] -> [Char]-> [GrammarRule]
checkRules [rule] nterm ntermTerm
    | isCharInStr leftSideOfRule nterm && isInStr rightSideOfRule ntermTerm = 
      [(head leftSideOfRule, rightSideOfRule)]
    | otherwise = error "Invalid rule!"
    where
        splitRule = splitOn "->" rule
        (leftSideOfRule, rightSideOfRule) = (head splitRule, last splitRule)

checkRules (rule:rules) nterm ntermTerm
    | isCharInStr leftSideOfRule nterm && isInStr rightSideOfRule ntermTerm = 
      (head leftSideOfRule, rightSideOfRule) : checkRules rules nterm ntermTerm
    | otherwise = error "Invalid rule!"
    where
        splitRule = splitOn "->" rule
        (leftSideOfRule, rightSideOfRule) = (head splitRule, last splitRule)

checkRules _ _ _ = []

-- isCharInStr checks if given string contains given char
isCharInStr :: [Char] -> [Char] -> Bool
isCharInStr [x] str = x `elem` str
isCharInStr (x:xs) str = False
isCHarInStr _ _ = False

-- isInStr checks if given string is substring of other given string
isInStr :: [Char] -> [Char] -> Bool
isInStr [x] str = x `elem` str
isInStr (x:xs) str = (x `elem` str) && isInStr xs str
isInStr _ _ = False

-- checkDuplicity checks if there are more same characters in given string
checkDuplicity :: [Char] -> Bool
checkDuplicity [x] = True
checkDuplicity (x:xs)
    | isInStr [x] xs = error "There is duplicity in grammar!"
    | otherwise = checkDuplicity xs
checkDuplicity _ = error "Something in grammar is missing!"

-- checkNonterms checks if given characters are only capital characters from alphabet
checkNonterms :: [String] -> [Char]
checkNonterms nonterms 
    | (length nonterms > 0) && (all (`elem` ['A'..'Z']) (concat nonterms))
      && (checkDuplicity (concat nonterms)) = concat nonterms
    | otherwise =  error "Invalid nonterminal!"

-- checkTerms checks if given characters are only small characters from alphabet
checkTerms :: [String] -> [Char]
checkTerms terms
    | (length terms > 0) && (all (`elem` ['a'..'z']) (concat terms)) 
      && (checkDuplicity (concat terms)) = concat terms
    | otherwise = error "Invalid terminal!"

-- checkInitialNonterm checks if given character is one of given nonterminals
checkInitialNonterm :: String -> String -> Char
checkInitialNonterm initialNonterm nonterms
    | (length initialNonterm == 1) && ((head initialNonterm) `elem` ['A'..'Z']) && isCharInStr initialNonterm nonterms = head initialNonterm
    | otherwise = error "Wrong initial nonterminal!"