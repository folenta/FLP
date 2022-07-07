{-
    FLP - Simplify BKG
    Author: Jan Folenta (xfolen00)
    Date: 11.4.2021
-}

import System.Environment
import System.IO
import System.Exit

import BKGdata
import ParseBKG
import Simplify

main :: IO()
main = do 
    args <- getArgs

    if (length args /= 1) && (length args /= 2)
        then error "Wrong number of arguments!"
        else return()

    let (option, input) = processArguments args

    input <- readInput input

    let grammar = parseGrammar input

    case option of 
        "-i" -> print grammar
        "-1" -> print (simplifyStep1 grammar)
        "-2" -> print (simplifyStep2 (simplifyStep1 grammar))

    return()

-- readInput reads input either from input file or stdin.
readInput :: String -> IO String
readInput fileName
    | fileName == "input" = getContents
    | otherwise = readFile fileName

-- processArguments checks arguments and finds if we will read from a file or stdin 
processArguments :: [String] -> (String, String)
processArguments [opt] = (checkOption opt, "input")
processArguments [opt, file] = (checkOption opt, file)

-- checkOption checks if correct "option" was chosen
checkOption :: String -> String
checkOption opt 
    | opt `elem` ["-i", "-1", "-2"] = opt
    | otherwise = error "Wrong option! You can choose from [-i, -1, -2]"
