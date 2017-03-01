

--TP1 dans le cadre du cours INF2160
--Lou-Gomes Neto et Pier-Olivier Decoste
--NETL14039105, DECP09059005

import System.Environment(getArgs)

-- fonction que vous devez completer.
-- pour l'instant elle retourne la chaine recu en argument,
-- vous devez changer la valeur de retour pour le resultat
-- de votre programme.
miniTex :: String -> String
miniTex ss = tables $ figures $ sections ss

{-
sections :: String -> String
sections [] = []
sections [x] = [x]
sections (x:y:xs) | x == '\\' && y == 's' = separerAcc ('S':xs)
                | otherwise = x : y : sections xs

separerAcc :: String -> [String]
separerAcc [] = []
separerAcc (x:xs) = modifier $ splitOneOf "{}" (x:xs)

modifier :: [String] -> String
modifier [] = []
modifier [x] = "\n"
modifier (x:xs)   | x == "Section" = indexerSection x ++ modifier xs
                  | otherwise = x ++ modifier xs

indexerSection :: String -> String
indexerSection (x:xs) = xs ++ "1 : "
-}
------------------------------------------------------

sections :: String -> String
sections [] = []
sections [x] = [x]
sections (x:y:xs) | x == '\\' && y == 's' = sections ('S':xs)
                | otherwise = x : y : sections xs

tables :: String -> String
tables [] = []
tables [x] = [x]
tables (x:y:xs) | x == '\\' && y == 't' = tables (y:xs)
                | otherwise = x : y : tables xs

figures :: String -> String
figures [] = []
figures [x] = [x]
figures (x:y:xs) | x == '\\' && y == 't' = figures (y:xs)
                | otherwise = x : y : figures xs

{-
separerChaine :: String -> String
separerChaine [] = []
separerChaine [x] = [x]
separerCahine (x:y:xs) | x == '\\' && y == 's' = figures ('S':xs)
                       | otherwise = x : y : separerChaine xs
-}


--------
--MAIN--
--------

main = do arguments <- getArgs
          contenuFichier <- readFile (head arguments)
          putStr (miniTex contenuFichier)
