--TP1 dans le cadre du cours INF2160
--Lou-Gomes Neto et Pier-Olivier Decoste
--NETL14039105, DECP09059005

import System.Environment(getArgs)

-- fonction que vous devez completer.
-- pour l'instant elle retourne la chaine recu en argument,
-- vous devez changer la valeur de retour pour le resultat
-- de votre programme.
miniTex :: String -> String
miniTex [] = []
miniTex (x:xs) = sections (x:xs)

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

--------
--MAIN--
--------

main = do arguments <- getArgs
          contenuFichier <- readFile (head arguments)
          putStr (miniTex contenuFichier)
