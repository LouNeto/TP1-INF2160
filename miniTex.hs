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
sections (x:xs) | x == '\\' = sections xs
                | otherwise = (x:xs)

tables :: String -> String
tables [] = []
tables (x:xs) = (x:xs)

figures :: String -> String
figures [] = []
figures (x:xs) = (x:xs)

--------
--MAIN--
--------

main = do arguments <- getArgs
          contenuFichier <- readFile (head arguments)
          putStr (miniTex contenuFichier)
