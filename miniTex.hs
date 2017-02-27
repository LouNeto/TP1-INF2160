--TP1 dans le cadre du cours INF2160
--Lou-Gomes Neto et Pier-Olivier Decoste
--NETL14039105, DECP09059005

import System.Environment(getArgs)

-- fonction que vous devez completer.
-- pour l'instant elle retourne la chaine recu en argument,
-- vous devez changer la valeur de retour pour le resultat
-- de votre programme.
miniTex :: String -> String
miniTex ss = ss

main = do arguments <- getArgs
          contenuFichier <- readFile (head arguments)
          putStr (miniTex contenuFichier)
