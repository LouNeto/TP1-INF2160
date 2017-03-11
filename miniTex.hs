-----------------------------------------
-- TP1 dans le cadre du cours INF2160
-- Lou-Gomes Neto et Pier-Olivier Decoste
-- NETL14039105, DECP09059005
-----------------------------------------

import System.Environment(getArgs)

-- miniTex prend en parametre une chaine de charactere et formate celle-ci
-- selon des fonctions specifies dans la chaine. Elle fait appel aux multiples
-- fonctions de formatage pour formater la chaine recue.
miniTex :: String -> String
miniTex [] = []
miniTex ss = titleFormat (makeRef (funcFormat) (refList funcFormat))
              where funcFormat = tables (figures (sections ss 1) 11) 11

sections :: String -> Int -> String
sections [] _ = []
sections [x] _ = [x]
sections (x:y:xs) n | x == '\\' && y == 's' = [x] ++ "Section " ++ show n
                      ++ " : " ++ sections (dropWhile (/= '{') xs) (n+1)
                    | otherwise = x : sections (y:xs) n

figures :: String -> Int -> String
figures [] _ = []
figures [x] _ = [x]
figures (x:y:xs) n
  | isNum x && ((read [x]::Int) > (div n 10)) = figures (x:y:xs) (nextSection n)
  | x == '\\' && y == 'f' = [x] ++ "Figure " ++ showDecimal n ++ " : "
      ++ figures (dropWhile (/= '{') xs) (n+1)
  | otherwise = x : figures (y:xs) n
  where isNum x = elem x "0123456789"

tables :: String -> Int -> String
tables [] _ = []
tables [x] _ = [x]
tables (x:y:xs) n
  | isNum x && ((read [x]::Int) > (div n 10)) = tables (x:y:xs) (nextSection n)
  | x == '\\' && y == 't' = [x] ++ "Table " ++ showDecimal n ++ " : "
      ++ tables (dropWhile (/= '{') xs) (n+1)
  | otherwise = x : tables (y:xs) n
  where isNum x = elem x "0123456789"

makeRef :: String -> [(String, String)] -> String
makeRef [] _ = []
makeRef [x] _ = [x]
makeRef (x:y:xs) zs
  | x == '\\' && y == 'r' =
    "(voir la " ++ getRef (takeWhile (/= '}') (drop 1 (dropWhile (/= '{') xs))) zs
      ++ ")" ++ makeRef (drop 1 (dropWhile (/= '}') xs)) zs
  | otherwise = x : makeRef (y:xs) zs

refList :: String -> [(String, String)]
refList [] = []
refList [x] = []
refList (x:y:xs) | x == '\\' && y /= 'r' = (getTitre, getId) : refList (y:xs)
                 | otherwise = refList (y:xs)
                where getTitre = take ((length(takeWhile (/= ':') (y:xs)))-1) (y:xs)
                      getId = takeWhile (/= '}') (drop 2 (dropWhile (/= '}') (y:xs)))

-- cherche dans la liste de tuple une String qui equivaut au deuxieme element d'un
-- tuples de references. Si le cas est rencontre on retourne l'element ce
-- trouvant a la premiere position du tuple.
getRef :: String -> [(String, String)] -> String
getRef _ [] = []
getRef s (x:xs) | s == snd x = fst x
                | otherwise = getRef s xs

--Prend un string en parametre et retourne celui-ci sans accolades autour.
--La focntion retire aussi les identificateurs entre accolades
titleFormat :: String -> String
titleFormat [] = []
titleFormat [x] = [x]
titleFormat (x:y:xs)
  | x == '\\' = titleFormat (y:xs)
  | x == '{' && isUpper' y = title ++ titleFormat (drop (length(title)) (y:xs))
  | x == '}' = titleFormat (dropWhile (/= '\n') (x:y:xs))
  | otherwise = x : titleFormat (y:xs)
  where title = takeWhile (/= '}') (y:xs)

--La fonction retourne true si le charactere en parametre est une majuscle.
isUpper' :: Char -> Bool
isUpper' c | c >= 'A' && c <= 'Z' = True
           | otherwise = False

--La fonction prend un chiffre correspondant a une table ou une figure et
--l'incremente d'une dizaine additionnelle pour suivre le numero de la section
--en cours.
nextSection :: Int -> Int
nextSection n = ((div (n+10) 10) * 10) + 1

--Cette fonction construit un string qui represente la valeur int en parametre sous
--forme d'un chiffre a decimale. ex: 31 = "3.1"
showDecimal :: Int -> String
showDecimal n = show (div n 10) ++ "." ++ show (mod n 10)

-------------------------------------------------------------------------------
-----------------------------------MAIN----------------------------------------
-------------------------------------------------------------------------------

main = do arguments <- getArgs
          contenuFichier <- readFile (head arguments)
          putStr (miniTex contenuFichier)
