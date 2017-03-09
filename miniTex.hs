--TP1 dans le cadre du cours INF2160
--Lou-Gomes Neto et Pier-Olivier Decoste
--NETL14039105, DECP09059005

import System.Environment(getArgs)

miniTex :: String -> String
miniTex [] = []
miniTex ss = titleFormat
  $ makeRef (tables (figures (sections ss 1) 1.1) 1.1)
            (references (tables (figures (sections ss 1) 1.1) 1.1))

sections :: String -> Int -> String
sections [] _ = []
sections [x] _ = [x]
sections (x:y:xs) n
  | x == '\\' && y == 's' = "\\Section " ++ show n
      ++ " : " ++ sections (dropWhile (/= '{') xs) (n+1)
  | otherwise = x : sections (y:xs) n

figures :: String -> Float -> String
figures [] _ = []
figures [x] _ = [x]
figures (x:y:xs) n
  | isNum x && ((read [x]::Int) > (floor n)) = figures (x:y:xs) (n+1)
  | x == '\\' && y == 'f' = "\\Figures " ++ show n ++ " : "
      ++ figures (dropWhile (/= '{') xs) (n + 0.1)
  | otherwise = x : figures (y:xs) n
  where isNum x = elem x "0123456789"

tables :: String -> Float -> String
tables [] _ = []
tables [x] _ = [x]
tables (x:y:xs) n
  | isNum x && ((read [x]::Int) > (floor n)) = tables (x:y:xs) (n+1)
  | x == '\\' && y == 't' = "\\Tables " ++ show n ++ " : "
      ++ tables (dropWhile (/= '{') xs) (n + 0.1)
  | otherwise = x : tables (y:xs) n
  where isNum x = elem x "0123456789"

references :: String -> [(String, String)]
references [] = []
references [x] = []
references (x:y:xs) | x == '\\' && y /= 'r' = (takeWhile (/= ':') (y:xs) , takeWhile (/= '}')
                      (drop 2 (dropWhile (/= '}') (y:xs)))) : references (y:xs)
                    | otherwise = references (y:xs)

makeRef :: String -> [(String,String)] -> String
makeRef [] _ = []
makeRef [x] _ = [x]
makeRef (x:y:xs) zs
  | x == '\\' && y == 'r' =
    "(voir la " ++ getRef (takeWhile (/= '}') (drop 1 (dropWhile (/= '{') xs))) zs
      ++ ")" ++ makeRef (drop 1 (dropWhile (/= '}') xs)) zs
  | otherwise = x : makeRef (y:xs) zs

-- cherche dans la liste de tuple une String qui equivaut a une String dans le
-- deuxieme element d'un tuple dans la list de tuple. Si le cas est rencontre
-- on retourne l'element ce trouvant a la premiere position du tuple.
getRef :: String -> [(String,String)] -> String
getRef _ [] = []
getRef s (x:xs) | s == snd x = fst x
                     | otherwise = getRef s xs

titleFormat :: String -> String
titleFormat [] = []
titleFormat [x] = [x]
titleFormat (x:y:xs)
  | x == '\\' = titleFormat (y:xs)
  | x == '{' && isUpper' y = title ++ titleFormat (drop (length(title)) (y:xs))
  | x == '}' = titleFormat (dropWhile (/= '\n') (x:y:xs))
  | otherwise = x : titleFormat (y:xs)
  where title = takeWhile (/= '}') (y:xs)

isUpper' :: Char -> Bool
isUpper' c | c >= 'A' && c <= 'Z' = True
           | otherwise = False

--------
--MAIN--
--------

main = do arguments <- getArgs
          contenuFichier <- readFile (head arguments)
          putStr (miniTex contenuFichier)
