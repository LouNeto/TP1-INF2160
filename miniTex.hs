

--TP1 dans le cadre du cours INF2160
--Lou-Gomes Neto et Pier-Olivier Decoste
--NETL14039105, DECP09059005

import System.Environment(getArgs)

miniTex :: String -> String
miniTex [] = []
miniTex ss = sections ss 1

sections :: String -> Int -> String
sections [] _ = []
sections [x] _ = [x]
sections (x:y:xs) n
  | x == '\\' && y == 's' = "Section " ++ show n ++ " : " ++ sections (dropWhile (/= '{') xs) (n+1)
  | x == '{' && isUpper y = title ++ sections (drop (length(title)) (y:xs)) n
  | x == '}' = sections (dropWhile (/= '\n') (x:y:xs)) n
  | otherwise = x : sections (y:xs) n
  where title = takeWhile (/= '}') (y:xs)



isUpper :: Char -> Bool
isUpper c | c >= 'A' && c <= 'Z' = True
          | otherwise = False

--------
--MAIN--
--------

main = do arguments <- getArgs
          contenuFichier <- readFile (head arguments)
          putStr (miniTex contenuFichier)

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

          {-
          separerChaine :: String -> String
          separerChaine [] = []
          separerChaine [x] = [x]
          separerCahine (x:y:xs) | x == '\\' && y == 's' = figures ('S':xs)
                                 | otherwise = x : y : separerChaine xs
          -}

          ------------------------------------------------------
