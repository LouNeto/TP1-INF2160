--TP1 dans le cadre du cours INF2160
--Lou-Gomes Neto et Pier-Olivier Decoste
--NETL14039105, DECP09059005

import System.Environment(getArgs)

miniTex :: String -> String
miniTex [] = []
miniTex ss = titleFormat $ tables (figures (sections ss 1) 1.1) 1.1

references :: String -> [(String, String)]
references [] = []
references (x:xs) | x == '\\' = (takeWhile (/= '{') xs , takeWhile (/= '}')
                      (drop 2 (dropWhile (/= '}') xs))) : references xs
                  | otherwise = references xs

sections :: String -> Int -> String
sections [] _ = []
sections [x] _ = [x]
sections (x:y:xs) n
  | x == '\\' && y == 's' = "Section " ++ show n
      ++ " : " ++ sections (dropWhile (/= '{') xs) (n+1)
  | otherwise = x : sections (y:xs) n

figures :: String -> Float -> String
figures [] _ = []
figures [x] _ = [x]
figures (x:y:xs) n
  | isNum x && ((read [x]::Int) > (floor n)) = figures (x:y:xs) (n+1)
  | x == '\\' && y == 'f' = "Figures " ++ show n ++ " : "
      ++ figures (dropWhile (/= '{') xs) (n + 0.1)
  | otherwise = x : figures (y:xs) n
  where isNum x = elem x "0123456789"

tables :: String -> Float -> String
tables [] _ = []
tables [x] _ = [x]
tables (x:y:xs) n
  | isNum x && ((read [x]::Int) > (floor n)) = tables (x:y:xs) (n+1)
  | x == '\\' && y == 't' = "Tables " ++ show n ++ " : "
      ++ tables (dropWhile (/= '{') xs) (n + 0.1)
  | otherwise = x : tables (y:xs) n
  where isNum x = elem x "0123456789"

-------------------------------------------------------------------------------

--newSection :: (Fractional a, RealFrac a1, Integral a) => a1 -> a
--newSection x = floor (x + 1) + 0.1

titleFormat :: String -> String
titleFormat [] = []
titleFormat [x] = [x]
titleFormat (x:y:xs)
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
          print (references contenuFichier)
          putStr (miniTex contenuFichier)
