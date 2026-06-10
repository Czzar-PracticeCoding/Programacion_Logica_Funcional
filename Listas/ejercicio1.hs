contarPares :: [Int] -> Int
contarPares [] = 0
contarPares (x:xs)
  | even x = 1 + contarPares xs
  | otherwise = contarPares xs

clasificarNota :: Int -> String
clasificarNota nota
  | nota >= 18 = "Excelente"
  | nota >= 14 = "Bueno"
  | nota >= 11 = "Aprobado"
  | otherwise = "Desaprobado"

main :: IO ()
main = do
  putStrLn "Ingrese una lista de numeros:"
  entrada <- getLine

  let numeros = map read (words entrada) :: [Int]

  putStrLn $ "Cantidad de pares: " ++ show (contarPares numeros)

  putStrLn "Ingrese una nota:"
  notaTexto <- getLine

  let nota = read notaTexto :: Int

  putStrLn $ clasificarNota nota