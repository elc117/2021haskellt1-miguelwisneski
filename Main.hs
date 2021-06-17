import Text.Printf
import Func

main :: IO ()
main = do
  putStrLn "Difite um valor aleatorio: "
  n <- readInt
  putStrLn "Digite o raio desejado: "
  r <- readInt
  putStrLn "Digite a largura desejada: "
  x <- readFloat
  putStrLn "Digite a altura desejada: "
  y <- readFloat

  let px = floatIntoInt x
      py = floatIntoInt y
  let arquivo = concat ([svgBegin x y] ++ (svgElements n px py r) ++ (svgElements' n px py r) ++ (svgElements'' n px py r) ++ (svgElements''' n px py r) ++ [svgEnd])
                
   in writeFile "img.svg" arquivo
