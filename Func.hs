module Func where
import Text.Printf

---------------------------------------------------------------------
-- Funções responsáveis por ler os valores digitadoes pelo usuário --
---------------------------------------------------------------------
readInt :: IO(Int)
readInt = do 
  int <- readLn
  return int

readFloat :: IO(Float)
readFloat = do 
  float <- readLn
  return float

floatIntoInt :: Float -> Int
floatIntoInt x = floor (0.1*x)

---------------------------------------------------------------------
-- Funções responsáveis criar a String SVG e colocar em uma lista  --
---------------------------------------------------------------------

-- Inicio da String 
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h

-- Final da String
svgEnd :: String
svgEnd = "</svg>"

-- Gera string representando elipse SVG 
svgellipse :: Int -> Int -> Int -> Int -> String -> String 
svgellipse x y r z style = 
  printf "<ellipse cx='%d' cy='%d' rx='%d' ry='%d' style='fill:%s' />\n" x y r z style

-- Cria um lista de elipses a partir do uso da função "svgellipse"
svgElements :: Int -> Int -> Int -> Int -> [String]
svgElements n x y r = [svgellipse (x*1) (w+y-50) ((r*3)-(r*2)) r (color (w+2*x-2*n) (w+2*y-2*r)) | w <- [n..r+10*n]]

svgElements' :: Int -> Int -> Int -> Int -> [String]
svgElements' n x y r = [svgellipse (x*4) (w+y) ((r*3)-(r*2)) r (color (w+2*x-2*n) (w+2*y-2*r)) | w <- [n..r+10*n]]

svgElements'' :: Int -> Int -> Int -> Int -> [String]
svgElements'' n x y r = [svgellipse (x*7) (w+y+50) ((r*3)-(r*2)) r (color (w+2*x-2*n) (w+2*y-2*r)) | w <- [n..r+10*n]]

svgElements''' :: Int -> Int -> Int -> Int -> [String]
svgElements''' n x y r = [svgellipse (x*10) (w+y+100) ((r*3)-(r*2)) r (color (w+2*x-2*n) (w+2*y-2*r)) | w <- [n..r+10*n]]

-- Gera uma cor aleatória para a elipse

color :: Int -> Int -> String
color x y
  | x < y = (\x -> "rgb" ++ show x) ((y-x)*2, x+5, y-x+10) 
  | x == y = (\x -> "rgb" ++ show x) (y-x+10, (y-x)*2, y+10) 
  | x > y = (\x -> "rgb" ++ show x) (x-y+10, y+5, (y-x)*2)
