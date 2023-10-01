--Daniela Brazolin Flauto
--Marcello Gonzatto Birkan
--Thiago Leandro Liporace

import System.IO
import Data.List
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Control.Arrow (first)

-- Definindo a estrutura da árvore de Huffman.
data HuffmanTree a = Leaf a Int | Node (HuffmanTree a) (HuffmanTree a) Int
    deriving Show

-- Função para contar a frequência de cada caractere.
countFrequency :: String -> [(Char, Int)]
countFrequency = map (\xs -> (head xs, length xs))
                . groupBy (==) . sort

-- Função para construir a árvore de Huffman.
buildTree :: [(Char, Int)] -> HuffmanTree Char
buildTree cs = build initialTrees
    where
        initialTrees = sortOn treeWeight $ map (\(c, freq) -> Leaf c freq) cs

        build :: [HuffmanTree Char] -> HuffmanTree Char
        build [t] = t
        build (t1:t2:ts) = build $ insertBy (comparing treeWeight) (combine t1 t2) ts

        treeWeight :: HuffmanTree Char -> Int
        treeWeight (Leaf _ w) = w
        treeWeight (Node _ _ w) = w

        combine :: HuffmanTree Char -> HuffmanTree Char -> HuffmanTree Char
        combine t1 t2 = Node t1 t2 (treeWeight t1 + treeWeight t2)

-- Função para criar um mapa de codificação a partir da árvore de Huffman.
generateCodes :: HuffmanTree Char -> Map.Map Char String
generateCodes = generate Map.empty ""
    where
        generate m prefix (Leaf c _) = Map.insert c prefix m
        generate m prefix (Node l r _) = generate (generate m (prefix ++ "0") l) (prefix ++ "1") r

-- Função para codificar o texto.
encode :: Map.Map Char String -> String -> String
encode m = concatMap (\c -> m Map.! c)

main :: IO ()
main = do
    content <- readFile "in.txt"
    putStrLn "Conteúdo lido do arquivo:"
    putStrLn content  

    let freqTable = countFrequency content
    let huffmanTree = buildTree freqTable
    let codeMap = generateCodes huffmanTree
    let encodedContent = encode codeMap content
  

    putStrLn "Conteúdo codificado:"
    putStrLn encodedContent 

    writeFile "out.txt" encodedContent
    putStrLn "Arquivo codificado com sucesso!"
