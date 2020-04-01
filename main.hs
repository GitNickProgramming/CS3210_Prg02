{-# LANGUAGE ParallelListComp #-}
import Data.List

type Seq   = [Char]
type Board = [Seq]

mainDiag :: Board -> [Seq]
mainDiag b = [ lst | x <- [0 .. ((diagonals b) - 1)], let lst = [ a | let y = mainDiagIndices b x, z <- [0 .. ((length y) - 1)], let a = (b!!(fst (y!!z)))!!(snd (y!!z))] ]

secDiagIndices :: Board -> Int -> [ (Int, Int) ]
secDiagIndices b p
  | p < n = [ (p - q, q) | q <- [0..p] ]
  | otherwise = [ (p - (n - 1 - q), n - 1 - q) | q <- [2 * (n - 1) - p, 2 * (n - 1) - p - 1..0] ]
  where n = size b

setup :: Int -> Board
setup n
    | n < 4 = [line | line <- replicate 4 (replicate 4 '-')]
    | otherwise = [line | line <- replicate n (replicate n '-')]

rows :: Board -> Int
rows b = length b

cols :: Board -> Int
cols b 
    | length(nub (map length b)) == 1 = length b
    | otherwise = 0

size :: Board -> Int
size b
    | cols b == rows b = rows b
    | otherwise = 0

queensSeq :: Seq -> Int
queensSeq s = length(filter (=='Q') s )

queensBoard :: Board -> Int
queensBoard b = length( filter (=='Q') (unwords b))

seqValid :: Seq -> Bool
seqValid s
    | queensSeq s < 2 = True
    | queensSeq s >= 2 = False

rowsValid :: Board -> Bool
rowsValid b
    | length b == 0 = True
    | seqValid (b!!((length b) - 1)) == False = False
    | otherwise = rowsValid(init b)

colsValid :: Board -> Bool
colsValid b
    | size b == 1 = True
    | length [ i | i <- [0 .. ((length (b!!0)) - 1)], j <- [1 ..  ((size b) - 1)], (b!!0)!!i == 'Q', (b!!0)!!i == (b!!j)!!i ] > 0 = False
    | otherwise = colsValid (tail b)

diagonals :: Board -> Int
diagonals b = (2 * (size b) - 1)

mainDiagIndices :: Board -> Int -> [ (Int, Int) ]
mainDiagIndices b p
  | p < n = [ (n - 1 - qr, q) | q <- [0..p] | qr <- [p,p-1..0] ]
  | otherwise = [ (q, (n - 1 - qr)) | q <- [0..2 * (n - 1) - p] | qr <- [2 * (n - 1) - p,2 * (n - 1) - p - 1..0] ]
  where n = size b

allMainDiagIndices :: Board -> [[ (Int, Int) ]]
allMainDiagIndices b = [ mainDiagIndices b n| n <- [0 .. ((diagonals b) -1)] ]

allSecDiagIndices :: Board -> [[ (Int, Int) ]]
allSecDiagIndices b = [ secDiagIndices b n| n <- [0 .. ((diagonals b) -1)] ]

secDiag :: Board -> [Seq]
secDiag b = [ lst | x <- [0 .. ((diagonals b) - 1)], let lst = [ a | let y = secDiagIndices b x, z <- [0 .. ((length y) - 1)], let a = (b!!(fst (y!!z)))!!(snd (y!!z))] ]

diagsValid :: Board -> Bool
diagsValid b
    | length ([ md | let i = mainDiag b, j <- [0 .. ((length i) - 1)], let l = i!!j, length(filter (=='Q') l) > 1, let md = l]) > 0 = False
    | length ([ md | let i = secDiag b, j <- [0 .. ((length i) - 1)], let l = i!!j, length(filter (=='Q') l) > 1, let md = l]) > 0 = False
    | otherwise = True

valid :: Board -> Bool
valid b
    | rowsValid b == False = False
    | colsValid b == False = False
    | diagsValid b == False = False
    | otherwise = True

solved :: Board -> Bool
solved b
    | valid b == False = False
    | (size b) /= (queensBoard b) = False
    | otherwise = True

setQueenAt :: Board -> Int -> [Board]
setQueenAt b i = do
  let z = replicate ((size b) - 1) '-'
  let p = nub (permutations ("Q" ++ z))
  [ [ (b!!k) | k <- [0..(i-1)] ] ++ [r] ++ [ (b!!k) | k <- [(i+1)..((rows b) - 1)] ] | r <- p ]

nextRow :: Board -> Int
nextRow b = head [ i | i <- [0 .. (size b) - 1], queensSeq (b!!i) == 0 ]

solve :: Board -> [Board]
solve b
  | solved b = [b]
  | otherwise = concat [ solve newB | newB <- setQueenAt b i, valid newB ]
    where i = nextRow b

main = do
  let b = setup 4
  let solution = [ solution | solution <- solve b ]
  print ("mainDiagIndices")
  print (mainDiagIndices (setup 4) 0)
  print (mainDiagIndices (setup 4) 1)
  print (mainDiagIndices (setup 4) 2)
  print (mainDiagIndices (setup 4) 3)
  print (mainDiagIndices (setup 4) 4)
  print (mainDiagIndices (setup 4) 5)
  print (mainDiagIndices (setup 4) 6)
  print ("secDiagIndices")
  print (secDiagIndices (setup 4) 0)
  print (secDiagIndices (setup 4) 1)
  print (secDiagIndices (setup 4) 2)
  print (secDiagIndices (setup 4) 3)
  print (secDiagIndices (setup 4) 4)
  print (secDiagIndices (setup 4) 5)
  print (secDiagIndices (setup 4) 6)
  print ("setup")
  print (setup 3)
  print (setup 5)
  print ("rows")
  print (rows ["----","----","----","----"])
  print (rows ["-----","-----","-----","-----","-----"])
  print ("cols")
  print (cols ["----","----","----","----"])
  print (cols ["-----","-----","-----","-----","-----"])
  print (cols ["----","-----","-----","-----","-----"])
  print ("size")
  print (size ["----","----","----","----"])
  print (size ["-----","-----","-----","-----","-----"])
  print (size ["----","-----","-----","-----","-----"])
  print ("queensSeq")
  print (queensSeq "----")
  print (queensSeq "-Q--")
  print (queensSeq "-Q-Q")
  print ("queensBoard")
  print (queensBoard ["----", "----", "----", "----"])
  print (queensBoard ["Q---", "--Q-", "--Q-", "----"])
  print ("seqValid")
  print (seqValid "----")
  print (seqValid "-Q--")
  print (seqValid "-Q-Q")
  print ("rowsValid")
  print (rowsValid ["----","----","----","----"])
  print (rowsValid ["-Q--","----","----","----"])
  print (rowsValid ["-Q--","Q---","----","----"])
  print (rowsValid ["-Q--","Q---","--Q-","----"])
  print (rowsValid ["-Q--","Q---","--QQ","----"])
  print ("colsValid")
  print (colsValid ["----","----","----","----"])
  print (colsValid ["-Q--","----","----","----"])
  print (colsValid ["-Q--","Q---","----","----"])
  print (colsValid ["-Q--","Q---","--Q-","----"])
  print (colsValid ["-Q--","Q---","--QQ","----"])
  print (colsValid ["-Q--","Q---","--QQ","---Q"])
  print ("diagonals")
  print (diagonals (setup 4))
  print (diagonals (setup 5))
  print ("allMainDiagIndices")
  print (allMainDiagIndices (setup 4))
  print (allMainDiagIndices (setup 5))
  print ("mainDiag")
  print (mainDiag (setup 4))
  print (mainDiag (setup 5))
  print ("allSecDiagIndices")
  print (allSecDiagIndices (setup 4))
  print (allSecDiagIndices (setup 5))
  print ("secDiag")
  print (secDiag (setup 4))
  print (secDiag (setup 5))
  print ("diagsValid")
  print (diagsValid ["Q---","--Q-","-Q--","---Q"])
  print (diagsValid ["-Q--","---Q","Q---","-Q--"])
  print (diagsValid ["Q----","--Q--","---Q-","-Q---", "---Q-"])
  print (diagsValid ["---Q-","-----","-Q---","-----", "----Q"])
  print (diagsValid ["--Q-----","------Q-","-Q------","-------Q","-----Q--","---Q----","Q-------","----Q---"])
  print ("valid")
  print (valid ["Q---","--QQ","----","----"])
  print (valid ["Q---","--Q-","--Q-","----"])
  print (valid ["Q---","--Q-","-Q--","---Q"])
  print (valid ["--Q-----","------Q-","-Q------","-------Q","-----Q--","---Q----","Q-------","----Q---"])
  print ("solved")
  print (solved ["Q---","--Q-","----","----"])
  print (solved ["--Q-","Q---","---Q","-Q--"])
  print ("Solving the N-Queens Puzzle")
  print ("setQueenAt")
  print (setQueenAt ["Q---","--Q-","----","----"])
  print (setQueenAt ["Q---","--Q-","-Q--","----"])
  print ("nextRow")
  print (nextRow["Q---","--Q-","----","----"])
  print (nextRow ["Q---","--Q-","-Q--","----"])
  print ("solve")
  print (solve (setup 4))
  print (solve (setup 5))
  print (solution)