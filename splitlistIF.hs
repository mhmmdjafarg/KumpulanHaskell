{- Buatlah definisi, spesifikasi, dan realisasi sebuah fungsi splitListIF yang menerima sebuah list of integer,
misalnya li dan sebuah fungsi f (menerima sebuah nilai integer dan menghasilkan nilai boolean), dan
menghasilkan dua buah list of integer. List pertama berisi elemen li yang memenuhi f, dan list kedua berisi
elemen li yang tidak memenuhi f. List li mungkin kosong.
Diasumsikan primitif untuk list of integer telah terdefinisi dan siap digunakan (lihat soal 3).
-}


--Definisi dan Spesifikasi
splitListIF :: [Int] -> (Int -> Bool) -> ([Int], [Int])
isGenap :: Int -> Bool
gThan5 :: Int -> Bool
isPos :: Int -> Bool
isEmpty :: [Int] -> Bool
isOneElmt :: [Int] -> Bool
konso :: Int -> [Int] -> [Int]


--Realisasi
isEmpty l = null l
isOneElmt l = (length l) == 1
konso e li = [e] ++ li


isGenap x
        | ((mod x 2) == 0) && (x > 0)= True
        | otherwise = False

gThan5 x
        | x > 5 = True
        | otherwise = False

isPos x
        | x >= 0 = True
        | otherwise = False

splitListIF li fungsi = if isEmpty li then ([],[])
                        else
                            let (l1,l2) = splitListIF (tail li) fungsi
                            in 
                                if (fungsi (head li)) then
                                    ((konso (head li) l1), l2)
                                else (l1, (konso (head li) l2))