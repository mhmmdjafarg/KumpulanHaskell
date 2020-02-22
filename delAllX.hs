delAllX :: [Int] -> Int -> ([Int],Int)
{- delAllX l x mengembalikan sebuah list l1 dan integer n, dengan l1 memuat elemen-elemen l
 setelah elemen bernilai x dihapus dan n memuat jumlah kemunculan elemen bernilai x pada l.
 Contoh: delAllX [3,2,6,2,3] 3 = ([2,6,2],2); delAllX [] 3 = ([],0) -}

isEmpty :: [Int] -> Bool
isOneElmt :: [Int] -> Bool
konso :: Int -> [Int] -> [Int]


--Realisasi
isEmpty l = null l
isOneElmt l = (length l) == 1
konso e li = [e] ++ li


delAllX li x = if (isEmpty li) then ([],0)
                else
                    let (m,n) = (delAllX (tail li) x)
                    in
                        if (head li) == x then (m, n+1)
                        else ((konso (head li) m), n)