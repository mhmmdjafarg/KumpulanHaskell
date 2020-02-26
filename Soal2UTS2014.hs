--Tuliskan realisasi dari keempat fungsi berikut (definisi dan spesifikasi fungsi tidak perlu ditulis ulang).


--Fungsi Dasar
isEmpty :: [Int] -> Bool
isOneElmt :: [Int] -> Bool
konso :: Int -> [Int] -> [Int]

isEmpty l = null l
isOneElmt l = (length l) == 1
konso e li = [e] ++ li





isAllGanjil :: [Int] -> Bool
{- isAllGanjil l mengembalikan true apabila seluruh elemen l adalah bilangan ganjil.
 Fungsi mengembalikan true jika l adalah list kosong -}

isAllGanjil li = if isEmpty li then True
                 else
                     if (mod (head li) 2) == 1 && (head li) > 0 then isAllGanjil (tail li)
                     else False




getSmallest :: [Int] -> Int
{- getSmallest l mengembalikan elemen terkecil di l. Prekondisi: l tidak kosong. -}

getSmallest li = if isOneElmt li then head li
                 else
                     let a = getSmallest (tail li)
                     in if (head li) < a then (head li) else a


delElement :: Int -> [Int] -> [Int]
{- delElement x l mengembalikan list l dengan elemen x yang telah dihapus dari l.
 Jika x bukan elemen l, maka fungsi mengembalikan l semula.
 Prekondisi: elemen l unik (setiap elemen hanya muncul 1 kali). -}
delElement x li = if isEmpty li then []
                  else if (head li) /= x then konso (head li) (delElement x (tail li))
                  else (delElement x (tail li))




sortList :: [Int] -> [Int]
{- sortList l mengembalikan hasil pengurutan list l hingga elemen-elemennya terurut membesar.
 Prekondisi: l tidak kosong dan semua elemennya unik. -}

sortList l = let a = getSmallest l
             in let l1 = delElement a l
                in if isOneElmt l then l
                   else [a] ++ sortList l1