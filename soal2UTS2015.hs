--Fungsi Dasar
isEmpty :: [Int] -> Bool
isOneElmt :: [Int] -> Bool
konso :: Int -> [Int] -> [Int]

isEmpty l = null l
isOneElmt l = (length l) == 1
konso e li = [e] ++ li


nilaiEkstrim :: [Int] -> (Int,Int)
{- nilaiEkstrim l mengembalikan pasangan integer (min,max), dengan min adalah nilai terkecil pada
 l dan max adalah nilai terbesar pada l.
 Prekondisi: l tidak kosong
 Contoh: nilaiEkstrim [3,2,6,5,8,0,1,2,12,56,44,3,28] = (0,56) -}

nilaiEkstrim l = if isOneElmt l then ((head l), (head l))
                  else
                      let (min,max) = nilaiEkstrim (tail l)
                      in
                          if (head l) < min then (head l,max)
                          else if (head l) > max then (min, head l)
                          else (min,max)


trunc :: [Int] -> Int -> [Int]
{- trunc l i mengembalikan i buah elemen terdepan dari l.
Apabila i lebih besar dari jumlah elemen pada l, maka fungsi mengirimkan l.
Prekondisi: i > 0
Contoh: trunc [3,2,6,5,8] 3 = [3,2,6] -}

trunc l i | i == 0 || (null l) = l
          | otherwise = [head l] ++ (trunc (tail l) (i-1))



splitList :: [Int] -> ([Int],[Int])
{- splitList l mengembalikan dua buah list lfront dan lback, dengan lfront memuat elemen l
bagian depan dan lback memuat elemen l bagian belakang.
Jumlah elemen pada lfront sama dengan atau 1 elemen lebih banyak dari pada lback.
Contoh: splitList [3,2,6,5,8] = ([3,2,6],[5,8]) -}

splitList li = if isOneElmt li then ([head li], [])
               else 
                   let (front,back) = splitList (tail(init li))
                   in
                       ([head li] ++ front , [last li] ++ back)


insSorted :: Int -> [Int] -> [Int]
{- insSorted x l menerima sebuah integer x dan sebuah list l yang terurut membesar serta
mengembalikan list l yang telah ditambahkan x sedemikian sehingga elemen-elemennya tetap
terurut membesar.
Contoh: insSorted 4 [2,3,5,6] = [2,3,4,5,6] -}

insSorted x li = if null li then [x]
                 else if head li < x then [head li] ++  (insSorted x (tail li))
                 else [x] ++ li