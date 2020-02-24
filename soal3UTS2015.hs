{- Buatlah definisi, spesifikasi, dan realisasi sebuah fungsi sumInteger yang menerima 2 (dua) buah integer positif
(>0), misalnya m dan n, dan sebuah fungsi f dan menghasilkan penjumlahan dari semua integer antara m dan n
(termasuk m dan n) yang memenuhi f. Jika dalam selang m dan n tidak ada yang memenuhi f, maka hasilnya adalah
0. -}
sumInteger :: Int -> Int -> (Int -> Bool) -> Int
sumInteger m n f = if m > n then 0
                   else if f n then n + sumInteger m (n-1) f
                   else sumInteger m (n-1) f

isGenap :: Int -> Bool
isGenap x = (mod x 2) == 0

gThan5 :: Int -> Bool
gThan5 x = x > 5



-- Bagian b
{- Tuliskan aplikasi fungsi sumInteger pada butir a untuk 3 (tiga) masukan di bawah ini dalam bentuk ekspresi
lambda dan tuliskan pula hasil aplikasinya.
m n f Menerima masukan sebuah integer x dan ...
1 100 menghasilkan true jika x dapat membagi habis 100.
1 100 menghasilkan true jika x adalah bilangan genap dan dapat dibagi habis oleh 10
atau jika x adalah bilangan ganjil dan dapat dibagi habis oleh 5. Selain itu,
menghasilkan false.
25 25 menghasilkan true, jika x lebih kecil dari 10. -}

--(\x -> (mod 100 x == 0))

--(\x -> if ((mod x 2 == 0) && (mod x 10 == 0)) then True else if ((mod x 2 == 1) && (mod x 5 == 0)) then True else False )

-- (\x -> x < 10)