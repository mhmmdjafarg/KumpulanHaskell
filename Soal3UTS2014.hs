{-Buatlah definisi, spesifikasi, dan realisasi sebuah fungsi offsetList yang menerima masukan dua buah fungsi, misalnya f
dan g, serta dua buah bilangan riil (float), a dan b. Fungsi offsetList akan menghasilkan sebuah list of float yang
merupakan penerapan fungsi f terhadap bilangan float antara a dan b, dimulai dari a dengan increment menggunakan
fungsi g.

Diasumsikan primitif untuk List of Float telah terdefinisi dan siap digunakan (lihat primitif pada soal 2, namun untuk
tipe data Float). Berikut adalah contoh masukan dan hasil dari fungsi:
Masukan Hasil offsetList f g a b
f g a b
id :: float -> float
-- id x mengirimkan nilai x
id x = x
p1 :: float -> float
-- p1 x mengirimkan nilai x + 1
p1 x = x + 1
1.0 5.0 [1.0, 2.0, 3.0, 4.0, 5.0]
p1 :: float -> float
-- p1 x mengirimkan nilai x + 1
p1 x = x + 1
p2 :: float -> float
-- p2 x mengirimkan nilai x + 2
p2 x = x + 2
5.0 10.0 [6.0,-}


offsetList :: (Float -> Float) -> (Float -> Float) -> Float -> Float -> [Float]

offsetList f g a b =
	if a>b then []
    else [f a] ++ offsetList f g (g a) b
    
-- ekspresi lambda
-- f g a b
-- dijumlah 2 dari fungsi g
-- (\x -> x) (\x -> x + 2) 1.2 7.1

{-Jika x < 0, akan mengirimkan nilai -999.0. Jika
x>=0 mengirimkan nilai x + 3.2
Mengirimkan nilai x + 0.5 - 1.0 1.0-}

-- (\x-> if(x < 0) then (-999.0) else (x+3.2)) (\x -> x + 0.5) (-1) 1


-- (\x -> x^2) (\x-> x+1) a b
	