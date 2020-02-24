{-} Fungsi ubahArah, menerima masukan berupa dua buah integer. Integer pertama (s) bernilai antara 0 hingga 359,
merepresentasikan arah pergerakan sebuah benda (dalam satuan derajat). Integer kedua (r) bernilai antara -359
hingga 359 menyatakan besar perubahan arah pergerakan benda tersebut (dalam satuan derajat): nilai positif
menyatakan perubahan arah ke kanan (searah jarum jam), sedangkan nilai negatif menyatakan perubahan arah ke kiri
(berlawanan arah jarum jam). Fungsi akan mengembalikan bilangan integer antara 0 sampai 359, yang menyatakan
arah pergerakan yang baru setelah diubah sebesar a derajat. Karena sudut paling besar adalah 360 derajat, maka 360
derajat sama dengan 0 derajat. 

-}
-- DEFINISI DAN SPESIFIKASI FUNGSI
ubahArah :: Int -> Int -> Int

ubahArah s r = if (s + r < 360) then (s + r)
               else ubahArah s (r-360)
{- ubahArah s r adalah fungsi yang menerima masukan dua buah integer s dan r yang merepresentasikan arah
pergerakan dan besar perubahan arah yang akan dilakukan (dalam satu derajat). Fungsi mengembalikan arah
pergerakan yang baru dalam range 0 s.d. 359, setelah s diputar sebesar r. -}
-- Aplikasi