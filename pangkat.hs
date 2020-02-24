{- Fungsi pangkat menerima masukan berupa dua buah integer, a dan b, dan mengembalikan hasil berupa a^b (a
pangkat b). Masukan diasumsikan selalu valid, yaitu a > 0 dan b ≥ 0.
Buatlah definisi, spesifikasi, realisasi dengan menggunakan ekspresi rekursif dan contoh aplikasi berikut hasilnya
(minimum 2 buah) dari fungsi pangkat. -}

pangkat :: Int -> Int -> Int

pangkat a b = if b == 0 then 1
              else a*(pangkat a (b-1))

-- aplikasi
-- pangkat 2 3 
-- output = 8

