-- Soal 1 
-- a. Konversi Suhu
{- Menerima mauskkan bilangan real dalam satuan Celcius dan karakter satuan bernilai 'R', 'F', 'K' (tidak memungkinkan karakter lain). Fungsi menghasilkan
konversi Celcius ke satuan lain sesuai kode

Rumus konversi suhu Celcius :
1. R = C * 4/5
2. K = C + 273.15
3. F = (C * 9/5) + 32
-}

--Definisi dan Spesifikasi
konversiSuhu ::  Float -> Char -> Float --karena real menggunakan float

konversiSuhu degree x | x == 'R' = degree * 4 / 5
                      | x == 'K' = degree + 273.15
                      | x == 'F' = (degree * 9/5) + 32


-- b. nextDetik 
{- Menerima masukkan 3 buah integer, masing masing merepresentasikan jam menit dan detik dengan 0 <= j <= 23; 0<=m<=59; dan 0<=d<=59 
dan menghasilkan dalam bentuk tuple (j1,m1,d1) dengan j1,m1,d1 adalah waktu satu detik kemudian
 -}

nextDetik :: Int -> Int -> Int -> (Int,Int,Int)

nextDetik j m d = if (d+1 /= 60) then (j,m,d+1)
                  else
                    if (m+1 /= 60) then (j,m+1,0)
                    else
                        if (j+1 /= 24) then (j+1,0,0)
                        else (0,0,0)



-- Soal no 2
{- Banyaknya faktor suatu bilangan dalam suatu selang

fungsi hitungFaktor menerima 3 buah integer positif, masukkan pertama adalah batas awal, masukkan kedua adalah batas akhir, dan masukkan ketiga adalah 
bilangan yang akan dicari faktornya (misal x). Fungsi in menghitung berapa banyak faktor dari x berada diantara a dan b (a dan b termasuk)
Contoh : hitungFaktor 1 20 31 : 3 (faktor dari 21 pada bilangan antara 1-20 adalah 1,3,7)
-}

--Definisi dan Spesifikasi
isFaktor :: Int -> Int -> Bool
hitungFaktor :: Int -> Int -> Int -> Int
--Menerima 2 bilangan y z, dan memeriksa apakah z faktor dari y

isFaktor y z = mod y z == 0

{- hitungFaktor a b x = if b < a then 0
                     else
                         if (isFaktor x b) then (hitungFaktor a (b-1) x) + 1
                         else 
                             hitungFaktor a (b-1) x -}

hitungFaktor a b x = if b < a then 0
                     else
                        let n = 0
                        in
                            (if (isFaktor x b) then n+1 else n) + (hitungFaktor a (b-1) x)