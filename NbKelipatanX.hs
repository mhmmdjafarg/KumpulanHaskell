-- Nama : Muhammad Jafar
-- Nim : 16519070
-- NbKelipatanX
-- Tanggal : 19 Februari 2020

module NbKelipatanX where

--definisi dan Spesifikasi
nbKelipatanX :: Int -> Int -> Int -> Int	-- nbKelipatanX m n x

--Realisasi
nbKelipatanX m n x = if m == n then -- basis
						if mod n x == 0 then 1
						else 0
					 else
						if mod n x == 0 then 1 + nbKelipatanX m (n-1) x
						else nbKelipatanX m (n-1) x
						
