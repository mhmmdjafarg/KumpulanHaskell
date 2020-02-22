
-- Definisi dan SPesifikasi
isSortedDown :: [Int] -> Bool

{- isSortedDown l mengembalikan True apabila l terurut mengecil.
 Prekondisi: list tidak kosong
 Contoh: isSortedDown [8,5,2,4] = False; isSortedDown [5,2,0] = True -}

isOneElmt :: [Int] -> Bool
-- isOneElmt l true jika list of integer l hanya mempunyai satu elemen
-- REALISASI
isOneElmt l = (length l) == 1 

-- Realisasi
isSortedDown li = if (isOneElmt li) then True
				  else 
					if ((head li) > (head (tail li))) then (isSortedDown (tail li))
					else False


-- Realisasi
-- isSortedDown [li]
