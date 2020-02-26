countEven :: [Int] -> Int

countEven li = if (null li) then 0
               else
                   if mod (head li) 2 == 0 then 1 + countEven (tail li)
                   else countEven (tail li)

                  
maxNb :: [Int] -> (Int,Int)

maxNb li = if length li == 1 then ((head li),1)
           else
               let (m,n) = maxNb (tail li)
               in
                   if (head li) > m then ((head li),1)
                   else if (head li) < m then (m,n)
                   else (m,n+1)

                 


sumList :: [Int] -> Int
sumList l = if length l == 1 then (head l)
            else (head l) + sumList (tail l)

             


filterGanjil :: [Int] -> [Int]

filterGanjil l = if null l then l
                 else if (mod (head l) 2 == 1) then [head l] ++ (filterGanjil (tail l))
                 else (filterGanjil (tail l))


delN :: Int -> [Int] -> [Int]
delN x l = if x == 1 then (tail l)
           else [head l] ++ (delN (x-1) (tail l))
          

iseqFront :: [Int] -> [Int] -> Bool

iseqFront t1 t2 = if length t1 == 1 then
                     if (head t1) == (head t2) then True
                     else False
                  else iseqFront (tail t1) (tail t2)
                     


isOrdered :: [Int] -> Bool
isOrdered l = if length l == 1 then True
              else if (head l) < (head (tail l)) then isOrdered (tail l)
              else False
            

offset :: [Int] -> (Int -> Int) -> [Int]


offset l f = if length l == 1 then [f(head l)]
             else [f(head l)] ++ (offset (tail l) f)

               

isAllGanjil :: [Int] -> Bool
isAllGanjil l = if null l then True
                else if mod (head l) 2 == 1 then isAllGanjil (tail l)
                else False


getSmallest :: [Int] -> Int

getSmallest l = if length l == 1 then (head l)
                else
                    let m = getSmallest (tail l)
                    in
                        if head l > m then m
                        else (head l)


delEl :: Int -> [Int] -> [Int]

delEl x l = if null l then []
            else
                if (head l) /= x then [head l] ++ (delEl x (tail l))
                else delEl x (tail l)


sortList :: [Int] -> [Int]

sortList l = let min = getSmallest l
             in let l1 = delEl min l
                in if length l == 1 then l
                   else [min] ++ (sortList l1)


offsetlist :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> [Int]

offsetlist f g a b = if b < a then []
                     else [f a] ++ (offsetlist f g (g a) b)

nilaiEkstrim :: [Int] -> (Int,Int)

nilaiEkstrim l = if length l == 1 then ((head l),(head l))
                 else let (min,max) = nilaiEkstrim (tail l)
                      in
                          if (head l) < min then ((head l), max)
                          else if (head l) > max then (min, head l)
                          else (min,max)

trunc :: [Int] -> Int -> [Int]
trunc l i = if i == 1 then [head l]
            else [head l] ++ (trunc (tail l) (i-1))

splitList :: [Int] -> ([Int],[Int])

splitList l = if length l == 1 then (l,[])
              else let (lfront,lback) = splitList (tail (init l))
                   in
                       (([head l] ++ lfront), (lback) ++ [last l])

insSorted :: Int -> [Int] -> [Int]
insSorted x l = if null l then [x] ++ []
                else if (head l) < x then [head l] ++ (insSorted x (tail l))
                else [x] ++ l

sumInteger :: Int -> Int -> (Int -> Bool) -> Int
sumInteger a b f = if b < a then 0
                   else
                       if (f a) then a + sumInteger (a+1) b f
                       else sumInteger (a+1) b f

isSortedDown :: [Int] -> Bool
isSortedDown l = if length l == 1 then True
                 else if (head l) > head (tail l) then isSortedDown (tail l)
                 else False

getElTengah :: [Int] -> Int
getElTengah l = if length l == 1 then (head l)
                else if null l then 0
                else
                    if length (tail l) == 1 then (head l)
                    else getElTengah (tail(init l))

elPosGanjil :: [Int] -> [Int]
elPosGanjil l = if null l then l
                else if length l == 1 then l
                else
                    [head l] ++ (elPosGanjil (tail (tail l)))

delAllX :: [Int] -> Int -> ([Int],Int)
delAllX l x = if null l then ([],0)
              else let (m,n) = delAllX (tail l) x
                   in
                       if (head l) /= x then ([head l] ++ m,n)
                       else (m,n+1)


splitListIF :: [Int] -> (Int->Bool) -> ([Int], [Int])
splitListIF l f = if null l then ([],[])
                  else
                      let (l1,l2) = splitListIF (tail l) f
                      in
                          if f(head l) then ([head l] ++ l1,l2)
                          else (l1,[head l] ++ l2)
