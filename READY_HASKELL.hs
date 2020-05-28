-- №2 Определите функцию, заменяющую в исходном списке все вхождения заданного значения другим.
repls list from to = map 
    (\x -> if x == from
     then to else x) list


-- № 22 Определите функцию ОБЪЕДИНЕНИЕ, формирующую объединение двух множеств

unification l1 l2 = l1 ++ l2


-- № 23 Определите функцию СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ, формирующую множество из
-- элементов входящих в оба множества
contain [] x = False
contain (x1:xs) x = if x1 == x then True else contain xs x

intersect l1 l2 = filter  (\x -> contain l1 x) l2


-- № 15 Определите функцию (ПЕРВЫЙ-СОВПАДАюЩИЙ х у), которая возвращает первый
--элемент, входящий в оба списка х и у, в противном случае NIL.
eqNN :: Integral a => [a] -> [a] -> a

eqNN x y = if head x == head y then head x else 0



main = do
print $ repls [3,10,13,26,33,26] 26 5
print $ unification [1, 2, 3, 5] [5, 3, 10]
print $ intersect [1, 4, 5] [5, 3, 7]
print $ eqNN [9, 1, 3] [9, 1, 5, 3]
