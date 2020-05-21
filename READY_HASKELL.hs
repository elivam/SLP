-- №2 Определите функцию, заменяющую в исходном списке все вхождения заданного значения другим.
repls list from to = map 
    (\x -> if x == from
     then to else x) list

main = do
print $ repls [3,10,13,26,33,26] 26 5
