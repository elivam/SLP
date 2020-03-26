;№ 28, Определите функцию, вычисляющую, сколько всего атомов в списке (списочной структуре).

(defun CountAtom (lst)
    (if (null lst) 0
        (+ (if (atom (car lst)) 1 0) 
                (CountAtom (cdr lst)))
    )
)

(print (CountAtom  '((1 3) 2 3 1 (5 6) (2 3) (6))))