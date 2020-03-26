;№ 28, Определите функцию, вычисляющую, сколько всего атомов в списке (списочной структуре).

(defun count-atom (lst)
    (if (null lst) 0
        (+ (if (atom (car lst)) 1 0) 
                (count-atom (cdr lst)))
    )
)

(print (count-atom  '((1 3) 2 3 1 (5 6) (2 3) (6))))