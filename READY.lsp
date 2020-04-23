; ----------------------- ПЕРЕДЕЛАННЫЕ --------------------------------------------------------
;------------------------------------------------------------
; № 4 Определите функцию, порождающую по заданному натуральному числу N список, состоящий из натуральных чисел от 1 до N.
;------------------------- ПРИЧИНА : "(через append) "----------------------------
(defun natur(N)
   (cond ((< N 1) nil)
         (t (append (natur (- N 1)) (list N) ))
   )
)
 
;(print(natur 4))
(print(natur 10))

;------------------------------------------------------------
;№ 46 Предположим, что отец и мать некоторого лица, хранятся как значения соответствующих свойств у символа, обозначающего это лицо. 
;Напишите функцию (РОДИТЕЛИ x), которая возвращает в качестве значения родителей, и 
;предикат (СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае, если x1 и x2 — сестры или братья, родные или с одним общим родителем.

(defun mother(name-child mother)
    (setf (get name-child 'm) mother)
)
(defun father(name-child father)
    (setf (get name-child 'f) father)
)

(defun get-parent (x p)
    (list (get x p))
)

(defun parents (x)
    (append (get-parent x 'm)
        (append (get-parent x 'f) nil )
    )
)

(defun relatives (child1 child2)
    ((lambda (p1 p2)
       (cond ((equalp (car p1) (car p2)) t) ((equalp (cadr p1) (car p2)) t)  
           ((equalp (car p1) (cadr p2)) t) ((equalp (cadr p1) (cadr p2)) t)
           )   
        
    ) (parents child1) (parents child2)
    )
)    

;1 child
(mother 'Alim 'Asye )
(father 'Alim 'Han)
;2 child
(mother 'Ilim 'Asye)
(father 'Ilim 'Ali)
;3 child
(mother 'Amet 'Syrie)
(father 'Amet 'Ali)

(print (parents 'Alim))
(print (relatives 'Amet 'Ilim))


