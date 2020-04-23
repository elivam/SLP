;-------------------------1 БЛОК ЗАДАЧ-------------------------------------------
;-------------------------ВАРИАНТ IV-----------------------------------
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

;-------------------------------НОВЫЕ-------------------------------------------
;-----------------------------------------------------------------------------
;№ 17 Создайте предикат, порождающий всевозможные перестановки исходного множества.
(defun insert-elem-in-each-position (elem list)
	(cond
		((null list) (list elem))
		((atom list) (insert-elem-in-each-position elem (list list)))
		(t (cons (cons elem list)
			     (insert-elem-in-each-position-aux elem nil list)))
	)
)

(defun insert-elem-in-each-position-aux (elem list1 list2)
	(cond
		((null list2) nil)
		(t
			((lambda (a)
				(cons
					(append (car a) (list elem) (cadr a))
					((lambda (x)
						(insert-elem-in-each-position-aux elem
							(first x)
							(second x)))
					a))
			)
			((lambda (list1 list2)
				(list (append list1 (list (car list2))) (cdr list2)))
			list1 list2)))
		)
	)


(defun add-elem-for-each-permutation (elem perm-lst)
	(cond
		((null perm-lst) nil)
		(t (append
				(insert-elem-in-each-position elem (car perm-lst))
				(add-elem-for-each-permutation elem (cdr perm-lst))))
	)
)

(defun my-permutation (lst)
	(cond
		((null lst) nil)
		((null (cdr lst)) (list lst))
		(t (add-elem-for-each-permutation
			(car lst)
			(my-permutation (cdr lst))))
	)
)

(my-permutation '(1 2 3))
; ((1 2 3) (2 1 3) (2 3 1) (1 3 2) (3 1 2) (3 2 1))

;----------------------------2 БЛОК ЗАДАЧ-----------------------------------------
;----------------------------НЕЧЕТНЫЕ-----------------------------------
;1 Определите FUNCALL через функционал APPLY

