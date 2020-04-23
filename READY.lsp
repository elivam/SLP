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


;----------------------------2 БЛОК ЗАДАЧ-----------------------------------------
;----------------------------НЕЧЕТНЫЕ-----------------------------------
;1  Определите FUNCALL через функционал APPLY
;(funcall 'car '(1 2 3)) =>  1

(defun my-funcall (function &rest arg)
	(apply function arg)
)
	                   
(print(my-funcall 'car '(0 f 7 2 3)))
;-------------------------------------------------------------------------
;3 Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка
;(f1 f2 ... fn)
;к соответствующему элементу списка
;x = (x1 x2 ... xn)
;и возвращает список, сформированный из результатов.

(defun apl-apply (functions arg-x)
	( (lambda (head-f tail-f first-x tail-x)
				(cond
					((null head-f) nil)
					(t (append 
							(list (apply head-f (list first-x )))
							(apl-apply tail-f tail-x)
						)
					)
				)
		) (car functions) (cdr functions) (car arg-x) (cdr arg-x)
	)
)
(defun mult-two (x)
  (* x 2)
)
 
(print (apl-apply '(car cadr add-one) '((10 11 34) ( 17 78 89 ) 70 1 2)))
;-------------------------------------------------------------------------
;9 Напишите генератор порождения чисел Фибоначчи: 0, 1, 1, 2, 3, 5, ...

(defun make-fibonachi-numbers-counter ()
    (let ((f1 0) (f2 1))
        (lambda ()
            (setq v f1
                f1 (+ f2 f1)
                f2 v
             )
         )
    )
)

(setq fib (make-fibonachi-numbers-counter))

(print (funcall fib))
(print (funcall fib))
(print (funcall fib))
(print (funcall fib))
(print (funcall fib))
(print (funcall fib))
