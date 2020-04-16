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

(print "Подскажите какие функции надо переписать или дописать. Задача 17. (Уже точно не помню. На лекции говорили, но я уже не помню
       какие точно)")



; -------------- СДАНЫ--------------------------
; № 1 Запишите последовательность вызовов CAR и CDR выделяющие из приведенных ниже списков символ цель. Упростите эти вызовы с помощью комбинации
;селекторов:
;• (1 2 цель 3 4)
;• ((1) (2 цель) (3 (4)))
;• ((1 (2 (3 4 цель))))

(print (caddr '(1 2 цель 3 4)))

(print (cadadr '( (1) (2 цель) (3 (4)) )))

(print (caddar (cdadar '((1(2 (3 4 цель)))))))

;------------------------------------------------------
;№ 20, Определите функцию ПЕРВЫЙ-АТОМ, результатом которой будет первый атом
;списка. Пример:
;> (ПЕРВЫЙ-АТОМ ’(((a b)) c d))
;A

(defun first-atom(lst)
   (cond ((atom (car lst)) (car lst))
          (t (first-atom(car lst)))
   )    
)

(print (first-atom '(((a b)) c d)))
(print (first-atom '((((a b) b)) c d)))
;------------------------------------------------------
;№ 28, Определите функцию, вычисляющую, сколько всего атомов в списке (списочной структуре).

(defun count-atom (lst)
    (if (null lst) 0
        (+ (if (atom (car lst)) 1 0) 
                (count-atom (cdr lst)))
    )
)

(print (count-atom  '((1 3) 2 3 1 (5 6) (2 3) (6))))

;------------------------------------------------------
;№ 43, Определите функцию, подсчитывающую количество всех вершин данного дерева заданной высоты.
; zerop(x) - функция возв TRUE , если х = 0.0 или 0 ; иначе nil 
; level - уровень дерева(бинарное)

(defun tree-el-count (tree level)
  (cond ((null tree) 0) ((zerop level) 1)
        (t (+ (tree-el-count (car tree) (- level 1))
              (tree-el-count (caddr tree) (- level 1))))
   )
)

(print( tree-el-count '(((nil 2 nil) 6 (nil -8 nil)) 7 (nil 12 nil)) 0))

;------------------------------------------------------------
;№ 40 Определите функцию РАЗНОСТЬ, формирующую разность двух множеств, т.е.
;удаляющую из первого множества все общие со вторым множеством элементы

(defun delel (lst el)
    ( 
        (lambda (el-car el-cdr)
            (cond ((null lst) NIL)
                ((equalp el-car el) el-cdr)
                (t (cons el-car (delel el-cdr el)))
            )   
        )
        (car lst) (cdr lst)
    )
)
 
(defun diff (minus deductible)
    (
        (lambda (el-car el-cdr)
            (cond
                ((null deductible) minus)
                (t (diff (delel minus el-car) el-cdr))
            )
        )
        (car deductible) (cdr deductible)
    )
)

(print (diff '(1 100 87 4) '(1 4)))
(print (diff '(34 5 67 6 1 4) '(1 34 3 4)))
(print (diff '(1 2 3 10) '(6 99 8)))
(print (diff '(h 2 3 10) '(6 j h 3)))
(print (diff '(Hello world) '(world)))

;------------------------------------------------------------
; № 3 Определите функцию, заменяющую в исходном списке все вхождения заданного значения другим
; х - замена, repEl - заменяемое значение в списке, list - список
;------------------------- ПРИЧИНА : "(зачем два лямбда-выражения?)"----------------------------

(defun replac (x repEl lst) 
   (
    (lambda (el-car el-cdr) 
      (cond ((null lst) nil)
         ((equal el-car repEl) (cons x (replac x repEl el-cdr)))
         ((cons el-car (replac x repEl el-cdr)))
      ) 
    ) (car lst) (cdr lst)
   ) 
)
 
(print(replac 90 1 '(1 2 3 4 5 6 1 1)))

;-----------------------------------------------------------------------------
;№ 9, Определите функцию, разделяющую исходный список на два подсписка. В
;первый из них должны попасть элементы с нечетными номерами, во второй —
;элементы с четными номерами.
;;------------------------- ПРИЧИНА : "(Повторяющиеся конструкции, что здесь значит str?)" --------

(defun division (ls)
    ( 
       (lambda (el-cddr el-car) 
                (cond ((null el-car) ls)
                    (t 
                        (list
                            (cons el-car (car  (division el-cddr)))
                            (cons (cadr ls) (cadr  (division el-cddr)))
                        )
                    )
                ))
         (cddr ls) (car ls) 
     )
)
(print (division '(1 7 6 4 53 6 11 67)))
(print (division '(m e a l v i i v l a e m)))

;------------------------------------------------------
;№ 45 Предположим, что у имени города есть свойства х и у, 
;которые содержат координаты места нахождения города относительно некоторого начала координат.
;Напишите функцию (РАССТОЯНИЕ a b), вычисляющую расстояние между городами а и b.
;Расстояние как между двумя координатами на плоскости
;;------------------------- ПРИЧИНА : "(добавить "удобные" функции, например, set-city)"-----------------------------
(defun dist(x1 y1 x2 y2)
  (sqrt (+  (expt (- x1 x2) 2) (expt (- y1 y2) 2) ))
)
(defun distance-between-cities (town1 town2)
    (
        (lambda (town1x town1y town2x town2y) 
            (dist town1x town1y town2x town2y)
        )
     
        (get town1 'x)
        (get town1 'y)
        (get town2 'x)
        (get town2 'y)
    )
)
(defun set-town (city_name coord_x coord_y)
        (setf (get city_name 'x) coord_x)
        (setf (get city_name 'y) coord_y)
)

(set-town  'Simferopol 10 20)
(set-town  'Kerch 235 50)

(print (distance-between-cities 'Simferopol 'Kerch))
;226.9912

