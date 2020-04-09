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

;  ПЕРЕДЕЛАННЫЕ 
;----------------------------------------------------------
; № 3 Определите функцию, заменяющую в исходном списке все вхождения заданного значения другим
; х - замена, repEl - заменяемое значение в списке, list - список

(defun replac (x repEl list) 
   ((lambda (el-cdr)
   ((lambda (el-car) 
   (cond ((null list) nil)
         ((equal el-car repEl) (cons x (replac x repEl el-cdr)))
         ((cons el-car (replac x repEl el-cdr)))
    ) ) (car list) ) ) (cdr list))
    
)
 
(print(replac 6 1 '(1 2 3 4 5 6 6)))
;------------------------------------------------------------
; № 4 Определите функцию, порождающую по заданному натуральному числу N список, состоящий из натуральных чисел от 1 до N.
(defun reverse-el (lst)
  (cond ((null lst) nil)
     ((reverse lst))
  )        
)

(defun natur(N)
   (cond ((< N 1) nil)
         (t (cons N (natur (- N 1))))
   )
)
 
;(print(natur 4))
(print(reverse-el (natur  10)))

;------------------------------------------------------
;№ 9, Определите функцию, разделяющую исходный список на два подсписка. В
;первый из них должны попасть элементы с нечетными номерами, во второй —
;элементы с четными номерами.

(defun str (ls)
               ((lambda (el-cddr) 
                (cond ((null (car ls)) ls)
                    (t 
                        (list
                            (cons (car ls) (car(str el-cddr)))
                            (cons (cadr ls) (car(str el-cddr)))
                        )
                    )
                ) ) (cddr ls) )
)

(print (str '(1 7 6 4 53 6 11 67)))
(print (str '(h e l l o)))

;           НОВЫЕ
;------------------------------------------------------
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


;------------------------------------------------------
;№ 45 Предположим, что у имени города есть свойства х и у, 
;которые содержат координаты места нахождения города относительно некоторого начала координат.
;Напишите функцию (РАССТОЯНИЕ a b), вычисляющую расстояние между городами а и b.
;Расстояние как между двумя координатами на плоскости

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

(setf (get 'Simferopol 'x) 10)
(setf (get 'Simferopol 'y) 20)
(setf (get 'Kerch 'x) 230)
(setf (get 'Kerch 'y) 50)

(print (distance-between-cities 'Simferopol 'Kerch))
;222.03603 
