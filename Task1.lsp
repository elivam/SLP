; № 1 Запишите последовательность вызовов CAR и CDR выделяющие из приведенных ниже списков символ цель. Упростите эти вызовы с помощью комбинации
;селекторов:
;• (1 2 цель 3 4)
;• ((1) (2 цель) (3 (4)))
;• ((1 (2 (3 4 цель))))

(print (caddr '(1 2 цель 3 4)))

(print (cadadr '( (1) (2 цель) (3 (4)) )))

(print (caddar (cdadar '((1(2 (3 4 цель)))))))


;------------------------------------------------------
; № 4 Определите функцию, порождающую по заданному натуральному числу N список, состоящий из натуральных чисел от 1 до N.

(defun reverse-el (w &optional acc)
  (cond ((null w) acc)
        ((reverse-el (cdr w) (cons (car w) acc)))
   )
)
 
(defun natur(N)
   (cond ((< N 1) nil)
         (t (cons N (natur (- N 1))))
   )
)
 
;(print(natur 4))
(print(reverse-el (natur 10)))


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


