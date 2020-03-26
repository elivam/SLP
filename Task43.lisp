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