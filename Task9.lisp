;№ 9, Определите функцию, разделяющую исходный список на два подсписка. В
;первый из них должны попасть элементы с нечетными номерами, во второй —
;элементы с четными номерами.

(defun str (ls)
                (cond ((null (car ls)) ls)
                    (t 
                        (setq qlst (str (cddr ls)))
                        (list
                            (cons (car ls) (car qlst))
                            (cons (cadr ls) (cadr qlst))
                        )
                    )
                )
)

(print (srt '(1 7 6 4 53 6 11 67)))
(print (srt '(h e l l o)))