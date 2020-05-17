;--------------------------III блок задач------------------------------

; №1 Определите макрос, который возвращает свой вызов.

(defmacro is-self () ''(is-self))
(print (is-self))


; №3 Определите лисповскую форму (IF условие p q) в виде макроса.
(defmacro is-if (cond-a-b a b)
  `(if ,cond-a-b ,a ,b))

(print (is-if (= 10 5) 'True 'False))
(print (is-if (< 5 10) 'True 'False))
(print (is-if (> 5 10) 'True 'False))
