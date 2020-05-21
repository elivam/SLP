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

; №5 Определите в виде макроса форму (REPEAT e UNTIL p) паскалевского типа.

(defmacro repeat (e until p)
  `(cond 
     (,p (progn ,e (repeat ,e until ,p)))))

(setq symbol '(m a v i l e))
(repeat (print (pop symbol)) until symbol)

; №2 Определите макрос (POP стек), который читает из стека верхний элемент и меняет значение переменной стека.

(defmacro pop-stack (stack)
  `(prog1
     (setq top (car ,stack))
     (setq ,stack (cdr ,stack))))

(setq number `(1 2 3 4 5 6))
(setq symbol `(-s -y -m -b -o -l))

(print (pop-stack symbol))
(print (pop-stack number))

(print (pop-stack symbol))
(print (pop-stack number))

(print (pop-stack symbol))
(print (pop-stack number))

(print (pop-stack symbol))
(print (pop-stack number))

(print (pop-stack symbol))
(print (pop-stack number))
