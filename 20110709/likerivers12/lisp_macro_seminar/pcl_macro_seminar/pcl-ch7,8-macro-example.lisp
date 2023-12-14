;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 매크로 작성 도구

'(a (+ 1 2) c)
`(a ,(+ 1 2) c)
`(a (list 1 2) c)
`(a ,(list 1 2) c)
`(a ,@(list 1 2) c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 간단한 매크로
;;; 
(defmacro nil! (var)
  (list 'setf var nil))

(nil! a)

;;; 함수로 구현하면?? - 1
(defun nil! (var)
  (list 'setf var nil))

;;; 함수로 구현하면?? - 2
(defun nil! (var)
  (setf var nil))

;;; 
(nil! b)
(setf b 1)
(nil! b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PCL - p33
(defmacro backwards (expr)
  (reverse expr))

(backwards ("hello, world" t format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 매크로는 문법을 바꿀 수 있다.
(defmacro in-fix (a op b)
  `(funcall #',op ,a ,b))

(macroexpand-1 '(in-fix 1 + 2))

(in-fix 1 + 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if + progn 
;;; -> when
(defparameter *n* 0)

(if (> *n* 0)
    (progn
      (format t "using if+progn~%")
      (format t "greater than 0~%")
      (setf *n* (+ *n* 1))
      (format t "n=[~A]~%~%" *n*)))

(when (> *n* 0)
  (format t "using when~%")
  (format t "greater than 0~%")
  (setf *n* (+ *n* 1))
  (format t "n=[~A]~%~%" *n*))

;;; when 을 만들어보자
(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))

(my-when (> *n* 0)
	 (format t "using my-when~%")
	 (format t "greater than 0~%")
	 (setf *n* (+ *n* 1))
	 (format t "n=[~A]~%~%" *n*))

;;; unless를 만들어보자
(defmacro my-unless (condition &rest body)
  `(if (not ,condition) 
       (progn ,@body)))

(my-unless (> *n* 0)
	 (format t "using my-unless~%")
	 (format t "not greter than 0~%")
	 (setf *n* (+ *n* 1))
	 (format t "n=[~A]~%~%" *n*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COND

(cond ((> *n* 0) (format t "greater than 0~%"))
      ((= *n* 0) (format t "equal to 0~%"))
      (t (format t "less than 0~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOLIST
(dolist (x '(1 2 3))
  (print x))

(dolist (x '(1 2 3))
  (print x)
  (if (evenp x)
      (return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOTIMES
(dotimes (i 4)
  (print i))

(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DO
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DO - end test form was omitted
(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))

(do ((i 0 (1+ i)))
    ((>= i 4) 5)
  (print i))

;;;-> dotimes version
(dotimes (i 4) 
  (print i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOOP
(loop
   (when (> (get-universal-time) 3467884800)
     (return))
   (format t "Waiting ...~%")
   (sleep 1))

;;; collects the numbers from 1 to 10
;;; do-version
(do ((nums nil)
     (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums))
;;; loop-version
(loop for i from 1 to 10 collecting i)

(loop for x from 1 to 10 summing (expt x 2))

(loop for x across "the quick brown fox jumps over the lazy dog"
     counting (find x "aeiou"))

(loop for i below 10
     and a = 0 then b
     and b = 1 then (+ b a)
     finally (return a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; primep
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;;; 1-1 호출 
(do-primes (p 0 19)
  (format t "~d " p))

;;; 1-2 확장된 형태를 만들
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))

;;; 2 DEFMACRO를 이용한 매크로 정의
;;; v1
(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))
	(start (second var-and-range))
	(end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
	 ((> ,var ,end))
       ,@body)))

;;; destructuring bind
(destructuring-bind (x y z) '(a b c)
  (list x y z))

(destructuring-bind (x (y) z) '(a (b) c)
  (list x y z))

(destructuring-bind (x (y) . z) '(a (b) c d)
  (list x y z))


;;; v2 - destructuring parameter, &body
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 매크로가 없다면 어떤 방법으로 가능할까?
(defun print-primes-fun (s e)
  (do ((p (next-prime s) (next-prime (1+ p))))
      ((> p e))
    (format t "~d " p)))

;;;
(print-primes-fun 0 19)

(defun do-primes-fun (s e fn)
  (do ((p (next-prime s) (next-prime (1+ p))))
      ((> p e))
    (funcall fn p)))

;;;

(do-primes-fun 0 19 #'(lambda (x) (format t "~d " x)))
;;;(funcall #'(lambda (x) (format t "~d " x)) 222)

(defun primes-body (x)
  (format t "~d " x))
;;;(funcall #'primes-body 22)
(do-primes-fun 0 19 #'primes-body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 매크로 확장 확인하기
(macroexpand-1 '(nil! a))
(macroexpand '(nil! a))

;;; 두번 확장
(macroexpand-1 (macroexpand-1 '(nil! a)))

(macroexpand '(dolist (i '(1 2 3)) (format t "~d " i)))
(macroexpand '(do-primes (p 1 19) (format t "~d " p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 새는 틈 - 여러 번 평가 수행
(macroexpand-1 
 `(do-primes (p 0 (random 100))
    (format t "~d " p)))

(do-primes (p 0 (random 100))
  (format t "~d " p))

;;;;;;
(setf n 16)

(macroexpand-1
 `(do-primes (p 0 (incf n))
    (format t "~d " p)))

(do-primes (p 0 (incf n))
  (format t "~d " p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 여러 번 평가 수행 해결
;;; v3
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	(ending-value ,end))
       ((> ,var ending-value))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 새는 틈 - 변수 낚아 채기 문제

(do-primes (ending-value 0 19)
  (format t "~d " ending-value))

(macroexpand-1 
 `(do-primes (ending-value 0 19)
  (format t "~d " ending-value)))

;;;;;;
(let ((ending-value 0))
  (do-primes (p 0 10)
    (incf ending-value p))
  ending-value)

(macroexpand-1 
 `(let ((ending-value 0))
    (do-primes (p 0 10)
      (incf ending-value p))
    ending-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 변수 낚아채기 문제 해결
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))

(macroexpand-1 '(do-primes (ending-value-name 0 19)
  (format t "~d " ending-value-name)))

(do-primes (ending-value-name 0 19)
  (format t "~d " ending-value-name))

;;;;;;
(let ((ending-value-name 0))
  (do-primes (p 0 10)
    (incf ending-value-name p))
  ending-value-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 매크로를 작성하는 매크로
(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(do-primes (ending-value-name 0 19) (format t "~d " ending-value-name))
(macroexpand-1 '(do-primes (ending-value-name 0 19) (format t "~d " ending-value-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-gensyms 분석
(setf names '(a b c))
(loop for n in names collect `(,n (gensym)))

(macroexpand-1 '(with-gensyms (a b c) 'body))
(with-gensyms (a b c) 'body)

;;;
(defmacro test-macro (a b)
  `(,(list a `(,b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "`", ",", ",@" 가 없다면
(defmacro do-primes-a ((var start end) &body body)
  (append '(do)
	  (list (list (list var
			    (list 'next-prime start)
			    (list 'next-prime (list '1+ var)))))
	  (list (list (list '> var end)))
	  body))

(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))

(do-primes (p 0 19)
  (format t "~d " p))

