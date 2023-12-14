;;;--------------------------------------------
(defun square (x)
  (* x x))

(square 3)



(dolist (x '(1 2 3))
  (print x))

(mapcar #'print '(1 2 3))



(defparameter x 1)
(when (> x 0)
  (print "positive"))


;;---------------------------------------
(with-open-file (out "tmp.txt"
		     :direction :output
		     :if-exists :supersede)
  (dotimes (x 5)
    (format out "123456~%")))



(with-open-file (in "tmp.txt"
		    :direction :input)
  (loop for line = (read-line in nil)
       while line
       do (format t "~A~%" line)))


























;;;-----------------------------------------------

(defmacro mac1 (x)
  `(+ ,x 1))

(mac1 (* 3 2))
;->
; 1) expansion
;    x |-> (* 3 2)
;    eval`(+ ,x 1)
;    => (+ (* 3 2) 1)
;
; 2) run
;    ev(+ (* 3 2) 1)
;    => 7


(defun f1 (x)
  (+ x 1))

(f1 (* 3 2))
;->
; run :
;  x |-> 6
;  eval(+ 6 1)
;  => 7














;;-------------------------------------------
;; (macroexpand-1 '(defmacro nil! (x) (setq ,x nil)))
;; (defmacro nil! (x) `(setq ,x nil))

;1)
(defmacro nil! (x)
  (list 'setf x nil))

(macro-function 'nil!)

(symbol-function 'nil!)

(nil! a)



;2)
(defun nil! (x)
  (list 'setf x nil))

(macro-function 'nil!)

(symbol-function 'nil!)



;3)
(defun nil! (x)
  (setf x nil))





;;---------------------
;; (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;   (SB-C::%DEFMACRO 'NIL!
;;                    #'(SB-INT:NAMED-LAMBDA (DEFMACRO NIL!)
;; 			 (#:WHOLE1084 #:ENVIRONMENT1085)
;; 		       (DECLARE (IGNORE #:ENVIRONMENT1085))
;; 		       (LET* ()
;; 			 (DECLARE
;; 			  (MUFFLE-CONDITIONS
;; 			   CODE-DELETION-NOTE))
;; 			 (LET ((#:ARGS1087
;; 				(CDR #:WHOLE1084)))
;; 			   (UNLESS
;; 			       (SB-INT:PROPER-LIST-OF-LENGTH-P
;; 				#:ARGS1087 1 1)
;; 			     (SB-KERNEL::ARG-COUNT-ERROR
;; 			      'DEFMACRO 'NIL! #:ARGS1087
;; 			      '(X) 1 1)))
;; 			 (LET* ((X (CAR (CDR #:WHOLE1084))))
;; 			   (BLOCK NIL!
;; 			     (LIST 'SETF X NIL)))))
;;                    '(X) NIL '(MACRO-FUNCTION NIL!)))
;; ;;--------------------------
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;; 	   (sb-c::%defmacro 'nil!
;; 			    #'(sb-int:named-lambda (defmacro nil!)
;; 				  (w e)
;; 				(declare (ignore e))
;; 				(let* ()
;; 				  (declare
;; 				   (muffle-conditions
;; 				    code-deletion-note))
;; 				  (let ((arg1 (cdr w)))
;; 				    (unless
;; 					(sb-int:proper-list-of-length-p
;; 					 arg1 1 1)
;; 				      (sb-kernel::arg-count-error
;; 				       'defmacro 'nil! arg1
;; 				       '(x) 1 1)))
;; 				  (let* ((x (car (cdr w))))
;; 				    (block nil!
;; 				      (list 'setf x nil)))))
;; 			    '(x) nil '(macro-function nil!)))
;;------------------------------
(defun f (a)
  (when (> a 1)
    2))






















;;--------------------------------------------------

(destructuring-bind (x y z) '(a b c)
  (list x y z))

(destructuring-bind (x (y) z) '(a (b) c)
  (list x y z))

(destructuring-bind (x (y) . z) '(a (b) c d)
  (list x y z))    
		     




















;;--------------------------------------------------
;; 문법 확장 - for
;; from ANSI CL
(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

(for x 0 9
  (princ x))

;; destructuring-bind 이용
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

(for (x 0 9)
  (princ x))






















;;------------------------------------------------
;; variable capture 발생
(defmacro bench (expr)
  `(let* ((start (get-universal-time))
	  (result ,expr))
     (list 'result result 'elapsed (- (get-universal-time) start))))


(bench (dotimes (x 10000000) (* 3 4)))

(let ((start 10)
      (end 20))
  (bench (+ start end)))
;;          ^^^^^
;;          입력된 표현에서의 start : 10

;;(let ((start 10)
;;      (end 20))
;;  (LET* ((START (GET-UNIVERSAL-TIME)) 
;;          ^^^^^
;;         (RESULT (+ START END)))
;;                    ^^^^^
;;                    원래 의도는 입력된 표현의 start -> (20)
;;                    캡쳐 : 매크로에서 정의한 START 가 사용됨. -> (시간)
;;    (LIST 'RESULT RESULT 'ELAPSED (- (GET-UNIVERSAL-TIME) START)))


;; variable capture 해결
(defmacro bench (expr)
  (let* ((start (gensym))
	 (result (gensym)))
    `(let* ((,start (get-universal-time))
	    (,result ,expr))
       (list 'result ,result 'elapsed (- (get-universal-time) ,start)))))
	 
(bench (dotimes (x 10000000) (* 3 4)))

(let ((start 10)
      (end 20))
  (bench (+ start end)))




















;;-------------------------------------------
;; anaphoric macro

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))


(aif 2
     (format t "~A is true~%" it)
     (format t "~A is false~%" it))


















;;-------------------------------------------
;;; primep
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))


(do-primes (p 0 19)
  (format t "~d " p))





;; 함수로 만들면
(defun do-primes-fun (s e fn)
  (do ((p (next-prime s) (next-prime (1+ p))))
      ((> p e))
    (funcall fn p)))

(do-primes-fun 0 19 #'(lambda (x) (format t "~d " x)))


























;;------------------------------------------------

;; from CLTL
(defmacro arithmetic-if (test neg-form zero-form pos-form)
  (let ((var (gensym)))
    `(let ((,var ,test))
       (cond ((< ,var 0) ,neg-form)
	     ((= ,var 0) ,zero-form)
	     (t ,pos-form)))))

(defparameter x 10)
(arithmetic-if (- x 4.0)
	       (- x)
	       (error "Strange zero")
	       x)


















;;--------------------------------------------------
(defstruct point
  x
  y)

(setf p (make-point :x 0 :y 0))

(point-x p)

(point-y p)














;;----------------------------------------------
(defparameter x 1)
(defparameter y 2)

(defmacro add (x y)
  (+ x y))

(add 1 2) ; 인자 평가안하고 몸체 수행 (+ 1 2)
;(add x y) ; 인자 평가안하고 몸체 수행 (+ x y)

(defun add (x y)
  (+ x y))

(add 1 2) ; 인자 평가하고 몸체 수행 (+ 1 2) : 숫자는 평가하면 숫자 그대로
(add x y) ; 인자 평가하고 몸체 수행 (+ 1 2) : x->1, y->2
;;(add 'x 'y) ;-> (+ 'x 'y)
