;;; eval 정의
(defun my-eval (exp env)
  (cond 
        ;; (미리 정의된 상수 : Built-in constants)
        ((self-evaluating? exp) exp)


	;; (변수 : variables)
	((variable? exp) (lookup-variable-value exp env))


	;; (특별한 형태 : Special Forms)
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (my-eval (cond->if exp) env))


	;; (람다 요약 : Lambda abstraction)
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))


	;; (적용 : application)
	((application? exp)
	 (my-apply (my-eval (operator exp) env)
		(list-of-values (operands exp) env)))


	(t
	 (format t "Unknown expression type -- EVAL ~A" exp))))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; apply
(defun my-apply (procedure arguments)
  (cond 
        
        ((primitive-procedure? procedure)
	 (format t "<<my-apply : primitive-procedure>>~%")
	 (apply-primitive-procedure procedure arguments))

	
   
        ((compound-procedure? procedure)
	 (format t "<<my-apply : compound-procedure>>~%")
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(t
	 (format t "Unknown procedure type -- APPLY ~A" procedure))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 프로시저 인자 
;;; 프로시저에 건네줄 인자 리스트를 만들기 위해서 list-of-values를 쓴다.
;;; -> 모든 피연산자의 값을 구한 다음에 그 값들을 리스트로 묶어서 내놓는다.

(defun list-of-values (exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;(list-of-values '(1 2 3) '())
;(list-of-values '((+ 1 2) (* 1 2)) the-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 조건 식 
;;; eval-if는 지정된 환경에서 if 식의 술어를 계산한다.

(defun eval-if (exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 잇단식
;;; apply가 프로시저 몸에서 잇단식을 만나거나
;;; eval이 begin으로 묶은 잇단식을 처리해야 할 때 eval-sequence를 쓴다.

(defun eval-sequence (exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
	(t (my-eval (first-exp exps) env)
	   (eval-sequence (rest-exps exps) env))))

;;(eval-sequence (begin-actions '(begin (define a 1) (set! a 2)))
;;	       '())
;; ->
;;(eval-sequence '((define a 1) (set! a 2)) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 덮어쓰기와 정의
;;; 덮어쓸 값을 얻어낸 다음, 그 값과 변수를 set-variable-value!로 넘겨서 지정된 환경을 고쳐쓴다.

(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
		       (my-eval (assignment-value exp) env)
		       env)
  'ok)

;(assignment-variable '(set! a 1))
;(assignment-value '(set! a 1))

;in driver loop
;(set! a 1)

(defun eval-definition (exp env)
  (format t "eval-definition~%")
  (define-variable! (definition-variable exp)
      (my-eval (definition-value exp) env)
    env)
  'ok)

;(definition-variable '(define a 1))
;(definition-value '(define a 1))	  

;in driver loop
;(define b 2)



;;--------------------------------------------------------
;; * 곧바로 값을 구할 수 있는 것은 수와 글줄(문자열-string) 뿐 이다.
(defun self-evaluating? (exp)
  (cond ((numberp exp) t) ;true)
	((stringp exp) nil) ;true)
	(t nil))) ;false)))


;;--------------------------------------------------------
;; * 변수는 글자로 나타낸다.
(defun variable? (exp) (symbolp exp))


;;--------------------------------------------------------
;; * 따온 식은 (quote <text-of-quotation>) 꼴로 나타낸다.
(defun quoted? (exp)
  (tagged-list? exp 'quote))

(defun text-of-quotation (exp) (cadr exp))

(defun tagged-list? (exp tag)
  (if (consp exp)
      (eq (car exp) tag)
      nil))


;;--------------------------------------------------------
;; * 덮어쓰기는 (set! <var> <value>)꼴로 나타낸다.
(defun assignment? (exp)
  (tagged-list? exp 'set!))

(defun assignment-variable (exp) (cadr exp))

(defun assignment-value (exp) (caddr exp))


;;--------------------------------------------------------
;; * 정의는 다음 두 꼬러 가운데 하나다.
;; - (define <var> <value>)
;; - (define (<var> <parameter1> ... <parameter_n>) 
;;     <body>)
;;   위는 아래 식의 달콤한 문법이다.
;;   (define <var>
;;      (lambda (<parameter1> ... <parameter_n>)
;;        <body>))
(defun definition? (exp)
  (tagged-list? exp 'define))

(defun definition-variable (exp)
  (if (symbolp (cadr exp))
      (cadr exp)           ; 1) (define a 1)
      (caadr exp)))        ; 2) (define (f x) ... )

(defun definition-value (exp)
  (if (symbolp (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)  ; 매개변수 이름
		   (cddr exp))))


;;--------------------------------------------------------
;; * lambda 식은 lambda 글자로 시작하는 리스트다.
(defun lambda? (exp) (tagged-list? exp 'lambda))

(defun lambda-parameters (exp) (cadr exp))

(defun lambda-body (exp) (cddr exp))

(defun make-lambda (parameters body)
  (cons 'lambda (cons parameters body)))

;; in driver loop
;(lambda (x) (+ x 1))

;;-------
(defun null? (x)
  (eq nil x))
;;-------


;;--------------------------------------------------------
;; * 조건식은 if로 시작하여 술어, 결과 식, 다른 결과 식으로 구성된다.
;;   다른 결과 식이 없으면 그 값은 언제나 false
(defun if? (exp) (tagged-list? exp 'if))

(defun if-predicate (exp) (cadr exp))

(defun if-consequent (exp) (caddr exp))

(defun if-alternative (exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'nil)) ;'false))

(defun make-if (predicate consequent alternative)
  (list 'if predicate consequent alternative))


;;--------------------------------------------------------
;; * begin은 잇단식을 하나로 묶어낸다.
;; - 첫 식과 나머지 모든 식을 골라내는 고르개
(defun begin? (exp) (tagged-list? exp 'begin))

(defun begin-actions (exp) (cdr exp))

(defun last-exp? (seq) (null? (cdr seq)))

(defun first-exp (seq) (car seq))

(defun rest-exps (seq) (cdr seq))

;; sequence->exp 짜맞추개
(defun sequence->exp (seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(t (make-begin seq))))

(defun make-begin (seq) (cons 'begin seq))

;(begin? '(begin (define a 1) (set! a 2)))
;(begin-actions '(begin (define a 1) (set! a 2)))
;(sequence->exp '((define a 1) (set! a 2)))

;; * 프로시적 적용은 또 다른 합친 식이다.
;;   식의 car는 연산자, cdr는 피연산자
(defun application? (exp) (consp exp))
;; exp :=
;; ( (lambda (<parameters...>) <body>)
;;   (<arguments ...>) )


(defun operator (exp) (car exp))

(defun operands (exp) (cdr exp))

(defun no-operands? (ops) (null? ops))

(defun first-operand (ops) (car ops))

(defun rest-operands (ops) (cdr ops))

;; in driver-loop
;((lambda (x) (+ x 1)) 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 이끌어낸 식 (derived expression)
;; 따로 프로시저를 만들어 처리하지 않아도 다른 특별한 형태의 힘을 빌려 처리할 수 있는 것
;; : 계산 방법을 따로 만들어 주어야 할 특별한 형태가 줄어들기 때문에,
;;   실행기가 단순해진다.
;; (cond ((> x 0) x)
;;       ((= x 0) (display 'zero) 0)
;;       (else (- x)))
;; ;;=>
;; (if (> x 0)
;;     x
;;     (if (= x 0)
;; 	(begin (display 'zero)
;; 	       0)
;; 	(- x)))

;; cond 에 대한 ..

(defun cond? (exp) (tagged-list? exp 'cond))

(defun cond-clauses (exp) (cdr exp))

(defun cond-else-clause? (clause)
  (eq (cond-predicate clause) 'else))

(defun cond-predicate (clause) (car clause))

(defun cond-actions (clause) (cdr clause))

(defun cond->if (exp)
  (format t "cond->if~%")
  (expand-clauses (cond-clauses exp)))

(defun expand-clauses (clauses)
  (if (null? clauses)
      'nil    ; else 절이 없을 경우
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(format t "ELSE clause isn't last -- COND->IF ~A"
		       clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

;; in driver-loop
;; (cond (true (+ 1 2) (* 1 2))
;;       (else 4))
;; (cond (false (+ 1 2) (* 1 2))
;;       (else 4))



;;-----------------------------------------------------
;; and, or 정의하기

;; a) 특별한 형태로 정의
;; (and a b c)
(defun and? (exp) (tagged-list? exp 'and))

(defun rest-predicates (exp) (cdr exp))

(defun eval-and (exp env)
  (labels ((eval-and-inner (predicates env2)
	     (let ((a-predicates (car predicates)))
	       (let ((a-res (my-eval a-predicates env2)))
		 (if a-res
		     (if (null? (cdr predicates))
			 a-res
			 (eval-and-inner (rest-predicates predicates) env2))
		     nil)))))
    (eval-and-inner (rest-predicates exp) env)))


(defun eval-or (exp env)
  (labels ((eval-or-inner (predicates env2)
	     (let ((a-predicates (car predicates)))
	       (let ((a-res (my-eval a-predicates env2)))
		 (if a-res
		     a-res
		     (if (null (cdr predicates))
			 a-res
			 (eval-or-inner (rest-predicates predicates) env2)))))))
    (eval-or-inner (rest-predicates exp) env)))

; in driver loop
;(and 1 2 3)
;(and true 2 false)

; in driver loop
;(or 1 2 3)
;(or true 2 3)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 술어 검사하기
;;; false 아닌 값이 오면 모두 참이라고 본다.
(defun true? (x)
  (not (eq x t)))

(defun false? (x)
  (eq x nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 프로시저 표현
;;; 언어의 기본 원소를 처리할 때, 다음과 같은 프로시저를 쓸 수 있다고 가정한다.

;; * (apply-primitive-procedure <proc> <args>
;; : <proc>에 <arg>리스트에 들어 있는 인자 값을 건네주고 그 값을 구할 때 쓴다.

;; * (primitive-procedure? <proc>
;; : <proc>이 기본 프로시저인지 아닌지 물어보는데 쓴다.

;; 인자, 프로시저 몸, 환경을 받아 새로 합친 프로시저를 만드는 짜맞추개
(defun make-procedure (parameters body env)
  (list 'procedure parameters body env))

(defun compound-procedure? (p)
  (tagged-list? p 'procedure))

(defun procedure-parameters (p) (cadr p))

(defun procedure-body (p) (caddr p))

(defun procedure-environment (p) (cadddr p))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 환경 연산
;;; 환경 : 여러 변수 일람표를 한 줄로 이어놓은 것
;;; 변수 일람표 : 변수와 값을 한 쌍으로 묶어둔 표


;; * (look-variable-value <var> <env>
;; : <env>를 뒤져서 <var>와 짝을 이루는 값을 찾아서 내놓는다.

;; * (extend-environment <variables> <values> <base-env>)
;; : 새 환경을 내놓는다.

;; * (define-variable! <var> <value> <env>)
;; : <env>환경의 첫번째 일람표에 <var> <value> 정의 집어넣기

;; * (set-variable-value! <var> <value> <env>)
;; : 값 고치기

;;; 환경을 변수 일람표들의 리스트로 표현
;; (cons '((c) (3)) (cons '((b) (2)) '((a) (1)))))
(defun enclosing-environment (env) (cdr env))

(defun first-frame (env) (car env))

(defparameter the-empty-environment '())

;;; 변수 일람표는 두 리스트의 쌍으로 나타낸다.
;;; ((var1 var2 ...) val1 val2 ...)
(defun make-frame (variables values)
  (cons variables values))

(defun frame-variables (frame) (car frame))

(defun frame-values (frame) (cdr frame))

(defun add-binding-to-frame! (var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;-------
(defun set-car! (lst val)
  (setf (car lst) val))

(defun set-cdr! (lst val)
  (setf (cdr lst) val))
;-------

;;------
;; (make-frame 'a 1)  ; '(a . 1)

;; (setf frame1 (make-frame '(a b c) '(1 2 3)))
;; frame1
;;  ; '((a b c) 1 2 3)

;; (add-binding-to-frame! 'd 4 frame1)
;; frame1
;((d a b c) 4 1 2 3)
;;------


;; 환경 넓히기
;; - 변수 리스트와 값 리스트로 새 변수 일람표를 만든 다음에 이를 환경에 이어 붙임
(defun extend-environment (vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (format t "Too many arguments supplied ~A ~A" vars vals)
	  (format t "Too few arguments supplied ~A ~A" vars vals))))

;;------
;; (setf env1 (extend-environment '(a b) '(1 2) the-empty-environment))
;; env1
;; ;; '(((a b) 1 2))

;; (setf env2 (extend-environment '(c d) '(3 4) env1))
;; env2
;; ;; '(((c d) 3 4) ((a b) 1 2))

;; (enclosing-environment env2)
;; '(((a b) 1 2))
;;------

;; 지정된 환경에서 변수를 찾을 때
;; - 첫 번째 일람표의 변수 리스트...
;;   없으면 그 일람표를 둘러싼 환경
(defun lookup-variable-value (var env)
  (labels ((env-loop (env)
	     (labels ((scan (vars vals)
			(cond ((null? vars)
			       (env-loop (enclosing-environment env)))
			      ((eq var (car vars))
			       (car vals))
			      (t (scan (cdr vars) (cdr vals))))))
	       (if (eq env the-empty-environment)
		   (format t "Unbound variable ~A" var)
		   (let ((frame (first-frame env)))
		     (scan (frame-variables frame)
			   (frame-values frame)))))))
    (env-loop env)))

;; (lookup-variable-value 'a env2)


;; 정해진 환경에서 어떤 변수 값을 고쳐쓸 때
(defun set-variable-value! (var val env)
  (labels ((env-loop (env)
	     (labels ((scan (vars vals)
			(cond ((null? vars)
			       (env-loop (enclosing-environment env)))
			      ((eq var (car vars))
			       (set-car! vals val))
			      (t (scan (cdr vars) (cdr vals))))))
	       (if (eq env the-empty-environment)
		   (format t "Unbound variable -- SET! ~A" var)
		   (let ((frame (first-frame env)))
		     (scan (frame-variables frame)
			   (frame-values frame)))))))
    (env-loop env)))

;;;-----------

;; env2
;; (lookup-variable-value 'a env2)

;; (set-variable-value! 'a 5 env2)
;; env2
;; (lookup-variable-value 'a env2)

;; (set-variable-value! 'a 1 env2)
;; env2
;; (lookup-variable-value 'a env2)
;;;-----------


;; 변수를 정의할 때
(defun define-variable! (var val env)
  (format t "define-varialbe!~%")
  (let ((frame (first-frame env)))
    (labels ((scan (vars vals)
	       (cond ((null? vars)
		      (add-binding-to-frame! var val frame))
		     ((eq var (car vars))
		      (set-car! vals val))
		     (t (scan (cdr vars) (cdr vals))))))
      (scan (frame-variables frame)
	    (frame-values frame)))))
  
;; env2 ;(((c d) 3 4) ((a b) 1 2))
;; (define-variable! 'e 5 env2)
;; env2 ;(((e c d) 5 3 4) ((a b) 1 2))





;; 각 기본 프로시저 미리 환경
(defun setup-environment ()
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true t initial-env)
    (define-variable! 'false nil initial-env)
    initial-env))

(defun primitive-procedure? (proc)
  (tagged-list? proc 'primitive))

(defun primitive-implementation (proc) (cadr proc))


(defun primitive-procedure-names ()
  (mapcar #'car
       primitive-procedures))


(defun primitive-procedure-objects ()
  (mapcar #'(lambda (proc) (list 'primitive (cadr proc)))
	  primitive-procedures))
     
(defparameter primitive-procedures
  (list (list 'car #'car)
	(list 'cdr #'cdr)
	(list 'cons #'cons)
	(list 'null? #'null?)
	;;<추가되는 기본 원소들>
;;	(list 'define define)
	(list '+ #'+)
	(list '- #'-)
	(list '* #'*)
	(list '/ #'/)
	))


(defparameter the-global-environment (setup-environment))

(defun apply-primitive-procedure (proc args)
  (funcall apply-in-underlying-scheme
   (primitive-implementation proc) args))

;;;----------
;; (defun apply-in-underlying-scheme proc args)
;;   (apply proc args))
;;(define apply-in-underlying-scheme my-apply)
(defparameter apply-in-underlying-scheme #'apply)

;;-----------

;;;---------------------
;;; 드라이버 루프 만들기
(defparameter input-prompt ";;; M-Eval input:")
(defparameter output-prompt ";;; M-Eval value:")

;; (defun exit? (exp)
;;   (eq exp '(exit!)))

;; (defun driver-loop ()
;;   (prompt-for-input input-prompt)
;;   (let ((input (read)))
;;     (if (exit? input)
;; 	(progn
;; 	  (format t "Exit!~%")
;; 	  (return-from driver-loop))
;; 	(let ((output (my-eval input the-global-environment)))
;; 	  (announce-output output-prompt)
;; 	  (user-print output))))
;;   (driver-loop))

(defun driver-loop ()
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (my-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
  
(defun prompt-for-input (str)
  (format t "~%~%~A~%" str))

(defun announce-output (str)
  (format t "~%~A~%" str))

(defun user-print (object)
  (if (compound-procedure? object)
      (print (list 'compound-procedure
		   (procedure-parameters object)
		   (procedure-body object)
		   '<procedure-env>))
      (print object)))


;; 드라이버 루프 시작
(defparameter the-global-environment (setup-environment))

(driver-loop)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
