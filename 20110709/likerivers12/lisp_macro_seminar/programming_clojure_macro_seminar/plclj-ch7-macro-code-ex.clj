;;;; 7.2
(if (= 1 1) 
  (println "yep, math still works today"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 잘못된 unless
(defn unless [expr form]
  (if expr nil  form))

;;
(unless false
	(println "this should print"))

;;
(unless true
	(println "this should not print"))

;;;
(defmacro unless [expr form]
  (list 'if expr nil form))

;;
(unless false
	(println "this should print"))

;;
(unless true
	(println "this should not print"))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; unless 함수 분석
(unless true
	(println "Not"))

(comment

;; 함수 호출은 아래의 코드를 수행하는 것과 유사하다
(def expr-arg 'true)
(def form-arg '(println "Not"))

(let [expr (eval expr-arg)
      form (eval form-arg)]  ;; 이 때 "Not"이 출력된다.
  (if expr nil form))

)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; unless 매크로 분석
(unless true
	(println "Not"))

(def expr-arg 'true)
(def form-arg '(println "not"))

;;; 매크로 확장은 아래의 코드를 수행하는 것과 같다.
(let [expr expr-arg
      form form-arg]
  (list 'if expr nil form))

;;; 매크로 확장과 수행을 모두 수행하는 것이 아래와 같다.
(eval (let [expr expr
	    form form]
	(list 'if expr nil form)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macroexpand-1, macroexpand, C-c-<RET>
(macroexpand-1 '(unless ture (println "this should not print")))
(macroexpand '(unless ture (println "this should not print")))

(macroexpand-1 '(.. arm getHand getFinger))
(macroexpand '(.. arm getHand getFinger))

(macroexpand '(and 1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 여러 구문 수행 매크로

;; 확장
(when-not false 
  (println "this") 
  (println "and also this"))

;; error!
(defmacro when-not [test & body]
  (list 'if test nil (cons 'do body)))

;;
(defmacro unless [test & body]
  (list 'if test nil (cons 'do body)))

;; macroexpand-1
(unless false 
  (println "this") 
  (println "and also this"))

;; & body 인자
;; body |-> ((println "this") (println "and also this"))
;;->
(def body '((println "this") (println "and also this")))
;; (cons 'do body)
;; = (cons 'do ((println "this") (println "and also this")))
(cons 'do body)
;; -> (do ((println "this") (println "and also this")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chain macro
(defmacro chain
  ([x form] (list '. x form))
  ([x form & more] (concat (list 'chain (list '. x form)) more)))

(macroexpand '(chain arm getHand getFinger))

;;------------
(def x 'arm)
(def form 'getHand)
(def more '(getFinger))
(concat (list 'chain (list '. x form)) more)
;;------------


;;


(defmacro chain
  ([x form] `(. ~x ~form))
  ([x form & more] `(chain (. x form) ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 매크로 작성 도구
`foo#

(gensym 'foo)

'(a (+ 1 2) c)

(def *a* 1)
`~*a*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bench macro
(defmacro bench [expr]
  `(let [start (System/nanoTime)
	 result ~expr]
     {:result result :elapsed (- (System/nanoTime) start)}))

(bench (str "a" "b"))
(macroexpand-1 '(bench (str "a" "b")))



;;
(defmacro bench [expr]
  `(let [start# (System/nanoTime)
	 result# ~expr]
     {:result result# :elapsed (- (System/nanoTime) start#)}))

(bench (str "a" "b"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol Capture in Common Lisp
(defmacro bench (expr)
  `(let* ((start (get-universal-time))
     (result ,expr))
     (list 'result result 'elapsed (- (get-universal-time) start))))

(bench (* 3 4))

(let ((start 10)
      (end 20))
  (bench (+ start end)))

(macroexpand-1 '(let ((start 10)
      (end 20))
  (bench (+ start end))))


;;; variable capture 문제 피하기
(defmacro bench (expr)
  (let* ((start (gensym))
     (result (gensym)))
    `(let* ((,start (get-universal-time))
        (,result ,expr))
       (list 'result ,result 'elapsed (- (get-universal-time) ,start)))))


(let ((start 10)
      (end 20))
  (bench (+ start end)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

(chain (.. arm getHand))
(+ 1 2)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 조건부 평가

;; and
(defmacro my-and
  ([] true)
  ([x] x)
  ([x & rest]
     `(let [and# ~x]
	(if and# (and ~@rest) and#))))

(my-and 1 0 nil false)

;; comment
(comment

(my-and 1 0 nil false)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; var 생성 - 1
(def person1 (create-struct :first-name :last-name))

(defstruct person2 :first-name :last-name)

(defmacro my-defstruct [name & keys]
  `(def ~name (create-struct ~@keys)))

(my-defstruct person3 :first-name :last-name)
;; macroexpand-1




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; var 생성 - 2
(def a)
(def b)
(def c)

(declare a b c)
;; me-1

;;
(defmacro my-declare [& names]
  `(do ~@(map #(list 'def %) names)))

(my-declare a b c)
;; me-1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 자바와 상호작용

Math/PI

(Math/pow 10 3)

;; def 이용
(def PI Math/PI)

PI

(def pow [b e]
     (Math/pow b e))

(pow 10 3)

;; import-static 매크로 이용
(use '[clojure.contrib.import-static :only (import-static)])
(import-static java.lang.Math PI pow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 평가 지연
(def slow-calc (delay (Thread/sleep 5000) "done!"))

(delay (Thread/sleep 5000))

(force slow-calc)

(force slow-calc)

(def slow2 (delay "done!!!"))

(force slow2)

(def slow3 (do (Thread/sleep 5000) "done!!"))
slow3

(def slow4 '(do (Thread/sleep 5000) "done!!"))
slow4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 구문 감싸기
(with-out-str (print "hello, ") (print "world"))
;;me-1
(defmacro my-with-out-str [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       ~@body
       (str s#))))

(my-with-out-str (print "hello, ") (print "world"))

(defmacro my-with-out-str [& body]
  `(let [s# (new java.io.StringWriter)]
     (let [*out* s#]
       ~@body
       (str s#))))
  
(assert (= 1 1))

(assert (= 1 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 불필요한 lambda 피하기
(defn bench-fn [f]
  (let [start (System/nanoTime)
	result (f)]
    {:result result :elapsed (- (System/nanoTime) start)}))

(bench-fn (fn [] (+ 1 2)))

