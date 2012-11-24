(ns church-encoding.work
  (:use [church-encoding.core]))

;;; Task is to implement arithmetic on Church numerals.
;;; Check this page: http://en.wikipedia.org/wiki/Church_encoding
;;; You can use utility function to-church-num and to-normal-num to convert normal to church and church to normal:
;;; Note that to-church-num returns function that takes 1 argument (f)
;;; and returns function that takes 1 argument (x) that calculates (f (f ... (f x)...))
;;; All functions in this task must 1 argument functions that return other functions.

;;; Example:

(def church-0 (to-church-num 0))    ; 0 in church numerals

(def church-5 (to-church-num 5))    ; 5 in church numerals

(defn print-star [x] (print "*") x) ; Takes 1 argument, prints a star and retuns argument without modification.

((church-5 print-star) nil)         ; Prints ***** to console

(to-normal-num church-5)            ; returns 5

(def church-2 (to-church-num 2))    ; we'll use it in examples later



;;; Implement + (plus) for church numerals.

(def plus (fn [a] (fn [b] (fn [f] (fn [x]
	((a f) ((b f) x)))))))

(to-normal-num ((plus church-2) church-2)) ; must return 4

(test-plus plus) ; test your solution
(println)


;;; Implement * (multiplication) for church numerals

(def mult (fn [a] (fn [b] (fn [f] (fn [x]
	((a (b f)) x))))))

(to-normal-num ((mult church-2) church-5)) ; must return 10

(test-mult mult) ; test your solution
(println)


;;; Implement ^ (pow function) for church numerals.

(def pow (fn [a] (fn [b] (b a))))

(to-normal-num ((pow church-2) church-5)) ; must return 32

(test-pow pow) ; test your solution
(println)


;;; Implement dec function for church numerals.

(def pair (fn [a] (fn [b] (fn [z]
	((z a) b)))))
(def cfirst (fn [a] (fn [b] a)))
(def csecond (fn [a] (fn [b] b)))
(def cinc (fn [a] (fn [f] (fn [x]
	((a f) (f x))))))
(def next-pair (fn [p]
	((pair (cinc (p cfirst))) (p cfirst))))

(def cdec (fn [n]
	(((n next-pair) ((pair church-0) church-0)) csecond)))

(to-normal-num (cdec church-5)) ; must return 4

(test-dec cdec) ; test your solution



;;;; Implement sum function. sum takes number n and returns sum of all numbers less or equals to n.
;;;; You'll need to use recursion here. For recursion you'll need lazy values.
;;;; You can use delay for that: http://clojuredocs.org/clojure_core/1.2.0/clojure.core/delay
;
;(def sum :YOUR_IMPLEMENTATION_HERE)
;
;(to-normal-num (sum church-2)) ; must return 3
;
;(test-sum sum)
;
;
;
;;;; Additional task.
;;;; Implement set of function to create/manipulate lists.
;;;; Your need to implement following functions:
;;;; empty? - checks if list is empty, returns true or false. see church booleans http://en.wikipedia.org/wiki/Church_encoding#Church_booleans
;;;; empty-list - used as "end" of the list.
;;;; head - returns head of a list
;;;; tail - returns tail of a list
;;;; cons - takes 2 arguments h and t, and creates a list such that (head (cons a b)) = a, (tail (cons a b)) = b
;;;;
;;;; Help: http://en.wikipedia.org/wiki/Church_encoding#List_encodings
;
;(def empty? :YOUR_IMPLEMENTATION_HERE)
;
;(def empty-list :YOUR_IMPLEMENTATION_HERE)
;
;(def head :YOUR_IMPLEMENTATION_HERE)
;
;(def tail :YOUR_IMPLEMENTATION_HERE)
;
;(def cons :YOUR_IMPLEMENTATION_HERE)
;
;(((empty? empty-list) true) false) ; must return true
;
;(head (cons "Hello" empty-list)) ; must return "Hello"
;
;(let [list (cons "Hello" empty-list)
;      t (tail list)]
;  ((empty? t) true) false) ; must return true
;
;(test-list {:empty? empty?
;            :empty-list empty-list
;            :head head
;            :tail tail
;            :cons cons}) ; test your solution
;
;
;
;