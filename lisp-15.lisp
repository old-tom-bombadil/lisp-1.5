
; lisp implementation as documented in Lisp 1.5 Programmers Manual

(defun tww-pairlis (x y a)
  "itemwise pair the first two lists and cons onto the third"
  (cond ((null x) a)
	(T (cons (cons (car x) (car y)) (tww-pairlis (cdr x ) (cdr y) a)))))

(defun tww-assoc (x a)
  "find the pair in a starting with x"
  (cond ((equal (caar a) x) (car a))
	(T (tww-assoc x (cdr a)))))

(defun tww-evcon (c a)
  (cond ((tww-eval (caar c) a)  (tww-eval (cadar c) a))
	(T (tww-evcon (cdr c) a))))


(defun tww-eval (e a)
  (cond ((atom e) (cdr (assoc e a))) ;atoms evaluate to their environment value
	((atom (car e))
	 (cond ((eq (car e) 'QUOTE) (cadr e))
	       ((eq (car e) 'COND) (tww-evcon (cdr e) a))
	       (T (tww-apply (car e) (tww-evlis (cdr e) a) a))))
	(T (tww-apply (car e) (tww-evlis (cdr e) a) a))))

(defun tww-apply (fn x a)
  (cond ((atom fn)
	 (cond ((eq fn 'CAR) (caar x))
	       ((eq fn 'CDR) (cdar x))
	       ((eq fn 'CONS) (cons (car x) (cadr x)))
	       ((eq fn 'ATOM ) (atom (car x)))
	       ((eq fn 'EQ) (eq (car x) (cadr x)))
	   (T (tww-apply (tww-eval fn a) x a))))
	((eq (car fn) 'LAMBDA) (tww-eval (caddr fn) (tww-pairlis (cadr fn) x a)))
	 ((eq (car fn) 'LABEL) (tww-apply (caddr fn) x (cons (cons (cadr fn) (caddr fn)) a)))))

(defun tww-evlis (m a)
  (cond ((null m) NIL)
	(T (cons (tww-eval (car m) a) (tww-evlis (cdr m) a)))))

(defun evalquote (fn x) (tww-apply fn x nil))


;;(evalquote '(LAMBDA (x) (COND ((ATOM X) (QUOTE BLAH)) ((QUOTE T) (CAR x))))  '(14))
;; (evalquote '(LAMBDA (x) (COND ((ATOM X) (QUOTE BLAH)) ((QUOTE T) (CAR x))))  '((1 2 3)))
