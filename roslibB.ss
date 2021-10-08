(module rosalind racket/base
	(provide (all-defined-out))
	(require srfi/1)
	(require srfi/13)
	(require (only-in racket/port call-with-input-string))
	(require (only-in "roslibA.ss" nuc-count))
;;
;;my-min3
;;
;(displayln "roslib2 loaded")
(define (my-min3 n1 n2 n3)
  (if (and (<= n1 n2)(<= n1 n3))
      (values n1 1)
      (if (and (<= n2 n1)(<= n2 n3))
	  (values n2 2)
	  (values n3 3))))
;;
;; find-first
;;
(define (find-first pred lst)
  (if (null? lst)
      #f
      (if (pred (car lst))
	  (car lst)
	  (find-first pred (cdr lst)))))


(define (find-first-pos0 pred lst)
  (if (null? lst)
      -99999
      (if (pred (car lst))
	  0
	  (+ 1 (find-first-pos0 pred (cdr lst))))))

(define (find-first-pos pred lst)
  (let ((it (find-first-pos0 pred lst)))
    (if (>= it 0)
	it
	#f)))
;;
;;  ABCDE
;;   BCDEF  (when n=4)
;;


(define (overlap? str1 str2 n)
    (string= (string-take-right str1 n)
	     (string-take str2 n)))

(define (intersect set1 set2)
  (filter (lambda(x)(member x set2)) set1 ))

;set1��set2�Ɋ܂܂�邩
(define (set-include? set1 set2)
  (if
   (every (lambda(x)(member x set2)) set1)
   #t #f))
#|
;set1��set2�Ɋ܂܂�邩(multiset)
(define (multiset-include? set1 set2)
  (if (empty? set1)
      #t
      (if (empty? set2)
	  #f
	  (if (member (car set1) set2)
	      (multiset-include? (cdr set1)(delete-once (car set1) set2))
	      #f))))
|#

  
(define (non-trivial? lst)
  (let ((cnt (nuc-count lst)))
    (and (= 2 (length (filter (lambda(x)(not (zero? x))) cnt)))
	 (not (ormap (lambda(x)(= 1 x)) cnt)))))


(define (bin-count lst)
  (map (lambda(y)(count (lambda(x)(equal? y x)) lst))
       '(#\0 #\1)))

(define (non-trivial-char? lst)
  (let ((cnt (bin-count lst)))
    (= 2 (length (filter (lambda(x)(not (zero? x))) cnt)))))


(define (newick->sexpr str)
  (let ((exp (regexp-replace* #rx"," str " ")))
    (call-with-input-string (regexp-replace #rx";$" exp "")
			    (lambda(in) (read in)))
    ))

)

    
