#lang racket
;; rosalind
;; Completing a Tree
;; [TREE] 2021/10/09
;; 2021/10/15 AC
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
(require "roslibB.ss")
(define *tree_out* "data\\tree_out.txt")
(define *data* #f)

(define (ros_tree . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_tree.txt"
		    (format "data\\rs_tree~a.txt" (car n)))))
	 (n (string->number (car data)))
	 (pairs (map string-tokenize (cdr data)))
	 (adjlist (map (lambda(x)(map string->number x)) pairs))
	 (grouplist '())
	 )
    ;;(set! *data* adjlist)
    (set! grouplist (merge-all adjlist))
    (+ (- (length grouplist) 1)(- n (length (flatten grouplist))))
    #|
    (call-with-output-file *tree_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

(define (merge-all listlist)
  (if (null? listlist)
      '()
      (merge-pair (merge-all (cdr listlist))(car listlist))))


(define (merge-pair listlist pair)
  (let ((set1 (included-set listlist (car pair)))
	(set2 (included-set listlist (cadr pair))))
    (if (and (not set1)(not set2))
	(cons pair listlist)
	(if (not set1)
	    (cons (cons (car pair) set2)(remove set2 listlist))
	    (if (not set2)
		(cons (cons (cadr pair) set1)(remove set1 listlist))
		(if (not (equal? set1 set2))
			 (cons (append set1 set2)(remove set1 (remove set2 listlist)))
			 listlist))))))
		
(define xaa '((1 2)(3 4)(1 5)(2 7)(1 3)))

(define (included-set listlist item)
  (find-first (lambda(x)(member item x)) listlist))

;;(ros_tree)
