#lang racket
;; rosalind
;; Introduction to Pattern Matching 
;; [TRIE] 2021/10/12 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")

(define *trie_out* "data\\trie_out.txt")

(define (ros_trie . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_trie.txt"
		    (format "data\\rs_trie~a.txt" (car n)))))
	 (trie (make-adjacency-list
		(add-number (all-prefix-all data)))))

    (call-with-output-file *trie_out*
      (lambda(out)
	(for-each (lambda(y)
		(for-each (lambda(x)(display (format "~a " x)out ))
		      y)
		(display "\n" out))
	      trie)
	)
      #:exists 'truncate/replace
      )))

(define (all-prefix str)
  (map (lambda(x)(string-take str x))
       (iota (string-length str) 1)))


(define (all-prefix-all strlist)
  (delete-duplicates (append-map all-prefix strlist)))


(define (add-number lis)
  (map (lambda(x y)(list x y))
       lis
       (iota (length lis) 2)))

(define (make-adjacency-list pairs)
  (let ((pairs1 (cons '("" 1) pairs)))
    (map
     (lambda(x)
       (list(cadr (assoc (string-drop-right (car x) 1) pairs1))
	    (cadr x)
	    (string-take-right (car x) 1)))
     pairs)))
		
