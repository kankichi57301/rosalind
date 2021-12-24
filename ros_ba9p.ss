#lang racket
;; rosalind
;; Implement TreeColoring
;; [BA9P] 2021/12/09 AC
;(require srfi/1)
(require srfi/13)
(require srfi/14)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba9p_out* "data\\ba9p_out.txt")
(define *node* #f)
(define *color* #f)

(define (ros_ba9p . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba9p.txt"
		    (format "data\\rs_ba9p~a.txt" (car n)))))
	 (res '())
	 )
    ;;data
    (set! *node* (make-hash))
    (set! *color* (make-hash))
    (for-each read-node-info data)
    (for-each get-color (hash-keys *node*))
    (set! res (sort
	       (hash-map *color* list)
	       (lambda(x y)(< (car x)(car y)))))
    
    
    (call-with-output-file *ba9p_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a: ~a" (car x)(cadr x))  out))
		  res))
      #:exists 'truncate/replace)
    
    res
))

(define (read-node-info str)
  (let ((parsed (regexp-match #rx"([0-9]+) -> ([0-9]+(,[0-9]+)*)" str)))
    (if parsed
	(hash-set! *node* (string->number (cadr parsed))
		   (map string->number (string-tokenize (caddr parsed) char-set:digit)))
 ;;--*--
	(let ((parsed2 (regexp-match #rx"([0-9]+): ([A-Za-z]+)" str)))
	  (if parsed2
	      (hash-set! *color* (string->number (cadr parsed2))(caddr parsed2))
	      #f)))))

(define (get-color n)
  (let ((col (hash-ref *color* n #f)))
    (if col
	col
	(let ((child (hash-ref *node* n '())))
	  (if (all-red? child)
	      (begin
		(hash-set! *color* n "red")
		"red")
	      (if (all-blue? child)
		  (begin
		    (hash-set! *color* n "blue")
		    "blue")
		  (begin
		    (hash-set! *color* n "purple")
		    "purple")))))))

(define (all-red? lst)
  (andmap (lambda(x)(equal? (get-color x) "red")) lst))

(define (all-blue? lst)
  (andmap (lambda(x)(equal? (get-color x) "blue")) lst))

(define (dump-result0 n)
  (format "~a[~a]" n (hash-ref *color* n)))

(define (dump-result1 n)
  (let ((child (hash-ref *node* n #f)))
    (if child
	(format "~a -> ~a" (dump-result0 n)
		(map dump-result0 child))
	(dump-result0 n))))

(define (dump-result)
  (for-each (lambda(n)(displayln (dump-result1 n)))
	    (hash-keys *color*)))
	  
