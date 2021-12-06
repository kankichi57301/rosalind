#lang racket
;; rosalind
;; Find the Length of a Longest Path in a Manhattan-like Grid
;; [BA5B] 2021/1
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba5b_out* "data\\ba5b_out.txt")
(define *grid* #f)            ;; key (x0,y0,x1,y1) value length 
(define *ns* '())             ;; north ->south  path
(define *we* '())             ;; west  ->east   path
(define *dist* #f)            ;; distance from (0,0)

(define (ros_ba5b . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba5b.txt"
		    (format "data\\rs_ba5b~a.txt" (car n)))))
	 (1stline (string-tokenize (car data)))
	 (n (string->number (car  1stline)))
	 (m (string->number (cadr 1stline)))
	 (ns-path (map string-tokenize (take (cdr data) n)))                  ;; north ->south
	 (we-path (map string-tokenize (take (drop data (+ n 2)) (+ n 1))))   ;; west  ->east
	 )
    (set! *grid* (make-hash))
    (set! *dist* (make-hash))
    (displayln (format "nm=~a,~a" n m))
    (set! *ns* ns-path)
    (set! *we* we-path)
    (set-ns ns-path n m)
    (set-we we-path n m)
    (fill-dp n m)
    (displayln (format "ans=~a" (hash-ref *dist* `(,n ,m))))
    ;;(displayln ns-path)
    ;;(displayln we-path)
    #|
    (call-with-output-file *ba5b_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

(define (set-ns ns-path n m)
  (for-each (lambda(i)
	      (for-each (lambda(j)
			  (hash-set! *grid* `(,i ,j ,(+ i 1),j) (string->number (list-ref (list-ref ns-path i) j))))
			(iota  (+ m 1))))
	    (iota  n)))

(define (set-we we-path n m)
  (for-each (lambda(i)
	      (for-each (lambda(j)
			  (hash-set! *grid* `(,i ,j ,i ,(+ j 1)) (string->number (list-ref (list-ref we-path i) j))))
			(iota  m )))
	    (iota (+ n 1))))
	      
			  
(define (dump-grid)
  (sort (hash-map *grid* list)
	(lambda(x y)(if (= (caar x)(caar y))
			(< (cadar x)(cadar y))
			(< (caar x)(caar y))))))
(define (dump-dist)
  (sort (hash-map *dist* list)
	(lambda(x y)(if (= (caar x)(caar y))
			(< (cadar x)(cadar y))
			(< (caar x)(caar y))))))

(define (fill-dp n m)
  (hash-set! *dist* '(0 0) 0)
;;--
  (for-each (lambda(x)(hash-set! *dist* `(,x 0)
				 (+ (hash-ref *dist* `(,(- x 1) 0))
				    (hash-ref *grid* `(,(- x 1) 0 ,x 0)))))
	    (iota n 1))
;;--  
  (for-each (lambda(y)(hash-set! *dist* `(0 ,y)
				 (+ (hash-ref *dist* `(0 ,(- y 1)))
				    (hash-ref *grid* `(0 ,(- y 1) 0 ,y)))))
	    (iota m 1))
;;--  
  (for-each (lambda(x)
	      (for-each (lambda(y)
			  (hash-set! *dist* `(,x ,y)
				    (max (+ (hash-ref *dist* `(,(- x 1) ,y))
					    (hash-ref *grid* `(,(- x 1) ,y ,x ,y)))
					 (+ (hash-ref *dist* `(,x ,(- y 1)))
					    (hash-ref *grid* `(,x ,(- y 1) ,x ,y))))))
			(iota m 1)))
	    (iota n 1))
)

;;(ros_ba5b 2)
