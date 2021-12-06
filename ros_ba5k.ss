#lang racket
;; rosalind
;; Find a Middle Edge in an Alignment Graph in Linear Space 
;; [BA5K] 2021/11/29 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(require "blosum62A.ss") 
(define *ba5k_out* "data\\ba5k_out.txt")
(define *gap-pen* -5)
(define *dp* #f)
(define *s1* "")
(define *s2* "")


(define (ros_ba5k . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba5k.txt"
		    (format "data\\rs_ba5k~a.txt" (car n)))))
	 )
    (set! *dp* (make-hash))
    (set! *s1* (car data))
    (set! *s2* (cadr data))
    (let-values ([(x1 y1 x2 y2)(apply solve_ba5k data)])
      (displayln (format "xyv=(~a,~a) (~a, ~a)" x1 y1 x2 y2))
      )
    #|
    (call-with-output-file *ba5l_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
    ))
#|
(define *x* 0)
(define *y* 0)
(define *cx* 0)
(define *cy* 0)
(define *s1* "")
(define *s2* "")
|#
(define (solve_ba5k str1 str2)
  (let ((xhalf (floor (/ (string-length str1) 2)))
	(yhalf (floor (/ (string-length str2) 2))))
    (displayln (format "xh=~a yh=~a" xhalf yhalf))
    (for-each (lambda(i)(hash-set! *dp* `(,i 0) (* *gap-pen* i)))(range 0 (+ xhalf 1)))
    (for-each (lambda(i)(hash-set! *dp* `(0 ,i) (* *gap-pen* i)))(range 0 (+ yhalf 1)))
    
    
    (for-each (lambda(y)
		(for-each (lambda(x)
			    #|
			    (set! *x* x)
			    (set! *y* y)
			    (set! *cx* (string-ref str1 (- x 1)))
			    (set! *cy* (string-ref str2 (- y 1)))
			    (set! *s1* str1)
			    (set! *s2* str2)
			    |#
			    (let* ((c (b62score (string-ref str1 (- x 1))
						(string-ref str2 (- y 1))))
				       
				 (v1 (+ *gap-pen* (hash-ref *dp* `(,(- x 1) ,y))))
				 (v2 (+ *gap-pen* (hash-ref *dp* `(,x  ,(- y 1)))))
				 (v3 (+ c 
					(hash-ref *dp* `(,(- x 1) ,(- y 1)))))
				 (val (max v1 v2 v3 0))
				 ;;(dir (max-index-of (list v1 v2 v3 0) identity))
				 )
			    (hash-set! *dp*  (list x y) val)
			    ;;(hash-set! *tb*  (list x y) dir)
			    ))
			  (range 1 (+ xhalf 1)))
		)
	      (range 1 (+ yhalf 1)))
    ;;(dump-dp xhalf yhalf)
    
    (define (get-max-edge)
      (let ((max-x -1)
	    (max-y -1)
	    (max-v 0))
	(for-each (lambda(j)
		    ;;(display (format "pos=~a" `(,xhalf ,j)))
		    (let ((v (hash-ref *dp* `(,xhalf ,j))))
		      (when (> v max-v)
			    (set! max-v v)
			    (set! max-x xhalf))
			    (set! max-y j)))
		  (range 1 (+ yhalf 1)))
	(for-each (lambda(i)
		    ;;(display (format "pos=~a" `(,i ,yhalf)))
		    (let ((v (hash-ref *dp* `(,i ,yhalf ))))
		      (when (> v max-v)
			    (set! max-v v)
			    (set! max-x i)
			    (set! max-y yhalf))))
		  (range 1 (+ xhalf 1)))
	(if (equal? (string-ref str1 max-x)(string-ref str2 max-y))
	    (values max-x max-y (+ 1 max-x)(+ 1 max-y))
	    (if (equal? max-x xhalf)
		(values max-x max-y (+ 1 max-x) max-y)
		(values max-x max-y max-x (+ 1 max-y))))
      ))
    
    (get-max-edge)
    
    ))

(define (dump-dp len1 len2)
  (for-each
   (lambda(y)
     (for-each
      (lambda(x)
	   (display (format "~a " (hash-ref *dp* `(,x ,y) '*))))
      (iota (+ 1 len1) 0))
     (display "\n"))
   (iota (+ 1 len2) 0))
  )

;;(ros_ba5k 3)
