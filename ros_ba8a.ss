#lang racket
;; rosalind
;; Implement FarthestFirstTraversal
;; [BA(A] 2021/11/23 AC
(require srfi/1)
(require srfi/13)
(define +INF 9999)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba8a_out* "data\\ba8a_out.txt")

(define *points* '())
(define *centers* '())


(define (ros_ba8a . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba8a.txt"
		    (format "data\\rs_ba8a~a.txt" (car n)))))
	 (1stline (map string->number (string-tokenize(car data))))
	 (k (car 1stline))
	 (m (cadr 1stline))
	 (points (map (lambda(x)(map string->number (string-tokenize x)))(cdr data)))
	 (n (length points))
	 (res '())
	 )
   (display (format "k=~a " k))
   (display (format "m=~a " m))
   (displayln points)
   (set! *points* points)

   (set! *centers* (list (car points))) ;; 1st element => 1st center
   (set! *points* (cdr *points*))


   (define (loop)
     (if (< (length *centers*) k)
          (let ((furthest-point (get-furthest-point *centers* *points*)))
            (displayln (format "furthest point=~a" furthest-point))
            (set! *points* (delete furthest-point *points*))
	    (set! *centers* (cons furthest-point *centers*))
	    (loop)
            )
          #t))
    (loop)
    
    (call-with-output-file *ba8a_out*
      (lambda(out)
	(for-each (lambda(x)
		    (for-each (lambda(y)
				(display (format "~a " y) out)) x)
		    (display "\n" out)
		    )
		  *centers*))
      #:exists 'truncate/replace)
    *centers*
))

(define (sq-dist point1 point2)
  ;(displayln (format "p1p2=~a,~a" point1 point2))
  (apply + (map (lambda(x y)(* (- x y)(- x y))) point1 point2)))

(define (get-nearest-dist centers point)
  (apply min (map (lambda(x)(sq-dist x point)) centers)))

(define (get-furthest-point centers points)
  (max-item points (lambda(x)(get-nearest-dist centers x))))

(ros_ba8a 1)
