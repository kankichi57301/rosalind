;; rosalind
;; Compute Distances Between Leaves
;; [BA7A] 2021/08/05 AC
;(require srfi/1)
(require srfi/13)
(require srfi/14)
(require "readfileA.ss")
(require "roslibB.ss")
(define *ba7a_out* "ba7a_out.txt")
(define *edges* '())
(define *visit* '())

(define (ros_ba7a . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba7a.txt"
		    (format "rs_ba7a~a.txt" (car n)))))
	 (n (string->number (car data)))
	 (edges0 (map parse-adj-list (cdr data)))
	 (edges  (map (lambda(x)(map string->number x))
		      edges0))
	 (nodes (group-by identity (map car edges)))
	 (leaves (map car (filter (lambda(x)(= 1 (length x))) nodes)))
	 (res '())
	 )

    (set! *edges* edges)
    (set! res (solve-ba7a leaves))
          
    (call-with-output-file *ba7a_out*
      (lambda(out)
	(for-each
	 (lambda(line)
		    (for-each (lambda(x)
				(display (format "~a " x) out))
			      line)
		    (display "\n" out))
	 res))
      #:exists 'truncate/replace)
    res
))

(define (parse-adj-list str)
       (string-tokenize str char-set:digit))


(define (solve-ba7a leaves)
  (map (lambda(x)
	 (map (lambda(y)(distance x y))
	      leaves))
       leaves))
	 

(define (distance from to)
  (if (equal? from to)
      0
      (begin
	(set! *visit* (make-hash))
	(distance1 from to))))

(define (distance1 from to)

  (let ((res 0))
    
    (define (distance0 from to acc)
      (hash-set! *visit* from #t)
      ;(displayln (format "from=~a to=~a acc=~a" from to acc))
      (if (equal? from to)
	  (set! res acc)
	  (for-each (lambda(x)(distance0 (cadr x) to (+ acc (caddr x ))))
		    (filter (lambda(x)(and (equal? from (car x))
					   (not (hash-ref *visit* (cadr x) #f)))
				   )
			    *edges*))))
	    
      (distance0 from to 0)
      res
      ))	


;;
;;(ros_ba7a 1)
