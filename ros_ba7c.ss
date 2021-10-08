;; rosalind
;; mplement AdditivePhylogeny
;; [BA7C] 2021/08/14 AC
(require (except-in srfi/1 remove))
(require srfi/13)
(require "readfileA.ss")

(define *ba7c_out* "ba7c_out.txt")
(define matD #t) 

(define (ros_ba7c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba7c.txt"
		    (format "rs_ba7c~a.txt" (car n)))))
	 (n (string->number (car data)))
	 (res 0)
	 )
    (set! matD (map(lambda(str)(map string->number (string-tokenize str)))(cdr data)))
    matD
    
    #|
    (call-with-output-file *ba7b_out*
      (lambda(out)
	(display res out))

      #:exists 'truncate/replace)
    |#
    
    ))

;; x,y 0-based
(define (mat-ref nlistlist x y)
  (list-ref (list-ref nlistlist x) y))

(define (solve-ba7b n j matD)
  (apply min (map (lambda(pair)
		    #|
		    (displayln (format "~a ~a | ~a (~a ~a ~a)"
				       (mat-ref matD (car pair) j)
				       (mat-ref matD (cadr pair) j)
				       (mat-ref matD (car pair)(cadr pair))
				       (car pair)
				       (cadr pair)
				       j
				       ))
		    |#
		    (/  (+ (mat-ref matD (car pair) j)
			   (mat-ref matD (cadr pair) j)
			   (- (mat-ref matD (car pair) (cadr pair))))
			2
			))
		  (combinations (remove j (iota n)) 2))))
