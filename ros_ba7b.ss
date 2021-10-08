;; rosalind
;; Compute Limb Lengths in a Tree
;; [BA7B] 2021/08/14 AC
(require (except-in srfi/1 remove))
(require srfi/13)
(require "readfileA.ss")
(define matD #f)
(define *ba7b_out* "ba7b_out.txt")

(define (ros_ba7b . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba7b.txt"
		    (format "rs_ba7b~a.txt" (car n)))))
	 (N (string->number (car data)))
	 (j (string->number (cadr data)))
	 
	 (res 0)
	 )
    (set! matD (map(lambda(str)(map string->number (string-tokenize str)))(cddr data)))
    (displayln (format "n=~a j=~a" N j))
    (set! res  (solve-ba7b N j matD))
    
    #|
    (call-with-output-file *ba7b_out*
      (lambda(out)
	(display res out))

      #:exists 'truncate/replace)
    |#
    res
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
