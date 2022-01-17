#lang racket
;; rosalind
;; Implement 2-BreakOnGenomeGraph
;; [BA6K] 2022/01/17 AC
(require (only-in srfi/1 iota))
(require (only-in srfi/13 string-tokenize))
(require "readfileA.ss")
(require "roslibA.ss")
(require "cycle.ss")

(require mzlib/string)  ;; string-replace (not srfi/13)
(define *ba6k_out* "data\\ba6k_out.txt")
(define *res* '())
(define *res2* '())

(define (ros_ba6k . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba6k.txt"
		    (format "data\\rs_ba6k~a.txt" (car n)))))
	 (graph (read-from-string (format "~a" (string-replace (car data) "," ""))))
	 (nums (map string->number (string-tokenize (string-replace (cadr data) "," " "))))
	 (i  (car nums))
	 (i1 (cadr nums))
	 (j  (caddr nums))
	 (j1 (fourth nums))
	 (n (length graph))
	 )
    ;(displayln n)
    ;(displayln (format "|i=~a i1=~a j=~a j1=~a" i i1 j j1))
    

    (set! *res*
     (find-cycles
      (append
       (group-per (iota (* 2 n) 1) 2)
       (2break-graph
	(cycle->num graph)
	i i1 j j1))))

    (set! *res2* (map (lambda(x)(map pair->synt x))(map (lambda(x)(group-per x 2))*res*)))
    
    (call-with-output-file *ba6k_out*
      (lambda(out)
	(map (lambda (cycle)
	       (display  (map (lambda(n)(if (positive? n)
					      (format "+~a" n)
					      (format "~a"  n)))
				cycle)
			  out))
	     *res2*))
      #:exists 'truncate/replace)
    (display *res2*)
    #t
))



(define (format-pair pair)
  (format "(~a, ~a)" (car pair)(cadr pair)))

(define (2break-graph graph i i1 j j1)
  ;(displayln (format ":i=~a i1=~a j=~a j1=~a" i i1 j j1))
  (cons `(,i ,j)
	(cons `(,i1 ,j1)
	      (remove `(,j ,j1)
		      (remove `(,i ,i1) graph pair= )
		      pair= ))))

(define (pair->synt pair)
  (if (even? (car pair))
      (- (/ (car pair) 2))
      (/ (+ 1 (car pair)) 2)))
