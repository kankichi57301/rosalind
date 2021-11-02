#lang racket
;; rosalind
;; Generate Contigs from a Collection of Reads
;; [BA3K] 2021/11/02 AC
(require srfi/1)
(require srfi/13)
(require srfi/19)
(require "readfileA.ss")
(require "roslibA.ss")
(require "roslibB.ss")
(define *ba3k_out* "data\\ba3k_out.txt")
(define k 3)
(define k-1 2)
(define dnac 0)
(define *dnalist* '())
(define *in* '())
(define *out* '())
(define *graph* '())
(define *all-nodes* '())
(define *res* '())
(define *time* #f)

(define (ros_ba3k . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba3k.txt"
		    (format "data\\rs_ba3k~a.txt" (car n)))))
	 (ans '())
	 )
    (set! *time* (current-time))
    (set! *in* (make-hash))
    (set! *out* (make-hash))
    (set! *res* (make-hash))
    (set! k (string-length (car data)))
    (set! k-1 (- k 1))
    (set! *dnalist* data)
    (set! dnac (length data))
    ;;
    (set! *graph* (get-all-graph data))
    (init-ba3k) 

    (displayln (format "dnacount=~a" dnac))
    
    (set! ans (sort (find-contigs dnac) string<))
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
    
    (call-with-output-file *ba3k_out*
      (lambda(out)
	(for-each (lambda(dna)
		    (display (format "~a~%" dna) out))
		  ans))
      #:exists 'truncate/replace)
    
    
   ))

(define (count-in edge)
  (inc-hash! *in* (cadr edge)))
(define (count-out edge)
  (inc-hash! *out* (car edge)))
(define (init-ba3k)
  (for-each (lambda(edge)
	      (count-in edge)
	      (count-out edge))
	    *graph*))


(define (connect? former latter)
  (string=? (string-take-right former k-1)
	    (string-take       latter k-1)))

(define (get-all-graph dnas)
  (filter (lambda(pair) 
	    
	    (and (not (= (car pair)(cadr pair)))
			    (connect? (list-ref dnas (car pair))(list-ref dnas (cadr pair)))))
	    
	  (cartesian-expt (iota dnac) 2)))

(define (find-contigs nodes)
  (solve-ba3k0 '(0) (cdr (iota nodes)))
  (map contign->char
       (hash-keys *res*)))

(define (solve-ba3k0 nodes rest)
  ;;(displayln (format "nodes=~a rest=~a" nodes rest))
  (if (null? rest)  ;; end of process
      (begin
	(hash-set! *res* nodes #t)
	#t)
      (let ((left (car nodes))
	    (right (car (take-right nodes 1))))
	;;(displayln (format "left=~a right=~a" left right))
	(let ((left-next (if (= 1 (hash-ref *in* left 0))
			     (car (find-first  (lambda(x)(equal? (cadr x) left)) *graph*))
			     #f)))
	  (if (and left-next (= 1 (hash-ref *out* left-next 0)))
	      (solve-ba3k0 (cons left-next nodes)(delete left-next rest))
	      (let ((right-next (if (= 1 (hash-ref *out* right 0))
				    (cadr (find-first  (lambda(x)(equal? (car x) right)) *graph*))
				    #f)))
		(if (and right-next (= 1 (hash-ref *in* right-next 0)))
		    (solve-ba3k0 (append nodes (list right-next))(delete right-next rest))
		    (begin
		      ;(displayln (format "nodes:=~a" nodes))
		      (hash-set! *res* nodes #t)
		      (solve-ba3k0 (list (car rest))(cdr rest))))))))))

(define (contign->char nlist)
  (string-append (list-ref *dnalist* (car nlist))
		 (apply string-append (map (lambda(n)(string-take-right (list-ref *dnalist* n) 1)) (cdr nlist)))))
		 
