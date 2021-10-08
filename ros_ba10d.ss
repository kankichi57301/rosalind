;; rosalind
;; 
;; Compute the Probability of a String Emitted by an HMM
;; [BA10D] 2012/06/20 AC
(require srfi/1)
(require srfi/13)
(require srfi/48)
(include "readfile.ss")
(include "roslib.ss")
(define *DEBUG* #f)
(define *FILE_OUT* #t)
(define *ba10d_out* "ba10d_out.txt")
(define fl-pat #rx"[0-9]*[.][0-9]+")
(define *matA* '())
(define *matB* '())
(define *seq* "")
(define *prob* 0)
;(define *from* 0)
(define *state* '())
(define *sigma* '())

(define (ros_ba10d . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba10d.txt"
		    (format "rs_ba10d~a.txt" (car n)))))
	 (seq (car data))
	 (seqlen (string-length seq))
	 (sigma (map (lambda(x)(string-ref x 0))
		     (regexp-match* #rx"[A-Za-z]" (third data))))
	 (state (map (lambda(x)(string-ref x 0))
		     (regexp-match* #rx"[A-Za-z]" (fifth data))))
	 (nstat (length state))
	 (matA  (map (lambda(x)(map string->number (regexp-match* fl-pat x)))
		     (take (drop  data 7) nstat)))
	 (matB  (map (lambda(x)(map string->number (regexp-match* fl-pat x)))
		     (take (drop  data (+ 9 nstat)) nstat)))
	 (first-out (string-ref seq 0))
	 )
		 
    (set! *prob* (make-hash))
    ;(set! *from* (make-hash))
    (set! *seq* seq)
    (set! *state* state)
    (set! *sigma* sigma)
    (set! *matA* matA)
    (set! *matB* matB)

    
    (define (init-vitervi)
      (for-each (lambda(x)(hash-set! *prob*
				     `(0 ,x)
				     ;;最初の状態の初期値は全部[1/状態数]で等しくする。
				     (* (/ 1.0 nstat)
					(list-ref (list-ref matB x)(index-of *sigma* first-out)))))
		(iota nstat)))
    
    

    (define (step-vitervi0 step)
      (map (lambda(to)
	     (map (lambda(from)
		    (let* ((p1 (hash-ref *prob* (list (- step 1) from)))           ;;一つ前の状態での最大の累積確率
			   (p2 (list-ref (list-ref *matA* from) to))                ;;注目する状態への状態遷移確率
			   (ch (string-ref seq step))
			   (p3 (list-ref (list-ref *matB* to) (index-of sigma ch))) ;;注目する状態での出力確率
			   (pall (* p1 p2 p3)))
		      (when *DEBUG*
			    (when (<= step 2)
				  (displayln (format "p=~1,5F:~1,5F:~1,5F[~a]::~1,5F" p1 p2 p3 ch pall))))
		      pall))	   
		  (iota nstat)))
	   (iota nstat)))

    (define (step-vitervi step)
      (let ((wk (step-vitervi0 step)))
	;;(when (<= step 2)
	  ;;    (displayln (format "[~a]wk=~a" step wk)))
	(for-each (lambda(x y)(hash-set! *prob* (list step x) y))
		  (iota nstat)
		  (map (lambda(x)(apply + x)) wk))  ;; 10c=>10d max=>+に改変
      ))
    
    ;;;--*-- max=> + && traceback不要
    (define (output-ba10d n)
      (apply +                         
       (map (lambda(x)
	      (hash-ref *prob* (list n x)))
	    (iota nstat ))))
      
       
    (define (sort-car-pair x y)
      (if (= (caar x)(caar y))
	  (< (cadar x)(cadar y))
	  (< (caar x)(caar y))))
    ;; --*-- main routine --*--
    (init-vitervi)
    (for-each (lambda(x)(step-vitervi x))(iota (- seqlen 1) 1)) 

    ;;(displayln (sort (hash-map *prob* list) sort-car-pair))
    ;;(displayln (sort (hash-map *from* list) sort-car-pair))
    (let ((result (output-ba10d (- seqlen 1))))
      (when *FILE_OUT*
	    (call-with-output-file *ba10d_out*
	      (lambda(out)
		(displayln result out ))
	      #:exists 'truncate/replace))

      (displayln result)
    )
 ))


  
(define (myformat x y)
  (cons x (list (format "~1,9F" y))))
;; (load "ros_ba10d.ss")
;; (ros_ba10d 1)

;;--*-- test prog --*--
(define (trans-zip state sigma)
  (transition (append-map list (string->list state)(string->list sigma))))
(define (transition3  arglist)
  (if (< (length arglist) 2)
      (/ 1.0 (length *state*)) 
      (* (trans-state (caddr arglist)(car arglist))
	 (transition2 (cdr arglist)))))

(define (transition2  arglist)
  (* (trans-sigma (cadr arglist)(car arglist))
     (transition3 (cdr arglist))
  ))
;; [AB]->[xyz]->[AB]->[xyz]
(define (transition   arglist)
  (transition2  (reverse arglist))
  )
;;; #\A,B -> #\A,B prob
(define (trans-state from to)
  (let ((x (index-of *state* from))
	(y (index-of *state* to)))
    (if (and x y)
	(list-ref (list-ref *matA* x) y)
	#f
	)))
;;; #\A,B -> #\x,y,z prob
(define (trans-sigma from to)
  (let ((x (index-of *state* from))
	(y (index-of *sigma* to)))
    (if (and x y)
	(list-ref (list-ref *matB* x) y)
	#f
	)))
;; 第一引数はindex (#\A,B でなく)　0,1->#\x,y,z prob
(define (trans-sigma2 x to)
  (let (
	(y (index-of *sigma* to)))
    (if (and x y)
	(list-ref (list-ref *matB* x) y)
	#f
	)))

;;--*-- test 2
;(ros_ba10c 5)


(define (all-seq n)
  (map (lambda(x)(apply string x))(cartesian-expt *state* n)))

(define (test-ba10d seq)
  (apply + (map (lambda(x)(trans-zip x seq))
		(all-seq (string-length seq)))))
	    

(define (check-ba10d)
  (test-ba10d *seq*))
;;--*-- test 3
;(test-ba10d "xyxzzxy")
;(trans-zip "ABAAABB" "xyxzzxy")
