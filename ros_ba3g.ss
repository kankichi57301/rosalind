;; rosalind
;; Find an Eulerian Path in a Graph 
;; [BA3G] 2021/07/27 AC
;(require srfi/1)
(require srfi/13)
(require srfi/14)
(include "readfile.ss")
(include "roslib.ss")
(include "roslib2.ss")
(define *ba3g_out* "ba3g_out.txt")

(define (ros_ba3g . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba3g.txt"
		    (format "rs_ba3g~a.txt" (car n)))))
	 )

      (call-with-output-file *ba3g_out*
	(lambda(out)
	  (displayln (solve-ba3g data) out))
	#:exists 'truncate/replace)
      #t    
    
))

;;


(define (solve-ba3g str)
  (let ((adj-list (extend-adj-list
		   (map parse-adj-list str)))
	(res '())
	)
    
    (displayln (format "#ofEdge=~a" (length adj-list)))
    (set! res (apply find-path (cons adj-list (calc-start-end adj-list))))
    (displayln (format "#ofEdge=~a" (length res)))
    (string-join (map number->string res) "->" )))





(define (calc-start-end adj-list)
  (let ((s-e
	 (map car
	      (filter (lambda(x)(odd? (length x)))(group-by identity (apply append adj-list))))))
    (if (start-node? adj-list (car s-e))
	s-e
	(reverse s-e))))

(define (start-node? adj-list node)
  (> (count (lambda(x)(equal? node (car x)))  adj-list)
     (count (lambda(x)(equal? node (cadr x))) adj-list)))



(define (extend-adj-list1 pair)
  (map (lambda(dst)(list (car pair) dst))(cadr pair)))

(define (extend-adj-list adj-list)
  (append-map extend-adj-list1 adj-list))
;;
;; '(A B X C D) '(X 1 2 3 X) ==> '(A B X 1 2 3 X C D)
(define (splice-loop main sub)
  (let ((pos (index-of main (car sub))))
    (append (take main pos)
	    sub
	    (drop main (+ 1 pos)))))

(define (find-cycle adj-list )
  (call-with-values (lambda()(find-cycle0 adj-list (list (caar adj-list))))
    (lambda(acc rest)
      (find-cycle1 rest acc))))
    
    

(define (find-cycle0 adj-list acc)
  ;(displayln (format "acc=~s" acc))
  (let ((next-edge (find-first (lambda(edge)(equal? (car acc)(car edge)))
			       adj-list
			       )))
      ;(displayln (format "next=~s" next-edge))
      (if next-edge
	  (find-cycle0 (remv next-edge adj-list)(cons (cadr next-edge) acc)) ;; not remove (remv)
	  (values (reverse acc) adj-list))))

  
(define (find-cycle1 adj-list acc)
  ;(displayln (format "acc=~s" acc))
  (if (empty? adj-list)
      acc
      (let* ((rest-start (map car adj-list))
	     (next (find-first (lambda(node)(member node rest-start))
			       (reverse acc))))
	;(displayln (format "next=~a" next))
	
	(call-with-values (lambda()(find-cycle0 adj-list (list next)))
	  (lambda(nextloop rest)
	    ;(displayln (format "next=~a:rest=~a" nextloop rest))
	    ;(displayln (format "marged=~a" (splice-loop acc nextloop)))
	    (find-cycle1 rest (splice-loop acc nextloop))
		   )))))


(define (find-path adj-list start end)
  (call-with-values (lambda()(find-path0 adj-list (list start) end))
    (lambda(acc rest)
      ;(displayln (format "~a::~a" acc rest))
      (find-cycle1 rest acc))))
    
    

(define (find-path0 adj-list acc end)
  ;(displayln (format "acc=~s" acc))
  (let ((next-edge (find-first (lambda(edge)(equal? (car acc)(car edge)))
			       adj-list
			       )))
      ;(displayln (format "next=~s" next-edge))
      (if (equal? next-edge end)
	  (values (reverse acc) adj-list)
	  (find-cycle0 (remv next-edge adj-list)(cons (cadr next-edge) acc))))) ;; not remove (remv)


