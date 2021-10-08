;; rosalind
;; Find an Eulerian Cycle in a Graph 
;; [BA3F] 2021/07/**
;(require srfi/1)
(require srfi/13)
(require srfi/14)
(include "readfile.ss")
(include "roslib.ss")
(include "roslib2.ss")
(define *ba3f_out* "ba3f_out.txt")

(define (ros_ba3f . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba3f.txt"
		    (format "rs_ba3f~a.txt" (car n)))))
	 )

    (solve-ba3f data)
    #|
    (call-with-output-file *ba3f_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
))

(define (solve-ba3f str)
  (extend-adj-list
   (map parse-adj-list str)))

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
  (find-cycle0 adj-list (list (caar adj-list))))

(define (find-cycle0 adj-list acc)
  (displayln (format "acc=~s" acc))
  (let ((next-edge (find-first (lambda(edge)(= (car acc)(car edge)))
			       adj-list
			       )))
      (displayln (format "next=~s" next-edge))
      (if next-edge
	  (find-cycle0 (remv next-edge adj-list)(cons (cadr next-edge) acc)) ;; not remove (remv)
	  (reverse acc))))

  
