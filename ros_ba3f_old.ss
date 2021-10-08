;; rosalind
;; Find an Eulerian Cycle in a Graph 
;; [BA3F] 2021/07/**
;(require srfi/1)
(require srfi/13)
(require srfi/14)
(include "readfile.ss")
(include "roslib.ss")
(define *ba3f_out* "ba3f_out.txt")

(define (ros_ba3f . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba3f.txt"
		    (format "rs_ba3f~a.txt" (car n)))))
	 )

    (extend-adj-list(solve-ba3f data))
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
  (map parse-adj-list str))

(define (extend-adj-list1 pair)
  (map (lambda(dst)(list (car pair) dst))(cadr pair)))

(define (extend-adj-list adj-list)
  (append-map extend-adj-list1 adj-list))
