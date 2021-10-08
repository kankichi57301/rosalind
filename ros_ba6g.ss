;; rosalind
;; Implement CycleToChromosome
;; [BA6G] 2021/08/**
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(require mzlib/string)
(define *ba6g_out* "ba6g_out.txt")

(define (ros_ba6g . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba6g.txt"
		    (format "rs_ba6g~a.txt" (car n)))))
	 (dat2 (read-from-string (car data)))
	 (res (solve-ba6g dat2))
	 )
    (call-with-output-file *ba6g_out*
      (lambda(out)
	(display "(" out)
	(display (string-join
		    (map (lambda(n)
			   (if (positive? n)
			       (format "+~a" n)
			       (format "~a"  n)))
			 res)
		    " ")
		 out)
	(display ")" out))
      #:exists 'truncate/replace)
    #t
))

(define (solve-ba6g-0 pair)
  (let ((former (car pair))
	(latter (cadr pair)))
    (if (< former latter)
	(/ latter 2)
	(/ (- former) 2))))

(define (solve-ba6g nlist)
  (map solve-ba6g-0 (group-per nlist 2)))

