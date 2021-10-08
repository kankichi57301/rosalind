;; rosalind
;; Implement ChromosomeToCycle
;; [BA6F] 2021/08/11
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(require mzlib/string)
(define *ba6f_out* "ba6f_out.txt")

(define (ros_ba6f . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba6f.txt"
		    (format "rs_ba6f~a.txt" (car n)))))
	 (res (solve-ba6f (read-from-string (car data))))
	 )
    
    
    (call-with-output-file *ba6f_out*
      (lambda(out)
	(displayln (format "~s" res) out))
      #:exists 'truncate/replace)
    res
))

(define (solve-ba6f nlist)
  (append-map (lambda(n)(if (positive? n)
		     `(,(- (* 2 n) 1) ,(* 2 n))
		     `(,(* -2 n) ,(- (* -2 n) 1))))
       nlist))


