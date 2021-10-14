#lang racket
;; rosalind
;; 
;; [DBRU] 
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *dbru_out* "data\\dbru_out.txt")

(define (ros_dbru . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_dbru.txt"
		    (format "data\\rs_dbru~a.txt" (car n)))))
	 (res (map split-3-mers (delete-duplicates (append data (map m-rc data)))))
	 (res2 (sort res (lambda(x y)(string<? (car x)(car y)))))
	 )
    
    
    (call-with-output-file *dbru_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "(~a,~a)" (car x)(cadr x)) out))
		  res2))
      #:exists 'truncate/replace)
    
    res2
))

(define (split-3-mers str)
  (list (string-drop-right str 1)
	(string-drop str 1)))
