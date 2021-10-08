#lang racket
;; rosalind
;;Find the 
;; [BA9F] 2021/09/
(require srfi/1)
(require racket/hash)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
;;(require suffixtree)
(define *ba9f_out* "ba9f_out.txt")
(define my-tree1 '())
(define my-tree2 '())
(define my-hash1  '())
(define my-hash2  '())

(define (ros_ba9f . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba9f.txt"
		    (format "rs_ba9f~a.txt" (car n)))))
	 (text1 (car data))
	 (text2 (cadr data))
	 )
    (noncommon-str text1 text2)
    #|
    (call-with-output-file *ba9f_out*
      (lambda(out)
	(displayln
	 (longest-multiple (tree-root my-tree))
	 out
	 ))
      #:exists 'truncate/replace)
    |#
    
    
    ))

(define (noncommon-str str1 str2)
  (ormap (lambda(n)(noncommon-str0 str1 str2 n))
	 (iota (string-length str1) 1)))
  
(define (noncommon-str0 str1 str2 n)
  ;(displayln (format "~a ~a [~a]" str1 str2 n))
  (if (or (< (string-length str2) n)(< (string-length str1) n))
      #f
      (let ((sub (string-take str1 n))) 
	(if (not (string-contains str2 sub))
	    sub
	    (noncommon-str0 (string-drop str1 1) str2 n)))))
