;; rosalind
;; Construct a Trie from a Collection of Patterns
;; [BA9A] 2021/09/09 AC!
;(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "trie.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba9a_out* "ba9a_out.txt")

(define (ros_ba9a . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba9a.txt"
		    (format "rs_ba9a~a.txt" (car n)))))
	 (cnt (if (> (length n) 1)
		  (cadr n)
		  #f))
	 (res '())
	 (data2 (if cnt
		    (take data cnt)
		    data))
	 )

    (set! res
	  
	   (map (lambda(ex)(format "~a->~a:~a" (car ex)(cadr ex)(caddr ex)))
		(test-trie
		 (map
		  (lambda(x)(map (lambda(c)(string->symbol (string c))) x))
		  (map string->list data2)))))
	   
    
    (call-with-output-file *ba9a_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a" x) out))
		  res))
      #:exists 'truncate/replace)
    res
    
))

