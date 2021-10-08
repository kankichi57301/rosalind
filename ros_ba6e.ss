;; rosalind
;; Find All Shared k-mers of a Pair of Strings
;; [BA6E] 2021/08/05 AC
(require srfi/1)
(require srfi/13)
(require srfi/19)
(require "roslibA.ss")
(include "readfile.ss")
(define *ba6e_out* "ba6e_out.txt")
(define myhash1 #f)
(define myhash2 #f)
(define *time* #f)
(define *res* '())

(define (ros_ba6e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba6e.txt"
		    (format "rs_ba6e~a.txt" (car n)))))
	 (k (string->number (car data)))
	 (lines (cdr data))
	 )
    (set! myhash1 (make-hash))
    (set! myhash2 (make-hash))
					;(apply solve-ba6e k lines)
    (apply init-ba6e k lines)
    (solve-ba6e)
    
    (call-with-output-file *ba6e_out*
      (lambda(out)
	(for-each (lambda(lst)
		    (displayln (format "(~a, ~a)" (car lst)(cadr lst)) out)
		    )
		  *res*))
      #:exists 'truncate/replace)
    
    #t
))


(define (init-ba6e n str1 str2)
  (set! *time* (current-time))
  (for-each
   (lambda(i)(hash-append! myhash1 (string-take (string-drop str1 i) n) i))
   (iota (+ 1 (- (string-length str1) n))))
  (displayln "step1")
  (for-each
   (lambda(i)(hash-append! myhash2 (string-take (string-drop str2 i) n) i))
   (iota (+ 1 (- (string-length str2) n))))
  (displayln "step2")
  (displayln (format "pass(1)elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
  )

(define (solve-ba6e )
  (set! *res*
	(append
	 (apply append
		(hash-map myhash1 (lambda(x y)(cartesian-product y (hash-ref myhash2 x '())))))
	 (apply append
		(hash-map myhash1 (lambda(x y)(cartesian-product y (hash-ref myhash2 (m-rc x) '())))))))
  (displayln (format "pass(2)elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
  )
