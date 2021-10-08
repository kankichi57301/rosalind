;; rosalind
;; Implement GreedySorting to Sort a Permutation by Reversals
;; [BA6A] 2021/07/24 AC
;(require srfi/1)
(require srfi/13)
(require srfi/14)
(include "readfile.ss")
(define *DEBUG* #t)
(define *ba6a_out* "ba6a_out.txt")


(define (ros_ba6a . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba6a.txt"
		    (format "rs_ba6a~a.txt" (car n)))))
	 (expr (call-with-input-string (car data)
			    (lambda(in)(read in))))
       )

    (if *DEBUG*
	(solve-ba6a expr (current-output-port))
   
	(call-with-output-file *ba6a_out*
	  (lambda(out)
	    (solve-ba6a expr out))
	  #:exists 'truncate/replace)
    )))
   
    
    

	 
				
(define (reverse-minus-span nlist start end)
  (append (take nlist start)
	  (map - (reverse (take (drop nlist start) (+ 1 (- end start)))))
	  (drop nlist (+ 1 end))))

(define (abs-eq? n1 n2)
  (= (abs n1)(abs n2)))


(define (find-pos lst item eq)
  (if (empty? lst)
      #f
      (if (eq (car lst) item)
	  0
	  (+ 1 (find-pos (cdr lst) item eq)))))
    


(define (solve-ba6a nlist out)
  (let ((flag #f))
    (for-each (lambda(pos)
		(let ((f (find-pos nlist (+ pos 1) abs-eq?)))
					;(display pos)
		  (unless (and (positive? (list-ref nlist f))(= pos f))
			  (if flag
			      (disp-sign-list	nlist out)
			      (set! flag #t))
			  (set! nlist (reverse-minus-span nlist pos f))
			  (when (negative? (list-ref nlist pos))
				(disp-sign-list	nlist out)
				(set! nlist (list-set nlist pos (+ 1 pos)))
		      ))))
		(iota (length  nlist)))
	      (disp-sign-list nlist out)))
 
      

  
					   
;;
(define aaa '(-3 +4 +1 +5 -2))

(define (disp-sign n)
  (if (positive? n)
      (format "+~a" n)
      (format "~a" n)))

(define (disp-sign-list nlist out)
  (displayln (format "(~a)" (string-join (map disp-sign nlist))) out))
