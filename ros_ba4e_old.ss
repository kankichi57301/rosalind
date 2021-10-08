;; rosalind
;; 
;; [BA4e] 2021/07/
(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "monoisotopic.ss")
(include "roslib.ss")
(define myhash #f)
(define *ba4e_out* "ba4e_out.txt")
(define *myhash* #f)
(define (ros_ba4e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba4e.txt"
		    (format "rs_ba4e~a.txt" (car n)))))
	 (nlist (map string->number (string-tokenize (car data))))
	 )
    ;(set! *myhash* (make-hash))
    (solve-ba4e nlist)
    ;(hash-map *myhash (lambda(x y) x))
    #|
    (call-with-output-file *****_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

(define (solve-ba4e nlist)
  (let ((mylist (delete 0  nlist)))
    (solve-ba4e0 mylist '())))

    ;maxval
    

(define (consistent? nlist acc new)
  (let ((it (maplist (lambda(x)(apply + (cons new x)))
		     acc)))
    (displayln (format "~a > ~a" nlist it))
    (if (include? it nlist)
	it
	#f)
))	


(define (solve-ba4e0 nlist acc)
    (if (empty? nlist)
        (hash-set! myhash acc #t)
	(for-each (lambda(new)
		    (let ((consist (consistent? nlist acc new)))
		      (solve-ba4e0 (exclude consist nlist)(cons new acc))))
		  (filter (lambda(x)(member x all-amino-weights)) nlist))))
       

      
  
  
;;--*-- test prog --*--
(define (cycle-peptide-mass nlist)
  (let* ((double (append nlist nlist))
	 (len (length nlist))
	 (sum (apply + nlist))
	 (res (append-map (lambda(x)
			    (map (lambda(y)(apply + (take (drop double x) y)))
				 (iota (- len 1) 1)))
			  (iota len))))
    (sort (cons 0 (cons sum res)) <)
  ))
		  
		  
