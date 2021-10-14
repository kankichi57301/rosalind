#lang racket
;; rosalind
;; Creating a Character Table from Genetic Strings
;; [CSTR]
;; 2021/01/18 AC
;; 2021/10/14 AC 
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *cstr_out* "data\\cstr_out.txt")

(define (ros_cstr . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_cstr.txt"
		    (format "data\\rs_cstr~a.txt" (car n)))))
	 (n (string-length (car data)))
	 )
    (for-each displayln
	       (map (lambda(x)(apply string (char-table-2values x)))
		    (filter non-trivial? (transpose (map string->list data)))))

    (call-with-output-file *cstr_out*
      (lambda(out)
	(for-each (lambda(x) (displayln x out))
		  (map (lambda(x)(apply string (char-table-2values x)))
		       (filter non-trivial? (transpose (map string->list data))))))
	#:exists 'truncate/replace)
    
   ))


(define (nuc-count lst)
  (map (lambda(y)(count (lambda(x)(equal? y x)) lst))
       '(#\A #\C #\G #\T)))
  
(define (non-trivial? lst)
  (let ((cnt (nuc-count lst)))
    (and (= 2 (length (filter (lambda(x)(not (zero? x))) cnt)))
	 (not (ormap (lambda(x)(= 1 x)) cnt)))))


(define (char-table-2values0 lst start)
  (if (null? lst)
      '()
      (cons (if (equal? (car lst) start)
		#\1
		#\0)
	    (char-table-2values0 (cdr lst) start))))

(define (char-table-2values lst)
  (char-table-2values0 lst (car lst)))

