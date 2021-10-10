#lang racket
;; rosalind
;; Genome Assembly as Shortest Superstring 
;; [LONG]
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(define *long_out* "data\\long_out.txt")

(define (overlap-len dna1 dna2 half)
  (let((len 
	(filter (lambda(x) (string=? (string-take-right dna1 x)
					(string-take       dna2 x)))
			   (range half (* 2 half)))))
    (if (null? len)
	0
	(car len))
    ))


(define testlist '())
(define testinfo '())

(define (overlap-all dnalist)
  (let* ((len (length dnalist))
	 (stlen (apply min (map string-length dnalist)))
	 (half  (floor (/ stlen 2))))

    (filter (lambda(s)(>= (caddr s) half))
	    (map
	     (lambda(x)
	       (append  x 
			(list
			 (overlap-len (list-ref dnalist (car  x))
				    (list-ref dnalist (cadr x)) half))))
			    
     (cartesian-product (iota len)(iota len)))
     )

    ))

#|
(define (ros_long)
  (let* ((fasta-data (edit-fasta(read-file "_long")))
	 (connection-inf (overlap-all fasta-data)))
    (set! testlist fasta-data)
    (set! testinfo connection-inf)

    fasta-data
    #|
    (connect-dna 
     connection-inf
     fasta-data)
    |#
    ))
|#
(define (ros_long . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_long.txt"
		    (format "data\\rs_long~a.txt" (car n)))))
	 (fasta-data (edit-fasta data))
	 (connection-inf (overlap-all fasta-data))
	 (res '())
	 )
    (set! res
	  (connect-dna 
	   connection-inf
	   fasta-data))
    
    (call-with-output-file *long_out*
      (lambda(out)
	(displayln res out))
      #:exists 'truncate/replace)
    res
))

;; overlap部分が等しいかはチェックしていない
(define (connect-overlap str1 str2 overlap-len)
  (string-append str1 (string-drop str2 overlap-len)))

;;(define (connect-dnas-0 conn-inf dnalist start)
;;  (if (null? conn-inf)
;;      ""
;;      (let* ((inf-1 (car conn-inf))
;;
;;	     )
(define (get-1st infolist)
  (let ((left (map car infolist))
	(right (map cadr infolist)))

    (car (set-minus left right))))

(define (set-minus set1 set2)
  (filter (lambda(x)(not (member x set2))) set1))

(define (connect-dna2 infolist start dnalist)
  (let ((nextinfo (assoc start infolist)))
    (if (not nextinfo)
	(list-ref dnalist start)
	(let* ((cur  (car nextinfo))
	       (cur-dna (list-ref dnalist cur))
	       (next (cadr nextinfo))
	       (len  (caddr nextinfo)))
	  (string-append (string-drop-right cur-dna len)
			 (connect-dna2 infolist next dnalist))))))

;;
;; (connect-dna2 '((0 1 3)) 0 '("AAATTT" "TTTGAT"))
;;

(define (connect-dna infolist dnalist)
  (connect-dna2 infolist (get-1st infolist) dnalist))
