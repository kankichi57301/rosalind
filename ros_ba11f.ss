#lang racket
;; rosalind
;; Implement PSMSearch
;; [BA11F] 2022/01/11 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(require "monoisotopicA.ss")
(define *ba11f_out* "data\\ba11f_out.txt")
(define *answers* '())

(define (ros_ba11f . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba11f.txt"
		    (format "data\\rs_ba11f~a.txt" (car n)))))
	 (spects (map (lambda(x)(map string->number (string-tokenize x)))(drop-right data 1)))
	 (proteome (car (take-right data 1)))            ;;proteome string
	 (protcharlist (string->list proteome))
	 ;;(T (string->number(car (take-right data 1))))   ;;threshold
	 (res '())
	 )
    ;(displayln (format "spects=~a" spects))
    ;(displayln (map length spects))
    
    ;(displayln (format "ptoteome=~a" protcharlist))
        
    (define (PSM0)
	(set! *answers* (make-hash))
	(for-each (lambda(spect)
		    (PSM1 spect (length spect) protcharlist 0 0 0))
		  spects))


    (define (PSM1 spect spectlen  prot wt-runsum sc-runsum len)
      ;;(displayln (format "spectlen=~a prot=~a sum=~a sc=~a" spectlen (take prot len) wt-runsum sc-runsum))
      
      (if (null? prot)                                       ;; when scan end
	  #t                                                 ;; ==> exit 
	  (if (or (>= len (length prot))(> wt-runsum spectlen))
	      (PSM1  spect spectlen (cdr prot)  0 0 0)

	      (let  ((wt-runsum2 (+ wt-runsum (cdr (assoc (list-ref prot len) amw5)))))

		(if (< wt-runsum spectlen)
		    (if   (<= wt-runsum2 spectlen)
			  (PSM1 spect spectlen prot wt-runsum2 (+ sc-runsum (list-ref spect (- wt-runsum2 1))) (+ len 1))
			  (PSM1 spect spectlen (cdr prot)  0 0 0))
;;match weight
		    (begin 
		      (when (= wt-runsum spectlen)
			    ;;(hash-set! *answers* (apply string (take prot len)) `(,wt-runsum ,sc-runsum)))
			    (register-ans (take prot len) wt-runsum sc-runsum))
		      (PSM1  spect spectlen (cdr prot)  0 0 0)))))))
		
    (define (register-ans prot wt sc)
      (let ((prev (hash-ref *answers* wt #f)))
	(if (not prev)
	    (hash-set! *answers* wt `(,(apply string prot) ,sc))
	    (when (< (cadr prev) sc)
		  (hash-set! *answers* wt `(,(apply string prot) ,sc))))))
    (PSM0)
    (set! res (map car (hash-values *answers*)))
    
    
    (call-with-output-file *ba11f_out*
      (lambda(out)
	(for-each (lambda(x)
		    (displayln (format "~a" x) out))
		  res))
      #:exists 'truncate/replace)
    
    res
	))

;;(ros_ba11f 1)









  
