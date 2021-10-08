;; rosalind
;; 
;;Construct a Profile HMM
;; [BA10E] 2012/06/20 
(require srfi/1)
(require srfi/13)
(require srfi/48)
(include "readfile.ss")
(include "roslib.ss")
(define *DEBUG* #f)
(define *FILE_OUT* #t)
(define *ba10e_out* "ba10e_out.txt")

(define *sigma* '())
(define *aligns* '())
(define *tr-align* '())
(define matA #f)
(define matB #f)
(define *result* '())
(define ins-lines '())
(define match-lines '())
(define *ins0* '())
(define *all-state* '())
;;
;; states S,E,Mn,In,Dn
;;
(define (ros_ba10e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba10e.txt"
		    (format "rs_ba10e~a.txt" (car n)))))
	 ;; threshold
	 (theta (string->number (car data))) 
	 ;;出力記号
	 (sigma (map (lambda(x)(string-ref x 0))
		     (regexp-match* #rx"[A-Za-z]" (third data))))
	 ;;出力記号の種類
	 (sym-cnt (length sigma))
	 ;;出力記号にgapを加えた記号
	 (sigmas (cons #\- sigma))
	 (aligns (map (lambda(y)
			(map (lambda(x)(string-ref x 0))
			     (regexp-match* #rx"[A-Za-z\\-]" y)))
		      (drop data 4)))
	 ;;アライメント後の長さ（共通）
	 (seqlen (length (car aligns)))
	 ;;アライメントの本数
	 (seqcnt (length aligns))
	 ;;転置したアライメント
	 (tr-align (transpose aligns))

	 (all-state '()) ;;全てのstate (S,E,I0を含む)
	 (main-count 0)
	 )
    

    (include "makeprofile.ss")
    
    (set! *sigma*  sigma)
    (set! *aligns* aligns)

    (set! *tr-align* tr-align)
    (set! matA (make-hash))
    (set! matB (make-hash))
    (set! ins-lines (make-hash))
    (set! match-lines (make-hash))
    ;;プロファイルhmmを作る
    
    (make-profile-all tr-align)
    (set! all-state (get-all-state main-count))
    (set! *all-state* all-state)
    
    (set! *result* (outB))

    (call-with-output-file *ba10e_out*
      (lambda(out)
	(for-each (lambda(lis)
		    (for-each (lambda(x)
				(display (format "~a " x) out))
			      lis)
		    (display "\n" out)
		  )
		  *result*))
      #:exists 'truncate/replace)
    
    (outA)
    ;(map displayln  *result*)
    #t
 ))

;; --*--
(ros_ba10e 5)

;;--*-- --*--

  
