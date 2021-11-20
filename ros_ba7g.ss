#lang racket
;; rosalind
;; Adapt SmallParsimony to Unrooted Trees
;; [BA7G] 2021/11/14 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(require "roslibB.ss")
(require mzlib/string)

(define *adj-list* '())
(define *leaves-alist* '())
(define *ba7g_out* "data\\ba7g_out.txt")
(define *result* #f)

(define *dnalen* 0)
(define *leaves-count* 1)
(define *all-nodes* '())
(define *node-dic* '())
(define *root* #f)
(define *output* #f)

(define (ros_ba7g . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba7g.txt"
		    (format "data\\rs_ba7g~a.txt" (car n)))))
	 (nleaf (string->number (car data)))
	 (root 0)
	 (sumhd 0)
	 (res '())
	 )
    (set! *output* (make-hash))
    
    (set! *adj-list*
     (map (lambda(x)
	    (read-from-string
	     (regexp-replace #rx"([0-9AGCT]*)->([0-9ACGT]*)" x "(\\1 \\2)")))
	  (cdr data)))
    (set! *all-nodes* (delete-duplicates (apply append *adj-list*)))
    (set! root (find-first number? *all-nodes*))
    ;;(displayln (format "root=~a" root))
    (set! *dnalen* (string-length (symbol->string (find-first symbol? *all-nodes*))))
    ;;(displayln (format "dna len=~a" *dnalen*))
    ;;(displayln *adj-list*)
    

    (set! *node-dic* (make-hash))
    
    (make-bin-tree root)
    (set! *root* (hash-ref *node-dic* root))
    
    
    (set! sumhd (apply + (map (lambda(i)(send *root* alph0-root i))(iota *dnalen*))))
    (displayln sumhd ) 
    (send *root* disp-2gen)
    (set! res (hash-keys *output*))
    res    
    
    (call-with-output-file *ba7g_out*
      (lambda(out)
	(displayln sumhd out)
	(for-each (lambda(x)
		    (displayln x out))
		  res))
      #:exists 'truncate/replace)
    
    ))

(define bintree-node-class%
  (class object%
	 (field
	  (name "")
	  (dna "")
	  (child '())
	  (from  '())
	  )
	 
	 (define/public (test01)
	   (displayln (format "~a[~a]" name dna))
	   )
	 
	 (define/public (hd-min-char-root n)
	   (let ((it2
		  (calc-tree-hd3 (send (car   child) hd-min-char n)
				 (send (cadr  child) hd-min-char n)
				 (send (caddr child) hd-min-char n)
				)))
	     (set! from (map cdr it2))
	     (map car it2)))
	 
	 
	 (define/public (hd-min-char n)
	   (let ((it 
		  (if (null? child)     ;; leaf node
		      (begin
			;;(displayln (format "dna =~a[~a]" dna (string-ref dna n)))
			(leaf->hd4 (string-ref dna n))
		      )
		      (let ((it2
			     (calc-tree-hd (send (car  child) hd-min-char n)
					   (send (cadr child) hd-min-char n))))
			(set! from (map cdr it2))
			(map car it2)))))
	     it))
	 
	 ;;
	 ;; 1st arg : dna position
	 ;; 2nd arg : nuc 0 A ,1 C ,2 G ,3 T 4 - ,root:#f
	 ;;
	 (define/public (alph0 pos nuc)
	   ;;(displayln (format "send alph=~a" name))
	   (let ((this-nuc nuc)
		 (minhdsum 0)
		 )
	     (when (not nuc)                           ;; root
		   (let ((root-enki-inf (hd-min-char pos)))   ;;side effect で子nodeの処理も行う
		     (set! this-nuc (min-index-of root-enki-inf identity))
		     (set! minhdsum (apply min root-enki-inf))))
	       ;; except root
	     (when (not (null? child))     ;; not leaf
		   (begin
		     ;;(displayln (format "spec=~a[~a] = ~a" name pos (list-ref all-enki this-nuc))) 
		     (string-set! dna pos (list-ref all-enki this-nuc))	;;* 2021/10/30     
		     (send (car  child) alph0 pos (car  (list-ref from this-nuc)))
		     (send (cadr child) alph0 pos (cadr (list-ref from this-nuc)))
		     ))
	     minhdsum
	     )
	   )
	 ;; root用
	 (define/public (alph0-root pos)
	   ;;(displayln (format "send alph root=~a" name))
	   (let* (
		  (minhdsum 0)
		  (root-enki-inf (hd-min-char-root pos))
		  (this-nuc (min-index-of root-enki-inf identity))
		  )
	     root-enki-inf
	     
	     (set! minhdsum (apply min root-enki-inf))
	   ;;(displayln (format "spec=~a[~a] = ~a" name pos (list-ref all-enki this-nuc))) 
	     (string-set! dna pos (list-ref all-enki this-nuc))	;;* 2021/10/30     
	     (send (car   child) alph0 pos (car   (list-ref from this-nuc)))
	     (send (cadr  child) alph0 pos (cadr  (list-ref from this-nuc)))
	     (send (caddr child) alph0 pos (caddr (list-ref from this-nuc)))
	     minhdsum
	     )
	   )
	 
	 
	 ;; alph0の後で呼ぶ
	 (define/public (disp-inner-node port)  
	   (when (not (null? child))
		 (displayln (format ">~a~%~a" name dna) port)
		 (send (car  child) disp-inner-node port)
		 (send (cadr child) disp-inner-node port)))

	 ;;for Parsimony
	 
	 (define/public (disp-2gen)
	   (when (not (null? child))
		 (let ((child-dnas (map (lambda(x)(get-field dna x))child)))
		   (for-each
		    (lambda(c-dna)
		      (hash-set! *output* (format "~a->~a:~a" dna c-dna (hamming-distance dna c-dna)) #t)
		      (hash-set! *output* (format "~a->~a:~a" c-dna dna (hamming-distance dna c-dna)) #t))
		      child-dnas))
		 (for-each (lambda(x)(send x disp-2gen)) child)))
		   
	 (define/public (test02)
	   (append-map (lambda(x)(send x disp-2gen)) child))
	  
	 ;;検算用上記の後で呼ぶ
	 (define/public (check-hamming)
	   (if (null? child)
	       0
	       (+ (apply + (map (lambda(x)(send x check-hamming)) child))
		  (apply + (map (lambda(x)(hamming-distance dna (get-field dna x))) child))
	       )))
			 
			 
;;;--*-- end of methods --*--
	 (super-new)
	 ))



(define (leaf->hd4 char)
  (case char
    [(#\A ) '(0 1 1 1)]
    [(#\C ) '(1 0 1 1)]
    [(#\G ) '(1 1 0 1)]
    [(#\T ) '(1 1 1 0)]
    ;;[(#\- ) '(1 1 1 1)]
    ))
(define (make-bintree-node item)
  (let ((my-instance (new bintree-node-class%)))
    (if (symbol? item)
	(begin
	  (set-field! name my-instance (format "L~a" *leaves-count*))
	  (set-field! dna  my-instance (symbol->string item))
	  (set! *leaves-count* (+ 1 *leaves-count*)))
	(begin
	  (set-field! name my-instance item)
	  (set-field! dna  my-instance  (make-string *dnalen* #\-)))
	)
    (hash-set! *node-dic* item my-instance)
    my-instance
    ))

(define (make-bin-tree root)
  (for-each make-bintree-node *all-nodes*)
  (connect-bintree root #f)
  )


(define (connect-bintree node par)
  ;;(displayln (format "connect=~a" node))
  (when (member node *all-nodes*)
	(let ((chld (map cadr (filter (lambda(pair)(and (equal? (car pair) node)
					                (not (equal? (cadr pair) par))))
					     *adj-list*))))
	  (when (not (null? chld))
		;;(displayln (format "child =~a" chld))
		(let ((my-instance (hash-ref *node-dic* node)))
		  (set-field! child my-instance (map (lambda(x)(hash-ref *node-dic* x)) chld)))
		(for-each (lambda(x)(connect-bintree x node)) chld)))))
;;
;; 4つ組 (当該サブツリーの根をAにしたときのサブツリー全体のハミング距離の和
;;                       C
;;                       G
;;                       T
;;      )
;; 子ノード２つの４つ組から親ノードの４つ組を求める
;; 
(define (calc-tree-hd hdlist1 hdlist2)
  (let ((ret 0)
	(hd  0)
	(jmax -1)
	(kmax -1)
	(retval '())
	)
    (set! retval
	  (map (lambda(i)
		 (set! ret 999999)
		 (for ([j '(0 1 2 3)][lefthd hdlist1])
		      (for ([k '(0 1 2 3)][righthd hdlist2])
			   (set! hd (+ lefthd righthd (if(= i j) 0 1)(if (= i k) 0 1)))
			   (when (< hd ret)
				 (begin
				   (set! ret hd)
				   (set! jmax j)
				   (set! kmax k)))))
		 (list ret jmax kmax)
	       )
	       (iota 4)))
    retval))

;(ros_ba7f 1)
;(define x1 (car (get-field child *root*)))
;(define x11 (car (get-field child x1)))
;;
;;3引数版 toot専用
;;
(define (calc-tree-hd3 hdlist1 hdlist2 hdlist3)
  (let ((ret 0)
	(hd  0)
	(jmax -1)
	(kmax -1)
	(lmax -1))
    ;;(displayln (format "hd3 ~a:~a:~a" hdlist1 hdlist2 hdlist3))
    (map (lambda(i)
	   (set! ret 999999)
	   (for ([j '(0 1 2 3)][lefthd hdlist1])
		(for ([k '(0 1 2 3)][midhd hdlist2])
		     (for ([l '(0 1 2 3)][righthd hdlist3])
			  (set! hd (+ lefthd midhd righthd (if(= i j) 0 1)(if (= i k) 0 1)(if (= i l) 0 1)))
			  (when (< hd ret)
				(begin
				  (set! ret hd)
				  (set! jmax j)
				  (set! kmax k)
				  (set! lmax l)
				  )))))
	   (list ret jmax kmax lmax)
	   )
	 (iota 4))
    ))

#|
(ros_ba7g 1)
(define ch (get-field child *root*))
(define c1 (car ch))
(define c2 (cadr ch))
(define c3 (caddr ch))
;;(define xac (map car (calc-tree-hd '(0 1 1 1) '(1 0 1 1))))
;;(define xag (map car (calc-tree-hd '(0 1 1 1) '(1 1 0 1))))
;;(define xat (map car (calc-tree-hd '(0 1 1 1) '(1 1 1 0))))
|#
