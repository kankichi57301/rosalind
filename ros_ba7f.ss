#lang racket
;; rosalind
;; Implement SmallParsimony
;; [BA7F] 2021/11/03 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(require mzlib/string)
;;(require "roslibB.ss")

(define *adj-list* '())
(define *leaves-alist* '())
(define *ba7f_out* "data\\ba7f_out.txt")
(define *result* #f)

(define *dnalen* 0)
(define *leaves-count* 1)
(define *node-dic* '())
(define *root* #f)
(define *output* #f)

(define (ros_ba7f . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba7f.txt"
		    (format "data\\rs_ba7f~a.txt" (car n)))))
	 (nleaf (string->number (car data)))
	 (root 0)
	 (sumhd 0)
	 (res '())
	 )
    (set! *output* (make-hash))
    (set! *dnalen* (string-length (regexp-replace #rx"([0-9]*)->([0-9ACGT]*)" (cadr data) "\\2")))    
    (set! *adj-list*
     (map (lambda(x)
	    (read-from-string
	     (regexp-replace #rx"([0-9]*)->([0-9ACGT]*)" x "(\\1 \\2)")))
	  (cdr data)))
    (displayln (format "dna len=~a" *dnalen*))
    ;(displayln *adj-list*)
    (set! root (get-root *adj-list*))
    (set! *node-dic* (make-hash))
    (adjlist->tree *adj-list*)
    (set! *root* (hash-ref *node-dic* root))
    (set! sumhd (apply + (map (lambda(i)(send *root* alph0 i #f))(iota *dnalen*))))
    (displayln sumhd ) 
    (send *root* disp-2gen)
    (set! res (hash-keys *output*))
        
    
    (call-with-output-file *ba7f_out*
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
	 (define/public (init-inner-dna n) ;; "---"
	   (when (not (null? child))
	       (begin
		 (set! dna (make-string n #\-))
		 (send (car  child) init-inner-dna n)
		 (send (cadr child) init-inner-dna n))))
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
		  (hamming-distance dna (get-field dna (car  child)))
		  (hamming-distance dna (get-field dna (cadr child))))))
			 
			 
;;;--*-- end of methods --*--
	 (super-new)
	 ))
;;ルートを求める
(define (get-root adjlist)
  (car
   (exclude (delete-duplicates (map cadr adjlist))
	    (delete-duplicates (map car adjlist)))))

(define (leaf->hd4 char)
  (case char
    [(#\A ) '(0 1 1 1)]
    [(#\C ) '(1 0 1 1)]
    [(#\G ) '(1 1 0 1)]
    [(#\T ) '(1 1 1 0)]
    ;;[(#\- ) '(1 1 1 1)]
    ))

(define (make-inner-node num)
  ;(displayln (format "make-inner ~a" num))
  (let ((my-instance (new bintree-node-class%)))
    (set-field! name my-instance num)
    (set-field! dna  my-instance  (make-string *dnalen* #\-))
    (hash-set! *node-dic* num my-instance)
    my-instance
    ))

(define (make-leaf-node dna)
  ;(displayln (format "make-leaf ~a" dnastr))
  (let ((my-instance (new bintree-node-class%)))
    (set-field! name my-instance (format "L~a" *leaves-count*))
    (set! *leaves-count* (+ 1 *leaves-count*)) 
    (set-field! dna  my-instance (symbol->string dna))
    my-instance
    ))
;; 親子のpairの隣接リストから木を作る。

(define (adjlist->tree0 pair)
  (let ((parent '())
	(chld   '()))
    (set! parent (hash-ref *node-dic* (car  pair) #f))
    (when (not parent)
	  (set! parent (make-inner-node (car pair))))

    (set! chld   (hash-ref *node-dic* (cadr pair) #f))
    (when (not chld)
	  (set! chld (if (number? (cadr pair))
			 (make-inner-node (cadr pair))
			 (make-leaf-node  (cadr pair)))))
    (set-field! child parent (cons chld (get-field child parent)))))

;;(adjlist->tree0 '(4 "CAAATCCC"))

(define (adjlist->tree pairlist)
  (for-each adjlist->tree0 pairlist))
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
