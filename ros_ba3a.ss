#lang racket
;; rosalind
;; Generate the k-mer Composition of a String
;; [BA3A] 2021/07/06 AC 
#|
How to execute
>racket
>>(enter! "ros_ba3a.ss")
>>(ros_ba3a 1)
引数の1は　"data\\rs_ba3a1.txt"を対象に操作することを指定する
(ros_ba3a)と引数を省略すると"data\\rosalind_ba3a.txt"(本番用ダウロードファイル)が対象になる
結果は変数*ba3a_out*の値"data\\ba3a_out.txt"で示すファイルに出力される。
|#
(require srfi/13)
(require "readfileA.ss")
(define *ba3a_out* "data\\ba3a_out.txt")

(define (ros_ba3a . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba3a.txt"
		    (format "data\\rs_ba3a~a.txt" (car n)))))
	 (k (string->number (car data)))
	 (x (cadr data))
	 (result (k-mers x k))
       )

    
    
    (call-with-output-file *ba3a_out*
      (lambda(out)
	(for-each (lambda(dna)
		    (displayln dna out))
		  result))
      #:exists 'truncate/replace)
  
  result
))

(define (k-mers dna k)
  (if (= (string-length dna) k)
      (list dna)
      (cons (string-take dna k)
	    (k-mers (string-drop dna 1) k))))


(ros_ba3a 1)				

