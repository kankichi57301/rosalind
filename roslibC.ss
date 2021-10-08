(module rosalind racket/base
	(provide (all-defined-out))
	(require srfi/1)
	(require srfi/13)
	(require "roslibA.ss")
	
	;; test
	(define (test-c n)
	  (* 3 n))

	

	(define (strncmp* str1 str2 n pred)
	  ;(displayln (format "str1=~a" str1))
	  ;(displayln (format "str2=~a" str2))
	  (andmap pred
		  (string->list (string-take str1 n))
		  (string->list (string-take str2 n))))

	(define (char-cmp-gap char1 char2)
	  (or (equal? char1 char2)
	      (equal? char1 #\-  )
	      (equal? char2 #\-  )))


	;;; n>0のときstr2がstr1の末尾からn文字はみ出るように照合
	;;; n<0のときstr2がstr1の先頭から-n文字はみ出るように照合
	;;; str2 が完全にstr1を覆い尽くさないこと
	(define (strcmp-shift str1 str2 . n)
	  (let ((n1 (if (null? n)
			1
			(car n))))
	    (if (>= n1 0)
		(strcmp-shiftA str1 str2 n1)
		(strcmp-shiftB str1 str2 (- n1)))))
       ;;; str2がstr1の末尾からn文字はみ出るように照合
       ;;; str1:       AAAAAAAA
       ;;; str2:           BBBB|BB
       ;;; (n=2の場合)
	(define (strcmp-shiftA str1 str2 n)
	  (let ((overlap-len (- (string-length str2) n)))
	    (strncmp* (string-take-right str1 overlap-len)
		      (string-take       str2 overlap-len)
		      overlap-len
		      char-cmp-gap
		      )))

       ;;; str2がstr1の先頭からn文字はみ出るように照合
       ;;; str1:       AAAAAAAA
       ;;; str2:    BB|BBBB
       ;;; (n=2の場合)

	(define (strcmp-shiftB str1 str2 n)
	  (let ((overlap-len (- (string-length str2) n)))
	    (strncmp* (string-take         str1 overlap-len)
		      (string-take-right   str2 overlap-len)
		      overlap-len
		      char-cmp-gap
		      )))
	

	  
	(define (match-gap char1 char2)
	  (if (char=? char1 char2)
	      char1
	      (if (char=? char1 #\-)
		  char2
		  (if (char=? char2 #\-)
		      char1
		      #\*))))
;;overlapしているstringを連結
	(define (strcat-gap str1 str2)
	  (apply string
		 (map match-gap (string->list str1)(string->list str2))))


(define (paste-shiftA str1 str2 n)
	  (let ((len2 (string-length str2)))
	    (format "~a~a~a"
		    (string-drop-right str1 (- len2 n))
		    (strcat-gap (string-take-right str1 (- len2 n))
				(string-take       str2 (- len2 n)))
		    (string-take-right str2 n))))
	
(define (check&paste str1 str2 . n)
  (let ((n2 (if (null? n)
		1
		(car n))))
    (if (strcmp-shift str1 str2 n2)
	(check&paste0 str1 str2 n2)
      #f)))

(define (check&paste0 str1 str2 n)
      (if (positive? n)
	  (paste-shiftA str1 str2 n)
	  (paste-shiftA str1 str2 n)))
      



  
	

		     

;;(foldl (lambda(x y)(check&paste y x 1)) (car aaa)(cdr aaa))



)  
