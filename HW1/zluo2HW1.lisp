;; zluo2
;; Zhiwen Luo
;; Homework #1

;; CS 480
;; 9/13/17

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1(a)
;; Iterative 

(defun printDot (num)
	(do ((i 1 (+ i 1)))
		;;loop from 1 to num
		((> i num) (format t "~%"))
		;;if i>num then change to new line 
	  (format t ".")))
		;;else 

;; Recursive 
(defun printDot2 (num)
	(if (eql num 0)
		;;check num == 0 or not 
		(format t "~%")
		;;if num == 0 then change to new line
		(progn 
			(format t ".")
			(printDot2 (- num 1)))))
		;;else ouput "." and num-1

;; 1(b)
;; Iterative
(defun finda (list)
  ;; set the counter variable 
  (let ((count 0))  
    (loop for sym in list do
      ;; if the symbol equals a increment the counter 
      (if (eq sym 'a) 
        (setq count (+ count 1) ) 
      ) 
    ) 
    (princ count)
  ) 
)

;; recersive 
(defun finda2 (list)
	(if (null list)
		;;check the list 
		0
		(if (eql 'a (car list))
			;;if the first symbol equal to 'a
			(+ 1 (finda2 (cdr list)))
			;;add 1 
			(finda2 (cdr list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2

;; (a) (defun summit (lst)
;;        (remove nil lst)
;;		  (apply #'+ lst))

;; This function fails due to the remove function not modifying
;; the actual list variable

;;correct version:
(defun summit (list)
	   (progn 
	     (remove nil list)
	     (apply #'+ list)))

;;(b) (defun summit (lst)
;;         (let ((x (car lst)))
;;	     	(if (null x)
;;			 (summit (cdr lst))
;;			 (+ x (summit (cdr lst))))))

;; This function causes an infinite loop because
;; the recursive function does not have a base case

;;correct version:
(defun summit2 (lst) 
  (let ((x (car lst))) 
  (progn (if (null lst) 
    0 
    (if (null x) 
      (summit2 (cdr lst)) 
      (+ x (summit2 (cdr lst))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3

;;(a) recursion
(defun pos2+ (lst)
  (if (null lst)
  	;;if the list is empty we'll just return it
      ()
    (let ((len (length lst)))
    	;;let the len == length of list
      (cons (- (+ (car (last lst)) len) 1) (pos2+ (subseq lst 0 (- len 1)))))))
		;;get the num from the list and add it from large to small order

(defun pos+ (lst)
  (reverse (pos2+ lst)))
	;;reverse and output the list from the pos2

;;(b) Iteration
(defun pos3+ (list) 
  (let ((num 0)) 
  (loop for x in list do 
    ;; increment the value at the index by the counter
    (setf (nth num list) (+ x num))
     (setq num (+ num 1)))) 
   	(princ list)
)

;; (c) mapcar
(defun pos4+ (list) 
  (let ((applist () ))
    ;; build a list of the index we want to apply 
    (loop for x from 0 to (length list) do 
      (setq applist (append applist (list x)) ))
        ;; use mapcar to add the list to the passed in list  
        (mapcar #'+ list applist)  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4

(let ((max 0))
(defun f (num)
	(if (> num max)
		;;if the number is greater than max 
		(setq max num))
		;; set max = num
		max
		)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5

;; extract the prefix from the exploded characters 					      
(defun GetPre (word len) 
	(let ((pre () )) 
  	(loop for x from 0 to (- len 1) 
  		do (setq pre (append pre (list (nth x word)))) 
  			) pre))

(defun lookup (pre dic) 
  (let ((returnList () ))
  ;; just return the list 
  (if (null dic) returnList
    ;; check if the prefix matches  
    (if (equal pre (GetPre (first dic) (length pre) ) )
      ;; if the prefix matches 
      ;; return the list plus the appended dictionary value 
      (progn 
      	(setf returnList (append (lookup pre (rest dic)) (list (first dic)) ) )  returnList )
      ;; Or just return the list 
      (progn 
      	(setf returnList (lookup pre (rest dic))) returnList) 
    )
  )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6

(defun occurrences (lst) 
  (let ((count 0) (appList () ))
    ;; the list isn't empty
    (loop while (not (null lst) ) do
      ;; each element in the list 
      (loop for x in lst do
        ;; elements are equal 
        (if (eql (first lst) x )
          ;; increment the count 
          (setq count (+ count 1) ) ))
        ;; add the result to the list
        (setq appList (
        	append appList (list (list (first lst) count)) ) )
        ;; remove the elements we just found 
        (setq lst 
        	(remove (first lst) lst) ) 
        ;; reset 
        (setq count 0)
    )
    ;; sort the list  
    (sort appList #'(lambda (x y) (> (second x) (second y) ) ) )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7

(defun find-fun (element) 
  (let ((returnList () ))
    ;; empty - return the list 
    (if (null element) returnList
      ;; find F U N in it
      (if (search '(F U N) (first element) )
        ;; the result contains F U N - add it to the list recursivly 
        (progn 
        	(setf returnList 
        		(append (find-fun (rest element)) (list (first element) ) ) ) 
        	returnList)
        ;; Or just return the list retursivly 
	(progn 
		(setf returnList (find-fun (rest element))))
      )
    ) 
  ) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8

;; mapping the hex non-numerics - number representation
(defun getNum(lst)
  ;; map the symbols to numbers 
  (let ( 
  	(Val lst) (temp (mapcar #'(lambda (x y) (list x y)) '(A B C D E F) '(10 11 12 13 14 15) ) )) 
  (loop for x in temp do
    ;; search for the characters number. 
    (if (eq (first x) lst ) 
      (setq Val (nth 1 x)) 
    ) 
  )
  Val 
  ) 
)

(defun conv16 (lst &optional (num 0))
  ;; if the list length is equal to one
  ;; just mutiply
  (if (eql 1 (length lst)) 
    (+ num (getNum (nth 0 lst)))
    ;; otherwisewe recurse and convert using the formula
    (progn (setq num (+ num (getNum (first lst) ) ) ) 
      (setq num (* num 16)) (conv16 (rest lst) num) ) 
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0 (extre credit)

(defun wrapperFunction ()

  ;; 1a iterative
  (princ "Question 1A-iterative:")
  (format t "~%")
  (printDot 0)  
  (format t "~%") 
  (printDot 1)  
  (format t "~%") 
  (printDot 2)  
  (format t "~%") 
  (printDot 3)  
  (format t "~%") 
  (printDot 4)  
  (format t "~%")

  (format t "~%")

  ;; 1a recursive
  (princ "Question 1A-recursive:")
  (format t "~%")

  (printDot2 0)  
  (format t "~%") 
  (printDot2 1)  
  (format t "~%") 
  (printDot2 2)  
  (format t "~%") 
  (printDot2 3)  
  (format t "~%") 
  (printDot2 4)  
  (format t "~%")
  
  (format t "~%")

  ;; 1b iterative
  (princ "Question 1B-iterative:")
  (format t "~%")

  (let ((a '(a b c e a d a)) (b '(a a a d a a) ) (c '() )  (d '(k e) ) (e '(a b c a a a c c b a)))
    (finda a)
    (format t "~%")
    (finda b)
    (format t "~%")
    (finda c)
    (format t "~%")
    (finda d)
    (format t "~%")
    (finda e)
    (format t "~%")
  )

  (format t "~%")

    ;; 1b recursive
    (princ "Question 1B-recursive:")
    (format t "~%")

    (let ((a '(a b c e a d a))) 
      (princ (finda2 a) ) )
    (format t "~%")
    (let ((b '(a a a a d a)))
      (princ (finda2 b) ) )
    (format t "~%")
    (let ((c '()))
      (princ (finda2 c) ) )
    (format t "~%")
    (let ((d '(k t y u i o e )))
      (princ (finda2 d) ) )
    (format t "~%")
    (let ((e '(a b c a a c a c b a) ))
      (princ (finda2 e) ) )
    (format t "~%")

  (format t "~%")
  (format t "~%")

  ;; 2a
  (princ "Question 2A:")
  (format t "~%")

  (princ (summit '(1 2 3 4 5 6))) ;; 21     
  (format t "~%")
  (princ (summit '(0 1))) ;; 1     
  (format t "~%")
  (princ (summit '())) ;; 0  
  (format t "~%")
  (princ (summit '(1 2))) ;; 3   
  (format t "~%")
  (princ (summit '(3 3 3 3 3 4))) ;; 19     
  (format t "~%")

  (format t "~%")

  ;; 2b
  (princ "Question 2B:")
  (format t "~%")

  (princ (summit2 '(1 2 3 4 5 6))) ;; 21     
  (format t "~%")
  (princ (summit2 '(0 1))) ;; 1     
  (format t "~%")
  (princ (summit2 '())) ;; 0  
  (format t "~%")
  (princ (summit2 '(1 2))) ;; 3    
  (format t "~%")
  (princ (summit2 '(3 3 3 3 3 4))) ;; 19    
  (format t "~%")

  (format t "~%")
  (format t "~%")
  ;; number three modifies actual data
  ;; 3a
  (princ "Question 3A:")
  (format t "~%")

  (princ (pos+ '(1 2 3 4 5 6))) 
  (format t "~%") 
  (princ (pos+ '(5 4 3 2 1 1))) 
  (format t "~%")
  (princ (pos+ '(1 1 1 1 1 1 1 1 1))) 
  (format t "~%")
  (princ (pos+ '()) )
  (format t "~%")
  (princ(pos+ '(1 0 -1 -2 4))) 
  (format t "~%")

  (format t "~%")

  ;; 3b
  (princ "Question 3B:")
  (format t "~%")

  (let ((a '(1 2 3 4 5)))
    (pos3+ a)) 
  (format t "~%")
  (let ((b '(6 5 4 3 2 1 0))) 
    (pos3+ b)) 
  (format t "~%")
  (let ((c '(1 1 1 1 1)))
    (pos3+ c)) 
  (format t "~%")
  (let ((d '(0 0 0 0 0)))
    (pos3+ d) )
  (format t "~%")
  (let ((e '(9 5 7 1 0)))
    (pos3+ e)) 
  (format t "~%")

  (format t "~%")

  ;; 3c
  (princ "Question 3C:")
  (format t "~%")
  (princ (pos4+ '( 1 2 5 6 3)))  
  (format t "~%")
  (princ (pos4+ '( 1 0 10 10)))  
  (format t "~%")
  (princ (pos4+ '( 10 9 8 7 6)))  
  (format t "~%")
  (princ (pos4+ '( 2 2 2 2 2 2 2)))  
  (format t "~%")
  (princ (pos4+ '( 0 )))  
  (format t "~%")

  (format t "~%")
  (format t "~%")

  ;; 4
  (princ "Question 4:")
  (format t "~%")

  (princ (f 0)) 
  (format t "~%")
  (princ (f 3)) 
  (format t "~%")
  (princ (f 4)) 
  (format t "~%")
  (princ (f 101)) 
  (format t "~%")
  (princ (f 52)) 
  (format t "~%")
  (princ (f 100000)) 
  (format t "~%")
  (princ (f 454)) 
  (format t "~%")

  (format t "~%")
  (format t "~%")

  ;;5
  (princ "Question 5:")
  (format t "~%")

  (let ((prefix '(S E T)) (dictionary '((S E T Q) (S E T F) (R E T V E R S E) (C A T) )) )
    (princ (lookup prefix dictionary))
    (format t "~%")
  )

  (let ((prefix '(S)) (dictionary '((S E T Q) (S E T F) (R E V E R S E) (C A T) (S E T) (R E A D))) )
    (princ (lookup prefix dictionary))
    (format t "~%")
  )

  (let ((prefix '(D E)) (dictionary '((S E T Q) (D E F U N) (D E M) (S E T F) (D E) (R E V E R S E) (C A T) )) )
    (princ (lookup prefix dictionary))
    (format t "~%")
  )

  (let ((prefix '(S I M)) (dictionary '((S E T F) (S I M) (R E V E R S E) (C A T) )) )
    (princ (lookup prefix dictionary))
    (format t "~%")
  )

  (let ((prefix '(S E)) (dictionary '((R E A D) (S E F) (R E V E R S E) (C A T) )) )
    (princ (lookup prefix dictionary))
    (format t "~%")
  )

  (format t "~%")
  (format t "~%")

  ;; 6
  (princ "Question 6:")
  (format t "~%")

  (princ (occurrences '(a b c c a d d)))
  (format t "~%")

  (princ (occurrences '(a b c b a d d a ca d e t h c)))
  (format t "~%")

  (princ (occurrences '()))
  (format t "~%")

  (princ (occurrences '(d d d a a a  d d d d)))
  (format t "~%")

  (princ (occurrences '(a b c a b c a a b a c c b c)))
  (format t "~%")

  (format t "~%")
  (format t "~%")

  ;; 7
  (princ "Question 7:")
  (format t "~%")

  (princ (find-fun '( (D E F U N) (U P) (F U N))))
  (format t "~%")

  (princ (find-fun '( (D E F U N) (M O R E F U N) (U P) (D O W N) (FUN) (N O T F U N) )))
  (format t "~%")

  (princ (find-fun '( () () () ())))
  (format t "~%")

  (princ (find-fun '( (D O W N) (D E F U N) (L E F T) (U P) (R I G H T) )))
  (format t "~%")

  (princ (find-fun '( (D E F U N) (D E F U N) (F U N) (R U N))))
  (format t "~%")

  (format t "~%")
  (format t "~%")

  ;; 8
  (princ "Question 8:")
  (format t "~%")
  (princ "7cd = ")
  (princ (conv16 '(7 c d)))
  (format t "~%")

  (princ "02 = ")
  (princ (conv16 '(0 2)))
  (format t "~%")

  (princ "fa = ")
  (princ (conv16 '(f a)))
  (format t "~%")

  (princ "7dbb = ")
  (princ (conv16 '(7 d b b)))
  (format t "~%")

  (princ "fff333 = ")
  (princ (conv16 '(f f f 3 3 3)))
  (format t "~%")

  (format t "~%")
)
