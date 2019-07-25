;; zluo2
;; Zhiwen Luo
;; Homework #2

;; CS 480 
;; 10/2/17

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part 1

;; In the first part you will write A function for checking whether 
;; an arbitrary permutation of n numbers solves n-queens problem.

;; You can write a function that checks if a board is
;; legal. A legal board configuration cannot have two queens in
;; the same column or diagonal. Two queens ci and cj (in rows i and j)
;; share a column if ci=cj; they share a diagonal if |ci-cj|=|i-j|,
;; where i,j=1,2,3,4 and |x| is the absolute value of x. Note that if you use
;; permutations as potential board configurations then you only need to check
;; if queens share a diagonal.

(defun check-list (lst)
	;;get the length of list and init conflict num 
	(let ((len (length lst)) (conflict 0))
		;;using two loops to check the list
		(do ((i 0 (+ i 1)))
			((= i (- len 1)))
			(do ((j (+ i 1) (+ j 1)))
				((= j len))
				;;if equal, this list is illeagl 
				(if (eq (abs (- (nth i lst) (nth j lst))) (abs (- i j)))
					(progn
						;;illeagl then add conflict number 
						(setf conflict (+ conflict 1))
						(return)
					)
				)
			)
		)
	conflict)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part 2

;; In the second part you will write a program to
;; generate all solutions of n-queens for (n=4,5, â€¦,10). You should only 
;; display solution for n<6 and report how many solutions there are for n>=6.

;; Generate configurations that correspond to permutations of 1..n. 
;; For example, (1,2,3,4) would have 24 permutations, 1..5 would have 120, 
;; 1..n would have n! (=1*2*3*...*n). You can generate all permutations of 
;; numbers 1..2 by starting from ((1)) and inserting 2 in all possible 
;; positions to obtain ((2 1) (1 2)). Similarly, you obtain all permutations 
;; of 1..3 by starting from ((2 1) (1 2)) and inserting 3 in all possible 
;; positions in each of the length-two lists to obtain ((3 2 1) (2 3 1) 
;; (2 1 3) (3 1 2) (1 3 2) (1 2 3)). Similarly, given all permutations of 
;; 1..n-1 you can create all permutations of 1..n. Hint: Use 'subseq' lisp 
;; function. Can you solve n-queens for n=4 . . .,10? How about n>10? How many 
;; solutions to n-queens are there for n>=4? 


(defun permutation (lst)
	;;check the list first 
	(if (null lst)
		nil
		;;using the cond to generate the permutation
		(cond ((null (cdr lst)) (list lst))
			;;if the rest of list is empty, then combinate this element 
			  (t (loop for element in lst
			  ;;else, using the loop to get the each element in list to generate the permutation
			  	;;(append '(1 2 3) '(4 5 6)) => (1 2 3 4 5 6)
			  	   append (mapcar (lambda (l) (cons element l))
			  	   	;;(mapcar (lambda (x) (+ x 10)) '(1 2 3 4)) => (11 12 13 14)
			  	   			(permutation (remove element lst))
			  	   			;;recursive the permutation
			  	   			)
			  	   )
			  )
		)
	)
)
							 


(defun generate-allsolution (num)
	;;init the list and count 
	(let ((retList ()) (tempList ()) (allList ()) (Solution 0))
		;;special situation (num == 1)
		(if (= num 1)
			(setq retList (list '1 '1))
		(progn
			;;generate the list from the num 
			(do ((i 1 (+ i 1)))
				((> i num))
				(setf tempList (append tempList (list i))))
			;;get the alllist through permutation
			(setf allList (permutation tempList))
			;;using the loop to check the alllist 
			(let ((len (length allList)))
				 (do ((i 0 (+ i 1)))
					((= i (- len 1)))
					;;if the list is legal 
					(if (= (check-list(nth i allList)) 0)
						(progn
							;;append the list together 
							(setq retList (cons (nth i allList) retList))
							;;add the solution number
							(setq Solution (+ Solution 1)))
						)
					)
				 )
			)
		)
	 	(if (> num 5)
	 		;;if num >=6 then return solution number 
	 		Solution
	 		;;else return the list 
	 		retList)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part 3

;; In the third part you will write a more efficient implementation that will 
;; search for a single placement of n-queens on the board. A detailed 
;; description of the problem follows.

;; In this part you will generate random permutations of n queens and
;; check if they correspond to legal boards. You can stop as soon as you find a legal
;; position. You will start by writing a function 'shuffle' that takes a list 
;; (1 2 3 ... n) and returns a list with the same elements in a random order (5p for 
;; this part). To generate a random shuffle you pick one number from (1â€¦n) at random and 
;; move it to a new list, then you pick another number randomly from the shortened list and
;; add it to the new list. Continue until you have moved all numbers from the old 
;; list to the new one. You will then run this list for increasing values of n to obtain solutions 
;; of the n-queens problem. How many times do you need to shuffle to obtain solutions for
;; n=4,...,10? 

;;Do you get the same answer for different runs? 

;;No, I don't the same answer for different runs.

;; Can your program handle n=11,12,13,14,15,...?  How large n can your program handle?

;;Yes, very quick.

(defun array-shuffle (arr)
	;;shuffle array ARR in-place 
  (do ((i (length arr) (1- i)))
      ((= i 2) arr)
    (rotatef (aref arr (random i))
	     (aref arr (1- i)))))

(defun list-shuffle (lst)
	;; shuffle list lst in-place copying from a shuffled vector copy
  (let ((array (array-shuffle (coerce lst 'vector))))
    (declare (dynamic-extent array))
    (map-into lst 'identity array)))

(defun get-solution (num)
	;;init the list and parameter
	(let ((randomTime 0) (tempList ()) (temp 0))
		(progn 
			;;generate the templist from the num 
			(do ((i 1 (+ i 1)))
				((> i num))
				(setf tempList (append tempList (list i))))
			;;using a loop to generate random permutations of n queens
			(do ((i 1 (+ i 1)))
				((< i 0))
				(progn
					;; check if they correspond to legal boards
					(setq temp (check-list(list-shuffle tempList)))	
					(if (= temp 0)
							(progn 
								;;randomtime ++ and stop
								(setq randomTime (+ randomTime 1))
								(return)
							)
							(setq randomTime (+ randomTime 1))
					)
				)
			)
		)randomTime
	)
)


















