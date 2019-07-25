;; zluo2
;; Zhiwen Luo
;; Homework #3

;; CS 480 
;; 11/24/17

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. (2p) How would you represent the edge costs to support your code? Write the chosen data
;; Lisp data structures for both graphs.

(defvar *map1* '((A B 300) (A C 100) (A D 100) (A E 75) 
				 (B C 50) (B D 100) (B E 125) 
				 (C D 75) (C E 125) 
				 (D E 50) ))

(defvar *map2* '((A B 10) (A C 15) (A E 14) (A M 11) (A S 10)
				 (B C 8) (B E 13) (B M 15) (B S 9)
				 (C E 11) (C M 16) (C S 10)
				 (E M 9) (E S 6)
				 (M S 9))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2. (3p) Write a Lisp function InitLoop which takes an initial loop with all cities listed in alphabetical
;; order and produces its random permutation. Your code must produce a real random
;; shuffle of the initial loop.

;;using this shuffle function from the HW2 solution
(defun shuffle (l)
	(do ((oldl l) (newl nil) (i 0 (+ i 1)) (elt 0))
	    ((= i (length l)) newl)
	    (setf elt (random (length oldl)))
	    (setf newl (cons (nth elt oldl) newl))
	    (setf oldl (remove (nth elt oldl) oldl))))	

(defun InitLoop (lst)
	;;init the retlist and templist 
	(let ((retList ()) (tempList ()))
		;;get the random permutation
		(setf tempList (shuffle lst))
		;;add the first element, and add to the end
		(setf retList (append tempList (list (car tempList))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3. (5p) Write a Lisp function LoopCost which computes the cost for a given loop.
(defun LoopCost (lst lstMap)
	;;lst is A - B - C - D - E - A
	;;lstMap is *map1* or *map2*
	;;init totalCost and some lists 
	(let ((totalCost 0) (tempList ()) (tempCost ()) (len (length lst)))
		(do ((i 0 (+ i 1)))
			((= i (- len 1)))
				(progn
					;;get an edge from lst
					(setf tempList (list (elt lst i) (elt lst (+ 1 i))))
					;;get the cost of edge from map
					(setf tempCost (find tempList lstMap :test #' (lambda (x y)
					(or
					(and (eql (first x) (first y)) (eql (second x) (second y)))
					(and (eql (first x) (second y)) (eql (second x) (first y)))))))
					;;calculate the total cost 
					(setf totalCost (+ totalCost (apply #'+ (last tempcost)))) 
				)
		)
		totalCost
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 4. (10p) Write a function ImproveLoop which takes a given loop and improves it if possible. The
;; improvement should use 2-opt heuristics which removes a pair of edges out of the loop and
;; reconnects the loop to lower its cost if possible. There is only one way to reconnect a pair of
;; edges to make a legal loop and it is shown in this figure.

;;2-opt-swap
(defun 2optSwap(lst index1 index2)
	;;example route: A ==> B ==> C ==> D ==> E ==> F ==> G ==> H ==> A
	;;example i(index1) = 4, example k(index2) = 7
	;;new_route:
	(let ((newList ()) (len (length lst)))
		;;(A ==> B ==> C)
		(do ((i 0 (+ i 1)))
			((= i index1))
			(setf newList (append newList (list (elt lst i)))))
		;;A ==> B ==> C ==> (G ==> F ==> E ==> D)
		(do ((i index2 (- i 1)))
			((= i (- index1 1)))
			(setf newList (append newList (list (elt lst i)))))
		;;A ==> B ==> C ==> G ==> F ==> E ==> D (==> H ==> A)
		(do ((i (+ index2 1) (+ i 1)))
			((= i len))
			(setf newList (append newList (list (elt lst i)))))
		newList
	)
)

(defun ImproveLoop (lst lstMap)
	;;lst is the existing route
	;;lstMap is *map1* or *map2*
	;;get the length of lst
	(let ((newLst ()) (len (length lst)))
	;;from 1 to len-3
		(do ((i 1 (+ i 1)))
			((= i (- len 3)))
			;;from 0 to len-2(exclude the last element)
			(do ((j (+ i 1) (+ j 1)))
				((= j (- len 2)))
				(progn
					;;get the new route
					(setf newLst (2optSwap lst i j))
					;;compare the cost of two routes
					(if (> (LoopCost lst lstMap) (LoopCost newLst lstMap))
						(setf lst newLst)
						nil
					)
				)
			)
		)
	)
	lst
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 5. (10p) Write a function SolveTSP which solves TSP given an input graph defined in part 1.
;; Your code should use the functions written in parts 2-4.

(defun SolveTSP (lstMap)
	(let ((retList ()) (tempList ()) (haveBetterSolution T))
		;;get the frist character from the map
		(setf tempList (append tempList (list (first (first lstMap)))))
		;;compare the first character from the map 
		;;if first elememt is equal
		;;then add the second to list
		;;(a b) (a c) (a d) (a e)
		;;a - b - c - d -e
		(dolist (element lstMap)
			(if (eql (first tempList) (first element))
				(setf tempList (append tempList (list (second element))))
				nil
			)
		)
		;;produce random permutation
		(setf retList (InitLoop tempList))
		(print (format nil "random permutaion: ~s" retList))
		(print (format nil "Cost: ~s" (LoopCost retList lstMap)))

		;;using while loop to find local optimal solution
		(loop while haveBetterSolution do
			(progn
				(setf tempList (ImproveLoop retList lstMap))
				;;compare the cost
				(if (> (LoopCost retList lstMap) (LoopCost tempList lstMap))
					(setf retList tempList)
					(setf haveBetterSolution nil)
				)
			)
		)
		;;output the solution and cost
		(print (format nil "Local Best Solution: ~s" retList))
		(format nil "Cost: ~s" (LoopCost retList lstMap))	
	)
)












