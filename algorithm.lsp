;; ------------------------------
;;     USEFUL FUNCTIONS
;; ------------------------------

(defun are_states_equal (state1 state2)
	(let ((flag T))
		(loop for i from 0 to 3 do 
			(if (= i 1)
				(when (not (eq (nth i state1) (nth i state2)))
					(setq flag NIL)
				)
				(let ((number_missionaries1 (length (remove 'C (nth i state1))))
					(number_cannibals1 (length (remove 'M (nth i state1) )))
					(number_missionaries2 (length (remove 'C (nth i state2))))
					(number_cannibals2 (length (remove 'M (nth i state2) ))))

					(when (not (AND 
								(eq number_cannibals1 number_cannibals2) 
								(eq number_missionaries1 number_missionaries2)))
						(setq flag NIL)			
					)
				)
			)
		)
		flag
	)
)


(defun is_state_in_list (state list)
	(loop for l in list do
		(when (are_states_equal l state)
			(return T))
	)
)

(defun delete-nth (n list)
	(if (zerop n)
		(cdr list)
		(let ((cons (nthcdr (1- n) list)))
			(when cons (setf (cdr cons) (cddr cons))) list)))

;; ------------------------------
;;     ALGORITHM FUNCTIONS
;; ------------------------------

;; RETURN THE FINAL STATE
(defun get_final_state ()
	(list (list 'M 'M 'M 'C 'C 'C) 'L (list ) (list )))


;; RETURN THE INITIAL STATE
(defun get_initial_state ()
	(list (list ) 'R (list ) (list  'C 'C 'M 'M 'M 'C)))


;; RETURN IF IT'S THE CORRECT STATE
(defun is_final_state(state)
	(AND (= (length (remove 'M (nth 0 state))) 3) (= (length (remove 'C (nth 0 state))) 3) )
)

(defun operator_move_the_boat (state)
	(if (> (length (nth 2 state)) 0) 
		(let ((copy (copy-tree state)))
			; Se mueve la barca
			(if (eq (nth 1 copy) 'R)
				(setf (nth 1 copy) 'L)
				(setf (nth 1 copy) 'R)
			)
			(list copy)
		)
	)
)

(defun operator_get_off_the_boat (state)
	(let ((temp (list )))
		(loop for i from 0 to (- (length (nth 2 state)) 1) do
			(let ((copy (copy-tree state)) (element (nth i (nth 2 state))))
				; Se quita un sólo personaje de la barca
				(setf (nth 2 copy) (delete-nth i (nth 2 copy)))
				; Se pone a su lado correspondiente
				(if (eq (nth 1 copy) 'R)
					(setf (nth 3 copy) (append (nth 3 copy) (list element)))
					(setf (nth 0 copy) (append (nth 0 copy) (list element)))
				)
				; Se agrega el nuevo estado
				(setq temp (append temp (list copy)))
			)
		)
		temp
	)
)

(defun operator_get_on_the_boat (state)
	(let ((temp (list )))
		(if (< (length (nth 2 state)) 2)
			(let ((zone (if (eq (nth 1 state) 'R) 3 0)))
				(loop for i from 0 to (- (length (nth zone state)) 1) do
					(let ((copy (copy-tree state)) (element (nth i (nth zone state))))
						; Se quita de la zona
						(setf (nth zone copy) (delete-nth i (nth zone copy)))
						; Se agrega a la barca
						(setf (nth 2 copy) (append (nth 2 copy) (list element)))

						; Se agrega el nuevo estado
						(setq temp (append temp (list copy)))
					)
				)
			)
		)
		temp
	)
)

;; THE STATE IS NOT VALID 
(defun has_ended (state)
	(let* ((zone_with_boat (if (eq (nth 1 state) 'R) 3 0)) (zone_without_boat (abs (- zone_with_boat 3))))
		(let ((n_missionaries (+ (length (remove 'C (nth zone_with_boat state))) 
							(length (remove 'C (nth 2 state))))) 
			 (n_cannibals 	  (+ (length (remove 'M (nth zone_with_boat state))) 
							(length (remove 'M (nth 2 state)))))
			 (n_missionaries2 (length (remove 'C (nth zone_without_boat state))))
			 (n_cannibals2    (length (remove 'M (nth zone_without_boat state)))))
			(if (OR 	(AND (> n_cannibals n_missionaries) (> n_missionaries 0))
					(AND (> n_cannibals2 n_missionaries2) (> n_missionaries2 0)))
				T 
			)
		)
	)
)


;; GENERATE CHILD STATES 
(defun operator (state)
	(if (not (has_ended state))
		(let ((states_list (list )))



			;; Get off the boat
			(setq states_list (append states_list (operator_get_off_the_boat state)))

			;; Move the boat
			(setq states_list (append states_list (operator_move_the_boat state)))

			;; Get on the boat
			(setq states_list (append states_list (operator_get_on_the_boat state)))







			

			
			
			

						


			states_list
		)
	)
)


;; Breadth First Search
(defun BFS (initial)
	;; VARIABLES
	(setq opened (list initial))
	(setq closed (list))
	(setq result NIL)

	;; MAIN LOOP
	(loop 
		(when (or (eq result T) (= (length opened) 0)) (return result)) ;; RETURN CONDITION

		(setq current (car opened))
		(setq opened (cdr opened))
		
		(if (is_final_state current)
			(setq result T)
			(progn
				(setq closed (append closed (list current)))
				(setq new_states (operator current))
				(loop for new in new_states do
					(if (and 	(not (is_state_in_list new opened)) 
							(not (is_state_in_list new closed)))
						(setq opened (append opened (list new)  )))
				)
			)
		)
		(print current)
	)
)


;; Depth First Search
(defun DFS (initial)
	;; VARIABLES
	(setq opened (list initial))
	(setq closed (list))
	(setq result NIL)

	;; MAIN LOOP
	(loop 
		(when (or (eq result T) (= (length opened) 0)) (return result)) ;; RETURN CONDITION

		(setq current (car opened))
		(setq opened (cdr opened))
		
		(if (is_final_state current)
			(setq result T)
			(progn
				(setq closed (append closed (list current)))
				(setq new_states (operator current))
				(loop for new in new_states do
					(if (and 	(not (is_state_in_list new opened)) 
							(not (is_state_in_list new closed)))
						(setq opened (append (list new) opened )))
				)
			)
		)
		(print current)
	)
)

;; ------------------------------
;;     MAIN FUNCTION
;; ------------------------------

(defun main ()
	(setq initial_state (get_initial_state))
	(setq final_state (get_final_state))  
	;(print (DFS initial_state))
	(print (BFS initial_state))
	(print (length closed))
)

(main)
