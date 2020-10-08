;; ------------------------------
;;     USEFUL FUNCTIONS
;; ------------------------------

(defun is_state_in_list (state list)
	(loop for l in list do
		(when (equal l state)
			(return T))
	)
)

;; ------------------------------
;;     ALGORITHM FUNCTIONS
;; ------------------------------

;; RETURN THE FINAL STATE
(defun get_final_state ()
	(list (list 'M 'M 'M 'C 'C 'C) 'L (list ) (list )))


;; RETURN THE INITIAL STATE
(defun get_initial_state ()
	(list (list ) 'R (list ) (list 'M 'M 'M 'C 'C 'C)))


;; RETURN IF IT'S THE CORRECT STATE
(defun is_final_state(state)
	(equal state (get_final_state))
)


;; GENERATE STATES 
(defun operator (list)
	(let ((states_list (list )))
		states_list
	)
)


;; Breadth First Search
(defun BFS (initial final)
	;; VARIABLES
	(setq opened (list initial))
	(setq closed (list))
	(setq result NIL)

	;; MAIN LOOP
	(loop 
		(when (or (eq result T) (= (length opened) 0)) (return result)) ;; RETURN CONDITION

		(print '(----------))
		(print opened)
		(print closed)

		(setq current (car opened))
		(setq opened (cdr opened))

		(print current)
		
		(if (equal current final)
			(setq result T)
			(progn
				(setq closed (append closed (list current)))
				(setq new_states (operator current))
				(loop for new in new_states do
					(if (and 	(not (is_state_in_list new opened)) 
							(not (is_state_in_list new closed)))
						(setq opened (append opened (list new))))
				)
			)
		)
	)
)

;; ------------------------------
;;     MAIN FUNCTION
;; ------------------------------

(defun main ()
	(setq initial_state (get_initial_state))
	(setq final_state (get_final_state))  
	;; (print (BFS initial_state final_state))
	(print initial_state)
	(print final_state)
)

(main)
