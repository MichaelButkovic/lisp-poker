;;;all associated game code (could probably break this out into checking, user and game files)


;;;global variables
(defparameter *pot* 0)
(defparameter *previous-raise* 0)
(defparameter *blinds* 200)
(defparameter *user-chips* 1000)
(defparameter *user-hand* '())
(defparameter *ai-hand* '())
(defparameter *ai-chips* 1000)
(defparameter *board* '())
(defparameter *deck* (make-instance 'deck))

;;;general function for user input
(defun user-choice-input ()
  (format t "~%Please enter your choice:~%")
  (format t "(1 to raise, 2 to check/call or 3 to fold)~%")
  (let ((user-input (read)))
    (cond
     ((equal user-input '1) '1)
     ((equal user-input '2) '2)
     ((equal user-input '3) '3)
     (t (format t "Not a valid command. Try again.~%")
        (user-choice-input)))))

;;;specific function for when the user chooses to raise
(defun user-raise ()
  (format t "~%You have ~:D chips" *user-chips*)
  (format t "~%By how much would you like to raise?~%")
  (let ((raise (read)))
    (cond
     ;;;if user tries to raise more chips then they have
     ((> raise *user-chips*)(format t "Whoa, that's more chips then you have!")(user-raise))
     ;;;else
     ((<= raise *user-chips*)
      (decf *user-chips* raise)
      (incf *pot* raise)
      (setf *previous-raise* raise))
     (t (user-raise)))))


;;;specific function for when the user chooses to fold
(defun user-fold ()
  (format t "You fold. You now have ~:D chips and your opponent has ~:D" *user-chips* *ai-chips*)
  (incf *ai-chips* *pot*)
  (hand-cleanup))


;;;function for when the user decides to call
(defun user-call ()
  (incf *pot* *previous-raise*)
  (decf *user-chips* *previous-raise*)
  (setf *previous-raise* 0))


;;;note: calling is not properly implemented. if there is enough time, fix this
;;;First Round of the hand
(defun action ()
  (format t "~%ACTION~%")
  (setf *user-hand* (deal *deck* 2))
  (print-game-status)
  (user-action)
  (ai-action))
  
(defun user-action ()
  (let ((action (user-choice-input)))
    (cond
     ((equal action '1)(user-raise))
     ((equal action '2)(decf *user-chips* *blinds*)(incf *pot* *blinds*))
     ((equal action '3)(user-fold))
     (t (user-action)))
    ))


;;;Second Round of the Hand
(defun flop ()
  (format t "~%FLOP~%")
  (setf *board* (deal *deck* 3))
  (print-game-status)
  (user-flop)
  (ai-flop))

(defun user-flop ()
  (let ((action (user-choice-input)))
     (cond
      ((equal action '1)(user-raise))
      ((equal action '2)(user-call))
      ((equal action '3)(user-fold))
      (t (user-flop)))
    ))


;;;Third Round of the Hand
(defun turn ()
  (format t "~%TURN~%")
  (setf *board* (append *board* (deal *deck* 1)))
  (print-game-status)
  (user-turn)
  (ai-turn))

(defun user-turn ()
  (let ((action (user-choice-input)))
     (cond
      ((equal action '1)(user-raise))
      ((equal action '2)(user-call))
      ((equal action '3)(user-fold))
      (t (user-turn)))
    ))


;;;Fourth Round of the Hand
(defun river ()
  (format t "~%RIVER~%")
  (setf *board* (append *board* (deal *deck* 1)))
  (print-game-status)
  (user-river)
  (ai-river))
  
(defun user-river ()
  (let ((action (user-choice-input)))
     (cond
     ((equal action '1)(user-raise))
     ((equal action '2)(user-call))
     ((equal action '3)(user-fold))
      (t (user-river)))
    ))


;;;Final Round of the Hand
(defun final-bets ()
  (format t "~%FINAL BETS~%")
  (print-game-status)
  (user-final-bets)
  (ai-final-bets))

(defun user-final-bets ()
  (let ((action (user-choice-input)))
	(cond
	((equal action '1)(user-raise))
	((equal action '2)(user-call))
	((equal action '3)(user-fold))
  (t (user-final-bets)))
    ))

;;;break these out and make the user type in part of the hand (unfortunate)
;;;(defun play-hand ()
;;;  (action)
;;;  (flop)
;;;  (turn)
;;;  (river)
;;;  (final-bets)
;;;  ;;;evaluate hands and see who wins
;;;  (cond
;;;  ;;;condition for if someone folded (bad fix, but i am unsure of how to fix it)
;;;   ((or (null *user-hand*)(null *ai-hand*)) 
;;;    (format t "~%You lost this hand!"))
;;;  ;;;if the users hand is better
;;;   ((> (eval-hand *user-hand*)(eval-hand *ai-hand*))
;;;    (format t "~%You won this hand!")
;;;    (incf *user-chips* *pot))
;;;  ;;;if the ais hand is better
;;;   ((< (eval-hand *user-hand*)(eval-hand *ai-hand*))
;;;    (format t "~%You lost this hand!")
;;;    (incf *ai-chips* *pot*))
;;;  ;;;if they have the same hand, user wins
;;;   (t (format t "~%Draw.")(incf *user-chips* *pot*)))
;;;  (hand-cleanup))

(defun evaluate-hand ()
  ;;;evaluate hands and see who wins
  (cond
  ;;;condition for if someone folded (kludgy fix, but i am unsure of how to fix it properly)
   ((or (null *user-hand*)(null *ai-hand*)) 
    (format t "~%You lost this hand!"))
  ;;;if the users hand is better
   ((> (eval-hand *user-hand*)(eval-hand *ai-hand*))
    (format t "~%You won this hand!")
    (incf *user-chips* *pot*))
  ;;;if the ais hand is better
   ((< (eval-hand *user-hand*)(eval-hand *ai-hand*))
    (format t "~%You lost this hand!")
    (incf *ai-chips* *pot*))
  ;;;if they have the same hand, user wins
   (t (format t "~%Draw.")(incf *user-chips* *pot*)))
  (hand-cleanup))


;;;returns a number based on the hand
(defun eval-hand (hand)
  (cond
   ((check-straight-flush hand) '8)
   ((check-4-of-a-kind hand) '7)
   ((check-full-house hand) '6)
   ((check-flush hand) '5)
   ((check-straight hand) '4)
   ((check-3-of-a-kind hand) '3)
   ((check-two-pair hand) '2)
   ((check-one-pair hand) '1)
   ;;;high card
   (t '0)))

;;;various functions to check hands
(defun check-straight-flush (hand)
  (and (check-straight hand)(check-flush hand)))

(defun check-4-of-a-kind (hand)
  (setf collected-list (collect-values (append hand *board*)))
  (setf collected-list (sort collected-list #'<))
  (= (length (destructive-test-loop collected-list)) 4))

(defun check-full-house (hand)
  (and (check-3-of-a-kind hand)(check-two-pair hand)))

(defun check-flush (hand)
  (setf collected-list (collect-suits (append hand *board*)))
  (setf high-number (check-flush-loop collected-list))
  (= high-number 5))

;;;returns the largest amount of a single suit
(defun check-flush-loop (list)
  (loop with spds-cntr = 0 and hrts-cntr = 0 and dmds-cntr = 0 and clbs-cntr = 0
      for suit in list
      if (equal suit 'spades) do(incf spds-cntr)
      if (equal suit 'hearts) do(incf hrts-cntr)
      if (equal suit 'diamonds) do(incf dmds-cntr)
      if (equal suit 'clubs) do(incf clbs-cntr)
        finally(return (max spds-cntr hrts-cntr dmds-cntr clbs-cntr))))

(defun check-straight (hand)
  (setf collected-list (collect-values (append hand *board*)))
  (setf collected-list (sort collected-list #'>))
  (setf returned-list (check-straight-loop collected-list))
  (or (equal 5 (straight-test returned-list)) (equal 5 (straight-test (sort returned-list #'>)))))
  
;;;returns a list of the numbers all incremented to the same amount  
(defun check-straight-loop (list)
  (loop with counter = 0 and return-list = ()
      for number in list
        do(push (incf number counter) return-list)
        do(incf counter)
        finally(return return-list)))

;;;checks if 5 numbers in a sorted list are equal
(defun straight-test (list)
  (setf num (car list))
  (loop with counter = 0
      for number in list
        if(= num number) do(incf counter)
        finally(return counter)))

;;;if the length of the returned list of numbers is 3 and the first are equal
(defun check-3-of-a-kind (hand)
  (setf collected-list (collect-values (append hand *board*)))
  (setf collected-list (sort collected-list #'<))
  (setf returned-list (destructive-test-loop collected-list))
  (and (equal 3 (length returned-list))(equal (car returned-list)(cadr returned-list))))

;;;if the length of the returned list is 3, but the first two aren't equal
(defun check-two-pair (hand)
  (setf collected-list (collect-values (append hand *board*)))
  (setf collected-list (sort collected-list #'<))
  (setf returned-list (destructive-test-loop collected-list))
  (and (equal 3 (length returned-list)) (not (equal (car returned-list)(cadr returned-list)))))

(defun check-one-pair (hand)
  (setf collected-list (collect-values (append hand *board*)))
  (setf collected-list (sort collected-list #'<))
  (test-two-loop collected-list))


;;;tests if two numbers are equal in a sorted list
(defun test-two-loop (list)
  (loop with index = 1
      for number in list
      if (equal number (elt list index)) do (return t)
      else do(if (= index (1- (length list)))
                 (return nil)
               (incf index))))

;;;returns a list of matching pairs in a list
(defun destructive-test-loop (list)
  (loop with index = 1 and return-list = ()
      for number in list
      if (equal number (elt list index)) do (push number return-list)
      else do(if (= index (1- (length list)))
                 nil
               (incf index))
        finally(return return-list)))

;;;collects the values of a given list of cards (simplifies the checking code)
(defun collect-values (list)
  (loop for card in list
        collect(card-value card)))

;;;collects the suits of a given list of cards (simplifies the checking code)
(defun collect-suits (list)
  (loop for card in list
        collect(card-suit card)))

;;;general cleanup function for the end of each hand
(defun hand-cleanup ()
  (setf *pot* 0)
  (setf *user-hand* nil)
  (setf *ai-hand* nil)
  (setf *board* nil)
  (setf *previous-raise* 0))

;;;prints the users hand
(defun print-user-hand ()
  (format t "~%Your hand is:")
  (print-hand *user-hand*))

;;;prints the current status of the game
(defun print-game-status ()
  (format t "~%The cards on the table are: ")
  (print-hand *board*)
  (print-user-hand)
  (format t "~%You have ~:D chips." *user-chips*)
  (format t "~%The pot is ~:D chips.~%" *pot*))


;;;sets the game parameters for the beginning of the game
(defun set-game-params (blinds user-chips ai-chips)
  (setf *blinds* blinds)
  (setf *user-chips* user-chips)
  (setf *ai-chips* ai-chips))