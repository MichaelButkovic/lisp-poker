;;;code for CLOS cards

(defclass card ()
  ((suit :initarg :suit :accessor card-suit)
   (value :initarg :value :accessor card-value))
  (:documentation "a card class"))

;;;Ace = 14, King = 13, Queen = 12, Jack = 11 for internalization purposes
(defclass deck ()
  ((cards :initarg :cards :accessor cards)
   (suits :initarg :suits :initform '(spades diamonds hearts clubs) :accessor deck-suits)
   (values :initarg :values :initform '(14 13 12 11 10 9 8 7 6 5 4 3 2) :accessor deck-values)) 
  (:documentation "a deck class"))

(defmethod initialize-instance :after ((this-deck deck) &rest args)
  (setf (cards this-deck)
    (mapcan #'(lambda (suit)
                (mapcar #'(lambda (value)
                            (make-instance 'card :suit suit :value value))
                  (deck-values this-deck)))
      (deck-suits this-deck)))
  (shuffle this-deck))

;;;deals a certain number of cards
(defmethod deal ((this-deck deck) &optional (cards 1))
  (let ((hand nil))
        (dotimes (i cards hand)
          (push (pop (cards this-deck))
                hand))))
;;;shuffles cards
(defmethod shuffle ((this-deck deck))
  (loop with cards = (cards this-deck)
      for i from 0 below (length cards)
      as this-card = (elt cards i)
      as other-pos = (random (length cards))
      as other-card = (elt cards other-pos)
      unless (= i other-pos)
      do (setf (elt cards i) other-card)
         (setf (elt cards other-pos) this-card)
        finally (setf (cards this-deck) cards)))

;;;makes a particular card
(defun make-card (suit value)
  (make-instance 'card :suit suit :value value))

;;;prints a hand w/r/t naming convention
(defun print-hand (hand)
  (loop for card in hand
        do(let ((cvalue (card-value card)))
            (cond
             ((equal cvalue 14) (format t "~%Ace of ~A" (card-suit card)))
             ((equal cvalue 13) (format t "~%King of ~A" (card-suit card)))
             ((equal cvalue 12) (format t "~%Queen of ~A" (card-suit card)))
             ((equal cvalue 11) (format t "~%Jack of ~A" (card-suit card)))
             (t (format t "~%~A of ~A" (card-value card)(card-suit card)))))))

(defun print-card (card)
  (format t "~%~A of ~A" (card-value card)(card-suit card)))

;;;puts a card in a deck
(defun put-card-in-deck (card deck)
  (push card (cards deck)))

(defun count-cards-with-value (value hand)
  (loop for card in hand
        count (equal value (card-value card))))

(defun calc-prob-for-next-card-value (deck value hand)
  (float (/ (- 4 (count-cards-with-value value hand)) 
            (length (cards deck)))))

(defun calc-prob-for-next-card-suit (deck suit hand)
  (float (/ (- 13 (count-cards-with-value suit hand)) 
            (length (cards deck)))))

;;;deals a certain number of cards
(defun deal-cards (deck &optional (numcards 1))
  (unless (zerop numcards)
    (cons (pop (cards deck))
          (deal-cards deck (1- numcards)))))
