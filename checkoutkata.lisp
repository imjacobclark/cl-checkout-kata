;;;; checkoutkata.lisp

(defpackage checkoutkata
  (:use :cl
        :prove))
(in-package :checkoutkata)

(defparameter *items* (make-hash-table))
(setf (gethash 'A *items*) 50)
(setf (gethash 'B *items*) 30)
(setf (gethash 'C *items*) 20)
(setf (gethash 'D *items*) 15)

(defparameter *offers* (make-hash-table))
(setf (gethash 'A *offers*) '(3 130))
(setf (gethash 'B *offers*) '(2 45))

(defun discounter(type basket threshold count)
  (if 
    (and 
      (eq 
        (member type basket) NIL) 
        (< count threshold))
    0
    (if 
      (eq threshold count)
        (first (last (gethash type *offers*)))
        (discounter 
          type 
          (remove type basket :count 1)
          threshold 
          (+ count 1)
        )))
)

(defun apply-discount(type basket)
  (let ( (offer (gethash type *offers*)) )
    (if (eq offer NIL)
      0
      (discounter type basket (first offer) 0)
    )
  )
)

(defun calculate-cost(items acumulator)
  (if (eq items NIL)
    acumulator 
      (if 
        (eq 
          (apply-discount 
            (first items) 
            items)
          0
        )
        (calculate-cost
          (remove (first items) items :count 1)
          (+ (gethash (first items) *items*) acumulator)
        )
        (calculate-cost
          (remove (first items) items :count (first (gethash (first items) *offers*)))
          (+ (apply-discount (first items) items) acumulator)
        )
      )
    )
)

(defun total(items)
  (calculate-cost items 0)
)

(defun runtests()
  (is (total '(A A)) 100 "total returns 100 for AA")
  (is (total '(A A A)) 130 "total returns 130 for AAA")
  (is (total '(A B C D)) 115 "total returns 115 for ABCD")
  (is (total '(D A B A B A)) 190 "total returns 190 for DABABA")
  (is (total '(A A A A A A)) 260 "total returns 260 for AAAAAA")

  (is (apply-discount 'A '(A B A B A)) 130 "applies discount on valid basket and returns discount price")
  (is (apply-discount 'A '(A B)) 0 "does not apply discount when discount is not relevant")
  (is (apply-discount 'D '(D)) 0 "does not apply discount when discount is not relevant")
)

(runtests)
