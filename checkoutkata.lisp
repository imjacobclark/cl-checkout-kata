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

(defun discounted-price(type)
  (first (last (gethash type *offers*)))
)

(defun discounted-offer(type)
  (first (gethash type *offers*))
)

(defun discounter(type basket discount-threshold count-in-basket)
  (if 
    (and 
      (eq (member type basket) NIL) 
      (< count-in-basket discount-threshold))
    0
    (if 
      (eq 
        discount-threshold 
        count-in-basket)
        (discounted-price type)
      (discounter 
        type 
        (remove type basket :count 1)
        discount-threshold 
        (+ count-in-basket 1)
      )))
)

(defun apply-discount(type basket)
  (if (eq (discounted-offer type) NIL)
    0
    (discounter 
      type 
      basket 
      (discounted-offer type)
      0
    )
  )
)

(defun sum(items acumulator)
  (let ((item (first items)))
    (if (eq items NIL)
      acumulator 
      (if 
        (eq 
          (apply-discount item items)
          0)
        (sum
          (remove item items :count 1)
          (+ (gethash item *items*) acumulator)
        )
        (sum
          (remove item items :count (first (gethash item *offers*)))
          (+ (apply-discount item items) acumulator)
        )
      )
    )
  ) 
)

(defun checkout(items)
  (sum items 0)
)

(defun runtests()
  (is (checkout '(A A)) 100 "checkout returns 100 for AA")
  (is (checkout '(A A A)) 130 "checkout returns 130 for AAA")
  (is (checkout '(A B C D)) 115 "checkout returns 115 for ABCD")
  (is (checkout '(D A B A B A)) 190 "checkout returns 190 for DABABA")
  (is (checkout '(A A A A A A)) 260 "checkout returns 260 for AAAAAA")

  (is (apply-discount 'A '(A B A B A)) 130 "applies discount on valid basket and returns discount price")
  (is (apply-discount 'A '(A B)) 0 "does not apply discount when discount is not relevant")
  (is (apply-discount 'D '(D)) 0 "does not apply discount when discount is not relevant")
)

(runtests)
