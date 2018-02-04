;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gardias-lab4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Lab 3
;; Przemek Gardias
;; pmgardias@wpi.edu

;;
;; Problem 1
;;

(define-struct deduction (type amount))
;; a deduction is a (make-deduction String Natural)
;; interp. (make-deduction type amount) is a tax deduction
;; with a type of deduction and an amount for the deduction.

;; ListOfDeduction is one of
;;    empty
;;    (cons deduction ListOfDeduction)

;; Examples of deductions
(define d1 (make-deduction "charitable donation" 1000))
(define d2 (make-deduction "charitable gifts" 2500))
(define d3 (make-deduction "medical expenses" 6500))
(define d4 (make-deduction "home mortgage" 8000))
(define d5 (make-deduction "loss due to theft" 5000))

;; Examples of ListOfDeductions
(define lod1 (cons d1 (cons d2 (cons d3 empty))))
(define lod2 (cons d2 (cons d3 (cons d4 empty))))
(define lod3 (cons d3 (cons d4 (cons d5 empty))))
(define lod4 (cons d4 (cons d5 (cons d1 empty))))
(define lod5 (cons d5 (cons d1 (cons d2 empty))))

;;
;; Problem 2
;;

;; template
;; fcn-for-deduction: deduction -> 

(define (fcn-for-deduction deduction)
  (... (deduction-type deduction) (deduction-amount deduction)))

;; template
;; fcn-for-lod ListOfDeduction ->

(define (fcn-for-lod lod)
  (cond [(empty? lod) (...)]
        [(cons? lod) (... (deduction-type (first lod)) (deduction-amount (first lod)))]))

;;
;; Problem 3
;;

;; a BST is one of
;;   false
;;   Taxpayer

(define-struct taxpayer (id name gross-income deductions left right))
;; a Taxpayer is a (make-taxpayer Natural String Number ListOfDeduction BST BST)
;; interp: false means that the tree ends, there are no more taxpayers past that point
;;         id is the taxpayer identification number
;;         name is the name of the taxpayer
;;         gross-income is the gross income of the taxpayer
;;         deductions is a list of deductions of the taxpayer
;;         left and right are the left and right subtrees
;; INVARIANT: for a given coursenode:
;;         id is > all course-ids in its left child
;;         id is < all course-ids in its right child
;;         the same id never appears twice in the same tree

;;
;; Problem 4
;;

;; Examples of taxpayers, arranged in a BST.
(define t1 (make-taxpayer 100 "Sebastian Green" 85000 lod1 t2 t3)))
(define t2 (make-taxpayer 80 "Amy Hoslowth" 140000 lod2 t4 false)) 
(define t3 (make-taxpayer 120 "Christopher Robin" 65000 lod3 false t5))
(define t4 (make-taxpayer 40 "Ryan Brown" 100000 lod4 false false))
(define t5 (make-taxpayer 160 "Bethy Knickerbrocker" 60000 lod5 false false))

;;
;; Problem 5
;;

;; template
;; fcn-for-taxpayer: taxpayer ->

(define (fcn-for-taxpayer t)
  (cond [(false? t) (...)]
        [(taxpayer? t) (... (taxpayer-id t)
                            (taxpayer-name t)
                            (taxpyaer-gross-income t)
                            (fcn-for-lod (taxpayer-deductions t))
                            (fcn-for-taxpayer (taxpayer-left t))
                            (fcn-for-taxpayer (taxpayer-right t)))]))