;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname olgado-gardias-hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Homework 4
;; Nicholas Olgado
;; npolgado@wpi.edu
;;
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
(define D1 (make-deduction "charitable donation" 1000))
(define D2 (make-deduction "charitable gifts" 2500))
(define D3 (make-deduction "medical expenses" 6500))
(define D4 (make-deduction "home mortgage" 8000))
(define D5 (make-deduction "loss due to theft" 5000))
(define D6 (make-deduction "you just straight up suck" 1000000))

;; Examples of ListOfDeductions
(define LOD1 (cons D1 (cons D2 (cons D3 empty)))) ;;total: 10000
(define LOD2 (cons D2 (cons D3 (cons D4 empty)))) ;;total: 17000
(define LOD3 (cons D3 (cons D4 (cons D5 empty)))) ;;total: 10000
(define LOD4 (cons D4 (cons D5 (cons D1 empty)))) ;;total: 19500
(define LOD5 (cons D5 (cons D1 (cons D2 empty)))) ;;total: 8500
(define LOD6 (cons D4 (cons D5 (cons D6 empty)))) ;;total: 1013000

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

(define T5 (make-taxpayer 160 "Bethy Knickerbrocker" 60000 LOD5 false false))
(define T4 (make-taxpayer 40 "Ryan Brown" 100000 LOD4 false false))
(define T3 (make-taxpayer 120 "Christopher Robin" 65000 LOD3 false T5))
(define T2 (make-taxpayer 80 "Amy Hoslowth" 140000 LOD2 T4 false)) 
(define T1 (make-taxpayer 100 "Sebastian Green" 85000 LOD1 T2 T3))

;;
;; Problem 5
;;

;; template
;; fcn-for-taxpayer: taxpayer ->

(define (fcn-for-taxpayer t)
  (cond [(boolean? t) (...)]
        [(taxpayer? t) (... (taxpayer-id t)
                            (taxpayer-name t)
                            (taxpayer-gross-income t)
                            (fcn-for-lod (taxpayer-deductions t))
                            (fcn-for-taxpayer (taxpayer-left t))
                            (fcn-for-taxpayer (taxpayer-right t)))]))


;; 
;; Problem 6
;;

;; any-owe-no-taxes?: BST --> Boolean
;; consumes a binary search tree, and produces true if any of the taxpayers in the tree have gross incomes of less than $10,000
;; (and thus don't owe any income tax).

(define GROSS-INCOME-LIMIT 10000) ;;constant for the cutoff we are looking for for gross income

(define (any-owe-no-taxes? t) 
  (cond [(boolean? t) false]
        [(taxpayer? t) (if (< (taxpayer-gross-income t) GROSS-INCOME-LIMIT) true (if (and (boolean=? (any-owe-no-taxes? (taxpayer-left t)) false)
                                                                             (boolean=? (any-owe-no-taxes? (taxpayer-right t)) false))
                                                                        false true))]))
 
;;check-expects
(check-expect (any-owe-no-taxes? false) false)
(check-expect (any-owe-no-taxes? T1) false)
(check-expect (any-owe-no-taxes? (make-taxpayer 160 "Bethy Knickerbrocker" 9000 LOD2 false false)) true)
(check-expect (any-owe-no-taxes? (make-taxpayer 160 "Bethy Powers" 12000 LOD2
                                                (make-taxpayer 150 "Bethy Bowers" 9000 LOD4 false
                                                               (make-taxpayer 155 "Jeb Bushmen" 100000 LOD2 false false)) false)) true)
(check-expect (any-owe-no-taxes? (make-taxpayer 160 "Bethy Powers" 12000 LOD2
                                                (make-taxpayer 150 "Bethy Bowers" 102000 LOD4
                                                               (make-taxpayer 160 "Yung Savage" 203123 LOD2 false false)
                                                               (make-taxpayer 155 "Jeb Bushmen" 100000 LOD2 false false))
                                                (make-taxpayer 160 "Tuck Frump" 100000 LOD2 false false))) false)


;;
;; Problem 7
;;

;; total-deductions-for: BST Natural --> Natural
;; consumes a binary search tree and a taxpayer ID, and produces the total amount of deductions for the taxpayer with the given ID.
(define (total-deductions-for t id)
  (cond [(boolean? t) 0]
        [(taxpayer? t) (if (= (taxpayer-id t) id)
                           (find-deduct (taxpayer-deductions t))
                           (if (left-or-right (taxpayer-id t) id)
                                (total-deductions-for (taxpayer-left t) id)
                                (total-deductions-for (taxpayer-right t) id)))]))

;; helper fxn left-or-right: Natural Natural --> Boolean
;; consumes two naturals, the current id and the id being searched for, and returns true if we should look at the left child, and returns false if we should look at the right child.
(define (left-or-right id-current id)
  (if (< id-current id) false true))

;; helper fxn find-deduct: ListOfDeductions --> Natural
;; consumes a ListOfDeductions, and produces the total amount of deductions in that list.
(define (find-deduct lod)
  (cond [(empty? lod) 0]
        [(cons? lod) (+ (deduction-amount (first lod)) (find-deduct (rest lod)))]))

;; check expects
(check-expect (total-deductions-for false 1) 0)
(check-expect (total-deductions-for T1 100) (+ 1000 2500 6500)) ;;lod 1 (d1, d2, and d3)
(check-expect (total-deductions-for T1 160) (+ 1000 2500 5000)) ;;lod 5 (d1, d2, and d5)
(check-expect (total-deductions-for T1 80) (+ 2500 6500 8000)) ;;lod 2 (d2, d3, and d4)


;;
;; Problem 8
;;

;; total-tax-for : BST Natural --> Natural
;; consumes a binary search tree, and a taxpayer ID, and produces the tax owed by that taxpayer.
(define (total-tax-for t id)
  (cond [(boolean? t) 0]
        [(taxpayer? t) (if (= (taxpayer-id t) id)
                           (find-tax-owed (taxpayer-deductions t) t)
                           (if (which-way (taxpayer-id t) id)
                                (total-tax-for (taxpayer-left t) id)
                                (total-tax-for (taxpayer-right t) id)))]))

;; helper fxn find-tax-owed : ListOfDeductions Taxpayer --> Natural
;; consumes a ListOfDeductions and Taxpayer, and produces a tax-owed based on the instructions given for finding such

;;constants:
(define MIN-GROSS-CUTOFF 10000) ;;below this number, 0 tax
(define MID-GROSS-CUTOFF 100000) ;; between the min and this, 20% tax, above this is 30%
(define MID-TAX 0.2) ;; tax for middle range
(define MAX-TAX 0.3) ;; tax for max range

(define (find-tax-owed lod t)
  (cond [(< (- (taxpayer-gross-income t) (total-deduct lod)) MIN-GROSS-CUTOFF) 0]
        [(and (>= (- (taxpayer-gross-income t) (total-deduct lod)) MIN-GROSS-CUTOFF)
              (<= (- (taxpayer-gross-income t) (total-deduct lod)) MID-GROSS-CUTOFF)) (* MID-TAX (- (taxpayer-gross-income t) (total-deduct lod)))]
        [(> (- (taxpayer-gross-income t) (total-deduct lod)) MID-GROSS-CUTOFF) (* MAX-TAX (- (taxpayer-gross-income t) (total-deduct lod)))]))

;; helper fxn which-way: Natural Natural --> Boolean
;; consumes two naturals, the current id and the id being searched for, and returns true if we should look at the left child, and returns false if we should look at the right child.
(define (which-way id-current id)
  (if (< id-current id) false true))

;; helper fxn total-deduct: ListOfDeductions --> Natural
;; consumes a ListOfDeductions, and produces the total amount of deductions in that list.
(define (total-deduct lod)
  (cond [(empty? lod) 0]
        [(cons? lod) (+ (deduction-amount (first lod)) (total-deduct (rest lod)))]))


(check-expect (total-tax-for T1 80) 36900) 
(check-expect (total-tax-for (make-taxpayer 100 "Bethy Knickerbrocker" 100 LOD4
                                            (make-taxpayer 90 "Boi Pablo" 10000 LOD2 false false)
                                            (make-taxpayer 110 "Brian Rown" 10 LOD6 false false)) 90) 0)
(check-expect (total-tax-for (make-taxpayer 100 "Bethy Knickerbrocker" 34500 LOD4
                                            (make-taxpayer 90 "Boi Pablo" 10000 LOD2 false false)                
                                            (make-taxpayer 110 "Brian Rown" 10 LOD6 false false)) 100) 4100)
(check-expect (total-tax-for (make-taxpayer 100 "Bethy Knickerbrocker" 100 LOD4
                                            (make-taxpayer 90 "Boi Pablo" 10000 LOD2 false false)
                                            (make-taxpayer 110 "Brian Rown" 11013000 LOD6 false false)) 110) 3000000)

;;
;; Problem 9
;;
;; Write a function list-names-in-order-by-id. The function consumes a binary search tree and produces a list of the names of the taxpayers, sorted in order by ascending taxpayer id.
;; (Hint: you don't have to write a sorting algorithm. Use what you know about the order of items in a binary search tree to help you. You will need to use the built-in function append
;; for this problem.)

;; list-names-in-order-by-id: BST --> ListofStrings
;; consumes a binary search tree and produces a list of the names of the taxpayers, sorted in order by ascending taxpayer id.
(define (list-names-in-order-by-id t)
  (cond [(boolean? t) empty]
        [(taxpayer? t) (append (list-names-in-order-by-id (taxpayer-left t))
                               (cons (taxpayer-name t) (list-names-in-order-by-id (taxpayer-right t))))]))

;;check expect test cases
(check-expect (list-names-in-order-by-id false) empty)
(check-expect (list-names-in-order-by-id T1) (list "Ryan Brown" "Amy Hoslowth" "Sebastian Green" "Christopher Robin" "Bethy Knickerbrocker"))
(check-expect (list-names-in-order-by-id (make-taxpayer 100 "Bethy Knickerbrocker" 100 LOD4
                                            (make-taxpayer 90 "Boi Pablo" 10000 LOD2 false false)
                                            (make-taxpayer 110 "Brian Rown" 10 LOD6 false false))) (list "Boi Pablo" "Bethy Knickerbrocker" "Brian Rown"))

;;
;; Problem 10
;;

;; add-taxpayer: BST Natural String Natural --> BST
;; consumes a binary search tree, a taxpayer id, a taxpayer's name, and the taxpayer's gross income, and creates a binary search tree the same
;; as the original except that a new taxpayer with the given information has been added to the tree.
(define (add-taxpayer t id name gross-income)
  (cond [(boolean? t) (make-taxpayer id name gross-income empty false false)]
        [(taxpayer? t) (cond [(and (boolean? (taxpayer-left t)) (> (taxpayer-id t) id))
                              (make-taxpayer (taxpayer-id t)
                                             (taxpayer-name t)
                                             (taxpayer-gross-income t)
                                             (taxpayer-deductions t)
                                             (make-taxpayer id name gross-income empty false false)
                                             (taxpayer-right t))]
                             [(and (boolean? (taxpayer-right t)) (< (taxpayer-id t) id))
                              (make-taxpayer (taxpayer-id t)
                                             (taxpayer-name t)
                                             (taxpayer-gross-income t)
                                             (taxpayer-deductions t) 
                                             (taxpayer-left t)
                                             (make-taxpayer id name gross-income empty false false))]
                             [(and (taxpayer? (taxpayer-right t)) (> (taxpayer-id t) id))
                              (make-taxpayer (taxpayer-id t)
                                             (taxpayer-name t)
                                             (taxpayer-gross-income t)
                                             (taxpayer-deductions t)
                                             (add-taxpayer (taxpayer-left t) id name gross-income)
                                             (taxpayer-right t))]
                             [(and (taxpayer? (taxpayer-right t)) (< (taxpayer-id t) id))
                              (make-taxpayer (taxpayer-id t)
                                             (taxpayer-name t)
                                             (taxpayer-gross-income t)
                                             (taxpayer-deductions t)
                                             (taxpayer-left t)
                                             (add-taxpayer (taxpayer-left t) id name gross-income))])]))

;;Check expect test cases

(check-expect (add-taxpayer T1 90 "boi" 150) (make-taxpayer 100 "Sebastian Green" 85000 LOD1
                                                                       (make-taxpayer 80 "Amy Hoslowth" 140000 LOD2
                                                                                      (make-taxpayer 40 "Ryan Brown" 100000 LOD4 false false)
                                                                                      (make-taxpayer 90 "boi" 150 empty false false))
                                                                       (make-taxpayer 120 "Christopher Robin" 65000 LOD3 false
                                                                                      (make-taxpayer 160 "Bethy Knickerbrocker" 60000 LOD5 false false))))
(check-expect (add-taxpayer false 90 "boy" 150) (make-taxpayer 90 "boy" 150 empty false false))
(check-expect (add-taxpayer (make-taxpayer 100 "yung" 150 LOD1
                                           (make-taxpayer 90 "babaom" 1000 LOD4 false false) false) 110 "boy" 150)   (make-taxpayer 100 "yung" 150 LOD1
                                                                                                                                   (make-taxpayer 90 "babaom" 1000 LOD4 false false)
                                                                                                                                   (make-taxpayer 110 "boy" 150 empty false false)))