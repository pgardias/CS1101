;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gardias-lab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Lab 2
;; Przemek Gardias
;; pmgardias@wpi.edu

;;
;; Problem 1
;;

(define-struct gift-card (message name))
;; Gift-card is (make-gift-card String String)
;; interp. (make-gift-card message name) is a gift card with
;; a message and name.

(define gc1 (make-gift-card "Hello!" "Nick"))
(define gc2 (make-gift-card "Thank you" "Przemek"))
;; Two examples of gift cards.

;;
;; Problem 2
;;

(define-struct teddy (outfit color size))
;; Teddy is (make-teddy String String Natural)
;; interp. (make-teddy outfit color size) is a teddy with
;; a outfit, a color, and a size.

(define t1 (make-teddy "biker" "brown" 22))
(define t2 (make-teddy "teacher" "red" 17))
;; Two examples of teddies.

;;
;; Problem 3
;;

(define-struct jewelry (type metal box))
;; Jewelry is (make-jewelry String Boolean Boolean)
;; interp. (make-jewelry type metal box) is a jewelry piece with
;; a type of jewelry given by a string, and the metal and box type given by
;; a boolean where true = gold, false = silver for metal and true =
;; heart-shaped box, false = standard box.

(define j1 (make-jewelry "ring" true false))
(define j2 (make-jewelry "brooch" false true))
;; Two examples of jewelry pieces.

;;
;; Problem 4
;;

(define-struct roses (color quantity))
;; Roses is (make-roses String Natural)
;; interp. (make-roses color quantity) is a bouquet of roses
;; with a color and quantity (in dozens of flowers).

(define r1 (make-roses "red" 3))
(define r2 (make-roses "white" 2))
;; Two examples of roses.

;;
;; Problem 5
;;

;; Gift card template.
(define (gift-card-function gc)
  (... (gift-card-message gc) (gift-card-name gc)))

;; Teddy template.
(define (teddy-function t)
  (... (teddy-outfit t) (teddy-color t) (teddy-size t)))

;; Jewelry template.
(define (jewelery-function j)
  (... (jewelry-type j) (jewelery-metal j) (jewelry-box j)))

;; Roses template.
(define (roses-function r)
  (... (roses-color r) (roses-quantity r)))

;;
;; Problem 6
;;

(define-struct gift (gift-card teddy jewelry roses))
;; Gift is (make-gift gift-card teddy jewelry roses)
;; interp. (make-gift gift-card teddy jewelry roses) is a gift with a gift card,
;; teddy, jewelry, and roses.

;; Gift template.
(define (gift-function g)
  (... (gift-gift-card g) (gift-teddy g) (gift-jewelry g) (gift-roses g)))

;;
;; Problem 7
;;

(define-struct date (month day year))
;; Date is (make-date Natural Natural Natural)
;; interp. (make-date month day year) is a date with a month, day, and year.

(define-struct order (card-number delivery-date gift))
;; Order is (make-order Natural date gift)
;; interp. (make-order card-number delivery-date gift) is an order with a
;; credit card number, delivery date, and a gift.

(define d1 (make-date 4 12 2017))
(define d2 (make-date 7 1 2017))
;; Two examples of a date.

(define o1 (make-order 4567890987654321 d1 (make-gift gc1 t1 j1 r1)))
(define o2 (make-order 1234567890987654 d2 (make-gift gc2 t2 j2 r2)))
;; Two examples of an order.

;;
;; Problem 8
;;

;; Date template.
(define (date-function d)
  (... (date-month d) (date-day d) (date-year d)))

;; Order template.
(define (order-function o)
  (... (order-card-number o) (order-delivery-date o) (order-gift o)))
