;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gardias-lab3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Lab 3
;; Przemek Gardias
;; pmgardias@wpi.edu

;;
;; Problem 1
;;

(define-struct donor (phone-number carrier))
;; Donor is (make-donor Natural String)
;; interp. (make-donor phone-number carrier) is a donor with
;; a phone number and carrier.

(define d1 (make-donor 5081234567 "AT&T"))
(define d2 (make-donor 6501234567 "Verizon"))
(define d3 (make-donor 2131234567 "T-Mobile"))
(define d4 (make-donor 4601234567 "T-Mobile"))
(define d5 (make-donor 9151234567 "AT&T"))
;; Examples of donor.

(define-struct ListOfDonor (list))
;; ListOfDonor is (make-ListOfDonor list)
;; interp. (make-ListOfDonor list) is a list of donors made of
;; type donor.
                            
(define los1 (make-ListOfDonor (cons d1 (cons d2 (cons d3 empty)))))
(define los2 (make-ListOfDonor (cons d1 (cons d4 (cons d5 empty)))))
(define los3 (make-ListOfDonor (cons d2 (cons d3 (cons d5 empty)))))
;; Three examples of ListOfDonor with at least three donors.

;;
;; Problem 2
;;

(define-struct agency (name text-message number donation lod))
;; Agency is (make-agency String String Natural Natural list)
;; interp. (make-agency name text-message number donation ListOfDonor)
;; is an agency with a name, text message that donors will enter,
;; number that the donors will enter, amount that will be collected,
;; and a list of donors.

(define a1 (make-agency "Save the Children Federation" "SYRIA" 20222 1500 los1))
(define a2 (make-agency "Red Cross" "REDCROSS" 56644 5200 los2))
(define a3 (make-agency "UNICEF" "CHILD" 80448 1000 los3))
;; Three examples of agencies.

(define-struct ListOfAgency (list))
;; ListOfAgency is (make-LiftOfAgency list)
;; interp. (make-ListOfAgency list) is a list of agencies made
;; of type agency.

(define loa1 (make-ListOfAgency (cons a1 (cons a2 (cons a3 empty)))))
;; An example of ListOfAgency with at least three agencies.

;;
;; Problem 3
;;

;; Template for donor data structure
;#
(define (fcn-for-donor d)
        (... (donor-phone-number d)
             (donor-carrier d)))

;; Template for ListofDonor
;#
(define (fcn-for-lod lod)
        (cond [(empty? lod)  (... (first lod))]
              [(cons? lod)   (... (rest lod))]))

;; Template for agency data structure
;#
(define (fcn-for-agency a)
        (... (agency-name a)
             (agency-text-message a)
             (agency-number a)
             (agency-donation a)
             (agency-lod a)))

;; Template for ListofAgency
;#
(define (fcn-for-loa loa)
        (cond [(empty? loa)  (... (first loa))]
              [(cons? loa)   (... (rest loa))]))