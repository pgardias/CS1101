;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname olgado-gardias-hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Homework 3
;; Nicholas Olgado
;; npolgado@wpi.edu
;;
;; Przemek Gardias
;; pmgardias@wpi.edu

;;
;; Problem 1
;;

(define-struct donor (cell carrier))
;; donor is a (make-donor Natural String)
;; interp: a donor is a cell phone number natural,
;; and a cell phone providor: e.g "Verizon"

;; ListofDonor can be:
;; empty
;; (cons donor ListofDonor)

;; example of Donors
(define D1 (make-donor 6503253254 "Verizon"))
(define D2 (make-donor 4084837327 "AT&T"))
(define D3 (make-donor 6504647184 "Sprint"))

;; example of ListOfDonor
(define LOD1 (cons D1 (cons D2 (cons D3 empty))))


;;
;; Problem 2
;;

(define-struct agency (name txtmessage num amt lod))
;; agency is a (make-agency String String Natural)
;; interp: an agency consists of a
;;    name that is a String
;;    txtmessage that is a String
;;    num that is the number you text too as a Natural
;;    amt that is the amount you donate for texting that number/message as a Natural
;;    and lod which is a ListOfDonors who texted that number/message as a ListofDonors

;; ListofAgency can be:
;; empty
;; (cons agency ListofAgency)

;;example of Agencies
(define A1 (make-agency "pray for syria" "SYRIA" 102931 15 LOD1))
(define A2 (make-agency "pray for the blind" "BLIND" 302010 10 LOD1))
(define A3 (make-agency "free Mandela" "TheDela" 696666 30.5 LOD1))
  
;;example of ListOfAgencies
(define LOA1 (cons A1 (cons A2 (cons A3 empty))))


;;
;; Problem 3
;;


;; Template for problem 1 data definition

;; template for donor data structure
;#
(define (fxn-for-donor d)
        (... (donor-cell d)
             (donor-carrier d)))

;; template for ListofDonor
;#
(define (fxn-for-lod lod)
        (cond [(empty? lod)  (...) ]
              [(cons? lod)   (... (first lod)...(rest lod))]))

;; Template for problem 2 data definitions(check-expect (total-donations LOA3) 40)

;; template for agency data structure
;#
(define (fxn-for-agency a)
        (... (agency-name a)
             (agency-txtmessage a)
             (agency-num a)
             (agency-amt a)
             (agency-lod a)))

;; template for ListofAgency
;#
(define (fxn-for-loa loa)
        (cond [(empty? loa)  (...) ]
              [(cons? loa)   (... (first loa)...(rest loa))]))


;; more defined donors, list of donors, agencies, and lists of agencies for the check-expects of problems 4-7
(define D4 (make-donor 3820238393 "Veribon"))
(define D5 (make-donor 0349587302 "AT&G"))
(define D6 (make-donor 2934620343 "Spint"))
(define D7 (make-donor 1092341732 "Zeribon"))
(define D8 (make-donor 2345893402 "AY&G"))
(define D9 (make-donor 2132121232 "Sint"))
(define LOD2 (cons D3 (cons D7 (cons D9 empty))))
(define LOD3 (cons D2 (cons D6 (cons D5 (cons D7 empty)))))
(define LOD4 (cons D1 (cons D5 (cons D8 (cons D4 empty)))))
(define A4 (make-agency "pray for Pacquiao" "THEPAQ" 234522 10 LOD4))
(define A5 (make-agency "1 like = 1 prayer" "nope" 123231 15 LOD2))
(define A5a (make-agency "1 like = 1 prayer" "nope" 123231 10 LOD1))
(define A6 (make-agency "send it?" "sendit!" 383838 5 LOD3))
(define A7 (make-agency "save the birds" "burd" 383293 10 empty))
(define A8 (make-agency "save the birds" "burd" 198313 20 empty))
(define A9 (make-agency "help the habitats" "habitat" 183923 35 empty))
(define LOA2 (cons A4 (cons A5 (cons A6 empty))))
(define LOA3 (cons A1 (cons A5 (cons A2 empty))))
(define LOA4 (cons A3 (cons A6 (cons A1 empty))))
(define LOA5 (cons A1 (cons A2 (cons A3 (cons A4 (cons A5 (cons A6 empty)))))))
(define LOA6 empty)
(define LOA7 (cons A9 (cons A1 (cons A8 (cons A5 empty)))))
(define LOA8 (cons A7 (cons A8 (cons A5 (cons A5a empty)))))
;; 

;;
;; Problem 4
;;

;; increase-ten-by-five: ListofAgency --> ListofAgency
;; consumes a list of agencies and produces the same list, but if an agency in the list has the donation amount 10$ it changes the amount to 15$

(define (increase-ten-by-five loa)
        (cond [(empty? loa) empty]
              [(cons? loa)   (cons (change-agency (first loa)) (increase-ten-by-five (rest loa)))]))





;; change-agency is a helper fxn: agency --> agency
;; consumes and agency and returns the same agency except if the original agency has an amount of 10, change it to 15.
(define (change-agency agency)
  (if(= (agency-amt agency) 10)
     (make-agency (agency-name agency) (agency-txtmessage agency) (agency-num agency) 15 (agency-lod agency))
     agency))

;; Test Cases
(check-expect (increase-ten-by-five empty) empty)
(check-expect (increase-ten-by-five LOA2) (cons (make-agency "pray for Pacquiao" "THEPAQ" 234522 15 LOD4) (cons A5 (cons A6 empty))))
(check-expect (increase-ten-by-five LOA8) (cons (make-agency "save the birds" "burd" 383293 15 empty) (cons A8 (cons A5 (cons (make-agency "1 like = 1 prayer" "nope" 123231 15 LOD1) empty)))))


;;
;; Problem 5
;;


;; agencies-with-no-donors: ListOfAgency --> ListOfAgency
;; consumes a ListOfAgency and produces a re(make-agency "1 like = 1 prayer" "nope" 123231 10 LOD2)vised list with agencies that only have no donors in their list of donors

(define (agencies-with-no-donors loa)
        (cond [(empty? loa) empty]
              [(cons? loa) (if (no-donors? loa)
                               (cons (first loa) (agencies-with-no-donors (rest loa))) (agencies-with-no-donors (rest loa)))]))

;; no-donors? is a helper fxn: ListOfAgency --> Boolean
;; consumes a list of agencies and produces true if the first agency of the list of agency does not have any donors, else produces false.

(define (no-donors? loa)
  (if (empty? (agency-lod (first loa))) true
                          false))

;; Test Cases
(check-expect (agencies-with-no-donors LOA6) empty)
(check-expect (agencies-with-no-donors LOA7) (cons A9 (cons A8 empty)))
(check-expect (agencies-with-no-donors LOA5) empty)




;;
;; Problem 6
;;

;; list-donor-numbers: ListOfAgency String --> ListofNatural
;; consumes a list of agencies and a string that is the name of the agency, it will produce a list of numbers that is the donor's numbers in the agency

(define (list-donor-numbers loa agencyname)
      (cond [(empty? loa) empty]
            [(cons? loa) (if (is-agency? (first loa) agencyname) (cons (donor-cell (first (agency-lod (first loa)))) (find-rest (rest (agency-lod (first loa))) agencyname (rest loa)))
                                                     (list-donor-numbers (rest loa) agencyname))]))

;; helper fxn is-agency?: Agency String --> boolean
;; consumes an agency and a string name and returns true if the string and the agency name are the same, and false if not.

(define (is-agency? agency agencyname) 
  (if(string=? (agency-name agency) agencyname) true false))

;; helper fxn find-rest: ListOfDonors --> natural 
;; consumes a list of donors and returns the rest of the list of phone numbers of the donors.

(define (find-rest lod agencyname loa)
        (cond [(empty? lod)  (list-donor-numbers loa agencyname) ]
              [(cons? lod) (cons (donor-cell (first lod)) (find-rest (rest lod) agencyname loa))]))


;;check expects for problem 6
(check-expect (list-donor-numbers empty "send it?") empty)
(check-expect (list-donor-numbers LOA2 "pray for Pacquiao") (cons 6503253254 (cons 0349587302 (cons 2345893402 (cons 3820238393 empty)))))
(check-expect (list-donor-numbers LOA3 "1 like = 1 prayer") (cons 6504647184 (cons 1092341732 (cons 2132121232 empty))))
(check-expect (list-donor-numbers LOA5 "free Mandela") (cons 6503253254 (cons 4084837327 (cons 6504647184 empty))))
;;(check-expect (list-donor-numbers LOA8 "save the birds") empty) 
;;(check-expect (list-donor-numbers LOA8 "1 like = 1 prayer") (cons 6504647184 (cons 1092341732 (cons 2132121232 (cons 6503253254 (cons 4084837327 (cons 6504647184 empty)))))))

;;
;; Problem 7
;;

;; total-donations: ListOfAgencies --> Natural
;; consumes a list of agencies and produces the sum of all the donation amounts in all the agencies in the list.

(define (total-donations loa)
        (cond [(empty? loa) 0]
              [(cons? loa) (+ (donation-amount loa) (total-donations (rest loa)))]))

;; donation-amount is a help fxn: ListOfAgency --> Natural
;; consumes a list of agencies and produces the donation amount of the first agency in the list.

(define (donation-amount loa)
  (agency-amt (first loa)))

;;check expects for problem 7 
(check-expect (total-donations empty) 0)
(check-expect (total-donations LOA2) 30)
(check-expect (total-donations LOA3) 40)
(check-expect (total-donations LOA5) 85.5)