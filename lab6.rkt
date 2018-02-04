;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname gardias-lab6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Lab 6
;; Przemek Gardias
;; pmgardias@wpi.edu

;; In Homework 3 you wrote several functions that operated on a list of agencies in a mobile-giving database. Please use the following data definitions from Homework 3 for problems 1-4:
;; Data definitions from Homework 3

(define-struct donor (cell carrier))
;; a Donor is a (make-donor Natural String)
;; interp:  a donor where
;;     cell is the donor's cell phone number
;;     carrier is the name of the cell phone carrier

;; a ListOfDonor is one of
;; empty,
;; (cons Donor ListOfDonor)

(define-struct agency (name msg code amt donors))
;; an Agency is a (make-agency String String Natural Number ListOfDonor)
;; interp:  an agency for charitable giving, where
;;    name is the name of the charity
;;    msg is the message that is sent to the agency in a text
;;    code is the number that the text is sent to
;;    amt is the amount that's charged to the donor when msg is texted to code
;;    donors is a list of donors who have texted the msg to code

;; a ListOfAgency is one of
;; empty,
;; (cons Agency ListOfAgency)

;; example of Donors
(define D1 (make-donor 6503253254 "Verizon"))
(define D2 (make-donor 4084837327 "AT&T"))
(define D3 (make-donor 6504647184 "Sprint"))

;; example of ListOfDonor
(define LOD1 (cons D1 (cons D2 (cons D3 empty))))

;; example of Agencies
(define A1 (make-agency "pray for syria" "SYRIA" 102931 15 LOD1))
(define A2 (make-agency "pray for the blind" "BLIND" 302010 10 LOD1))
(define A3 (make-agency "free Mandela" "TheDela" 696666 30.5 LOD1))
  
;; example of ListOfAgencies
(define LOA1 (cons A1 (cons A2 (cons A3 empty))))

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
;; Problem 1
;;
;; Using filter and/or map, redefine this function from Homework 3:
;; Write a function increase-ten-by-five that consumes a list of agencies and produces a list of agencies.
;; The list that is produced is the same as the original, except that every agency in the list that currently
;; has a donation amount of $10 has its amount changed to $15.


;; increase-ten-by-five: ListofAgency --> ListofAgency
;; consumes a list of agencies and produces the same list, but if an agency in the list has the donation amount 10$ it changes the amount to $15

(define (increase-ten-by-five aloa)
  (map increase-ten aloa))

;; increase-ten: Agency --> Agency
;; consumes an Agency and checks if the donations amount is 10,
;; if so, it returns the same agency with a donation total of 15,
;; otherwise it remains unchanged.
(define (increase-ten agency)
  (if (= (agency-amt agency) 10)
      (make-agency (agency-name agency) (agency-msg agency) (agency-code agency) 15 (agency-donors agency))
      agency))

;; Test cases
(check-expect (increase-ten-by-five empty) empty)
(check-expect (increase-ten-by-five LOA2) (cons (make-agency "pray for Pacquiao" "THEPAQ" 234522 15 LOD4) (cons A5 (cons A6 empty))))
(check-expect (increase-ten-by-five LOA8) (cons (make-agency "save the birds" "burd" 383293 15 empty) (cons A8 (cons A5 (cons (make-agency "1 like = 1 prayer" "nope" 123231 15 LOD1) empty)))))


;;
;; Problem 2
;;
;; Using filter and/or map, redefine this function from Homework 3: Write a function agencies-with-no-donors that consumes a list of agencies
;; and produces a list of only those agencies that have no donors in their donor list.

;; agencies-with-no-donors: ListOfAgency --> ListOfAgency
;; consumes a ListOfAgency and produces a revised list with agencies that only have no donors in their list of donors.
(define (agencies-with-no-donors aloa)
  (filter no-donors? aloa))

;; no-donors?: ListOfAgency --> Boolean
;; consumes a list of agencies and produces true if the first agency of the list of agency does not have any donors, else produces false.
(define (no-donors? a-agency)
  (empty? (agency-donors a-agency)))

;; Test cases
(check-expect (agencies-with-no-donors LOA6) empty)
(check-expect (agencies-with-no-donors LOA7) (cons A9 (cons A8 empty)))
(check-expect (agencies-with-no-donors LOA5) empty)


;;
;; Problem 3
;;
;; Using filter and/or map, redefine this function from Homework 3: Write a function list-donor-numbers that consumes a list of agencies
;; and a String representing the name of an agency, and produces a list of naturals. The function produces a list of the telephone numbers
;; of all donors for the agency with the given name. You should assume that there may be several agencies with the same name. (This problem is not quite as
;; straightforward as problems 1 and 2. Use filter and map where you can; you may find that you need to develop a list function as a helper where neither filter nor map is appropriate.)


;; list-donor-numbers: ListOfAgency String --> ListofNatural
;; consumes a list of agencies and a string that is the name of the agency, it will produce a list of numbers that is the donor's numbers in the agency
(define (list-donor-numbers aloa name)
  (local [(define (agency-checker agency)
                               (string=? (agency-name agency) name))]
   (map donor-cell(donor-lister(map agency-donors (filter agency-checker aloa))))))

;; donor-lister: ListOfDonors --> ListOfDonors
;; consumes a ListOfDonor split into multiple sub-lists
;; and appends them all to one list
(define (donor-lister alod)
  (cond [(empty? alod) empty]
        [(cons? alod) (append (first alod) (donor-lister (rest alod)))]))


;; Test cases
(check-expect (list-donor-numbers empty "send it?") empty)
(check-expect (list-donor-numbers LOA2 "pray for Pacquiao") (cons 6503253254 (cons 0349587302 (cons 2345893402 (cons 3820238393 empty)))))
(check-expect (list-donor-numbers LOA3 "1 like = 1 prayer") (cons 6504647184 (cons 1092341732 (cons 2132121232 empty))))
(check-expect (list-donor-numbers LOA5 "free Mandela") (cons 6503253254 (cons 4084837327 (cons 6504647184 empty))))

;;
;; Problem 4
;;
;; Using accumulator-style programming, redefine this function from Homework 3: Write a function total-donations that consumes a list of agencies and produces the
;; total amount of all donations pledged to all agencies in the list. (If you would like to, you may make use of the built-in Racket function length, which consumes
;; a list of any type and produces the number of items in the list.)

;; total-donations: ListOfAgencies --> Natural
;; consumes a list of agencies and produces the sum of all the donation amounts in all the agencies in the list.
(define (total-donations aloa)
  (local [(define (total-donations aloa acc)
            (cond [(empty? aloa) 0]
                  [(cons? aloa) (+ acc (agency-amt (first aloa)) (total-donations (rest aloa) acc))]))]
    (total-donations aloa 0)))

;; Test cases
(check-expect (total-donations empty) 0)
(check-expect (total-donations LOA2) 30)
(check-expect (total-donations LOA3) 40)
(check-expect (total-donations LOA5) 85.5)

