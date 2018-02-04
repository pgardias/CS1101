;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gardias-lab1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Lab 1 - 1/18/17
;; Przemek Gardias
;; pmgardias@wpi.edu

;; Function season: string number -> string
;; The function consumes an input of the date, with month as a string and the day as an integer.
;; The ouput of the function is the season which the given date falls in, given as a string. 
;; spring: Mar 20 - Jun 19
;; summer: Jun 20 - Sept 21
;; fall: Sept 22 - Dec 20
;; winter: Dec 21 - Mar 19

(define (season month day)
  (cond [(string=? "January" month) "Winter"]
        [(string=? "February" month) "Winter"]
        [(and(string=? "March" month) (< day 20)) "Winter"]
        [(and(string=? "March" month) (>= day 20)) "Spring"]
        [(string=? "April" month) "Spring"]
        [(string=? "May" month) "Spring"]
        [(and(string=? "June" month) (< day 20)) "Spring"]
        [(and(string=? "June" month) (>= day 20)) "Summer"]
        [(string=? "July" month) "Summer"]
        [(string=? "August" month) "Summer"]
        [(and(string=? "September" month) (< day 22)) "Summer"]
        [(and(string=? "September" month) (>= day 22)) "Fall"]
        [(string=? "October" month) "Fall"]
        [(string=? "November" month) "Fall"]
        [(and(string=? "December" month) (< day 21)) "Fall"]
        [(and(string=? "December" month) (>= day 21)) "Winter"]))

;; Test function season.
(check-expect (season "March" 20) "Spring")
(check-expect (season "June" 20) "Summer")
(check-expect (season "September" 22) "Fall")
(check-expect (season "December" 21) "Winter")
(check-expect (season "July" 14) "Summer")
