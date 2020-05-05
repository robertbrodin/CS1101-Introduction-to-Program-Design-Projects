;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname brodin-mcdonough-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Robert Brodin - rbrodin@wpi.edu - Homework 1
;; Homework Partner - Conor McDonough - cmcdonough

;; 1.

(define-struct date (DAY MONTH YEAR))
;; Date is (make-date Natural Natural Natural), where
;; interp.
;; the day is, DAY, a natural
;; the month is, MONTH, a natural
;; the year is, YEAR, a natural

(define DATE_TEST_ONE (make-date 05 06 1997))
(define DATE_TEST_TWO (make-date 06 07 1998))
(define DATE_TEST_THREE (make-date 08 09 2000))

;; Template: (define DATE_NAME (make-date day1 month1 year1))

(define-struct film (TITLE GENRE RUN_TIME OPEN_DATE TOTAL_RECEIPTS))
;; Film: (make-film String String Natural Date Natural)
;; interp.
;; the title of the film is, TITLE, a string
;; the genre of the film is, GENRE, a string
;; the running time of the film (in minutes) is, RUN_TIME, a natural
;; the opening date of the film, OPEN_DATE is, a date (date struct defined below)
;; the total box office receipts collected for the film (in millions of dollars) is, TOTAL_RECEIPTS, a natural.
(define FILM_TEST_ONE (make-film "Star Wars: A New Hope" "Space" 100 (make-date 08 10 1976) 100))
(define FILM_TEST_TWO (make-film "Star Wars: The Empire Strikes Back" "Space" 100 (make-date 07 10 1979) 300))
(define FILM_TEST_THREE (make-film "Star Wars: Revenge of the Sith" "Space" 100 (make-date 07 10 2004) 200))

;; Template: (define FILM_NAME (make-film (title genre runtime opendate totalreceipts))


;; 3.

;; COMEDY_ONE is a thousand minutes, and will be used in the check-expect, and NOT_COMEDY_ONE is 10 minutes, and will be used in the check-expect
(define COMEDY_ONE (make-film "A Comedy" "Comedy" 1000 (make-date 07 10 2005) 0))
(define NOT_COMEDY_ONE (make-film "Another Comedy" "Not a Comedy" 1000 (make-date 07 10 2005) 1))
;; COPY_OF_COMEDY_ONE is used 
(define COPY_OF_COMEDY_ONE (make-film "A Comedy" "Comedy" 1000 (make-date 07 10 2005) 0))

;; Template: (define FILM_NAME (make-film title genre runtime opendate totalreceipts))

;; Check expects are designed to be right before the cutoff of true and false. For example, if the runtime is 1000 for the movie and the cutoff is 1000, the function will return false.
(check-expect (short-comedy? COMEDY_ONE 100) false)
(check-expect (short-comedy? COMEDY_ONE 1001) true)
(check-expect (short-comedy? NOT_COMEDY_ONE 100) false)
(check-expect (short-comedy? NOT_COMEDY_ONE 1001) false)
(check-expect (short-comedy? NOT_COMEDY_ONE 1000) false)

;; short-comedy?: Film Natural -> Boolean
;; short-comedy? takes a film and some number of minutes, and returns a boolean indicating whether the film is a comedy and is less than the number of minutes supplied.
;; otherwise, the function returns false.
;; Stub: (define (short-comedy? film runtime) true)
;; Template: (define (short-comedy? n1 n2) (... n n2))

(define (short-comedy? THE_FILM RUN_TIME) (cond[(and (equal? "Comedy" (film-GENRE THE_FILM)) (< (film-RUN_TIME THE_FILM) RUN_TIME)) true]
                                               [else false]))

;; 4.

;; The check expects check if the film has more receipts, and the order is switched on the movies to make sure it works both ways.
;; COPY_OF_COMEDY_ONE is used to make sure that the else statement works, meaning that the receipts are equal.
(check-expect (film-with-more-receipts COMEDY_ONE NOT_COMEDY_ONE) NOT_COMEDY_ONE)
(check-expect (film-with-more-receipts NOT_COMEDY_ONE COMEDY_ONE) NOT_COMEDY_ONE)
(check-expect (film-with-more-receipts COMEDY_ONE COPY_OF_COMEDY_ONE) COMEDY_ONE)
(check-expect (film-with-more-receipts COPY_OF_COMEDY_ONE COMEDY_ONE) COPY_OF_COMEDY_ONE)

;; film-with-more-receipts: Film Film -> Film
;; film-with-more-receipts: takes two Films and returns the Film with the higher number of box office receipts, if they are equal, the function will return the first Film.
;; Stub: (define (film-with-more-receipts film1 film2) film1)
;; Template: (define (film-with-more-receipts film1 film2) (... film1))

(define (film-with-more-receipts FILM_ONE FILM_TWO) (cond[(< (film-TOTAL_RECEIPTS FILM_ONE) (film-TOTAL_RECEIPTS FILM_TWO)) FILM_TWO]
                                                         [(< (film-TOTAL_RECEIPTS FILM_TWO) (film-TOTAL_RECEIPTS FILM_ONE)) FILM_ONE]
                                                         [else FILM_ONE]))
;; 5.

;; The first check expect checks the function with 0 trailers, so the runtime would stay the same.
(check-expect (add-time-for-trailers COMEDY_ONE 0) (make-film "A Comedy" "Comedy" 1000 (make-date 07 10 2005) 0))
;; The second check expect checks the function with 1 trailer, so the runtime would be 1000 + 3, which is 1003.
(check-expect (add-time-for-trailers COMEDY_ONE 1) (make-film "A Comedy" "Comedy" 1003 (make-date 07 10 2005) 0))
;; The third check expect checks the function with 2 trailers, to the runtime would be 1000 + (3 * 2), which is 1006.
(check-expect (add-time-for-trailers COMEDY_ONE 2) (make-film "A Comedy" "Comedy" 1006 (make-date 07 10 2005) 0))

;; add-time-for-trailers: Film -> Film
;; add-time-for-trailers: takes a Film and makes a new film based on the film taken, but changes the runtime by three minutes multiplied by the number of trailers. Everything else is the same. The new film is then returned.
;; Stub: (define (add-time-for-trailers film1) Film)
;; Template: (define (add-time-for-trailers film1) (.... film1))

(define (add-time-for-trailers THE_FILM NUMBER_OF_TRAILERS) (make-film (film-TITLE THE_FILM) (film-GENRE THE_FILM) (+ (film-RUN_TIME THE_FILM) (* NUMBER_OF_TRAILERS 3)) (film-OPEN_DATE THE_FILM) (film-TOTAL_RECEIPTS THE_FILM)))


;; All of these variables will be compared against October 3, 2019.
;; FILM_DATE_CHECK_ONE is a year before, to check the first conditional.
;; FILM_DATE_CHECK_TWO is a month before, to check the second conditional.
;; FILM_DATE_CHECK_THREE is a day before, to check the third conditional.
;; FILM_DATE_CHECK_FOUR is the day of, to check the last conditional (if the else statement catches equal to).
;; FILM_DATE_CHECK_FIVE is the day after, to check the last conditional (if the else statement catches greater than).
(define DATE_CHECK (make-date 03 10 2019))
(define FILM_DATE_CHECK_ONE (make-film "A Comedy" "Comedy" 1000 (make-date 03 10 2018) 0))
(define FILM_DATE_CHECK_TWO (make-film "A Comedy" "Comedy" 1000 (make-date 02 10 2019) 0))
(define FILM_DATE_CHECK_THREE (make-film "A Comedy" "Comedy" 1000 (make-date 03 09 2019) 0))
(define FILM_DATE_CHECK_FOUR (make-film "A Comedy" "Comedy" 1000 (make-date 03 10 2019) 0))
(define FILM_DATE_CHECK_FIVE (make-film "A Comedy" "Comedy" 1000 (make-date 03 11 2019) 0))

;; 6.

;; Read the comment above definitions, which explains how the check-expects work in relation to the variables.
(check-expect (opens-before? FILM_DATE_CHECK_ONE DATE_CHECK) true)
(check-expect (opens-before? FILM_DATE_CHECK_TWO DATE_CHECK) true)
(check-expect (opens-before? FILM_DATE_CHECK_THREE DATE_CHECK) true)
(check-expect (opens-before? FILM_DATE_CHECK_FOUR DATE_CHECK) false)
(check-expect (opens-before? FILM_DATE_CHECK_FIVE DATE_CHECK) false)

;; opens-before?: Film Date -> Boolean
;; opens-before?: Checks if the given film opens before the given date, the function returns true if it does, and false if it does not.
;; Stub: (define (opens-before? film1 date1) true)
;; Template: (define opens-before? (film1 date1) (... film1 date1))

(define (opens-before? THE_FILM THE_DATE) (cond[(< (date-YEAR (film-OPEN_DATE THE_FILM)) (date-YEAR THE_DATE)) true]
                                               [(< (date-MONTH (film-OPEN_DATE THE_FILM)) (date-MONTH THE_DATE)) true]
                                               [(< (date-DAY (film-OPEN_DATE THE_FILM)) (date-DAY THE_DATE)) true]
                                               [else false]))