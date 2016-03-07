#lang racket

;;Student Name:Daotong Dai  Student No:0646436

(define (GregorianDay month day year);;this is the main function, in order to calculate a given date in Gregorian Calendar 
  (local [(define (findDayOfWeek month day year);;this is the first function, in order to return the day number after module 7
            (local [(define (doomsDay year) ;;this is a local function of finddayofweek, in order to find the doomsday of the year
                      (local [(define (findAnchorDay year) ;;this is a local function of doomsDay, in order to find the anchor's of the year
                                (let ((c (floor(/ year 100))))
                                  ( + (modulo (* 5 (modulo c 4)) 7) 2);;(5*(c mod 4)) mod 7=anchor
                                  ))
                              (define (evenOrOdd year);;in order to see whether the year is even or odd,if odd then add 11 
                                (let ((t (modulo  year 100)))
                                  (cond ((= (modulo  t 2) 1)(+ t 11) )
                                        (else t)     
                                        ))
                                )]
                        (modulo (+ (findAnchorDay year) (- 7 (modulo (evenOrOdd (/ (evenOrOdd year) 2)) 7))) 7));;calculate the doomsday of a year
                      )]
              ;;this is the body of function finddayofweek
              (cond ((not(integer? year)) "Year Type wrong") ;;test the year type
                    (else (let ((doomsday (doomsDay year)))
                            (cond ((=(modulo year 4) 0);;if the year is a leap year,then calculate the day based on the situation of  different months
                                   (cond ((= month 1) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+ (- day 4)doomsday)  7)))
                                         ((= month 2) (if (or (not(integer? day))(> day 29)(< day 1)) "Day Type Wrong" (modulo (+ (- day 29)doomsday) 7)))
                                         ((= month 3) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+  day  doomsday) 7)))
                                         ((= month 4) (if (or (not(integer? day))(> day 30)(< day 1)) "Day Type Wrong" (modulo (+ (- day 4)doomsday) 7)))
                                         ((= month 5) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+ (- day 9)doomsday) 7)))
                                         ((= month 6) (if (or (not(integer? day))(> day 30)(< day 1)) "Day Type Wrong" (modulo (+ (- day 6)doomsday) 7)))
                                         ((= month 7) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+ (- day 11)doomsday) 7)))
                                         ((= month 8) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+ (- day 8)doomsday) 7)))
                                         ((= month 9) (if (or (not(integer? day))(> day 30)(< day 1)) "Day Type Wrong" (modulo (+ (- day 5)doomsday) 7)))
                                         ((= month 10) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong"(modulo (+ (- day 10)doomsday) 7)))
                                         ((= month 11) (if (or (not(integer? day))(> day 30)(< day 1)) "Day Type Wrong" (modulo (+ (- day 7)doomsday) 7)))
                                         ((= month 12) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+ (- day 12)doomsday) 7)))
                                         (else "Month Type Wrong")));;if the month is not between 1-12, return error

                                  ;;if the year is not a leap year,then calculate the day based on the situation of  different months
                                  (else  (cond ((= month 1) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+ (- day 3)doomsday)  7)))
                                               ((= month 2) (if (or (not(integer? day))(> day 28)(< day 1)) "Day Type Wrong" (modulo (+ (- day 28)doomsday) 7)))
                                               ((= month 3) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+  day doomsday) 7)))
                                               ((= month 4) (if (or (not(integer? day))(> day 30)(< day 1)) "Day Type Wrong" (modulo (+ (- day 4)doomsday) 7)))
                                               ((= month 5) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+ (- day 9)doomsday) 7)))
                                               ((= month 6) (if (or (not(integer? day))(> day 30)(< day 1)) "Day Type Wrong" (modulo (+ (- day 6)doomsday) 7)))
                                               ((= month 7) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+ (- day 11)doomsday) 7)))
                                               ((= month 8) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+ (- day 8)doomsday) 7)))
                                               ((= month 9) (if (or (not(integer? day))(> day 30)(< day 1)) "Day Type Wrong" (modulo (+ (- day 5)doomsday) 7)))
                                               ((= month 10) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+ (- day 10)doomsday) 7)))
                                               ((= month 11) (if (or (not(integer? day))(> day 30)(< day 1)) "Day Type Wrong" (modulo (+ (- day 7)doomsday) 7)))
                                               ((= month 12) (if (or (not(integer? day))(> day 31)(< day 1)) "Day Type Wrong" (modulo (+ (- day 12)doomsday) 7)))
                                               (else "Month Type Wrong"))) ;;if the month is not between 1-12, return error
                                  )))))
            )
         (define (outPutWeek week) ;;this auxiliary function in order to return the name of date based on the return value of findDayOfWeek
           (if (not(integer? week)) week
               (cond ((= week 0) "Sunday" )
                     ((= week 1) "Monday" )
                     ((= week 2) "Tuesday" )
                     ((= week 3) "Wednesday" )
                     ((= week 4) "Thursday" )
                     ((= week 5) "Friday" )
                     ((= week 6) "Saturday" )
                     )
               ))]
    
    (outPutWeek (findDayOfWeek month day year))  ;;this is the combination of these two functions in the main function GregorianDay
  ))



;;test the results

;;today's example ,return wednesday
(GregorianDay  11 11 2015)

;;the example on the question sheet,return monday
(GregorianDay  8 15 2011)

;;the leap year example,return saturday
(GregorianDay  2 15 2014)

;;day wrong type, November do not have the 31th day
(GregorianDay  11 31 2015)

;;month wrong type, the 13th month do not exist
(GregorianDay  13 30 2015)



