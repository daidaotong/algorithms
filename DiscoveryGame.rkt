#lang racket
(define Direction '("North" "South" "West" "East"))
(define NorthStreet '("NorthStreet" "GoOut" "TownSquire" "ThiefGuild" "Bakery" "Sign" "Pen"))
(define SouthStreet '("SouthStreet" "TownSquire" "GoOut" "BlackSmith" "WizardTower" "Sign" "Shoe"))
(define WestStreet '("WestStreet" "ThiefGuild" "BlackSmith" "UsedAnvils" "TownSquire" "Sign" "Paper"))
(define EastStreet '("EastStreet" "Bakery" "WizardTower" "TownSquire" "GoOut" "Sign" "Money"))
(define TownSquire '("TownSquire" "NorthStreet" "SouthStreet" "WestStreet" "EastStreet" "Fountain" "Map" ))
(define Bakery  '("Bakery" "GoOut" "EastStreet" "NorthStreet" "GoOut" "Advertisement" "Bread"))
(define ThiefGuild '("ThiefGuild" "GoOut" "WestStreet" "GoOut" "NorthStreet" "Document" "Hammer"))
(define UsedAnvils '("UsedAnvils" "GoOut" "GoOut" "GoOut" "WestStreet" "Instruction" "Stone"))
(define BlackSmith '("BlackSmith" "WestStreet" "GoOut" "GoOut" "GoOut" "Photo" "Clothes"))
(define WizardTower '("WizardTower" "WestStreet" "GoOut" "SouthStreet" "ObsDeck" "Scenery" "Picture"))
(define ObsDeck '("ObsDeck" "GoOut" "WizardTower" "WestStreet" "GoOut" "Whole Town" "Gold"))
(define Inventory '())

(define GoOut '(""))

(define (get-answer)
  (letrec ([input (read-line)])
    (displayln input)
    (string->number input)))  

(define (describe-room room)
  (let ([name (car room)]
        [north (cadr room)]
        [south (caddr room)]
        [west (cadddr room)]
        [east (car(cdr(cdddr room)))]
        [items (cdr(cdr(cdddr room)))])
    (displayln (string-append "You are in a " name))
     (displayln (string-append "North is " north))
     (displayln (string-append "South is " south))
     (displayln (string-append "West is " west))
     (displayln (string-append "East is " east))
    (displayln (string-append "In the space, there is " (list-items items)))))

(define (list-items items)
  (cond [(= 0 (length items)) "nothing"]
        [(= 1 (length items)) (string-append "a " (car items))]
        [else (string-append "a " (car items) ", " (list-items (cdr items)))]))

                              
(define (show-menu room-thing)
  (displayln "What do you want to do?")
  (displayln (string-append "  1: Go to " (car Direction)) )
  (displayln (string-append "  2: Go to " (cadr Direction)) )
  (displayln (string-append "  3: Go to " (caddr Direction)) )
   (displayln (string-append " 4: Go to " (cadddr Direction)) )
  (displayln (string-append " 5: See the " (car room-thing)) )
  (displayln (string-append " 6: Pick up " (car(cdr room-thing))) )
   (displayln (string-append " 7: See the inventory " ) )
 
  (display "Your choice? "))

(define (convert string)
  (cond
    (( eqv? string "TownSquire") TownSquire)
    (( eqv? string "NorthStreet") NorthStreet)
    (( eqv? string "SouthStreet") SouthStreet)
    (( eqv? string "WestStreet") WestStreet)
    (( eqv? string "EastStreet") EastStreet)
    (( eqv? string "Bakery") Bakery)
    (( eqv? string "ThiefGuild") ThiefGuild)
    (( eqv? string "UsedAnvils") UsedAnvils)
    (( eqv? string "BlackSmith") BlackSmith)
    (( eqv? string "WizardTower") WizardTower)
    (( eqv? string "ObsDeck") ObsDeck)
     (( eqv? string "GoOut") GoOut)
  ))

(define (see item)
  (displayln (string-append " You are seeing the " item) )
  )
(define (pick-up item)
  (displayln (string-append " You pick up the " item) )
  (append* item Inventory)
  )

(define (see-inventory)
  (display "You have ")
  (show Inventory)
  )
(define (show ls)
  (if(null? ls)
  (displayln "nothing")
  ((displayln (car ls))(show cdr ls)
  )))

(define (inside-room room)
  (if( eqv? (car room) "" )
     (displayln "Thank you for Playing")
  (let ([room-thing (cdr(cdr(cdddr room)))])
    (describe-room room)
    (show-menu room-thing)
    (let ([ans (get-answer)])
      (cond
        [(= ans 1) (inside-room (convert(cadr room)))]
        [(= ans 2) (inside-room (convert(caddr room)))]
        [(= ans 3) (inside-room (convert(cadddr room)))]
        [(= ans 4) (inside-room (convert(car(cdr(cdddr room)))))]
        [(= ans 5) (see (car(cdr(cdr(cdddr room)))))(inside-room room)]
        [(= ans 6) (pick-up (car(cdr(cdr(cdr(cdddr room))))))(inside-room room)]
         [(= ans 7) (see-inventory)(inside-room room)]
        [else (displayln "Sorry, I did not understand your choice") (inside-room room)])))))






  (displayln "GUTEN TAG")
  (displayln "Text Adventure Demo!")
  (displayln "====================")
  
  (inside-room TownSquire)


 

 
