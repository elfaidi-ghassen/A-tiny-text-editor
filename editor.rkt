;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname final_project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
;; A simple text editor
;; =================
;; Constants:
(define HEIGHT 30)
(define WIDTH 400)
(define MTS (empty-scene WIDTH HEIGHT))
(define YCTR (/ HEIGHT 2))
(define CARET-IMG (rectangle 1 HEIGHT "solid" "black"))

;; =================
;; Data definitions:
(define-struct input (index text))
;; input is (make-input (Natural String))
;; interp. (make-input (index text)) contains the relavent data about a text input
;; - the index of the caret, starting at 0
;; - the text inside the input fields
(define C1 (make-input 0 "hello")) ; |hello
(define C2 (make-input 1 "hello")) ; h|ello
(define C3 (make-input 5 "hello")) ; hello|
(define C4 (make-input 0 ""     )) ; |


#;(define (fn-for-input i)
    (... (input-index i) (input-text i)))

;; template rules used
;; - compound data: 2 fields

;; =================
;; Functions:
;; input -> input
;; start the world with (make-input (0 ""))
;; 
(define (main input)
 (big-bang input ; input
 (to-draw render) ; input -> Image
 (on-key key-handler))) ; input KeyEvent -> input


;; input KeyEvent -> input
;; update the input data when a key is pressed
;; 
(check-expect (key-handler (make-input 0 "") "h") (make-input 1 "h"))
(check-expect (key-handler (make-input 1 "h") "i") (make-input 2 "hi"))
(check-expect (key-handler (make-input 0 "ello") "h") (make-input 1 "hello"))
(check-expect (key-handler (make-input 2 "tet") "s") (make-input 3 "test"))

(check-expect (key-handler (make-input 0 "") "\b") (make-input 0 ""))
(check-expect (key-handler (make-input 0 "hello") "\b") (make-input 0 "hello"))
(check-expect (key-handler (make-input 1 "hello") "\b") (make-input 0 "ello"))
(check-expect (key-handler (make-input 1 "h") "\b") (make-input 0 ""))

(check-expect (key-handler (make-input 5 "hello") "left") (make-input 4 "hello"))
(check-expect (key-handler (make-input 4 "hello") "right") (make-input 5 "hello"))
(check-expect (key-handler (make-input 5 "hello") "right") (make-input 5 "hello"))
(check-expect (key-handler (make-input 0 "hello") "left") (make-input 0 "hello"))
(check-expect (key-handler (make-input 0 "") "left") (make-input 0 ""))
(check-expect (key-handler (make-input 0 "") "right") (make-input 0 ""))




(define (key-handler i key) 
(cond
  [(key=? key "shift") i]
  [(key=? key "rshift") i]
  [(key=? key "control") i]
  [(key=? key "rcontrol") i]
  [(key=? key "escape") i]
  [(key=? key "up") i]
  [(key=? key "down") i]
  [(key=? key "start") i]
  [(key=? key "cancel") i]
  [(key=? key "clear") i]
  [(key=? key "menu") i]
  [(key=? key "pause") i]
  [(key=? key "capital") i]
  [(key=? key "prior") i]
  [(key=? key "next") i]
  [(key=? key "end") i]
  [(key=? key "home") i]
  [(key=? key "select") i]
  [(key=? key "print") i]
  [(key=? key "execute") i]
  [(key=? key "snapshot") i]
  [(key=? key "insert") i]
  [(key=? key "help") i]
  [(key=? key "f1") i]
  [(key=? key "f2") i]
  [(key=? key "f3") i]
  [(key=? key "f4") i]
  [(key=? key "f5") i]
  [(key=? key "f6") i]
  [(key=? key "f7") i]
  [(key=? key "f8") i]
  [(key=? key "f9") i]
  [(key=? key "f10") i]
  [(key=? key "f11") i]
  [(key=? key "f12") i]
  [(key=? key "f13") i]
  [(key=? key "f14") i]
  [(key=? key "f15") i]
  [(key=? key "f16") i]
  [(key=? key "f17") i]
  [(key=? key "f18") i]
  [(key=? key "f19") i]
  [(key=? key "f20") i]
  [(key=? key "f21") i]
  [(key=? key "f22") i]
  [(key=? key "f23") i]
  [(key=? key "f24") i]
  [(key=? key "numlock") i]
  [(key=? key "scroll") i]

  [(key=? key "left")
      (make-input (add-in-range (input-index i) -1 0 (string-length (input-text i))) (input-text i))]

  [(key=? key "right")
      (make-input (add-in-range (input-index i) 1 0 (string-length (input-text i))) (input-text i))]

  [(key=? key "\b")
      (make-input (add-in-range (input-index i) -1 0 (string-length (input-text i))) (remove-char (input-text i) (+ (input-index i) -1)))]

  [else
      (make-input (+ (input-index i) 1) (add-char (input-text i) key (input-index i)))]
  ))


;; String Natural -> String
;; remove a character at a specific position
(check-expect (remove-char "hello" -1) "hello")
(check-expect (remove-char "hello" 0) "ello")
(check-expect (remove-char "hello" 1) "hllo")
(check-expect (remove-char "b" 0) "")
(check-expect (remove-char "" 0) "")

; (define (remove-char str index))

(define (remove-char str index)
  (cond [(or (= index -1) (string=? str "")) str]
        [else
         (string-append(substring str 0 index)
          (substring str (+ index 1)))
         ]
        )
  )




;; String String Natural -> String
;; add a string at the a specific position
(check-expect (add-char "howd" "y" 4) "howdy")
(check-expect (add-char "" "A" 0) "A")
(check-expect (add-char "i" "H" 0) (string-append (substring "i" 0 0) "H" (substring "i" 0)))


(define (add-char str c index)
  (string-append (substring str 0 index) c (substring str index))
  )




;; Integer Integer Intger Integer -> Integer
;; produce the result of adding a and b if the result is in range, else return a

(check-expect (add-in-range 5 1 0 5) 5) ; and not 6 because (+ 5 1) is not in [0 5]
(check-expect (add-in-range 5 2 0 10) 7) ; 7 because (+ 5 2) is in [0 10]
(define (add-in-range a b min max)
  (if (and (>= (+ a b)  min) (<= (+ a b)  max))
      (+ a b)
      a
      ))
  


;; input -> Image
;; render the input fields with text and caret at correct place

(check-expect (render (make-input 0 "")) (place-image (beside
                                              (text "" 24 "black")
                                              CARET-IMG
                                              (text "" 24 "black")
                                              ) 0 YCTR MTS))

(check-expect (render(make-input 1 "hello")) (place-image/align (beside
                                              (text "h" 24 "black")
                                              CARET-IMG
                                              (text "ello" 24 "black")
                                              ) 0 YCTR "left" "center" MTS))

(check-expect (render (make-input 5 "hello")) (place-image/align (beside
                                              (text "hello" 24 "black")
                                              CARET-IMG
                                              (text "" 24 "black")
                                              ) 0 YCTR "left" "center" MTS))

(define (render i)
(place-image/align (beside
   (text (substring (input-text i) 0 (input-index i)) 24 "black")
      CARET-IMG
      (text (substring (input-text i) (input-index i)) 24 "black")
      ) 0 YCTR "left" "center" MTS)

  )