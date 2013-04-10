;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
; Andrew Tran-Snake Game

(require 2htdp/image)
(require 2htdp/universe)

;------------------
; Data definitions
;------------------

; A Direction is current direction of travel for the worm, and is either
; - 0: up
; - 1: right
; - 2: down
; - 3: left

(define-struct worm (pos dir))
; A worm is a structure:
; - (make-worm posn number)
; interp. (make-worm x dir) represents the position of the worm x,
; and the direction the worm is moving, dir

(define-struct game (worm low))
; A game is a structure:
; - (make-game (list-of-worms))
; interp. (make-game (low)) represents a head
; and represents a list of worm segments that make up the tail

;-------------------
; Physical Contents
;-------------------

(define SCALE 30)
(define HEIGHT 30)
(define BODY-RADIUS (/ SCALE 2))
(define BACKGROUND-HEIGHT (* HEIGHT 20))
(define BACKGROUND-WIDTH (* SCALE 20))

; Snake
(define HEAD (circle BODY-RADIUS "solid" "red"))
(define BODY (circle BODY-RADIUS "solid" "orange"))


; Graphical Constants
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))
(define FOOD (circle BODY-RADIUS "solid" "blue"))


(define TEST-WORM-1 (make-worm (make-posn 50 50) 0))
(define TEST-WORM-2 (make-worm (make-posn 50 50) 1))
(define TEST-WORM-3 (make-worm (make-posn 50 50) 2))
(define TEST-WORM-4 (make-worm (make-posn 50 50) 3))
(define INITIAL-POSN (make-posn (* SCALE 5) (* HEIGHT 5)))
(define INITIAL-WORM (make-worm INITIAL-POSN 1))
(define INITIAL-WORM2 (make-worm (make-posn (* SCALE 4) (* HEIGHT 5))  1))
(define INITIAL-GAME (make-game  INITIAL-WORM (cons INITIAL-WORM2 empty)))

;---------------------------
; Core Function Definitions
;---------------------------

; Game -> Game 
; move the snake based on its direction
(check-expect (move TEST-WORM-1) (make-worm (make-posn 50 35) 0))
(check-expect (move TEST-WORM-2) (make-worm (make-posn 65 50) 1))
(check-expect (move TEST-WORM-3) (make-worm (make-posn 50 65) 2))
(check-expect (move TEST-WORM-4) (make-worm (make-posn 35 50) 3))
(define (move gs)
  (let*([pos (worm-pos gs)]
        [dir (worm-dir gs)]
        [x (posn-x pos)]
        [y (posn-y pos)])
    (cond [(= dir 0) (make-worm (make-posn x (- y (/ SCALE 2))) dir)]
          [(= dir 1) (make-worm (make-posn (+ x (/ SCALE 2)) y) dir)]
          [(= dir 2) (make-worm (make-posn x (+ y (/ SCALE 2))) dir)]
          [(= dir 3) (make-worm (make-posn (- x (/ SCALE 2)) y) dir)])))


; Game -> Game 
; move the snake based on its direction
(define (move-worm low)   
    (cond   
      [(empty? (rest low)) (cons (move (first low)) empty)]
      [(cons? low)(cons (move (first low)) (move-worm (rest low)))]))

; Game -> Game 
; move the snake based on its direction
(define (move-worm* gs)
  (make-game (move (game-worm gs)) (move-worm (game-low gs))))

; worm Command -> worm
; change the direction of the worm based on the command
(check-expect (change-dir TEST-WORM-2 "up") 
              (make-worm (worm-pos TEST-WORM-2) 0))
(check-expect (change-dir TEST-WORM-2 "right") 
              (make-worm (worm-pos TEST-WORM-2) 1))
(check-expect (change-dir TEST-WORM-2 "down") 
              (make-worm (worm-pos TEST-WORM-2) 2))
(check-expect (change-dir TEST-WORM-2 "left") 
              (make-worm (worm-pos TEST-WORM-2) 3))
(check-expect (change-dir TEST-WORM-2 "a") 
              (make-worm (worm-pos TEST-WORM-2) 1))
(define (change-dir s cmd)
  (cond  
    [(key=? cmd "up") (make-worm (worm-pos s) 0)]
    [(key=? cmd "right") (make-worm (worm-pos s) 1)]
    [(key=? cmd "down") (make-worm (worm-pos s) 2)]
    [(key=? cmd "left") (make-worm (worm-pos s) 3)]
    [else s]))

; worm Command -> worm
; change the direction of the worm based on the command
(define (change-dir* gs cmd)   
    (cond   
      [(empty? (rest gs)) (cons (change-dir (first gs) cmd) empty)]
      [(cons? gs) (cons (change-dir (first gs) cmd) (change-dir* (rest gs) cmd))]))

; Game Command -> Game
; change the direction of the worm based on the command
(define (change-dir*2 gs cmd)
  (make-game (change-dir (game-worm gs) cmd ) (change-dir* (game-low gs) cmd)))


; worm -> worm
; determine if the snake has hit the top of the background
(define (hit-top s)
  (<= (posn-y (worm-pos s)) BODY-RADIUS))

; worm -> worm
; determine if the snake has hit the bottom of the background
(define (hit-bottom s)
  (>= (posn-y (worm-pos s)) BACKGROUND-HEIGHT)) 

; worm -> worm
; determine if the snake has hit the left of the background
(define (hit-left s)
  (<= (posn-x (worm-pos s)) BODY-RADIUS)) 

; worm -> worm
; determine if the snake has hit the right of the background
(define (hit-right s)
  (>= (posn-x (worm-pos s)) BACKGROUND-WIDTH)) 

; worm -> worm
; determine if the snake has collided with a wall
(define (detect-collision s)
  (or (hit-top s)
      (hit-bottom s)
      (hit-left s)
      (hit-right s)))

;-------------------
; Display Rendering
;-------------------

; Game Structure -> Scene
; Render the worm on the screen
(define (render-worm s)
  (place-image HEAD
               (posn-x (worm-pos s)) (posn-y (worm-pos s))
               BACKGROUND))


; Game Structure -> Scene
; Render the game on the screen
(define (render-game s)
  (let* ([gws (game-low s)])
    (place-image HEAD 
                 (posn-x (worm-pos (game-worm s))) (posn-y (worm-pos (game-worm s)))
    (cond 
      [(empty? (rest gws)) (place-image BODY
                                        (posn-x (worm-pos (first (game-low s)))) (posn-y (worm-pos (first (game-low s))))
                                        BACKGROUND)]
      [else (place-image BODY  
                         (posn-x (worm-pos (first (game-low s)))) (posn-y (worm-pos (first (game-low s)))) 
                          (render-game (make-game (rest (game-low s)))))]))))
  
  ; Game Structure -> Scene
  ; Render the worm on the screen with an ending message
  (define (render-endgame s)
    (place-image HEAD (posn-x (worm-pos (first s))) (posn-y (worm-pos (first s)))
                 (posn-x (worm-pos s)) (posn-y (worm-pos s))
                 (overlay/align "left" "bottom" (text " worm hit border" 24 "black") 
                                BACKGROUND))) 
  
  ; Create the world
  (big-bang INITIAL-GAME
            (on-tick move-worm* 0.15)
            (on-key change-dir*2)
            (to-draw render-game))
  
  
  
  