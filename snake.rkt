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

(define-struct game (pos dir))
; A game is a structure:
; - (make-game posn number)
; interp. (make-game x dir) represents the position of the worm x,
; and the direction the worm is moving, dir

;-------------------
; Physical Contents
;-------------------

(define SCALE 30)
(define HEIGHT 30)
(define BODY-RADIUS (/ SCALE 5))
(define BACKGROUND-HEIGHT (* HEIGHT 10))
(define BACKGROUND-WIDTH (* SCALE 10))
 
; Snake
(define HEAD (circle BODY-RADIUS "solid" "orange"))
(define BODY (circle BODY-RADIUS "solid" "red"))


; Graphical Constants
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))
(define FOOD (circle (/ SCALE 2) "solid" "blue"))


(define TEST-GAME-1 (make-game (make-posn 50 50) 0))
(define TEST-GAME-2 (make-game (make-posn 50 50) 1))
(define TEST-GAME-3 (make-game (make-posn 50 50) 2))
(define TEST-GAME-4 (make-game (make-posn 50 50) 3))
(define INITIAL-POSN (make-posn (* SCALE 5) (* HEIGHT 5)))
(define INITIAL-GAME (make-game INITIAL-POSN 1))

;---------------------------
; Core Function Definitions
;---------------------------

; Game -> Game 
; move the snake based on its direction
(check-expect (move TEST-GAME-1) (make-game (make-posn 50 35) 0))
(check-expect (move TEST-GAME-2) (make-game (make-posn 65 50) 1))
(check-expect (move TEST-GAME-3) (make-game (make-posn 50 65) 2))
(check-expect (move TEST-GAME-4) (make-game (make-posn 35 50) 3))

(define (move gs)
  (let*([pos (game-pos gs)]
        [dir (game-dir gs)]
        [x (posn-x pos)]
        [y (posn-y pos)])
    (cond [(= dir 0) (make-game (make-posn x (- y (/ SCALE 2))) dir)]
          [(= dir 1) (make-game (make-posn (+ x (/ SCALE 2)) y) dir)]
          [(= dir 2) (make-game (make-posn x (+ y (/ SCALE 2))) dir)]
          [(= dir 3) (make-game (make-posn (- x (/ SCALE 2)) y) dir)])))

; Game Command -> Game
; change the direction of the game based on the command
(check-expect (change-dir TEST-GAME-2 "up") 
                          (make-game (game-pos TEST-GAME-2) 0))
(check-expect (change-dir TEST-GAME-2 "right") 
                          (make-game (game-pos TEST-GAME-2) 1))
(check-expect (change-dir TEST-GAME-2 "down") 
                          (make-game (game-pos TEST-GAME-2) 2))
(check-expect (change-dir TEST-GAME-2 "left") 
                          (make-game (game-pos TEST-GAME-2) 3))
(check-expect (change-dir TEST-GAME-2 "a") 
                          (make-game (game-pos TEST-GAME-2) 1))
(define (change-dir s cmd)
  (cond  
    [(key=? cmd "up") (make-game (game-pos s) 0)]
    [(key=? cmd "right") (make-game (game-pos s) 1)]
    [(key=? cmd "down") (make-game (game-pos s) 2)]
    [(key=? cmd "left") (make-game (game-pos s) 3)]
    [else s]))

; Game -> Game
; determine if the snake has hit the top of the background
(define (hit-top s)
  (<= (posn-y (game-pos s)) BODY-RADIUS))

; Game -> Game
; determine if the snake has hit the bottom of the background
(define (hit-bottom s)
  (>= (posn-y (game-pos s)) BACKGROUND-HEIGHT)) 

; Game -> Game
; determine if the snake has hit the left of the background
(define (hit-left s)
  (<= (posn-x (game-pos s)) BODY-RADIUS)) 

; Game -> Game
; determine if the snake has hit the right of the background
(define (hit-right s)
  (>= (posn-x (game-pos s)) BACKGROUND-WIDTH)) 

; Game -> Game
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
(define (render-game s)
  (place-image HEAD
               (posn-x (game-pos s)) (posn-y (game-pos s))
               BACKGROUND))

; Game Structure -> Scene
; Render the worm on the screen with an ending message
(define (render-endgame s)
  (place-image HEAD
               (posn-x (game-pos s)) (posn-y (game-pos s))
               (overlay/align "left" "bottom" (text " worm hit border" 24 "black") 
                                        BACKGROUND))) 

; Create the world
(big-bang INITIAL-GAME
          (on-tick move 0.15)
          (on-key change-dir)
          (to-draw render-game)
          (stop-when detect-collision render-endgame))
               
               

