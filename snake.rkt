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
(define HEIGHT 19)

; Snake
(define HEAD (circle (/ SCALE 2) "solid" "orange"))
(define BODY (circle (/ SCALE 2) "solid" "red"))

; Graphical Constants
(define BACKGROUND (empty-scene (* SCALE 10) (* HEIGHT 10)))
(define FOOD (circle (/ SCALE 2) "solid" "blue"))


(define TEST-GAME-1 (make-game (make-posn 50 50) 0))
(define TEST-GAME-2 (make-game (make-posn 50 50) 1))
(define TEST-GAME-3 (make-game (make-posn 50 50) 2))
(define TEST-GAME-4 (make-game (make-posn 50 50) 3))

;---------------------------
; Core Function Definitions
;---------------------------

; Game -> Game 
; move the snake based on its direction
(check-expect (move TEST-GAME-1) (make-game (make-posn 50 49) 0))
(check-expect (move TEST-GAME-2) (make-game (make-posn 49 50) 1))
(check-expect (move TEST-GAME-3) (make-game (make-posn 50 51) 2))
(check-expect (move TEST-GAME-4) (make-game (make-posn 51 50) 3))

(define (move gs)
  (let*([pos (game-pos gs)]
        [dir (game-dir gs)]
        [x (posn-x pos)]
        [y (posn-y pos)])
    (cond [(= dir 0) (make-game (make-posn x (- y 1)) dir)]
          [(= dir 1) (make-game (make-posn (- x 1)  y)  dir)]
          [(= dir 2) (make-game (make-posn x (+ y 1)) dir)]
          [(= dir 3) (make-game (make-posn (+ x 1) y)  dir)])))






