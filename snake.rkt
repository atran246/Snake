;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
; Andrew Tran-Snake Game

; Data definitions
; direction is current direction of travel for the worm, and is either
; - 0: up
; - 1: right
; - 2: down
; - 3: left

(define-struct game (posn dir))
; A worm is a structure:
; - (make-worm posn number)
; interp. (make-worm x  dir) represents the position of the worm x,
; and the direction the disk is moving, dir



; Physical Contents
(define SCALE 30)
(define HEIGHT 19)

; Snake
(define HEAD (circle (/ SCALE 2) "solid" "orange"))
(define BODY (circle (/ SCALE 2) "solid" "red"))

; Graphical Constants
(define BACKGROUND (empty-scene (* SCALE 10) (* HEIGHT 10)))
(define FOOD (circle (/ SCALE 2) "solid" "blue"))



; Functions

; Game -> Game 
(define (move gs)
  (let*([pos (game-pos gs)]
        [dir (game-dir gs)]
        [x (posn-x pos)]
        [y (posn-y pos)])
    (cond [(= dir 0) (make-game (make-posn x (- y 1)) dir)]
          [(= dir 1) (make-game (make-posn (- x 1)  y)  dir)]
          [(= dir 2) (make-game (make-posn x (+ y 1)) dir)]
          [(= dir 3) (make-game (make-posn (+ x 1) y)  dir)])))






