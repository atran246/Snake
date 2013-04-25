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

(define-struct food (x y))
;A food is a structure:
; -(make-food (x y))
; interp. (make-food x y)) represents the x position x and y position of the food y

(define-struct game (worm low food))
; A game is a structure:
; - (make-game ( worm list-of-worms food))
; interp. (make-game (low)) represents a head and represents a list of 
; worm segments that make up the tail and the food positions

;-------------------
; Physical Contents
;-------------------

(define SCALE 30)
(define HEIGHT 30)
(define BODY-RADIUS (/ SCALE 2))
(define BACKGROUND-HEIGHT (* HEIGHT 20))
(define BACKGROUND-WIDTH (* SCALE 20))
(define MAX 20)

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
(define INITIAL-POSN (make-posn (* SCALE 8) (* HEIGHT 5)))
(define INITIAL-FOOD (make-food (* SCALE (random MAX)) 
                                  (* HEIGHT(random MAX))))
(define INITIAL-WORM (make-worm INITIAL-POSN 1))
(define INITIAL-WORM2 (make-worm (make-posn (* SCALE 7) (* HEIGHT 5))  1))
(define INITIAL-WORM3 (make-worm (make-posn (* SCALE 6) (* HEIGHT 5))  1))
(define INITIAL-WORM4 (make-worm (make-posn (* SCALE 5) (* HEIGHT 5))  1))
(define INITIAL-WORM5 (make-worm (make-posn (* SCALE 4) (* HEIGHT 5))  1))
(define INITIAL-WORM6 (make-worm (make-posn (* SCALE 3) (* HEIGHT 5))  1))
(define INITIAL-WORM7 (make-worm (make-posn (* SCALE 2) (* HEIGHT 5))  1))
(define INITIAL-WORM8 (make-worm (make-posn (* SCALE 1) (* HEIGHT 5))  1))
(define INITIAL-GAME (make-game INITIAL-WORM 
                                 (cons INITIAL-WORM2 
                                       (cons INITIAL-WORM3 empty))
                                 INITIAL-FOOD))


;---------------------------
; Core Function Definitions
;---------------------------

; Game -> Game 
; move the snake based on its direction
(check-expect (move TEST-WORM-1) (make-worm (make-posn 50 20) 0))
(check-expect (move TEST-WORM-2) (make-worm (make-posn 80 50) 1))
(check-expect (move TEST-WORM-3) (make-worm (make-posn 50 80) 2))
(check-expect (move TEST-WORM-4) (make-worm (make-posn 20 50) 3))
(define (move gs)
  (let*([pos (worm-pos gs)]
        [dir (worm-dir gs)]
        [x (posn-x pos)]
        [y (posn-y pos)])
    (cond [(= dir 0) (make-worm (make-posn x (- y SCALE )) dir)]
          [(= dir 1) (make-worm (make-posn (+ x  SCALE ) y) dir)]
          [(= dir 2) (make-worm (make-posn x (+ y  SCALE )) dir)]
          [(= dir 3) (make-worm (make-posn (- x SCALE ) y) dir)])))


; Game -> Game 
; move the snake based on its direction
(define (move-worm low)   
  (cond   
    [(empty? (rest low)) empty]
    [(cons? low) (cons (first low) (move-worm (rest low)))]))

; Game -> Game 
; move the snake based on its direction
(define (move-worm* gs)
  (make-game (move (game-worm gs)) (cons (game-worm gs)
                                         (move-worm (game-low gs)))
             (game-food gs)))

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


; Game Command -> Game
; change the direction of the worm based on the command
(define (change-dir* gs cmd)
  (make-game (change-dir (game-worm gs) cmd) (game-low gs) (game-food gs))) 


; Game Command -> Game
; change the direction of the worm based on the command
(define (change-worm-dir low)
  (cond 
    [(empty? (rest low)) empty]
    [(cons? (rest low)) (cons  (first (rest low)) (change-worm-dir (rest low)))]))

; Game Command -> Game
; change the direction of the worm based on the command 
(define (change-worm-dir* gs)
  (let* ([low (game-low gs)])
    (make-game (game-worm gs) 
               (cons (make-worm (worm-pos (first low)) (worm-dir (game-worm gs)))
                     (change-worm-dir low))
               (game-food gs))))

; Game -> Game 
; move the snake 
(define (update-worm gs)
  (move-worm* (change-worm-dir* gs)))


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
  (or (hit-top (game-worm s)) 
      (hit-bottom (game-worm s))
      (hit-left (game-worm s))
      (hit-right (game-worm s))))

; worm low -> worm low
; determine if the worm has hit itself
(define (hit-self? h t)
  (let*(
        [wpos (worm-pos h)]
        [wy (posn-y wpos)] 
        [wx (posn-x wpos)]
        [f (first t)]
        [fpos (worm-pos f)]
        [fy (posn-y fpos)]
        [fx (posn-x fpos)]
        [rt (reverse t)]
        [fr (first rt)]
        [rpos (worm-pos fr)]
        [rx (posn-x rpos)]
        [ry (posn-y rpos)])
    (or (cond
          [(empty? (rest t)) false]
          [(and (= wy fy) (= wx fx)) true]
          [else (hit-self? h (rest t))])
        (and (= wy ry) (= wx rx)))))

; game -> game
; determine if the worm has collided with the wall or itself
(define (collided? g)
  (or (detect-collision g)
      (hit-self? (game-worm g) (game-low g))))


; Posn -> Posn 
; make a new food once the worm has eaten the previous one
(define (food-create p)
  (food-check-create p (make-food (* SCALE (random MAX)) 
                                  (* HEIGHT(random MAX)))))


; Posn Posn -> Posn 
; generative recursion 
; determine if the worm is eating the food the make a new food when it has
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

; Game -> Game
; Update the game state
(define (update-game gs)
  (update-worm (make-game (game-worm gs) 
                          (game-low gs) 
                          (food-create (game-food gs )))))
  

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
  (let* ([gws (game-low s)]
         [gf (game-food s)]
         [fx (food-x gf)]
         [fy (food-y gf)])
    (place-image HEAD 
                 (posn-x (worm-pos (game-worm s))) (posn-y (worm-pos (game-worm s)))
                 (cond 
                   [(empty? (rest gws)) (place-image BODY
                                                     (posn-x (worm-pos (first (game-low s)))) (posn-y (worm-pos (first (game-low s))))
                                                     (place-image FOOD fx fy BACKGROUND))]
                   [else (place-image BODY  
                                      (posn-x (worm-pos (first (game-low s)))) (posn-y (worm-pos (first (game-low s)))) 
                                      (render-game (make-game (game-worm s) (rest (game-low s)) (game-food s))))]))))

; Game Structure -> Scene
; Render the worm on the screen with an ending message
(define (render-endgame s)
  (let* ([gws (game-low s)])
    (place-image HEAD 
                 (posn-x (worm-pos (game-worm s))) (posn-y (worm-pos (game-worm s)))
                 (cond 
                   [(empty? (rest gws)) (place-image BODY
                                                     (posn-x (worm-pos (first (game-low s)))) (posn-y (worm-pos (first (game-low s))))
                                                     (overlay/align "left" "bottom" (text " lol you lost :D" 36 "black") 
                                                                    BACKGROUND))] 
                   [else (place-image BODY  
                                      (posn-x (worm-pos (first (game-low s)))) (posn-y (worm-pos (first (game-low s)))) 
                                      (render-endgame (make-game (game-worm s) (rest (game-low s)))))]))))


; Create the world
(big-bang INITIAL-GAME
          (on-tick update-game 0.15)
          (on-key change-dir*)
          (to-draw render-game)
          (stop-when collided? render-endgame))


