#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 5)
(define MISSILE-SPEED 10)

(define EMPTY-IMG (square 0 "solid" "white"))

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 150 100 3))    


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =================
;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty T0))
;; 
(define (main g)
  (big-bang g                 ; Game
    (on-tick   tock)          ; Game -> Game
    (to-draw   render)        ; Game -> Image
    (stop-when game-over)     ; Game -> Boolean
    (on-key    handle-key)))  ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next state of the game:
;; - update the pos. of the tank, invaders, missiles 
(check-expect (tock G0) (make-game empty empty
                                   (make-tank (/ WIDTH 2) 1)))
(check-expect (tock G2) (make-game (list (make-invader (+ 150 12)
                                                       (+ 100 INVADER-Y-SPEED) 12))
                                   (list (make-missile 150 (- 300 MISSILE-SPEED)))
                                   (make-tank 50 1)))

; (define (tock g) G0) ;stub

(define (tock g)
  (make-game
   (update-invader-pos (game-invaders g) (game-missiles g))
   (update-missile-pos (game-missiles g) (game-invaders g))
   (game-tank g)))

;; ListOfInvader -> ListOfInvader
;; update the pos of every invader in the list
;; if an invader and missile collides remove the invader
(check-expect (update-invader-pos (list I1) empty)
              (list (make-invader (+ 150 12)
                                  (+ 100 INVADER-Y-SPEED) 12)))
(check-expect (update-invader-pos (list I1 I2) empty)
              (list (make-invader (+ 150 12)
                                  (+ 100 INVADER-Y-SPEED) 12)
                    (make-invader (+ 150 -10)
                                  (+ HEIGHT INVADER-Y-SPEED) -10)))
(check-expect (update-invader-pos (list (make-invader WIDTH 30 3)) empty)
              (list (make-invader (- WIDTH 3) (+ 30 INVADER-Y-SPEED) -3))) ;right edge bounce off
(check-expect (update-invader-pos (list (make-invader 0 30 -3)) empty)
              (list (make-invader 3 (+ 30 INVADER-Y-SPEED) 3)))            ;left edge bounce off
(check-expect (update-invader-pos (list (make-invader 10 10 3))
                                  (list (make-missile 10 10))) empty)      ;missile & invad 

; (define (update-invader-pos loi) (list I1)) ;stub

(define (update-invader-pos loi ms)
  (cond [(empty? loi) empty]
        [(missile-invader-collision? (first loi) ms)
         (update-invader-pos (rest loi) ms)]
        [(bounce-off? (first loi))
         (cons (advance-invader (first loi) -1)
               (update-invader-pos (rest loi) ms))]
        [else
         (cons (advance-invader (first loi) 1)
               (update-invader-pos (rest loi) ms))]))

;; Invader -> Boolean
;; produces true if the invader bounces off the edge of the screen
(check-expect (bounce-off? I1) false)
(check-expect (bounce-off? (make-invader 30 30 1)) false)
(check-expect (bounce-off? (make-invader WIDTH 30 1)) true)
(check-expect (bounce-off? (make-invader 0 20 1)) true)

; (define (bounce-off? inv) false) ;stub

(define (bounce-off? inv)
  (or (>= (invader-x inv) WIDTH) (<= (invader-x inv) 0)))

;; Invader Number -> Invader
;; advance invader pos to next state; perform screen bounce off
(check-expect (advance-invader I1 1) (make-invader (+ 150 12)
                                                   (+ 100 INVADER-Y-SPEED) 12))
(check-expect (advance-invader I1 -1) (make-invader (+ 150 -12)
                                                    (+ 100 INVADER-Y-SPEED) -12))

; (define (advance-invader inv n) (make-invader 0 0 0)) ;stub

(define (advance-invader inv n)
  (make-invader
   (+ (invader-x inv)
      (* n (invader-dx inv)))
   (+ (invader-y inv)
      INVADER-Y-SPEED)
   (* n (invader-dx inv))))

;; Invader ListOfMissile -> Boolean
;; produce true if invader collides with a missile
(check-expect (missile-invader-collision? I1 empty) false)
(check-expect (missile-invader-collision? I1 (list M1)) false)
(check-expect (missile-invader-collision? (make-invader 100 200 1)
                                          (list (make-missile 100 (- 200 HIT-RANGE)))) true)
(check-expect (missile-invader-collision? (make-invader 100 200 1)
                                          (list (make-missile 100 (+ 200 HIT-RANGE)))) true)
(check-expect (missile-invader-collision? (make-invader 100 200 1)
                                          (list (make-missile (+ 100 HIT-RANGE) 200))) true)
(check-expect (missile-invader-collision? (make-invader 100 200 1)
                                          (list (make-missile (- 100 HIT-RANGE) 200))) true)
(check-expect (missile-invader-collision? (make-invader 100 200 1)
                                          (list (make-missile 100 (+ 200 (/ HIT-RANGE 2))))) true)
(check-expect (missile-invader-collision? (make-invader 100 200 1)
                                          (list (make-missile 100 (+ 200 (* HIT-RANGE 2))))) false)
(check-expect (missile-invader-collision? (make-invader 100 200 1)
                                          (list (make-missile 100 (- 200 (* HIT-RANGE 2))))) false)
(check-expect (missile-invader-collision? (make-invader 100 200 1)
                                          (list (make-missile (+ 100 (* HIT-RANGE 2)) 200))) false)
(check-expect (missile-invader-collision? (make-invader 100 200 1)
                                          (list (make-missile (- 100 (* HIT-RANGE 2)) 200))) false)

; (define (missile-invader-collision? inv lom) false) ;stub

(define (missile-invader-collision? inv lom)
  (cond [(empty? lom) false]
        [(and (<= (abs (- (invader-x inv) (missile-x (first lom)))) HIT-RANGE)
              (<= (abs (- (invader-y inv) (missile-y (first lom)))) HIT-RANGE))
         true]
        [else
         (missile-invader-collision? inv (rest lom))]))

;; ListOfMissile Invader -> Boolean
;; produce true if missile collised with an invader
;; TESTS are the reverse of tests for missile-invader-collision?

; (define (invader-missile-collision? ms loi) true) ;stub

(define (invader-missile-collision? ms loi)
  (cond [(empty? loi) false]
        [(and (<= (abs (- (missile-x ms) (invader-x (first loi)))) HIT-RANGE)
              (<= (abs (- (missile-y ms) (invader-y (first loi)))) HIT-RANGE))
         true]
        [else
         (invader-missile-collision? ms (rest loi))]))

;; ListOfMissile -> ListOfMissile
;; update the pos of every missile in the list
;; remove missile from list if collides with an invader
(check-expect (update-missile-pos (list M1) empty)
              (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (update-missile-pos (list (make-missile 10 -1)) empty) empty) ;missile is out of map

; (define (update-missile-pos m) (list M1)) ;stub

(define (update-missile-pos ms loi)
  (cond [(empty? ms) empty]
        [(invader-missile-collision? (first ms) loi)
         (update-missile-pos (rest ms) loi)]
        [else
         (if (< (missile-y (first ms)) 0)
             (update-missile-pos (rest ms) loi)
             (cons (make-missile (missile-x (first ms))
                                 (- (missile-y (first ms)) MISSILE-SPEED))
                   (update-missile-pos (rest ms) loi)))]))

;; Game -> Image
;; render the tank, invaders and missiles as an image 
(check-expect (render G0) (place-images
                           (list TANK)
                           (list (make-posn (/ WIDTH 2) (- HEIGHT 10)))
                           BACKGROUND))
(check-expect (render G2) (place-images
                           (list TANK INVADER MISSILE)
                           (list (make-posn 50 (- HEIGHT 10))
                                 (make-posn 150 100)
                                 (make-posn 150 300))
                           BACKGROUND))

;(define (render g) (square 0 "solid" "white")) ;stub

(define (render g)
  (place-images
   (append (img-list (append
                      (game-invaders g)
                      (game-missiles g)))
           (list TANK))
   (append (pos-list (append
                      (game-invaders g)
                      (game-missiles g)))
           (list (make-posn (tank-x (game-tank g)) (- HEIGHT 10))))
   BACKGROUND))

;; ListOfMovingElement -> ListOfImage
;; build a list of invader and missile images
(check-expect (img-list empty) empty)
(check-expect (img-list (list
                         (make-invader 10 10 1)
                         (make-missile 10 10)))
              (list INVADER MISSILE))

; (define (img-list lome) (list INVADER)) ;stub

(define (img-list lome)
  (cond [(empty? lome) empty]
        [else
         (if (invader? (first lome))
             (cons INVADER (img-list (rest lome)))
             (cons MISSILE (img-list (rest lome))))]))

;; ListOfMovingElement -> ListOfImage
;; build a list of posn pairs that hold the positions of invaders and missiles
(check-expect (pos-list (list (make-invader 10 10 1)
                              (make-missile 13 10)))
              (list (make-posn 10 10)
                    (make-posn 13 10)))

; (define (pos-list lome) (list (make-posn 0 0))) ;stub

(define (pos-list lome)
  (cond [(empty? lome) empty]
        [else
         (if (invader? (first lome))
             (cons (make-posn
                    (invader-x (first lome))
                    (invader-y (first lome)))
                   (pos-list (rest lome)))
             (cons (make-posn
                    (missile-x (first lome))
                    (missile-y (first lome)))
                   (pos-list (rest lome))))]))

;; Game KeyEvent -> Game
;; handle key events for the game
;; - SPACE will shoot a missle from the tank towards the invaders
;;   - and also create a new invader from a starting position random on the top of the screen
;; - RARROW will move the tank right
;; - LARROW will move the tank left
;; - do not allow the player to move out of map
;; Note: due to (random ...) it is not possible to test the the 1st condition of the function
(check-expect (handle-key G1 "left") (make-game empty empty (make-tank 45 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 10 -1)) "left")
              (make-game empty empty (make-tank 10 -1))) ;left edge
(check-expect (handle-key (make-game empty empty (make-tank (- WIDTH 10) 1)) "right")
              (make-game empty empty (make-tank (- WIDTH 10) 1))) ;right edge
(check-expect (handle-key G0 "a") G0) ;do not change

; (define (handle-key g key) (make-game empty empty T1)) ;stub

(define (handle-key g key)
  (cond [(key=? key " ")
         (make-game (cons (make-invader (round (+ (random (- WIDTH 20)) 20)) -10 3)
                          (game-invaders g))
                    (cons
                     (make-missile (tank-x (game-tank g)) (- HEIGHT 10))
                     (game-missiles g))
                    (game-tank g))]
        [(key=? key "left")
         (if (<= (tank-x (game-tank g)) 10)
             g
             (make-game (game-invaders g)
                        (game-missiles g)
                        (make-tank (- (tank-x (game-tank g)) TANK-SPEED) -1)))]
        [(key=? key "right")
         (if (>= (tank-x (game-tank g)) (- WIDTH 10))
             g  
             (make-game (game-invaders g)
                        (game-missiles g)
                        (make-tank (+ (tank-x (game-tank g)) TANK-SPEED) 1)))]
        [else g]))

;; Game -> Boolean
;; produces true if game is over
(check-expect (game-over (make-game empty empty T0)) false)
(check-expect (game-over (make-game
                          (list (make-invader 10 HEIGHT 3)) empty T0))
              true) ;invader reached end
(check-expect (game-over (make-game
                          (list (make-invader 10 100 3)) empty T0))
              false) ;invader did not reach end

; (define (game-over g) false) ;stub

(define (game-over g)
  (reached-end? (game-invaders g)))

;; ListOfInvader -> Boolean
;; produces true if any invader reached the end of the map
(check-expect (reached-end? empty) false)
(check-expect (reached-end? (list (make-invader 10 10 3))) false)
(check-expect (reached-end? (list (make-invader 10 HEIGHT 3))) true)

; (define (reached-end? loi) false) ;stub

(define (reached-end? loi)
  (cond [(empty? loi) false]
        [(>= (invader-y (first loi)) HEIGHT) true]
        [else
         (reached-end? (rest loi))]))
