;;; othello.scm
(use coops srfi-41)
;;; state 'empty 'black 'white #f
(define-values (white black empty outside) (values 0 1 2 3))
(define *position-list* (iota 64))
(define-values (
                A1 B1 C1 D1 E1 F1 G1 H1
                A2 B2 C2 D2 E2 F2 G2 H2
                A3 B3 C3 D3 E3 F3 G3 H3
                A4 B4 C4 D4 E4 F4 G4 H4
                A5 B5 C5 D5 E5 F5 G5 H5
                A6 B6 C6 D6 E6 F6 G6 H6
                A7 B7 C7 D7 E7 F7 G7 H7
                A8 B8 C8 D8 E8 F8 G8 H8)
  (apply values *position-list*))

(define (make-board)
  (make-vector 64 empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-board board)
  (display "  ＡＢＣＤＥＦＧＨ\n")
  (do ([i 0 (add1 i)])
      ((<= 64 i))
    (when (= 0 (modulo i 8)) (printf "~A " (+ 1(quotient i 8))))
    (display-state (board-ref board i))
    (when (= 7 (modulo i 8)) (newline))))

(define (display-state state)
  (display
   (cond [(black? state) "〇"]
         [(white? state) "●"]
         [else "　"])))

(define (black? state)
  (fx= state black))
(define (white? state)
  (fx= state white))
(define (empty? state)
  (fx= state empty))
(define (outside? state)
  (fx= state outside))
(define (flip-color color)
  (if (black? color) white black))

(define (board-copy board)
  (let ([new (make-board)])
    (vector-copy! board new)
    new))

(define (xy->index x y)
  (fx+ y (fx* x 8)))

(define (board-ref board pos)
  (handle-exceptions exn outside
    (vector-ref board pos)))

(set! (setter board-ref)
  (lambda (board pos state)
    (vector-set! board pos state)))

(define (board-set! board pos color)
  (vector-set! board pos color))


(define (board-init! board)
  (board-set! board D4 white)
  (board-set! board E4 black)
  (board-set! board E5 white)
  (board-set! board D5 black))

(define (board-put! board pos color)
  (if (empty? (board-ref board pos))
      (let ([count (+ (board-flip-line! board pos color -9)
                      (board-flip-line! board pos color -8)
                      (board-flip-line! board pos color -7)
                      (board-flip-line! board pos color -1)
                      (board-flip-line! board pos color 1)
                      (board-flip-line! board pos color 7)
                      (board-flip-line! board pos color 8)
                      (board-flip-line! board pos color 9))])
        (if (zero? count) #f
            (begin (board-set! board pos color)
                   count)))
      #f))

;;; board の start に color を置いたときに引っくり返した石の数を返す
(define (board-flip-line! board start color dir)
  (define (inner current-pos count)
    (let ([state (board-ref board current-pos)])
      (cond [(or (empty? state) (outside? state)) 0]
            [(= color state)
             (do ([i (- current-pos dir) (- i dir)])
                 ((= start i) count)
               (board-set! board i color))]
            [else
             (inner (+ dir current-pos) (+ count 1))])))
  (inner (+ start dir) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (start-board-tree board pos)
  (board-copy board))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <game> ()
  ([board: #:initform (make-board)
           #:accessor board-of]
   [turn: #:initform black
          #:accessor turn-of]
   [white: #:initform 0
           #:accessor white-count]
   [black: #:initform 0
           #:accessor black-count]))

(define-method (turn-count (game <game>))
  (if (black? (turn-of game))
      (black-count game)
      (white-count game)))
(define-method (turn-count-set! (game <game>) value)
  (if (black? (turn-of game))
      (set! (black-count game) value)
      (set! (white-count game) value)))
(define-method (turn-change! (game <game>))
  (set! (turn-of game) (flip-color (turn-of game))))

(define-method (game-put! (game <game>) pos)
  (let ([count (board-put! (board-of game) pos (turn-of game))])
    (if count
        (begin (turn-count-set! game (+ count (turn-count game) 1))
               (turn-change! game)
               count)
        #f)))
(define-method (game-init! (game <game>))
  (board-init! (board-of game))
  (set! (black-count game) 2)
  (set! (white-count game) 2))

(define (turn->string turn)
  (if (black? turn) "black" "white"))

(define-method (game-display (game <game>))
  (printf "black: ~A\nwhite: ~A\n" (black-count game) (white-count game))
  (printf "~A turn\n" (turn->string (turn-of game)))
  (display-board (board-of game)))

(define *game* (make <game>))


