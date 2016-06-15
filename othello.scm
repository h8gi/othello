;;; othello.scm
(use srfi-41 streams-utils (except vector-lib vector-copy!))
;;; state 'empty 'black 'white #f
(define-values (white black empty outside) (values 0 1 2 3))
(define *position-list* (iota 64))
(define *position-stream* (list->stream (iota 64)))
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
         [else "　" "＋"])))

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
  (board-set! board D5 black)
  board)

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
                   (+ count 1)		; add self
		   )))
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
;;; 状態
;;; 木の中で、手に応じて葉を選択したい

(define-record gtree
  board					; 盤の状態
  move-number				; 現在の手数
  value					; 盤の評価値
  turn					; 現在の手番
  move					; 現在の状態に至った動き
  children				; 未来
  )

(define-record-printer (gtree x out)
  (fprintf out "#<gtree: (turn ~A) (next ~A) ~A>"
	   (gtree-move-number x)
	   (turn->string (gtree-turn x))
	   (gtree-board x)))

(define (first-state)
  (make-gtree
   (board-init! (make-board))
   0
   #f
   black
   #f
   #f))

(define (gtree-children-num gtree)
  (stream-length (gtree-children gtree)))

(define (stream-filter-map predicate strm)
  (stream-filter identity
		 (stream-map predicate strm)))

(define (start-gtree)
  (define (inner gtree color other move-number)   
    (gtree-children-set!
     gtree
     (stream-filter-map
      (lambda (move)
	(let* ([new-board (board-copy
			   (gtree-board gtree))]
	       [count (board-put! new-board move color)])
	  (if count
	      (letrec ([new-gtree (make-gtree
				   new-board
				   (+ 1 move-number)
				   #f
				   other
				   move
				   #f)])
		(inner new-gtree other color (+ 1 move-number)))
	      #f)))
      *position-stream*))
    gtree)
  (inner (first-state) black white 0))

(define gtree (start-gtree))

(define (gtree-travers gtree move)
  (let loop ([children (gtree-children gtree)])
    (cond [(stream-null? children) #f]
	  [(= move (gtree-move (stream-car children)))
	   (stream-car children)]
	  [else (loop (stream-cdr children))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; game eval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://uguisu.skr.jp/othello/5-1.html

;;; 局面評価用テーブル 
(define *weight-table*
  (vector  30 -12  0 -1 -1  0 -12  30
	   -12 -15 -3 -3 -3 -3 -15 -12
	   0  -3  0 -1 -1  0  -3   0
	   -1  -3 -1 -1 -1 -1  -3  -1
	   -1  -3 -1 -1 -1 -1  -3  -1
	   0  -3  0 -1 -1  0  -3   0
	   -12 -15 -3 -3 -3 -3 -15 -12
	   30 -12  0 -1 -1  0 -12  30))

(define (eval-board board table color other)
  (vector-fold (lambda (i score x)
		 (cond [(= color x) (+ score (vector-ref table i))]
		       [(= other x) (- score (vector-ref table i))]
		       [else score]))
	       0
	       board))

(define (eval-simple board color)
  (eval-board board *weight-table* color (flip-color color)))

;;; 先手
;; (define (min-max-black gtree)
;;   ())

