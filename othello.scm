;;; othello.scm
(use srfi-41 streams-utils (except vector-lib vector-copy!))
;;; state 'empty 'black 'white #f
(define-values (empty white black outside) (values 0 1 2 3))
(define *position-list* (iota 81))
(define-values (
                A1 B1 C1 D1 E1 F1 G1 H1 W
                A2 B2 C2 D2 E2 F2 G2 H2 W
                A3 B3 C3 D3 E3 F3 G3 H3 W
                A4 B4 C4 D4 E4 F4 G4 H4 W
                A5 B5 C5 D5 E5 F5 G5 H5 W
                A6 B6 C6 D6 E6 F6 G6 H6 W
                A7 B7 C7 D7 E7 F7 G7 H7 W
                A8 B8 C8 D8 E8 F8 G8 H8 W
		W  W  W  W  W  W  W  W  W)
  (apply values *position-list*))

(define *position-alist* (map cons '(
				     A1 B1 C1 D1 E1 F1 G1 H1 W
				     A2 B2 C2 D2 E2 F2 G2 H2 W
				     A3 B3 C3 D3 E3 F3 G3 H3 W
				     A4 B4 C4 D4 E4 F4 G4 H4 W
				     A5 B5 C5 D5 E5 F5 G5 H5 W
				     A6 B6 C6 D6 E6 F6 G6 H6 W
				     A7 B7 C7 D7 E7 F7 G7 H7 W
				     A8 B8 C8 D8 E8 F8 G8 H8 W
				     W  W  W  W  W  W  W  W  W)
			      (iota 81)))
(define *wall-pos* (map cdr (filter (compose (cut eq? <> 'W) car) *position-alist*)))
(define *board-pos* (map cdr (filter (compose not (cut eq? <> 'W) car) *position-alist*)))
(define *board-strm* (list->stream *board-pos*))

(define (make-board)
  (let ([board (make-vector 81 empty)])
    (vector-for-each (lambda (i x)
		       (if (memv i *wall-pos*)
			   (vector-set! board i outside)))
		     board)
    board))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-board board)
  (display "  A B C D E F G H\n")
  (do ([i 0 (add1 i)])
      ((<= 72 i))
    (when (= 0 (modulo i 9)) (printf "~A " (+ 1 (quotient i 9))))
    (unless (= 8 (modulo i 9)) (display-state (board-ref board i)))
    (when (= 8 (modulo i 9)) (newline))))

(define (display-state state)
  (display
   (cond [(black? state) "O "]
         [(white? state) "@ "]
         [else " " "+ "])))

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
(define (color->string color)
  (if (black? color) "black" "white"))
(define (board-copy board)
  (let ([new (make-board)])
    (vector-copy! board new)
    new))

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
      (let ([count (+ (board-flip-line! board pos color -10)
                      (board-flip-line! board pos color -9)
                      (board-flip-line! board pos color -8)
                      (board-flip-line! board pos color -1)
                      (board-flip-line! board pos color 1)
                      (board-flip-line! board pos color 8)
                      (board-flip-line! board pos color 9)
                      (board-flip-line! board pos color 10))])
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

(define (board-count board pred?)
  (vector-count (lambda (i x) (pred? x)) board))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 状態
;;; 木の中で、手に応じて葉を選択したい

(define-record gtree
  board					; 盤の状態
  move-number				; 現在の手数
  value					; 盤の評価値
  turn					; 次の手番
  move					; 現在の状態に至った動き
  children				; 未来  
  pass					; pass
  )

(define-record-printer (gtree x out)
  (fprintf out "#<gtree: (turn ~A) (next ~A) ~A>"
	   (gtree-move-number x)
	   (color->string (gtree-turn x))
	   (gtree-board x)))

(define (first-state)
  (make-gtree
   (board-init! (make-board))
   0
   #f
   black
   #f
   #f
   #f))

(define (gtree-children-num gtree)
  (stream-length (gtree-children gtree)))

(define (stream-filter-map predicate strm) 
  (stream-filter identity
		 (stream-map predicate strm)))

(define (start-gtree gtree)
  (define (inner gtree color other move-number)   
    (let ([children (stream-filter-map
                     (lambda (move)
                       (let* ([new-board (board-copy
                                          (gtree-board gtree))]
                              [count (board-put! new-board move color)])
                         (if count
			     ;; 石をmoveに置けた
                             (letrec ([new-gtree (make-gtree
                                                  new-board
                                                  (+ 1 move-number)
                                                  #f
                                                  other
                                                  move
                                                  #f
						  #f)])
                               (delay
				 (inner new-gtree other color (+ 1 move-number))))
			     ;; 置けなかった… (fiter-mapで消される)
			     #f))) *board-strm*)])
      ;; 普通に手があるか、パス2連続
      ;; occupiedで遅くなっている模様
      ;; 遅延リストが評価されてしまうのだ(なんで?)
      ;; stream-filterが問題っぽい
      (if (or (stream-occupied? children) (gtree-pass gtree))
      	  (begin (gtree-children-set! gtree children) gtree)
      	  ;; パス
      	  (begin (gtree-pass-set! gtree #t) (inner gtree other color move-number))
      	  ;; (begin (gtree-children-set! gtree children) gtree)
	  )
      ))
  (inner gtree black white 0))


(define (gtree-travers gtree move)
  (let loop ([children (gtree-children gtree)])
    (cond [(stream-null? children) #f]
	  [(= move (gtree-move (force (stream-car children))))
	   (force (stream-car children))]
	  [else (loop (stream-cdr children))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; game eval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://uguisu.skr.jp/othello/5-1.html

;;; 局面評価用テーブル 
(define *weight-table*
  (vector  30 -12  0 -1 -1  0 -12  30 'W
	   -12 -15 -3 -3 -3 -3 -15 -12 'W
	   0  -3  0 -1 -1  0  -3   0 'W
	   -1  -3 -1 -1 -1 -1  -3  -1 'W
	   -1  -3 -1 -1 -1 -1  -3  -1 'W
	   0  -3  0 -1 -1  0  -3   0 'W
	   -12 -15 -3 -3 -3 -3 -15 -12 'W
	   30 -12  0 -1 -1  0 -12  30 'W
	   'W 'W 'W 'W 'W 'W 'W 'W 'W))

(define (eval-board board table color other)
  (vector-fold (lambda (i score x)
		 (cond [(= color x) (+ score (vector-ref table i))]
		       [(= other x) (- score (vector-ref table i))]
		       [else score]))
	       0
	       board))


(define (eval-simple gtree color)
  (let ([value (gtree-value gtree)])
    (cond [value value]
          [(eval-board (gtree-board gtree)
                       *weight-table* color (flip-color color))
           => (lambda (new-value)
                (gtree-value-set! gtree new-value)
                new-value)]
          [else #f])))



;;; 先手
;; (define (min-max-black gtree)
;;   ())

(define (game gtree)
  (define (inner gtree eval1 eval2)
    (let ([children (gtree-children (force gtree))])
      (if (stream-null? children) (force gtree)
	  (let ([new (stream-maximum (lambda (gtree1 gtree2)
				       (< (eval1 (force gtree1))
					  (eval2 (force gtree2))))
				     children)])
	    (display-board (gtree-board (force new)))
	    (inner new eval2 eval1)))))
  (inner gtree (cut eval-simple <> black) (cut eval-simple <> white)))


(define gtree (start-gtree (first-state)))
(game (start-gtree (first-state)))

(define (next-moves gtree)
  (stream->list
   (stream-map (compose gtree-move force)
	       (gtree-children (force gtree)))))

;; (define (game gtree)
;;   (define (user-turn gtree)
;;     (let ([nexts (next-moves gtree)])
;;       (cond [(null? nexts) (force gtree)]
;; 	    [(gtree-pass (force gtree)) (comp-turn gtree)]
;; 	    [else
;; 	     (display "your move> ")
;; 	     (let loop ([input (read)])
;; 	       (if (member (alist-ref input *position-alist*) nexts)
;; 		   (let ([new (gtree-travers (force gtree) (alist-ref input *position-alist*))])
;; 		     (display-board (gtree-board (force new)))
;; 		     (comp-turn new))
;; 		   (begin (display "your move> ") (loop (read)))))])))
;;   (define (comp-turn gtree)
;;     (let ([children (gtree-children (force gtree))])
;;       (cond [(stream-null? children) (force gtree)]
;; 	    [(gtree-pass (force gtree)) (user-turn gtree)]
;; 	    [else (let ([new (stream-maximum (lambda (gtree1 gtree2)
;; 					       (< (eval-simple (force gtree1) black)
;; 						  (eval-simple (force gtree2) black)))
;; 					     children)])
;; 		    (display-board (gtree-board (force new)))
;; 		    (user-turn new))])))
;;   (comp-turn gtree))

