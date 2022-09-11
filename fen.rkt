#lang racket

(require "util.rkt")

(provide fen->board)

(define (fen->board fen)
  (match (string-split fen)
    [(list piece-placement-data active-color castling-availability
           en-passant-target-square halfmove-clock fullmove-number)
     (hash 'piece-placement-data (map board-row (reverse (string-split piece-placement-data "/")))
           'active-color (hash-ref color->symbol active-color)
           'castling-availability (castling castling-availability)
           'en-passant-target-square (en-passant en-passant-target-square)
           'halfmove-clock (string->number halfmove-clock)
           'fullmove-number (string->number fullmove-number))]))

(define (castling availability)
  (if (string=? "-" availability)
      '()
      (let*-values ([(castles) (map (curry hash-ref char->piece) (string->list availability))]
                    [(whites blacks) (partition (λ (piece) (eq? 'white (car piece))) castles)])
        (hash 'white (map cadr whites) 'black (map cadr blacks)))))

(define (en-passant target-square)
  (if (string=? "-" target-square)
      '()
      (let ([square (string->list target-square)])
        (list (hash-ref file->index (car square))
              (hash-ref rank->index (cadr square))))))

(define (board-row fen-row)
  (append-map (λ (ch)
                (if (hash-has-key? char->number ch)
                    (make-list (hash-ref char->number ch) 'none)
                    (list (hash-ref char->piece ch))))
              (string->list fen-row)))

(define color->symbol (hash "w" 'white "b" 'black))

(define char->number (hash #\1 1 #\2 2 #\3 3 #\4 4 #\5 5 #\6 6 #\7 7 #\8 8))
