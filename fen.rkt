#lang racket/base

(require
  (only-in racket/function curry thunk)
  (only-in racket/list append-map make-list)
  (only-in racket/match match)
  (only-in racket/string string-split)
  "util.rkt")

(provide fen->board)

(define (fen->board fen)
  (match (string-split fen)
    [(list piece-placement-data active-color castling-availability
           en-passant-target-square halfmove-clock fullmove-number)
     (hash 'piece-placement-data (map board-row (string-split piece-placement-data "/"))
           'active-color (hash-ref color->symbol active-color)
           'castling-availability (castling castling-availability)
           'en-passant-target-square (en-passant en-passant-target-square)
           'halfmove-clock (string->number halfmove-clock)
           'fullmove-number (string->number fullmove-number))]))

(define (castling availability)
  (if (string=? "-" availability)
      '()
      (map (curry hash-ref char->castle) (string->list availability))))

(define (en-passant target-square)
  (if (string=? "-" target-square)
      '()
      (apply cons (string->list target-square))))

(define (board-row fen-row)
  (append-map (λ (char)
                (let ([str (char->string char)])
                  (if (regexp-match #rx"[1-8]" str)
                      (make-list (string->number str) 'empty)
                      (list (hash-ref char->piece char)))))
              (string->list fen-row)))

(define color->symbol (hash "w" 'white "b" 'black))

(define char->castle
  (hash #\K '(white . kingside-castle)
        #\Q '(white . queenside-castle)
        #\k '(black . kingside-castle)
        #\q '(black . queenside-castle)))

(module+ test
  (require rackunit)

  (test-case
   "Row Parsing"
   (check-equal? (board-row "rnbqkbnr")
                 '((black . rook)
                   (black . knight)
                   (black . bishop)
                   (black . queen)
                   (black . king)
                   (black . bishop)
                   (black . knight)
                   (black . rook)))
   (check-equal? (board-row "8") '(empty empty empty empty empty empty empty empty))
   (check-equal? (board-row "4P3") '(empty empty empty empty (white . pawn) empty empty empty))))
