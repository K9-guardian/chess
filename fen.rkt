#lang racket/base
(require
  (only-in racket/function curry thunk)
  (only-in racket/list append-map make-list)
  (only-in racket/match match)
  (only-in racket/sequence in-slice sequence->list)
  (only-in racket/string string-split)
  threading
  "util.rkt")

(provide fen->game-state piece-placement-data->board)

(define (fen->game-state fen)
  (match (string-split fen)
    [(list piece-placement-data active-color available-castles
           en-passant-target-square halfmoves fullmoves)
     (hash 'board (piece-placement-data->board piece-placement-data)
           'active-color (hash-ref color->symbol active-color)
           'available-castles (parse-available-castles available-castles)
           'en-passant-target-square (parse-en-passant en-passant-target-square)
           'halfmoves (string->number halfmoves)
           'fullmoves (string->number fullmoves))]))

(define (parse-available-castles availability)
  (if (string=? "-" availability)
      '()
      (map (curry hash-ref char->castle) (string->list availability))))

(define (parse-en-passant target-square)
  (if (string=? "-" target-square)
      '()
      (apply cons (string->list target-square))))

(define (piece-placement-data->board data)
  (define rows (string-split data "/"))
  (define positions
    (for*/list ([rank (in-inclusive-range 8 1 -1)]
                [file (in-list '(a b c d e f g h))])
      (cons file rank)))
  (~>> rows
       (map fen-row->board-row)
       (apply append)
       (map cons positions)
       (in-slice 8)
       sequence->list))

(define (fen-row->board-row fen-row)
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
   (check-equal? (fen-row->board-row "rnbqkbnr")
                 '((black . rook)
                   (black . knight)
                   (black . bishop)
                   (black . queen)
                   (black . king)
                   (black . bishop)
                   (black . knight)
                   (black . rook)))
   (check-equal? (fen-row->board-row "8") '(empty empty empty empty empty empty empty empty))
   (check-equal? (fen-row->board-row "4P3") '(empty empty empty empty (white . pawn) empty empty empty))))
