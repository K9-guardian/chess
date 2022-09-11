#lang racket/base

(provide char->number char->piece char->string file->index rank->index string->char)

(define (char->string char) (list->string (list char)))

(define (char->number char) (string->number (char->string char)))

(define (string->char str) (and (= (string-length str) 1) (string-ref str 0)))

(define char->piece
  (hash #\P '(white . pawn)
        #\N '(white . knight)
        #\B '(white . bishop)
        #\R '(white . rook)
        #\Q '(white . queen)
        #\K '(white . king)
        #\p '(black . pawn)
        #\n '(black . knight)
        #\b '(black . bishop)
        #\r '(black . rook)
        #\q '(black . queen)
        #\k '(black . king)))

(define rank->index (hash #\1 0 #\2 1 #\3 2 #\4 3 #\5 4 #\6 5 #\7 6 #\8 7))

(define file->index (hash #\a 0 #\b 1 #\c 2 #\d 3 #\e 4 #\f 5 #\g 6 #\h 7))
