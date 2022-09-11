#lang racket

(provide char->piece rank->index file->index)

(define char->piece
  (hash #\P '(white pawn)
        #\N '(white knight)
        #\B '(white bishop)
        #\R '(white rook)
        #\Q '(white queen)
        #\K '(white king)
        #\p '(black pawn)
        #\n '(black knight)
        #\b '(black bishop)
        #\r '(black rook)
        #\q '(black queen)
        #\k '(black king)))

(define rank->index (hash #\1 0 #\2 1 #\3 2 #\4 3 #\5 4 #\6 5 #\7 6 #\8 7))

(define file->index (hash #\a 0 #\b 1 #\c 2 #\d 3 #\e 4 #\f 5 #\g 6 #\h 7))
