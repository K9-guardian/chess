#lang racket

(provide fen->board)

(define (fen->board fen)
  (match (string-split fen)
    [(list piece-placement-data active-color castling-availability
           en-passant-target-square halfmove-clock fullmove-number)
     (list (list 'piece-placement-data (map board-row (reverse (string-split piece-placement-data "/"))))
           (list 'active-color (hash-ref color->symbol active-color))
           (list 'castling-availability (castling castling-availability))
           (list 'en-passant-target-square (en-passant en-passant-target-square))
           (list 'halfmove-clock (string->number halfmove-clock))
           (list 'fullmove-number (string->number fullmove-number)))]))

(define (castling availability)
  (if (string=? "-" availability)
      '()
      (let*-values ([(castles) (map (curry hash-ref char->piece) (string->list availability))]
                    [(whites blacks) (partition (λ (piece) (eq? 'white (car piece))) castles)])
        (map list '(white black) (list (map cadr whites) (map cadr blacks))))))

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

(define char->piece
  (hasheqv #\P '(white pawn)
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

(define char->number (hasheqv #\1 1 #\2 2 #\3 3 #\4 4 #\5 5 #\6 6 #\7 7 #\8 8))

(define rank->index (hasheqv #\1 0 #\2 1 #\3 2 #\4 3 #\5 4 #\6 5 #\7 6 #\8 7))

(define file->index (hasheqv #\a 0 #\b 1 #\c 2 #\d 3 #\e 4 #\f 5 #\g 6 #\h 7))
