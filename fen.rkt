#lang racket

(provide fen->board)

(define (fen->board fen)
  (match (string-split fen)
    [(list piece-placement-data active-color castling-availability
           en-passant-target-square halfmove-clock fullmove-number)
     (list (list 'piece-placement-data (map board-row (string-split piece-placement-data "/")))
           (list 'active-color (string-ref active-color 0))
           (list 'castling-availability (castling castling-availability))
           (list 'en-passant-target-square (en-passant en-passant-target-square))
           (list 'halfmove-clock (string->number halfmove-clock))
           (list 'fullmove-number (string->number fullmove-number)))]))

(define (castling availability)
  (if (string=? "-" availability)
      '()
      (let ([castles (string->list availability)])
        (list (list #\w (map char-downcase
                             (filter (curryr member '(#\K #\Q)) castles)))
              (list #\b (filter (curryr member '(#\k #\q)) castles))))))

(define (en-passant target-square)
  (if (string=? "-" target-square)
      '()
      (let ([coord (string->list target-square)])
        (list (- (char->integer (car coord)) 97)
              (- (char->integer (cadr coord)) 49)))))

(define (board-row fen-row)
  (append-map (λ (ch)
                (if (char-numeric? ch)
                    (make-list (- (char->integer ch) 48) #\_)
                    (list ch)))
              (string->list fen-row)))
