#lang racket/base

(require
  (only-in racket/function curry)
  (only-in racket/match match match-lambda)
  threading
  "util.rkt")

(define (an->move an)
  (define (string?->char str) (and str (string->char str)))
  (match an
    [(regexp kingside-castle) 'kingside-castle]
    [(regexp queenside-castle) 'queenside-castle]
    [(regexp promotion (cons _ (app (curry map string?->char)
                                    (list from-file from-rank to-file to-rank promotion))))
     (~>> (list (~>> (or promotion #\Q) (hash-ref char->piece) cdr)
                (cons from-file from-rank)
                (cons to-file to-rank))
          (map cons '(promotion from to))
          make-immutable-hash)]
    [(regexp standard-move (cons _ (app (curry map string?->char)
                                        (list piece from-file from-rank to-file to-rank))))
     (~>> (list (~>> (or piece #\P) (hash-ref char->piece) cdr)
                (cons from-file from-rank)
                (cons to-file to-rank))
          (map cons '(piece from to))
          make-immutable-hash)]))

(define kingside-castle #rx"^0-0[+#]?$")

(define queenside-castle #rx"^0-0-0[+#]?$")

(define promotion #rx"^([a-h])?([1-8])?x?([a-h])([18])[=/]?\\(?([NBRQ])?\\)?[+#]?$")

(define standard-move #rx"^([NBRQK])?([a-h])?([1-8])?x?([a-h])([1-8])[+#]?$")

(module+ test
  (require rackunit)

  (check-equal? (an->move "0-0") 'kingside-castle)
  (check-equal? (an->move "0-0-0") 'queenside-castle)

  (test-case
   "Promotion"
   (check-equal? (an->move "e8") (hash 'promotion 'queen 'from '(#f . #f) 'to '(#\e . #\8)))
   (check-equal? (an->move "d1") (hash 'promotion 'queen 'from '(#f . #f) 'to '(#\d . #\1)))
   (check-equal? (an->move "e7xd8=R") (hash 'promotion 'rook 'from '(#\e . #\7) 'to '(#\d . #\8)))
   (check-equal? (an->move "fe8/N") (hash 'promotion 'knight 'from '(#\f . #f) 'to '(#\e . #\8))))

  (test-case
   "Standard Move"
   (check-equal? (an->move "e4") (hash 'piece 'pawn 'from '(#f . #f) 'to '(#\e . #\4)))
   (check-equal? (an->move "Nxc6") (hash 'piece 'knight 'from '(#f . #f) 'to '(#\c . #\6)))
   (check-equal? (an->move "Rdf8") (hash 'piece 'rook 'from '(#\d . #f) 'to '(#\f . #\8)))))
