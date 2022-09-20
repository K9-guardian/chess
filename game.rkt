#lang racket/base
(require threading)

(define (game-state-moves game-state position) null)

(define (board-find board piece)
  (~>> board
       (apply append)
       (filter (λ~> cdr (equal? piece)))
       (map car)))

(define (game-state-checkmate? game-state) null)
(define (game-state-check? game-state) null)

(module+ test
  (require
    "fen.rkt"
    rackunit)

  (define opening (piece-placement-data->board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")))
