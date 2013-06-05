#lang racket

;; A solver for instances of the exact cover problemer. The interface
;; matches the "usual" text-book style.
;; 
;; Author: Simon Holm Jensen <simon@hjensen.net>

(require "dancing-links.rkt")

(struct exact-cover-instance (dl-rep 	;Dancing links representation
			      sets-mapping 	;Subsets of universe 
			      universe-mapping)) ;All possible elements

(define (make-exact-cover sets universe)
  (let* ([row-idx->elements (for/hash ([e universe]
				       [i (in-range (set-count universe))])
			     (values i e))]
	 [row->sets (for/hash ([s sets]
			       [i (in-range (set-count sets))])
		      (values i s))]
	 [getter (lambda (x y)
		   (if (equal? x 0)
		       x		;The header
		       (let* ([set (hash-ref row->sets y)]
			      [elm (hash-ref row-idx->elements x)])
			 (cond
			  [(set-member? set elm) 1]
			  [else 0]))))])
    (displayln row-idx->elements)
    (displayln row->sets)
    (exact-cover-instance (build-dl getter (add1 (set-count sets)) (set-count universe)))))


;; Tests
(let ([A (set 1 4 7)]
      [B (set 1 4)]
      [C (set 4 5 7)]
      [D (set 3 5 6)]
      [E (set 2 3 6 7)]
      [F (set 2 7)]
      [universe (set 1 2 3 4 5 6 7)])
  (make-exact-cover (set A B C D E F) universe))
