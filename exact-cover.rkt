#lang racket

;; A solver for instances of the exact cover problemer. The interface
;; matches the "usual" text-book style.
;; 
;; Author: Simon Holm Jensen <simon@hjensen.net>

(require racket/generator)
(require rackunit)
(require "dancing-links.rkt")

(struct exact-cover-instance (dl-rep 	;Dancing links representation
			      sets-mapping 	;Subsets of universe 
			      universe-mapping)) ;All possible elements

(define (make-exact-cover sets universe)
  (let* ([row-idx->elements (for/hash ([e universe]
				       [i (in-range (set-count universe))])
			     (values i e))]
	 [row->sets (for/hash ([s sets]
			       [i (in-range 1 (add1 (set-count sets)))])
		      (values i s))]
	 [getter (lambda (x y)
		   (if (equal? y 0)
		       x		;The header
		       (let* ([set (hash-ref row->sets y)]
			      [elm (hash-ref row-idx->elements x)])
			 (cond
			  [(set-member? set elm) 1]
			  [else 0]))))])
    (exact-cover-instance (build-dl getter (add1 (set-count sets)) (set-count universe)) row->sets row-idx->elements)))

(define (solve-exact-cover problem)
  (let ([dl (dlx (exact-cover-instance-dl-rep problem))]
	[sets (exact-cover-instance-sets-mapping problem)]
	[universe (exact-cover-instance-universe-mapping problem)])
    (generator ()
      (let loop ([val (dl)])
	(cond
	 [(void? val) (void)]
	 [else (begin
		 (yield (list->set (map (lambda (idx)
			       (hash-ref sets idx)) val)))
		 (loop (dl)))])))))
    
(module+ test
  
  (let* ([A (set 1 4 7)]
	 [B (set 1 4)]
	 [C (set 4 5 7)]
	 [D (set 3 5 6)]
	 [E (set 2 3 6 7)]
	 [F (set 2 7)]
	 [universe (set 1 2 3 4 5 6 7)]
	 [ins (make-exact-cover (set A B C D E F) universe)]
	 [sol (solve-exact-cover ins)])
    (check-equal? (set B D F) (sol))))
      
