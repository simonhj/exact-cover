#lang racket

;; A solver for instances of the exact cover and derived problems. The
;; interface matches the "usual" text-book style.
;; 
;; Author: Simon Holm Jensen <simon@hjensen.net>

(require racket/generator)
(require "dancing-links.rkt")

(provide 
 make-exact-cover
 solve-exact-cover
 make-hitting-set
 solve-hitting-set)

(struct exact-cover-instance (dl-rep 	;Dancing links representation
			      sets-mapping 	;Subsets of universe 
			      universe-mapping)) ;All possible elements

(struct hitting-set (exact-cover-rep
		     row-mapping))

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
    (exact-cover-instance (build-dl getter (add1 (set-count sets)) (set-count universe)) 
			  row->sets row-idx->elements)))

(define (solve-exact-cover problem)	; TODO: Rewrite to use match
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
    
(define (make-hitting-set sets universe)
  (let ([subsets-hash (for/hash ([e universe])
			(values (for/set ([s sets] #:when (set-member? s e))
				    s) e))])
    (let* ([subsets (hash-keys subsets-hash)]
	   [universe (foldl set-union (set) subsets)])
      (hitting-set (make-exact-cover (list->set subsets) universe) subsets-hash))))

(define (solve-hitting-set problem)
  (match problem
    [(struct hitting-set (ex rm)) (let ([ec-sol (solve-exact-cover ex)])
				    (generator () (let loop ([val (ec-sol)])
						    (cond 
						     [(void? val) (void)]
						     [else (begin
							     (yield (list->set (set-map val (lambda (v)
											      (hash-ref rm v)))))
							     (loop (ec-sol)))]))))]))
		  
(module+ test

  (require rackunit)

  ;; From the wikipedia example
  (define A (set 1 4 7))
  (define B (set 1 4))
  (define C (set 4 5 7))
  (define D (set 3 5 6))
  (define E (set 2 3 6 7))
  (define F (set 2 7))
  (define U (set 1 2 3 4 5 6 7))
  
  
  (check-equal? (set B D F) ((solve-exact-cover (make-exact-cover (set A B C D E F) U))))
  (check-equal? (set 1 2 5) ((solve-hitting-set (make-hitting-set (set A B C D E F) U)))))
