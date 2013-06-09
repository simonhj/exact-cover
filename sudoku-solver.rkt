#lang racket

(require "exact-cover.rkt")

(define (constraint v x y)
  (list x y v))

(define (row-column N x y)
  (for/set ([i (in-range 1 (add1 N))])
    (constraint i x y)))

(define (all-row-column N)
  (for*/set ([x (in-range N)]
	     [y (in-range N)]
	     [val (in-range 1 (add1 N))])
    (constraint val x y)))
		
(define (row-number N x val)
  (for/set ([i (in-range N)])
    (constraint val x i)))

(define (all-row-number N)
  (for*/set ([val (in-range 1 (add1 N))]
	     [x (in-range N)])
    (row-number N x val)))

(define (column-number N y val)
  (for/set ([i (in-range N)])
    (constraint val i y)))

(define (all-column-number N)
  (for*/set ([val (in-range 1 (add1 N))]
	     [y (in-range N)])
    (column-number N y val)))

;; Box idx should be upper left point.
(define (box-number N box-x box-y val)
  (let ([p (sqrt N)])
    (for*/set ([dx (in-range p)]
	       [dy (in-range p)])
      (constraint val (+ box-x dx) (+ box-y dy)))))

(define (boxes N)
  (let ([p (sqrt N)])
    (for*/list ([dx (in-range p)]
		[dy (in-range p)])
      (cons (* p dx) (* p dy)))))

(define (all-box-number N)
  (for*/set ([val (in-range 1 (add1 N))]
	     [box (boxes N)])
    (box-number N (car box) (cdr box) val)))

;; Expected format: vector of rows
(define (sudoku->hitting-set s)
  (let* ([N (vector-length s)]
	 [universe (all-row-column N)]
	 [rc-constraints (for*/set ([y (in-range N)]
				    [x (in-range N)])
			   (let ([val (vector-ref (vector-ref s y) x)])
			     (cond 
			      [(number? val) (set (constraint val x y))]
			      [else (row-column N x y)])))]
	 [cs (set-union rc-constraints (all-row-number N) (all-column-number N) (all-box-number N))])
    (make-hitting-set cs universe)))

(define (solve-sudoku hs)
  (let ([sol (solve-hitting-set hs)])
    sol))
	 
(define S1 ; Medium difficulty
  #[
    #[e 9 e e e e e e 8]
    #[2 e e e e e 1 9 e]
    #[4 e e 2 e 1 e e e]
    #[e 3 e e e 6 e 8 7]
    #[e e e 7 e 9 e e e]
    #[7 1 e 8 e e e 3 e]
    #[e e e 5 e 3 e e 6]
    #[e 8 6 e e e e e 1]
    #[5 e e e e e e 7 e]])

(define S2
  #[
    #[3 e 4 e]
    #[e 1 e 2]
    #[e 4 e 3]
    #[2 e 1 e]])
			     
				   
  
  
