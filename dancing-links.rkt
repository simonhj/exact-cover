#lang racket

(provide 
 get-from-table
 build-dl
 dlx)

(require racket/generator)

;; Implementation of Knuth's dancing links algorithm for solving the
;; exact cover problem.  
;;
;; Source: http://arxiv.org/pdf/cs/0011047v1.pdf 
;; Author: Simon Holm Jensen <simon@hjensen.net>

(struct data (left right up down column row-idx) #:mutable)
(struct chead data (size name) #:mutable)
(struct dl (h headers))

;; Utility functions tables represented by vectors
(define (get-from-table table x y)
  (vector-ref (vector-ref table y) x))

;; Matrix rep of exact cover -> dancing links datastructure
;; Expected structure #[ #[header strings] #[row1] #[row2] ... #[rowN] ]
(define (build-dl get size-rows size-columns)
  (define (count-1s y)
    (for/sum ([x (in-range size-columns)])
      (let ([val (get x y)])
	(if (number? val) val 0))))
  (define (connect-lr l r)
    (set-data-right! l r)
    (set-data-left! r l))
  (define (connect-du d u)
    (set-data-up! d u)
    (set-data-down! u d))
  (let* ([dummy (chead -1 -2 -3 -4 -5 -6 -7 "dummy")]
	 [h (chead dummy dummy dummy dummy dummy dummy dummy "h")]
	 [headers (for/vector ([x (in-range size-columns)])
		    (chead 0 0 0 0 0 0 0 (get x 0)))]
	 [made (make-hash)])		; table idx cons -> dl object
    (define (walk-up x y)		; will hit column header evetually, no wrap around
      (let/cc return
        (for ([i (in-range 1 (add1 size-rows))])
          (let ([p (cons x (modulo (- y i) size-rows))])
            (if (hash-has-key? made p)
                (return (hash-ref made p))
                0)))
        (raise exn:fail)))
    (define (walk-left x y)
      (let/cc return
        (for ([i (in-range 1 (add1 size-columns))])
          (let ([p (cons (modulo (- x i) size-columns) y)]) ;Possible wrap around
            (if (hash-has-key? made p)
                (return (hash-ref made p))
                0)))
        (raise exn:fail)))
    (define (walk-right x y)
      (let/cc return
        (for ([i (in-range 1 (add1 size-columns))])
          (let ([p (cons (modulo (+ x i) size-columns) y)]) ;Possible wrap around
            (if (hash-has-key? made p)
                (return (hash-ref made p))
                0)))
        (raise exn:fail)))
    ;; Set the left, right and column links in the headers and set
    ;; initial sizes
    (for ([i (in-range size-columns)])
      (let ([curr (vector-ref headers i)])
        (set-data-left! curr (vector-ref headers (modulo (sub1 i) size-columns)))
        (set-data-right! curr (vector-ref headers (modulo (add1 i) size-columns)))
        (set-data-column! curr curr)
        (set-chead-size! curr (count-1s i))
        (hash-set! made (cons i 0) curr)))
    (connect-lr h (vector-ref headers 0))
    (connect-lr (vector-ref headers (- size-columns 1)) h)
    (for ([y (in-range 1 size-rows)])		;Skip the header
      (for ([x (in-range size-columns)])
        (let ([val (get x y)])
          (cond
	   [(equal? val 0) null]
	   [(equal? val 1) (let ([ob (data 0 0 0 0 (hash-ref made (cons x 0)) y)])
			     (hash-set! made (cons x y) ob)
			     (set-data-down! ob (hash-ref made (cons x 0)))
			     (set-data-right! ob ob)
			     (connect-lr (walk-left x y) ob)
			     (connect-du ob (walk-up x y)))]
	   [else (raise exn:fail)])))
      (let ([p (cons (- size-columns 1) y)]) ;Set wrap around pointer.
	(if (hash-has-key? made p)
	    (connect-lr (hash-ref made p) (walk-right (car p) (cdr p)))
	    (connect-lr (walk-left (car p) (cdr p)) (walk-right (car p) (cdr p))))))
    ;; Connect up pointers for column headers
    (for ([i (in-range size-columns)])
      (let ([c (vector-ref headers i)])
        (connect-du c (walk-up i 0))))
    (dl h headers)))

;; Knuths DLX algorithm and helpers

;; Arguments: direction selector S and a dancing links node n
;; Returns: (i <- S[n], S[S[n]] ... while i \neq n) as a sequence
(define link-walker 
  (curry (lambda (selector node)
	   (in-generator (let loop ([curr (selector node)])
			   (when (not (eq? curr node))
			     (yield curr)
			     (loop (selector curr))))))))

(define down (link-walker data-down))
(define up (link-walker data-up))
(define left (link-walker data-left))
(define right (link-walker data-right))

(define (decrement-size col)
  (set-chead-size! col (sub1 (chead-size col))))

(define (increment-size col)
  (set-chead-size! col (add1 (chead-size col))))

(define (cover c)
  (set-data-left! (data-right c) (data-left c))
  (set-data-right! (data-left c) (data-right c))
  (for ([i (down c)])
    (for ([j (right i)])
      (set-data-up! (data-down j) (data-up j))
      (set-data-down! (data-up j) (data-down j))
      (decrement-size (data-column j)))))

(define (uncover c)
  (for* ([i (up c)]
	 [j (left i)])
    (increment-size (data-column j))
    (set-data-up! (data-down j) j)
    (set-data-down! (data-up j) j))
  (set-data-left! (data-right c) c)
  (set-data-right! (data-left c) c))

(define (dlx instance)
  (let ([h (dl-h instance)]
        [headers (dl-headers instance)])
    (define (choose-column)		; Not pretty...
      (let ([min (data-right h)])
        (for ([he (right h)])
          (when (< (chead-size he) (chead-size min))
            (set! min he)))
        min))
    (define (search k O)
      (if (eq? (data-right h) h)
          (yield (map data-row-idx O))
          (let ([c (choose-column)])
            (cover c)
            (for ([r (down c)])
              (for ([j (right r)])
                (cover (data-column j)))
              (search (add1 k) (cons r O))
              (set! c (data-column r))
              (for ([j (left r)])
                (uncover (data-column j))))
            (uncover c))))
    (generator ()
      (search 0 (list)))))

;; Tests

(module* test #f

  (require rackunit)

  (define EC1
    #[ 
      #['A 'B 'C 'D 'E 'F 'G]
      #[0 0 1 0 1 1 0]   ;1
      #[1 0 0 1 0 0 1]   ;2
      #[0 1 1 0 0 1 0]   ;3
      #[1 0 0 1 0 0 0]   ;4
      #[0 1 0 0 0 0 1]   ;5
      #[0 0 0 1 1 0 1]]) ;6

  (define EC2				;Has multiple solutions
    #[ 
      #['A 'B 'C 'D 'E 'F 'G]
      #[1 1 1 1 1 1 1]
      #[0 0 1 0 1 1 0]   ;1
      #[1 0 0 1 0 0 1]   ;2
      #[0 1 1 0 0 1 0]   ;3
      #[1 0 0 1 0 0 0]   ;4
      #[0 1 0 0 0 0 1]   ;5
      #[0 0 0 1 1 0 1]]) ;6

  (define simple
    #[
      #['A 'B]
      #[1   0]
      #[0   1]])

  (test-case "dlx-test"
    (check-equal? (list 1 2) (sort ((dlx (build-dl ((curry get-from-table) simple) 3 2))) <))
    (check-equal? (list 1 4 5)  (sort ((dlx (build-dl ((curry get-from-table) EC1) 7 7))) <))
    (let ([mul (dlx (build-dl ((curry get-from-table) EC2) 8 7))])
      (check-equal? (for/set ([s (in-producer mul (void))])
		      (sort s <)) (set (list 2 5 6) (list 1))))))
