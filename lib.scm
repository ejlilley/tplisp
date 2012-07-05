(define (not x) (if x #f #t))

;(define (map proc l)
;  (if (null? (cdr l))
;      (cons (proc (car l)) '())
;      (cons (proc (car l)) (map proc (cdr l)))))

(define (reverse list)
  (define (reverse-iter list result)
    (if (null? list)
        result
        (reverse-iter (cdr list) (cons (car list) result))))
  (reverse-iter list '()))

(define (map proc l)
  (define (map-iter proc l accum)
    (if (null? l)
        accum
        (map-iter proc (cdr l) (cons (proc (car l)) accum))))
  (reverse (map-iter proc l '())))

(define (square x) (* x x))

(define (append x y) (if (null? x) y (cons (car x) (append (cdr x) y))))

;(define (fold proc base l)
;  (if (null? (cdr l))
;      (proc (car l) base)
;      (proc (car l) (fold proc base (cdr l)))))

(define (fold proc base l)
  (define (fold-iter proc base l accum)
    (if (null? l)
        accum
        (fold-iter proc base (cdr l) (proc (car l) accum))))
  (fold-iter proc base l base))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

;(define (range b e)
;  (if (= b e)
;      (cons e '())
;      (cons b (range (if (< b e)
;                         (inc b)
;                         (dec b))
;                     e))))

(define (range b e)
  (define (range-iter b e accum)
    (if (= b e)
        (cons b accum)
        (range-iter (if (< b e)
                        (inc b)
                        (dec b))
                    e
                    (cons b accum))))
  (reverse (range-iter b e '())))

(define (iota x)
  (if (= x 0) (cons 0 '()) (cons x (iota (dec x)))))

(define (sum proc n)
  (fold + 0 (map proc (range 1 n))))

(define (even x) (= (% x 2) 0))

(define (odd x) (= (% x 2) 1))

(define (equal? s1 s2)
  (cond ((not (and (list? s1) (list? s2))) (eq? s1 s2))
        (else (fold (lambda (x y) (and x y))
                    #t
                     (map eq? s1 s2)))))

(display "Loaded library.")

(newline)
