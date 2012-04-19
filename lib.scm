(define (map proc l)
  (if (null (cdr l))
      (cons (proc (car l)) '())
      (cons (proc (car l)) (map proc (cdr l)))))

(define (square x) (* x x))

(define (append x y) (if (null x) y (cons (car x) (append (cdr x) y))))

(define (fold proc base l) (if (null (cdr l)) (proc (car l) base) (proc (car l) (fold proc base (cdr l)))))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (range b e)
  (if (= b e)
      (cons e '())
      (cons b (range (if (< b e)
                         (inc b)
                         (dec b))
                     e))))

(define (iota x)
  (if (= x 0) (cons 0 '()) (cons x (iota (dec x)))))

(define (sum proc n)
  (fold + 0 (map proc (range 1 n))))

(define (even x) (= (remainder x 2) 0))

(define (odd x) (= (remainder x 2) 1))

(display "Loaded library.")

(newline)
