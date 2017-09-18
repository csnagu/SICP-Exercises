(define (square x) (* x x))

(define (exam1-3 a b c)
  (cond ((and (< a b) (< a c)) (+ (square b) (square c)))
        ((and (< b a) (< b c)) (+ (square a) (square c)))
        ((and (< c a) (< c a)) (+ (square a) (square b)))))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))


;; square root
(define (old-sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x) 0.001)))

(define (new-good-enough? guess x)
  (< (abs (/ (- (improve guess x) guess) guess)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; cubic root
(define (cbrt x)
  (cbrt-iter 1.0 x))

(define (cb-good-enough? guess x)
  (< (abs (/ (- (improve-cubic guess x) guess) guess)) 0.001))

(define (cbrt-iter guess x)
  (if (cb-good-enough? guess x)
      guess
      (cbrt-iter (improve-cubic guess x) x)))

(define (improve-cubic guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))


;; factorial
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (exam1-9)
  (define (+ a b)
    (if (= a 0)
        b
        (inc (+ (dec a) b))))
  (define (+ a b)
    (if (= a 0)
        b
        (+ (dec a) (inc b)))))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f n)
  (f-iter 2 1 0 n))
(define (f-iter a b c count)
  (cond ((= count 0) c)
        ((= count 1) b)
        (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))


;; 反復プロセスの練習
;;;
(define (fact-recur n)
  (if (= n 1)
      1
      (* n (fact-recur (- n 1)))))

(define (fact n)
  (iter-fact 1 n))
(define (iter-fact n count)
  (if (= count 1)
      n
      (iter-fact (* n count) (- count 1))))
;;;
(define (hanoi n)
  (if (= n 0)
      0
      (+ (* 2 (hanoi (- n 1))) 1)))

(define (hanoii n)
  (iter-hanoi 0 n))
(define (iter-hanoi ans count)
  (if (= count 0)
      ans
      (iter-hanoi (+ (* 2 ans) 1) (- count 1))))
;; 反復プロセスの練習ここまで ;;

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1)
           angle
           (p (sine (/ angle 3.0))))))


(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))


(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt2 b n)
  (fe-iter b n 1))
(define (fe-iter b n a)
  (cond ((= n 0) 1)
        ((even? n) (fe-iter ()
                            ()
                            (square b)))
        ((else ()))))
