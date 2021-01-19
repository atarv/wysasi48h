(load "stdlib.scm")

(define fizz-buzz
    (fold 
        (lambda (acc x) 
            (displayln (cond ((= (mod x 15) 0) "fizzbuzz")
                ((= (mod x 5) 0) "buzz")
                ((= (mod x 3) 0) "fizz")
                (else x))))
        0
        (range 1 100)))
(fizz-buzz)