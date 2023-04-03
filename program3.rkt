#lang racket
(require data/either)
(require data/monad)

; <-- Stack Related Operations -->
(define (push item lst)
  (append (list item) lst))

(define (pop lst)
  (rest lst))

; <-- General Argument Checking -->
(define (length-check stack)
  (if (< (size-helper stack) 2)
    (failure (format "Argument for operation is empty!\nStack: ~a" stack))
    (success stack)))

(define (div-zero stack op)
  (if (and (zero? (first stack)) (equal? op /))
    (failure (format "Cannot divide by zero!\nStack: ~a" stack))
    (success stack)))

; <-- Calculator Operations -->
(define (perform-op stack op)
  (do [valid-stack <- (length-check stack)]
      [div-check <- (div-zero valid-stack op)]
      (success (push (op (second stack) (first stack)) (pop (pop stack))))))

(define (clr) 
  (success '()))

(define (show stack)
  (display (format "~a\n" stack))
  (success stack))

(define (top stack)
  (if (empty? stack)
    (display (format "~a\n" '()))
    (display (format "~a\n" (first stack))))
  (success stack))

(define (size stack)
  (display (format "~a\n" (size-helper stack)))
  (success stack))

(define (size-helper stack)
  (if (empty? stack)
    0
    (+ 1 (size-helper (pop stack)))))

(define (dup stack)
  (if (empty? stack)
    (failure (format "Stack is empty!\nStack: ~a" stack))
    (success (push (first stack) stack))))

(define (end)
  (failure "Program Complete!\n"))

; <-- Main Program Logic -->
(define (read-input stack)
  (display "> ")
  (define input (regexp-split #px" +" (read-line (current-input-port) 'any)))
  (define converted-input (convert-string input '()))
  (either display read-input (process-operation converted-input stack)))

(define (convert-string input lst)
  (if (empty? input)
    lst
    (if (number? (string->number (first input)))
      (convert-string (rest input) (append lst (list (string->number (first input)))))
      (convert-string (rest input) (append lst (list (first input)))))))

(define (process-operation input stack)
  (if (empty? input)
    (success stack)
    (do
      [result <- (cond
        [(number? (first input)) (success (push (first input) stack))]
        [(equal? (first input) "ADD") (perform-op stack +)]
        [(equal? (first input) "SUB") (perform-op stack -)]
        [(equal? (first input) "MUL") (perform-op stack *)]
        [(equal? (first input) "DIV") (perform-op stack /)]
        [(equal? (first input) "CLR") (clr)]
        [(equal? (first input) "SHOW") (show stack)]
        [(equal? (first input) "TOP") (top stack)]
        [(equal? (first input) "SIZ") (size stack)]
        [(equal? (first input) "DUP") (dup stack)]
        [(equal? (first input) "END") (end)]
        [else (failure (format "Unrecognized operation!\nStack: ~a" stack))])]
      (process-operation (pop input) result))))

(define start-program (read-input '()))
