#lang racket

;; Export statement
(provide (all-defined-out))

;; 1. divisible-by-x?
(define divisible-by-x?
  ;; Check if x % y returns 0
  (lambda (x)
    ;; y is outside lambda call nested in with function call
    (lambda (y)
      (zero? (remainder y x))
      )
    )
  )

;; 2. function-9
(define function-9
  ;; Run on the function in the parameter
  (lambda (func)
    (func 9)
    )
  )

;; 3. my-map
(define my-map
  (lambda (proc lst)
    ;; If lst null, return null
    ;; Else modify head, and recursively rest of lst
    (if (null? lst)
        null
        (cons (proc (car lst)) (my-map proc (cdr lst)))
        )
    )
  )

;; 4. pair-up
(define pair-up
  (lambda (lst1 lst2)
    ;; If lst1 null, make empty list
    ;; Else check if lst 2 is null
    (if (null? lst1)
        '()

        ;; If lst2 null make empty list
        ;; Else, pair heads of lst1 and lst2, and recursively pair rest of lst1 and lst2
        (if (null? lst2)
            '()
            (cons (list (car lst1) (car lst2)) (pair-up (cdr lst1) (cdr lst2)))
            )
        )
    )
  )
        

;; 5. classify
(define classify
  (lambda (test? lst)
    ;; If lst is null, return empty list of lists
    ;; Else, classify items that evaluate true for test in one list
    (if (null? lst)
        '(() ())
        
        ;; Classify items that evaluate false for test in another list (concat to empty list)
        (cons (classify1 test? lst) (cons (classify2 test? lst) '()))
        )
    )
  )

;; 5. classify (helper function)
(define classify1
  (lambda (test? lst)
    ;; If lst is null, return empty list
    ;; Else, check if our test is true
    (if (null? lst)
        '()

        ;; If test is true, put head of lst and recursion of rest of elements into list
        ;; Else, just recurse down the rest of list after head
        (if (test? (car lst))
            (cons (car lst) (classify1 test? (cdr lst)))
            (classify1 test? (cdr lst))
            )
        )
    )
  )

;; 5. classify (helper function)
(define classify2
  ;; If lst is null, return empty list
  ;; Else, check if our test is true
  (lambda (test? lst)
    (if (null? lst)
        '()

        ;; If test is false, put head of lst and recursion of rest of elements into list
        ;; Else, just recurse down the rest of list after head
        (if (test? (car lst))
            (classify2 test? (cdr lst))
            (cons (car lst) (classify2 test? (cdr lst)))
            )
        )
    )
  )
            
;; 6. is-member?
(define is-member?
  (lambda (elem lst)
    ;; If lst is null, return false since not found
    ;; Else, check if elem is member of lst
    (if (null? lst)
        #f

        ;; If head of lst is equal to elem, true
        ;; Else, recurse down rest of list after head
        (if (equal? (car lst) elem)
            #t
            (is-member? elem (cdr lst))
            )
        )
    )
  )

;; 7. my-sorted?
(define my-sorted?
  (lambda (sym lst)
    ;; If lst is null, return true because it's sorted
    ;; Else check if rest of list is null
    (if (null? lst)
        #t

        ;; If rest of list is null, return true because it's sorted
        ;; Else, check if head of list isn't in place, compared to head of rest of list
        (if (null? (cdr lst))
            #t

            ;; If head of list is not in order, return false
            ;; Else, recurse down rest of list after head
            (if (equal? (sym (car lst) (car (cdr lst))) #f)
                #f
                (my-sorted? sym (cdr lst))
                )
            )
        )
    )
  )

;; 8. my-flatten
(define my-flatten
  (lambda (lst)
    ;; If lst is null, return empty list
    ;; Else, flatten elements and pairs/sublists of lst
    (if (null? lst)
        '()

        ;; If lst has a pair, combone recursive calls of head of list and rest of list after head
        ;; Else, make a list out out of lst and return it
        (if (pair? lst)
            (append (my-flatten (car lst)) (my-flatten (cdr lst)))
            (list lst)
            )
        )
    )
  )
        
;; 9. upper-threshold
(define upper-threshold
  (lambda (lst elem)
    ;; If lst is null, return empty list
    ;; Else, evaluate less than
    (if (null? lst)
        '()

        ;; If head < elem, put head of lst and recursion of rest of elements into list
        ;; Else, just recurse down rest of list after head
        (if (< (car lst) elem)
            (cons (car lst) (upper-threshold (cdr lst) elem))
            (upper-threshold (cdr lst) elem)
           )
        )
    )
  )
                  
;; 10. my-list-ref
(define my-list-ref
  (lambda (lst ind)
    ;; If lst is null, return error
    ;; Else, evaluate function
    (if (null? lst)
        (error "ERROR: Index out of bounds") 

        ;; If ind is 0, return head of lst
        ;; Else, decrement ind and recurse down rest of lst after head
        (if (equal? 0 ind)
            (car lst)
            (my-list-ref (cdr lst) (- ind 1))
            )
        )
    )
  )

;; 11. deep-reverse
(define deep-reverse
  (lambda (lst)
    ;; If lst is null, return empty list
    ;; Else, see if list size is 1
    (if (null? lst)
        '()

        ;; If list size 1, flatten and return
        
    )
  )

#|
- Create an empty list
- If length of list is 1, return inputs
- Else, starting from last element to first, call itself using that element, and add it to the empty list
- Return the list
|#