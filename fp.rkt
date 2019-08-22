(require eopl)

(define-datatype tree tree?
  [leaf (key number?)]
  [node (key number?) (left-parent tree?) (right-parent tree?)])

(define node-1
  (leaf 1))
(define node-2
  (leaf 2))
(define root
  (node 3 node-1 node-2))

; (tree/map f tr): F X TR -> TR
; returns a new tree by applying each node to tr
(define tree/map
  (lambda (f tr)
    (cases tree tr
      (leaf (key)
            (leaf (f key)))
      (node (key left-parent right-parent)
            (node (f key) (tree/map f left-parent) (tree/map f right-parent))))))

; (tree/reduce f init tr): F X V X TR -> V
; reduces tree of values to a single value
(define tree/reduce
  (lambda (f init tr)
    (cases tree tr
      (leaf (key)
            (f key init))
      (node (key left-parent right-parent)
            (f key (f (tree/reduce f init left-parent) (tree/reduce f init right-parent)))))))

(define treeduce tree/reduce)
(define reduce tree/reduce)

; (tree/filter f tr): F X TR -> TR
; filter part of tree which satisfies f
(define tree/filter
  (lambda (f tr)
    (cases tree tr
      (leaf (key)
            (if (f key) (leaf key) (leaf 0)))
      (node (key left-parent right-parent)
            (if (f key) (node key (tree/filter f left-parent) (tree/filter f right-parent)) (leaf 0))))))

; (tree/path n tr): N X TR -> L
; returns list of lefts, rights showing path to n in tree tr, #f if not found
(define tree/path
  (lambda (n tr)
    (cases tree tr
      (leaf (key)
            (if (= key n) (list) #f))
      (node (key left-parent right-parent)
            (cond
              [(= key n) (list)]
              [(tree/path n left-parent) (cons `left (tree/path n left-parent))]
              [(tree/path n right-parent) (cons `right (tree/path n right-parent))]
              [else #f])))))

(define path tree/path)



; (list/reduce f init lst): F X V X L -> V
; reduces list of values to a single value
(define list/reduce
  (lambda (f init lst)
    (if (null? lst)
        init
        (f (car lst) (list/reduce f init (cdr lst))))))

; (list/append n lst): N X L -> L
; appends a value to end of list
(define list/append
  (lambda (n lst)
    (list/reduce cons (list n) lst)))

; (list/reverse lst): L -> L
; reverses the order of elements in a list
(define list/reverse
  (lambda (lst)
    (list/reduce list/append (list) lst)))

(define reverse list/reverse)



; (pair/add1 p): P -> P
; increments first value of pair only
(define pair/add1
  (lambda (p)
    (cons (add1 (car p)) (cdr p))))

; (list/map f lst): F X L -> L
; applies a function to every element of list
(define list/map
  (lambda (f lst)
    (if (null? lst)
        (list)
        (cons (f (car lst)) (list/map f (cdr lst))))))

; (g el lst): E X L -> L
; increment 1st value of all pairs, and return (el . lst)
(define g
  (lambda (el lst)
    (cons el (list/map pair/add1 lst))))



; (atmost1? lst): L -> B
; return #t if list has atmost 1 element(s).
(define atmost1?
  (lambda (lst)
    (or (null? lst) (null? (cdr lst)))))

; (swap lst): L -> L
; swaps the first two elements of list
(define swap
  (lambda (lst)
    (if (atmost1? lst)
        lst
        (cons (cadr lst) (cons (car lst) (cddr lst))))))

; (swap-by lst f): L X F -> L
; swaps the first 2 elements of list using given function
(define swap-by
  (lambda (lst f)
    (if (or (atmost1? lst) (f (car lst) (cadr lst)))
        lst
        (swap lst))))

; (bubble-once-by lst f): L X F -> L
; runs a single pass of bubble sort on list
(define bubble-once-by
  (lambda (lst f)
    (if (atmost1? lst)
        lst
        (let ([lst (swap-by lst f)])
          (cons (car lst) (bubble-once-by (cdr lst) f))))))

; (bubble-sort-by lst f): L X F -> L
; bubble sorts a list with given predicate f
(define bubble-sort-by
  (lambda (lst f)
    (if (atmost1? lst)
        lst
        (bubble-once-by (cons (car lst) (bubble-sort-by (cdr lst) f)) f))))

; (bubble-sort lst): L -> L
; bubble sorts a list in ascending order
(define bubble-sort
  (lambda (lst)
    (bubble-sort-by lst <=)))
