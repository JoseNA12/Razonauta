#lang scheme


(define Deduccion
    (class object%
        ;(init-field expresion)
        (field
            (premisa null) (conclusion null) ; ej. (p->q) => ((~q)->(~p))
            (formula null)
            (regla null)
        )

        (define/public (set-premisa pPremisa)
            (set! premisa pPremisa)            
        )

        (define/public (get-premisa) premisa)

        (define/public (set-conclusion pConclusion)
            (set! conclusion pConclusion)            
        )

        (define/public (get-conclusion) conclusion)

        (define/public (set-formula pFormula)
            (set! formula pFormula)
        )

        (define/public (set-regla pRegla)
            (set! regla pRegla)
        )

        (super-new)
    )
)


(define badtree0 '(0 () (1 (2 () () ) ()  ) ))
(define badtree1 "fnord")
(define badtree2 '("fnord"))
(define badtree3 '("cat" ("bird" () ()) ("dog" () ("snail" ("parrot" () ())))))
(define badtree4 '("F" ("B" ("A" ()) ("D" ("C" () () ) ("E" () ()) ) ) ("G" () ("I" ("H" () () ) () ))))
(define btree1 '("cat" () ("dog" ("bird" () () ) ()  ) ))
(define bstree0 '())
(define bstree1 '("calcium" () ()))
(define bstree3 '("calcium" ("barium" () () ) ("deuterium" () ())))
(define bstree4 '("calcium" ("barium" () () ) ("deuterium" () ("selenium" () () ))))
(define bstree5 '("calcium" ("barium" () ()) ("deuterium" () ("selenium" ("plumbium" () ()) () ))))
(define bstree6 '("Mars" ("Earth" () ("Jupiter" () ())) ("Venus" ("Saturn" ("Pluto" () ()) ()) ())))
(define bstree7 '("Mars" ("Earth" () ("Jupiter" () ())) ("Venus" ("Saturn" ("Pluto" () ()) ("Uranus" () ())) ())))
(define bstree9 '("F" ("B" ("A" () ()) ("D" ("C" () () ) ("E" () ()) ) ) ("G" () ("I" ("H" () () ) () ))))

; instructor test cases
(define tree1 '("red" () () ))
(define tree2 '( "blue"  ("azure" () () )   ("green"  ("cyan" () () )  ("lemon" () () ) ) ))
(define tree3 '("g" ( "b" () () ) ("k" ()  ("r" ("m" () ("n" () () ) ) ("t" () ("v" () () ))))))
(define z1 '( "orange"  ()  ( "apple" () () ) ))
(define z2 '( "cat"  ()  ( "dog") ))
(define z3 '(  a ( b () () ) () ))
(define z4 '("g" ( "b" () () ) ("z" ()  ("r" ("m" () ("n" () () ) ) ("t" () ("v" () () ))))))
(define z5 1234)

; no-arg constructor -- make an empty tree
(define makeEmpty
  (lambda ()
    '()))

; constructor -- takes root and subtree args
(define makeTree
  (lambda (value leftSub rightSub)
    (list value leftSub rightSub)))

; helper functions for simpler tree navigation
(define getRoot
  (lambda (t)
    (car t)))

(define getLeft
  (lambda (t)
    (car (cdr t))))

(define getRight
  (lambda (t)
    (car (cdr (cdr t)))))

; check if node is a leaf
(define leaf?
  (lambda (t)
    (cond
      ((empty? t) #f)
      (else (and (empty? (getLeft t)) (empty? (getRight t)))))))
                  
; check for empty tree
(define empty?
  (lambda (t)
    (null? t)))

; insert passed-in value into tree
(define insert
  (lambda (t v)
    (cond
      ; if no tree, make tree and insert root node
      ((empty? t)
       (makeTree v (makeEmpty) (makeEmpty)))
      ; if value is less than root, copy tree and add node to left subtree
      ((string<? v (getRoot t))
       (makeTree (getRoot t)
                 (insert (getLeft t) v)
                 (getRight t)))
      ; if value is more than root, copy tree and add node to right subtree
      ((string>? v (getRoot t))
       (makeTree (getRoot t)
                 (getLeft t)
                 (insert (getRight t) v)))
      ; else we have a duplicate value, don't insert it
      (else t))))

; return height of tree -- empty tree has height 0
(define height
  (lambda (t)
    (cond
      ((empty? t) 0)
      (else (+ 1 (max (height (getLeft t))
                      (height (getRight t))))))))

; search tree for passed-in value
(define contains    
  (lambda (t v)
    (cond
      ((empty? t) #f)
      ((equal? v (getRoot t)) #t)
      ((string<? v (getRoot t)) (contains (getLeft t) v))
      (else (contains (getRight t) v)))))

; print inorder tree's values
(define inorderprint
  (lambda (t)
    ; get list of inorder tree values and call helper print function
    (let ([lst (inorderList t)])
      (printList lst))))

; helper print function prints passed-in list (comma-delimited)
(define printList
  (lambda (lst)
    (cond
      ((null? lst) (display ""))
      (else (#|display|# print (car lst)) ; print with/without quotes
            (cond
              ((null? (cdr lst)) (display ""))
              (else (display ", ")))
            (printList (cdr lst))))))

; return inorder list of tree's values
(define inorderList
  (lambda (t)
    (cond
      ; if empty tree, return empty list
      ((empty? t) '())
      (else (append
             (inorderList (getLeft t))
             (list (getRoot t))
             (inorderList (getRight t)))))))

; return preorder list of tree's values
(define preorderList
  (lambda (t)
    (cond
      ; if empty tree, return null list
      ((empty? t) '())
      (else (append
             (list (getRoot t))
             (preorderList (getLeft t))
             (preorderList (getRight t)))))))

; return smallest value
(define smallest
  (lambda (t)
    (cond
      ; if empty tree, return empty list
      ((empty? t) '())
      ((not (null? (getLeft t))) (smallest (getLeft t)))
      (else car(getRoot t)))))

; return largest value
(define largest
  (lambda (t)
    (cond
      ; if empty tree, return empty list
      ((empty? t) '())
      ((not (null? (getRight t))) (largest (getRight t)))
      (else car(getRoot t)))))

; check if passed-in object is a binary tree
(define binarytree?
  (lambda (t)
    (cond
      ; empty tree is okay
      ((empty? t) #t)
      ; throw false if not a list
      ((not (list? t)) #f)
      ; throw false if not a list with length 3
      ((not (= (length t) 3)) #f)
      ; throw false if node value is not a string
      ((not (string? (car t))) #f)
      ; if we've gotten this far, recurse through left and right subtrees
      (else (and (binarytree? (getLeft t)) (binarytree? (getRight t)))))))

; check if passed-in object is a binary search tree
(define bst?
  (lambda (t)
    (cond
      ; if it's not a binary tree, it can't be a binary search tree
      ((not (binarytree? t)) #f)
      ; if we've gotten this far, check if the inorder list is sorted
      (else (sorted? (inorderList t))))))
        
; recursive helper function to check if list is sorted
(define sorted?
  (lambda (lst)
    (cond
      ((or (null? lst) (= (length lst) 1)) #t)
      ((string<=? (car lst) (car(cdr lst))) (sorted? (cdr lst)))
      (else #f))))