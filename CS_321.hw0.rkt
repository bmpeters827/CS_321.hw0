#lang plai
(define eight-principles
  (list "Know your rights."
        "Acknowledge your sources."
        "Protect your work."
        "Avoid suspicion."
        "Do your own work."
        "Never falsify a record or permit another person to do so."
        "Never fabricate data, citations, or experimental results."
        "Always tell the truth when discussing your work with your instructor."))

(define-type Tree
  [positive-leaf (val natural?)]
  [negative-leaf (val natural?)]
  [interior-node (left Tree?) (right Tree?)])

#|
Implement a contains? function that takes a:
Tree as its first argument,
and an integer (positive or negative) as its second,
and that returns true if the given integer is present anywhere in the tree. It should return false otherwise
|#

(define (contains? tree num)
  (type-case Tree tree
    [positive-leaf (pos) (equal? pos num)]
    [negative-leaf (neg) (equal? (- 0 neg) num)]
    [interior-node (lhs rhs) (if (or (contains? lhs num) (contains? rhs num)) true false)]))

(test (contains? (interior-node (positive-leaf 1)
                                (positive-leaf 0))
                 0)
      true)

(test (contains? (interior-node (positive-leaf 0) (positive-leaf 0)) 3)
      false)

(test (contains? (interior-node (positive-leaf 0) (positive-leaf 0)) 3)
      false)

(test (contains? (interior-node (positive-leaf 0) (positive-leaf 0)) -3)
      false)

(test (contains? (interior-node (interior-node (positive-leaf 5)
                                               (negative-leaf 4))
                                (positive-leaf 3))
                 -4)
      true)

(test (contains? (interior-node (interior-node (positive-leaf 5)
                                              (negative-leaf 4))
                               (interior-node (positive-leaf 2)
                                              (interior-node (positive-leaf 0)
                                                             (negative-leaf 5))))
                 0)
      true)

(test (contains? (interior-node (interior-node (positive-leaf 5)
                                              (negative-leaf 4))
                               (interior-node (positive-leaf 2)
                                              (interior-node (positive-leaf 0)
                                                             (negative-leaf 5))))
                 10)
      false)

#|
Implement a smallest function, which:
takes a Tree as argument
and returns the integer (i.e., not the node) that has the smallest (i.e., closest to negative infinity, not closest to zero) value
|#

(define (smallest tree)
  (type-case Tree tree
    [positive-leaf (pos) pos]
    [negative-leaf (neg) (- 0 neg)]
    [interior-node (lhs rhs) (if (>= (smallest lhs) (smallest rhs)) (smallest rhs) (smallest lhs))]))

(test (smallest (interior-node (interior-node (positive-leaf 5)
                                              (negative-leaf 4))
                               (positive-leaf 3)))
      -4)

(test (smallest (interior-node (interior-node (positive-leaf 5)
                                              (negative-leaf 4))
                               (interior-node (positive-leaf 2)
                                              (interior-node (positive-leaf 0)
                                                             (negative-leaf 5)))))
      -5)

#|
We will consider a Tree to be balanced if the sum of the values of its leaves is zero.
Implement a balanced? function, which
takes a Tree as argument
and returns true if the tree is balanced,
and returns false otherwise
|#

(define (balanced? tree)
  (define (balance-tree tree)
    (type-case Tree tree
      [positive-leaf (pos) pos]
      [negative-leaf (neg) (- 0 neg)]
      [interior-node (lhs rhs) (+ (balance-tree lhs) (balance-tree rhs))]))
  (= 0 (balance-tree tree)))

(test (balanced? (interior-node (interior-node (positive-leaf 5)
                                               (negative-leaf 4))
                                (negative-leaf 1)))
      true)

(test (balanced? (interior-node (interior-node (positive-leaf 5)
                                               (negative-leaf 4))
                                (interior-node (positive-leaf 2)
                                               (interior-node (positive-leaf 0)
                                                              (negative-leaf 5)))))
      false)

(test (balanced? (interior-node (interior-node (positive-leaf 5)
                                               (negative-leaf 4))
                                (interior-node (positive-leaf 2)
                                               (interior-node (positive-leaf 0)
                                                              (negative-leaf 3)))))
      true)

#|
We will consider a Tree to be deeply balanced if and only if all the interior nodes in the tree are balanced.
Implement a deep-balanced? function, which
takes a Tree as argument,
and returns true if it is deeply balanced,
and returns false otherwise.
|#

(define (deep-balanced? tree)
  (define (deep-b tree)
    (type-case Tree tree
      [positive-leaf (pos) pos]
      [negative-leaf (neg) (- 0 neg)]
      [interior-node (lhs rhs) (cond
                                 [(equal? false (deep-b lhs)) false]
                                 [(equal? false (deep-b rhs)) false]
                                 [(equal? 0 (+ (deep-b lhs) (deep-b rhs))) 0]
                                 [(number? (+ (deep-b lhs) (deep-b rhs))) false])]))
  (number? (deep-b tree)))

(test (deep-balanced? (interior-node (interior-node (positive-leaf 5)
                                                    (negative-leaf 5))
                                     (negative-leaf 1)))
      false)

(test (deep-balanced? (interior-node (interior-node (positive-leaf 5)
                                                    (negative-leaf 5))
                                     (negative-leaf 0)))
      true)

(test (deep-balanced? (interior-node (interior-node (positive-leaf 5)
                                                    (negative-leaf 4))
                                     (interior-node (positive-leaf 2)
                                                    (interior-node (positive-leaf 0)
                                                                   (negative-leaf 5)))))
      false)

(test (deep-balanced? (interior-node (interior-node (positive-leaf 5)
                                                    (negative-leaf 5))
                                     (interior-node (positive-leaf 0)
                                                    (interior-node (positive-leaf 3)
                                                                   (negative-leaf 3)))))
      true)

#|
Implement a negate function, which
takes a Tree as argument,
and returns a new Tree of the same shape, but where the value of each leaf is negated.
|#

(define (negate tree)
  (type-case Tree tree
    [positive-leaf (pos) (negative-leaf pos)]
    [negative-leaf (neg) (positive-leaf neg)]
    [interior-node (lhs rhs) (interior-node (negate lhs) (negate rhs))]))

(test (negate (interior-node (interior-node (positive-leaf 5)
                                            (negative-leaf 4))
                             (negative-leaf 1)))
      (interior-node (interior-node (negative-leaf 5)
                                    (positive-leaf 4))
                     (positive-leaf 1)))


(test (negate (interior-node (interior-node (positive-leaf 5)
                                            (negative-leaf 4))
                             (interior-node (positive-leaf 2)
                                            (interior-node (positive-leaf 0)
                                                           (negative-leaf 3)))))
      (interior-node (interior-node (negative-leaf 5)
                                    (positive-leaf 4))
                     (interior-node (negative-leaf 2)
                                    (interior-node (negative-leaf 0)
                                                   (positive-leaf 3)))))

#|
Implement an add function, which
takes a Tree as its first argument
and an integer as its second,
and returns a new tree where the given integer is added to the value of all the leaves in the tree input tree.
|#

(define (add tree num)
  (type-case Tree tree
    [positive-leaf (pos) (if (<= 0 (+ pos num)) (positive-leaf (+ pos num)) (negative-leaf (- 0 (+ pos num))))]
    [negative-leaf (neg) (if (>= 0 (+ (- 0 neg) num)) (negative-leaf (- 0 (+ (- 0 neg) num))) (positive-leaf (+ (- 0 neg) num)))]
    [interior-node (lhs rhs) (interior-node (add lhs num) (add rhs num))]))

(test (add (interior-node (interior-node (positive-leaf 5)
                                         (negative-leaf 4))
                          (negative-leaf 1))
           5)
      (interior-node (interior-node (positive-leaf 10)
                                    (positive-leaf 1))
                     (positive-leaf 4)))

(test (add (interior-node (interior-node (positive-leaf 5)
                                         (negative-leaf 4))
                          (interior-node (positive-leaf 2)
                                         (interior-node (positive-leaf 0)
                                                        (negative-leaf 3))))
           5)
      (interior-node (interior-node (positive-leaf 10)
                                    (positive-leaf 1))
                     (interior-node (positive-leaf 7)
                                    (interior-node (positive-leaf 5)
                                                   (positive-leaf 2)))))

(test (add (interior-node (interior-node (positive-leaf 5)
                                         (negative-leaf 4))
                          (negative-leaf 1))
           -7)
      (interior-node (interior-node (negative-leaf 2)
                                    (negative-leaf 11))
                     (negative-leaf 8)))

(test (add (interior-node (interior-node (positive-leaf 5)
                                         (negative-leaf 4))
                          (interior-node (positive-leaf 2)
                                         (interior-node (positive-leaf 0)
                                                        (negative-leaf 3))))
           -10)
      (interior-node (interior-node (negative-leaf 5)
                                    (negative-leaf 14))
                     (interior-node (negative-leaf 8)
                                    (interior-node (negative-leaf 10)
                                                   (negative-leaf 13)))))

#|
Implement a positive-thinking function, which
takes a Tree as argument
and produces a new tree which removes all negative leaves from the original tree.
If the resulting tree would have no nodes, return false.
Hint: the total number of nodes (leaf and interior) in the resulting tree will be less than or equal to the
total number of nodes in the original, minus the number of negative leaves in the original.
|#

(define (tot-neg tree)
    (type-case Tree tree
      [positive-leaf (pos) 0]
      [negative-leaf (neg) 1]
      [interior-node (lhs rhs) (+ (tot-neg lhs) (tot-neg rhs))]))
(define (tot-leafs tree)
  (type-case Tree tree
    [positive-leaf (pos) 1]
    [negative-leaf (neg) 1]
    [interior-node (lhs rhs) (+ (tot-leafs lhs) (tot-leafs rhs))]))

(define (positive-thinking tree)
  (define (fin tree)
    (type-case Tree tree
    [positive-leaf (pos) (positive-leaf pos)]
    [negative-leaf (neg) false]
    [interior-node (lhs rhs) (cond
                                 [(and (Tree? (positive-thinking lhs)) (Tree? (positive-thinking rhs)))
                                  (interior-node (positive-thinking lhs)
                                                 (positive-thinking rhs))]
                                 [(and (Tree? (positive-thinking lhs)) (boolean? (positive-thinking rhs)))
                                  (positive-thinking lhs)]
                                 [(and (boolean? (positive-thinking lhs)) (Tree? (positive-thinking rhs)))
                                  (positive-thinking rhs)]
                                 [(and (boolean? (positive-thinking lhs)) (boolean? (positive-thinking rhs)))
                                  false])]))
  (fin tree))

(test (positive-thinking (interior-node (interior-node (positive-leaf 5)
                                                       (negative-leaf 4))
                                        (negative-leaf 1)))
      (positive-leaf 5))

(test (positive-thinking (interior-node (interior-node (positive-leaf 5)
                                                       (negative-leaf 4))
                                        (interior-node (positive-leaf 2)
                                                       (interior-node (positive-leaf 0)
                                                                      (negative-leaf 3)))))
      (interior-node (positive-leaf 5)
                     (interior-node (positive-leaf 2)
                                    (positive-leaf 0))))

(test (positive-thinking (interior-node (interior-node (negative-leaf 5)
                                                       (negative-leaf 4))
                                        (negative-leaf 1)))
      false)

(test (positive-thinking (interior-node (interior-node (negative-leaf 5)
                                                       (negative-leaf 4))
                                        (interior-node (negative-leaf 2)
                                                       (interior-node (negative-leaf 0)
                                                                      (negative-leaf 3)))))
      false)

#| break here |#

(test (positive-thinking (interior-node (interior-node (positive-leaf 5)
                                                       (negative-leaf 9))
                                        (interior-node (positive-leaf 1)
                                                       (interior-node (negative-leaf 10)
                                                                      (negative-leaf 4)))))
      (interior-node (positive-leaf 5)
                     (positive-leaf 1)))
      

(test (positive-thinking (interior-node (interior-node (interior-node (negative-leaf 5)
                                                                      (negative-leaf 9))
                                                       (interior-node (negative-leaf 1)
                                                                      (interior-node (negative-leaf 10)
                                                                                     (negative-leaf 4))))
                                        (interior-node (negative-leaf 5)
                                                       (negative-leaf 15))))
      false)

(test (positive-thinking (interior-node (interior-node (interior-node (positive-leaf 5)
                                                                      (negative-leaf 9))
                                                       (interior-node (positive-leaf 1)
                                                                      (interior-node (positive-leaf 10)
                                                                                     (negative-leaf 4))))
                                        (interior-node (positive-leaf 5)
                                                       (negative-leaf 15))))
      (interior-node (interior-node (positive-leaf 5)
                                    (interior-node (positive-leaf 1)
                                                   (positive-leaf 10)))
                     (positive-leaf 5)))