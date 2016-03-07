#lang racket
;;Daotong Dai 0646436
;;Exercise 1 In this part I defined the binary tree by using the structures
(define-struct binaryTree (root leftSubTree rightSubTree))



(define (checkBinaryTree t);;for the present of expressions by using binary tree,there are four situations for the lefttree and right tree.   
  (cond ((and(number? (binaryTree-leftSubTree t))(number? (binaryTree-rightSubTree t))) 1);;left and right are numbers
        ((and(binaryTree? (binaryTree-leftSubTree t))(number? (binaryTree-rightSubTree t))) 2);;left is another expression(btree) and right is number
        ((and(number? (binaryTree-leftSubTree t))(binaryTree? (binaryTree-rightSubTree t))) 3);;left is number but right is expression
        ((and(binaryTree? (binaryTree-leftSubTree t))(binaryTree? (binaryTree-rightSubTree t))) 4);;left and right are all expressions
))
        

(define (reverseBinaryTree t);;implement the reverse
  (cond ((null? t) t)
        (else (cond ((= (checkBinaryTree t) 1)(make-binaryTree (binaryTree-root t)(binaryTree-rightSubTree t)(binaryTree-leftSubTree t)));;in the first situation, we just need to reverse left and right directly
                    ((= (checkBinaryTree t) 2)(make-binaryTree (binaryTree-root t)(binaryTree-rightSubTree t)(reverseBinaryTree (binaryTree-leftSubTree t))));; in the second situation,because left is another expression, so it need to run reverse recursively, therefore,switch left and right 
                     ((= (checkBinaryTree t) 3)(make-binaryTree (binaryTree-root t)(reverseBinaryTree(binaryTree-rightSubTree t))(binaryTree-leftSubTree t)));;in the third situation, the same as previous, righttree need reverse recursively and switch
                     ((= (checkBinaryTree t) 4) (make-binaryTree (binaryTree-root t)(reverseBinaryTree(binaryTree-rightSubTree t)) (reverseBinaryTree (binaryTree-leftSubTree t)))  ;; this situation left and right both need to run recursively and switch             
  )))))
  

(define (preorderTraverse t);;display a binary tree in preorder, for the expressions is the same as DrRacket
  (define (visitNode x) (display "( ") (display x) (display " "));;visit the root
  (cond ((null? t) display "binary tree is null")
      (else (visitNode (binaryTree-root t));;the same as previous, the display of an expression btree need to consider these four situations
            (cond ((= (checkBinaryTree t) 1) (display "( ")(display (binaryTree-leftSubTree t))(display " )")(display " ")(display "( ")(display (binaryTree-rightSubTree t))(display " )"))
                  ((= (checkBinaryTree t) 2) (preorderTraverse (binaryTree-leftSubTree t))(display "( ")(display (binaryTree-rightSubTree t))(display " )"))
                  ((= (checkBinaryTree t) 3) (display "( ")(display (binaryTree-leftSubTree t))(display " )")(display " ")(preorderTraverse (binaryTree-rightSubTree t)))
                  ((= (checkBinaryTree t) 4) (preorderTraverse (binaryTree-leftSubTree t))(preorderTraverse (binaryTree-rightSubTree t))))))(display " )") )


(define t(make-binaryTree '* (make-binaryTree '+ 2 3)(make-binaryTree '- 7 8)));;t is the example ( * ( + ( 2 ) ( 3 ) )( - ( 7 ) ( 8 ) ) )
(preorderTraverse t);; the original sequence
(displayln "")
(preorderTraverse (reverseBinaryTree t));;the reverse sequence output is ( * ( - ( 8 ) ( 7 ) )( + ( 3 ) ( 2 ) ) )






;;Exercise 2
;;because this task should be devided into different situations, and for the binarytree to represented by list it is easy to operate, so I represented my binarytree data structures by using lists   
(define(binaryTreeLeaf e)(list e));;leaf node
(define (binaryTreeNode e left right)(list left right));; node



;;Here, I use three functions to achieve the streamline,(1)streamlineInner is the logic function in order to shorten the tree which is no more than two levels, (2)streamlineOuter is to deal with sometimes,
;;for example, (3 (3))-(3 3)-(3)which may need to update the tree several times,(3)streamline is the main function, because the update need to be begin from the innermost subtree,and the construction
;;of the new tree also need to begin from the inner level,therefore, streamline is the recursive function that used to streamline the inner tree first and then the outer tree


(define (streamlineInner t);; this is the inner operation, in order to deal with thestreamline for subtrees
  (if (null? t) t
  (cond( (null?(cdr t))(car t));;if the tree only has one number,that means it is the shortest and we do not need to operate
       (else (cond ((equal? (car t)(car(cdr t)))(list (car t)));;for a  tree that left and right are equal,delete the right 
                   ((and(list? (car t))(list? (cadr t))) ;;fot the situation that left is a subtree and right is a subtree can be divided into four situations
                                                          (cond ((and(null?(cdar t))(null?(cdadr t)))(list (caar t)(caadr t)));;if leftsubtree only has one node and the same as right, move them up eg:((3)(5))-(3 5)
                                                               ( (and(not(null?(cdar t)))(null?(cdadr t)))(list (car t)(caadr t)) );;if left tree has two nodes but right has one node, only move right up eg:(3 (5))-(3 5))
                                                                ( (and(not(null?(cdadr t)))(null?(cdar t)))(list (caar t)(cadr t)) );;the same as last part, the difference is lefttree has one node
                                                                (else t);;if left and right both have two nodes, we do not need to operate
                                                               ))
                   ((and(not(list? (car t)))(list? (cadr t))) (if (null?(cdadr t))(list (car t)(caadr t)) t) );;for this situation only right is a subtree, as the previous,if the right tree only has one node, move it up
                   ((and(list? (car t))(not(list? (cadr t)))) (if (null?(cdar t))(list (caar t)(cadr t)) t) );;the same as previous
                   (else t)
                   ))
 )))

(define (streamlineOuter t);;outer operation in order to update the same tree several times,;for example, (3 (3))-(3 3)-(3)
 
  ( if (equal? t (streamlineInner t));;if the update has no changes, return the tree
       t (if(number? (streamlineInner t))t (streamlineOuter (streamlineInner t))));;if the update has changes,then update it again until there is no changes

   )

(define (streamline t);;main recursive function in order to update and build the tree from the innermost to outermost 
  (if (null? t) t;; if tree is empty, return
  (cond ( (null?(cdr t)) t);;if the tree only has one node, we do not need to operate
  (else (let((left (car t))(right (cadr t))) ;;else if both left tree and right tree are exist we need to operate the lefttree and right tree
    (cond ((and(list? left)(list? right))(streamlineOuter (list (streamline left)(streamline right))));;the left tree and right tree are both have their subtrees,we need to update the tree from the lefttree and right tree,then update the whole tree
          ((and(not(list? left))(not(list? right))) (streamlineOuter t));;if the left and right tree are numbers, we just need to update the whole tree
          ((and(list? left)(not(list? right))) (streamlineOuter (list (streamline left) right)));;if the left tree is a subtree, update that and merge the right branch before update the whole tree
          ((and(list? right)(not(list? left))) (streamlineOuter (list left (streamline right))));;the same as previous
    )
  )))))

;;I use two examples to test the answer, both answers should be (3 4)
(define tree1 (binaryTreeNode null (binaryTreeNode null 3 (binaryTreeNode null 4 4)) (binaryTreeNode null 3 (binaryTreeLeaf 4))));;((3 (4 4))(3 (4)))

(define tree2 (binaryTreeNode null (binaryTreeNode null  3 4) (binaryTreeNode null (binaryTreeLeaf 3) (binaryTreeLeaf 4))));;((3 (4 4))((3) (4)))
(displayln "")
(display tree1);;((3 (4 4))(3 (4)))
(displayln "")
(display (streamline tree1));;(3 4)
(displayln"")
(displayln"")
(display tree2);;((3 (4 4))((3) (4)))
(displayln"")
(display (streamline tree2));;(3 4)