#lang racket
;;Daotong Dai 0646436 BTree delete
(define-struct binaryTree (root leftSubTree rightSubTree))

(define (leaf? btree);;judge whether tree lsubtree and rsubtree are null
  (if (and(not(null?(binaryTree-root btree)))(null?(binaryTree-leftSubTree btree))(null?(binaryTree-rightSubTree btree)))#t #f)
  )


(define (bTreeHeight btree);;calculate trees height(because we need to find right most node in left tree or left most node in right tree to substitute the position of deleted node, this function is mainly for considering whether we should find substitute node in left tree or right tree)
  (cond ((null? btree) 0)
        ((leaf? btree) 1)
        (else (cond((and(not(null?(binaryTree-leftSubTree btree)))(not(null?(binaryTree-rightSubTree btree))))
                (+ 1 (max (bTreeHeight (binaryTree-leftSubTree btree))
                       (bTreeHeight (binaryTree-rightSubTree btree)))))
               ((and(null?(binaryTree-leftSubTree btree))(not(null?(binaryTree-rightSubTree btree))))
                (+ 1 (bTreeHeight (binaryTree-rightSubTree btree))))
               ((and(null?(binaryTree-rightSubTree btree))(not(null?(binaryTree-leftSubTree btree))))
                (+ 1 (bTreeHeight (binaryTree-leftSubTree btree))))
               (else 1)
    ))))



(define (getRightMost t);;return the right most node
  (cond ((null? t) null)
        ((leaf? t)(binaryTree-root t) )
        (else (cond((not(null?(binaryTree-rightSubTree t))) (getRightMost (binaryTree-rightSubTree t)))
                   (else (binaryTree-root t))
                   ))

  ))

(define (getRightMostInLeftSubTree t);;by using the getRightNost node to find the substitute node in left tree
  (cond ((null? t) null)
        ((leaf? t)(binaryTree-root t) )
        (else (cond((not(null?(binaryTree-leftSubTree t))) (getRightMost (binaryTree-leftSubTree t)))
                   (else (binaryTree-root t))
                   ))

  ))


(define (getLeftMost t);;return the left most node
  (cond ((null? t) null)
        ((leaf? t)(binaryTree-root t) )
        (else (cond((not(null?(binaryTree-leftSubTree t))) (getLeftMost (binaryTree-leftSubTree t)))
                   (else (binaryTree-root t))
                   ))

  ))
(define (getLeftMostInRightSubTree t) ;;by using the getLeftNost node to find the substitute node in right tree
  (cond ((null? t) null)
        ((leaf? t)(binaryTree-root t) )
        (else (cond((not(null?(binaryTree-rightSubTree t))) (getLeftMost (binaryTree-rightSubTree t)))
                   (else (binaryTree-root t))
                   ))

  ))

(define (deleteLeafNode t n);;because the substitute node must be leaf node, this function is mainly for deleting the original place of substitute node
   (cond ((null? t) t)
        ((leaf? t) (if(=(binaryTree-root t) n) null t ))
        (else (cond((<(binaryTree-root t) n)(if (not(null?(binaryTree-rightSubTree t))) (make-binaryTree (binaryTree-root t) (binaryTree-leftSubTree t) (deleteLeafNode (binaryTree-rightSubTree t) n)) t))
                   ((>(binaryTree-root t) n)(if (not(null?(binaryTree-leftSubTree t))) (make-binaryTree (binaryTree-root t) (deleteLeafNode (binaryTree-leftSubTree t) n) (binaryTree-rightSubTree t)) t))
                   ((=(binaryTree-root t) n) null)
  ))))



(define (preorderTraverse t);;display a binary tree in preorder
  (define (visitNode x) (display "( ") (display x) (display " "))
  (cond ((null? t) display "binary tree is null")
      (else (visitNode (binaryTree-root t))
            (cond ((and(null?(binaryTree-rightSubTree t))(null?(binaryTree-leftSubTree t))) (display "()")(display " ")(display "()"))
                  ((and(null?(binaryTree-rightSubTree t))(not(null?(binaryTree-leftSubTree t)))) (preorderTraverse (binaryTree-leftSubTree t))(display "()"))
                  ((and(null?(binaryTree-leftSubTree t))(not(null?(binaryTree-rightSubTree t)))) (display "()")(display " ")(preorderTraverse (binaryTree-rightSubTree t)))
                  ( (and(not(null?(binaryTree-leftSubTree t)))(not(null?(binaryTree-rightSubTree t))))(preorderTraverse (binaryTree-leftSubTree t))(preorderTraverse (binaryTree-rightSubTree t))))))(display " )") )




(define (delete t n);;this is the main function, t is the sorted tree, n is the node that need to be deleted 
 (cond ((null? t) t)
        ((leaf? t) (if(=(binaryTree-root t) n)null t ));;if n equals the t's root, then delete successful else delete fails
        (else (cond((<(binaryTree-root t) n);;if n is larger than root ,then find the node in the rightsubtree and run the function recursively
                                          (if (not(null?(binaryTree-rightSubTree t))) (make-binaryTree (binaryTree-root t) (binaryTree-leftSubTree t) (delete (binaryTree-rightSubTree t) n)) t))
                   ((>(binaryTree-root t) n);;if n is smaller than root, then find the node in the left subtree and run the function recursively
                                           (if (not(null?(binaryTree-leftSubTree t))) (make-binaryTree (binaryTree-root t) (delete (binaryTree-leftSubTree t) n) (binaryTree-rightSubTree t)) t))
                   ((=(binaryTree-root t) n);;if n equals root, then we should find the substitute node to take position of the root,if leftsubtree's hight is biggerthan right, then find the rightmost node in left tree,vice visa
                                           (cond 
                                                  ((and(null?(binaryTree-rightSubTree t))(not(null?(binaryTree-leftSubTree t))))(make-binaryTree (getRightMostInLeftSubTree t) (deleteLeafNode (binaryTree-leftSubTree t) (getRightMostInLeftSubTree t))  (binaryTree-rightSubTree t) ) )
                                                  ((and(null?(binaryTree-leftSubTree t))(not(null?(binaryTree-rightSubTree t)))) (make-binaryTree (getLeftMostInRightSubTree t) (binaryTree-leftSubTree t) (deleteLeafNode (binaryTree-rightSubTree t) (getLeftMostInRightSubTree t)) ))
                                                  ((and(not(null?(binaryTree-leftSubTree t)))(not(null?(binaryTree-rightSubTree t))))(if(> (bTreeHeight (binaryTree-leftSubTree t)) (bTreeHeight (binaryTree-rightSubTree t)))
                                                                                                                                        (make-binaryTree (getRightMostInLeftSubTree t) (deleteLeafNode (binaryTree-leftSubTree t) (getRightMostInLeftSubTree t)) (binaryTree-rightSubTree t) )
                                                                                                                                        (make-binaryTree (getLeftMostInRightSubTree t) (binaryTree-leftSubTree t) (deleteLeafNode (binaryTree-rightSubTree t) (getLeftMostInRightSubTree t)) ) ))
                                                    (else t)))
                                         

                  
                   ))
       
  ))

;;two examples to test the results
(define g (make-binaryTree 2  null (make-binaryTree 3 (make-binaryTree 4 null null) (make-binaryTree 6 (make-binaryTree 5 null null)(make-binaryTree 8 (make-binaryTree 7 null null)(make-binaryTree 9 null null))))))
(define h(make-binaryTree 5 (make-binaryTree 3 (make-binaryTree 2 null null) (make-binaryTree 4 null null)) (make-binaryTree 8 (make-binaryTree 7 null null) (make-binaryTree 10 (make-binaryTree 9 null null)(make-binaryTree 11 null null)))))
(preorderTraverse h)
(displayln"")
(preorderTraverse (delete h 5));;result is correct(delete 5 from h)
(displayln"")
(displayln"")
(preorderTraverse g)
(displayln"")
(preorderTraverse (delete g 6));;result is correct(delete 6 from g)