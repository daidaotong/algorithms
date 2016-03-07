#lang racket
;;Daotong Dai 0646436 Hamilton Graph
(define-struct edge(orig dest))

(define (successors node graph);;in order to find the successors of a node
(cond [(empty? graph) empty]
[else
(local [(define an-edge (first graph))]
(cond [(symbol=? node (edge-orig an-edge))
(cons (edge-dest an-edge)
(successors node (rest graph)))]
[else
(successors node (rest graph))]))]))



(define (nodeNumber g nodes-so-far);;calculate the number of vertices in a graph ,nodes-so-far is the accumulator and the beginning value is '()
  (cond ((empty? g) 0)
        (else (local [(define an-edge (first g))]
                (cond[(and(cons? (member (edge-orig an-edge) nodes-so-far))(not (cons? (member (edge-dest an-edge) nodes-so-far))));;if a edge's origin is calculated before, and the destination is not,add destionation into node-so-far value+1
                      (+ 1 (nodeNumber (rest g) (cons (edge-dest an-edge) nodes-so-far )))]
                     [(and(cons? (member (edge-orig an-edge) nodes-so-far)) (cons? (member (edge-dest an-edge) nodes-so-far)));;if the destination and origin are all calculated before, value do not added
                      (nodeNumber (rest g) nodes-so-far)]
                     [(and(cons? (member (edge-dest an-edge) nodes-so-far))(not (cons? (member (edge-orig an-edge) nodes-so-far))));;if a edge's destination is calculated before, and the origin is not,add origin into node-so-far, value+1
                      (+ 1 (nodeNumber (rest g) (cons (edge-orig an-edge) nodes-so-far )))]
                     [(and(not(cons? (member (edge-orig an-edge) nodes-so-far)))(not (cons? (member (edge-dest an-edge) nodes-so-far))));;if destionation and origin are both not calculated before,add them into accumulator, value +2
                      (+ 2 (nodeNumber (rest g) (cons (edge-orig an-edge) (cons (edge-dest an-edge) nodes-so-far ))))])))))



(define (hamiltonRoute g);;main function
  (cond((empty? g) empty)
  (else (local[(define origin (edge-orig (first g)));;because the hamilton route is a circle, so there is no matter which vertice is origin, I choose the origin of first edge as origin
              (define (hamiltonRouteNode orig g nodes-so-far);;one node function
                   (cond ((and(= (nodeNumber g '()) (length nodes-so-far))(eqv? orig origin))(reverse (cons orig nodes-so-far)) );;if all the nodes are visited and there is no duplicate visit, then get the nodes-so-far reversely
                         (else (if (cons? (member orig nodes-so-far)) false (hamiltonRouteNodes (successors orig g) g (cons orig nodes-so-far))));;if nodes are not all visited and visit the duplicate node, then this function return false,if not, send the successors of this node to the multiple nodes function 
                ))
               (define (hamiltonRouteNodes origins g nodes-so-far);;multiple nodes function
                (cond ((empty? origins) false)
                      (else (local [(define a-route (hamiltonRouteNode (first origins) g nodes-so-far))]
                              (if (equal? a-route false);;if one route fails, then try to find other successor routes
                                  (hamiltonRouteNodes (rest origins) g nodes-so-far) a-route)))))]           
    (hamiltonRouteNode origin g '())))))




(define G3  ;;this is the test graph, the shape of this graph is in the photo
  (list (make-edge 'a 'b)
        (make-edge 'b 'a)
        (make-edge 'b 'c)
        (make-edge 'c 'b)
        (make-edge 'c 'd)
        (make-edge 'd 'c)
        (make-edge 'd 'e)
        (make-edge 'e 'd)
        (make-edge 'e 'a)
        (make-edge 'a 'e)
        (make-edge 'a 't)
        (make-edge 't 'a)
        (make-edge 'b 'r)
        (make-edge 'r 'b)
        (make-edge 'c 'p)
        (make-edge 'p 'c)
        (make-edge 'n 'd)
        (make-edge 'd 'n)
        (make-edge 'f 'e)
        (make-edge 'e 'f)
        (make-edge 'q 'p)
        (make-edge 'p 'q)
        (make-edge 'r 'q)
        (make-edge 'q 'r)
        (make-edge 'r 's)
        (make-edge 's 'r)
        (make-edge 's 't)
        (make-edge 't 's)
        (make-edge 't 'g)
        (make-edge 'g 't)
        (make-edge 'g 'f)
        (make-edge 'f 'g)
        (make-edge 'f 'm)
        (make-edge 'm 'f)
        (make-edge 'm 'n)
        (make-edge 'n 'm)
        (make-edge 'o 'n)
        (make-edge 'n 'o)
        (make-edge 'p 'o)
        (make-edge 'o 'p)
        (make-edge 'q 'j)
        (make-edge 'j 'q)
        (make-edge 's 'i)
        (make-edge 'i 's)
        (make-edge 'h 'g)
        (make-edge 'g 'h)
        (make-edge 'l 'm)
        (make-edge 'm 'l)
        (make-edge 'o 'k)
        (make-edge 'k 'o)
        (make-edge 'j 'k)
        (make-edge 'k 'j)
        (make-edge 'j 'i)
        (make-edge 'i 'j)
        (make-edge 'i 'h)
        (make-edge 'h 'i)
        (make-edge 'h 'l)
        (make-edge 'l 'h)
        (make-edge 'l 'k)
        (make-edge 'k 'l)))

                      
(hamiltonRoute G3);;answer is correct