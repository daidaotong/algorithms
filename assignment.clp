(deftemplate father-of (slot father) (slot child))
(deftemplate mother-of (slot mother) (slot child))
 (deftemplate parent-of (slot parent) (slot child))
 (deftemplate sister-of (slot sister) (slot sibling))
(deftemplate brother-of (slot brother) (slot sibling))
 (deftemplate aunt-of (slot aunt) (slot nephew-or-niece))
 (deftemplate uncle-of (slot uncle) (slot nephew-or-niece))
(deftemplate cousin-of (slot cousin-1) (slot cousin-2))
(deftemplate grandfather-of (slot grandfather) (slot grandchild))
 (deftemplate grandmother-of (slot grandmother) (slot grandchild))
 (deftemplate grandparent-of (slot grandparent) (slot grandchild))
 (deftemplate wife-of (slot wife) (slot husband))
 (deftemplate husband-of (slot husband) (slot wife))
 (deftemplate ancestor-of (slot ancestor)(slot person))
 (deftemplate male (slot person))
 (deftemplate female (slot person))


(deffacts fathers
	(father-of (father Michael)(child John))
	(father-of (father Michael)(child Peter))
	(father-of (father Peter)(child Todd))
	(father-of (father Peter)(child Jackie))
	(father-of (father John)(child Hayley))
	(father-of (father John)(child Emily))
	)

(deffacts mothers
	(mother-of (mother Shirley)(child John))
	(mother-of (mother Shirley)(child Peter))
	(mother-of (mother Cathy)(child Todd))
	(mother-of (mother Cathy)(child Jackie))
	(mother-of (mother Shelly)(child Hayley))
	(mother-of (mother Shelly)(child Emily))
	)

(deffacts husbands
	(husband-of (husband Michael)(wife Shirley))
	(husband-of  (husband Peter)(wife Cathy))
	(husband-of  (husband John)(wife Shelly)))

(deffacts males
	(male (person Michael))
	(male (person Peter))
	(male (person John))
	(male (person Todd)))

(deffacts females
	(female (person Shirley))
	(female (person Cathy))
	(female (person Shelly))
	(female (person Jackie))
               (female (person Hayley))
	(female (person Emily)))





(defrule brother-of (mother-of (mother ?mom)(child ?child1))
                           (father-of (father ?dad)(child ?child1))
                           (mother-of (mother ?mom)(child ?child2))
                           (father-of (father ?dad)(child ?child2))
                           (male (person ?child2))
		
                           =>
		(if (neq ?child1 ?child2)

                           then(assert (brother-of(brother ?child2)(sibling ?child1)))
                           (printout t ?child2 " is a brother of" ?child1 crlf)))

(defrule sister-of (mother-of (mother ?mom)(child ?child1))
                           (father-of (father ?dad)(child ?child1))
                           (mother-of (mother ?mom)(child ?child2))
                           (father-of (father ?dad)(child ?child2))
                           (female (person ?child2))
                           =>
		(if (neq ?child1 ?child2)
                           then(assert (sister-of(sister ?child2)(sibling ?child1)))
                           (printout t ?child2 " is a sister of" ?child1 crlf)))
(defrule parent-of 
		(or (father-of (father ?parent)(child ?child))
		 (mother-of (mother ?parent)(child ?child)))
		 =>
                           (assert (parent-of(parent ?parent)(child ?child))))

(defrule husband-wife 
               (husband-of (husband ?husband) (wife ?wife))
               =>
               (assert (wife-of (wife ?wife) (husband ?husband))))

(defrule uncle-of-has-blood-relationship "if children's parents has brother,then he is the uncle of children"
                                                     (parent-of (parent ?parent)(child ?child))
                                                      (brother-of (brother ?uncle)(sibling ?parent))
                                                      =>
                                                      (assert (uncle-of (uncle ?uncle)(nephew-or-niece ?child)))
                                                       (printout t ?uncle " is a uncle of" ?child crlf))


(defrule uncle-of-not-has-blood-relationship "if children's parents has sister,then her husband is the uncle of children"
                                                     (parent-of (parent ?parent)(child ?child))
                                                      (sister-of (sister ?aunt)(sibling ?parent))
			           (husband-of (husband ?uncle)(wife ?aunt))
                                                      =>
                                                      (assert (uncle-of (uncle ?uncle)(nephew-or-niece ?child)))
                                                       (printout t ?uncle " is a uncle of" ?child crlf))

(defrule aunt-of-has-blood-relationship "if children's parents has aunt,then she is the aunt of children"
                                                     (parent-of (parent ?parent)(child ?child))
                                                     (sister-of (sister ?aunt)(sibling ?parent))
                                                      =>
                                                     (assert (aunt-of (aunt ?aunt)(nephew-or-niece ?child)))
                                                       (printout t ?aunt " is a aunt of" ?child crlf))

(defrule aunt-of-not-has-blood-relationship "if children's parents has brother,then his wife is the aunt of children"
                                                     (parent-of (parent ?parent)(child ?child))
                                                      (brother-of (brother ?uncle)(sibling ?parent))
			           (husband-of (husband ?uncle)(wife ?aunt))
                                                      =>
                                                      (assert (aunt-of (aunt ?aunt)(nephew-or-niece ?child)))
                                                       (printout t ?aunt " is a aunt of" ?child crlf))

(defrule cousin-of-from-uncle-has-blood-relationship "if children's parents has brother,then his children are the cousin of the children"
				(parent-of (parent ?parent)(child ?child))
                                                      (brother-of (brother ?uncle)(sibling ?parent))
			           (father-of (father ?uncle)(child ?cousin))
			           =>
                                                      (assert (cousin-of (cousin-1 ?cousin)(cousin-2 ?child)))
                                                       (printout t ?cousin " is a cousin of" ?child crlf))

 (defrule cousin-of-from-aunt-has-blood-relationship "if children's parents has sister,then her children are the cousin of the children"
				(parent-of (parent ?parent)(child ?child))
                                                      	(sister-of (sister ?aunt)(sibling ?parent))
			          	(mother-of (mother ?aunt)(child ?cousin))
			           =>
                                                      (assert (cousin-of (cousin-1 ?cousin)(cousin-2 ?child)))
                                                       (printout t ?cousin " is a cousin of" ?child crlf))

(defrule grandfather-of  "the frandfather is the  parent's father"
			(parent-of (parent ?parent)(child ?child))
			(father-of (father ?grandfather)(child ?parent))
			 =>
                                              (assert (grandfather-of (grandfather ?grandfather)(grandchild ?child)))
                                              (printout t ?grandfather " is a grandfather of" ?child crlf))

(defrule grandmother-of  "the frandmother is the  parent's mother"
			(parent-of (parent ?parent)(child ?child))
			(mother-of (mother ?grandmother)(child ?parent))
			 =>
                                              (assert (grandmother-of (grandmother ?grandmother)(grandchild ?child)))
                                              (printout t ?grandmother" is a grandmother of" ?child crlf))

(defrule grandparent-of  "the grandparent is the  grandmother or grandfather"
			(or(grandmother-of (grandmother ?grandparent)(grandchild ?child))
			     (grandfather-of (grandfather ?grandparent)(grandchild ?child)))	
			 =>
                                              (assert (grandparent-of (grandparent ?grandparent)(grandchild ?child)))
                                              (printout t ?grandparent" is a grandparent of" ?child crlf))



(defrule mother-or-father-is-ancestor 
               (or(mother-of (mother ?ancestor)(child ?child))
	     (father-of (father ?ancestor)(child ?child)))
               =>
               (assert (ancestor-of(ancestor ?ancestor)(person ?child)))
	(printout t ?ancestor" is a ancestor of" ?child crlf))


(defrule parents-ancestor-is-ancestor 
                (parent-of (parent ?parent)(child ?child))
                 (ancestor-of (ancestor ?ancestor)(person ?parent))
                 =>
                  (assert (ancestor-of(ancestor ?ancestor)(person ?child)))
	(printout t ?ancestor" is a ancestor of" ?child crlf))

