; Concepts of Programming Languages ;
; Lab 3 ;
; Emmanuel Armstrong & Matthew Rohrlach ;

(define (f list)
	; (a) ;
	(if (null? list)
		; (b) ;
		'()
		; (c) ;
		(cons (1 + (car list)) (f (cdr list)))))
		
(define (member? e list)
	; checks if the list is empty ;
	(cond ((null? list)#f)
	;checks if e is equal to the first object in the list, if so return T, if not then it runs member recursively on the rest of the list ;
	((equal? e (car list))#t) (else (member? e (cdr list)))))
	
(define (set? list)
	; checks if the list is empty ;
	(cond ((null? list) #t)
		; if the item at the beginning of the list appears in the rest of the list, return F;
		((member? (car list) (cdr list)) #f)
		; if it is not present run set again on the list minus the first element ;
		(else (set? (cdr list)))))
		
(define (union list1 list2)
	; checks if either list is empty,  if one is then it returns the other list ;
	(cond ((null? list1) list2) ((null? list2) list1)
		; if the first element of list 1 is in list 2 then call the function again on the rest of both lists;
		((member? (car list1) list2) (union (cdr list1) list2))
			; if it isnt then concatenate the first element of list 1 with the remainder of list 1 and list 2 ;
			(else (cons (car list1) (union (cdr list1) list2)))))
			
(define (intersect list1 list2)
	; if either set is empty return empty set ;
	(cond ((or (null? list1) (null? list2)) '())
		; if the first element of list 1 is in list 2 return the concatenation of the first element and the intersect of the rest of the lists ;
		((member? (car list1) list2) (cons (car list1)
		(intersect (cdr list1) list2)))
			; Otherwise run the function atgain on the rest of list 1 and list 2 ;
			(else (intersect (cdr list1) list2))))