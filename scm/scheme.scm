(define (reverse lst)
  (define (rev-loc acc lst)
    (if (null? lst)
	acc
	(rev-loc (cons (car lst)
		       acc)
		 (cdr lst))))
  (rev-loc '() lst))

(define (map f lst)
  (if (null? lst)
      lst
      (cons (f (car lst))
	    (map f (cdr lst)))))

(define (filter p lst)
  (if (null? lst)
      lst
      (let ((h (car lst)))
	(if (p h)
	    (cons h (cdr lst))
	    (cdr lst)))))

(define (reduce f lst)
  (if (null? lst)
      lst
      (let ((init (car lst)))
	(define (red-rec f acc lst)
	  (if (null? lst)
	      acc
	      (red-rec f
		       (f acc (car lst))
		       (cdr lst))))
	(red-rec f init (cdr lst)))))
