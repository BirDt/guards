(module guards (define-guarded)
  (import
    scheme
    (chicken base)
    (chicken syntax))

 (define-for-syntax (filter fn lst)
   (let loop ((lst lst) (result '()))
     (if (null? lst)
         (reverse result)
         (let ((item (car lst)))
           (loop (cdr lst)
                 (if (fn item) result (cons item result)))))))

 (define-syntax define-guarded
   (ir-macro-transformer
    (lambda (exp inject compare)
      (let* ((name (if (list? (caadr exp))
		       (car (caadr exp))
		       (caadr exp)))
	     (param-handler (lambda (x)
			      (if (list? x)
				  (car x)
				  x)))
	     (params (map param-handler (cdadr exp)))
	     (guard-handler (lambda (x)
			      (if (list? x)
				  (map (lambda (y) `(,(car y) ,(car x) ,@(cdr y))) (cdr x))
				  '())))
	     (guards (map (lambda (guard) `(assert (and ,@guard)))
			  (filter null? (map guard-handler (cdadr exp)))))
	     (body (cddr exp))
	     (r (gensym))
	     (return-guard (if (list? (caadr exp))
			       `(and ,@(map (lambda (y) `(,(car y) ,r ,@(cdr y))) (cdr (caadr exp))))
			       #f)))
	`(define (,name ,@params) ,@guards ,@(if return-guard
						 `((let ((,r (begin ,@body)))
						     (assert ,return-guard)
						     ,r))
						 body)))))))

