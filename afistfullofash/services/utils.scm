(define-module (afistfullofash services utils)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (scm->yaml))

(define (scm->yaml-inner val indent-level)
  (let ((indent (make-string indent-level #\space))
	(next-indent-level (+ indent-level 2)))
    (cond
     ;; 1. The "Default_Config" case: Null/False
     ((not val)
      (format #f ""))
     
     ;; 2. CONS = MAP (Alist)
     ((pair? val)
      (let ((fmt-function (lambda (pair prev)
                            (if (or (not (cdr pair))
				    (list? (cdr pair))
				    (pair? (cdr pair))
				    (vector? (cdr pair)))
                                (format #f "~a~a~a:~%~a"
					prev
					indent
					(car pair)
					(scm->yaml (cdr pair) next-indent-level))
				(format #f "~a~a~a: ~a~%"
					prev
					indent
					(car pair)
					(scm->yaml (cdr pair) next-indent-level))))))
	(if (and (list? val) (pair? (car val)))	    
            (fold
	     fmt-function
	     ""
	     val)
	    (fmt-function val ""))))

     ;; 3. VECTOR = LIST (Sequence)
     ((vector? val)
      (fold (lambda (item prev)
	      (format #f "~a~a- ~a~%"
		      prev
		      indent
		      (string-trim (scm->yaml item indent-level))))
	    ""
	    (vector->list val)))
     
     ;; 4. SCALARS (Strings, Numbers)
     (else
      (format #f "~a" val)))))

(define scm->yaml
  (case-lambda
    ((val) (scm->yaml val 0))
    ((val indent-level) (scm->yaml-inner val indent-level))))
