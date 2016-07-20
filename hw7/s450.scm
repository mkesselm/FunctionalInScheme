;;; file: s450.scm (base)
;;;
;;; Metacircular evaluator from chapter 4 of STRUCTURE AND
;;; INTERPRETATION OF COMPUTER PROGRAMS (2nd edition)
;;;
;;; Modified by kwn, 3/4/97
;;; Modified and commented by Carl Offner, 10/21/98 -- 10/12/04
;;;
;;; This code is the code for the metacircular evaluator as it appears
;;; in the textbook in sections 4.1.1-4.1.4, with the following
;;; changes:
;;;
;;; 1.  It uses #f and #t, not false and true, to be Scheme-conformant.
;;;
;;; 2.  Some function names were changed to avoid conflict with the
;;; underlying Scheme:
;;;
;;;       eval => xeval
;;;       apply => xapply
;;;       extend-environment => xtend-environment
;;;
;;; 3.  The driver-loop is called s450.
;;;
;;; 4.  The booleans (#t and #f) are classified as self-evaluating.
;;;
;;; 5.  These modifications make it look more like UMB Scheme:
;;;
;;;        The define special form evaluates to (i.e., "returns") the
;;;          variable being defined.
;;;        No prefix is printed before an output value.
;;;
;;; 6.  I changed "compound-procedure" to "user-defined-procedure".
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 xeval and xapply -- the kernel of the metacircular evaluator
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; define xeval
(define (xeval exp env)
  (let ((action (lookup-action (type-of exp))))
    (if action
	(action exp env)
	(cond ((self-evaluating? exp) exp)
	      ((eof-object? exp) 'exit)
	      ((variable? exp)
	       (if (lookup-action exp)
		   (begin
		     (display "Special form: ")
		     (display exp)
		     (newline))
		   (lookup-variable-value exp env)))
	      ((application? exp)
	       ;(display "XEVAL-APPLICATION---")(display exp)
	       ;(newline)
	       (xapply (xeval (operator exp) env)
		       (list-of-values (operands exp)
				       (procedure-parameters (xeval (operator exp) env))
				       env) env))
	      (else
	       (error450 "Unknown expression type -- XEVAL " exp)))))
  )

(define target '())

(define (delayed? exp)
  (if (pair? exp)
      (equal? 'delayed (car exp))
      #f))

(define (dynamic? exp)
  (if (pair? exp)
      (equal? 'dynamic (car exp))
      #f))

(define (reference? exp)
  (if (pair? exp)
      (equal? 'reference (car exp))
      #f))

(define (eval-map args env)
  (begin
    ;(display "EVALMAP---");(display args)
    ;(newline)
    (if (null? args)
	'()
	(cons (xeval (car args) env)
	      (eval-map (cdr args) env)))))

(define (xapply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure (eval-map arguments env)))
        ((user-defined-procedure? procedure)
	 (let ((output
		(begin
		  ;(display "XAPPLY-USERDEFPROC ")
		  ;(newline)
		  (eval-sequence
		   (procedure-body procedure)
		   (xtend-environment
		    (procedure-parameters procedure)
		    arguments
		    (procedure-environment procedure))))))
	   (pop)
	   output))
        (else
         (error450
          "Unknown procedure type -- XAPPLY " procedure))))

;;; Handling procedure arguments

(define (list-of-values exps params env)
  (begin
    ;(display "LIST-OF-VALUES----")(display params)
    ;(newline)
    (cond ((no-operands? exps) '())
	  ((not (pair? params))
	   (cons (xeval (first-operand exps) env)
		 (list-of-values (rest-operands exps) params env)))
	  ((delayed? (car params))
	   (cons (list 'thunk env (first-operand exps))
		 (list-of-values (rest-operands exps) (cdr params) env)))
	  ((dynamic? (car params))
	   (cons (xeval (first-operand exps) the-dynamic-environment)
		 (list-of-values (rest-operands exps) (cdr params) env)))
	  ((reference? (car params))
	   (cons (list 'referent (lookup-variable-environment (first-operand exps) env) (first-operand exps))
		 (list-of-values (rest-operands exps) (cdr params) env)))
	  (else (cons (xeval (first-operand exps) env)
		      (list-of-values (rest-operands exps) (cdr params) env))))))

;;; These functions, called from xeval, do the work of evaluating some
;;; of the special forms:

(define (eval-if exp env)
  (if (true? (xeval (if-predicate exp) env))
      (xeval (if-consequent exp) env)
      (xeval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (begin
    ;(display exps)
    ;(display "EVAL-SEQUENCE")
    ;(newline)
    (cond ((last-exp? exps) (xeval (first-exp exps) env))
	  (else (xeval (first-exp exps) env)
		(eval-sequence (rest-exps exps) env)))))

(define (eval-assignment exp env)
  (let ((name (assignment-variable exp)))
    (if (lookup-action name)
	(error450 "Cannot re-assign special form: " name)
	(set-variable-value! name
			     (xeval (assignment-value exp) env)
			     env))
  name))    ;; A & S return 'ok

(define (eval-definition exp env)
  (let ((name (definition-variable exp)))
    (if (lookup-action name)
	(error450 "Cannot redefine special form: " name)
	(define-variable! name
	  (xeval (definition-value exp) env)
	  env))
    name))     ;; A & S return 'ok

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Representing expressions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Numbers, strings, and booleans are all represented as themselves.
;;; (Not characters though; they don't seem to work out as well
;;; because of an interaction with read and display.)

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (boolean? exp)
      ))

;;; variables -- represented as symbols

(define (variable? exp)
  (symbol? exp))

;;; quote -- represented as (quote <text-of-quotation>)

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;;; assignment -- represented as (set! <var> <value>)

(define (assignment? exp) 
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;; definitions -- represented as
;;;    (define <var> <value>)
;;;  or
;;;    (define (<var> <parameter_1> <parameter_2> ... <parameter_n>) <body>)
;;;
;;; The second form is immediately turned into the equivalent lambda
;;; expression.

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;;; lambda expressions -- represented as (lambda ...)
;;;
;;; That is, any list starting with lambda.  The list must have at
;;; least one other element, or an error will be generated.

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;; conditionals -- (if <predicate> <consequent> <alternative>?)

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


;;; sequences -- (begin <list of expressions>)

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


;;; procedure applications -- any compound expression that is not one
;;; of the above expression types.

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;;; Derived expressions -- the only one we include initially is cond,
;;; which is a special form that is syntactically transformed into a
;;; nest of if expressions.

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      #f                          ; no else clause -- return #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error450 "ELSE clause isn't last -- COND->IF "
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Truth values and procedure objects
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Truth values

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))


;;; Procedures

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (user-defined-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Representing environments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An environment is a list of frames.

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (pop) (set! the-dynamic-environment (cdr the-dynamic-environment)))
(define (push frame) (set! the-dynamic-environment (cons frame the-dynamic-environment)))

;;; Each frame is represented as a pair of lists:
;;;   1.  a list of the variables bound in that frame, and
;;;   2.  a list of the associated values.

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;;; Extending an environment
(define the-dynamic-environment '())

;; Scrub variables of any tags (e.g., dynamic, reference, etc)
(define (no-tag vars)
  (cond ((null? vars) '())
	((pair? (car vars))
	 (cons (cadar vars)
	       (no-tag (cdr vars))))
	(else (cons (car vars) (no-tag (cdr vars))))))

(define (xtend-environment vars vals base-env)
  (let ((clean-vars (no-tag vars)))
    (cond ((= (length clean-vars) (length vals))
	   (let ((new-frame (make-frame clean-vars vals)))
	     (push new-frame)
	     (cons new-frame base-env)))
	  ((< (length clean-vars) (length vals))
	   (error450 "Too many arguments supplied " vars vals))
	  (else (error450 "Too few arguments supplied " vars vals)))))

;;; Looking up a variable in an environment

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (begin
	       ;(display "FOUND----")(display var)
	       ;(newline)
	       (car vals)))
            (else (begin
		    ;(display "RECURSELVV----")(display var)
		    ;(newline)
		    (scan (cdr vars) (cdr vals))))))
    (if (eq? env the-empty-environment)
        (error450 "Unbound variable " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;; Looking up a variable's containing environment

(define (lookup-variable-environment var env)
  (define (env-loop env)
    (define (scan vars)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     env)
	    (else (scan (cdr vars)))))
    (if (eq? env the-empty-environment)
	(error450 "Unbound variable " var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)))))
  (env-loop env))

;;; Setting a variable to a new value in a specified environment.
;;; Note that it is an error if the variable is not already present
;;; (i.e., previously defined) in that environment.

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET! " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;; Defining a (possibly new) variable.  First see if the variable
;;; already exists.  If it does, just change its value to the new
;;; value.  If it does not, define the new variable in the current
;;; frame.

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (begin
	       ;(newline)
	       ;(display "ADDING BINDING FOR ")(display var)
	       ;(newline)
	       (add-binding-to-frame! var val frame)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 The initial environment AND ADDITIONAL PROCEDURES
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is initialization code that is executed once, when the the
;;; interpreter is invoked.

(define (setup-environment)
  (let ((initial-env
         (xtend-environment (primitive-procedure-names)
                            (primitive-procedure-objects)
                            the-empty-environment)))
    initial-env))

;;; Define the primitive procedures

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '= =)
	(list 'stream-car car)
	(list 'stream-cdr (lambda(x)(force (cdr x))))
	(list 'stream-null? null?)
	(list 'the-empty-stream '())
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;;; Here is where we rely on the underlying Scheme implementation to
;;; know how to apply a primitive procedure.

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

;;; initialize the table of special forms
;;; N.B., removed application as a special form and placed it in the xeval cond space after cleaning this up and looking at other people's implementation--this was the only way to be able to call a procedure linked to a symbol, though I do not really understand why this would work any differently than calling the application from the special forms table
(define eval-table
  (list	(cons 'set! (lambda (exp env)
		      (eval-assignment exp env)))
	(cons 'define (lambda (exp env)
			(eval-definition exp env)))
	(cons 'if (lambda (exp env)
		    (eval-if exp env)))
	(cons 'begin (lambda (exp env)
		       (eval-sequence (begin-actions exp) env)))
	(cons 'cond (lambda (exp env)
		      (xeval (operator exp) env)))
	(cons 'quote (lambda (exp env)
		       (text-of-quotation exp)))
	(cons 'lambda (lambda (exp env)
			(make-procedure (lambda-parameters exp) (lambda-body exp) env)))
	(cons 'thunk (lambda (exp env)
	 	       (begin
			 ;(display "THUNK---")(display (caddr exp))(newline)
			 (xeval (caddr exp) (cadr exp)))))
	(cons 'referent (lambda (exp env)
			  (xeval (caddr exp) (cadr exp))))
	(cons 'locally-defined? (lambda (exp env)
			  (let ((name (cadr exp)))
			    (find-if-local? name env))))
	(cons 'defined? (lambda (exp env)
			  (let ((name (cadr exp)))
			    (find-in-env? name env))))
	(cons 'locally-make-unbound! (lambda (exp env)
			       (let ((name (cadr exp)))
				 (unbind-locally name env))))
	(cons 'make-unbound! (lambda (exp env)
			       (let ((name (cadr exp)))
				 (unbind-globally name env))))
	(cons 'exit (lambda (exp env)
		      'exit))
	(cons 'cons-stream (lambda (exp env)
			     (cons (cadr exp) (list 'thunk env (caddr exp))))))
  )
  
;;; look-up action obfuscates the table implementation and providing
;;; table look-up

(define (lookup-action form)
  (let ((record (assoc form eval-table)))
    (if record
	(cdr record)
	#f))
  )

;;; add a new special form or modify an existing one
(define (install-special-form form action)
  (let ((record (assoc form eval-table)))
    (if record
	(error "This special form is already defined.")
	(set-cdr! eval-table
		  (cons (cons form action) (cdr eval-table)))))
  form
  )

;;; type-of pulls the special form from the expression
;;; after researching online, I found that I had to produce this conditional
;;; structure or it would continue to basically break everything-however,
;; I don't really understand why this is the case
(define (type-of exp)
  (if (pair? exp)
      (car exp)
      '())
  )

;;; check if a symbol is defined in the environment
;;; structured similarly to the procedure for defining a variable
(define (find-in-local? var env)
  (let ((frame (first-frame env)))
    (define (scan vars)
      (cond ((null? vars) #f)
	    ((eq? var (car vars)) #t)
	    (else (scan (cdr vars)))))
    (scan (frame-variables frame))))


;;; similar to above, except it also pops through all of the environment's frames
(define (find-in-env? var env)
  (let ((frame (car env)))
    (define (scan vars env)
      (cond ((null? vars)
	     (if (null? (cdr env))
		 #f
		 (find-in-env? var (cdr env))))
	    ((eq? var (car vars)) #t)
	    (else (scan (cdr vars) env))))
    (scan (frame-variables frame) env)))

(define (unbind-locally var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) var)
	    ((eq? var (car vars))
	     (if (null? (cdr vars))
		 (begin
		   (set-car! vars '())
		   (set-car! vals '())
		   var)
		 (begin
		   (set-car! vars (cadr vars))
		   (set-car! vals (cadr vals))
		   var)))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define (unbind-globally var env)
  (if (null? env)
      var
      (begin
	(unbind-locally var env)
	(unbind-globally var (cdr env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 The main driver loop
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Note that (read) returns an internal representation of the next
;;; Scheme expression from the input stream.  It does NOT evaluate
;;; what is typed in -- it just parses it and returns an internal
;;; representation.  It is the job of the scheme evaluator to perform
;;; the evaluation.  In this case, our evaluator is called xeval.

(define input-prompt "s450==> ")
(define exit-prompt '())

(define error450 (lambda (. args)
		(newline)
		(display "Error: ")
		(apply display* args)
		(newline)
		(call/cc target)))

(define (s450)
  (call/cc
   (lambda(here)
     (set! target here)))
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (xeval input the-global-environment)))
      (user-print output)
      (set! exit-prompt output)))
  (cond ((equal? exit-prompt 'exit)
	 (begin
	   (newline)
	   (display "Returning to UMB Scheme")))
	(else (s450))))

(define (prompt-for-input string)
  (newline) (newline) (display string))

;;; Note that we would not want to try to print a representation of the
;;; <procedure-env> below -- this would in general get us into an
;;; infinite loop.

(define (user-print object)
  (if (user-defined-procedure? object)
      (display (list 'user-defined-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Here we go:  define the global environment and invite the
;;;        user to run the evaluator.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-dynamic-environment (setup-environment))
(define the-global-environment (setup-environment))

(display "... loaded the metacircular evaluator. (s450) runs it.")
(newline)

(load "load.s450")
(install-special-form 'eval-load eval-load)
