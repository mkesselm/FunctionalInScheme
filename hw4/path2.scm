;;PART_2

;; read-file produces a list whose elements are the expressions in the file.
(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
	'()
	(cons expr (read-file)))))

;; Here we go: read in the file that defines the graph

(define data (with-input-from-file "dist.dat" read-file))

;; Lecture implementation of lookup table

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      #f))
	#f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable
			(cons (cons key-2 value)
			      (cdr subtable)))))
	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table)))))
  value)

(define (make-table)
  (list '*table*))

(define (table-ize listoflists newtable)
  (cond ((null? listoflists) newtable)
	((lookup (caar listoflists) (cadar listoflists) newtable) (table-ize (cdr listoflists) newtable))
	(else (insert! (caar listoflists) (cadar listoflists) (caddar listoflists) newtable)
	      (table-ize (cdr listoflists) newtable)))
  )

(define datatable (table-ize data (make-table)))

(define (insert1d! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table)))))
  value)

(define (lookup1d key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	#f)))

;; create a 1d table of vertex values from a list of node-node-edge lists
(define (collect-vertices nnel newtable startholder)
  (let ((init (cons '- 'empty)))
    (cond ((null? nnel) (insert1d! (car startholder) (cons 0 'start) newtable)
			 (insert1d! (cadr startholder) init newtable)
			 newtable)
	  ((eq? (caar nnel) 'start) (collect-vertices (cdr nnel) newtable (car nnel)))
	  (else (insert1d! (caar nnel) init newtable)
		(insert1d! (cadar nnel) init newtable)
		(collect-vertices (cdr nnel) newtable startholder))))
  )

(define pathmap (collect-vertices data (make-table) 'dummy))

;; make a list of neighbors of node

(define (hello-neighbor node table)
  (cond ((null? (cdr table)) #f)
	((eq? (caadr table) node) (cdadr table))
	(else (hello-neighbor node (cdr table))))
  )

;; filter function
(define (filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence))))
  )

;; cost is my edge out to cheapest neighbor, whose cost is edge out to cheapest neighbor, except for 'end whose cost is 0

(define (find-cheapest-child node children return)
  (let* ((child-edge (cdar children))
	 (child-cost (car (lookup1d (caar children) pathmap))))
    (cond ((null? children) return)
	  ((eq? child-cost '-) (find-cheapest-child node (cdr children) return))
	  (( < (+ child-edge child-cost) (+ (cdr return) (lookup node (car return) datatable)))
	   (find-cheapest-child node (cdr children) (car children)))
	  (else find-cheapest-child node (cdr children) return)))
  )

(define (naive-cost nodeish)
  (cond ((eq? nodeish 'end) 0)
        ((not (hello-neighbor nodeish datatable)) 99999)
	(else (let* ((shortest (apply min (map naive-cost
					       (map (lambda (x) (car x))
						    (hello-neighbor nodeish datatable)))))
		     (short-pred (filter (lambda (x) (equal? shortest (car x)))
					 (map (lambda (x) (car x))
					      (hello-neighbor nodeish datatable))))
		     (short-edge (lookup nodeish short-pred datatable)))
		(insert1d! nodeish (cons short-pred (+ shortest short-edge) pathmap)))))
  )
