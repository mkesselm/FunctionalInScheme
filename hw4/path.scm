;;PART_2

;; read-file produces a list whose elements are the expressions in the file.
(begin 
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

;; My implementation is actually Bellman Ford, which I chose because it seems to me to basically be what the instructions called for anyway and I am also familiar with it.

;; Gather list of verticex pairs and edge weight into a list of pairs, the car of which is a unique vertex and the cdr of which is initialized to a pair containing a cost of "-" and a "through-node" of "init"--rather than use infinity, I use "-" as an "unexplored" value.

;; (define (collect-vertices table)
;;   (cond ((null? table) '())
;; 	((eq? (car table) '*table*) (collect-vertices (cdr table)))
;; 	(else (cons (list (caar table)) (collect-vertices (cdr table)))))
;;   )

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



;; create shortest path to all nodes through memoization
;; get caadr of pathmap
;;  get its neighbors
;;   for each neighbor, if path to neighbor + path to source is less than neighbor's current 1d table entry's car, enter node entry's car + lookup of node and neighbor in datatable into neighbor's car and node into cdr of neighbor's current 1d table entry
;; do this to end of pathmap
;; repeat (length pathmap) - 1 times

(define (massage-node node neighbor-list cost-table)
  (let* ((bid (+ (car (lookup1d node cost-table)) (lookup node (caar neighbor-list) datatable)))
	 (neighbor (caar neighbor-list))
	 (current (car (lookup1d (caar neighbor-list) cost-table)))
	 (edge (lookup node (caar neighbor-list) datatable))
	 (memo (cons bid node)))
    (cond ((null? (cdr neighbor-list))
	   (if (or (equal? current '-) (> current bid))
	       (insert1d! neighbor memo cost-table)))
	  ((equal? current '-)
	   (insert1d! neighbor memo cost-table)
	   (massage-node node (cdr neighbor-list) cost-table))
	  ((> current bid)
	   (insert1d! neighbor memo cost-table)
	   (massage-node node (cdr neighbor-list) cost-table))
	  (else (massage-node node (cdr neighbor-list) cost-table))))
  )

(define (easy-list cost-table)
  (if (null? cost-table)
      '()
      (cons (caar cost-table) (easy-list (cdr cost-table))))
  )

(define quicklist (easy-list (cdr pathmap)))

(define (relax-edges list)
  (if (null? list)
      '()
      (let* ((cost (car (lookup1d (car list) pathmap)))
	     (tail (cdr list))
	     (node (car list))
	     (pred (cdr (lookup1d (car list) pathmap)))
	     (neighbors (hello-neighbor (car list) datatable))
	     )
	(cond ((not neighbors) (relax-edges tail))
	      ((equal? '- cost) (relax-edges tail))
	      (else (massage-node node neighbors pathmap)
		    (relax-edges (cdr list))))))
  )

(define V (- (length pathmap) 1))

;; repeating the algorithm to find paths
(define (sauna count)
  (if (>= count 1)
      (let ((iter (- count 1)))
	(relax-edges quicklist)
	(sauna (- count 1)))
      '())
  )

(sauna V)

;; the pathmap is now complete and we can trace our steps back from the target

(define (traceback cost-table target)
  (let* ((next (cdr (lookup1d target cost-table))))
  (if (equal? 'start next)
      (cons next '())
      (cons target (traceback cost-table next))))
  )

;; generic cost function that will give you the shortest path from 'start to any node in the graph that is reachable--NB, nodes with no inbound edges report back as unreachable as they may only be originating nodes

(define (cost node)
  (if (lookup1d node pathmap)
      (if (equal? 'empty (cdr (lookup1d node pathmap)))
	  (error "It looks like this node has no inbound edges! Please choose a different node.")
	  (cons
	   (car (lookup1d node pathmap))
	   (reverse (traceback pathmap node))))
      (error "Please choose a target node that is a member of the graph."))
  )

(display (cons
	   (car (lookup1d 'end pathmap))
	   (reverse (traceback pathmap 'end))))
)

(define (naive-cost nodeish)
  (cond ((eq? nodeish 'end) '0)
	((list? nodeish) (if (car (map list? nodeish))
			     (min (map naive-cost nodeish))
			     (lookup (car nodeish) (cadr nodeish) datatable)))
	(else (let* (children (hello-neighbor nodeish))
		(if children
		    (+ naive-cost children)
		    (99999)))))
  )
