;;1| is-list? procedure--
;;;Since a list is defined as basically a nesting doll of pairs ending in the null, all is-list? has to do is check whether it is looking at a pair or a null.  If it is neither, then we know it does not fit the definition of a list can simply return false without any further investigation.			;

(define (is-list? x)
  (cond ((null? x) #t)
	((pair? x) (is-list? (cdr x)))
	(else #f)
	)
  )

(define a (list 1 2 3))
(define b (cons 1 2))
(define c '())
  
(is-list? a)
(is-list? b)
(is-list? c)

a

;;;2| 2.55 in the case of ''abacadabra, car() is returning a quote because the very first quote denotes the symbol following it as a literal list.  From experimenting, it seems that it actually creates a cons for the interpreter to read, where the car is the literal object and the cdr is the empty--when defining a as the cdr of ''(123), a's car becomes the literal and its cdr is the empty.

;;;So in the case of ''abacadabra, we actually have '('abacadabra ()) or '('(abacadabra ()) ()) which is why the car returns the second quote, whereas the cdr would give us abacadabra and the cdr of THAT would give us a ().

;;3| list reverse procedure
;;;While this reverses lists effectively, regardless of whether they are integers or literals, it could be made more robust by actually checking that it is actually taking a list as an argument and, if not, simply returning the argument itself.  This problem also presented an odd issue that seems to be unique to my interpreter installation wherein I cannot simply output the contents of a list directly from a function but rather have to define another symbol as the function on something and to THEN display the contents of that newly defined symbol.  It isn't really problematic; however, it does make testing very slightly more tedious.

(define (myreverse x)
  (define (myreverse-h x xr)
    (if (null? x)
	xr
	(myreverse-h (cdr x) (cons (car x) xr)))
    )
  (myreverse-h x '())
  )
  
(define b (myreverse a))

b

;;4| 2.20 parity filter
;;;I implemented this using two local helper functions--one for evens and one for odds.  This probably could have been implemented with a large, nested conditional, but I felt the local helpers resulted in a much more readable procedure.  With a fairly simple tweak, this can also check whether the list passed is divisible by the integer argument passed.

(define (same-parity x)
  (define (same-parity-eh i o)
    (if (null? i)
	o
	(if (= 0 (modulo(car i) 2))
	    (same-parity-eh (cdr i) (cons (car i) o))
	    (same-parity-eh (cdr i) o))
	))
  (define (same-parity-oh i o)
    (if (null? i)
	o
	(if (= 1 (modulo(car i) 2))
	    (same-parity-oh (cdr i) (cons (car i) o))
	    (same-parity-oh (cdr i) o))
	))
  (if (= 0 (modulo (car x) 2))
      (same-parity-eh x '())
      (same-parity-oh x '())
      )
  )

(define a '(1 2 3 4 5 6))
(define b '(2 3 4 5 6 7))

(define c (same-parity a))
c
(define c (same-parity b))
c

;;5| 2.21
;;;Of note in this one is that the mapping function is clearly a useful and more powerful function than creating bespoke procedures for every single function we want to apply to an entire list.  With that said, the handcrafting of a mapping procedure could be useful in spotting room for improvement (or errors) in the function itself being "mapped".

(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list(cdr items)))))

(define a (square-list '(1 2 3 4)))
a

(define (square-list items)
  (map (lambda (x) (* x x)) items)
  )

(define a (square-list '(1 2 3 4)))
a

;;6| 2.23
;;;This procedure simply takes in another procedure as an argument and a list as a second argument.  First it needs to check for the null case to provide a base case for the recursion and then, if that base case is not met, it simply recurses down the cdr of the passed list.  While this function does actually do what it is supposed to do, it always completes by throwing a bad function object error--I haven't been able to spot why, though.

(define (my-for-each predicate passed-list)
  (if (null? passed-list)
      '()
      ((predicate (car passed-list)) (my-for-each predicate (cdr passed-list)))
   )
  )

(define (testpred x) (eqv? x 1))
(define (displaypred x)
  (display x)
  (newline)
  )

(my-for-each displaypred (list 1 2 3 4))

;;7| 2.24, 2.25, 2.26 -- completed

;;8| 2.27
;;;This was an interesting problem in that it really makes explicit the way functional languages treat arguments--as, unsurprisingly, functions of values.  Keeping in mind that the standard reverse procedure from above works just fine, we have to include the condition that when the argument passed it is actually a list itself, we need to run a reversal function on that argument before continuing our recursion.  Since there is no real difference between a naked argument and an argument passed through a function, we simply do exactly that and pass that argument through a function before continuing the recursion as if the argument was not also a list itself.

;;;A really neat observation that results from this is that when a literal list is passed, the "quote" notation actually is reversed onto the other side of the list as well.  I think this is because the quote notation is an actual symbol as well and so gets treated as, more or less, yet another item in a list.

(define (my-deep-reverse x)
  (define (my-deep-reverse-h x xr)
    (cond 
     ((null? x) xr)
     ((is-list? (car x)) (my-deep-reverse-h (cdr x)(cons (my-deep-reverse (car x)) xr)))
     (else (my-deep-reverse-h (cdr x)(cons (car x) xr)))
     )
    )
  (my-deep-reverse-h x '())
  )

(define a '(1 2 '(3 4) 5))
a
(define b (my-deep-reverse a))
b

;;9| 2.54, eqv?->eq?
;;;I had orignally substituted eqv for eq at every point in my implementation; however, when passing literals, the second if's eq always results in #f.  When I use eqv in that same if procedure, it works properly.  I'm honestly not sure if that has to do with literals or there is some other underlying mechanism.  Just testing eqv? and eq? in UMB Scheme on raw arguments leads me to believe that eq? requires symbols and does not process literals, whereas eqv works on both (i.e., eq? 1 1 returns false, however eq? c d, where both c and d are defined as 1, respectively, returns true; on the other hand, eqv? 1 1 returns true, where as eqv? c d also returns true).

;;;The issue arises that if passed a quote list, eqv seems to become necessary as eq won't return true even if the literals should be interpreted as true.  Actually, talking about this issue with Josh DeCosta, I realized that I was substituting in the wrong direction and changed my usage of eq? to eqv?, as reflected below--however, it is worth noting that eq? works properly in the else cond, but I have come to the conclusion that eqv? is almost entirely superior to eq? and eq? kind of sucks.

(define (my-equal x y)
  (cond
   ((null? x) (null? y))
   ((pair? x) (if (pair? y)
		  (if (eqv? (car x) (car y))
		       (my-equal (cdr x) (cdr y))
		       #f)
		     #f))
   ((pair? y) #f)
   (else (eqv? x y))
    )
  )

(define a (list 1 2 3))
(define b '(1 2 3))
(define c '(1 2))
(define d '())

a b
(my-equal a b)
a c
(my-equal a c)
b c
(my-equal b c)
d d
(my-equal d d)
a d
(my-equal a d)
d b
(my-equal d b)
(my-equal '(2 3 4) '(2 3 4))

;;10|
;;;Had I not implemented my every? the way I did, it would return #t when passed the empty list.  Because I use a helper procedure that gets called inside an empty-list checker, my every? returns false if an empty list is passed to it--it first checks if what was passed is empty and, if it is, it returns false, otherwise it passes that list to a local procedure that equates to #t when it sees a null (which is guaranteed to not be the first element because of the first empty-check) or recurses down the cdr when the predicate on the car equates to #t or equates to #f otherwise.

;;;AND and passing an appended list as an argument will equate to the same value with my implementation.  Since append basically slaps a list onto another, removing the leading list's closing '(), my every? will simply recurse down it until it hits the last '() and return true (or return false if the predicate does not hold at some point in either list.  Similarly, when passed to AND, the two lists will individually equate to #t for reasons stated in the abov paragraph and, since the AND holds, ultimately equate to #t--on the flip side, if one or both equate to #f, the AND will equate to #f due to the nature of AND.

(define (every? pred seq)
  (define (every?-h pred seq)
    (cond
     ((null? seq) #t)
     ((pred (car seq)) (every?-h pred (cdr seq)))
     (else #f)
     )
    )
  (if (null? seq)
      #f
      (every?-h pred seq))
  )

(every? testpred '())
(every? testpred '(1))
(every? testpred '(1 1 0))

;;11| 2.59
;;;I used a modified version of the text's memq.  The book's memq returns a list starting with the first appearance of the passed union-set argument; however, my version simply equates to a boolean depending on the appearance of the union-set argument.

;;;In a similar fashion to the earlier problems, I found the use of a local helper function to be very useful here--until I realized I had been thinking of the intersection of the two sets rather than the union.  Because the principle structure was still sound, I modified the helper function.  Realizing that we are guaranteed the entirety of each individual set will make it into the union, I passed a version of y into the helper that builds the output.  Then, recursing down x until I hit the null base case, I check y for car x--if car x is within y, then I can skip it an move on as I am building my union output using y as a seed.  If, on the other hand, car x is not in y, I append car x onto y when I recurse down x.

;;;A more efficient way to do this would have been to simply append onto y directly until we hit our base case, at which point the function just collapses to the union set.  For similar reasons as above, this would work perfectly fine as we can be sure that any car x we actually want to add to the union set will not have been included in the prior steps and so, even though we check the car x against what is y plus some previous cars of x, we would find no duplicates are output.

(define (memq-b item x)
  (cond
   ((null? x) #f)
   ((eqv? item (car x)) #t)
   (else (memq-b item (cdr x)))
   )
  )

(define (unordered-union-set x y)
  (define (unordered-union-set-h x y z)
    (cond
     ((null? x) z)
     ((memq-b (car x) y) (unordered-union-set-h (cdr x) y  z))
     (else (unordered-union-set-h (cdr x) y (cons (car x) z)))
     )
    )
  (unordered-union-set-h x y y)
  )

(memq-b 2 (list 1 2 3))
(define a (unordered-union-set '(1 2 4) '(0 2 4)))
a

;;12| 2.62
;;;Before passing into the helper, my procedure makes at most two comparisons and so does not actually grow.  Once it moves into the helper function, there are a maximum of 3*(n-1) compares, where n is the total number of set members across all sets.  The factor of 3 comes from the symmetry of the recursion as if a compare holds true at the bottom of the line of conditionals, then necessarily a recursion must hold true higher up the chain of conditionals at some later point (e.g., where car list2 is larger than car list1, later on either car list1 is bigger than car list2 or list1 tests null--similarly, where an else test holds, we are one recursion closer to the first test AND holding and thus skipping the five compares following it).

;;;For the above reasons, my ordered-union-set has a O(n) growth (I also ignore the myreverse function in my analysis above; however, that still would not push the rate of growth out of O(n)).

(define (ordered-union-set list1 list2)
  (define (ordered-union-set-h list1h list2h union)
    (cond
     ((AND (null? list1h)(null? list2h)) (myreverse union))
     ((null? list1h) (ordered-union-set-h list1h (cdr list2h) (cons (car list2h) union)))
     ((null? list2h) (ordered-union-set-h (cdr list1h) list2h (cons (car list1h) union)))
     ((> (car list1h) (car list2h)) (ordered-union-set-h list1h (cdr list2h) (cons (car list2h) union)))
     ((< (car list1h) (car list2h)) (ordered-union-set-h (cdr list1h) list2h (cons (car list1h) union)))
     (else (ordered-union-set-h (cdr list1h) (cdr list2h) (cons (car list1h) union)))
     )
    )
  (cond
   ((> (car list1) (car list2)) (ordered-union-set-h list1 (cdr list2) (list (car list2))))
   ((< (car list1) (car list2)) (ordered-union-set-h (cdr list1) list2 (list (car list1))))
   (else (ordered-union-set-h (cdr list1) (cdr list2) (list (car list1))))
   )
  )

(define a (ordered-union-set '(1 2 3) '(1 4 5)))
a

;;13| remove-val
;;;I didn't think about this one super hard because...this was a ton of problems and the immediate solution to this one was easy enough to implement.  I create a helper function so I can pass a seed '() and then only grow that on recursions where I do not see the value to be filtered.  Finally, because the resulting list is ordered in the reverse, I run it through myreverse before completing the function.

(define (remove-val x thislist)
  (define (remove-val-h x thislist returnlist)
     (cond
      ((null? thislist) (myreverse returnlist))
      ((eqv? x (car thislist)) (remove-val-h x (cdr thislist) returnlist))
      (else (remove-val-h x (cdr thislist) (cons (car thislist) returnlist)))
      )
     )
  (remove-val-h x thislist '())
  )

(define a (remove-val 3 (list 4 5)))
a
(define a (remove-val 3 '(2 3 4 3)))
a
