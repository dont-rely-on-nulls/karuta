(defmodule kanren_test)

(include-lib "eunit/include/eunit.hrl")

(defun is-variable_test_ ()
  (let* ((bound-var (make_ref))
         (unbound-var (make_ref))
         (bindings `#m(,bound-var 42)))
    (list
      (tuple "Returns true when var is a reference AND a key in bindings"
             (_assert (kanren:is-variable bound-var bindings)))
      (tuple "Returns false when var is a reference NOT in bindings"
             (_assertNot (kanren:is-variable unbound-var bindings)))
      (tuple "Returns false when var is not a reference even if present as a map key"
             (_assertNot (kanren:is-variable 'x `#m(x 42)))))))

(defun boundedness_test_ ()
  (let* ((var-in-map (make_ref))
         (var-not-in-map (make_ref))
         (bindings `#m(,var-in-map 42)))
    (list
      (tuple "Returns 'unbound when var is a reference present in bindings"
             (_assertEqual 'unbound (kanren:boundedness var-in-map bindings)))
      (tuple "Returns 'bound when var is a reference NOT in bindings"
             (_assertEqual 'bound (kanren:boundedness var-not-in-map bindings)))
      (tuple "Returns 'bound when var is a non-reference atom"
             (_assertEqual 'bound (kanren:boundedness 'x `#m(x 42)))))))

(defun fresh_test_ ()
  (let* ((existing-var (make_ref))
         (initial-bindings `#m(,existing-var 42))
         ((tuple new-var updated-bindings) (kanren:fresh initial-bindings)))
    (list
      (tuple "Returns a reference as the newly created variable"
             (_assert (is_reference new-var)))
      (tuple "Registers the new variable inside the updated bindings map"
             (_assert (kanren:is-variable new-var updated-bindings)))
      (tuple "Initializes the new variable's value to 'unbound"
             (_assertEqual 'unbound (mref updated-bindings new-var))))))

(defun deref_test_ ()
  (let* ((unbound-var (make_ref))
         (missing-var (make_ref))
         (direct-var (make_ref))
         (chain-start (make_ref))
         (chain-mid (make_ref))
         (unbound-chain-start (make_ref))
         (state `#m(,unbound-var unbound
                    ,direct-var #(bound 42)
                    ,chain-mid #(bound hello)
                    ,chain-start #(bound ,chain-mid)
                    ,unbound-chain-start #(bound ,unbound-var))))
    (list
      (tuple "Returns non-reference ground values unchanged"
             (_assertEqual 'atom-val (kanren:deref state 'atom-val)))
      (tuple "Returns the variable itself when its state is 'unbound"
             (_assertEqual unbound-var (kanren:deref state unbound-var)))
      (tuple "Returns the variable itself when maps:find returns 'error"
             (_assertEqual missing-var (kanren:deref state missing-var)))
      (tuple "Resolves a variable directly bound to a ground value"
             (_assertEqual 42 (kanren:deref state direct-var)))
      (tuple "Walks a chain of bound variables down to the final ground value"
             (_assertEqual 'hello (kanren:deref state chain-start)))
      (tuple "Walks a chain of bound variables ending in an unbound variable"
             (_assertEqual unbound-var (kanren:deref state unbound-chain-start))))))

(defun deref-query-var_test_ ()
  (let* ((unbound-var (make_ref))
         (num-var (make_ref))
         (atom-var (make_ref))
         (chain-var (make_ref))
         (list-var (make_ref))
         (tuple-var (make_ref))
         (map-var (make_ref))
         (nested-var (make_ref))
         (state `#m(,unbound-var unbound
                    ,num-var #(bound 99)
                    ,atom-var #(bound ok)
                    ,chain-var #(bound ,atom-var)
                    ,list-var #(bound (,num-var ,chain-var ,unbound-var))
                    ,tuple-var #(bound #(tag ,num-var ,unbound-var))
                    ,map-var #(bound #m(a ,num-var b ,unbound-var))
                    ,nested-var #(bound (#(item #m(val (,chain-var ,unbound-var)))))
                    query #m(q-unbound ,unbound-var
                             q-ground literal
                             q-chain ,chain-var
                             q-list ,list-var
                             q-tuple ,tuple-var
                             q-map ,map-var
                             q-nested ,nested-var))))
    (list
      (tuple "Returns ground query values unchanged"
             (_assertEqual 'literal (kanren:deref-query-var state 'q-ground)))
      (tuple "Returns unbound variable reference when query variable is unbound"
             (_assertEqual unbound-var (kanren:deref-query-var state 'q-unbound)))
      (tuple "Resolves a chained query variable to its final ground value"
             (_assertEqual 'ok (kanren:deref-query-var state 'q-chain)))
      (tuple "Recursively resolves bound variables inside a list while preserving unbound ones"
             (_assertEqual (list 99 'ok unbound-var)
                           (kanren:deref-query-var state 'q-list)))
      (tuple "Recursively resolves bound variables inside a tuple while preserving unbound ones"
             (_assertEqual (tuple 'tag 99 unbound-var)
                           (kanren:deref-query-var state 'q-tuple)))
      (tuple "Recursively resolves bound variables inside a map while preserving unbound ones"
             (_assertEqual `#m(a 99 b ,unbound-var)
                           (kanren:deref-query-var state 'q-map)))
      (tuple "Deeply resolves nested combinations of lists, tuples, and maps"
             (_assertEqual (list (tuple 'item `#m(val (ok ,unbound-var))))
                           (kanren:deref-query-var state 'q-nested))))))

(defun deref-query_test_ ()
  (let* ((unbound-var (make_ref))
         (bound-var (make_ref))
         (chain-var (make_ref))
         (nested-var (make_ref))
         (state `#m(,unbound-var unbound
                    ,bound-var #(bound 42)
                    ,chain-var #(bound ,bound-var)
                    ,nested-var #(bound (,chain-var #(pair ,unbound-var ok)))
                    query #m(x ,bound-var
                             y ,unbound-var
                             z ,nested-var
                             const static))))
    (list
      (tuple "Returns an empty map when 'query key is missing from state"
             (_assertEqual #m() (kanren:deref-query `#m(,bound-var #(bound 42)))))
      (tuple "Returns an empty map when 'query map is empty"
             (_assertEqual #m() (kanren:deref-query `#m(query #m()))))
      (tuple "Resolves all bound, chained, nested, and unbound entries in the query map"
             (_assertEqual `#m(x 42
                               y ,unbound-var
                               z (42 #(pair ,unbound-var ok))
                               const static)
                           (kanren:deref-query state))))))

(defun unify_test_ ()
  (let* ((x (make_ref))
         (y (make_ref))
         (z (make_ref))
         (discard-var (make_ref))
         (unregistered-ref (make_ref))
         (state `#m(,x unbound
                    ,y unbound
                    ,z #(bound 100)
                    ,discard-var discard)))
    (list
      ;; Primitives & identity
      (tuple "Succeeds with unchanged state when unifying identical primitives"
             (_assertEqual (list state) (kanren:unify state 42 42)))
      (tuple "Fails with empty list when unifying different primitives"
             (_assertEqual '() (kanren:unify state 'foo 'bar)))
      (tuple "Fails when unifying integer and float with same numeric value"
             (_assertEqual '() (kanren:unify state 1 1.0)))
      (tuple "Succeeds with unchanged state when unifying an unbound variable with itself"
             (_assertEqual (list state) (kanren:unify state x x)))

      ;; Variable binding
      (tuple "Binds an unbound variable on the left to a value on the right"
             (_assertEqual (list (mset state x #(bound hello)))
                           (kanren:unify state x 'hello)))
      (tuple "Binds an unbound variable on the right to a value on the left"
             (_assertEqual (list (mset state x #(bound hello)))
                           (kanren:unify state 'hello x)))
      (tuple "Binds two distinct unbound variables together"
             (_assertEqual (list (mset state x `#(bound ,y)))
                           (kanren:unify state x y)))
      (tuple "Dereferences bound variables before unifying"
             (_assertEqual (list (mset state x #(bound 100)))
                           (kanren:unify state x z)))
      (tuple "Fails when bound variable value conflicts with target value"
             (_assertEqual '() (kanren:unify state z 999)))
      (tuple "Fails when unifying with a reference not registered in state"
             (_assertEqual '() (kanren:unify state unregistered-ref 42)))

      ;; Discard variables
      (tuple "Succeeds without mutating state when left variable is 'discard"
             (_assertEqual (list state) (kanren:unify state discard-var 42)))
      (tuple "Succeeds without mutating state when right variable is 'discard"
             (_assertEqual (list state) (kanren:unify state 42 discard-var)))

      ;; Lists
      (tuple "Unifies two empty lists"
             (_assertEqual (list state) (kanren:unify state '() '())))
      (tuple "Unifies elements across matching lists and binds variables"
             (_assertEqual (list (mset (mset state x #(bound 1)) y #(bound 2)))
                           (kanren:unify state (list x 2) (list 1 y))))
      (tuple "Fails when lists have different lengths"
             (_assertEqual '() (kanren:unify state (list 1 2) (list 1 2 3))))
      (tuple "Fails when an element inside a list fails to unify"
             (_assertEqual '() (kanren:unify state (list x 2) (list 1 99))))
      ;; Tuples
      (tuple "Unifies elements across matching tuples and binds variables"
             (_assertEqual (list (mset (mset state x #(bound a)) y #(bound b)))
                           (kanren:unify state (tuple 'pair x 'b) (tuple 'pair 'a y))))
      (tuple "Fails when tuples have different sizes"
             (_assertEqual '() (kanren:unify state (tuple 1 2) (tuple 1 2 3))))
      (tuple "Fails when an element inside a tuple fails to unify"
             (_assertEqual '() (kanren:unify state (tuple 'ok x) (tuple 'error 1))))
      ;; Maps
      (tuple "Unifies values across maps with identical keys and binds variables"
             (let ((map-a `#m(a ,x b 20))
                   (map-b `#m(a 10 b ,y))
                   (expected (list (mset (mset state x #(bound 10)) y #(bound 20)))))
               (_assertEqual expected (kanren:unify state map-a map-b))))
      (tuple "Fails when maps have different sizes"
             (_assertEqual '() (kanren:unify state #m(a 1) #m(a 1 b 2))))
      (tuple "Fails when maps have the same size but different keys"
             (_assertEqual '() (kanren:unify state #m(a 1) #m(b 1)))))))

(defun bind_test_ ()
  (let ((double-goal (lambda (s) (list s (* s 2))))
        (fail-goal (lambda (_) '()))
        (inc-goal (lambda (s) (list (+ s 1)))))
    (list
     (tuple "Returns empty list when stream is empty regardless of goal"
            (_assertEqual '() (kanren:bind '() double-goal)))
     (tuple "Applies goal to a single-element stream"
            (_assertEqual '(5 10) (kanren:bind '(5) double-goal)))
     (tuple "Flat-maps goal across multiple stream elements and merges results via inc-goal"
            (_assertEqual '(2 3 4) (kanren:bind '(1 2 3) inc-goal)))
     (tuple "Returns empty list when goal fails for every element in stream"
            (_assertEqual '() (kanren:bind '(1 2 3) fail-goal)))
     (tuple "Wraps suspended function streams in a new thunk without forcing them early"
            (let* ((suspended (lambda () '(10 20)))
                   (result (kanren:bind suspended inc-goal)))
              (_assert (is_function result))))
     (tuple "Evaluates bound goal when the returned suspended thunk is forced"
            (let* ((suspended (lambda () '(10 20)))
                   (result (kanren:bind suspended inc-goal)))
              (_assertEqual '(11 21) (funcall result)))))))

(defun ifte_test_ ()
  (let ((succeed-many (lambda (s) (list `#(first ,s) `#(second ,s))))
        (succeed-lazy (lambda (s) (lambda () (lambda () (list `#(lazy ,s))))))
        (succeed-lazy-tail (lambda (s) (cons `#(first ,s) (lambda () (list `#(second ,s))))))
        (fail-goal (lambda (_) '()))
        (fail-lazy (lambda (_) (lambda () (lambda () '()))))
        (then-goal (lambda (s) (list `#(then ,s))))
        (else-goal (lambda (s) (list `#(else ,s)))))
    (list
      (tuple "Runs 'then' goal across ALL results when 'test' goal produces multiple states"
             (_assertEqual '(#(then #(first init)) #(then #(second init)))
                           (funcall (kanren:ifte succeed-many then-goal else-goal) 'init)))
      (tuple "Unwraps nested suspended thunks in 'test' via pull and runs 'then' when a result appears"
             (_assertEqual '(#(then #(lazy init)))
                           (funcall (kanren:ifte succeed-lazy then-goal else-goal) 'init)))
      (tuple "Preserves suspended tail stream from pull when binding 'then' over remaining results"
             (let* (((cons head tail) (funcall (kanren:ifte succeed-lazy-tail then-goal else-goal) 'init))
                    (expected-head #(then #(first init)))
                    (ok (andalso (=:= head expected-head) (is_function tail))))
               (_assert ok)))
      (tuple "Falls back to 'else' goal with original state when 'test' fails immediately"
             (_assertEqual '(#(else init))
                           (funcall (kanren:ifte fail-goal then-goal else-goal) 'init)))
      (tuple "Falls back to 'else' goal with original state when nested suspended thunks yield no results"
             (_assertEqual '(#(else init))
                           (funcall (kanren:ifte fail-lazy then-goal else-goal) 'init))))))
