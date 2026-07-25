(defmodule karuta
  (export
    (fresh 1)
    (unify 3)
    (deref 2)
    (is-variable 2)
    (int 1)
    (plus 3)
    (divmod 4)
    (call-with-fresh 1)
    (eq 2)
    (conj 1)
    (disj 1)
    (conj 2)
    (disj 2)
    (delay 1)
    (pull 1)
    (start 2)
    (true 1)
    (false 1)
    (t-dee 1)
    (t-dum 1)
    (take-all 1)
    (take 2)
    (deref-query-var 2)
    (deref-query 1)
    (query-variable 3)
    (bind-results 2)
    (run-lazy 2)))

(eval-when-compile
  (defun gensym ()
    (list_to_atom
     (lc ((<- _ (lists:seq 1 128)))
         (+ 96 (rand:uniform 26)))))

  (defun tuple-pattern (size position-of-false)
    (cond
      ((=:= size 0) '())
      ((< 0 size) (cons (if (=:= position-of-false 0) ''false '_)
                        (tuple-pattern (- size 1) (- position-of-false 1))))))

  (defun state-sym+args->arg-names+bindings+case-tuple+checks (state-sym args)
    (let* ((arg-count (length args))
           ((tuple arg-names bindings case-tuple predicate-checks _)
            (lists:foldr
             (lambda (arg-pair acc)
               (let (((tuple name-acc bindings-acc case-acc checks-acc position) acc)
                     ((list pred-name arg-name) arg-pair))
                 (tuple (cons arg-name name-acc)
                        (cons (list arg-name `(karuta:deref ,state-sym ,arg-name))
                              bindings-acc)
                        (cons `(karuta:is-variable ,arg-name ,state-sym)
                              case-acc)
                        (cons `((tuple ,@(tuple-pattern arg-count
                                                        (+ arg-count position)))
                                (when (not (,pred-name ,arg-name)))
                                (fun karuta:false 1))
                              checks-acc)
                        (- position 1))))
             (tuple '() '() '() '() -1)
             args)))
      (tuple arg-names bindings case-tuple predicate-checks))))

(defmacro defpred
  "Assumes that args is a list of lists of two atoms denoting a predicate
  to apply to the argument and the argument's name."
  (`[,name ,args . ,clauses]
   (let* ((state-sym (gensym))
          ((tuple arg-names bindings case-tuple predicate-checks)
           (state-sym+args->arg-names+bindings+case-tuple+checks state-sym args)))
     `(defun ,name ,arg-names
        (lambda (,state-sym)
          (let ,bindings
            (funcall
             (case (tuple ,@case-tuple)
               ,@predicate-checks
               ,@clauses)
             ,state-sym)))))))

(defun is-variable (var bindings)
  (and (is_reference var) (is_map_key var bindings)))

(defun fresh (bindings)
  (let ((var (make_ref)))
    (tuple var (mset bindings var 'unbound))))

(defun deref
  ((state var) (when (is_reference var))
   (case (maps:find var state)
     ((tuple 'ok (tuple 'bound value)) (deref state value))
     (_ var)))
  ((_ value) value))

(defun deref-tuple
  ((state position tuple) (when (=< position (tuple_size tuple)))
   (deref-tuple state
                (+ position 1)
                (setelement position tuple (deref-all state (element position tuple)))))
  ((_ _ tuple) tuple))

(defun deref-map (state m)
  (maps:map (lambda (_ v) (deref-all state v)) m))

(defun deref-all (state var)
  (case (deref state var)
    ((cons h t) (cons (deref-all state h) (deref-all state t)))
    (t (when (is_tuple t)) (deref-tuple state 1 t))
    (m (when (is_map m)) (deref-map state m))
    (something-else something-else)))

(defun deref-query-var (state var-name)
  (deref-all state
    (clj:-> state (mref 'query) (mref var-name))))

(defun deref-query (state)
  (deref-all state (maps:get 'query state #m())))

(defun query-variable (var name goal)
  (lambda (state)
    (let* ((query (maps:get 'query state #m()))
           (next-query (mset query name var))
           (next-state (mset state 'query next-query)))
      (funcall goal next-state))))

(defun unify (state lhs rhs)
  (unify-dereferenced state (deref state lhs) (deref state rhs)))

(defun discard? (state var)
  (=:= (maps:get var state 'not-found) 'discard))

(defun unify-variable (state var value)
  (if (or (discard? state var) (discard? state value))
    (list state)
    (list (mset state var (tuple 'bound value)))))

(defun unify-tuple
  ((state size position left right) (when (=< position size))
   (maybe
     (?= (list next-state)
         (unify state (element position left) (element position right)))
     (unify-tuple next-state size (+ position 1) left right)))
  ((state _ _ _ _) (list state)))

(defun unify-kv
  ((key value (list state) m) (when (is_map_key key m))
    (unify state value (mref m key)))
  ((_ _ _ _) ()))

(defun unify-map (state a b)
  (maps:fold
    (lambda (key value acc) (unify-kv key value acc b))
    (list state)
    a))

(defun same_primitive (a b)
  (andalso
    (orelse
      (andalso (is_atom a) (is_atom b))
      (andalso (is_bitstring a) (is_bitstring b))
      (andalso (is_float a) (is_float b))
      (andalso (is_integer a) (is_integer b))
      (andalso (is_pid a) (is_pid b))
      (andalso (is_port a) (is_port b))
      (andalso (is_reference a) (is_reference b)))
    (=:= a b)))

(defun unify-dereferenced (state a b)
  (cond
    ((same_primitive a b) (list state))
    ((andalso (is_reference a) (is_map_key a state)) (unify-variable state a b))
    ((andalso (is_reference b) (is_map_key b state)) (unify-variable state b a))
    ((andalso (is_map a) (is_map b) (== (map_size a) (map_size b)))
     (unify-map state a b))
    ((andalso (is_tuple a) (is_tuple b) (== (tuple_size a) (tuple_size b)))
      (unify-tuple state (tuple_size a) 1 a b))
    ('true
     (case (tuple a b)
       ((tuple (cons ha ta) (cons hb tb))
        (maybe
          (?= (list next-state) (unify state ha hb))
          (unify next-state ta tb)))
       ((tuple '() '()) (list state))
       (_ (list))))))

(defun mplus
  ((() rhs) rhs)
  (((cons h t) rhs) (cons h (mplus t rhs)))
  ((lhs rhs) (when (is_function lhs))
   (lambda () (mplus rhs (funcall lhs)))))

(defun bind
  ((() _) ())
  (((cons h t) goal) (mplus (funcall goal h) (bind t goal)))
  ((stream goal) (when (is_function stream))
   (lambda () (bind (funcall stream) goal))))

(defun delay (goal)
  (lambda (state)
    (lambda () (funcall goal state))))

(defun pull
  ((()) #(error no-result))
  (((cons h t)) (tuple 'ok h t))
  ((stream) (when (is_function stream)) (pull (funcall stream))))

(defun start
  (((= config (map 'db_port port 'db_address address 'db_root db-root)) goal)
   (let* ((timeout-map (maps:get 'db_timeouts config (map)))
          (connect-timeout (maps:get 'connect timeout-map (* 60 1000)))
          (send-timeout (maps:get 'send timeout-map (* 5 1000)))
          (tcp-opts (list 'binary
                          #(active false)
                          #(packet 0)
                          (tuple 'send_timeout send-timeout))))
     (case (gen_tcp:connect address port tcp-opts connect-timeout)
       ((tuple 'ok socket)
        (funcall goal
                 ; TODO: uncomment this when we add the DB schema check
                 ; (conj
                 ;   (sakura:ask (map) (funcall (fun db-root 'init-payload 0)))
                 ;   goal)
                 (map 'db_config (map 'socket socket
                                      'timeouts timeout-map))))
       ((tuple 'error error) (erlang:error (tuple 'bad_start error))))))
  ((_ goal) (funcall goal #m())))

(defun take-all (stream)
  (case (pull stream)
    ((tuple 'ok res next) (cons res (take-all next)))
    ((tuple 'error 'no-result) '())))

(defun take (n stream)
  (if (=< n 0)
    '()
    (case (pull stream)
      ((tuple 'ok res next) (cons res (take (- n 1) next)))
      ((tuple 'error 'no-result) '()))))

(defun eq (lhs rhs)
  (lambda (state) (unify state lhs rhs)))

(defun next-int (n)
  (if (< n 0)
    (- n)
    (erlang:bnot n)))

(defun every-int (variable current)
  (disj (eq variable current)
        (every-int variable (next-int current))))

(defun int (n)
  (lambda (state)
    (let* ((deref-n (deref state n))
           (goal (if (is-variable deref-n state)
                   (every-int deref-n 0)
                   (eq 'true (is_integer deref-n)))))
      (funcall goal state))))

(defpred plus ((is_integer lhs) (is_integer rhs) (is_integer out))
  ((tuple 'false 'false _) (eq out (+ lhs rhs)))
  ((tuple 'true 'false 'false) (eq lhs (- out rhs)))
  ((tuple 'false 'true 'false) (eq rhs (- out lhs)))
  ((tuple 'true 'true 'false) (conj (int lhs) (plus lhs rhs out))) ; FIXME: generate pairs of operands from the target result
  ((tuple 'true 'false 'true) (conj (int lhs) (plus lhs rhs out)))
  ((tuple 'false 'true 'true) (conj (int rhs) (plus lhs rhs out)))
  ((tuple 'true 'true 'true) (conj (int out) (plus lhs rhs out))))

(defun divmod* (dividend divisor)
  (let ((q (div dividend divisor))
        (r (rem dividend divisor)))
    (if (< r 0)
      (if (> divisor 0)
        (tuple (- q 1) (+ r divisor))
        (tuple (+ q 1) (- r divisor)))
      (tuple q r))))

(defpred divmod ((is_integer dividend)
                 (is_integer divisor)
                 (is_integer quotient)
                 (is_integer remainder))
  ; TODO: implement the other cases
  ((tuple 'true 'false 'false 'false)
   (eq dividend (+ remainder (* divisor quotient)))))

(defun true (state) (list state))
(defun false (_) '())
(defun t-dee (state) (list state))
(defun t-dum (_) '())

(defun call-with-fresh (f)
  (lambda (state)
    (let* (((tuple var new-state) (fresh state))
           (goal (funcall f var)))
      (funcall goal new-state))))

(defun disj (g1 g2)
  (lambda (state) (mplus (funcall g1 state) (funcall g2 state))))

(defun conj (g1 g2)
  (lambda (state) (bind (funcall g1 state) g2)))

(defun disj (goals)
  (lists:foldr
    (lambda (elem acc) (disj (delay elem) acc))
    (fun false 1)
    goals))

(defun conj (goals)
  (lists:foldr
    (lambda (elem acc) (conj (delay elem) acc))
    (fun true 1)
    goals))

(defun bind-results (pattern results)
  (lambda (state)
    (case (pull results)
      ((tuple 'ok head tail)
       (funcall
         (disj (eq pattern head)
               (bind-results pattern tail))
         state))
      ((tuple 'error 'no-result) '()))))

(defun stream-map
  ((_ ()) '())
  ((f (cons h t)) (cons (funcall f h) (stream-map f t)))
  ((f stream) (when (is_function stream))
   (lambda () (stream-map f (funcall stream)))))

(defun run-lazy (config goal)
  (stream-map (fun deref-query 1) (start config goal)))
