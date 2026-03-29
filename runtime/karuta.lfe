(defmodule karuta
  (export
    (fresh 1)
    (unify 3)
    (discard 1)
    (deref 2)
    (is_variable 2)
    (nat 1)
    (call_with_fresh 1)
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
    (take_all 1)
    (deref_query_var 2)
    (deref_query 1)
    (query_variable 3)
    (bind_results 2)
    (run_lazy 2)))

(defun is_variable (var bindings)
  (and (is_reference var) (is_map_key var bindings)))

(defun fresh (bindings)
  (let ((var (make_ref)))
    #(var (maps:put var 'unbound bindings))))

(defun discard (bindings)
  (case (maps:find 'discard bindings)
    (#(ok discard-var)
      #(discard-var bindings))
    ('error
      (let ((discard-var (make_ref)))
        #(discard-var
          (maps:put discard-var 'discard (maps:put 'discard discard-var bindings)))))))

(defun deref
  ((state var) (when (is_reference var))
    (let ((found (maps:find var state)))
      (if (and (is_tuple found) (=:= (element 1 found) 'ok))
        (let ((bound-pair (element 2 found)))
          (if (and (is_tuple bound-pair) (=:= (element 1 bound-pair) 'bound))
            (deref state (element 2 bound-pair))
            var))
        var)))
  ((_ value) value))

(defun deref_tuple
  ((state position tuple) (when (=< position (tuple_size tuple)))
    (deref_tuple state
                (+ position 1)
                (setelement position tuple (deref_all state (element position tuple)))))
  ((_ _ tuple) tuple))

(defun deref_map (state m)
  (maps:map (lambda (_ v) (deref_all state v)) m))

(defun deref_all (state var)
  (let ((val (deref state var)))
    (case val
      (() ())
      ((cons h t) (cons (deref_all state h) (deref_all state t)))
      (_
        (if (is_tuple val)
          (deref_tuple state 1 val)
          (if (is_map val)
            (deref_map state val)
            val))))))

(defun deref_query_var (state var-name)
  (let ((query (maps:get 'query state (maps:new))))
    (deref_all state (maps:get var-name query))))

(defun deref_query (state)
  (deref_all state (maps:get 'query state (maps:new))))

(defun query_variable (var name goal)
  (lambda (state)
    (let* ((query (maps:get 'query state (maps:new)))
           (next-query (maps:put name var query))
           (next-state (maps:put 'query next-query state)))
      (funcall goal next-state))))

(defun unify (state lhs rhs)
  (unify_dereferenced state (deref state lhs) (deref state rhs)))

(defun unify_variable (state var value)
  (cond
    ((=:= (maps:get var state 'not-found) 'discard) (list state))
    ((=:= (maps:get value state 'not-found) 'discard) (list state))
    ('true (list (maps:put var #(bound value) state)))))

(defun unify_tuple
  ((state size position left right) (when (=< position size))
    (case (unify state (element position left) (element position right))
      ((cons next-state ())
        (unify_tuple next-state size (+ position 1) left right))
      (_ ())))
  ((state _ _ _ _) (list state)))

(defun unify_kv
  ((key value (cons state ()) m) (when (is_map_key key m))
    (unify state value (maps:get key m)))
  ((_ _ _ _) ()))

(defun unify_map (state a b)
  (maps:fold
    (lambda (key value acc) (unify_kv key value acc b))
    (list state)
    a))

(defun same_primitive (a b)
  (and
    (or
      (and (is_atom a) (is_atom b))
      (and (is_bitstring a) (is_bitstring b))
      (and (is_float a) (is_float b))
      (and (is_integer a) (is_integer b))
      (and (is_pid a) (is_pid b))
      (and (is_port a) (is_port b))
      (and (is_reference a) (is_reference b)))
    (=:= a b)))

(defun unify_dereferenced (state a b)
  (cond
    ((same_primitive a b)
      (list state))
    ((and (is_reference a) (is_map_key a state))
      (unify_variable state a b))
    ((and (is_reference b) (is_map_key b state))
      (unify_variable state b a))
    ((and (is_list a) (is_list b))
      (case a
        ((cons ha ta)
          (case b
            ((cons hb tb)
              (case (unify state ha hb)
                ((cons next-state ()) (unify next-state ta tb))
                (_ ())))
            (_ ())))
        (()
          (case b
            (() (list state))
            (_ ())))
        (_ ())))
    ((and (is_map a) (is_map b) (== (map_size a) (map_size b)))
      (unify_map state a b))
    ((and (is_tuple a) (is_tuple b) (== (tuple_size a) (tuple_size b)))
      (unify_tuple state (tuple_size a) 1 a b))
    ('true ())))

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
  ((()) #(error no_result))
  (((cons h t)) #(ok h t))
  ((stream) (when (is_function stream))
    (pull (funcall stream))))

(defun start (config goal)
  (if (and (is_map_key 'db_port config)
           (is_map_key 'db_address config)
           (is_map_key 'db_timeouts config))
    (let* ((port (maps:get 'db_port config))
           (address (maps:get 'db_address config))
           (timeout-map (maps:get 'db_timeouts config))
           (connect-timeout (maps:get 'connect timeout-map (* 60 1000)))
           (send-timeout (maps:get 'send timeout-map (* 5 1000)))
           (tcp-opts (list 'binary #(packet 0) #(send_timeout send-timeout))))
      (let ((conn (gen_tcp:connect address port tcp-opts connect-timeout)))
        (if (and (is_tuple conn) (=:= (element 1 conn) 'ok))
          (let* ((socket (element 2 conn))
                 (db-config (maps:put 'timeouts timeout-map (maps:put 'socket socket (maps:new))))
                 (state (maps:put 'db_config db-config (maps:new))))
            (funcall goal state))
          (erlang:error #(bad_start (element 2 conn))))))
    (funcall goal (maps:new))))

(defun take_all (stream)
  (let ((pulled (pull stream)))
    (if (and (is_tuple pulled) (=:= (element 1 pulled) 'ok))
      (cons (element 2 pulled) (take_all (element 3 pulled)))
      ())))

(defun eq (lhs rhs)
  (lambda (state) (unify state lhs rhs)))

(defun nat (n)
  (lambda (state)
    (let* ((deref-n (deref state n))
           (goal (conj
                   (eq 'true (is_integer deref-n))
                   (eq 'true (=< 0 deref-n)))))
      (funcall goal state))))

(defun true (state) (list state))
(defun false (_) ())

(defun call_with_fresh (f)
  (lambda (state)
    (let* ((pair (fresh state))
           (var (element 1 pair))
           (new-state (element 2 pair))
           (goal (funcall f var)))
      (funcall goal new-state))))

(defun disj (g1 g2)
  (lambda (state) (mplus (funcall g1 state) (funcall g2 state))))

(defun conj (g1 g2)
  (lambda (state) (bind (funcall g1 state) g2)))

(defun disj (goals)
  (lists:foldr
    (lambda (elem acc) (disj (delay elem) acc))
    (lambda (state) (false state))
    goals))

(defun conj (goals)
  (lists:foldr
    (lambda (elem acc) (conj (delay elem) acc))
    (lambda (state) (true state))
    goals))

(defun bind_results (pattern results)
  (lambda (state)
    (let ((pulled (pull results)))
      (if (and (is_tuple pulled) (=:= (element 1 pulled) 'ok))
        (let ((mplus-res (mplus
                           (eq pattern (element 2 pulled))
                           (delay (bind_results pattern (element 3 pulled))))))
          (funcall mplus-res state))
        ()))))

(defun stream_map
  ((f ()) ())
  ((f (cons h t)) (cons (funcall f h) (stream_map f t)))
  ((f stream) (when (is_function stream))
    (lambda () (stream_map f (funcall stream)))))

(defun run_lazy (config goal)
  (stream_map (lambda (s) (deref_query s)) (start config goal)))
