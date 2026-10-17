;; Karuta runtime for the WebAssembly target: a WasmGC port of karuta.lfe.
;;
;; This file only holds module fields. The compiler splices it into every
;; program it emits, next to the program's predicates, its atom table
;; ($out_base and the data segment) and the exported memory.
;;
;; Terms are eqrefs: atoms are i31 ids (sorted by name, -1 is nil), integers
;; are $Int, logic variables are $Var and functors are $Fun. Goals and streams
;; are defunctionalized: a goal is a tagged struct interpreted by $eval and an
;; immature stream is a tagged thunk forced by $force, following the
;; lambdas in karuta.lfe one to one so the search order is the same.

(rec
  (type $Env (array (mut eqref)))
  (type $Pred (func (param (ref $Env)) (result (ref $Goal))))
  (type $Int (struct (field $value i64)))
  (type $Var (struct (field $id i32)))
  (type $Cons (struct (field $head eqref) (field $tail eqref)))
  (type $Fun (struct (field $name i32) (field $args (ref $Env))))
  (type $Bind (struct (field $id i32) (field $value eqref) (field $next (ref null $Bind))))
  (type $Query (struct (field $name i32) (field $var eqref) (field $next (ref null $Query))))
  (type $State (struct (field $subst (ref null $Bind)) (field $counter i32) (field $query (ref null $Query))))

  ;; Goal tags: 0 true, 1 false, 2 eq, 3 conj, 4 disj, 5 delay, 6 fresh,
  ;; 7 query-variable, 8 ifte, 10 and up are the arithmetic builtins.
  (type $Goal (sub (struct (field $tag i32))))
  (type $GTerms (sub final $Goal (struct (field $tag i32) (field $args (ref $Env)))))
  (type $GPair (sub final $Goal (struct (field $tag i32) (field $lhs (ref $Goal)) (field $rhs (ref $Goal)))))
  (type $GDelay (sub final $Goal (struct (field $tag i32) (field $goal (ref $Goal)))))
  (type $GFresh (sub final $Goal (struct (field $tag i32) (field $body (ref $Pred)) (field $env (ref $Env)))))
  (type $GQuery (sub final $Goal (struct (field $tag i32) (field $name i32) (field $var eqref) (field $goal (ref $Goal)))))
  (type $GIfte (sub final $Goal (struct (field $tag i32) (field $test (ref $Goal)) (field $then (ref $Goal)) (field $else (ref $Goal)))))

  ;; Stream tags: 0 empty, 1 cons, 2 delayed goal, 3 delayed mplus, 4 delayed bind.
  (type $Stream (sub (struct (field $tag i32))))
  (type $SCons (sub final $Stream (struct (field $tag i32) (field $state (ref $State)) (field $next (ref $Stream)))))
  (type $SApply (sub final $Stream (struct (field $tag i32) (field $goal (ref $Goal)) (field $state (ref $State)))))
  (type $SMplus (sub final $Stream (struct (field $tag i32) (field $lhs (ref $Stream)) (field $rhs (ref $Stream)))))
  (type $SBind (sub final $Stream (struct (field $tag i32) (field $stream (ref $Stream)) (field $goal (ref $Goal)))))
)

(global $nil (ref i31) (ref.i31 (i32.const -1)))
(global $g_true (ref $Goal) (struct.new $Goal (i32.const 0)))
(global $g_false (ref $Goal) (struct.new $Goal (i32.const 1)))
(global $s_nil (ref $Stream) (struct.new $Stream (i32.const 0)))
(global $out_ptr (mut i32) (i32.const 0))

;; Terms

(func $int (param $n i64) (result (ref $Int))
  (struct.new $Int (local.get $n)))

(func $is_int (param $t eqref) (result i32)
  (ref.test (ref $Int) (local.get $t)))

(func $is_var (param $t eqref) (result i32)
  (ref.test (ref $Var) (local.get $t)))

(func $iv (param $t eqref) (result i64)
  (struct.get $Int $value (ref.cast (ref $Int) (local.get $t))))

;; Whether T is an integer within [Lo, Hi]
(func $int_in (param $t eqref) (param $lo i64) (param $hi i64) (result i32)
  (if (result i32) (call $is_int (local.get $t))
    (then
      (i32.and
        (i64.ge_s (call $iv (local.get $t)) (local.get $lo))
        (i64.le_s (call $iv (local.get $t)) (local.get $hi))))
    (else (i32.const 0))))

(func $abs (param $n i64) (result i64)
  (select (i64.sub (i64.const 0) (local.get $n)) (local.get $n)
    (i64.lt_s (local.get $n) (i64.const 0))))

(func $lookup (param $id i32) (param $s (ref null $Bind)) (result (ref null $Bind))
  (loop $scan
    (if (ref.is_null (local.get $s)) (then (return (ref.null $Bind))))
    (if (i32.eq (struct.get $Bind $id (local.get $s)) (local.get $id))
      (then (return (local.get $s))))
    (local.set $s (struct.get $Bind $next (local.get $s)))
    (br $scan))
  (unreachable))

(func $walk (param $t eqref) (param $s (ref null $Bind)) (result eqref)
  (local $b (ref null $Bind))
  (loop $again
    (if (call $is_var (local.get $t))
      (then
        (local.set $b
          (call $lookup
            (struct.get $Var $id (ref.cast (ref $Var) (local.get $t)))
            (local.get $s)))
        (if (ref.is_null (local.get $b)) (then (return (local.get $t))))
        (local.set $t (struct.get $Bind $value (local.get $b)))
        (br $again))))
  (local.get $t))

(func $same (param $a eqref) (param $b eqref) (result i32)
  (if (ref.eq (local.get $a) (local.get $b)) (then (return (i32.const 1))))
  (if (i32.and (call $is_int (local.get $a)) (call $is_int (local.get $b)))
    (then (return (i64.eq (call $iv (local.get $a)) (call $iv (local.get $b))))))
  (if (i32.and (call $is_var (local.get $a)) (call $is_var (local.get $b)))
    (then
      (return
        (i32.eq
          (struct.get $Var $id (ref.cast (ref $Var) (local.get $a)))
          (struct.get $Var $id (ref.cast (ref $Var) (local.get $b)))))))
  (i32.const 0))

(func $extend (param $s (ref $State)) (param $var eqref) (param $value eqref) (result (ref $State))
  (struct.new $State
    (struct.new $Bind
      (struct.get $Var $id (ref.cast (ref $Var) (local.get $var)))
      (local.get $value)
      (struct.get $State $subst (local.get $s)))
    (struct.get $State $counter (local.get $s))
    (struct.get $State $query (local.get $s))))

(func $unify (param $s (ref $State)) (param $a eqref) (param $b eqref) (result (ref null $State))
  (local $sub (ref null $Bind))
  (local $fa (ref null $Fun))
  (local $fb (ref null $Fun))
  (local $ca (ref null $Cons))
  (local $cb (ref null $Cons))
  (local $r (ref null $State))
  (local $i i32)
  (local $n i32)
  (local.set $sub (struct.get $State $subst (local.get $s)))
  (local.set $a (call $walk (local.get $a) (local.get $sub)))
  (local.set $b (call $walk (local.get $b) (local.get $sub)))
  (if (call $same (local.get $a) (local.get $b)) (then (return (local.get $s))))
  (if (call $is_var (local.get $a))
    (then (return (call $extend (local.get $s) (local.get $a) (local.get $b)))))
  (if (call $is_var (local.get $b))
    (then (return (call $extend (local.get $s) (local.get $b) (local.get $a)))))
  (if (i32.and (ref.test (ref $Fun) (local.get $a)) (ref.test (ref $Fun) (local.get $b)))
    (then
      (local.set $fa (ref.cast (ref $Fun) (local.get $a)))
      (local.set $fb (ref.cast (ref $Fun) (local.get $b)))
      (local.set $n (array.len (struct.get $Fun $args (local.get $fa))))
      (if (i32.or
            (i32.ne (struct.get $Fun $name (local.get $fa)) (struct.get $Fun $name (local.get $fb)))
            (i32.ne (local.get $n) (array.len (struct.get $Fun $args (local.get $fb)))))
        (then (return (ref.null $State))))
      (local.set $r (local.get $s))
      (loop $args
        (if (i32.lt_u (local.get $i) (local.get $n))
          (then
            (local.set $r
              (call $unify (ref.as_non_null (local.get $r))
                (array.get $Env (struct.get $Fun $args (local.get $fa)) (local.get $i))
                (array.get $Env (struct.get $Fun $args (local.get $fb)) (local.get $i))))
            (if (ref.is_null (local.get $r)) (then (return (ref.null $State))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $args))))
      (return (local.get $r))))
  (if (i32.and (ref.test (ref $Cons) (local.get $a)) (ref.test (ref $Cons) (local.get $b)))
    (then
      (local.set $ca (ref.cast (ref $Cons) (local.get $a)))
      (local.set $cb (ref.cast (ref $Cons) (local.get $b)))
      (local.set $r
        (call $unify (local.get $s)
          (struct.get $Cons $head (local.get $ca))
          (struct.get $Cons $head (local.get $cb))))
      (if (ref.is_null (local.get $r)) (then (return (ref.null $State))))
      (return
        (call $unify (ref.as_non_null (local.get $r))
          (struct.get $Cons $tail (local.get $ca))
          (struct.get $Cons $tail (local.get $cb))))))
  (ref.null $State))

;; Goal constructors

(func $mk_conj (param $l (ref $Goal)) (param $r (ref $Goal)) (result (ref $Goal))
  (struct.new $GPair (i32.const 3) (local.get $l) (local.get $r)))

(func $mk_disj (param $l (ref $Goal)) (param $r (ref $Goal)) (result (ref $Goal))
  (struct.new $GPair (i32.const 4) (local.get $l) (local.get $r)))

(func $mk_delay (param $g (ref $Goal)) (result (ref $Goal))
  (struct.new $GDelay (i32.const 5) (local.get $g)))

(func $mk_fresh (param $body (ref $Pred)) (param $env (ref $Env)) (result (ref $Goal))
  (struct.new $GFresh (i32.const 6) (local.get $body) (local.get $env)))

(func $mk_query (param $name i32) (param $var eqref) (param $g (ref $Goal)) (result (ref $Goal))
  (struct.new $GQuery (i32.const 7) (local.get $name) (local.get $var) (local.get $g)))

(func $mk_ifte (param $t (ref $Goal)) (param $a (ref $Goal)) (param $b (ref $Goal)) (result (ref $Goal))
  (struct.new $GIfte (i32.const 8) (local.get $t) (local.get $a) (local.get $b)))

(func $prim1 (param $op i32) (param $a eqref) (result (ref $Goal))
  (struct.new $GTerms (local.get $op) (array.new_fixed $Env 1 (local.get $a))))

(func $prim2 (param $op i32) (param $a eqref) (param $b eqref) (result (ref $Goal))
  (struct.new $GTerms (local.get $op) (array.new_fixed $Env 2 (local.get $a) (local.get $b))))

(func $prim3 (param $op i32) (param $a eqref) (param $b eqref) (param $c eqref) (result (ref $Goal))
  (struct.new $GTerms (local.get $op)
    (array.new_fixed $Env 3 (local.get $a) (local.get $b) (local.get $c))))

(func $prim4 (param $op i32) (param $a eqref) (param $b eqref) (param $c eqref) (param $d eqref) (result (ref $Goal))
  (struct.new $GTerms (local.get $op)
    (array.new_fixed $Env 4 (local.get $a) (local.get $b) (local.get $c) (local.get $d))))

(func $eq (param $a eqref) (param $b eqref) (result (ref $Goal))
  (call $prim2 (i32.const 2) (local.get $a) (local.get $b)))

(func $eq0 (param $a eqref) (result (ref $Goal))
  (call $eq (local.get $a) (call $int (i64.const 0))))

;; Builtins callable from Karuta code (the `karuta` module)

(func $karuta.t-dee (type $Pred) (param $args (ref $Env)) (result (ref $Goal))
  (global.get $g_true))

(func $karuta.t-dum (type $Pred) (param $args (ref $Env)) (result (ref $Goal))
  (global.get $g_false))

(func $karuta.eq (type $Pred) (param $args (ref $Env)) (result (ref $Goal))
  (struct.new $GTerms (i32.const 2) (local.get $args)))

(func $karuta.int (type $Pred) (param $args (ref $Env)) (result (ref $Goal))
  (struct.new $GTerms (i32.const 10) (local.get $args)))

(func $karuta.nat (type $Pred) (param $args (ref $Env)) (result (ref $Goal))
  (struct.new $GTerms (i32.const 12) (local.get $args)))

(func $karuta.leq (type $Pred) (param $args (ref $Env)) (result (ref $Goal))
  (struct.new $GTerms (i32.const 18) (local.get $args)))

(func $karuta.lt (type $Pred) (param $args (ref $Env)) (result (ref $Goal))
  (struct.new $GTerms (i32.const 19) (local.get $args)))

(func $karuta.plus (type $Pred) (param $args (ref $Env)) (result (ref $Goal))
  (struct.new $GTerms (i32.const 20) (local.get $args)))

(func $karuta.mult (type $Pred) (param $args (ref $Env)) (result (ref $Goal))
  (struct.new $GTerms (i32.const 21) (local.get $args)))

(func $karuta.divmod (type $Pred) (param $args (ref $Env)) (result (ref $Goal))
  (struct.new $GTerms (i32.const 22) (local.get $args)))

;; minus(L, R, O) = plus(R, O, L)
(func $karuta.minus (type $Pred) (param $args (ref $Env)) (result (ref $Goal))
  (call $prim3 (i32.const 20)
    (array.get $Env (local.get $args) (i32.const 1))
    (array.get $Env (local.get $args) (i32.const 2))
    (array.get $Env (local.get $args) (i32.const 0))))

;; neg(N, M) = plus(N, M, 0)
(func $karuta.neg (type $Pred) (param $args (ref $Env)) (result (ref $Goal))
  (call $prim3 (i32.const 20)
    (array.get $Env (local.get $args) (i32.const 0))
    (array.get $Env (local.get $args) (i32.const 1))
    (call $int (i64.const 0))))

;; Streams

(func $unit (param $s (ref $State)) (result (ref $Stream))
  (struct.new $SCons (i32.const 1) (local.get $s) (global.get $s_nil)))

(func $tag (param $st (ref $Stream)) (result i32)
  (struct.get $Stream $tag (local.get $st)))

(func $mplus (param $a (ref $Stream)) (param $b (ref $Stream)) (result (ref $Stream))
  (local $c (ref null $SCons))
  (if (i32.eqz (call $tag (local.get $a))) (then (return (local.get $b))))
  (if (i32.eq (call $tag (local.get $a)) (i32.const 1))
    (then
      (local.set $c (ref.cast (ref $SCons) (local.get $a)))
      (return
        (struct.new $SCons (i32.const 1)
          (struct.get $SCons $state (local.get $c))
          (call $mplus (struct.get $SCons $next (local.get $c)) (local.get $b))))))
  (struct.new $SMplus (i32.const 3) (local.get $a) (local.get $b)))

(func $bind (param $st (ref $Stream)) (param $g (ref $Goal)) (result (ref $Stream))
  (local $c (ref null $SCons))
  (if (i32.eqz (call $tag (local.get $st))) (then (return (global.get $s_nil))))
  (if (i32.eq (call $tag (local.get $st)) (i32.const 1))
    (then
      (local.set $c (ref.cast (ref $SCons) (local.get $st)))
      (return
        (call $mplus
          (call $eval (local.get $g) (struct.get $SCons $state (local.get $c)))
          (call $bind (struct.get $SCons $next (local.get $c)) (local.get $g))))))
  (struct.new $SBind (i32.const 4) (local.get $st) (local.get $g)))

(func $force (param $st (ref $Stream)) (result (ref $Stream))
  (local $a (ref null $SApply))
  (local $m (ref null $SMplus))
  (local $b (ref null $SBind))
  (if (i32.eq (call $tag (local.get $st)) (i32.const 2))
    (then
      (local.set $a (ref.cast (ref $SApply) (local.get $st)))
      (return
        (call $eval
          (struct.get $SApply $goal (local.get $a))
          (struct.get $SApply $state (local.get $a))))))
  (if (i32.eq (call $tag (local.get $st)) (i32.const 3))
    (then
      (local.set $m (ref.cast (ref $SMplus) (local.get $st)))
      (return
        (call $mplus
          (struct.get $SMplus $rhs (local.get $m))
          (call $force (struct.get $SMplus $lhs (local.get $m)))))))
  (local.set $b (ref.cast (ref $SBind) (local.get $st)))
  (call $bind
    (call $force (struct.get $SBind $stream (local.get $b)))
    (struct.get $SBind $goal (local.get $b))))

(func $pull (param $st (ref $Stream)) (result (ref $Stream))
  (loop $again
    (if (i32.ge_u (call $tag (local.get $st)) (i32.const 2))
      (then
        (local.set $st (call $force (local.get $st)))
        (br $again))))
  (local.get $st))

;; Evaluation

(func $eval (param $g (ref $Goal)) (param $s (ref $State)) (result (ref $Stream))
  (local $tag i32)
  (local $args (ref null $Env))
  (local $r (ref null $State))
  (local $p (ref null $GPair))
  (local $f (ref null $GFresh))
  (local $env (ref null $Env))
  (local $n i32)
  (local $q (ref null $GQuery))
  (local $i (ref null $GIfte))
  (local $st (ref null $Stream))
  (local.set $tag (struct.get $Goal $tag (local.get $g)))
  (block $prim
    (block $ifte
      (block $query
        (block $fresh
          (block $delay
            (block $disj
              (block $conj
                (block $eq
                  (block $false
                    (block $true
                      (br_table $true $false $eq $conj $disj $delay $fresh $query $ifte $prim
                        (local.get $tag)))
                    (return (call $unit (local.get $s))))
                  (return (global.get $s_nil)))
                (local.set $args (struct.get $GTerms $args (ref.cast (ref $GTerms) (local.get $g))))
                (local.set $r
                  (call $unify (local.get $s)
                    (array.get $Env (local.get $args) (i32.const 0))
                    (array.get $Env (local.get $args) (i32.const 1))))
                (if (ref.is_null (local.get $r)) (then (return (global.get $s_nil))))
                (return (call $unit (ref.as_non_null (local.get $r)))))
              (local.set $p (ref.cast (ref $GPair) (local.get $g)))
              (return
                (call $bind
                  (call $eval (struct.get $GPair $lhs (local.get $p)) (local.get $s))
                  (struct.get $GPair $rhs (local.get $p)))))
            (local.set $p (ref.cast (ref $GPair) (local.get $g)))
            (return
              (call $mplus
                (call $eval (struct.get $GPair $lhs (local.get $p)) (local.get $s))
                (call $eval (struct.get $GPair $rhs (local.get $p)) (local.get $s)))))
          (return
            (struct.new $SApply (i32.const 2)
              (struct.get $GDelay $goal (ref.cast (ref $GDelay) (local.get $g)))
              (local.get $s))))
        ;; call-with-fresh: extend the closure's environment with a new variable
        (local.set $f (ref.cast (ref $GFresh) (local.get $g)))
        (local.set $n (array.len (struct.get $GFresh $env (local.get $f))))
        (local.set $env (array.new $Env (ref.null eq) (i32.add (local.get $n) (i32.const 1))))
        (array.copy $Env $Env
          (local.get $env) (i32.const 0)
          (struct.get $GFresh $env (local.get $f)) (i32.const 0)
          (local.get $n))
        (array.set $Env (local.get $env) (local.get $n)
          (struct.new $Var (struct.get $State $counter (local.get $s))))
        (return
          (call $eval
            (call_ref $Pred
              (ref.as_non_null (local.get $env))
              (struct.get $GFresh $body (local.get $f)))
            (struct.new $State
              (struct.get $State $subst (local.get $s))
              (i32.add (struct.get $State $counter (local.get $s)) (i32.const 1))
              (struct.get $State $query (local.get $s))))))
      (local.set $q (ref.cast (ref $GQuery) (local.get $g)))
      (return
        (call $eval
          (struct.get $GQuery $goal (local.get $q))
          (struct.new $State
            (struct.get $State $subst (local.get $s))
            (struct.get $State $counter (local.get $s))
            (struct.new $Query
              (struct.get $GQuery $name (local.get $q))
              (struct.get $GQuery $var (local.get $q))
              (struct.get $State $query (local.get $s)))))))
    (local.set $i (ref.cast (ref $GIfte) (local.get $g)))
    (local.set $st
      (call $pull (call $eval (struct.get $GIfte $test (local.get $i)) (local.get $s))))
    (if (i32.eq (call $tag (ref.as_non_null (local.get $st))) (i32.const 1))
      (then
        (return
          (call $bind (ref.as_non_null (local.get $st)) (struct.get $GIfte $then (local.get $i))))))
    (return (call $eval (struct.get $GIfte $else (local.get $i)) (local.get $s))))
  (call $eval_prim
    (local.get $tag)
    (struct.get $GTerms $args (ref.cast (ref $GTerms) (local.get $g)))
    (local.get $s)))

(func $check (param $ok i32) (param $s (ref $State)) (result (ref $Stream))
  (if (result (ref $Stream)) (local.get $ok)
    (then (call $unit (local.get $s)))
    (else (global.get $s_nil))))

(func $fresh_state (param $s (ref $State)) (result (ref $State))
  (struct.new $State
    (struct.get $State $subst (local.get $s))
    (i32.add (struct.get $State $counter (local.get $s)) (i32.const 1))
    (struct.get $State $query (local.get $s))))

(func $fresh_var (param $s (ref $State)) (result (ref $Var))
  (struct.new $Var (struct.get $State $counter (local.get $s))))

;; disj [eq(V, Cur), Next] as built by `generate` in karuta.lfe
(func $generate (param $v eqref) (param $cur eqref) (param $next (ref $Goal)) (result (ref $Goal))
  (call $mk_disj
    (call $mk_delay (call $eq (local.get $v) (local.get $cur)))
    (call $mk_disj (call $mk_delay (local.get $next)) (global.get $g_false))))

;; Floored division with a non-negative remainder, as `divmod/2` in karuta.lfe
(func $divmod (param $a i64) (param $b i64) (result i64 i64)
  (local $q i64)
  (local $r i64)
  (local.set $q (i64.div_s (local.get $a) (local.get $b)))
  (local.set $r (i64.rem_s (local.get $a) (local.get $b)))
  (if (i64.lt_s (local.get $r) (i64.const 0))
    (then
      (if (i64.gt_s (local.get $b) (i64.const 0))
        (then
          (local.set $q (i64.sub (local.get $q) (i64.const 1)))
          (local.set $r (i64.add (local.get $r) (local.get $b))))
        (else
          (local.set $q (i64.add (local.get $q) (i64.const 1)))
          (local.set $r (i64.sub (local.get $r) (local.get $b)))))))
  (local.get $q)
  (local.get $r))

(func $next_int (param $n i64) (result i64)
  (if (result i64) (i64.lt_s (local.get $n) (i64.const 0))
    (then (i64.sub (i64.const 0) (local.get $n)))
    (else (i64.xor (local.get $n) (i64.const -1)))))

(func $eval_prim (param $op i32) (param $args (ref $Env)) (param $s (ref $State)) (result (ref $Stream))
  (local $n i32)
  (local $i i32)
  (local $mask i32)
  (local $a eqref)
  (local $b eqref)
  (local $c eqref)
  (local $d eqref)
  (local $k eqref)
  (local $q i64)
  (local $r i64)
  (local $st (ref null $State))
  (local $raw (ref null $Env))
  ;; Dereference every argument into a copy (the goal may run again in another
  ;; state); bit i of $mask is set when argument i is bound.
  (local.set $raw (local.get $args))
  (local.set $n (array.len (local.get $args)))
  (local.set $args (array.new $Env (ref.null eq) (local.get $n)))
  (loop $deref
    (if (i32.lt_u (local.get $i) (local.get $n))
      (then
        (array.set $Env (local.get $args) (local.get $i)
          (call $walk
            (array.get $Env (local.get $raw) (local.get $i))
            (struct.get $State $subst (local.get $s))))
        (if (i32.eqz (call $is_var (array.get $Env (local.get $args) (local.get $i))))
          (then (local.set $mask (i32.or (local.get $mask) (i32.shl (i32.const 1) (local.get $i))))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $deref))))
  (local.set $a (array.get $Env (local.get $args) (i32.const 0)))
  (if (i32.gt_u (local.get $n) (i32.const 1))
    (then (local.set $b (array.get $Env (local.get $args) (i32.const 1)))))
  (if (i32.gt_u (local.get $n) (i32.const 2))
    (then (local.set $c (array.get $Env (local.get $args) (i32.const 2)))))
  (if (i32.gt_u (local.get $n) (i32.const 3))
    (then (local.set $d (array.get $Env (local.get $args) (i32.const 3)))))

  ;; defpred argument checks: a bound argument of the wrong type fails.
  (if (i32.ge_u (local.get $op) (i32.const 18))
    (then
      (local.set $i (i32.const 0))
      (loop $checks
        (if (i32.lt_u (local.get $i) (local.get $n))
          (then
            (if (i32.and
                  (i32.and (local.get $mask) (i32.shl (i32.const 1) (local.get $i)))
                  (i32.eqz (call $is_int (array.get $Env (local.get $args) (local.get $i)))))
              (then (return (global.get $s_nil))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $checks))))
      (if (i32.and (i32.eq (local.get $op) (i32.const 22)) (i32.and (local.get $mask) (i32.const 2)))
        (then
          (if (i64.eqz (call $iv (local.get $b)))
            (then (return (global.get $s_nil))))))))

  ;; int(N)
  (if (i32.eq (local.get $op) (i32.const 10))
    (then
      (if (local.get $mask)
        (then (return (call $check (call $is_int (local.get $a)) (local.get $s)))))
      (return
        (call $eval (call $prim2 (i32.const 14) (local.get $a) (call $int (i64.const 0))) (local.get $s)))))
  ;; int(N, Limit)
  (if (i32.eq (local.get $op) (i32.const 11))
    (then
      (if (i32.and (local.get $mask) (i32.const 1))
        (then
          (return
            (call $check
              (call $int_in (local.get $a)
                (i64.const 0x8000000000000000)
                (call $abs (call $iv (local.get $b))))
              (local.get $s)))))
      (return
        (call $eval
          (call $prim3 (i32.const 15) (local.get $a) (call $int (i64.const 0))
            (call $int (call $abs (call $iv (local.get $b)))))
          (local.get $s)))))
  ;; nat(N)
  (if (i32.eq (local.get $op) (i32.const 12))
    (then
      (if (local.get $mask)
        (then
          (return
            (call $check
              (call $int_in (local.get $a) (i64.const 0) (i64.const 0x7fffffffffffffff))
              (local.get $s)))))
      (return
        (call $eval (call $prim2 (i32.const 16) (local.get $a) (call $int (i64.const 0))) (local.get $s)))))
  ;; nat(N, Limit)
  (if (i32.eq (local.get $op) (i32.const 13))
    (then
      (if (i32.and (local.get $mask) (i32.const 1))
        (then
          (return
            (call $check
              (call $int_in (local.get $a)
                (i64.const 0)
                (i64.sub (call $iv (local.get $b)) (i64.const 1)))
              (local.get $s)))))
      (return
        (call $eval
          (call $prim3 (i32.const 17) (local.get $a) (call $int (i64.const 0)) (local.get $b))
          (local.get $s)))))
  ;; Generators: every-int and every-nat, unbounded (14, 16) or bounded (15, 17)
  (if (i32.eq (local.get $op) (i32.const 14))
    (then
      (return
        (call $eval
          (call $generate (local.get $a) (local.get $b)
            (call $prim2 (i32.const 14) (local.get $a)
              (call $int (call $next_int (call $iv (local.get $b))))))
          (local.get $s)))))
  (if (i32.eq (local.get $op) (i32.const 15))
    (then
      (if (i64.gt_s (call $iv (local.get $b)) (call $iv (local.get $c)))
        (then (return (global.get $s_nil))))
      (return
        (call $eval
          (call $generate (local.get $a) (local.get $b)
            (call $prim3 (i32.const 15) (local.get $a)
              (call $int (call $next_int (call $iv (local.get $b))))
              (local.get $c)))
          (local.get $s)))))
  (if (i32.eq (local.get $op) (i32.const 16))
    (then
      (return
        (call $eval
          (call $generate (local.get $a) (local.get $b)
            (call $prim2 (i32.const 16) (local.get $a)
              (call $int (i64.add (call $iv (local.get $b)) (i64.const 1)))))
          (local.get $s)))))
  (if (i32.eq (local.get $op) (i32.const 17))
    (then
      (if (i64.ge_s (call $iv (local.get $b)) (call $iv (local.get $c)))
        (then (return (global.get $s_nil))))
      (return
        (call $eval
          (call $generate (local.get $a) (local.get $b)
            (call $prim3 (i32.const 17) (local.get $a)
              (call $int (i64.add (call $iv (local.get $b)) (i64.const 1)))
              (local.get $c)))
          (local.get $s)))))
  ;; leq(L, R)
  (if (i32.eq (local.get $op) (i32.const 18))
    (then
      (if (i32.eq (local.get $mask) (i32.const 3))
        (then
          (return
            (call $check
              (i64.le_s (call $iv (local.get $a)) (call $iv (local.get $b)))
              (local.get $s)))))
      (if (i32.eqz (local.get $mask))
        (then
          (return
            (call $eval
              (call $mk_conj
                (call $prim1 (i32.const 10) (local.get $a))
                (call $prim2 (i32.const 18) (local.get $a) (local.get $b)))
              (local.get $s)))))
      (local.set $k (call $fresh_var (local.get $s)))
      (return
        (call $eval
          (call $mk_conj
            (call $prim1 (i32.const 12) (local.get $k))
            (call $prim3 (i32.const 20) (local.get $a) (local.get $k) (local.get $b)))
          (call $fresh_state (local.get $s))))))
  ;; lt(L, R)
  (if (i32.eq (local.get $op) (i32.const 19))
    (then
      (if (i32.eq (local.get $mask) (i32.const 3))
        (then
          (return
            (call $check
              (i64.lt_s (call $iv (local.get $a)) (call $iv (local.get $b)))
              (local.get $s)))))
      (if (i32.eqz (local.get $mask))
        (then
          (return
            (call $eval
              (call $mk_conj
                (call $prim1 (i32.const 10) (local.get $a))
                (call $prim2 (i32.const 19) (local.get $a) (local.get $b)))
              (local.get $s)))))
      (local.set $k (call $fresh_var (local.get $s)))
      (return
        (call $eval
          (call $mk_conj
            (call $prim1 (i32.const 12) (local.get $k))
            (call $mk_ifte
              (call $eq0 (local.get $k))
              (global.get $g_false)
              (call $prim3 (i32.const 20) (local.get $a) (local.get $k) (local.get $b))))
          (call $fresh_state (local.get $s))))))
  ;; plus(L, R, O)
  (if (i32.eq (local.get $op) (i32.const 20))
    (then
      (if (i32.eq (i32.and (local.get $mask) (i32.const 3)) (i32.const 3))
        (then
          (return
            (call $eval
              (call $eq (local.get $c)
                (call $int (i64.add (call $iv (local.get $a)) (call $iv (local.get $b)))))
              (local.get $s)))))
      (if (i32.eq (local.get $mask) (i32.const 6))
        (then
          (return
            (call $eval
              (call $eq (local.get $a)
                (call $int (i64.sub (call $iv (local.get $c)) (call $iv (local.get $b)))))
              (local.get $s)))))
      (if (i32.eq (local.get $mask) (i32.const 5))
        (then
          (return
            (call $eval
              (call $eq (local.get $b)
                (call $int (i64.sub (call $iv (local.get $c)) (call $iv (local.get $a)))))
              (local.get $s)))))
      (return
        (call $eval
          (call $mk_conj
            (call $prim1 (i32.const 10)
              (if (result eqref) (i32.eq (local.get $mask) (i32.const 1))
                (then (local.get $b))
                (else
                  (if (result eqref) (i32.eqz (local.get $mask))
                    (then (local.get $c))
                    (else (local.get $a))))))
            (call $prim3 (i32.const 20) (local.get $a) (local.get $b) (local.get $c)))
          (local.get $s)))))
  ;; mult(L, R, O)
  (if (i32.eq (local.get $op) (i32.const 21))
    (then
      (if (i32.eq (i32.and (local.get $mask) (i32.const 3)) (i32.const 3))
        (then
          (return
            (call $eval
              (call $eq (local.get $c)
                (call $int (i64.mul (call $iv (local.get $a)) (call $iv (local.get $b)))))
              (local.get $s)))))
      (if (i32.eq (local.get $mask) (i32.const 6))
        (then
          (return
            (call $eval
              (call $mk_ifte (call $eq0 (local.get $b))
                (call $mk_conj (call $eq0 (local.get $c)) (call $prim1 (i32.const 10) (local.get $a)))
                (call $mk_ifte (call $eq0 (local.get $c))
                  (call $eq0 (local.get $a))
                  (call $prim3 (i32.const 23) (local.get $a) (local.get $c) (local.get $b))))
              (local.get $s)))))
      (if (i32.eq (local.get $mask) (i32.const 5))
        (then
          (return
            (call $eval
              (call $mk_ifte (call $eq0 (local.get $a))
                (call $mk_conj (call $eq0 (local.get $c)) (call $prim1 (i32.const 10) (local.get $b)))
                (call $mk_ifte (call $eq0 (local.get $c))
                  (call $eq0 (local.get $b))
                  (call $prim3 (i32.const 23) (local.get $b) (local.get $c) (local.get $a))))
              (local.get $s)))))
      (if (i32.eq (local.get $mask) (i32.const 4))
        (then
          (return
            (call $eval
              (call $mk_ifte (call $eq0 (local.get $c))
                (call $mk_disj
                  (call $mk_conj (call $eq0 (local.get $a)) (call $prim1 (i32.const 10) (local.get $b)))
                  (call $mk_conj (call $eq0 (local.get $b))
                    (call $prim2 (i32.const 14) (local.get $a) (call $int (i64.const -1)))))
                (call $mk_conj
                  (call $prim2 (i32.const 11) (local.get $a) (local.get $c))
                  (call $prim3 (i32.const 21) (local.get $a) (local.get $b) (local.get $c))))
              (local.get $s)))))
      (if (i32.eq (local.get $mask) (i32.const 2))
        (then
          (return
            (call $eval
              (call $mk_ifte (call $eq0 (local.get $b))
                (call $mk_conj (call $eq0 (local.get $c)) (call $prim1 (i32.const 10) (local.get $a)))
                (call $mk_conj
                  (call $prim1 (i32.const 10) (local.get $a))
                  (call $prim3 (i32.const 21) (local.get $a) (local.get $b) (local.get $c))))
              (local.get $s)))))
      (if (i32.eq (local.get $mask) (i32.const 1))
        (then
          (return
            (call $eval
              (call $mk_ifte (call $eq0 (local.get $a))
                (call $mk_conj (call $eq0 (local.get $c)) (call $prim1 (i32.const 10) (local.get $b)))
                (call $mk_conj
                  (call $prim1 (i32.const 10) (local.get $b))
                  (call $prim3 (i32.const 21) (local.get $a) (local.get $b) (local.get $c))))
              (local.get $s)))))
      (return
        (call $eval
          (call $mk_conj
            (call $prim1 (i32.const 10) (local.get $a))
            (call $prim3 (i32.const 21) (local.get $a) (local.get $b) (local.get $c)))
          (local.get $s)))))
  ;; divmod(Dividend, Divisor, Quotient, Remainder)
  (if (i32.eq (local.get $op) (i32.const 22))
    (then
      (if (i32.eq (i32.and (local.get $mask) (i32.const 3)) (i32.const 3))
        (then
          (call $divmod (call $iv (local.get $a)) (call $iv (local.get $b)))
          (local.set $r)
          (local.set $q)
          (local.set $st (call $unify (local.get $s) (local.get $c) (call $int (local.get $q))))
          (if (ref.is_null (local.get $st)) (then (return (global.get $s_nil))))
          (local.set $st
            (call $unify (ref.as_non_null (local.get $st)) (local.get $d) (call $int (local.get $r))))
          (if (ref.is_null (local.get $st)) (then (return (global.get $s_nil))))
          (return (call $unit (ref.as_non_null (local.get $st))))))
      (if (i32.eq (local.get $mask) (i32.const 14))
        (then
          (return
            (call $eval
              (call $eq (local.get $a)
                (call $int
                  (i64.add (call $iv (local.get $d))
                    (i64.mul (call $iv (local.get $b)) (call $iv (local.get $c))))))
              (local.get $s)))))
      (if (i32.eq (i32.and (local.get $mask) (i32.const 11)) (i32.const 2))
        (then
          (return
            (call $eval
              (call $mk_conj
                (call $prim2 (i32.const 13) (local.get $d)
                  (call $int (call $abs (call $iv (local.get $b)))))
                (call $prim4 (i32.const 22) (local.get $a) (local.get $b) (local.get $c) (local.get $d)))
              (local.get $s)))))
      (return
        (call $eval
          (call $mk_conj
            (call $prim1 (i32.const 10)
              (if (result eqref) (i32.eq (local.get $mask) (i32.const 10))
                (then (local.get $c))
                (else (local.get $b))))
            (call $prim4 (i32.const 22) (local.get $a) (local.get $b) (local.get $c) (local.get $d)))
          (local.get $s)))))
  ;; X = Dividend / Divisor when exact: (eq (tuple X 0) (divmod Dividend Divisor))
  (call $divmod (call $iv (local.get $b)) (call $iv (local.get $c)))
  (local.set $r)
  (local.set $q)
  (if (i64.ne (local.get $r) (i64.const 0)) (then (return (global.get $s_nil))))
  (call $eval (call $eq (local.get $a) (call $int (local.get $q))) (local.get $s)))

;; Output, written to linear memory starting at $out_base

(func $emit (param $byte i32)
  (if (i32.ge_u (global.get $out_ptr) (i32.mul (memory.size) (i32.const 65536)))
    (then (drop (memory.grow (i32.const 1)))))
  (i32.store8 (global.get $out_ptr) (local.get $byte))
  (global.set $out_ptr (i32.add (global.get $out_ptr) (i32.const 1))))

(func $emit_sep
  (call $emit (i32.const 44))
  (call $emit (i32.const 32)))

(func $emit_atom (param $id i32)
  (local $off i32)
  (local $len i32)
  (local $i i32)
  (if (i32.lt_s (local.get $id) (i32.const 0))
    (then
      (call $emit (i32.const 91))
      (call $emit (i32.const 93))
      (return)))
  (local.set $off (i32.load (i32.mul (local.get $id) (i32.const 8))))
  (local.set $len (i32.load offset=4 (i32.mul (local.get $id) (i32.const 8))))
  (loop $chars
    (if (i32.lt_u (local.get $i) (local.get $len))
      (then
        (call $emit (i32.load8_u (i32.add (local.get $off) (local.get $i))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $chars)))))

(func $emit_digits (param $n i64)
  (if (i64.ge_u (local.get $n) (i64.const 10))
    (then (call $emit_digits (i64.div_u (local.get $n) (i64.const 10)))))
  (call $emit (i32.add (i32.const 48) (i32.wrap_i64 (i64.rem_u (local.get $n) (i64.const 10))))))

(func $emit_int (param $n i64)
  (if (i64.lt_s (local.get $n) (i64.const 0))
    (then
      (call $emit (i32.const 45))
      (local.set $n (i64.sub (i64.const 0) (local.get $n)))))
  (call $emit_digits (local.get $n)))

(func $emit_term (param $t eqref) (param $s (ref null $Bind))
  (local $f (ref null $Fun))
  (local $i i32)
  (local.set $t (call $walk (local.get $t) (local.get $s)))
  (if (ref.test (ref i31) (local.get $t))
    (then
      (call $emit_atom (i31.get_s (ref.cast (ref i31) (local.get $t))))
      (return)))
  (if (call $is_int (local.get $t))
    (then
      (call $emit_int (call $iv (local.get $t)))
      (return)))
  (if (call $is_var (local.get $t))
    (then
      (call $emit (i32.const 86))
      (call $emit (i32.const 97))
      (call $emit (i32.const 114))
      (call $emit_int
        (i64.extend_i32_u (struct.get $Var $id (ref.cast (ref $Var) (local.get $t)))))
      (return)))
  (if (ref.test (ref $Cons) (local.get $t))
    (then
      (call $emit (i32.const 91))
      (loop $elements
        (call $emit_term
          (struct.get $Cons $head (ref.cast (ref $Cons) (local.get $t)))
          (local.get $s))
        (local.set $t
          (call $walk
            (struct.get $Cons $tail (ref.cast (ref $Cons) (local.get $t)))
            (local.get $s)))
        (if (ref.test (ref $Cons) (local.get $t))
          (then
            (call $emit_sep)
            (br $elements))))
      (call $emit (i32.const 93))
      (return)))
  (local.set $f (ref.cast (ref $Fun) (local.get $t)))
  (call $emit_atom (struct.get $Fun $name (local.get $f)))
  (call $emit (i32.const 91))
  (loop $args
    (if (i32.lt_u (local.get $i) (array.len (struct.get $Fun $args (local.get $f))))
      (then
        (if (local.get $i) (then (call $emit_sep)))
        (call $emit_term
          (array.get $Env (struct.get $Fun $args (local.get $f)) (local.get $i))
          (local.get $s))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $args))))
  (call $emit (i32.const 93)))

;; One solution: a divider, then every query variable sorted by name.
(func $emit_solution (param $s (ref $State))
  (local $i i32)
  (local $last i32)
  (local $best i32)
  (local $q (ref null $Query))
  (local $var eqref)
  (loop $dashes
    (call $emit (i32.const 45))
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $dashes (i32.lt_u (local.get $i) (i32.const 9))))
  (call $emit (i32.const 10))
  (local.set $last (i32.const -1))
  (loop $vars
    (local.set $best (i32.const -1))
    (local.set $q (struct.get $State $query (local.get $s)))
    (block $scanned
      (loop $scan
        (br_if $scanned (ref.is_null (local.get $q)))
        (if (i32.and
              (i32.gt_s (struct.get $Query $name (local.get $q)) (local.get $last))
              (i32.or
                (i32.lt_s (local.get $best) (i32.const 0))
                (i32.lt_s (struct.get $Query $name (local.get $q)) (local.get $best))))
          (then
            (local.set $best (struct.get $Query $name (local.get $q)))
            (local.set $var (struct.get $Query $var (local.get $q)))))
        (local.set $q (struct.get $Query $next (local.get $q)))
        (br $scan)))
    (if (i32.ge_s (local.get $best) (i32.const 0))
      (then
        (call $emit_atom (local.get $best))
        (call $emit (i32.const 32))
        (call $emit (i32.const 61))
        (call $emit (i32.const 32))
        (call $emit_term (local.get $var) (struct.get $State $subst (local.get $s)))
        (call $emit (i32.const 10))
        (local.set $last (local.get $best))
        (br $vars)))))

;; Runs the query and prints up to $limit solutions (all when negative).
;; Returns how many bytes of output were written from $out_base.
(func $run (param $goal (ref $Goal)) (param $limit i32) (result i32)
  (local $st (ref null $Stream))
  (local $c (ref null $SCons))
  (local $count i32)
  (global.set $out_ptr (global.get $out_base))
  (local.set $st
    (call $eval (local.get $goal)
      (struct.new $State (ref.null $Bind) (i32.const 0) (ref.null $Query))))
  (block $done
    (loop $next
      (br_if $done (i32.eq (local.get $count) (local.get $limit)))
      (local.set $st (call $pull (ref.as_non_null (local.get $st))))
      (br_if $done (i32.eqz (call $tag (ref.as_non_null (local.get $st)))))
      (local.set $c (ref.cast (ref $SCons) (local.get $st)))
      (call $emit_solution (struct.get $SCons $state (local.get $c)))
      (local.set $st (struct.get $SCons $next (local.get $c)))
      (local.set $count (i32.add (local.get $count) (i32.const 1)))
      (br $next)))
  (i32.sub (global.get $out_ptr) (global.get $out_base)))
