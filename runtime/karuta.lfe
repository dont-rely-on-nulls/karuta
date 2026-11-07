(defmodule karuta
  (export
    (leq 2)
    (lt 2)
    (int 1)
    (nat 1)
    (plus 3)
    (minus 3)
    (neg 2)
    (mult 3)
    (divmod 4)
    (eq 2)
    (dee 1)
    (dum 1)))

(eval-when-compile
  (defun gensym ()
    (list_to_atom
     (lc ((<- _ (lists:seq 1 128)))
         (+ 96 (rand:uniform 26)))))

  (defun tuple-pattern (size position-of-bound)
    (cond
      ((=:= size 0) '())
      ((< 0 size) (cons (if (=:= position-of-bound 0) ''bound '_)
                        (tuple-pattern (- size 1) (- position-of-bound 1))))))

  (defun state-sym+args->arg-names+bindings+case-tuple+checks (state-sym args)
    (let* ((arg-count (length args))
           ((tuple arg-names bindings case-tuple predicate-checks _)
            (lists:foldr
             (lambda (arg-pair acc)
               (let (((tuple name-acc bindings-acc case-acc checks-acc position) acc)
                     ((list pred-name arg-name) arg-pair))
                 (tuple (cons arg-name name-acc)
                        (cons (list arg-name `(kanren:deref ,state-sym ,arg-name))
                              bindings-acc)
                        (cons `(kanren:boundedness ,arg-name ,state-sym)
                              case-acc)
                        (cons `((tuple ,@(tuple-pattern arg-count
                                                        (+ arg-count position)))
                                (when (not (,pred-name ,arg-name)))
                                (fun kanren:false 1))
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

(defun eq (lhs rhs)
  (lambda (state) (kanren:unify state lhs rhs)))

(defun ifte (test then else)
  (lambda (state)
    (let ((test-results (funcall test state)))
      (case (kanren:pull test-results)
        ((tuple 'ok res next) (kanren:bind (cons res next) then))
        ((tuple 'error 'no-result) (funcall else state))))))

(defun next-int (n)
  (if (< n 0)
    (- n)
    (erlang:bnot n)))

(defun next-nat (n) (+ n 1))

(defun generate (next variable current)
  (kanren:disj
   (list (eq variable current)
     (lambda (state)
       (funcall (generate next variable (funcall next current)) state)))))

(defun generate-while (pred next variable current)
  (kanren:disj
   (if (funcall pred current)
     (list (eq variable current)
       (lambda (state)
         (funcall (generate-while pred next variable (funcall next current)) state)))
     (list))))

(defun every-int (variable current)
  (generate (fun next-int 1) variable current))

(defun every-int (variable current limit)
  (generate-while
    (lambda (current) (=< current limit))
    (fun next-int 1)
    variable
    current))

(defun every-nat (variable current)
  (generate (fun next-nat 1) variable current))

(defun every-nat (variable current limit)
  (generate-while
    (lambda (current) (< current limit))
    (fun next-nat 1)
    variable
    current))

(defun type (pred generate)
  (lambda (v)
    (lambda (state)
      (let* ((deref-v (kanren:deref state v))
             (goal (if (kanren:is-variable deref-v state)
                     (funcall generate deref-v)
                     (eq 'true (funcall pred deref-v)))))
        (funcall goal state)))))

(defun int (n)
  (funcall (type (fun is_integer 1)
                 (lambda (n) (every-int n 0)))
           n))

(defun int (n limit)
  (funcall (type (lambda (n)
                   (andalso (is_integer n)
                     (=< n (abs limit))))
                 (lambda (n) (every-int n 0 (abs limit))))
           n))

(defun nat (n)
  (funcall (type (lambda (n) (andalso (is_integer n) (=< 0 n)))
                 (lambda (n) (every-nat n 0)))
           n))

(defun nat (n limit)
  (funcall (type (lambda (n)
                   (andalso (is_integer n)
                     (=< 0 n)
                     (< n limit)))
                 (lambda (n) (every-nat n 0 limit)))
           n))

(defpred leq ((is_integer lhs) (is_integer rhs))
  ((tuple 'bound 'bound) (if (=< lhs rhs) (fun kanren:true 1) (fun kanren:false 1)))
  ((tuple 'unbound 'unbound) (kanren:conj (int lhs) (leq lhs rhs)))
  ((tuple _ _)
   (kanren:call-with-fresh
    (lambda (n)
      (kanren:conj (nat n)
        (plus lhs n rhs))))))

(defpred lt ((is_integer lhs) (is_integer rhs))
  ((tuple 'bound 'bound) (if (< lhs rhs) (fun kanren:true 1) (fun kanren:false 1)))
  ((tuple 'unbound 'unbound) (kanren:conj (int lhs) (lt lhs rhs)))
  ((tuple _ _)
   (kanren:call-with-fresh
    (lambda (n)
      (kanren:conj (nat n)
        (ifte (eq n 0)
          (fun kanren:false 1)
          (plus lhs n rhs)))))))

(defpred plus ((is_integer lhs) (is_integer rhs) (is_integer out))
  ((tuple 'bound 'bound _) (eq out (+ lhs rhs)))
  ((tuple 'unbound 'bound 'bound) (eq lhs (- out rhs)))
  ((tuple 'bound 'unbound 'bound) (eq rhs (- out lhs)))
  ((tuple 'unbound 'unbound 'bound) (kanren:conj (int lhs) (plus lhs rhs out)))
  ((tuple 'unbound 'bound 'unbound) (kanren:conj (int lhs) (plus lhs rhs out)))
  ((tuple 'bound 'unbound 'unbound) (kanren:conj (int rhs) (plus lhs rhs out)))
  ((tuple 'unbound 'unbound 'unbound) (kanren:conj (int out) (plus lhs rhs out))))

(defun minus (lhs rhs out)
  (plus rhs out lhs))

(defun neg (n -n)
  (plus n -n 0))

(defun divmod (dividend divisor)
  (let ((q (div dividend divisor))
        (r (rem dividend divisor)))
    (if (< r 0)
      (if (> divisor 0)
        (tuple (- q 1) (+ r divisor))
        (tuple (+ q 1) (- r divisor)))
      (tuple q r))))

(defpred mult ((is_integer lhs) (is_integer rhs) (is_integer out))
  ((tuple 'bound 'bound _) (eq out (* lhs rhs)))
  ((tuple 'unbound 'bound 'bound)
   (ifte (eq rhs 0)
     (kanren:conj (eq out 0) (int lhs))
     (ifte (eq out 0)
       (eq lhs 0)
       (lambda (state)
         (funcall (eq (tuple lhs 0) (divmod out rhs)) state)))))
  ((tuple 'bound 'unbound 'bound)
   (ifte (eq lhs 0)
     (kanren:conj (eq out 0) (int rhs))
     (ifte (eq out 0)
       (eq rhs 0)
       (lambda (state)
         (funcall (eq (tuple rhs 0) (divmod out lhs)) state)))))
  ((tuple 'unbound 'unbound 'bound)
   (ifte (eq out 0)
     (kanren:disj (kanren:conj (eq lhs 0) (int rhs))
       (kanren:conj (eq rhs 0) (every-int lhs -1)))
     (kanren:conj (int lhs out) (mult lhs rhs out))))
  ((tuple 'unbound 'bound 'unbound)
   (ifte (eq rhs 0)
     (kanren:conj (eq out 0) (int lhs))
     (kanren:conj (int lhs) (mult lhs rhs out))))
  ((tuple 'bound 'unbound 'unbound)
   (ifte (eq lhs 0)
     (kanren:conj (eq out 0) (int rhs))
     (kanren:conj (int rhs) (mult lhs rhs out))))
  ((tuple 'unbound 'unbound 'unbound) (kanren:conj (int lhs) (mult lhs rhs out))))

(defmacro non_zero_integer (n)
  `(andalso (is_integer ,n) (not (=:= ,n 0))))

(defpred divmod ((is_integer dividend)
                 (non_zero_integer divisor)
                 (is_integer quotient)
                 (is_integer remainder))
  ((tuple 'bound 'bound _ _)
   (eq (tuple quotient remainder) (divmod dividend divisor)))
  ((tuple 'unbound 'bound 'bound 'bound)
   (eq dividend (+ remainder (* divisor quotient))))
  ((tuple 'unbound 'bound _ 'unbound)
   (kanren:conj (nat remainder (abs divisor))
     (divmod dividend divisor quotient remainder)))
  ((tuple 'unbound 'bound 'unbound 'bound)
   (kanren:conj (int quotient)
     (divmod dividend divisor quotient remainder)))
  ((tuple _ 'unbound _ _) ; TODO: revisit this when we have not-equals
   (kanren:conj (int divisor)
     (divmod dividend divisor quotient remainder))))

(defun dee (state) (kanren:true state))
(defun dum (state) (kanren:false state))
