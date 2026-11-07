(defmodule sakura
  (export (ask 2) (rows 1) (solution 1)))

(defun ask (pattern path)
  "What a persisted Sakura predicate compiles to: calls what is at Path
in RNT, telling it the arguments already bound, and unifies each row
it answers with Pattern. Plans belong to the programs, not here."
  (lambda (state)
    (let* ((bound (maps:filter (lambda (_ value) (not (is_reference value)))
                               (maps:map (lambda (_ value) (kanren:deref state value)) pattern)))
           (relation (rnt:invoke (lists:map (fun unicode:characters_to_binary 1) path)
                                 (rnt:attributes bound))))
      (funcall (kanren:bind-results pattern (rows relation)) state))))

(defun rows (relation)
  "An RNT relation as a Karuta lazy stream, fetching 64 rows only when
the stream is pulled that far."
  (lambda ()
    (let (((tuple tuples rest) (funcall relation 64)))
      (lists:foldr (lambda (tuple stream) (cons (solution tuple) stream))
                   (if (=:= rest 'done) '() (rows rest))
                   tuples))))

(defun solution
  "An RNT tuple in the shape compiled predicates unify against:
capitalised attribute names to Karuta values."
  (((tuple _ attributes))
   (maps:fold (lambda (name value row)
                (mset row (binary_to_atom (string:titlecase name)) (karuta-value value)))
              #m()
              attributes)))

(defun karuta-value
  "Strings come back as atoms, which is what Karuta compares and prints.
This creates atoms from data, so keep relations bounded for now."
  ((value) (when (is_binary value)) (binary_to_atom value))
  ((value) value))
