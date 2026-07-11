(defmodule sakura
  (export (ask 2)
          (get-response' 3)
          (deserialize 1)))

(defun serialize (term) (lfe_io_pretty:term term))
(defun deserialize (str) (lfe_io:read_string str))

(defun keyvals->map (keyvals)
  (lists:foldl
    (lambda (elem acc)
      (let (((list key val) elem))
        (mset acc key val)))
    (map)
    keyvals))

(defun cursor-sexp->map (cursor)
  (case cursor
    ((cons 'cursor keyvals)
     (maps:update_with
       'rows
       (lambda (rows)
         (lists:map
           (lambda (row)
             (clj:->> row
               (lists:map
                 (lambda (kv)
                   (let (((list key val) kv))
                     (clj:-> key
                       (atom_to_list)
                       (string:titlecase)
                       (list_to_atom)
                       (list val)))))
               (keyvals->map)))
           rows))
       (list)
       (keyvals->map keyvals)))))

(defun create-session (socket receive-timeout query)
  (let ((session-creation-payload (serialize (list 'scl query))))
    (maybe
      (?= 'ok (gen_tcp:send socket session-creation-payload))
      (?= (tuple 'ok raw-data) (gen_tcp:recv socket 0 receive-timeout))
      (?= (tuple 'ok (list deserialized-session)) (deserialize (binary_to_list raw-data)))
      ; TODO: How do we identify that this receive is about the above send? We need to have some nonce about it.
      ; TODO: deserialized-session is not a map anymore
      (tuple 'ok (mref deserialized-session 'cursor))
      (else
        (((tuple 'error error)) (erlang:error error))))))

(defun get-response (socket receive-timeout session-id)
  (let ((serialized-fetch (serialize `(scl (fetch ,session-id)))))
    (maybe
      (?= 'ok (gen_tcp:send socket serialized-fetch))
      (?= (tuple 'ok raw-data) (gen_tcp:recv socket 0 receive-timeout))
      ; TODO: map -> list
      (?= (tuple 'ok (list (map 'has_more more 'row rows))) (deserialize raw-data))
      (case rows
        ('() '())
        ((list row)
         (case more
           ('true (cons row (lambda () (get-response socket receive-timeout session-id))))
           ('false (list row))))
        (_ (erlang:error "Unreachable: ask primitive should only fetch one piece of data at a time")))
      (else
        (((tuple 'error error)) (erlang:error error))))))

(defun get-response' (socket receive-timeout query)
  ;; This is temporary until SCL is working again.
  (maybe
    (?= 'ok (gen_tcp:send socket query))
    (?= (tuple 'ok raw-data) (gen_tcp:recv socket 0 receive-timeout))
    (?= (tuple 'ok (list cursor-sexp)) (deserialize (binary_to_list raw-data)))
    (mref (cursor-sexp->map cursor-sexp) 'rows)
    (else
      (((tuple 'error error)) (erlang:error error)))))

(defun ask (pattern raw-query)
  (lambda (state)
    (let* (((map 'db_config (map 'socket socket 'timeouts timeout-map)) state)
          (receive-timeout (maps:get 'receive timeout-map (* 5 1000)))
          (query (serialize `(drl ,raw-query))))
     (maybe
       ; TODO: uncomment this when SCL is back.
       ; (?= (tuple 'ok session-id) (create-session socket receive-timeout query))
       (funcall
         (karuta:bind-results pattern
           (lambda () (get-response' socket receive-timeout query)))
         state)
       (else
         (((tuple 'error error)) (erlang:error error)))))))
