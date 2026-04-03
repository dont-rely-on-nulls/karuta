(defmodule sakura
  (export (ask 2)))

(include-lib "sakura.hrl")

(defun serialize (term) (lfe_io_pretty:term term))
(defun deserialize (str) (lfe_io:read_string str))

(defun create-session (socket receive-timeout query)
  (maybe
    (?= (tuple 'ok session-creation-payload) (serialize (list 'scl query)))
    (?= 'ok (gen_tcp:send socket session-creation-payload))
    (?= (tuple 'ok raw-data) (gen_tcp:recv socket 0 receive-timeout))
    (?= deserialized-session (deserialize raw-data))
    ; TODO: How do we identify that this receive is about the above send? We need to have some nonce about it.
    ; TODO: deserialized-session is not a map anymore
    (tuple 'ok (mref deserialized-session 'cursor))
    (else
      (((tuple 'error error)) (erlang:error error)))))

(defun get-response (socket receive-timeout session-id)
  (maybe
    (?= (tuple 'ok serialized-fetch) (serialize `(scl (fetch ,session-id))))
    (?= 'ok (gen_tcp:send socket serialized-fetch))
    (?= (tuple 'ok raw-data) (gen_tcp:recv socket 0 receive-timeout))
    ; TODO: map -> list
    (?= (tuple 'ok (map 'has_more more 'row rows)) (deserialize raw-data))
    (case rows
      ('() '())
      ((list row)
       (case more
         ('true (cons row (lambda () (get-response socket receive-timeout session-id))))
         ('false (list row))))
      (_ (erlang:error "Unreachable: ask primitive should only fetch one piece of data at a time")))
    (else
      (((tuple 'error error)) (erlang:error error)))))

(defun ask (pattern raw-query)
  (lambda (state)
    (let* (((map 'db_config (map 'socket socket 'timeouts timeout-map)) state)
           (receive-timeout (maps:get 'receive timeout-map (* 5 1000))))
      (maybe
        (?= (tuple 'ok query) (serialize `(drl ,raw-query)))
        (?= (tuple 'ok session-id) (create-session socket receive-timeout query))
        (funcall (karuta:bind-results pattern
                   (lambda () (get-response socket receive-timeout session-id)))
                 state)
        (else
          (((tuple 'error error)) (erlang:error error)))))))
