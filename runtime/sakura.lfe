(defmodule sakura
  (export (ask 2)))

(include-lib "sakura.hrl")

(defun serialize_primitive
  ((primitive) (when (is_binary primitive)) (list "\"" primitive "\""))
  ((primitive) (when (is_integer primitive)) (integer_to_list primitive))
  ((primitive) (when (is_float primitive)) (float_to_list primitive))
  ((primitive) (when (is_boolean primitive)) (atom_to_list primitive)))

(defun serialize_tag_value (tag-value)
  (case tag-value
    ('int "Int")
    ('float "Float")
    ('str "Str")
    ('bool "Bool")))

(defun serialize_value
  ((#(tag primitive))
    (list "(" (serialize_tag_value tag) " " (serialize_primitive primitive) ")")))

(defun serialize_symbol
  ((key) (when (is_binary key)) (serialize_primitive key))
  ((key) (when (is_atom key)) (atom_to_list key)))

(defun serialize_map (m serialize-value)
  (list
    (maps:fold
      (lambda (key value acc)
        (let ((serialized-key (serialize_symbol key))
              (serialized-value (funcall serialize-value value)))
          (list acc "(" serialized-key " " serialized-value ")")))
      "("
      m)
    ")"))

(defun serialize_list_of_symbols (list-of-symbols)
  (lists:map (lambda (s) (serialize_symbol s)) list-of-symbols))

(defun serialize_drl_body (query)
  (case query
    (#(base relation-name)
      (list "(Base " (serialize_symbol relation-name) ")"))
    (#(const const)
      (list "(Const " (serialize_map const (lambda (v) (serialize_value v))) " )"))
    (#(select query1 query2)
      (list "(Select " (serialize query1) " " (serialize query2) ")"))
    (#(join list-of-symbols query1 query2)
      (list "(Join " (serialize_list_of_symbols list-of-symbols) " "
            (serialize query1) " " (serialize query2) ")"))
    (#(cartesian query1 query2)
      (list "(Cartesian " (serialize query1) " " (serialize query2) ")"))
    (#(project list-of-symbols query1)
      (list "(Project " (serialize_list_of_symbols list-of-symbols) " " (serialize query1) ")"))
    (#(rename rename-map query1)
      (list "(Rename " (serialize_map rename-map (lambda (s) (serialize_symbol s))) " " (serialize query1) " )"))
    (#(union query1 query2)
      (list "(Union " (serialize query1) " " (serialize query2) ")"))
    (#(diff query1 query2)
      (list "(Diff " (serialize query1) " " (serialize query2) ")"))
    (#(take how-many query1)
      (list "(Take " (serialize_primitive how-many) " " (serialize query1) ")"))))

(defun serialize_scl_body (query)
  (case query
    (#(begin q)
      (let ((drl-serialized-body (serialize_drl_body q)))
        (list "(Begin (query " drl-serialized-body ")" " (limit (0)))")))
    (#(fetch cursor)
      (list "(Fetch (cursor " cursor ")" " (limit (1)))"))))

(defun serialize_sublanguage (tag)
  (case tag
    ('drl #("drl" #'serialize_drl_body/1))
    ('ddl #("ddl" #'serialize_drl_body/1))
    ('dml #("dml" #'serialize_drl_body/1))
    ('scl #("scl" #'serialize_scl_body/1))
    (_ (erlang:error (list "We do not support the following sublanguage yet:" (atom_to_list tag))))))

(defun serialize
  (#(tag query)
   (let* ((#(serialized-tag serialize-body-fun)) (serialize_sublanguage tag)
          (body (funcall serialize-body-fun (element 2 query))))
    #(ok (list "(" serialized-tag " " body ")")))))

(defun deserialize
  ((#(drl _response))
    (erlang:error "TODO: Wait for the LFE migration"))
  ((#(scl session _response))
    (erlang:error "TODO: Wait for the LFE migration"))
  ((#(scl data response))
    (deserialize #(drl response)))
  ((response)
    #(ok response)))

(defun create_session (socket receive-timeout query)
  (case (serialize #(scl query))
    (#(ok session-creation-payload)
      (case (gen_tcp:send socket session-creation-payload)
        ('ok
          (case (gen_tcp:recv socket 0 receive-timeout)
            (#(ok raw-data)
              (let ((deserialized-session (deserialize #(scl session raw-data))))
                (case deserialized-session
                  (#(ok session) #(ok (maps:get 'cursor session)))
                  (#(error err) (erlang:error err)))))
            (#(error err) (erlang:error err))))
        (#(error err) (erlang:error err))))
    (#(error err) (erlang:error err))))

(defun get_response (socket receive-timeout session-id)
  (case (serialize #(scl #(fetch session-id)))
    (#(ok serialized-fetch)
      (case (gen_tcp:send socket serialized-fetch)
        ('ok
          (case (gen_tcp:recv socket 0 receive-timeout)
            (#(ok raw-data)
              (case (deserialize raw-data)
                (#(ok response)
                  (let ((more (maps:get 'has_more response))
                        (rows (maps:get 'row response)))
                    (case rows
                      ('() '())
                      ((cons row '())
                        (if more
                          (cons row (lambda () (get_response socket receive-timeout session-id)))
                          (list row)))
                      (_ (erlang:error "Unreachable: ask primitive should only fetch one piece of data at a time")))) )
                (#(error err) (erlang:error err))))
            (#(error err) (erlang:error err))))
        (#(error err) (erlang:error err))))
    (#(error err) (erlang:error err))))

(defun ask (pattern raw-query)
  (lambda (state)
    (let* ((db-config (maps:get 'db_config state))
           (socket (maps:get 'socket db-config))
           (timeout-map (maps:get 'timeouts db-config))
           (receive-timeout (maps:get 'receive timeout-map (* 5 1000))))
      (case (serialize #(drl raw-query))
        (#(ok query)
          (case (create_session socket receive-timeout query)
            (#(ok session-id)
              (let ((results
                      (karuta:bind_results
                        pattern
                        (lambda () (get_response socket receive-timeout session-id)))))
                (funcall results state)))
            (#(error err) (erlang:error err))))
        (#(error err) (erlang:error err))))))
