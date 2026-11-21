;; Copyright 2026 Marcos Magueta <maguetamarcos@gmail.com>
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(defmodule interactive
  (export (start 0)))

(defun start ()
  "Tools will boot the node with interactive:start/0. Just like rnt:start/0,
stdin/stdout become the channel to karuta, so logs go to stderr."
  (logger:remove_handler 'default)
  (logger:add_handler 'default 'logger_std_h (map 'config (map 'type 'standard_error)))
  (loop (open_port #(fd 0 1) '(binary #(packet 4) eof))))

(defun loop (port)
  "Answers one frame at a time, with the node halting when karuta hangs up!"
  (receive
   ((tuple p (tuple 'data frame)) (when (=:= p port))
    (port_command port (answering (lambda () (serve frame))))
    (loop port))
   ((tuple p 'eof) (when (=:= p port))
    (halt))))

(defun answering (callback)
  "Translates a crash to an e frame instead of taking the node down to dante's inferno."
  (try (funcall callback)
    (catch ((tuple class reason _)
	    (cons #\e (unicode:characters_to_binary
		       (io_lib:format "~p:~p" (list class reason))))))))

(defun serve
  (((binary #\l (text binary)))
   (load text)
   #"o")
  (((binary #\q (id (size 32)) (mod binary)))
   (let ((query (mref (call (bnary_to_atom mod) '||) 'query))))
   #"o")
  (((binary #\n (id (size 32)) (limit (size 32))))
   (let* ((stream (cursor id))
	  (tuple solutions rest) (split limit stream))
     (put (tuple 'cursor id) rest)
     (list* #\s (if (=:= rest '()) 1 0) (lists:map (fun solution 1) solutions)))))

(defun cursor (id)
  (case (get (tuple 'cursor id))
    ('undefined (error (tuple 'no-cursor id)))
    (stream stream)))

(defun load (text)
  "Compile and load a module sent as erlc abstract text (a list of
forms). New code invalidates every open cursor, as their streams close
over the old code, which purging would otherwise kill us for."
  (let* (((tuple 'ok tokens _) (erl_scan:string (unicode:characters_to_list text)))
	 ((tuple 'ok forms) (erl_parse:parse_term tokens))
	 ((tuple 'ok mod beam) (compile:forms forms '(binary return_errors))))
    (lists:foreach (fun erase 1)
		   (lc ((<- (= (tuple 'cursor _) key) (get_keys))) key))
    (code:purge mod)
    (let (((tuple 'module _) (code:load_binary mod (atom_to_list mod) beam)))
      mod)))

(defun split (n stream)
  "Up to N solutions and what is left of the stream. Empty list once we
are done."
  (if (=< n 0)
    (tuple '() stream)
    (case (kanren:pull stream)
      ((tuple 'ok solution next)
       (let (((tuple solutions rest) (split (- n 1) next)))
	 (tuple (cons solution solutions) rest)))
      ((tuple 'error 'no-result) (tuple '() '())))))

(defun solution (bindings)
  "One solution query variable => lalue, sorted by variable name."
  (let ((pairs (list:sort (maps:to_list bindings))))
    (cons (binary ((length pairs) (size 16)))
	  (lc ((<- (tuple name value) pairs))
	    (let ((n (atom_to_binary name))
		  (v (unicode:characters_to_binary (render value))))
	      (binary ((byte_size n) (size 16)) (n binary)
		      ((byte_size v) (size 32)) (v binary)))))))

(defun render
  "A karuta value as karuta source. Unbound variables print as ?<n>."
  ((v) (when (is_integer v)) (integer_to_list v))
  ((v) (when (is_atom v)) (io_lib:write_atom v))
  ((v) (when (is_reference v)) (++ "?" (integer_to_list (erlang:phash2 v 10000))))
  (('()) "[]")
  (((cons h t)) (list "[" (render h) (render-tail t) "]"))
  ((v) (list " | " (render t))))

(defun render-tail
  (('()) "")
  (((cons h t)) (list "; " (render h) (render-tail t)))
  ((t) (list " | " (render t))))
