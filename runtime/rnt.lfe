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

(defmodule rnt
  (export (start 0)))

(defun start ()
  (loop))

(defun loop ()
  (serve (frame))
  (loop))

(defun frame ()
  "Blocks for the next frame from RNT. The node only halts when RNT
closes the channel, so it shouldn't outlive the kernel lifetime."
  (let ((port (get 'rnt-port)))
    (receive
      ((tuple p (tuple 'data frame)) (when (=:= p port)) frame)
      ((tuple p 'eof) (when (=:= p port)) (halt)))))

(defun answering (callback)
  "Runs a request and returns any crash into an e frame, so a failing
program becomes an RNT condition, instead of killing the entire node."
  (try (funcall callback)
    (catch
      ((tuple class reason _)
       (list #\e (io_lib:format "~p:~p" (list class reason)))))))

(defun send (frame)
  "Writes one frame to RNT, with {packet 4} adds the length prefix."
  (port_command (get 'rnt-port) frame))


(defun serve
  "Answers to RNT requests where `h` runs the initialization procedures,
i prepares a program image (loaded lazily, so load errors bubble on
the first fetch), f reads the next batch and c forgets about it."
  (((binary #\h))
   (send (answering
	  (lambda ()
	    (progn (initialize)
		   #"o")))))
  (((binary #\i (id (size 64))
	    (m (size 16))
	    (b (size 32))
	    (f (size 16))
	    (func binary (size f))
	    (b (size 32))
	    (beam binary (size b))))
   (put (tuple 'poll id)
	(lambda (limit)
	  (let ((name (load (erlang:binary_to_atom func) beam)))
	    (funcall (call name (binary_to_existing_atom func)) limit)))))
  (((binary #\f (id (size 64)) (limit (size 32))))
   (send (answerinng (lambda () (fetch id limit)))))
  (((binary #\c (id (size 64))))
   (erase (tuple 'poll id))))

(defun initialize ()
  "Let every loaded module register its programs with RNT at boot time,
for example a compiled Karuta query registering itself as an image."
  (lc ((<- (tuple mod _) (code:all_loaded))
       (erlang:function_exported mod 'rnt_init 0))
    (call mod 'rnt_init)))

(defun load (name beam)
  "Loads the .beam an image carries, unless that exact code is already
loaded, so the image RNT stores is the source of truth for the code."
  (let (((tuple 'ok (tuple _ md5)) (beam_lib:md5 beam)))
    (if (andalso (erlang:module_loaded name) (=:= (call name 'module_info 'md5) md5))
      name
      (progn
        (code:purge name)
        (let (((tuple 'module _) (code:load_binary name (++ (atom_to_list name) ".beam") beam)))
          name)))))

(defun fetch (id limit)
  "Pulls up to Limit tuples from a running image and keeps the rest of
its stream for the next fetch."
  (case (get (tuple 'poll id))
    ('done (batch '() 'true))
    (relation
     (let (((tuple rows rest) (funcall relation limit)))
       (put (tuple 'poll id) rest)
       (batch rows (=:= rest 'done))))))

(defun batch (rows exhausted)
  "Encodes a b frame: the exhausted flag, then each {Type, #{Name =>
Value}} tuple as a sized term_to_binary."
  (list* #\b (if exhausted 1 0)
         (lc ((<- row rows))
           (let ((term (term_to_binary row)))
             (list (binary ((byte_size term) (size 32))) term)))))

(defun define (path evaluator source)
  "Registers Source as a program of Evaluator (karuta:beam, fol, ...)
at Path in RNT's tree, so it can be invoked later by anyone."
  (request (list #\D (encode-path path) (sized evaluator) source)))

(defun invoke (path)
  "Runs the program registered at Path and hands back its result, an
ephemeral relation, as a lazy fun(Limit)."
  (relation (request (list #\X (encode-path path)))))

(defun evaluate (evaluator source)
  "Runs a program without registering it, for plans built on the fly
such as the FOL query behind a persisted Sakura predicate."
  (relation (request (list #\E (sized evaluator) source))))
