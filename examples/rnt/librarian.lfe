(defmodule librarian
  (export (|| 0)))

(defun || ()
  "The same entry points a compiled Karuta module has: rnt-init
registers, from inside the node, a FOL program over main's Karuta
query, the distinct authors, at \\karuta\\program\\authors; query is
what RNT runs for librarian's image."
  (map
    'rnt-init (lambda ()
                (rnt:define '(#"karuta" #"program" #"authors")
                  #"fol"
                  (fol:source (fol:project (fol:base '(#"karuta" #"program" #"main"))
                                           '(#"author")))))
    'query (lambda ()
             (kanren:call-with-fresh
               (lambda (title)
                 (kanren:query-variable title 'Title (titles title)))))))

(defun titles (title)
  "A plan for the titles, issued inline and read back here, so a BEAM
program can consume a FOL result as an ephemeral relation."
  (lambda (state)
    (let ((plan (fol:project (fol:base '(#"karuta" #"program" #"main")) '(#"title"))))
      (funcall (kanren:bind-results (map 'Title title)
                                    (sakura:rows (rnt:evaluate #"fol" (fol:source plan))))
               state))))
