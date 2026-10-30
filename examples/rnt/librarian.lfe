(defmodule librarian
  (export (rnt_init 0) (titles 0)))

(defun rnt_init ()
  "Registers, from inside the node, a FOL program over main's Karuta
query: the distinct authors, at \\karuta\\program\\authors."
  (rnt:define '(#"karuta" #"program" #"authors")
              #"fol"
              (fol:source (fol:project (fol:base '(#"karuta" #"program" #"main"))
                                       '(#"author")))))

(defun titles ()
  "A plan for the titles, issued inline and read back here, so a BEAM
program can consume a FOL result as an ephemeral relation."
  (rnt:evaluate #"fol"
                (fol:source (fol:project (fol:base '(#"karuta" #"program" #"main"))
                                         '(#"title")))))
