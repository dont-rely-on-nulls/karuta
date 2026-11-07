(defmodule leap
  (export (|| 0) (leap 1)))

(defun || ()
  "The same entry points a compiled Karuta module has. rnt-init
registers this module's image where calendar.skr's persisted
leap[Year] is read from, so a Karuta query asking
db:calendar:public:leap[Year] calls Erlang's calendar. query is what
RNT runs for the image: it is procedural, taking the bound Year, so it
can tell whether a year is a leap year but never lists them."
  (map
    'rnt-init (lambda ()
                (rnt:image 'leap '(Year)
                           '(#"system" #"branch" #"master"
                             #"multigroup" #"calendar"
                             #"schema" #"public"
                             #"relation" #"leap")))
    'query (match-lambda
             (((map 'Year year)) (kanren:query-variable year 'Year (leap year))))))

(defun leap (year)
  "Erlang's calendar:is_leap_year/1 as a procedural predicate: it only
decides whether a bound Year is a leap year, and never lists them."
  (lambda (state)
    (let ((value (kanren:deref state year)))
      (if (andalso (is_integer value) (calendar:is_leap_year value))
        (list state)
        '()))))
