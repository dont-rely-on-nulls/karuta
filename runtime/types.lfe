(deftype primitive
  (UNION (binary)
         (integer)
         (float)
         (boolean)))

(deftype symbol
  (UNION (binary)
         (atom)))

(deftype query
  (UNION
    #(base symbol)                    ; base relation by name
    #(const #M(symbol primitive))     ; constant single-tuple relation
    #(select query query)             ; σ semijoin: (filter, source)
    #(join (list symbol) query query) ; ⋈ natural equijoin on named attrs
    #(cartesian query query)          ; × Cartesian product
    #(project (list symbol) query)    ; π restrict columns
    #(rename #M(symbol symbol) query) ; ρ rename (old,new) pairs
    #(union query query)              ; ∪ — compatible schemas assumed
    #(diff query query)               ; −
    #(take (integer) query)))         ; τ
