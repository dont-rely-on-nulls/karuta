include Types
module Lookup = Lookup
module Preprocessor = Preprocessor

let check_dependency_cycle _ f = f ()
let compile_directive = Directive.compile
let compile_declaration = Declaration.compile
let compile_query = Query.compile
