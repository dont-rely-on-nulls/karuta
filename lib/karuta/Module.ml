include Types
module Lookup = Lookup
module Preprocessor = Preprocessor

let check_dependency_cycle = Lookup.check_dependency_cycle
let compile_directive = Directive.compile
let compile_declaration = Declaration.compile
let compile_query = Query.compile
