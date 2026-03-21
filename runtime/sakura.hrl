-type primitive() :: binary() | integer() | float() | boolean().

-type query() ::
    {base, binary()}                           % base relation by name
  | {const, #{binary() => primitive()}}        % constant single-tuple relation
  | {select, query(), query()}                 % σ semijoin: (filter, source)
  | {join, [binary()], query(), query()}       % ⋈ natural equijoin on named attrs
  | {cartesian, query(), query()}              % × Cartesian product
  | {project, [binary()], query()}             % π restrict columns
  | {rename, #{binary() => binary()}, query()} % ρ rename (old,new) pairs
  | {union, query(), query()}                  % ∪ — compatible schemas assumed
  | {diff, query(), query()}                   % −
  | {take, integer(), query()}.                % τ
