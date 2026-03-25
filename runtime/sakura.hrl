-type primitive() :: binary() | integer() | float() | boolean().

-type symbol() :: binary() | atom().

-type query() ::
    {base, symbol()}                               % base relation by name
  | {const, #{symbol() => primitive()}} % constant single-tuple relation
  | {select, query(), query()}                     % σ semijoin: (filter, source)
  | {join, [symbol()], query(), query()}           % ⋈ natural equijoin on named attrs
  | {cartesian, query(), query()}                  % × Cartesian product
  | {project, [symbol()], query()}                 % π restrict columns
  | {rename, #{symbol() => symbol()}, query()}     % ρ rename (old,new) pairs
  | {union, query(), query()}                      % ∪ — compatible schemas assumed
  | {diff, query(), query()}                       % −
  | {take, integer(), query()}.                    % τ
