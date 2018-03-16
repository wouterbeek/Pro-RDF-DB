rdf_update_typed_literal(Literal1, D, Literal2) :-
  rdf_literal_value(Literal1, Value),
  rdf_update_typed_literal(Literal1, D, Value1, Value2),
  rdf_literal(D, Value2, 

rdf_update_typed_literal(Literal1, D, Value, Literal2) :-
  rdf_subdatatype(D, xsd:integer), !,
  value_to_number(Value, M),
  number_to_integer(M, N).
rdf_update_typed_literal(Literal1, D, Value1, Value2) :-
  rdf_subdatatype(D, xsd:double), !,
  value_to_number(Value1, M),
  number_to_float(M, Value2).

value_to_number(N, N) :-
  number(N), !.
value_to_number(Value, N) :-
  atom(Value), !,
  atom_number(Value, N).
value_to_number(Value, N) :-
  string(Value), !,
  number_string(N, Value).

number_to_float(N, N) :-
  float(N), !.
number_to_float(M, N) :-
  N is float(M).

number_to_integer(N, N) :-
  integer(N), !.
number_to_integer(M, N) :-
  number(M), !,
  N is round(M).
