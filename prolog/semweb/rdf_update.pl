:- module(
  rdf_update,
  [
    rdf_update/2, % :From_0, To_0
    rdf_update/3  % :From_0, To_0, -N
  ]
).

/** <module> RDF in-memory updates

@author Wouter Beek
@version 2019
*/

:- use_module(library(counter)).
:- use_module(library(debug_ext)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).

:- meta_predicate
    rdf_update(0, 0),
    rdf_update(0, 0, -).

:- rdf_meta
   rdf_update(t, t),
   rdf_update(t, t, -).





%! rdf_update(:From_0, :To_0) is det.
%! rdf_update(:From_0, :To_0, -N:nonneg) is det.

rdf_update(From_0, To_0) :-
  rdf_update(From_0, To_0, _).


rdf_update(From_0, To_0, N) :-
  create_counter(Counter),
  forall(
    From_0,
    (   rdf_transaction(To_0)
    ->  increment_counter(Counter)
    ;   print_message(warning, update(From_0,To_0))
    )
  ),
  counter_value(Counter, N).
