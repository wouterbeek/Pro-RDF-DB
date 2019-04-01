:- module(
  rdf_update,
  [
    rdf_update/2,              % :From_0, To_0
    rdf_update/3,              % :From_0, To_0, -N
    rdf_update_add_datatype/3, % +B, +P, +D
    rdf_update_prefix/3        % +B, +Prefix1, +Prefix2
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
:- use_module(library(semweb/rdf_term)).

:- meta_predicate
    rdf_update(0, 0),
    rdf_update(0, 0, -).

:- rdf_meta
   rdf_update(t, t),
   rdf_update(t, t, -),
   rdf_update_add_datatype(t, r, r),
   rdf_update_prefix(t, +, +).





%! rdf_update(:From_0, :To_0) is det.
%! rdf_update(:From_0, :To_0, -N:nonneg) is det.

rdf_update(From_0, To_0) :-
  rdf_update(From_0, To_0, N),
  debug(rdf(update), "~D updates", [N]).


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



%! rdf_update_add_datatype(+B, +P:rdf_predicate, +D:datatype_iri) is det.

rdf_update_add_datatype(B, P, D) :-
  rdf_update(
    tp(B, S, P, Term),
    (
      term_lexical_form_(Term, Lex),
      assert_triple(B, S, P, literal(type(D,Lex))),
      tp_retractall(B, S, P, Lex)
    )
  ).

term_lexical_form_(Lit, Lex) :-
  rdf_is_literal(Lit), !,
  rdf_literal_lexical_form(Lit, Lex).
term_lexical_form_(Lex, Lex).



%! rdf_update_prefix(+B, +Prefix1:compound, +Prefix2:atom) is det.

rdf_update_prefix(B, object(Prefix1), Prefix2) :- !,
  rdf_update(
    (
      tp(B, S, P, O1),
      rdf_is_iri(O1),
      atom_concat(Prefix1, Local, O1)
    ),
    (
      atom_concat(Prefix2, Local, O2),
      assert_triple(B, S, P, O2),
      tp_retractall(B, S, P, O1)
    )
  ).
rdf_update_prefix(B, predicate(Prefix1), Prefix2) :- !,
  rdf_update(
    (
      tp(B, S, P1, O),
      atom_concat(Prefix1, Local, P1)
    ),
    (
      atom_concat(Prefix2, Local, P2),
      assert_triple(B, S, P2, O),
      tp_retractall(B, S, P1, O)
    )
  ).
rdf_update_prefix(B, subject(Prefix1), Prefix2) :-
  rdf_update(
    (
      tp(B, S1, P, O),
      rdf_is_iri(S1),
      atom_concat(Prefix1, Local, S1)
    ),
    (
      atom_concat(Prefix2, Local, S2),
      assert_triple(B, S2, P, O),
      tp_retractall(B, S1, P, O)
    )
  ).
