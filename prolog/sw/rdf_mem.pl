:- module(
  rdf_mem,
  [
    rdf_assert_triple/4,      % +S, +P, +O, +G
    rdf_list_member/3,        % ?X, ?L, ?G
    rdf_load_file/1,          % +File
    rdf_load_file/2,          % +File, +Options
    rdf_triple/4,             % ?S, ?P, ?O, ?G
    rdf_triple_list_member/4, % ?S, ?P, ?X, ?G
   %rdf_retract_graph/1,      % ?G
   %rdf_retract_triples/4,    % ?S, ?P, ?O, ?G
    rdf_update/4,             % ?S, ?P, ?O, +Action
    rdf_update/5              % ?S, ?P, ?O, ?G, +Action
  ]
).

/** <module> Memory-based RDF storage

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(error)).
:- reexport(library(semweb/rdf_db), [
     rdf_retractall/4 as rdf_retract_triples,
     rdf_unload_graph/1 as rdf_retract_graph
   ]).
:- use_module(library(sgml)).
:- use_module(library(zlib)).

:- use_module(library(sw/rdf_term)).

:- rdf_meta
   rdf_assert_triple(r, r, o, r),
   rdf_list_member(o, r, r),
   rdf_triple(r, r, o, r),
   rdf_triple_list_member(r, r, o, r),
   rdf_update(r, r, o, t),
   rdf_update(r, r, o, r, t).





%! rdf_assert_triple(+S:rdf_subject, +P:rdf_prefix, +O:rdf_object, +G:rdf_graph) is det.

rdf_assert_triple(S, P, O1, G) :-
  rdf_assert_object_(O1, O2),
  rdf_db:rdf_assert(S, P, O2, G).

rdf_assert_object_(O) :-
  var(O), !,
  instantiation_error(O).
% language-tagged string
rdf_assert_object_(String-LTag, literal(lang(LTag,Lex))) :- !,
  atom_string(Lex, String).
% @tbd Support a more convenient/unifor date/time input format.
% date/3, date_time/[6.7], month_day/2, time/3, year_month/2
rdf_assert_object_(Compound, literal(type(D,Lex))) :-
  xsd_date_time_term_(Compound), !,
  xsd_time_string(Compound, D, String),
  atom_string(Lex, String).
% integer → xsd:integer
rdf_assert_object_(Value, literal(type(D,Lex))) :-
  integer(Value), !,
  rdf_equal(D, xsd:integer),
  atom_number(Lex, Value).
% float → xsd:double
rdf_assert_object_(Value, literal(type(D,Lex))) :-
  float(Value), !,
  rdf_equal(D, xsd:double),
  xsd_number_string(Value, String),
  atom_string(Lex, String).
% string → xsd:string
rdf_assert_object_(Value, literal(type(D,Lex))) :-
  string(Value), !,
  rdf_equal(D, xsd:string),
  atom_string(Lex, Value).
% regular typed literal
rdf_assert_object_(literal(type(D,Lex)), literal(type(D,Lex))) :- !.
% regular language-tagged string
rdf_assert_object_(literal(lang(LTag,Lex)), literal(lang(LTag,Lex))) :- !.
% blank node, IRI
rdf_assert_object_(O, O).

xsd_date_time_term_(date(_,_,_)).
xsd_date_time_term_(date_time(_,_,_,_,_,_)).
xsd_date_time_term_(date_time(_,_,_,_,_,_,_)).
xsd_date_time_term_(month_day(_,_)).
xsd_date_time_term_(time(_,_,_)).
xsd_date_time_term_(year_month(_,_)).



%! rdf_list_member(?X:rdf_term, ?L:rdf_list, ?G:rdf_graph) is nondet.

rdf_list_member(X, L, G) :-
  rdf_triple(L, rdf:first, X, G).
rdf_list_member(X, L, G) :-
  rdf_triple(L, rdf:rest, T, G),
  rdf_list_member(X, T, G).



%! rdf_load_file(+File:atom) is det.
%! rdf_load_file(+File:atom, +Options:list(compound)) is det.
%
% Loads RDF from a local file.  The format is determined based on the
% file extension of File.  The RDF serialization format can be
% overruled with the option format/1.

rdf_load_file(File) :-
  rdf_load_file(File, []).


rdf_load_file(File, Options) :-
  file_name_extension(Base, gz, File), !,
  guess_format_from_file(Base, Format, Options),
  setup_call_cleanup(
    gzopen(File, read, In),
    rdf_load_stream(In, Format, Options),
    close(In)
  ).
rdf_load_file(File, Options) :-
  guess_format_from_file(File, Format, Options),
  setup_call_cleanup(
    open(File, read, In),
    rdf_load_stream(In, Format, Options),
    close(In)
  ).

guess_format_from_file(_, Format, Options) :-
  option(format(Ext), Options),
  rdf_format_extension_(Format, Ext), !.
guess_format_from_file(File, Format, _) :-
  file_name_extension(_, Ext, File),
  rdf_format_extension_(Format, Ext), !.
guess_format_from_file(File, _, _) :-
  print_message(warning, unknown_rdf_format(File)).

rdf_load_stream(In, Format, Options1) :-
  merge_options([anon_prefix('_:'),format(Format)], Options1, Options2),
  rdf_db:rdf_load(In, Options2).

rdf_format_extension_(nquads, nq).
rdf_format_extension_(ntriples, nt).
rdf_format_extension_(rdf, html).
rdf_format_extension_(trig, trig).
rdf_format_extension_(turtle, ttl).
rdf_format_extension_(xml, rdf).



%! rdf_triple(?S:rdf_subject, ?P:rdf_predicat, ?O:rdf_object, ?G:rdf_graph) is nondet.

rdf_triple(S, P, O, G) :-
  pre_graph_(G, G0),
  rdf_db:rdf(S, P, O, G0),
  post_graph_(G, G0).

pre_graph_(G, _) :-
  var(G), !.
pre_graph_(G, G) :-
  atom(G), !.
pre_graph_(G, _) :-
  type_error(rdf_graph, G).

post_graph_(G, G0:_) :- !,
  G = G0.
post_graph_(G, G).



%! rdf_triple_list_member(?S:rdf_subject, ?P:rdf_predicate, ?X:rdf_term, ?G:rdf_graph) is nondet.

rdf_triple_list_member(S, P, X, G) :-
  ground(X), !,
  rdf_list_member(X, L, G),
  rdf_triple(S, P, L, G).
rdf_triple_list_member(S, P, X, G) :-
  rdf_triple(S, P, L, G),
  rdf_list_member(X, L, G).



%! rdf_update(?S, ?P, ?O, +Action:compound) is det.
%! rdf_update(?S, ?P, ?O, ?G, +Action:compound) is det.
%
% @arg Action is either of the following compound terms:
%
%   * datatype(iri)
%   * graph(iri)
%   * language_tag(atom)
%   * object(rdf_object)
%   * predicate(rdf_predicate)
%   * subject(rdf_subject)

rdf_update(S, P, O, Action) :-
  rdf_update(S, P, O, _, Action).


rdf_update(S, P, O1, G, datatype(D)) :- !,
  forall(
    rdf_triple(S, P, O1, G),
    (
      rdf_literal_lexical_form(O1, Lex),
      rdf_retract_triples(S, P, O1, G),
      rdf_literal(D, _, Lex, O2),
      rdf_assert_triple(S, P, O2, G)
    )
  ).
rdf_update(S, P, O, G1, graph(G2)) :- !,
  forall(
    rdf_triple(S, P, O, G1),
    (
      rdf_retract_triples(S, P, O, G1),
      rdf_assert_triple(S, P, O, G2)
    )
  ).
rdf_update(S, P, O1, G, ltag(LTag)) :- !,
  forall(
    rdf_triple(S, P, O1, G),
    (
      rdf_update_language_tagged_string(O1, LTag, O2),
      rdf_retract_triples(S, P, O1, G),
      rdf_assert_triple(S, P, O2, G)
    )
  ).
rdf_update(S, P, O1, G, object(O2)) :- !,
  forall(
    rdf_triple(S, P, O1, G),
    (
      rdf_retract_triples(S, P, O1, G),
      rdf_assert_triple(S, P, O2, G)
    )
  ).
rdf_update(S, P1, O, G, predicate(P2)) :- !,
  forall(
    rdf_triple(S, P1, O, G),
    (
      rdf_retract_triples(S, P1, O, G),
      rdf_assert_triple(S, P2, O, G)
    )
  ).
rdf_update(S1, P, O, G, subject(S2)) :- !,
  forall(
    rdf_triple(S1, P, O, G),
    (
      rdf_retract_triples(S1, P, O, G),
      rdf_assert_triple(S2, P, O, G)
    )
  ).

rdf_update_language_tagged_string(literal(lang(_,Lex)), LTag, literal(lang(LTag,Lex))).
