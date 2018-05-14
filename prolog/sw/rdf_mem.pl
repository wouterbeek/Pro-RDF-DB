:- module(
  rdf_mem,
  [
    rdf_assert_triple/1,                 % +Triple
    rdf_assert_triple/3,                 % +S, +P, +O
    rdf_assert_triple/4,                 % +S, +P, +O, +G
    rdf_container_membership_property/1, % ?P
    rdf_list_member/3,                   % ?X, ?L, ?G
    rdf_load_file/1,                     % +File
    rdf_load_file/2,                     % +File, +Options
   %rdf_predicate/1,                     % ?P
   %rdf_retract_graph/1,                 % ?G
   %rdf_retractall_triples/4,            % ?S, ?P, ?O, ?G
    rdf_triple/1,                        % ?Triple
   %rdf_triple/3,                        % ?S, ?P, ?O
    rdf_triple/4,                        % ?S, ?P, ?O, ?G
    rdf_triple_list_member/4,            % ?S, ?P, ?X, ?G
    rdf_update/4,                        % ?S, ?P, ?O, +Action
    rdf_update/5                         % ?S, ?P, ?O, ?G, +Action
  ]
).

/** <module> Memory-based RDF storage

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(error)).
:- reexport(library(semweb/rdf_db), [
     rdf/3 as rdf_triple,
     rdf_retractall/4 as rdf_retractall_triples,
     rdf_unload_graph/1 as rdf_retract_graph
   ]).
:- reexport(library(semweb/rdf11), [
     rdf_predicate/1
   ]).
:- use_module(library(semweb/rdf_ntriples), []).
:- use_module(library(semweb/rdfa), []).
:- use_module(library(semweb/turtle), []).
:- use_module(library(sgml)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- use_module(library(file_ext)).
:- use_module(library(sw/rdf_media_type)).
:- use_module(library(sw/rdf_term)).

:- dynamic
    rdf_mem:rdf_assert_object_hook/2.

:- multifile
    rdf_mem:rdf_assert_object_hook/2.

:- rdf_meta
   rdf_assert_triple(t),
   rdf_assert_triple(r, r, o),
   rdf_assert_triple(r, r, o, r),
   rdf_container_membership_property(r),
   rdf_list_member(o, r, r),
   rdf_retract_graph(r),
   rdf_retractall_triples(r, r, o, r),
   rdf_triple(t),
   rdf_triple(r, r, o, r),
   rdf_triple_list_member(r, r, o, r),
   rdf_update(r, r, o, t),
   rdf_update(r, r, o, r, t).





%! rdf_assert_triple(+Triple:rdf_triple) is det.
%! rdf_assert_triple(+S:rdf_subject, +P:rdf_prefix, +O:rdf_object) is det.
%! rdf_assert_triple(+S:rdf_subject, +P:rdf_prefix, +O:rdf_object, +G:rdf_graph) is det.

rdf_assert_triple(rdf(S,P,O)) :-
  rdf_assert_triple(S, P, O).


rdf_assert_triple(S, P, O1) :-
  rdf_assert_object_(O1, O2),
  rdf_db:rdf_assert(S, P, O2).


rdf_assert_triple(S, P, O1, G) :-
  rdf_assert_object_(O1, O2),
  rdf_db:rdf_assert(S, P, O2, G).

rdf_assert_object_(Term, _) :-
  var(Term), !,
  instantiation_error(Term).
% hook
rdf_assert_object_(Term, O) :-
  rdf_mem:rdf_assert_object_hook(Term, O), !.
% language-tagged string
rdf_assert_object_(String-LTag, literal(lang(LTag,Lex))) :- !,
  atom_string(Lex, String).
% @tbd Support a more convenient/uniform date/time input format.
% date/3, date_time/[6.7], month_day/2, time/3, year_month/2
rdf_assert_object_(Compound, literal(type(D,Lex))) :-
  xsd_date_time_term_(Compound), !,
  xsd_time_string(Compound, D, String),
  atom_string(Lex, String).
% nonneg/1 → xsd:nonNegativeInteger
rdf_assert_object_(nonneg(N), literal(type(D,Lex))) :- !,
  rdf_equal(D, xsd:nonNegativeInteger),
  must_be(nonneg, N),
  xsd_number_string(N, Lex).
% positive_integer/1 → xsd:positiveInteger
rdf_assert_object_(positive_integer(N), literal(type(D,Lex))) :- !,
  rdf_equal(D, xsd:positiveInteger),
  must_be(positive_integer, N),
  xsd_number_string(N, Lex).
% str/1 → xsd:string
rdf_assert_object_(str(Atomic), literal(type(D,Lex))) :- !,
  atom_string(Atomic, String),
  rdf_equal(D, xsd:string),
  atom_string(Lex, String).
% uri/1 → xsd:anyURI
rdf_assert_object_(uri(Uri), literal(type(D,Uri))) :- !,
  rdf_equal(xsd:anyURI, D).
% year/1 → xsd:gYear
rdf_assert_object_(year(Year), literal(type(D,Lex))) :- !,
  rdf_equal(D, xsd:gYear),
  atom_number(Lex, Year).
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
% atom `false' and `true' → xsd:boolean
rdf_assert_object_(Lex, literal(type(D,Lex))) :-
  memberchk(Lex, [false,true]), !,
  rdf_equal(D, xsd:boolean).
% blank node, IRI
rdf_assert_object_(O, O).

xsd_date_time_term_(date(_,_,_)).
xsd_date_time_term_(date_time(_,_,_,_,_,_)).
xsd_date_time_term_(date_time(_,_,_,_,_,_,_)).
xsd_date_time_term_(month_day(_,_)).
xsd_date_time_term_(time(_,_,_)).
xsd_date_time_term_(year_month(_,_)).



%! rdf_container_membership_property(+P:atom) is semidet.
%! rdf_container_membership_property(-P:atom) is nondet.

rdf_container_membership_property(P) :-
  rdf_equal(rdf:'_', Prefix),
  rdf_predicate(P),
  string_concat(Prefix, NumS, P),
  number_string(N, NumS),
  integer(N),
  N >= 0.



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
% file extension of File.

rdf_load_file(File) :-
  rdf_load_file(File, []).


rdf_load_file(File, Options) :-
  rdf_file_name_media_type(File, MediaType),
  call_stream_file(
    File,
    {MediaType,Options}/[In]>>rdf_load_stream(In, MediaType, Options)
  ).

rdf_load_stream(In, MediaType, Options1) :-
  rdf_media_type_format_(MediaType, Format), !,
  merge_options([anon_prefix('_:'),format(Format)], Options1, Options2),
  rdf_db:rdf_load(In, Options2).
rdf_load_stream(_, MediaType, _) :-
  existence_error(rdf_media_type, MediaType).

rdf_media_type_format_(media(application/'n-quads',[]), nquads).
rdf_media_type_format_(media(application/'n-triples',[]), ntriples).
rdf_media_type_format_(media(text/html,[]), rdfa).
rdf_media_type_format_(media(application/trig,[]), trig).
rdf_media_type_format_(media(text/turtle,[]), turtle).
rdf_media_type_format_(media(application/'rdf+xml',[]), xml).



%! rdf_triple(?Triple:compound) is nondet.

rdf_triple(rdf(S,P,O)) :-
  rdf_triple(S, P, O).



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
      rdf_retractall_triples(S, P, O1, G),
      rdf_literal(D, _, Lex, O2),
      rdf_assert_triple(S, P, O2, G)
    )
  ).
rdf_update(S, P, O, G1, graph(G2)) :- !,
  forall(
    rdf_triple(S, P, O, G1),
    (
      rdf_retractall_triples(S, P, O, G1),
      rdf_assert_triple(S, P, O, G2)
    )
  ).
rdf_update(S, P, O1, G, ltag(LTag)) :- !,
  forall(
    rdf_triple(S, P, O1, G),
    (
      rdf_update_language_tagged_string(O1, LTag, O2),
      rdf_retractall_triples(S, P, O1, G),
      rdf_assert_triple(S, P, O2, G)
    )
  ).
rdf_update(S, P, O1, G, object(O2)) :- !,
  forall(
    rdf_triple(S, P, O1, G),
    (
      rdf_retractall_triples(S, P, O1, G),
      rdf_assert_triple(S, P, O2, G)
    )
  ).
rdf_update(S, P1, O, G, predicate(P2)) :- !,
  forall(
    rdf_triple(S, P1, O, G),
    (
      rdf_retractall_triples(S, P1, O, G),
      rdf_assert_triple(S, P2, O, G)
    )
  ).
rdf_update(S1, P, O, G, subject(S2)) :- !,
  forall(
    rdf_triple(S1, P, O, G),
    (
      rdf_retractall_triples(S1, P, O, G),
      rdf_assert_triple(S2, P, O, G)
    )
  ).

rdf_update_language_tagged_string(literal(lang(_,Lex)), LTag, literal(lang(LTag,Lex))) :- !.
rdf_update_language_tagged_string(literal(type(D,Lex)), LTag, literal(lang(LTag,Lex))) :-
  rdf_equal(D, xsd:string).
