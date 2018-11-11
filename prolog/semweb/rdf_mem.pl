:- module(
  rdf_mem,
  [
    rdf_assert_list/2,                   % +L, -BNode
    rdf_assert_list/3,                   % +L, -BNode, +G
    rdf_assert_list_triple/3,            % +S, +P, +L
    rdf_assert_list_triple/4,            % +S, +P, +L, +G
    rdf_assert_shape/3,                  % +S, +Shape, +G
    rdf_assert_shape/4,                  % +S, +Shape, +G, -Geometry
    rdf_assert_triple/1,                 % +Tuple
    rdf_assert_triple/3,                 % +S, +P, +O
    rdf_assert_triple/4,                 % +S, +P, +O, +G
    rdf_container_membership_property/1, % ?P
   %rdf_graph/1,                         % ?G
    rdf_load_file/1,                     % +File
    rdf_load_file/2,                     % +File, +Options
   %rdf_predicate/1,                     % ?P
    rdf_read_prefixes/2,                 % +File, -PrefixMap
   %rdf_reset_db/0,
   %rdf_retract_graph/1,                 % ?G
   %rdf_retractall_triples/3,            % ?S, ?P, ?O
   %rdf_retractall_triples/4,            % ?S, ?P, ?O, ?G
    rdf_save_file/1,                     % +File
    rdf_save_file/2,                     % +File, +Options
   %rdf_transaction/1,                   % :Goal_0
    rdf_triple/1,                        % ?Triple
   %rdf_triple/3,                        % ?S, ?P, ?O
    rdf_triple/4,                        % ?S, ?P, ?O, ?G
    rdf_triple_value/3,                  % ?S, ?P, -Value
    rdf_triple_value/4,                  % ?S, ?P, -Value, ?G
    rdf_update/4,                        % ?S, ?P, ?O, +Action
    rdf_update/5                         % ?S, ?P, ?O, ?G, +Action
  ]
).

/** <module> Memory-based RDF storage

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(option)).
:- reexport(library(semweb/rdf_db), [
     rdf/3 as rdf_triple,
     rdf_graph/1,
     rdf_reset_db/0,
     rdf_retractall/3 as rdf_retractall_triples,
     rdf_retractall/4 as rdf_retractall_triples,
     rdf_transaction/1,
     rdf_unload_graph/1 as rdf_retract_graph
   ]).
:- reexport(library(semweb/rdf11), [
     rdf_predicate/1
   ]).
:- use_module(library(semweb/rdf_ntriples), []).
:- use_module(library(semweb/rdfa), []).
:- use_module(library(semweb/turtle), []).
:- use_module(library(sgml)).
:- use_module(library(uri)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- use_module(library(file_ext)).
:- use_module(library(media_type)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_media_type)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(semweb/turtle)).

:- maplist(rdf_register_prefix, [geo,rdf]).

:- multifile
    rdf_api:assert_triple_/4,
    rdf_api:retractall_triples_/4,
    rdf_api:triple_/4,
    rdf_api:triple_count_/5.

rdf_api:assert_triple_(mem(G), S, P, O) :-
  rdf_assert_triple(S, P, O, G).

rdf_api:retractall_triples_(mem(G), S, P, O) :-
  rdf_retractall_triples(S, P, O, G).

rdf_api:triple_(mem(G), S, P, O) :-
  rdf_triple(S, P, O, G).

rdf_api:triple_count_(mem(G), S, P, O, N) :-
  aggregate_all(count, rdf_triple(S, P, O, G), N).

:- rdf_meta
   rdf_assert_list(t, -),
   rdf_assert_list(t, -, r),
   rdf_assert_list_triple(r, r, t),
   rdf_assert_list_triple(r, r, t, r),
   rdf_assert_shape(r, +, r),
   rdf_assert_shape(r, +, r, -),
   rdf_assert_triple(t),
   rdf_assert_triple(r, r, o),
   rdf_assert_triple(r, r, o, r),
   rdf_container_membership_property(r),
   rdf_retract_graph(r),
   rdf_retractall_triples(r, r, o),
   rdf_retractall_triples(r, r, o, r),
   rdf_triple(t),
   rdf_triple(r, r, o),
   rdf_triple(r, r, o, r),
   rdf_triple_value(r, r, -),
   rdf_triple_value(r, r, -, r),
   rdf_update(r, r, o, t),
   rdf_update(r, r, o, r, t).





%! rdf_assert_list(+L:list, -BNode:bnode) is det.
%! rdf_assert_list(+L:list, -BNode:bnode, +G:rdf_graph) is det.

rdf_assert_list(L, BNode) :-
  rdf_default_graph(G),
  rdf_assert_list(L, BNode, G).


rdf_assert_list(L, BNode, G) :-
  must_be(list, L),
  rdf_transaction(rdf_assert_list_(L, BNode, G)).

rdf_assert_list_([], Nil, _) :-
  rdf_equal(rdf:nil, Nil).
rdf_assert_list_([H|T], L2, G) :-
  (var(L2) -> rdf_bnode_iri(L2) ; true),
  rdf_assert_triple(L2, rdf:type, rdf:'List', G),
  rdf_assert_triple(L2, rdf:first, H, G),
  (   T == []
  ->  rdf_assert_triple(L2, rdf:rest, rdf:nil, G)
  ;   rdf_bnode_iri(T2),
      rdf_assert_triple(L2, rdf:rest, T2, G),
      rdf_assert_list_(T, T2, G)
  ).



%! rdf_assert_list_triple(+S:rdf_nonliteral, +P:iri, +L:list) is det.
%! rdf_assert_list_triple(+S:rdf_nonliteral, +P:iri, +L:list, +G:rdf_graph) is det.

rdf_assert_list_triple(S, P, L) :-
  rdf_default_graph(G),
  rdf_assert_list_triple(S, P, L, G).


rdf_assert_list_triple(S, P, L, G) :-
  rdf_assert_list(L, BNode, G),
  rdf_assert_triple(S, P, BNode, G).



%! rdf_assert_shape(+Feature:rdf_term, +Shape:compound, +G:rdf_graph) is det.
%! rdf_assert_shape(+Feature:rdf_term, +Shape:compound, +G:rdf_graph, -Geometry:iri) is det.

rdf_assert_shape(Feature, Shape, G) :-
  rdf_assert_shape(Feature, Shape, G, _).


rdf_assert_shape(Feature, Shape, G, Geometry) :-
  rdf_bnode_iri(Geometry),
  rdf_assert_triple(Feature, geo:hasGeometry, Geometry, G),
  rdf_assert_triple(Geometry, rdf:type, geo:'Geometry', G),
  rdf_assert_triple(Geometry, geo:asWKT, Shape, G).



%! rdf_assert_triple(+Tuple:rdf_tuple) is det.
%! rdf_assert_triple(+S:rdf_nonliteral, +P:rdf_prefix, +O:rdf_term) is det.
%! rdf_assert_triple(+S:rdf_nonliteral, +P:rdf_prefix, +O:rdf_term, +G:rdf_graph) is det.

rdf_assert_triple(rdf(S,P,O)) :- !,
  rdf_assert_triple(S, P, O).
rdf_assert_triple(rdf(S,P,O,G)) :-
  rdf_assert_triple(S, P, O, G).


rdf_assert_triple(S, P, O) :-
  rdf_default_graph(G),
  rdf_assert_triple(S, P, O, G).


rdf_assert_triple(S, P, O, G) :-
  rdf_db:rdf_assert(S, P, O, G).



%! rdf_container_membership_property(+P:atom) is semidet.
%! rdf_container_membership_property(-P:atom) is nondet.

rdf_container_membership_property(P) :-
  rdf_equal(rdf:'_', Prefix),
  rdf_predicate(P),
  string_concat(Prefix, NumS, P),
  number_string(N, NumS),
  integer(N),
  N >= 0.



%! rdf_load_file(+File) is det.
%! rdf_load_file(+File, +Options:list(compound)) is det.
%
% Loads RDF from a local file.  The format is determined based on the
% file extension of File.
%
% If graph/1 is not specified as part of Options, the file URI of File
% is used as the default graph name.
%
% Ensures that blank nodes receive a unique prefix.

rdf_load_file(Spec) :-
  rdf_load_file(Spec, []).


rdf_load_file(Spec, Options1) :-
  absolute_file_name(Spec, File, [access(read)]),
  rdf_file_name_media_type(File, MediaType),
  set_graph_option(Options1, File, Options2),
  read_from_file(
    File,
    {MediaType,Options2}/[In]>>rdf_load_stream(In, MediaType, Options2)
  ).

set_graph_option(Options, File, Options) :-
  option(graph(G), Options),
  (   var(G)
  ->  % Return the graph name that is based on the file name.
      uri_file_name(G, File)
  ;   % Use the set graph name.
      true
  ).
% Add the graph/1 option, setting the graph name based on the file
% name.
set_graph_option(Options1, File, Options2) :-
  uri_file_name(G, File),
  merge_options([graph(G)], Options1, Options2).

rdf_load_stream(In, MediaType, Options1) :-
  rdf_media_type_format_(MediaType, Format), !,
  % Blank nodes must be manually assigned a unique enough prefix.
  uuid(Prefix1),
  atom_concat('_:', Prefix1, Prefix2),
  merge_options([anon_prefix(Prefix2),format(Format)], Options1, Options2),
  rdf_db:rdf_load(In, Options2).
rdf_load_stream(_, MediaType, _) :-
  existence_error(rdf_media_type, MediaType).

rdf_media_type_format_(media(application/'n-quads',[]), nquads).
rdf_media_type_format_(media(application/'n-triples',[]), ntriples).
rdf_media_type_format_(media(text/html,[]), rdfa).
rdf_media_type_format_(media(application/trig,[]), trig).
rdf_media_type_format_(media(text/turtle,[]), turtle).
rdf_media_type_format_(media(application/'rdf+xml',[]), xml).



%! rdf_read_prefixes(+File:atom, -Map:assoc) is det.

rdf_read_prefixes(File, Map) :-
  read_from_file(File, rdf_read_prefixes_stream(Map)).

rdf_read_prefixes_stream(Map, In) :-
  turtle:rdf_read_turtle(In, _, [prefixes(Pairs1)]),
  transpose_pairs(Pairs1, Pairs2),
  list_to_assoc(Pairs2, Map).



%! rdf_save_file(+File:atom) is det.
%! rdf_save_file(+File:atom, +Options:list(compound)) is det.
%
% If the file name ends in `.gz', contents will be compressed using
% GNU zip.
%
% The following options are defined:
%
%   * media_type(+compound)
%
%     The default value is `media(application/'n-quads',[])'.

rdf_save_file(File) :-
  rdf_file_name_media_type(File, MediaType),
  rdf_save_file(File, [media_type(MediaType)]).


rdf_save_file(File, Options) :-
  write_to_file(File, {Options}/[Out]>>rdf_save_stream(Out, Options)).




%! rdf_save_stream(+Out:blob) is det.
%! rdf_save_stream(+Out:blob, +Options:list(compound)) is det.

rdf_save_stream(Out) :-
  rdf_save_stream(Out, []).


rdf_save_stream(Out, Options1) :-
  select_option(media_type(MediaType), Options1, Options2, media(application/'n-quads',[])),
  media_type_encoding(MediaType, Encoding),
  rdf_save_stream_(Out, MediaType, Encoding, Options2).

% application/n-quads
rdf_save_stream_(Out, media(application/'n-quads',_), _, _) :- !,
  forall(
    rdf_triple(S, P, O, G),
    rdf_write_quad(Out, S, P, O, G)
  ).
% application/n-triples
rdf_save_stream_(Out, media(application/'n-triples',_), _, Options) :- !,
  ignore(option(graph(G), Options)),
  forall(
    rdf_triple(S, P, O, G),
    rdf_write_triple(Out, S, P, O)
  ).
% application/rdf+xml
rdf_save_stream_(Out, media(application/'rdf+xml',_), Encoding, Options1) :- !,
  must_be(oneof([iso_latin_1,utf8]), Encoding),
  merge_options([encoding(Encoding)], Options1, Options2),
  rdf_db:rdf_save(Out, Options2).
% application/trig
rdf_save_stream_(Out, media(application/trig,_), _, _) :- !,
  forall(
    rdf_graph(G),
    (
      trig_open_graph_(Out, G),
      forall(
        rdf_triple(S, P, O),
        rdf_write_triple(Out, S, P, O)
      ),
      trig_close_graph_(Out, G)
    )
  ).
% text/turtle
rdf_save_stream_(Out, media(text/turtle,_), _, Options) :-
  rdf_save_canonical_turtle(Out, Options).

trig_open_graph_(_, G) :-
  rdf_default_graph(G), !.
trig_open_graph_(Out, G) :-
  rdf_write_iri(Out, G),
  format(Out, " {\n", []).

trig_close_graph_(_, G) :-
  rdf_default_graph(G), !.
rdf_close_graph_(Out, _) :-
  format(Out, "}\n", []).



%! rdf_triple(?Triple:compound) is nondet.

rdf_triple(rdf(S,P,O)) :-
  rdf_triple(S, P, O).



%! rdf_triple(?S:rdf_nonliteral, ?P:rdf_predicate, ?O:rdf_term, ?G:rdf_graph) is nondet.

rdf_triple(S, P, O, G) :-
  pre_graph_(G, G0),
  rdf_db:rdf(S, P, O, G0),
  post_graph_(G, G0).

pre_graph_(G, _) :-
  var(G), !.
pre_graph_(G, _) :-
  rdf_default_graph(G), !.
pre_graph_(G, G) :-
  atom(G), !.
pre_graph_(G, _) :-
  type_error(rdf_graph, G).

post_graph_(G, G0:_) :- !,
  post_graph_(G, G0).
post_graph_(G, user) :- !,
  rdf_default_graph(G).
post_graph_(G, G).



%! rdf_triple_value(?S:rdf_nonliteral, ?P:rdf_prediate, -Value) is nondet.
%! rdf_triple_value(?S:rdf_nonliteral, ?P:rdf_prediate, -Value, ?G:rdf_graph) is nondet.

rdf_triple_value(S, P, Value) :-
  rdf_triple(S, P, O),
  rdf_literal_value(O, Value).


rdf_triple_value(S, P, Value, G) :-
  rdf_triple(S, P, O, G),
  rdf_literal_value(O, Value).



%! rdf_update(?S, ?P, ?O, +Action:compound) is det.
%! rdf_update(?S, ?P, ?O, ?G, +Action:compound) is det.
%
% @arg Action is either of the following compound terms:
%
%   * datatype(iri)
%   * graph(iri)
%   * language_tag(atom)
%   * object(rdf_term)
%   * predicate(iri)
%   * subject(rdf_nonliteral)

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
