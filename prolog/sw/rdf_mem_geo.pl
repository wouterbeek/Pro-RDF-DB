:- module(
  rdf_mem_geo,
  [
    rdf_assert_wkt/3, % +Feature, +Shape, +G
    rdf_assert_wkt/4, % +Feature, +Shape, +G, -Geometry
    rdf_triple_wkt/2, % ?Feature, ?Shape
    rdf_triple_wkt/3  % ?Feature, ?Shape, ?G
  ]
).

/** <module> RDF Geography Plugin

Prefix notation does not work in hooks (workaround: datatype
match in body).

Multifile hooks do not work (workaround: module prefix).

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(dcg)).
:- use_module(library(geo/wkt_generate)).
:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).

:- rdf_assert_prefix(geo, 'http://www.opengis.net/ont/geosparql#').

:- rdf_meta
   rdf_assert_wkt(r, +, r),
   rdf_assert_wkt(r, +, r, -),
   rdf_triple_wkt(r, ?),
   rdf_triple_wkt(r, ?, r).





%! rdf_assert_wkt(+Feature, +Shape, +G) is det.
%! rdf_assert_wkt(+Feature, +Shape, +G, -Geometry) is det.

rdf_assert_wkt(Feature, Shape, G) :-
  rdf_assert_wkt(Feature, Shape, G, _).


rdf_assert_wkt(Feature, Shape, G, Geometry) :-
  rdf_bnode_iri(Geometry),
  rdf_assert_triple(Feature, geo:hasGeometry, Geometry, G),
  atom_phrase(wkt_generate(Shape), Lex),
  rdf_assert_triple(Geometry, geo:asWKT, literal(type(geo:wktLiteral,Lex)), G),
  rdf_assert_triple(Geometry, rdf:type, geo:'Geometry', G).



%! rdf_triple_wkt(?Feature, ?Shape) is nondet.
%! rdf_triple_wkt(?Feature, ?Shape, ?G) is nondet.

rdf_triple_wkt(Feature, Shape) :-
  rdf_triple_wkt(Feature, Shape, _).


rdf_triple_wkt(Feature, Shape, G) :-
  rdf_triple(Feature, geo:hasGeometry, BNode, G),
  pre_object_(Shape, Lex),
  rdf_triple(BNode, geo:asWKT, literal(type(geo:wktLiteral,Lex)), G),
  post_object_(Shape, Lex).

pre_object_(Shape, _) :-
  var(Shape), !.
pre_object_(Shape, Lex) :-
  atom_prase(wkt_generate(Shape), Lex).

post_object_(Shape, _) :-
  ground(Shape), !.
post_object_(Shape, Lex) :-
  atom_phrase(wkt_parse(Shape), Lex).
