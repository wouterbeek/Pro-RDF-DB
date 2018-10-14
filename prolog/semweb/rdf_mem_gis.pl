:- module(
  rdf_mem_gis,
  [
    rdf_assert_wkt/3, % +Feature, +Shape, +G
    rdf_assert_wkt/4, % +Feature, +Shape, +G, -Geometry
    rdf_triple_wkt/2, % ?Feature, ?Shape
    rdf_triple_wkt/3  % ?Feature, ?Shape, ?G
  ]
).

/** <module> RDF GIS Support

Prefix notation does not work in hooks (workaround: datatype
match in body).

Multifile hooks do not work (workaround: module prefix).

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).

:- use_module(library(gis/gis)).
:- use_module(library(gis/wkt)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).

:- maplist(rdf_register_prefix, [geo,rdf]).

:- rdf_meta
   rdf_assert_wkt(r, +, r),
   rdf_assert_wkt(r, +, r, -),
   rdf_triple_wkt(r, ?),
   rdf_triple_wkt(r, ?, r).





%! rdf_assert_wkt(+Feature:rdf_term, +Shape:compound, +G:rdf_graph) is det.
%! rdf_assert_wkt(+Feature:rdf_term, +Shape:compound, +G:rdf_graph, -Geometry:iri) is det.

rdf_assert_wkt(Feature, Shape, G) :-
  rdf_assert_wkt(Feature, Shape, G, _).


rdf_assert_wkt(Feature, Shape, G, Geometry) :-
  rdf_bnode_iri(Geometry),
  rdf_assert_triple(Feature, geo:hasGeometry, Geometry, G),
  wkt_shape_atom(Shape, Lex),
  rdf_assert_triple(Geometry, geo:asWKT, literal(type(geo:wktLiteral,Lex)), G),
  rdf_assert_triple(Geometry, rdf:type, geo:'Geometry', G).



%! rdf_triple_wkt(?Feature:rdf_term, ?Shape:compound) is nondet.
%! rdf_triple_wkt(?Feature:rdf_term, ?Shape:compound, ?G:rdf_graph) is nondet.

rdf_triple_wkt(Feature, Shape) :-
  rdf_triple_wkt(Feature, Shape, _).


rdf_triple_wkt(Feature, Shape, G) :-
  rdf_triple(Feature, geo:hasGeometry, BNode, G),
  pre_shape_(Shape, Lex),
  rdf_triple(BNode, geo:asWKT, literal(type(geo:wktLiteral,Lex)), G),
  post_shape_(Shape, Lex).

pre_shape_(Shape, _) :-
  var(Shape), !.
pre_shape_(Shape, Lex) :-
  wkt_shape_atom(Shape, Lex).

post_shape_(Shape, _) :-
  ground(Shape), !.
post_shape_(Shape, Lex) :-
  wkt_shape_atom(Shape, Lex).
