:- module(
  rdf_mem,
  [
   %rdf_graph/1,          % ?G
    rdf_load_file/1,      % +File
    rdf_load_file/2,      % +File, +Options
    rdf_read_prefixes/2,  % +File, -PrefixMap
   %rdf_reset_db/0,
   %rdf_retract_graph/1,  % ?G
    rdf_retract_graphs/0,
    rdf_save_file/1,      % +File
    rdf_save_file/2       % +File, +Options
   %rdf_transaction/1     % :Goal_0
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
     rdf_graph/1,
     rdf_reset_db/0,
     rdf_transaction/1,
     rdf_unload_graph/1 as rdf_retract_graph
   ]).
:- reexport(library(semweb/rdf11), []).
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
    rdf_api:predicate_/2,
    rdf_api:tp_/4,
    rdf_api:tp_count_/5,
    rdf_api:tp_retractall_/4.

rdf_api:assert_triple_(mem(G), S, P, O) :-
  rdf_db:rdf_assert(S, P, O, G).

rdf_api:predicate_(mem(G), P) :-
  rdf11:rdf_predicate(P),
  once(rdf_db:rdf(_, P, _, G)).

rdf_api:tp_(mem(G), S, P, O) :-
  pre_graph_(G, G0),
  rdf_db:rdf(S, P, O, G0),
  post_graph_(G, G0).

rdf_api:tp_count_(mem(G), S, P, O, N) :-
  aggregate_all(count, rdf_db:rdf(S, P, O, G), N).

rdf_api:tp_retractall_(mem(G), S, P, O) :-
  rdf_db:rdf_retractall(S, P, O, G).

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

:- rdf_meta
   rdf_retract_graph(r),
   rdf_retractall_triples(r, r, o),
   rdf_retractall_triples(r, r, o, r).





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
  set_graph_option_(Options1, File, Options2),
  read_from_file(
    File,
    {MediaType,Options2}/[In]>>rdf_load_stream_(In, MediaType, Options2)
  ).

set_graph_option_(Options, File, Options) :-
  option(graph(G), Options),
  (   var(G)
  ->  % Return the graph name that is based on the file name.
      uri_file_name(G, File)
  ;   % Use the set graph name.
      true
  ).
% Add the graph/1 option, setting the graph name based on the file
% name.
set_graph_option_(Options1, File, Options2) :-
  uri_file_name(G, File),
  merge_options([graph(G)], Options1, Options2).

rdf_load_stream_(In, MediaType, Options1) :-
  rdf_media_type_format_(MediaType, Format), !,
  % Blank nodes must be manually assigned a unique enough prefix.
  uuid(Prefix1),
  atom_concat('_:', Prefix1, Prefix2),
  merge_options([anon_prefix(Prefix2),format(Format)], Options1, Options2),
  rdf_db:rdf_load(In, Options2).
rdf_load_stream_(_, MediaType, _) :-
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



%! rdf_retract_graphs is det.

rdf_retract_graphs :-
  forall(
    rdf_graph(G),
    rdf_retract_graph(G)
  ).



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
    rdf_db:rdf(S, P, O, G),
    rdf_write_quad(Out, S, P, O, G)
  ).
% application/n-triples
rdf_save_stream_(Out, media(application/'n-triples',_), _, Options) :- !,
  ignore(option(graph(G), Options)),
  forall(
    rdf_db:rdf(S, P, O, G),
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
        rdf_db:rdf(S, P, O),
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
