:- module(
  geonovum_datasets,
  [
    init_beeldbank/0,
    init_bgt/0,
    init_cbs2015/0,
    init_gemeente/0,
    init_monumenten/0
  ]
).

/** <module> GeoNovum testbed datasets

This generates the following datasets:

  * Basisregistratie Grootschalige Topografie (BGT)

  * Beeldbank

  * Gemeente geschiedenis

  * Wijk- en Buurtkaart 2013-2015 (CBS)

  * Monumenten

@author Wouter Beek
@tbd The same bgt:id appears multiple times.
@version 2016/05-2016/07
*/

:- set_prolog_stack(global, limit(7*10**9)).

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(conv/csv2rdf)).
:- use_module(library(conv/json2rdf)).
:- use_module(library(conv/xml2rdf)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(filesex)).
:- use_module(library(http/http_download)).
:- use_module(library(http/http_io)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/geold)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io)).
:- use_module(library(os/thread_ext)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(q/q_wgs84)).
:- use_module(library(q/q_wkt)).
:- use_module(library(q/qb)).
:- use_module(library(q/qu)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(thread)).
:- use_module(library(uri)).
:- use_module(library(xml/xml_dom)).
:- use_module(library(xpath)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- qb_alias(beeldbank, 'http://beeldbank.nl/').
:- qb_alias(bgt, 'http://bgt.nl/').
:- qb_alias(cbs, 'http://cbs.nl/').
:- qb_alias(gemeente, 'http://gemeentechiedenis.nl/').
:- qb_alias(monumenten, 'http://monumenten.nl/').

:- multifile
  q_fs:q_source2store_hook/4.

:- rdf_meta
   bgt_class(r).

:- debug(io).
:- debug(qu(_)).

init_beeldbank :-
  q_source2store(beeldbank).
init_bgt :-
  q_source2store(bgt).
init_cbs2015 :-
  q_source2store(cbs2015).
init_gemeente :-
  q_source2store(gemeente).
init_monumenten :-
  q_source2store(monumenten).





% DATA %

q_fs:q_source2store_hook(beeldbank, File, Graph, G) :-
  q_store_call(
    xml2rdf_stream(File, [record], _{entry_name:Graph,ltag_attr:language}),
    G
  ),
  M1 = rdf,
  M2 = rdf,
  rdf_equal(beeldbank:data, G),
  q_load(M1, G),
  qu_rm_col(M1, M2, ex:'acquisition.source.type_text', G),
  qu_rm_col(M1, M2, ex:'edit.date', G),
  qu_rm_col(M1, M2, ex:'edit.time', G),
  qu_rm_col(M1, M2, ex:'input.date', G),
  qu_replace_string(M1, M2, _, ex:'monument_number.x_coordinates', ",", G, "."),
  qu_change_datatype(M1, M2, ex:'monument_number.x_coordinates', G, xsd:float),
  qu_replace_string(M1, M2, _, ex:'monument_number.y_coordinates', ",", G, "."),
  qu_change_datatype(M1, M2, ex:'monument_number.y_coordinates', G, xsd:float),
  qu_replace_flat_wgs84_point(M1, M2, ex:'monument_number.x_coordinates', ex:'monument_number.y_coordinates', G).
  %qu_split_string(M1, M2, ex:'monument.complex_number', G, " "),
  %qu_change_datatype(M1, M2, ex:'monument.county', G, xsd:nonNegativeInteger).



bgt_load_data(G) :-
  bgt_scrape_data(G),
  M1 = rdf,
  M2 = rdf,
  qu_replace_nested_wkt_geometry(M1, M2, G),
  qu_geold_flatten_properties(M1, M2, G),
  qu_geold_rm_feature_collections(M1, M2, G),
  qu_change_datatype(M1, M2, bgt:eindRegistratie, G, xsd:dateTime),
  qu_change_datatype(M1, M2, bgt:inOnderzoek, G, xsd:boolean),
  qu_change_datatype(M1, M2, bgt:'LV-publicatiedatum', G, xsd:dateTime),
  qu_change_datatype(M1, M2, bgt:objectBeginTijd, G, xsd:date),
  qu_change_datatype(M1, M2, bgt:objectEindTijd, G, xsd:date),
  qu_change_datatype(M1, M2, bgt:tijdstipRegistratie, G, xsd:dateTime),
  qu_rm(M1, M2, _, rdf:type, geold:'Feature', G),
  q_save(M2, G).


bgt_scrape_data(G) :-
  bgt_class(C),
  bgt_scrape_data(C, 1, G),
  fail.
bgt_scrape_data(_) :-
  debug(conv(bgt), "[DONE] Scraping BGT", []).


% @bug In the original data ("geometry" key appears twice in JSON object.
bgt_scrape_data('http://www.ldproxy.net/bgt/Pand', 6, _) :- !.
bgt_scrape_data(C, N1, G) :-
  debug(conv(bgt), "~w: ~D", [C,N1]),
  rdf_global_id(bgt:Local, C),
  atomic_list_concat(['',bgt,Local], /, Path),
  uri_query_components(Query, [page=N1]),
  uri_components(Iri, uri_components(http,'www.ldproxy.net',Path,Query,_)),
  http_retry_until_success(
    geold_tuples(Iri, bgt, _{}, _{'@type': C}, Triples)
  ),
  %%%%(debugging(conv(bgt)) -> q_print_triples(Triples) ; true),
  (   Triples = []
  ->  true
  ;   maplist({G}/[Triple]>>qb(rdf, Triple, G), Triples),
      N2 is N1 + 1,
      bgt_scrape_data(C, N2, G)
  ).


bgt_class(bgt:'Bak').
bgt_class(bgt:'BegroeidTerreindeel').
bgt_class(bgt:'Bord').
bgt_class(bgt:'GebouwInstallatie').
bgt_class(bgt:'Kast').
bgt_class(bgt:'Kunstwerkdeel').
bgt_class(bgt:'Mast').
bgt_class(bgt:'OnbegroeidTerreindeel').
bgt_class(bgt:'OndersteunendWaterdeel').
bgt_class(bgt:'OndersteunendWegdeel').
bgt_class(bgt:'OpenbareRuimteLabel').
bgt_class(bgt:'Overbruggingsdeel').
bgt_class(bgt:'OverigBouwwerk').
bgt_class(bgt:'Paal').
bgt_class(bgt:'Pand').
bgt_class(bgt:'Put').
bgt_class(bgt:'Scheiding').
bgt_class(bgt:'Sensor').
bgt_class(bgt:'Straatmeubilair').
bgt_class(bgt:'VegetatieObject').
bgt_class(bgt:'Waterdeel').
bgt_class(bgt:'Waterinrichtingselement').
bgt_class(bgt:'Wegdeel').
bgt_class(bgt:'Weginrichtingselement').



q_fs:q_source2store_hook(cbs, File, Graph, G) :-
  q_store_call(csv2rdf_stream(source(File), [entry_name(Graph)]), G),
  M1 = rdf,
  M2 = rdf,
  q_load(M1, G),
  % “Het cijfer is onbekend, onvoldoende betrouwbaar of geheim.”
  qu_rm_null(M1, M2, _, "-99999999"^^xsd:string, G),
  % Typo?
  qu_rm_null(M1, M2, _, "-99999999.0"^^xsd:string, G),
  qu_rm_null(M1, M2, _, ""^^xsd:string, G),
  qu_rm_error(M1, M2, _, cbs:'WATER', "B"^^xsd:string, G),
  qu_change_lex(M1, M2, cbs:'WATER', G, cbs_boolean),
  qu_change_datatype(M1, M2, cbs:'WATER', G, xsd:boolean),
  q_save(M2, G).



gemeente_load_data(G) :-
  q_store_call(
    json2rdf_stream(
      source('gemeentegeschiedenis.pits.ndjson.gz'),
      _{alias: gemeente}
    ),
    G
  ),
  M1 = rdf,
  M2 = rdf,
  q_load(M1, G),
  qu_rm_col(M1, M2, gemeente:data_amsterdamse_code, G),
  qu_replace_predicate(M1, M2, gemeente:type, G, rdf:type),
  qu_rm_null(M1, M2, gemeente:validSince, "0"^^xsd:string, G),
  qu_change_datatype(M1, M2, gemeente:validSince, G, xsd:gYear),
  qu_change_datatype(M1, M2, gemeente:validUntil, G, xsd:gYear),
  qu_lex_to_iri(M1, M2, rdf:type, gemeente, G, type_to_local0),
  qu_lex_to_iri(M1, M2, gemeente:geometry_type, wkt, G, lowercase),
  qu_replace_flat_wkt_geometry(M1, M2, gemeente:geometry_type, gemeente:geometry_coordinates, G),
  qu_add_ltag(M1, M2, gemeente:name, nl, G),
  q_save(M2, G).



q_fs:q_source2store_hook(monumenten, File1, Graph, G) :-
  q_store_file(monumenten, File2),
  rdf_change_format(
    File1,
    File2,
    [entry_name(Graph),from_format(turtle),to_format(ntriples)]
  ),
  M1 = rdf,
  M2 = rdf,
  q_load(M1, G),
  qu_change_datatype(M1, M2, wgs84:lat, G, xsd:float),
  qu_change_datatype(M1, M2, wgs84:long, G, xsd:float),
  qu_replace_flat_wgs84_point(M1, M2, G),
  q_save(M2, G).
  %qu_lex_to_iri(M1, M2, dbo:municipality, monumenten, G, spaces_to_underscores),





% GRAMMAR %

cbs_boolean, "true" --> "JA".
cbs_boolean, "false" --> "NEE".



%! buurt_code(
%!   -Gemeente:between(0,9999),
%!   -Wijk:between(0,99),
%!   -Buurt:between(0,99)
%! )// is det.
%
% @tbd Fictieve buurten ‘9998’ en ‘9997’.

buurt_code(Gemeente, Wijk, Buurt) -->
  "WK",
  gemeente_code0(Gemeente),
  wijk_code0(Wijk),
  buurt_code0(Buurt).


buurt_code0(Buurt) -->
  #(2, digit, Ds3),
  {pos_sum(Ds3, Buurt)}.



%! gemeente_code(-Gemeente:between(0,9999))// is det.

gemeente_code(Gemeente) -->
  "GM",
  gemeente_code0(Gemeente).


gemeente_code0(Gemeente) -->
  #(4, digit, Ds1),
  {pos_sum(Ds1, Gemeente)}.



spaces_to_underscores, "_" -->
  " ", !,
  spaces_to_underscores.
spaces_to_underscores, [C] -->
  [C], !,
  spaces_to_underscores.
spaces_to_underscores --> "".



type_to_local0, Cs -->
  "hg:",
  rest(Cs).



%! wijk_code(-Gemeente:between(0,9999), -Wijk:between(0,99))// is det.

wijk_code(Gemeente, Wijk) -->
  "WK",
  gemeente_code0(Gemeente),
  wijk_code0(Wijk).


wijk_code0(Wijk) -->
  #(2, digit, Ds2),
  {pos_sum(Ds2, Wijk)}.





% VOCABULARY %

bgt_load_vocab(G) :-
  M = rdf,
  Source = 'http://definities.geostandaarden.nl/resource?subject=http%3A%2F%2Fdefinities.geostandaarden.nl%2Fconcepten%2Fid%2Fstandaard%2Fimgeo&date=&lang=nl',
  html_download(Source, Dom),
  forall((
    xpath(Dom, //table/tr, Tr),
    xpath_chk(Tr, //td(2), Td2),
    xpath_chk(Td2, //a(@href), Iri),
    xpath_chk(Td2, //a(content), [Lbl]),
    xpath_chk(Tr, //td(3)//span(content), [Comment])
  ), (
    qb_label(M, Iri, Lbl@nl, G),
    qb_comment(M, Iri, Comment@nl, G)
  )),
  q_save(M, G).



cbs_load_vocab(G) :-
  M = rdf,
  
  % A_BST_B
  qb_label(M, cbs:'A_BST_B', "Personenauto’s; brandstof benzine"@nl, G),
  qb_label(M, cbs:'A_BST_B', "Het aantal personenauto’s rijdend op benzine.  Benzine is een mengsel van koolwaterstof dat wordt gebruikt als brandstof voor benzinemotoren."@nl, G),

  % A_BST_NB
  qb_label(M, cbs:'A_BST_NB', "Personenauto’s; overige brandstof"@nl, G),
  qb_comment(M, cbs:'A_BST_NB', "Het aantal personenauto’s met overige brandstof.  Hieronder vallen: diesel, LPG, elektriciteit (incl. hybride), waterstof, alcohol en CNG."@nl, G),
  
  % A_LFTJ6J
  qb_label(M, cbs:'A_LFTJ6J', "Personenauto’s; jonger dan 6 jaar"@nl, G),
  qb_comment(M, cbs:'A_LFTJ6J', "Het aantal personenauto’s van jonger dan 6 jaar.  Deze leeftijd is afgeleid van het bouwjaar van het voertuig."@nl, G),

  % A_LFTO6J
  qb_label(M, cbs:'A_LFTO6J', "Personenauto’s; 6 jaar en ouder"@nl, G),
  qb_comment(M, cbs:'A_LFTO6J', "Het aantal personenauto’s van 6 jaar en ouder.  Deze leeftijd is afgeleid van het bouwjaar van het voertuig."@nl, G),

  % A_SOZ_OW
  qb_label(M, cbs:'A_SOZ_OW', "Personen met een AOW-uitkering totaal"@nl, G),
  qb_comment(M, cbs:'A_SOZ_OW', "Personen die een basispensioen van de Rijksoverheid ontvangen op grond van de Algemene Ouderdomswet (AOW).  De AOW is een algemene, de gehele bevolking omvattende, verplichte verzekering die personen met de AOW-gerechtigde leeftijd een inkomen garandeert.  In het Nederlandse sociale zekerheidsstelsel is dit een volksverzekering.  In principe is iedereen die nog niet de AOW-gerechtige leeftijd heeft bereikt en in Nederland woont, verzekerd voor de AOW.  Ook degenen die niet in Nederland wonen, maar in Nederland in dienstbetrekking arbeid verrichten waarover loonbelasting wordt betaald, zijn verzekerd.  Voor perioden die men in het buitenland woont, kan men zich verzekeren tegen verlies van aanspraak op een AOW-uitkering.  Een uitkering kan, binnen het kader van de wet Beperking export uitkeringen (wet BEU), naar het buitenland worden overgemaakt.  De AOW-leeftijd is de leeftijd waarop recht is ontstaan op het basispensioen van de Rijksoverheid op grond van de Algemene Ouderdomswet (AOW).  Tot 1 januari 2013 was de AOW-leeftijd 65 jaar. Vanaf die datum gaat de AOW-leeftijd jaarlijks met één of meerdere maanden omhoog. Zo was de AOW-leeftijd in 2013 65 jaar en één maand, in 2014 was die leeftijd 65 jaar en twee maanden.  De AOW-leeftijd wordt vanaf 2016 in stappen van 3 maanden verhoogd en vanaf 2018 in stappen van 4 maanden.  Daarmee wordt de AOW-leeftijd 66 jaar in 2018 en 67 jaar in 2021.  Vanaf 2022 wordt de AOW-leeftijd gekoppeld aan de levensverwachting.  Het betreft in 2015 voorlopige cijfers."@nl, G),

  % AANTAL_HH
  qb_label(M, cbs:'AANTAL_HH', "Huishoudens totaal"@nl, G),
  qb_comment(M, cbs:'AANTAL_HH', "Het aantal particuliere huishoudens op 1 januari.  Dit is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  De cijfers zijn tot en met het jaar 2009 afgerond op tientallen.  Vanaf het jaar 2010 zijn ze aselect afgerond op vijftallen."@nl, G),
  
  % AANT_INW
  qb_label(M, cbs:'AANT_INW', "Aantal inwoners"@nl, G),
  qb_comment(M, cbs:'ANNT_INW', "Het aantal inwoners op 1 januari.  Dit gegeven is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  De standcijfers van het aantal inwoners kunt u niet gebruiken voor een correcte weergave van de ontwikkeling in de tijd.  De grenzen of codes van wijken en buurten kunnen jaarlijks wijzigen waardoor adressen van een andere code worden voorzien.  Om redenen van statistische geheimhouding zijn de aantallen op wijk- en buurtniveau aselect afgerond op veelvouden van 5.  Bij aselect afronden wordt door loten bepaald of een getal naar boven of naar beneden wordt afgerond.  De daarbij gehanteerde kansen zijn omgekeerd evenredig met de afrondverschillen.  Gemiddeld wordt een getal hierdoor op zichzelf afgerond.  Het gemiddelde afrondverschil per getal is evenwel groter dan het geval is bij afronding op het dichtstbijzijnde veelvoud van 5.  Door afrondverschillen is de som van afgeronde getallen niet altijd gelijk aan de afgeronde som.  Hierdoor kan het voorkomen dat wanneer een wijk uit één buurt bestaat of een gemeente uit één wijk, de afzonderlijke afgeronde aantallen niet overeenkomen."@nl, G),

  % AANT_MAN
  qb_label(M, cbs:'AANT_MAN', "Mannen"@nl, G),
  qb_comment(M, cbs:'AANT_MAN', "Het aantal mannen op 1 januari.  Dit is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA)."@nl, G),

  % AANT_VROUW
  qb_label(M, cbs:'AANT_VROUW', "Vrouwen"@nl, G),
  qb_comment(M, cbs:'AANT_VROUW', "Het aantal vrouwen op 1 januari.  Dit is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA)."@nl, G),

  % AO_UIT_TOT
  qb_label(M, cbs:'AO_UIT_TOT', "AO-uitkeringen totaal"@nl, G),
  qb_comment(M, cbs:'AO_UIT_TOT', "Personen die een arbeidsongeschiktheidsuitkering ontvangen op grond van de Wet op de arbeidsongeschiktheidsverzekering (WAO), de Wet arbeidsongeschiktheidsverzekering zelfstandigen (WAZ), de Wet werk en Inkomen naar arbeidsvermogen (WIA), de Wet arbeidsongeschiktheidsvoorziening jonggehandi-capten (Wajong) en de Wet werk en arbeidsondersteuning jonggehandicapten (wet Wajong).  De wet op de arbeidsongeschiktheidsverzekering (WAO) heeft als doel om personen in loondienst te verzekeren van een loonvervangende uitkering bij langdurige arbeidsongeschiktheid.  De wet arbeidsongeschiktheidsverzekering zelfstandigen (WAZ) is een verplichte verzekering voor zelfstandigen, beroepsbeoefenaren, directeuren-grootaandeelhouders en meewerkende echtgenoten tegen de financiële gevolgen van langdurige arbeidsongeschiktheid.  De WAZ is met ingang van 1 augustus 2004 geblokkeerd.  De wet arbeidsongeschiktheidsvoorziening jonggehandicapten (Wajong) is een wettelijke voorziening in de financiële gevolgen van langdurige arbeidsongeschiktheid van mensen die geen aanspraak kunnen maken op de WAO/WIA omdat er geen arbeidsverleden is opgebouwd.  Dit zijn mensen die arbeidsongeschikt zijn voor de dag dat zij 17 jaar worden of na hun 17e jaar arbeidsongeschikt worden en een opleiding of studie volgen.  Met ingang van 1 januari 2010 is de Wet werk en arbeidsondersteuning jonggehandicapten (Wet Wajong) in werking getreden.  In tegenstelling tot de ‘oude’ Wajong hebben jongeren met een ziekte of handicap in de eerste plaats recht op hulp bij het vinden en houden van werk.  Daaraan gekoppeld kunnen ze een inkomensondersteuning krijgen.  De ‘oude’ Wajong blijft gelden voor jongeren die voor 1 januari 2010 een uitkering hebben aangevraagd.  De werk en inkomen naar arbeidsvermogen (WIA) geeft werknemers die na een wachttijd van twee jaar nog minstens 35 procent arbeidsongeschikt zijn, recht op een uitkering.  De wet is zo opgezet dat een persoon gestimuleerd wordt om naar vermogen te werken.  De WIA kent twee regelingen: de regeling inkomensvoorziening volledig arbeidsongeschikten (IVA) en de regeling werkhervatting gedeeltelijk arbeidsgeschikten (WGA).  De IVA regelt een loonvervangende uitkering voor werknemers die volledig en duurzaam arbeidsongeschikt zijn.  De WGA regelt een aanvulling op het met arbeid verdiende inkomen of een minimumuitkering als men niet of onvoldoende werkt.  Het betreft in 2015 voorlopige cijfers."@nl, G),

  % AUTO_HH
  qb_label(M, cbs:'AUTO_HH', "Personenauto’s per huishouden"@nl, G),
  qb_comment(M, cbs:'AUTO_HH', "Het aantal personenauto’s per (particulier) huishouden op 1 januari.  De personenauto’s worden regionaal ingedeeld met behulp van de kentekenregistratie.  Personenauto’s die geregistreerd staan op het adres van het lease- of verhuurbedrijf vertekenen daarom de autodichtheid per huishouden.  Het aantal personenauto’s per huishouden is vermeld bij minimaal 50 huishoudens en bij een waarde van maximaal 2,5 personenauto’s per huishouden."@nl, G),

% AUTO_LAND
  qb_label(M, cbs:'AUTO_LAND', "Personenauto’s naar oppervlakte"@nl, G),
  qb_comment(M, cbs:'AUTO_LAND', "Het aantal personenauto’s per km2 land op 1 januari.  De personenauto’s worden regionaal ingedeeld met behulp van de kentekenregistratie.  Personenauto’s die geregistreerd staan op het adres van het lease- of verhuurbedrijf vertekenen daarom de autodichtheid per oppervlakte.  Het aantal personenauto’s per km 2 is vermeld bij minimaal 50 huishoudens en bij een waarde van maximaal 2,5 personenauto’s per huishouden."@nl, G),

  % AUTO_TOT
  qb_label(M, cbs:'AUTO_TOT', "Personenauto’s totaal"@nl, G),
  qb_comment(M, cbs:'AUTO_TOT', "Het aantal motorvoertuigen ingericht voor het vervoer van ten hoogste 8 passagiers (exclusief bestuurder) op 1 januari.  De cijfers zijn afgerond op vijftallen."@nl, G),

  % BEDR_AUTO
  qb_label(M, cbs:'BEDR_AUTO', "Bedrijfsmotorvoertuigen totaal"@nl, G),
  qb_comment(M, cbs:'BEDR_AUTP', "Het aantal bestelauto’s, vrachtauto’s, trekkers (motorvoertuigen ingericht voor het trekken van opleggers), speciale voertuigen (bedrijfsauto’s voor bijzondere doeleinden zoals bijvoorbeeld brandweerauto’s, reinigingsauto’s, takelwagens) en autobussen op 1 januari.  De cijfers zijn afgerond op vijftallen."@nl, G),

  % BEV_DICHTH
  qb_label(M, cbs:'BEV_DICHTH', "Bevolkingsdichtheid"@nl, G),
  qb_comment(M, cbs:'BEV_DICHTH', "Het (niet afgeronde) aantal inwoners op 1 januari gedeeld door de (niet afgeronde) landoppervlakte.  Wanneer een buurt minder dan 10 inwoners telt, is dit gegeven geheimgehouden."@nl, G),

  % BU_CODE
  qb_label(M, cbs:'BU_CODE', "Buurtcode"@nl, G),
  qb_comment(M, cbs:'BU_CODE', "Voor de codering van de binnen wijken onderscheiden buurten is een code van acht posities opgenomen.  Gemeentecode (4) + wijkcode (2) + buurtcode (2)."@nl, G),

  % BU_NAAM
  qb_label(M, cbs:'BU_NAAM', "Buurtnaam"@nl, G),
  qb_comment(M, cbs:'BU_NAAM', "De buurtnaam is opgegeven door de gemeente die hiervan eigenaar is."@nl, G),

  % DEK_PREC
  qb_label(M, cbs:'DEK_PERC', "Meest voorkomende postcode; dekkingspercentage"@nl, G),
  qb_comment(M, cbs:'DEK_PERC', "Indicatie (in zes klassen) van het percentage adressen in een buurt met de meest voorkomende postcode.  Dit percentage is ontleend aan het Geografisch Basisregister (GBR, definitieve versie).  De volgende klassenindeling is gehanteerd:  (1) >90% van de adressen heeft dezelfde vermelde numerieke postcode;  (2) 81–90% van de adressen heeft dezelfde vermelde numerieke postcode;  (3) 71–80% van de adressen heeft dezelfde vermelde numerieke postcode;  (4) 61–70% van de adressen heeft dezelfde vermelde numerieke postcode;  (5) 51–60% van de adressen heeft dezelfde vermelde numerieke postcode; (6) 50% of minder van de adressen heeft dezelfde vermelde numerieke postcode."@nl, G),

  % GM_CODE
  qb_label(M, cbs:'GM_CODE', "Gemeentecode"@nl, G),
  qb_comment(M, cbs:'GM_CODE', "De gemeentecode geeft de numerieke aanduiding van gemeenten weer, die door GNL in overleg met het Ministerie van Binnenlandse Zaken en Koninkrijksrelaties (BZK) wordt vastgesteld.  Deze viercijferige code is gekoppeld aan de naam van de gemeente: wijzigt de naam van een gemeente, dan wijzigt ook de code."@nl, G),

  % GEM_HH_GR
  qb_label(M, cbs:'GEM_HH_GR', "Gemiddelde huishoudensgrootte"@nl, G),
  qb_comment(M, cbs:'GEM_HH_GR', "Het aantal in particuliere huishoudens levende personen gedeeld door het aantal particuliere huishoudens. De gemiddelde huishoudensgrootte is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  De gemiddelde huishoudensgrootte tot en met 2003 is een voorlopig cijfer. De cijfers vanaf 2004 zijn definitief en komen overeen met de definitieve cijfers in de Regionale Kerncijfers Nederland.  Wanneer een buurt minder dan 10 inwoners telt, is dit gegeven geheimgehouden."@nl, G),
  
  % GM_NAAM
  qb_label(M, cbs:'GM_NAAM', "Gemeentenaam"@nl, G),
  qb_comment(M, cbs:'GM_NAAM', "De naam van de bestuurlijke gemeente.  Deze naam volgt de officiële schrijfwijze."@nl, G),

  % IND_WBI
  qb_label(M, cbs:'IND_WBI', "Indelingswijziging wijken en buurten"@nl, G),
  qb_comment(M, cbs:'IND_WBI', "Deze indicator geeft per wijk en buurt aan of de cijfers uit deze tabel zonder problemen kunnen worden gekoppeld aan en vergeleken met de cijfers van een jaar eerder, of dat er wijzigingen in de Wijk- en Buurtindeling zijn waardoor dit niet kan.  Detailinformatie over wijzigingen in de Wijk- en Buurtindeling kan worden verkregen door de wijk- en buurtkaart van twee opeenvolgende jaren met elkaar te vergelijken.  De indicator kent drie mogelijke waarden:  (1) De codering en afbakening van deze wijk/buurt is ongewijzigd ten opzichte van het voorgaande jaar.  Het is wel mogelijk dat een naamswijziging heeft plaatsgevonden.  De cijfers kunnen worden gekoppeld en vergeleken met die van het voorgaande jaar;  (2) De codering van de wijk/buurt is veranderd ten opzichte van het voorgaande jaar.  De afbakening is ongewijzigd.  Om te kunnen koppelen met cijfers van het voorgaande jaar zal eerst moeten worden achterhaald wat de codering van het voorgaande jaar was.  Is de koppeling eenmaal geslaagd dan kunnen de cijfers alsnog met elkaar worden vergeleken;  (3) De afbakening van de wijk/buurt is veranderd ten opzichte van het voorgaande jaar.  Dit kan gepaard zijn gegaan met een gewijzigde codering.  De cijfers kunnen niet zonder meer worden vergeleken met die van het voorgaande jaar.  Verschillen kunnen immers samenhangen met de verandering in de afbakening van de wijk of buurt.  Voor een wijk of buurt wordt alleen een wijziging in de afbakening geconstateerd wanneer een grens circa 5 meter of meer is verlegd.  Kleinere grenswijzigingen worden niet als significant beschouwd."@nl, G),

  % MOTOR_2W
  qb_label(M, cbs:'MOTOR_2W', "Motortweewielers totaal"@nl, G),
  qb_comment(M, cbs:'MOTOR_2W', "Het aantal motorrijwielen, scooters, motorcarriers en motorinvalidenwagens met een motorrijwielkentekenbewijs op 1 januari.  De cijfers zijn afgerond op vijftallen."@nl, G),

  % OAD
  qb_label(M, cbs:'OAD', "Omgevingsadressendichtheid"@nl, G),
  qb_comment(M, cbs:'OAD', "Het gemiddeld aantal adressen van een buurt, wijk of gemeente per vierkante kilometer binnen een cirkel met een straal van één kilometer op 1 januari van het betreffende jaar.  De OAD beoogt de mate van concentratie van menselijke activiteiten (wonen, werken, schoolgaan, winkelen, uitgaan etc.) weer te geven.  GNL gebruikt de OAD om de stedelijkheid van een bepaald gebied te bepalen.  Voor de berekening hiervan wordt eerst voor ieder adres de OAD vastgesteld.  Daarna is het gemiddelde berekend van de omgevingsadressendichtheden van alle afzonderlijke adressen binnen het beschouwde gebied. De adressen zijn afkomstig uit het Geografisch Basisregister van het betreffende jaar (definitieve versie).  Dit register bevat alle adressen van Nederland die zijn voorzien van een postcode, gemeentecode en wijk- en buurtcode.  De gemeentelijke OAD in deze publicatie wijkt af van de gemeentelijke OAD in de Regionale Kerncijfers Nederland (RKN).  In deze laatste publicatie wordt de OAD berekend zonder gegevens over de nieuwe adressen van het betreffende kalenderjaar.  Het gemeentelijk cijfer van de OAD in deze publicatie komt overeen met de definitieve OAD in de publicatie Maatstaven ruimtelijke gegevens Financiële verhoudingswet (Fvw)."@nl, G),

  % OPP_LAND
  qb_label(M, cbs:'OPP_LAND', "Oppervlakte land"@nl, G),
  qb_comment(M, cbs:'OPP_LAND', "De oppervlakte land is bepaald door het meest recente digitale bestand Bodemgebruik te combineren met het digitale bestand van gemeente-, wijk- en buurtgrenzen.  Voor de jaren 2013 tot en met 2015 is uitgegaan van het bestand Bodemgebruik 2010.  De oppervlakte land wordt uitgedrukt in hele hectaren (ha.)."@nl, G),
  
  % OPP_TOT
  qb_label(M, cbs:'OPP_TOT', "Oppervlakte totaal"@nl, G),
  qb_comment(M, cbs:'OPP_TOT', "De totale oppervlakte is de som van de oppervlakten water en land in hele hectaren (ha.)."@nl, G),

  % OPP_WATER
  qb_label(M, cbs:'OPP_WATER', "Oppervlakte water"@nl, G),
  qb_comment(M, cbs:'OPP_WATER', "Oppervlakte water omvat zowel binnen- als buitenwater.  Tot binnenwater wordt gerekend alle water niet onderhevig aan getijden en breder dan 6 meter, zoals het IJsselmeer, Markermeer, Randmeren, sloten, rivieren, kanalen en dergelijke.  Onder het buitenwater valt alle water onderhevig aan getijden, zoals de Waddenzee, Oosterschelde, Westerschelde en het gemeentelijk ingedeelde gedeelte van de Noordzee.  De oppervlakte water is bepaald door het meest recente digitale bestand Bodemgebruik te combineren met het digitale bestand van gemeente-, wijk- en buurtgrenzen.  Voor de jaren 2013 tot en met 2015 is uitgegaan van het bestand Bodemgebruik 2010.  Het buitenwater is alleen op gemeenteniveau vermeld, water per wijk of buurt bestaat alleen uit binnenwater.  De oppervlakte water wordt uitgedrukt in hele hectaren (ha.)."@nl, G),

  % P_00_14_JR
  qb_label(M, cbs:'P_00_14_JR', "Personen 0 tot 15 jaar"@nl, G),
  qb_comment(M, cbs:'P_00_14_JR', "Het aantal inwoners op 1 januari van 0 tot 15 jaar oud, uitgedrukt in hele procenten van het totaal aantal inwoners.  Dit gegeven is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Wanneer een buurt minder dan 50 inwoners telt, is dit gegeven geheimgehouden (.)."@nl, G),

  % P_15_24_JR
  qb_label(M, cbs:'P_15_24_JR', "Personen 15 tot 25 jaar"@nl, G),
  qb_comment(M, cbs:'P_15_24_JR', "Het aantal inwoners op 1 januari van 15 tot 25 jaar oud, uitgedrukt in hele procenten van het totaal aantal inwoners.  Dit gegeven is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Wanneer een buurt minder dan 50 inwoners telt, is dit gegeven geheimgehouden (.)."@nl, G),

  % P_25_44_JR
  qb_label(M, cbs:'P_25_44_JR', "Personen 25 tot 45 jaar"@nl, G),
  qb_comment(M, cbs:'P_25_44_JR', "Het aantal inwoners op 1 januari van 25 tot 45 jaar oud, uitgedrukt in hele procenten van het totaal aantal inwoners.  Dit gegeven is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Wanneer een buurt minder dan 50 inwoners telt, is dit gegeven geheimgehouden (.)."@nl, G),

  % P_45_64_JR
  qb_label(M, cbs:'P_45_64_JR', "Personen 45 tot 65 jaar"@nl, G),
  qb_comment(M, cbs:'P_45_64_JR', "Het aantal inwoners op 1 januari van 45 tot 65 jaar oud, uitgedrukt in hele procenten van het totaal aantal inwoners.  Dit gegeven is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Wanneer een buurt minder dan 50 inwoners telt, is dit gegeven geheimgehouden (.)."@nl, G),

  % P_65_EO_JR
  qb_label(M, cbs:'P_65_EO_JR', "Personen 65 jaar en ouder"@nl, G),
  qb_comment(M, cbs:'P_65_EO_JR', "Het aantal inwoners weer op 1 januari van 65 jaar of ouder, uitgedrukt in hele procenten van het totaal aantal inwoners.  Dit gegeven is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Wanneer een buurt minder dan 50 inwoners telt, is dit gegeven geheimgehouden (.)."@nl, G),

  % P_ANT_ARU
  qb_label(M, cbs:'P_ANT_ARU', "Nederlandse Antillen en Aruba"@nl, G),
  qb_comment(M, cbs:'P_ANT_ARU', "Het aandeel allochtonen met herkomstgroep van de tot het Nederlandse koninkrijk
behorende eilanden Bonaire, Curaçao, Saba, Sint-Eustatius, Sint-Maarten en Aruba op 1 januari, uitgedrukt in hele procenten van het aantal inwoners.  Vanaf 10 oktober 2010 zijn de Nederlands Antillen ontbonden.  Het Koninkrijk der Nederlanden bestaat vanaf die datum uit vier landen: Nederland, Aruba, Curaçao en Sint Maarten.  Alle eilanden hebben een nieuwe status. Curaçao en Sint Maarten zijn nieuwe landen binnen het Koninkrijk.  Met een ‘Status aparte’ binnen het Koninkrijk zijn Curaçao en Sint Maarten autonome landen.  De landen hebben een zelfstandig bestuur en zijn niet meer afhankelijk van Nederland.  De openbare lichamen Bonaire, Sint Eustatius en Saba, ook wel Caribisch Nederland, hebben een diepere band met Nederland en functioneren als een bijzondere gemeente van Nederland.  Wanneer een buurt minder dan 50 inwoners én minder dan 10 niet-westerse allochtonen telt, is dit gegeven geheimgehouden."@nl, G),

  % P_EENP_HH
  qb_label(M, cbs:'P_EENP_HH', "Eenpersoonshuishoudens"@nl, G),
  qb_comment(M, cbs:'P_EENP_HH', "Het aantal huishoudens met één persoon, die ouder is dan 14 jaar, uitgedrukt in hele procenten van het totaal aantal particuliere huishoudens.  Het aandeel eenpersoonshuishoudens is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Het aandeel eenpersoonshuishoudens tot en met 2003 is een voorlopig cijfer. De cijfers vanaf 2004 zijn definitief en komen overeen met de definitieve cijfers in de Regionale Kerncijfers Nederland.  Wanneer een buurt minder dan 10 inwoners telt, is dit gegeven geheimgehouden."@nl, G),

  % P_GEHUWD
  qb_label(M, cbs:'P_GEHUWD', "Gehuwd"@nl, G),
  qb_comment(M, cbs:'P_GEHUWD', "Het aantal inwoners dat op 1 januari gehuwd is, uitgedrukt in hele procenten van het totaal aantal inwoners.  De burgerlijke staat ‘gehuwd’ ontstaat na sluiting van een huwelijk of het aangaan van een geregistreerd partnerschap.  Tot de gehuwden worden ook personen gerekend die gescheiden zijn van tafel en bed, want zij blijven formeel gehuwd.  Dit gegeven is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Wanneer een buurt minder dan 50 inwoners telt, is dit gegeven geheimgehouden (.)."@nl, G),

  % P_HH_M_K
  qb_label(M, cbs:'P_HH_M_K', "Huishoudens met kinderen"@nl, G),
  qb_comment(M, cbs:'P_HH_M_K', "Het aantal meerpersoonshuishoudens met kinderen uitgedrukt in hele procenten van het totaal aantal particuliere huishoudens.  Meerpersoonshuishoudens met kinderen bestaan uit niet-gehuwde paren met kinderen, echtparen met kinderen en eenouderhuishoudens.  Het aandeel huishoudens met kinderen is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Het aandeel meerpersoonshuishoudens met kinderen tot en met 2003 is een voorlopig cijfer.  De cijfers vanaf 2004 zijn definitief en komen overeen met de definitieve cijfers in de Regionale Kerncijfers Nederland.  Wanneer een buurt minder dan 10 inwoners telt, is dit gegeven geheimgehouden."@nl, G),

  % P_HH_Z_K
  qb_label(M, cbs:'P_HH_Z_K', "Huishoudens zonder kinderen"@nl, G),
  qb_comment(M, cbs:'P_HH_Z_K', "Het aantal meerpersoonshuishoudens zonder kinderen uitgedrukt in hele procenten van het totaal aantal particuliere huishoudens. Meerpersoonshuishoudens zonder kinderen bestaan uit niet-gehuwde paren zonder kinderen, echtparen zonder kinderen en overige huishoudens.  Het aandeel huishoudens zonder kinderen is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Het aandeel meerpersoonshuishoudens zonder kinderen tot en met 2003 is een voorlopig cijfer.  De cijfers vanaf 2004 zijn definitief en komen overeen met de definitieve cijfers in de Regionale Kerncijfers Nederland.  Wanneer een buurt minder dan 10 inwoners telt, is dit gegeven geheimgehouden."@nl, G),

  % P_MAROKKO
  qb_label(M, cbs:'P_MAROKKO', "Marokko"@nl, G),
  qb_comment(M, cbs:'P_MAROKKO', "Het aandeel allochtonen met herkomstgroep Marokko, Ifni, Spaanse Sahara en Westelijke Sahara op 1 januari, uitgedrukt in hele procenten van het aantal inwoners.  Wanneer een buurt minder dan 50 inwoners én minder dan 10 niet-westerse allochtonen telt, is dit gegeven geheimgehouden."@nl, G),

  % P_N_W_AL
  qb_label(M, cbs:'P_N_W_AL', "Niet-westers totaal"@nl, G),
  qb_comment(M, cbs:'P_N_W_AL', "Het aantal allochtonen met een niet-westerse herkomst op 1 januari, uitgedrukt in hele procenten van het aantal inwoners.  Dit gegeven is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Wanneer een buurt minder dan 50 inwoners telt, is dit gegeven geheimgehouden.  Tot en met 2003 gold bovendien de aanvullende eis van de aanwezigheid van minimaal tien niet-westerse allochtonen en werd het aantal vóór omrekening naar een percentage eerst afgerond op vijftallen."@nl, G),

  % P_ONGEHUWD
  qb_label(M, cbs:'P_ONGEHUWD', "Ongehuwd"@nl, G),
  qb_comment(M, cbs:'P_ONGEHUWD', "Het aantal inwoners dat op 1 januari ongehuwd is, uitgedrukt in hele procenten van het totaal aantal inwoners.  De burgerlijke staat ‘ongehuwd’ geeft aan dat een persoon nog nooit een huwelijk heeft gesloten of een geregistreerd partnerschap is aangegaan.  Dit gegeven is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Wanneer een buurt minder dan 50 inwoners telt, is dit gegeven geheimgehouden (.)."@nl, G),

  % P_TURKIJE
  qb_label(M, cbs:'P_TURKIJE', "Turkije"@nl, G),
  qb_comment(M, cbs:'P_TURKIJE', "Het aandeel allochtonen met herkomstgroep Turkije op 1 januari, uitgedrukt in hele procenten van het aantal inwoners.  Het percentage is vermeld bij 50 of meer inwoners per buurt en minimaal 10 niet-westerse allochtonen per buurt."@nl, G),

  % P_GESCHEID
  qb_label(M, cbs:'P_GESCHEID', "Gescheiden"@nl, G),
  qb_comment(M, cbs:'P_GESCHEID', "Het aantal inwoners dat op 1 januari gescheiden is, uitgedrukt in hele procenten van het totaal aantal inwoners.  De burgerlijke staat ‘gescheiden’ ontstaat na ontbinding van een huwelijk door echtscheiding of na ontbinding van een geregistreerd partnerschap anders dan door het overlijden van de partner.  Personen die gescheiden zijn van tafel en bed worden tot de gehuwden gerekend.  Dit gegeven is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Wanneer een buurt minder dan 50 inwoners telt, is dit gegeven geheimgehouden."@nl, G),

  % P_OVER_NW
  qb_label(M, cbs:'P_OVER_NW', "Overig niet-westers"@nl, G),
  qb_comment(M, cbs:'P_OVER_NW', "Het aandeel allochtonen met een overige niet-westerse herkomst op 1 januari, uitgedrukt in hele procenten van het aantal inwoners.  Het percentage is vermeld bij 50 of meer inwoners per buurt en minimaal 10 niet-westerse allochtonen per buurt."@nl, G),

  % P_SURINAM
  qb_label(M, cbs:'P_SURINAM', "Suriname"@nl, G),
  qb_comment(M, cbs:'P_SURINAM', "Het aandeel allochtonen met herkomstgroep Suriname op 1 januari, uitgedrukt in hele procenten van het aantal inwoners.  Wanneer een buurt minder dan 50 inwoners én minder dan 10 niet-westerse allochtonen telt, is dit gegeven geheimgehouden."@nl, G),

  % P_VERWEDUW
  qb_label(M, cbs:'P_VERWEDUW', "Verweduwd"@nl, G),
  qb_comment(M, cbs:'P_VERWEDUW', "Het aantal inwoners dat op 1 januari verweduwd is, uitgedrukt in hele procenten van het totaal aantal inwoners.  De burgerlijke staat ‘verweduwd’ ontstaat na ontbinding van een huwelijk of geregistreerd partnerschap door overlijden van de partner.  Dit gegeven is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Wanneer een buurt minder dan 50 inwoners telt, is dit gegeven geheimgehouden."@nl, G),

  % P_WEST_AL
  qb_label(M, cbs:'P_WEST_AL', "Westers totaal"@nl, G),
  qb_comment(M, cbs:'P_WEST_AL', "Het aantal allochtonen met een westerse herkomst op 1 januari, uitgedrukt in hele procenten van het aantal inwoners.  Dit gegeven is ontleend aan de Structuurtelling Gemeentelijke Basisadministratie (GBA).  Tot de westerse allochtonen behoren allochtonen uit Europa, Noord-Amerika, Oceanië, Indonesië en Japan.  Wanneer een buurt minder dan 50 inwoners telt, is dit gegeven geheimgehouden."@nl, G),

  % POSTCODE
  qb_label(M, cbs:'POSTCODE', "Meest voorkomende postcode"@nl, G),
  qb_comment(M, cbs:'POSTCODE', "De meest voorkomende numerieke postcode in een buurt, op grond van het aantal adressen in het Geografisch Basisregister (GBR, definitieve versie) per 1 januari."@nl, G),

  % STED
  qb_label(M, cbs:'STED', "Stedelijkheid"@nl, G),
  qb_comment(M, cbs:'STED', "Op grond van de omgevingsadressendichtheid is aan iedere buurt, wijk of gemeente een stedelijkheidsklasse toegekend.  De volgende klassenindeling is gehanteerd:  (1) zeer sterk stedelijk ≥2 500 adressen per km2;  (2) sterk stedelijk 1 500–2 500 adressen per km2;  (3) matig stedelijk 1 000–1 500 adressen per km2;  (4) weinig stedelijk 500–1 000 adressen per km2;  (5) niet stedelijk <500 adressen per km2."@nl, G),

  % WK_CODE
  qb_label(M, cbs:'WK_CODE', "Wijkcode"@nl, G),
  qb_comment(M, cbs:'WK_CODE', "Voor de codering van de binnen gemeenten onderscheiden wijken is een code van zes posities opgenomen.  Gemeentecode (4) + wijkcode (2)."@nl, G),

  % WK_NAAM
  qb_label(M, cbs:'WK_NAAM', "Wijknaam"@nl, G),
  qb_comment(M, cbs:'WK_NAAM', "De wijknaam is opgegeven door de gemeente die hiervan eigenaar is."@nl, G),

  % WW_UIT
  qb_label(M, cbs:'WW_UIT_TOT', "WW-uitkeringen totaal"@nl, G),
  qb_comment(M, cbs:'WW_UIT_TOT', "Personen die een uitkering ontvangen op grond van de Werkloosheidswet (WW).  De werkloosheidswet (WW) heeft tot doel werknemers te verzekeren tegen de financiële gevolgen van werkloosheid.  De wet voorziet in een uitkering die gerelateerd is aan het laatstverdiende inkomen uit dienstbetrekking.  De duur van de uitkering is afhankelijk van het arbeidsverleden van de verzekerde.  Het Uitvoeringsinstituut Werknemersverzekeringen (UWV) beoordeelt of men voor een WW-uitkering in aanmerking komt.  Het betreft in 2015 voorlopige cijfers."@nl, G),

  % WWB_UITTOT
  qb_label(M, cbs:'WWB_UITTOT', "Algemene bijstandsuitkeringen totaal"@nl, G),
  qb_comment(M, cbs:'WWB_UITTOT', "Personen die een bijstandsuitkering op grond van de Wet werk en bijstand (WWB, tot 1 januari 2015) of de Participatiewet (vanaf 1 januari 2015) ontvangen.  Het gaat om algemeen periodieke uitkeringen aan thuiswonende personen tot de AOW-leeftijd.  Het betreft in 2015 voorlopige cijfers."@nl, G),

  q_save(M, G).



gemeente_load_void(M, Dataset, G) :-
  % @tbd "edits": "In de oorspronkelijke dataset is van elk jaar tussen 1812 en 1997 een shapefile van alle dat jaar in Nederland voorkomende gemeentes beschikbaar. Wij hebben alleen van de jaren waarin een gemeente haar grenzen wijzigde een geometrie opgenomen.\r\nAlle data is van de gemeentegeschiedenis api betrokken, zie http://www.gemeentegeschiedenis.nl/pagina/5/",
  qb(M, Dataset, dc:created, 2015^^xsd:gYear, G),
  qb(M, Dataset, dc:creator, "Ad van der Meer"@nl, G),
  qb(M, Dataset, dc:creator, "Onno Boonstra"@nl, G),
  qb(M, Dataset, dc:description, "Nederland telt bijna 400 gemeenten.  In 1812 waren dat er meer dan 1100. In twee eeuwen vol herindelingen, annexaties en fusies heeft Nederland bijna 1700 gemeenten gekend.  De data van gemeentegeschiedenis is gebouwd op twee datasets: het Repertorium van Nederlandse gemeenten vanaf 1812 van Ad van der Meer en Onno Boonstra en de bij DANS gedeponeerde NLGis shapefiles van Onno Boonstra, waarin van elk jaar van 1812 tot 1997 de gemeentegrenzen zijn vastgelegd."@nl, G),
  % @tbd Menno den Engelse is part of team Erfgoed en Locatie.
  % @tbd Role should be "editor".
  qb(M, Dataset, dc:publisher, "Menno den Engelse"@nl, G),
  qb(M, Dataset, dc:title, "Gemeentegeschiedenis"@nl, G),
  qb(M, Dataset, foaf:page, 'http://www.gemeentegeschiedenis.nl/', G).
