beeldbank_scrape(N1, G) :-
  debug(beeldbank(scrape), "~D", [N1]),
  uri_query_components(
    Query,
    [database=images,limit=10,startfrom=N1,search='pointer 1009']
  ),
  uri_components(
    Iri,
    uri_components(
      http,
      'cultureelerfgoed.adlibsoft.com',
      '/harvest/wwwopac.ashx',
      Query,
      _
    )
  ),
  flag(xml_record, _, 1),
  (   http_retry_until_success(
	hdt__call(xml2rdf_stream(Iri, [beeldbank_record]), G)
      )
  ->  sleep(1),
      N2 is N1 + 1,
      beeldbank_scrape(N2, G)
  ;   true
  ).

beeldbank_record(Dom, Record) :-
  xpath(Dom, //record(content), Record).
