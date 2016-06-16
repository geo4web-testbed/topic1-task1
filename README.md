Modern Ways of Spatial Data Publishing
======================================

The research question to answer is:

> How do the other lessons learned within the GeoNovum tender meet the
> constraints (e.g. budgets) and capabilities (e.g. in-house know-how)
> of governmental organizations on the one hand, and of data users on
> the other?

Steps involved in the high-quality publication of spatial data
--------------------------------------------------------------

  1. **Obtaining** public data (⋆⋆).
  
  2. **Data conversion (1/2)** from proprietary / binary to an open /
     text formats.  This allows the data to be freely read and
     processed in SotA tools (⋆⋆⋆).
  
  3. **Data conversion (2/2)** from an open and processable format to
     Linked Data, using a straightforward mapping (i.e., fully
     automated).
  
  4. **Data tansformation** Perform a recorded sequence of Linked Data
     transformations in order to raise the quality of the data (⋆⋆⋆⋆).
  
  5. **Link to extenal sources** once the data has reached high enough
     quality (⋆⋆⋆⋆⋆).
  
  6. **Data publication** of high quality data, ensuring high
     performance and high availability.
  
  7. **Data exposure** through APIs and interaction paradigms the
     users like / want / already know.
  
  8. **Uptake** by users who recognize the value of the data,
     resulting in real-world use.
  
  9. **Sustainability** by reducing the cost of long-term maintenance
     and balancing the server cost / client benefit model.

Some of the earlier steps can be expressed in the 5⋆ Linked Data
model.  The challenges encountered in step (1) and (8) are different
from the others: they are social rather than technological.  Step (2)
and (3) are already well-known and can be easily quantified.  Step (4)
and (5) are challenging: ATM these are either high-costly or
low-quality.  Rather than finding a ‘silver bullet’ here, we try to
formulate an upgrade path from low-cost/low-quality to
high-cost/high-quality.  The main requirement for the upgrade path is
that the cost/benefit trade-off is at most linear.  Steps (6) and (9)
are a well-known problems in Linked Data publishing.  Luckily,
significant improvements have been made recently that allow the
performance/availability as well as the client/server trade-off to be
made on a more fine-grained level.  Step (7) largely depends on
formulating the intended user groups and gaining knowledge about their
existing capabilities and preferences.

![](https://raw.githubusercontent.com/geo4web-testbed/topic1-task1/master/img/5star.png "5⋆ Linked Data")



Geo API
-------

The Geo API anders incremental proximity queries, i.e. queries of the
form: “Give me descriptions of resources that are located near this
given geo coordinate.”

### For human access

The indended user for this endpoint is machines that use the data in
interesting ways.  We also allow humans to directly access the
endpoint through HTML+CSS+JS, but this is only for demoing and
debugging purposes.  The human-facing demo uses Bootstrap, jQuery and
Leaflet.  You can open the following URL in a modern Web browser:

http://146.185.182.204/map?lng=51.34&lat=5.47

Click on a position in Valkenswaard to request the closest 100
objects.  (While the backend can also return the closest 10,000
objects, the frontend/browser cannot draw so many elements within
acceptable time.)

### For machine access

The intended user for this endpoint is machines.  At the moment there
is no limit to the number of results machines can retrieve.  We do
this on purpose to test out where the challenges for scalability lie.
Since the machine API is currently being developed there will be
changes made to the API that will break compatibility with the
currently described version.

| **Key**   | **Value**      | **Description**                       |
|:---------:|:--------------:|:-------------------------------------:|
| lat       | between(0.0,?) | The latitude of the reference point.  |
| lng       | between(0.0,?) | The longitude of the reference point. |
| page      | nonneg         | The number of the results page.       |
| page_size | nonneg         | The number of results per page.       |
| zoom      | between(1,20)  | The zoom level.                       |

### Retrieve results in JSON-LD

```bash
$ curl -H "Accept: application/ld+json" "http://146.185.182.204/map?lng=51.34&lat=5.47&page_size=1&page=10"
[
  {
    "@context": {
      "bgt":"http://bgt.nl/",
      "bgt:LV-publicatiedatum": {"@type":"xsd:dateTime"},
      "bgt:bgt-fysiekVoorkomen": {"@type":"xsd:string"},
      "bgt:bgt-status": {"@type":"xsd:string"},
      "bgt:bronhouder": {"@type":"xsd:string"},
      "bgt:id": {"@type":"xsd:string"},
      "bgt:identificatie.lokaalID": {"@type":"xsd:string"},
      "bgt:identificatie.namespace": {"@type":"xsd:string"},
      "bgt:objectBeginTijd": {"@type":"xsd:date"},
      "bgt:plus-status": {"@type":"xsd:string"},
      "bgt:relatieveHoogteligging": {"@type":"xsd:integer"},
      "bgt:tijdstipRegistratie": {"@type":"xsd:dateTime"},
      "geold":"http://geojsonld.com/vocab#",
      "geold:geometry": {"@type":"wkt:polygon"},
      "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdf:type": {"@type":"@id"},
      "wkt":"http://geojsonld.com/wkt#",
      "xsd":"http://www.w3.org/2001/XMLSchema#"
    },
    "@type":"bgt:BegroeidTerreindeel",
    "bgt:LV-publicatiedatum":"2014-08-10T16:22:23",
    "bgt:bgt-fysiekVoorkomen":"gemengd bos",
    "bgt:bgt-status":"bestaand",
    "bgt:bronhouder":"G0858",
    "bgt:id":"_A2F312CBEA70A4C8FE0532B0B5B0A8DC1",
    "bgt:identificatie.lokaalID":"G0858.2771436def1d4735aa4dd102fa93bd44",
    "bgt:identificatie.namespace":"NL.IMGeo",
    "bgt:objectBeginTijd":"2014-01-14",
    "bgt:plus-status":"geenWaarde",
    "bgt:relatieveHoogteligging":"0",
    "bgt:tijdstipRegistratie":"2014-07-09T07:22:06",
    "geold:geometry":"POLYGON((5.487835752320481 51.344092597195434,5.487927029644246 51.34418160547943,5.487938865390421 51.3441923097941,5.488064103429175 51.344314780556715,5.4881915364132 51.34443001335103,5.4876689865560815 51.344585872075186,5.4874022583633915 51.34469088380313,5.486915580819125 51.343774775260165,5.486925480675344 51.343773274642494,5.48700571084064 51.34375378117402,5.487224418678102 51.34368473960974,5.4873271095409475 51.34365851201897,5.48734711214012 51.34365643637731,5.487358220155411 51.34365612119487,5.487366719524288 51.34365739027459,5.487378238602867 51.34366126348433,5.4873894185779175 51.34366757293089,5.487555107247507 51.3438265485754,5.487655512204552 51.343922335687225,5.487700199049025 51.34396494860377,5.487835752320481 51.344092597195434))"
  }
]
```

### Retrieve results in N-Triples

```bash
$ curl -H "Accept: application/n-triples" "http://146.185.182.204/map?lng=51.34&lat=5.47"
_:197 <http://bgt.nl/LV-publicatiedatum> "2015-04-20T13:32:14"^^<http://www.w3.org/2001/XMLSchema#dateTime> .
_:197 <http://bgt.nl/bgt-functie> "rijbaan lokale weg"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/bgt-fysiekVoorkomen> "onverhard"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/bgt-status> "bestaand"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/bronhouder> "G0858"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/id> "_A2F312CC592444A60E0532B0B5B0A2B0A"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/identificatie.lokaalID> "G0858.a8b7fc9754b748ec837c6fda1048b3ff"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/identificatie.namespace> "NL.IMGeo"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/objectBeginTijd> "2014-01-14"^^<http://www.w3.org/2001/XMLSchema#date> .
_:197 <http://bgt.nl/plus-fysiekVoorkomen> "zand"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/plus-status> "geenWaarde"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/relatieveHoogteligging> "0"^^<http://www.w3.org/2001/XMLSchema#integer> .
_:197 <http://bgt.nl/tijdstipRegistratie> "2015-03-23T13:38:37"^^<http://www.w3.org/2001/XMLSchema#dateTime> .
_:197 <http://geojsonld.com/vocab#geometry> "POLYGON((5.492516449340307 51.346086971261236,5.492628635174663 51.346168577180485,5.492893883016512 51.34636152223043,5.4933364466992565 51.346693603353614,5.4939095997997045 51.34713433567864,5.493885192372249 51.347149926546095,5.493855903148815 51.34712799394038,5.493807875839638 51.34709082462906,5.493318404477951 51.34671205569906,5.493218029167581 51.3466367767346,5.492870808791516 51.3463763745987,5.492710616710512 51.346260142940125,5.492581786672217 51.346168619634746,5.492282680442654 51.34595613600364,5.491927966052825 51.34570289285435,5.491572588081234 51.345440193076215,5.491193354431858 51.345194016852695,5.490466287484249 51.34472015859787,5.490078455651503 51.344487343629936,5.4896537103263565 51.34424051949709,5.489190755062231 51.34399638799289,5.488687461084072 51.34375263142738,5.488186635740219 51.343519432302145,5.488014715824227 51.34344430084035,5.487744589957158 51.34332626097074,5.487666139900226 51.3432933311374,5.487382709264393 51.34317434012717,5.486942527391111 51.342990359618334,5.486694281575637 51.342879822113794,5.486569370039527 51.34283127291284,5.48632209050604 51.34276171341073,5.486081816310218 51.34269914065988,5.486038789826036 51.34268631436234,5.485997982358285 51.342670915386925,5.485959767374793 51.342653087238546,5.485950298534275 51.342648070578946,5.485915895109115 51.342627416718365,5.485957771731817 51.34261298121908,5.485996237225108 51.342633838357344,5.486037783115443 51.34265223894172,5.486071333878926 51.34266351824562,5.486221816874759 51.34270412713905,5.4864530102872955 51.34276846914256,5.486578874615322 51.34280652780587,5.48672220464786 51.342863870192176,5.48710244238137 51.343031327822935,5.487453261037086 51.3431792500995,5.4876660817796745 51.343266940275925,5.487816285025011 51.34332883266506,5.488150494704297 51.343473144751954,5.48832492384426 51.34355350527266,5.488686556279923 51.34372010200288,5.48878805217363 51.343769882867996,5.48919092007147 51.343967480092466,5.489698201424276 51.34423337021132,5.490222466975923 51.344537022710114,5.490727461824756 51.34485244726244,5.4913585979377775 51.34526294737976,5.491693343345598 51.34548595414154,5.491910235338363 51.345650783174726,5.492118159181525 51.34580353005235,5.492322990931311 51.34594676028869,5.492516449340307 51.346086971261236))"^^<http://geojsonld.com/wkt#polygon> .
_:197 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://bgt.nl/Wegdeel> .
_:197 <http://bgt.nl/LV-publicatiedatum> "2015-04-20T13:32:14"^^<http://www.w3.org/2001/XMLSchema#dateTime> .
_:197 <http://bgt.nl/bgt-functie> "rijbaan lokale weg"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/bgt-fysiekVoorkomen> "onverhard"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/bgt-status> "bestaand"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/bronhouder> "G0858"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/id> "_A2F312CC592444A60E0532B0B5B0A2B0A"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/identificatie.lokaalID> "G0858.a8b7fc9754b748ec837c6fda1048b3ff"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/identificatie.namespace> "NL.IMGeo"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/objectBeginTijd> "2014-01-14"^^<http://www.w3.org/2001/XMLSchema#date> .
_:197 <http://bgt.nl/plus-fysiekVoorkomen> "zand"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/plus-status> "geenWaarde"^^<http://www.w3.org/2001/XMLSchema#string> .
_:197 <http://bgt.nl/relatieveHoogteligging> "0"^^<http://www.w3.org/2001/XMLSchema#integer> .
_:197 <http://bgt.nl/tijdstipRegistratie> "2015-03-23T13:38:37"^^<http://www.w3.org/2001/XMLSchema#dateTime> .
_:197 <http://geojsonld.com/vocab#geometry> "POLYGON((5.492516449340307 51.346086971261236,5.492628635174663 51.346168577180485,5.492893883016512 51.34636152223043,5.4933364466992565 51.346693603353614,5.4939095997997045 51.34713433567864,5.493885192372249 51.347149926546095,5.493855903148815 51.34712799394038,5.493807875839638 51.34709082462906,5.493318404477951 51.34671205569906,5.493218029167581 51.3466367767346,5.492870808791516 51.3463763745987,5.492710616710512 51.346260142940125,5.492581786672217 51.346168619634746,5.492282680442654 51.34595613600364,5.491927966052825 51.34570289285435,5.491572588081234 51.345440193076215,5.491193354431858 51.345194016852695,5.490466287484249 51.34472015859787,5.490078455651503 51.344487343629936,5.4896537103263565 51.34424051949709,5.489190755062231 51.34399638799289,5.488687461084072 51.34375263142738,5.488186635740219 51.343519432302145,5.488014715824227 51.34344430084035,5.487744589957158 51.34332626097074,5.487666139900226 51.3432933311374,5.487382709264393 51.34317434012717,5.486942527391111 51.342990359618334,5.486694281575637 51.342879822113794,5.486569370039527 51.34283127291284,5.48632209050604 51.34276171341073,5.486081816310218 51.34269914065988,5.486038789826036 51.34268631436234,5.485997982358285 51.342670915386925,5.485959767374793 51.342653087238546,5.485950298534275 51.342648070578946,5.485915895109115 51.342627416718365,5.485957771731817 51.34261298121908,5.485996237225108 51.342633838357344,5.486037783115443 51.34265223894172,5.486071333878926 51.34266351824562,5.486221816874759 51.34270412713905,5.4864530102872955 51.34276846914256,5.486578874615322 51.34280652780587,5.48672220464786 51.342863870192176,5.48710244238137 51.343031327822935,5.487453261037086 51.3431792500995,5.4876660817796745 51.343266940275925,5.487816285025011 51.34332883266506,5.488150494704297 51.343473144751954,5.48832492384426 51.34355350527266,5.488686556279923 51.34372010200288,5.48878805217363 51.343769882867996,5.48919092007147 51.343967480092466,5.489698201424276 51.34423337021132,5.490222466975923 51.344537022710114,5.490727461824756 51.34485244726244,5.4913585979377775 51.34526294737976,5.491693343345598 51.34548595414154,5.491910235338363 51.345650783174726,5.492118159181525 51.34580353005235,5.492322990931311 51.34594676028869,5.492516449340307 51.346086971261236))"^^<http://geojsonld.com/wkt#polygon> .
_:197 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://bgt.nl/Wegdeel> .
_:198 <http://bgt.nl/LV-publicatiedatum> "2014-08-10T16:22:23"^^<http://www.w3.org/2001/XMLSchema#dateTime> .
_:198 <http://bgt.nl/bgt-functie> "rijbaan lokale weg"^^<http://www.w3.org/2001/XMLSchema#string> .
_:198 <http://bgt.nl/bgt-fysiekVoorkomen> "onverhard"^^<http://www.w3.org/2001/XMLSchema#string> .
_:198 <http://bgt.nl/bgt-status> "bestaand"^^<http://www.w3.org/2001/XMLSchema#string> .
_:198 <http://bgt.nl/bronhouder> "G0858"^^<http://www.w3.org/2001/XMLSchema#string> .
_:198 <http://bgt.nl/eindRegistratie> "2015-03-23T13:38:37"^^<http://www.w3.org/2001/XMLSchema#dateTime> .
_:198 <http://bgt.nl/id> "_A2F312CC3A6764A60E0532B0B5B0A2B0A"^^<http://www.w3.org/2001/XMLSchema#string> .
_:198 <http://bgt.nl/identificatie.lokaalID> "G0858.a8b7fc9754b748ec837c6fda1048b3ff"^^<http://www.w3.org/2001/XMLSchema#string> .
_:198 <http://bgt.nl/identificatie.namespace> "NL.IMGeo"^^<http://www.w3.org/2001/XMLSchema#string> .
_:198 <http://bgt.nl/objectBeginTijd> "2014-01-14"^^<http://www.w3.org/2001/XMLSchema#date> .
_:198 <http://bgt.nl/plus-fysiekVoorkomen> "zand"^^<http://www.w3.org/2001/XMLSchema#string> .
_:198 <http://bgt.nl/plus-status> "geenWaarde"^^<http://www.w3.org/2001/XMLSchema#string> .
_:198 <http://bgt.nl/relatieveHoogteligging> "0"^^<http://www.w3.org/2001/XMLSchema#integer> .
_:198 <http://bgt.nl/tijdstipRegistratie> "2014-01-14T07:50:10"^^<http://www.w3.org/2001/XMLSchema#dateTime> .
_:198 <http://geojsonld.com/vocab#geometry> "POLYGON((5.492628635174663 51.346168577180485,5.492893883016512 51.34636152223043,5.4933364466992565 51.346693603353614,5.4939095997997045 51.34713433567864,5.493885192372249 51.347149926546095,5.493855903148815 51.34712799394038,5.493807875839638 51.34709082462906,5.493318404477951 51.34671205569906,5.492870808791516 51.3463763745987,5.492710616710512 51.346260142940125,5.492581786672217 51.346168619634746,5.492282680442654 51.34595613600364,5.491927966052825 51.34570289285435,5.491572588081234 51.345440193076215,5.491193354431858 51.345194016852695,5.490466287484249 51.34472015859787,5.490078455651503 51.344487343629936,5.4896537103263565 51.34424051949709,5.489190755062231 51.34399638799289,5.488687461084072 51.34375263142738,5.488186635740219 51.343519432302145,5.487744589957158 51.34332626097074,5.487666139900226 51.3432933311374,5.487382709264393 51.34317434012717,5.486942527391111 51.342990359618334,5.486694281575637 51.342879822113794,5.486569370039527 51.34283127291284,5.48632209050604 51.34276171341073,5.486081816310218 51.34269914065988,5.486038789826036 51.34268631436234,5.485997982358285 51.342670915386925,5.485959767374793 51.342653087238546,5.485950298534275 51.342648070578946,5.485915895109115 51.342627416718365,5.485957771731817 51.34261298121908,5.485996237225108 51.342633838357344,5.486037783115443 51.34265223894172,5.486071333878926 51.34266351824562,5.486221816874759 51.34270412713905,5.4864530102872955 51.34276846914256,5.486578874615322 51.34280652780587,5.48672220464786 51.342863870192176,5.48710244238137 51.343031327822935,5.487453261037086 51.3431792500995,5.4876660817796745 51.343266940275925,5.487816285025011 51.34332883266506,5.488150494704297 51.343473144751954,5.488686556279923 51.34372010200288,5.48919092007147 51.343967480092466,5.489698201424276 51.34423337021132,5.490222466975923 51.344537022710114,5.490727461824756 51.34485244726244,5.4913585979377775 51.34526294737976,5.491693343345598 51.34548595414154,5.491910235338363 51.345650783174726,5.492118159181525 51.34580353005235,5.492322990931311 51.34594676028869,5.492516449340307 51.346086971261236,5.492628635174663 51.346168577180485))"^^<http://geojsonld.com/wkt#polygon> .
_:198 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://bgt.nl/Wegdeel> .
_:198 <http://bgt.nl/LV-publicatiedatum> "2014-08-10T16:22:23"^^<http://www.w3.org/2001/XMLSchema#dateTime> .
_:198 <http://bgt.nl/bgt-functie> "rijbaan lokale weg"^^<http://www.w3.org/2001/XMLSchema#string> .
_:198 <http://bgt.nl/bgt-fysiekVoorkomen> "onverhard"^^<http://www.w3.org/2001/XMLSchema#string> .
_:198 <http://bgt.nl/bgt-status> "bestaand"^^<http://www.w3.org/2001/XMLSchema#string> .
```
