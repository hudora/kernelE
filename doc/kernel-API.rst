DRAFT $Date$ $Revision$

===================
HTTP-API zum Kernel
===================

Der Kernel soll in Zukunft nur noch per HTTP angesteuert werden. Als Datenformat
wird JSON verwendet.


Kommissionieraufträge
=====================

`GET /kommiauftrag` - Liste aller anstehenden Kommiaufträge
-----------------------------------------------------------

Listet alle im myPL anstehenden Kommiauftrge aus.

::

  $ curl http://hurricane.local.hudora.biz:8000/kommiauftrag
  ["3098737","3098718","3098057","3098721", ...]

`GET /kommiauftrag/{oid}` - Daten zu einem Kommiauftrag
-------------------------------------------------------

Das Datenformat selbst ist noch im Fluss ...

::
  
  $ curl http://hurricane.local.hudora.biz:8000/kommiauftrag/3098737
  {"kommiauftragsnr":"3098737","liefertermin":"2009-10-14","liefertermin_ab":"2009-10-14",
   "versandtermin":"2009-10-13","versandtermin_ab":"2009-10-13",
   "fixtermin":false,"gewicht":9510,"volumen":130.356,
   "land":"DE","plz":"01796","info_kunde":"","auftragsnr":"1089297",
   "kundenname":"Sport-Wolf OHG ","kundennr":"19858","tries":0,
   "provisioninglists":[],"priority":2,"shouldprocess":"yes","status":"new",
   "orderlines":[{"menge":18,"artnr":"83161","posnr":1,"gewicht":0,"auftragsposition":1},
                 {"menge":12,"artnr":"83162","posnr":2,"gewicht":0,"auftragsposition":2}],
   "kernel_customer":[49,57,56,53,56],"kernel_enqueued_at":"2009-11-02T15:16:20","anbruch":true,
   "versandpaletten":0.1,"art":"A","paletten":0.1,
   "kep":true}


`DELETE /kommiauftrag/{oid}` - Kommiauftrag "Stornieren"
--------------------------------------------------------

Dies ist nur möglich, bei Kommiaufträgen, bei denen noch kein Kommischein erzeugt wurde. 
https://cybernetics.hudora.biz/intern/trac/browser/projects/myPLfrontend/trunk/myplfrontend/bin/mypl_rueckmelde_server?rev=6904


`PUT /kommiauftrag/{oid}`
--------------------------

Neuen Kommiauftrag in das System spielen - TBD


Kommischeine
============


Units
=====

`GET /unit` - Liste aller Unit IDs (MUIs)
-----------------------------------------

::

  $ curl http://hurricane.local.hudora.biz:8000/unit
  ["340059981002419744","340059981002412974",...]


`GET /unit/{mui}` - liefert Informationen zu einer Unit
-------------------------------------------------------

::

  $ curl http://hurricane.local.hudora.biz:8000/unit/340059981002380709
  {"mui":"340059981002380709","menge":30,"artnr":"24537",
   "height":1795,"pick_quantity":0,"location":"064601",
   "created_at":"2009-06-04T05:45:33.507573Z","source":"umlagerung",
   "komminr":"3074892","picks":[],"movements":[]}


`POST /unit/{mui}` - ändert Daten einer Unit
--------------------------------------------

Im Body wird ein JSON directory übergeben, dass nur folgende Keys beinhalten darf:

- height

::

  $ curl -X POST -d '{"height":1800}' http://hurricane.local.hudora.biz:8000/unit/340059981002380709
  {"mui":"340059981002380709","menge":30,"artnr":"24537",
   "height":1800,"pick_quantity":0,"location":"064601",
   "created_at":"2009-06-04T05:45:33.507573Z","source":"umlagerung",
   "komminr":"3074892","picks":[],"movements":[]}


Movements
=========

Wichtig ist es, zu beachten, dass es zwei arten von Movements gibt: "basic Movements" und "retrievals".
Retrievals gehören zu Kommischeinen und sollten nur im Zusammenhang mit diesen Verarbeitet werden.

`GET /movement` - Liste aller aktiven Palettenbewegungen
--------------------------------------------------------

::

  $ curl http://hurricane.local.hudora.biz:8000/movement
  ["mb08473973","mb08484313",...]


`POST /movement` - lässt den Kernel eine Palettenbewegung erzeugen
------------------------------------------------------------------

Dies lässt den Kernel mehrere Lagerinterne Bewegungen nach seinem Gusto erzeugen.
In der Regel geht es dabei um das herunterlagen von Paletten für Picks.
Wenn der Kernel keine Vorgänge erzeugen kann, wird der Status 404 zurückgeliefert.

::

  $ curl -X POST -d '{"attr":"test"}' http://hurricane.local.hudora.biz:8000/movement
  {"oid":"mb08510898","mui":"340059981002320613",
  "from_location":"180202","to_location":"011301",
  "created_at":"2009-11-02T23:15:40.081982Z",
  "menge":118,"artnr":"71680/XX",
  "mypl_notify_requesttracker":true,"reason":"requesttracker",
  "attr":"test","status":"open"}


`GET /movement/{oid}` - liefert Informationen zu einer Palettenbewegung
-----------------------------------------------------------------------

::

  curl http://hurricane.local.hudora.biz:8000/movement/mb08473973
  {"oid":"mb08473973","mui":"340059981002567254",
   "from_location":"EINLAG","to_location":"194503",
   "created_at":"2009-10-27T10:50:04.955635Z",
   "menge":12,"artnr":"10316","status":"open"}


`DELETE /movement/{oid}` - bricht eine Palettenbewegung ab
----------------------------------------------------------

Die Palette wird wieder zurück auf from_location gebucht.

::

  $ curl -X DELETE http://hurricane.local.hudora.biz:8000/movement/mb08473973
  ok


Locations
=========

Locations sind die Lager-/Regalplätze


`GET /location` - liefert eine Liste aller Lagerplätze
------------------------------------------------------

::

  $ curl http://hurricane.local.hudora.biz:8000/location
  ["011301","011302","011303", ...]


`GET /loation/{name}` - liefert Informationen zu einem Lagerplatz
-----------------------------------------------------------------

::

  $ curl http://hurricane.local.hudora.biz:8000/location/033201
  {"name":"033201","height":2100,"floorlevel":"true","preference":5,
  "info":"","allocated_by":["340059981002581212"],"reserved_for":[]}


Products
========


`GET /product` - liefert eine Liste aller Artikel im Lager
----------------------------------------------------------

::

  $ curl http://hurricane.local.hudora.biz:8000/product
  ["01020","01023","10008","10016","10030","10106/01","10106/WK","10111","10112", ...]


`GET /product/{artnr}` - liefert Informationen zu einem Artikel im Lager
------------------------------------------------------------------------

Liefert Daten zu einem Artikel am Lager.

::

  $ curl http://hurricane.local.hudora.biz:8000/product/10118
  {"artnr":"10118",
  "full_quantity":16,"available_quantity":16,"pick_quantity":0,"movement_quantity":0,
  "muis":["340059981002381621"]}


`POST /product/{artnr}` - Lasst das Lager einen Kommissioniervorschalg erstellen
--------------------------------------------------------------------------------

Hiermit kann ein KOmissioniervorschlag für eine bestimmte Menge eines Artikels erstellt werden. Im Body wird ein JSON directory übergeben, dass nur folgende Keys beinhalten darf:

- menge

Falls keine passenden Mengen gefunden werden können - z.B. wil erst noch eine Umlagerung durchgeführt werden muss oder der Artikel nicht am lager ist - wird der Statuscide 404 zurückgegeben.

::

  $ curl -X POST -d '{"menge":5}' http://hurricane.local.hudora.biz:8000/product/10118

Diese funktion wird nicht per Get aufgerufen, weil sie *nicht* idempotent ist. Ein aufruf dieser URL verändert das interene Scheduling im kernel.


Picks
=====

Picks müssen eigentlich nie einzeln verwendet werden, sonder sollten immer über Kommischeine addressiert
werden.

`GET /pick` - liefert eine Liste aller offenen Picks
----------------------------------------------------

::

  $ curl http://hurricane.local.hudora.biz:8000/pick
  ["P08462015", ...]


`GET /pick/{oid}` - liefert Informationen zu einem Pick
-------------------------------------------------------

::

  $ curl http://hurricane.local.hudora.biz:8000/pick/P08462015
  {"oid":"P08462015","from_unit":"340059981002563638","from_location":"100901",
  "menge":13,"artnr":"12540","created_at":"2009-10-23T11:12:24.275216Z",
  "kernel_published_at":"2009-10-23T11:12:24.000000Z",
  "kernel_provisioninglist_id":"p08462027","status":"open"}


Diverses
========


`GET /statistics`
-----------------

::

  $ curl http://hurricane.local.hudora.biz:8000/statistics
  {"empty_pickable_locations":22,
   "multi_floorunits":158,
   "requesstracker_entries":4,
   "open_movements":7, "open_picks":1,
   "oldest_movement":"2009-10-27T10:50:04.955635Z",
   "oldest_pick":"2009-10-23T11:12:24.275216Z"}


`GET /abc`
----------

::

  $ curl http://hurricane.local.hudora.biz:8000/abc       
  {"a":[[522,"76666"],[464,"76686"],...],"b":[[439,"76650"],[202,"76676"],...],...}


`GET /requesttracker`
---------------------

:: 

  $ curl http://hurricane.local.hudora.biz:8000/requesttracker
  [{"artnr":"WK61020","menge":60,"lastseen":"2009-11-02T23:14:57.628355Z","priority":"{3,true,<<\"2009-11-02\">>,<<\"2009-11-03\">>,0,\"19770\"}"},
   {"artnr":"62100/WK","menge":300,"lastseen":"2009-11-02T23:14:57.632614Z","priority":"{3,true,<<\"2009-11-02\">>,<<\"2009-11-03\">>,0,\"19770\"}"},
   ...]


