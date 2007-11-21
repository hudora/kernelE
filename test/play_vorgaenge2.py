# -*- encoding: utf-8 -*-
# spielt von dump_softm.py gespeicherte Vorgaenge gegen das myPL ab.
# Dies ist eigentlich eine suimulationsengine
import sys, pickle, gzip, datetime, time
sys.path.extend(['./python', './test'])
from kernelE import Kerneladapter
from load_softm import load_platzbestand

THEDATE = "20071112"
"20071018"
"20071019"
"20071022"
"20071023"
"20071025"
"20071026"
"20071029"
"20071030"
"20071104"
"20071107"
"20071108"
"20071110"
"20071112"
"20071114"
"20071115"

byauftrag = {}
bydate = {}

class Orderline(object):
    def __repr__(self):
        return repr((self.menge, self.artnr, self.auftragsnummer, self.liefer_date))

class Lieferung(object):
    def __init__(self, oid, kundennummer, liefertermin, positionen):
        self.oid = oid
        self.kundennummer = kundennummer
        self.liefertermin = liefertermin
        self.positionen = positionen
        

def load_orders(lieferungen_todo):
    """Spielt die Aufträge in den Kernel."""
    k = Kerneladapter()
    for lieferung in lieferungen_todo:
        positionen = []
        for pos in lieferung.positionen:
            positionen.append((pos.menge, pos.artnr, {}))
        k.insert_pipeline(lieferung.oid, positionen, 5, lieferung.kundennummer,
                              0, 0, {'liefertermin': lieferung.liefertermin.strftime("%Y-%m-%d")})
        
def simulate():
    """Simuliert Lagerbewegungen."""
    starttime = time.time()
    k = Kerneladapter()
    
    # im folgenden das ist im Grunde der Prozess, der biem komissionieren passiert.
    while 1:
        picklistids = []
        retrievallistids = []
        
        # Ein "Pickbeleg" wird von jemandem mit Handwagen angefordert.
        p1list = k.get_picklists()
        for p1 in p1list:
            picklistId, vorgangsnummer, zielplatz, totallists, attributes, picks = p1
            picklistids.append(picklistId)
            print "PPP", repr((picklistId, vorgangsnummer, zielplatz, attributes, picks))
            # print kommibeleg via jasper
        
        # ein staplerfahrer kommt.
        m1 = k.get_movementlist()
        print "MMM", repr(m1)
        
        r1list = k.get_retrievallists()
        for r1 in r1list:
            retrievalId, vorgangsnummer, zielplatz, totallists, attributes, retrievals = r1
            print "RRR", repr((retrievalId, vorgangsnummer, zielplatz, attributes, picks))
            retrievallistids.append(retrievalId)
        
        # nun munteres zurueckmelden
        for theid in picklistids:
            k.commit_picklist(theid)
        picklistids = []
        for theid in retrievallistids:
            k.commit_retrievallist(theid)
        retrievallistids = []
        #for m in m1:
        #   k.commit_movement()
        
        # start over again
        print "loop"
        time.sleep(0.1)
            
def main():
    print "loading base quantities"
    platzbestand = pickle.load(gzip.GzipFile('test/data/platzbestand-%s.pickle.gz' % THEDATE, 'r'))
    load_platzbestand(platzbestand)
    
    print "generating order records"
    lieferungen_todo = []
    vorgaenge = pickle.load(gzip.GzipFile('test/data/vorgaenge-%s.pickle.gz' % THEDATE, 'r'))
    for v in vorgaenge:
        o = Orderline()
        o.menge, o.artnr, o.auftragsnummer, o.liefer_date, o.kundennummer, o.vorgangsnummer = v['menge'], v['artnr'], v['auftragsnummer'], v['liefer_date'], v['kundennummer'], v['vorgangsnummer']
        byauftrag.setdefault(o.auftragsnummer, []).append(o)
    for auftragsnummer in byauftrag.keys():
        positionen = byauftrag[auftragsnummer]
        l = Lieferung(positionen[0].vorgangsnummer, positionen[0].kundennummer, positionen[0].liefer_date, positionen)
        lieferungen_todo.append(l)
        
    print "loading orders into kernel"
    load_orders(lieferungen_todo)
    print "simulating a day of myPL at work"
    simulate()
    

main()
