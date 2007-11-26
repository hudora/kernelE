# -*- encoding: utf-8 -*-
# spielt von dump_softm.py gespeicherte Vorgaenge gegen das myPL ab.
# Dies ist eigentlich eine suimulationsengine
import sys, pickle, gzip, datetime, time
sys.path.extend(['./python', './test'])
from kernelE import Kerneladapter
from load_softm import load_platzbestand

THEDATE = "20071125"

# Mögliche Datasets
alldates = [
 "20071018" # Tag funktioniert
#"20071019" # Fehlbestand bei 14600/03
#"20071022" # Fehlbestand bei 14600/03
 "20071023" # Tag funktioniert
 "20071025" # Tag funktioniert
 "20071026" # Tag funktioniert
 "20071029" # Tag funktioniert
 "20071030" # Tag funktioniert
 "20071104" # Tag funktioniert
#"20071107" # Fehlbestend bei 74500/TR
#"20071108" # Fehlbestand bei 71570 und 76111
#"20071110" # Fehlbestand bei 14600/03 62120 62121 62124
#"20071112" # Fehlbestand bei 62120 und 62121
#"20071114" # Fehlbestand bei 14600/03
#"20071115" # Fehlbestand bei 14600/03
 "20071121" # Tag funktioniert
 "20071123" # Tag funktioniert
 "20071125" # Tag funktioniert
]

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
        movementids = []
        
        # Ein "Pickbeleg" wird von jemandem mit Handwagen angefordert.
        p1list = k.get_picklists()
        for p1 in p1list:
            picklistId, vorgangsnummer, zielplatz, totallists, attributes, picks = p1
            picklistids.append(picklistId)
            print "PPP", repr((picklistId, vorgangsnummer, zielplatz, attributes, picks))
            # print kommibeleg via jasper
        
        # ein staplerfahrer kommt.
        m1list = k.get_movementlist()
        print "MMM", repr(m1list)
        movementids.extend(m1list)
        
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
        for m in movementids:
           k.commit_movement(m)
        
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
