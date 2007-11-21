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

globalstats = {}

class Lieferung(object):
    def __init__(self, oid, kundennummer, liefertermin, positionen):
        self.oid = oid
        self.kundennummer = kundennummer
        self.liefertermin = liefertermin
        self.positionen = positionen
        

class PlayerOfOrders(object):
    """this is a prototype for interacting with kernel-E, specifically the processing of orders/provisionings"""
    
    def __init__(self):
        self.lieferungen_done = set()
        self.lieferungen_todo = []
        self.open_picks = []
        self.open_retrievals = []
        self.open_movements = []
        self.picks_per_round = 4
        self.retrievals_per_round = 1
        self.movements_per_round = 1
        self.stats = {}
    
    def add_lieferungen(self, lieferungen):
        """Neue Lieferungen der Engine bekannt machen."""
        for lieferung in lieferungen:
            if lieferung not in self.lieferungen_done:
                self.lieferungen_todo.append(lieferung)
        
    def statnote(self, name, value):
        if not name in self.stats:
            self.stats[name] = 0
        self.stats[name] = self.stats[name] + value
    
    def init_oracle(self):
        k = Kerneladapter()
        for lieferung in self.lieferungen_todo:
            positionen = []
            for pos in lieferung.positionen:
                positionen.append((pos.menge, pos.artnr, {}))
            k.insert_pipeline(lieferung.oid, positionen, 5, lieferung.kundennummer,
                              0, 0, {'liefertermin': lieferung.liefertermin.strftime("%Y-%m-%d")})
        
        
    
    def simulate(self):
        """Simuliert Lagerbewegungen. Hierbei wird simuliert jede Kommibeleg Position würde unabhaengig von den anderen zurueckgemeldet."""
        starttime = time.time()
        k = Kerneladapter()
        thisround = 0
        while self.lieferungen_todo:
            
            # everybody loves statistics
            thisround += 1
            print "Round %d: todo:%d picks:%d retrievals:%d movements:%d" % (thisround, 
                   len(self.lieferungen_todo), len(self.open_picks), len(self.open_retrievals),
                   len(self.open_movements))
            print self.stats
            
            p1 = k.get_picklists()
            print p1
            p2 = k.get_picklists()
            # m1 = k.get_movements()
            r1 = k.get_retrievallistss()
            sys.exit(1)
            for p in p1 + p2:
                k.commit_picklist(p)
            for r in r1:
               k.get_retrievallist(r)
            for m in m1:
               k.commit_movement()
                
        
        # cleanup
        while self.open_picks:
            k.commit_pick(self.open_picks.pop(0))
            self.statnote('picks', 1)
        while self.open_retrievals:
            k.commit_movement(self.open_retrievals.pop(0)) # FIXME: this actually leaves the goods on AUSLAG
            self.statnote('retrievals', 1)
        while self.open_movements:
            k.commit_movement(self.open_movements.pop(0))
            self.statnote('movements', 1)
                
        # generate highscore information
        fd = open("highscores.txt", 'a')
        fd.write('#dataset  rounds    Revision             ret.orders  time\n')
        fd.write('%s    %04d    $Revision$    %04d        %04.1fs\n' % (THEDATE, thisround, 
                  self.stats['retried_orders'], time.time()-starttime))
        fd.close()
    
def main():
    print "loading base quantities"
    platzbestand = pickle.load(gzip.GzipFile('test/data/platzbestand-%s.pickle.gz' % THEDATE, 'r'))
    load_platzbestand(platzbestand)
    
    oldnofit = []
    print "Simulating a day of myPL at work"
    vorgaenge = pickle.load(gzip.GzipFile('test/data/vorgaenge-%s.pickle.gz' % THEDATE, 'r'))
    for v in vorgaenge:
        o = Orderline()
        o.menge, o.artnr, o.auftragsnummer, o.liefer_date, o.kundennummer, o.vorgangsnummer = v['menge'], v['artnr'], v['auftragsnummer'], v['liefer_date'], v['kundennummer'], v['vorgangsnummer']
        byauftrag.setdefault(o.auftragsnummer, []).append(o)
    sim = PlayerOfOrders()
    for auftragsnummer in byauftrag.keys():
        positionen = byauftrag[auftragsnummer]
        l = Lieferung(positionen[0].vorgangsnummer, positionen[0].kundennummer, positionen[0].liefer_date, positionen)
        sim.add_lieferungen([l])
    sim.init_oracle()
    sim.simulate()
    

main()
