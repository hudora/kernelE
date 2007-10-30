# -*- encoding: utf-8 -*-
# spielt von dump_softm.py gespeicherte Vorgaenge gegen das myPL ab.
# Dies ist eigentlich eine suimulationsengine
import sys, pickle, gzip, datetime, time
sys.path.extend(['./python', './test'])
from kernelE import Kerneladapter
from load_softm import load_platzbestand

THEDATE = "20071019"

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
        positionen = []
        for poslist in [lieferung.positionen for lieferung in self.lieferungen_todo]:
            for pos in poslist:
                positionen.append((pos.menge, pos.artnr))
        k.init_dayforcast(positionen)
        
    
    def simulate(self):
        """Simuliert Lagerbewegungen. Hierbei wird simuliert jede Kommibeleg Position würde unabhaengig von den anderen zurueckgemeldet."""
        k = Kerneladapter()
        thisround = 0
        while self.lieferungen_todo:
            
            # everybody loves statistics
            thisround += 1
            print "Round %d: todo:%d picks:%d retrievals:%d movements:%d" % (thisround, 
                   len(self.lieferungen_todo), len(self.open_picks), len(self.open_retrievals),
                   len(self.open_movements))
            print self.stats
            time.sleep(1)
            
            # process picks
            for i in range(self.picks_per_round):
                if self.open_picks:
                    pickId = self.open_picks.pop(0)
                    k.commit_pick(pickId)
                    self.statnote('picks', 1)
            
            # process retrievals (= "picks for full pallets")
            for i in range(self.retrievals_per_round):
                if self.open_retrievals:
                    retrievalId = self.open_retrievals.pop(0)
                    k.commit_retrieval(retrievalId)
                    self.statnote('retrievals', 1)
            
            # process movements ("umlagerungen")
            for i in range(self.movements_per_round):
                if self.open_movements:
                    movementId = self.open_movements.pop(0)
                    k.commit_movement(movementId)
                    self.statnote('movements', 1)
            
            #### now generate new picks/retrievals (unles both queues are still full)
            init_provisionings_multi_count = 0
            while ((not self.open_picks) or (not self.open_retrievals) 
                   and (len(self.open_picks) <= 3*self.picks_per_round 
                        and len(self.open_retrievals) <= 4*self.retrievals_per_round)):
                lieferung = self.lieferungen_todo.pop(0)
                ret = k.init_provisionings_multi([(pos.menge, pos.artnr) for pos in lieferung.positionen])
                if ret[0] == 'error':
                    # can't be retrieved at the moment
                    #  print ret, [(pos.menge, pos.artnr) for pos in positionen]
                    self.statnote('retried_orders', 1)
                    self.lieferungen_todo.append(lieferung)
                else:
                    self.statnote('finished_orders', 1)
                    ok, retrievals, picks = ret
                    self.open_picks.extend(picks)
                    self.open_retrievals.extend(retrievals)
                    self.lieferungen_done.add(lieferung)
                    self.stats['toppicksper'] = max([self.stats.get('toppicksper', 0), len(picks)])
                    self.stats['topretrievalsper'] = max([self.stats.get('topretrievalsper', 0), len(retrievals)])
                
                # escape from deadlocks
                init_provisionings_multi_count += 1
                if init_provisionings_multi_count > 10:
                    break
            
            # movements to refill floorlevel
            if not self.open_movements:
                movements = k.create_automatic_movements()
                self.open_movements.extend(movements)
        
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
        fd.write('#dataset  rounds    $Revision$ ret.orders\n' % (THEDATE, thisround, self.stats['retried_orders']))
        fd.write('%s    %04d    $Revision$    %04d\n' % (THEDATE, thisround, self.stats['retried_orders']))
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
