# spielt von dump_softm.py gespeicherte Vorgaenge gegen das myPL ab.
# Dies ist eigentlich eine suimulationsengine
import sys, pickle, gzip, datetime
sys.path.extend(['./python'])
from kernelE import Kerneladapter


byauftrag = {}
bydate = {}

class Orderline(object):
    def __repr__(self):
        return repr((self.menge, self.artnr, self.auftragsnummer, self.liefer_date))

globalstats = {}

def process_orders(auftragsnummern):
    global byauftrag
    global globalstats
    nofit = []
    stats = {}
    
    def statnote(name, value):
        if not name in stats:
            stats[name] = 0
        stats[name] = stats[name] + value
        if not name in globalstats:
            globalstats[name] = 0
        globalstats[name] = globalstats[name] + value
    
    k = Kerneladapter()
    for auftragsnummer in auftragsnummern:
        # here we iterate over all auftrag items 
        positionen = byauftrag[auftragsnummer]
        # print auftragsnummer, len(positionen)
        # print auftragsnummer, len(positionen), [(pos.menge, pos.artnr) for pos in positionen]
        has_movements = False
        # print "k.init_provisionings_multi(%r)" % ([(pos.menge, pos.artnr) for pos in positionen])
        ret = k.init_provisionings_multi([(pos.menge, pos.artnr) for pos in positionen])
        if ret[0] == 'error':
            print ret, [(pos.menge, pos.artnr) for pos in positionen]
            statnote('retried_orders', 1)
            nofit.append(auftragsnummer)
        else:
            statnote('finished_orders', 1)
            # commit all the stuiff
            ok, movements, picks = ret
            for mov in movements:
                has_movements = True
                statnote('retrievals', 1)
                # print "commit", mov, 
                k.commit_movement(mov)
            for pick in picks:
                statnote('picks', 1)
                # print "commit", pick, 
                k.commit_pick(pick)
        
        if not has_movements:
            movements = k.create_automatic_movements()
            for mov in movements:
                # print "commit", mov, 
                k.commit_movement(mov)
                statnote('automovements', 1)
    
    # print nofit
    print "nofits left", len(set(nofit))
    print "statistics, auftraege=", len(auftragsnummern), stats
    return nofit
    

def main():
    oldnofit = []
    print "Simulating a day of myPL at work"
    vorgaenge = pickle.load(gzip.GzipFile('test/data/vorgaenge-20071019.pickle.gz', 'r'))
    for v in vorgaenge:
        o = Orderline()
        o.menge, o.artnr, o.auftragsnummer, o.liefer_date =  v['menge'], v['artnr'], v['auftragsnummer'], v['liefer_date']
        byauftrag.setdefault(o.auftragsnummer, []).append(o)
        bydate.setdefault(o.liefer_date, set()).add(o.auftragsnummer)
    days = sorted(bydate.keys())
    for day in days:
        print day
        auftragsnummern = bydate[day]
        nofit = process_orders(auftragsnummern)
        if nofit:
            print "nofit", len(nofit)
            oldnofit.extend(process_orders(nofit))
    
    print "******************** Final"
    print "nofits left", len(set(oldnofit))
    print "statistics", globalstats
    print "last run"
    oldnofit = process_orders(list(set(oldnofit)))
    print "nofits left", len(set(oldnofit))
    print "statistics", globalstats
    print oldnofit, [byauftrag[auftragsnummer] for auftragsnummer in oldnofit]
    
    for auftragsnummer in oldnofit:
        positionen = byauftrag[auftragsnummer]
        print "mypl_provisioning:find_provisioning_candidates_multi([%s])" % ','.join(["{%s,%s}" % (pos.menge, pos.artnr) for pos in positionen])
    
    movements = True
    k = Kerneladapter()
    while movements:
        movements = k.create_automatic_movements()
        for mov in movements:
            print mov
            # print "commit", mov, k.commit_movement(mov)

main()
