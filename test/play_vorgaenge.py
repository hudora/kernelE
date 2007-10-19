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

print "Simulating a day of myPL at work"
def main():
    answers = {}
    
    k = Kerneladapter()
    vorgaenge = pickle.load(gzip.GzipFile('test/vorgaenge.pickle.gz', 'r'))
    print len(vorgaenge)
    for v in vorgaenge:
        o = Orderline()
        o.menge, o.artnr, o.auftragsnummer, o.liefer_date =  v['menge'], v['artnr'], v['auftragsnummer'], v['liefer_date']
        byauftrag.setdefault(o.auftragsnummer, []).append(o)
        bydate.setdefault(o.liefer_date, set()).add(o.auftragsnummer)
    days = sorted(bydate.keys())
    for day in days:
        for auftragsnummer in bydate[day]:
            # here we iterate over all auftrag items 
            positionen = byauftrag[auftragsnummer]
            print auftragsnummer, len(positionen), [(pos.menge, pos.artnr) for pos in positionen]
            print repr(k.init_provisionings_multi([(pos.menge, pos.artnr) for pos in positionen]))
            ret =  k.init_provisionings_multi([(pos.menge, pos.artnr) for pos in positionen])
            if ret[0] == 'error':
                print ret
            else:
                ok, movements, picks = ret
                for mov in movements:
                    print "commit", mov, k.commit_movement(mov)
                for pick in picks:
                    print "commit", pick, k.commit_pick(pick)
            
        
    print answers
    print len(vorgaenge)
    
main()
