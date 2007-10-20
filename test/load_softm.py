# spielt von dump_softm.py gespeicherte bestaende ins myPL.
import sys, pickle, gzip, socket
sys.path.extend(['./python'])
from kernelE import Kerneladapter

def main():
    platzbestand = pickle.load(gzip.GzipFile('test/data/platzbestand-20071017.pickle.gz', 'r'))
    for platz in platzbestand.keys():
        (artnr, menge) = platzbestand[platz]
        if artnr and menge > 0:
            if platz in ['KATALO', '######', 'BEREIT', 'FERTAB', 'FERTZU', 'SOFORT', 'SONDER', 'UMLAG']:
                continue
            k = Kerneladapter()
            loc = k.location_info(platz)
            for mui in loc['allocated_by']:
                k.retrive(mui)
            try:
                k.store_at_location(platz, menge, artnr)
            except RuntimeError, msg:
                print msg
        
main()
