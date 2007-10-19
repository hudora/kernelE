# spielt von dump_softm.py gespeicherte bestaende ins myPL.
import sys, pickle, gzip
sys.path.extend(['./python'])
from kernelE import Kerneladapter

def main():
    k = Kerneladapter()
    platzbestand = pickle.load(gzip.GzipFile('test/platzbestand.pickle.gz', 'r'))
    for platz in platzbestand.keys():
        if platz.endswith('01'):
            k.init_location(platz, floorlevel=True)
        elif platz.isdigit():
            k.init_location(platz, floorlevel=False)
        else:
            continue
        loc = k.location_info(platz)
        for mui in loc['allocated_by']:
            k.retrive(mui)
        (artnr, menge) = platzbestand[platz]
        if artnr and menge > 0:
            k.store_at_location(platz, menge, artnr)
        
main()
