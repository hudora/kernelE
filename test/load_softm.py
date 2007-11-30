# spielt von dump_softm.py gespeicherte bestaende ins myPL.
import sys, pickle, gzip, socket
sys.path.extend(['./python'])
from kernelE import Kerneladapter

def load_platzbestand(platzbestand):
    k = Kerneladapter()
    loc = k.location_info("AUSLAG")
    for mui in loc['allocated_by']:
         k.retrieve(mui)
    loc = k.location_info("FEHLER")
    for mui in loc['allocated_by']:
         k.retrieve(mui)
    
    for platz in platzbestand.keys():
        (artnr, menge) = platzbestand[platz]
        if artnr and menge > 0:
            if platz in ['KATALO', '######', 'BEREIT', 'FERTAB', 'FERTZU', 'SOFORT', 'SONDER', 'UMLAG', 'RETOUR', 'VERSAN']:
                print "problem mit %r" % ((artnr, menge, platz),)
                platz = 'FEHLER'
            k = Kerneladapter()
            loc = k.location_info(platz)
            for mui in loc['allocated_by']:
                if platz != 'FEHLER':
                    unit = k.unit_info(mui)
                    for movement in unit['movements']:
                        k.rollback_movement(movement)
                    for pick in unit['picks']:
                        k.rollback_pick(pick)
                    k.retrieve(mui)
            try:
                mui = k.store_at_location(platz, menge, artnr)
                if platz == 'FEHLER':
                    # move Muis on FEHLER somewhere else
                    movementid = k.init_movement_to_good_location(mui)
                    k.commit_movement(movementid)
            except RuntimeError, msg:
                print msg
                raise

def main():
    platzbestand = pickle.load(gzip.GzipFile('test/data/platzbestand-20071128-umstellung.pickle.gz', 'r'))
    load_platzbestand(platzbestand)
    
if __name__ == '__main__':
    main()
