# laed Platz und Vorgagsdaten aus SoftM und speichert sie als Pickle
import sys, pickle, gzip, datetime
sys.path.extend(['../..', './python'])
from kernelE import Kerneladapter
from mofts.client import as400

def main():
    datestr = datetime.datetime.today().strftime('%Y%m%d')
    print "reading from SoftM"
    softm = as400.MoftSconnection()
    plaetze = softm.get_fixplaetze() + softm.get_belegteplaetze() + softm.get_freieplaetze()
    vorgaenge = softm.get_protokomissioniervorgaenge()
    filename = 'test/data/vorgaenge-%s.pickle.gz' % datestr
    print "saving", filename
    pickle.dump(vorgaenge, gzip.GzipFile(filename, 'w'))
    
    print "fetching location data"
    platzbestand = {}
    for platz in plaetze:
        (artnr, menge) = softm.get_platzbestand(platz)
        platzbestand[platz] = (artnr, menge)
    filename = 'test/data/platzbestand-%s.pickle.gz' % datestr
    print "saving", filename
    pickle.dump(platzbestand, gzip.GzipFile(filename, 'w'))

main()