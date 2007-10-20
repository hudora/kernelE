# laed Platz und Vorgagsdaten aus SoftM und speichert sie als Pickle
import sys, pickle, gzip, datetime
sys.path.extend(['../..', './python'])
from kernelE import Kerneladapter
from mofts.client import as400

hoehenmapping = {'00': 2100, '03': 1800, '04': 1450, '05': 1150, '06': 1050}

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
    platzinfo = {}
    for platz in plaetze:
        (artnr, menge) = softm.get_platzbestand(platz)
        platzbestand[platz] = (artnr, menge)
        pinfo = softm.get_platzinfo(platznr=platz)[0]
        hoehe = hoehenmapping.get(pinfo['behaelterstandart'], 1950)
        if int(pinfo['platztyp']) == 0:
            pinfo['platztyp'] = 50
        preferaenz = (108- int(pinfo['platztyp'])) / 10
        if platz.endswith('02'):
            # prefer medium row
            preferaenz += 2
        if platz.endswith('03'):
            # dicourage upper row
            preferaenz -= 3
            if hoehe > 2000:
                hoehe = 1800
        if platz[2] in ['2', '3']:
            # prefer center locations
            preferaenz += 1
        if platz[2] in ['5']:
            # avoid outmost locations
            preferaenz -= 1
        if pinfo['info']:
            # if something is special about the platz discourage it
            preferaenz -= 2
        if platz.startswith('2'):
            # avoid last row
            preferaenz -= 1
        preferaenz = min([preferaenz, 9])
        preferaenz = max([preferaenz, 1])
        if pinfo['EinlagRestr'] == '99':
            preferaenz = 0
        if pinfo['info'] == 'BRUECKE':
            hoehe = 3000
        platzinfo[platz] = {'preferaenz': preferaenz, 'hoehe': hoehe, 'info': pinfo['info']}
        print platz, platzinfo[platz]
    filename = 'test/data/platzinfo.pickle.gz'
    print "saving", filename
    pickle.dump(platzinfo, gzip.GzipFile(filename, 'w'))
    filename = 'test/data/platzbestand-%s.pickle.gz' % datestr
    print "saving", filename
    pickle.dump(platzbestand, gzip.GzipFile(filename, 'w'))

main()

{
'artnr': u'72111', 
'bebaeltertyp01': u'', 
'bebaeltertyp02': u'', 
'bebaeltertyp03': u'', 
'bebaeltertyp04': u'', 
'bebaeltertyp05': u'', 
'bebaeltertyp06': u'',
'bebaeltertyp07': u'', 
'bebaeltertyp08': u'', 
'bebaeltertyp09': u'', 
'bebaeltertyp10': u'',
'behaelteranzahl': 9, 
'behaelterbelegung': u'00', 
'behaelterstandart': u'00', 
'info': u'', 
'platzfreigabe': 0,
'platztyp': 0,
'status': u'', 
'LPSPEZ': 'C',
}
