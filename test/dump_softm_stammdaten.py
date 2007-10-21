# Speichert die SoftM Platzstammdaten.
import sys, pickle, gzip, datetime
sys.path.extend(['../..', './python'])
from kernelE import Kerneladapter
from mofts.client import as400

hoehenmapping = {'00': 2100, '03': 1800, '04': 1450, '05': 1150, '06': 1050}

def main():
    print "reading from SoftM & writing to myPL"
    k = Kerneladapter()
    softm = as400.MoftSconnection()
    plaetze = softm.get_fixplaetze() + softm.get_belegteplaetze() + softm.get_freieplaetze()
    
    print "fetching location data"
    platzinfo = {}
    for platz in plaetze:
        if platz in ["RETOUR", "KATALO", "FERTZU", "FERTAB", "BEREIT", "SOFORT", "######", "SONDER", "UMLAG"]:
            continue
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
        if platz.startswith('01') and (platz.endswith('02') or platz.endswith('03')):
            preferaenz = 0
        if pinfo['EinlagRestr'] == '99':
            preferaenz = 0
        if pinfo['info'] == 'BRUECKE':
            hoehe = 3000
        if not platz.isdigit():
            preferaenz = 0
        platzinfo[platz] = {'preferaenz': preferaenz, 'hoehe': hoehe, 'info': pinfo['info']}
        print platz, platzinfo[platz]
        if platz.endswith('01'):
            k.init_location(platz, height=hoehe, floorlevel=True, preference=preferaenz, info=pinfo['info'])
        else:
            k.init_location(platz, height=hoehe, floorlevel=False, preference=preferaenz, info=pinfo['info'])
        # platz leeren
        loc = k.location_info(platz)
        for mui in loc['allocated_by']:
            k.retrive(mui)

    filename = 'test/data/platzinfo.pickle.gz'
    print "saving", filename
    pickle.dump(platzinfo, gzip.GzipFile(filename, 'w'))

main()

