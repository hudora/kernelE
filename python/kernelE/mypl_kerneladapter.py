#!/usr/bin/env python
# encoding: utf-8
"""
mypl_kerneladapter.py

Created by Maximillian Dornseif on 2007-10-12.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""


import unittest
import socket, uuid, simplejson, pickle, datetime, time, types

DEBUG = True

def e2string(data):
    # if we got a list of numbers turn it into a string
    if data and data[0] and type(data[0]) == types.IntType: # type(17) == Integer
        return ''.join([chr(x) for x in data])
    return data


def e2datetime(data):
    date, time = data[:2]
    year, month, day = date
    hour, minute, second = time
    if len(data) == 2:
        return datetime.datetime(year, month, day, hour, minute, second)
    if len(data) == 3:
        microsecond = data[2]
        return datetime.datetime(year, month, day, hour, minute, second, microsecond)


def attributelist2dict(l, fixattnames=[]):
    """Converts an Erlang Proplit to a Python Dict.
    
    See http://www.erlang.org/doc/man/proplists.html for proplists.
    Using Json we have issues converting Erlang strings to Python strings.
    This function tries to convert all keys to strings and all values where
    the key is present in fixattnames.
    """
    ret = {}
    for name, value in l:
        if name in fixattnames:
            ret[e2string(name)] = e2string(value)
        else:
            ret[e2string(name)] = value
    return ret

def attributelist2dict_str(l):
    """Like attributelist2dict but tries to convert _all_ values to strings."""
    
    ret = {}
    for name, value in l:
        if type(value) == types.ListType:
            ret[e2string(name)] = e2string(value)
        else:
            ret[e2string(name)] = value
    return ret


def print_timing(func):
    """Decorator to print execution time of functions if options.debug is True."""
    
    def _wrapper(*args, **kwargs):
        """Closure providing the actual functionality of print_timing()"""
        
        if DEBUG:
            print "calling %r: " % (func),
        start = time.time()
        try:
            ret = func(*args, **kwargs)
        except:
            print '\n', repr((args, kwargs)), '\n'
            raise
        finally:
            if DEBUG:
                delta = time.time() - start
                print "\t%2.5fs" % (delta)
        
        return ret
    return _wrapper


class Kerneladapter:
    def __init__(self):
        self.host = 'localhost'
        self.port = 5711
        self.connected = False
        self.debug = False
    
    def __del__(self):
        if self.connected:
            self.sock.close()
    
    def _init_connection(self):
        """connects to server"""
        if not  self.connected:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.connect((self.host, self.port))
            self.connected = True
            header = self.sock.makefile().readline().strip()
            if not header.startswith("200 "):
                raise RuntimeError, "Error reading header: %r" % header
    
    def _read(self):
        data = self.sock.makefile().readline().strip()
        if self.debug:
            print "<<<", data
        return data
    
    def _read_json(self, code):
        data = self._read()
        codestr = "%d " % code
        if not data.startswith(codestr):
            raise RuntimeError, "unexpected reply: %r" % data
        data = data[len(codestr):]
        return simplejson.loads(data)
    
    def _read_code(self, code):
        data = self._read()
        codestr = "%d " % code
        if not data.startswith(codestr):
            raise RuntimeError, "unexpected reply: %r" % data
        return data[4:]
    
    def _send(self, line):
        self._init_connection()
        if self.debug:
            print ">>>", line
        self.sock.send(line + '\n')
    
    def count_product(self, product):
        """Gibt die Mengen und NVEs zu einem Produkt an.
        
        >>> import kernelE
        >>> k = kernelE.Kerneladapter_mock()
        >>> k.count_product("14612/01")
        ([85, 85, 0, 0], ['4d183fee-7fb4-11dc-97fa-0017f2c8caff', '4e20d496-7fb4-11dc-97fa-0017f2c8caff'])
        """
        
        mui = product.replace(',','').replace('\n','').replace('\r','')
        self._send("count_product %s" % (product,))
        mengen, muis = self._read_json(220)
        return (mengen, [e2string(x) for x in muis])
    
    def count_products(self):
        """Gibt die Mengen für alle Produkte im Lager zurück.
        
        >>> import kernelE
        >>> k = kernelE.Kerneladapter_mock()
        >>> k.count_products()
        [('10011', 8, 8, 0, 0), ('10104', 131, 131, 0, 0), '...']
        """
        
        self._send("count_products")
        ret = self._read_json(220)
        return [(e2string(product), fmenge, amenge, rmenge, pmenge) for (product, fmenge, amenge, rmenge, pmenge) in ret]
        #return (mengen, [e2string(x) for x in muis])
    
    def location_list(self):
        """Returns a list of all location names.
        
        >>> import kernelE
        >>> location_list()
        ['011601', '011701', '011801', '012001', '...']
        """
        
        self._send("location_list")
        return [e2string(x) for x in self._read_json(220)]
    
    def location_info(self, name):
        """Gibt Informationen zu einem Lagerplatz aus.
        
        >>> import kernelE
        >>> k = kernelE.Kerneladapter_mock()
        >>> k.location_info("104103")
        {u'info': 'BRUECKE', u'reserved_for': [], u'name': '104103', u'height': 3000, u'preference': 3, u'floorlevel': False, u'attributes': [], u'allocated_by': ['4d820122-7fb4-11dc-97fa-0017f2c8caff']}
        """
        
        self._send("location_info %s" % name)
        ok, d = self._read_json(220)
        d = attributelist2dict(d, ['name'])
        d['allocated_by'] = [e2string(x) for x in d['allocated_by']]
        d['reserved_for'] = [e2string(x) for x in d['reserved_for']]
        d['info'] = e2string(d['info'])
        return d
    
    def unit_info(self, name):
        """
        >>> import kernelE
        >>> k = kernelE.Kerneladapter_mock()
        >>> k.unit_info("4dac128c-7fb4-11dc-97fa-0017f2c8caff")
        {u'product': '10120', u'created_at': datetime.datetime(2007, 10, 21, 9, 2, 8, 823722), u'height': 1950, u'pick_quantity': 0, u'location': '100903', u'picks': [], u'attributes': [], u'movements': [], u'mui': '4dac128c-7fb4-11dc-97fa-0017f2c8caff', u'quantity': 24},
        """
        
        self._send("unit_info %s" % name)
        ok, d = self._read_json(220)
        d = attributelist2dict(d, ['mui', 'product', 'location'])
        d['movements'] = [e2string(x) for x in d['movements']]
        d['picks'] = [e2string(x) for x in d['picks']]
        d['created_at'] = e2datetime(d['created_at'])
        return d
    
    def movement_info(self, name):
        """
        >>> import kernelE
        >>> k = kernelE.Kerneladapter_mock()
        >>> k.movement_info('m1195651098.535517')
        {u'to_location': '212402', u'from_location': '012801', u'created_at': datetime.datetime(2007, 11, 21, 13, 18, 18, 538711), u'attributes': [], u'mui': '012801|30.0|10106|340059981000000463', u'id': 'm1195651098.535517'}
        """
        self._send("movement_info %s" % name)
        ok, d = self._read_json(220)
        d = attributelist2dict_str(d)
        d['created_at'] = e2datetime(d['created_at'])
        return d
        
    
    def movement_list(self):
        """Liefert eine Liste aller (offenen) Movements.
        
        >>> import kernelE
        >>> k = kernelE.Kerneladapter_mock()
        >>> k.movement_list()
        ["m1193-85203-450126-mypl_test@lichtblick",
         "m1193-85203-453117-mypl_test@lichtblick",
         "m1193-85203-455263-mypl_test@lichtblick",
         "m1193-85203-456898-mypl_test@lichtblick",
         "m1193-85203-459094-mypl_test@lichtblick"]
        """
        self._send("movement_list")
        return [e2string(x) for x in self._read_json(220)]
    
    
    def pick_list(self):
        """Liefert eine Liste aller (offenen) Picks.
        
        >>> import kernelE
        >>> k = kernelE.Kerneladapter_mock()
        >>> k.pick_list()
        ["p1193-85203-460109-mypl_test@lichtblick", "p1193-85203-461143-mypl_test@lichtblick"]
        """
        
        raise NotImplementedError
    
    
    def init_location(self, name, height=1950, floorlevel=False, preference=5, info='', attributes=[]):
        name = name.replace(',','').replace('\n','').replace('\r','')
        info = info.replace(',',' ').replace('\n','').replace('\r','')
        if not info: info = ' ' # the tokenizer used in kernelE can't handle 'foo,,bar', only 'foo, ,bar'
        # attributes are not implemented so far
        self._send("init_location %s,%d,%r,%d,%s,[]" % (name, height, floorlevel, preference, info))
        return self._read_code(220)
        
    def init_movement_to_good_location(self, mui):
        """Initialisiert ein movement an einen geeigneten Ort"""
        self._send("init_movement_to_good_location %s" % mui)
        ok, ret = self._read_json(220)
        return e2string(ret)
    
    def make_nve(self):
        self._send("make_nve")
        ret = self._read_json(220)
        return e2string(ret)
    
    def store_at_location(self, name, quantity, artnr, mui=None, height=1950):
        if mui == None:
            mui = "%s" % (self.make_nve())
        name = name.replace(',','').replace('\n','').replace('\r','')
        artnr = artnr.replace(',','').replace('\n','').replace('\r','')
        mui = mui.replace(',','').replace('\n','').replace('\r','')
        self._send("store_at_location %s,%s,%d,%s,%d" % (name, mui, quantity, artnr, height))
        ok, mui = self._read_json(220)
        return e2string(mui)
    
    def retrieve(self, mui):
        mui = mui.replace(',','').replace('\n','').replace('\r','')
        self._send("retrieve %s" % (mui,))
        ok, ret = self._read_json(220)
        ret[1] = e2string(ret[1])
        return ret
        
    
    def find_provisioning_candidates(self, quantity, artnr):
        artnr = artnr.replace(',','').replace('\n','').replace('\r','')
        self._send("find_provisioning_candidates %d,%s" % (quantity, artnr))
        ret = self._read_json(220)
        if ret[0] == 'ok':
            ok, retrievals, picks = ret
            retrievals = [e2string(x) for x in retrievals]
            picks = [(x[0], e2string(x[1])) for x in picks]
            ret = (ok, retrievals, picks)
        return ret
    
    def find_provisioning_candidates_multi(self, poslist):
        self._send("find_provisioning_candidates_multi %s" % (simplejson.dumps(poslist)))
        ret = self._read_json(220)
        if ret[0] == 'ok':
            ok, retrievals, picks = ret
            retrievals = [e2string(x) for x in retrievals]
            picks = [(x[0], e2string(x[1])) for x in picks]
            ret = (ok, retrievals, picks)
        return ret
    
    def init_provisionings_multi(self, poslist):
        self._send("init_provisionings_multi %s" % (simplejson.dumps(poslist)))
        ret = self._read_json(220)
        if ret[0] == 'ok':
            ok, retrievals, picks = ret
            retrievals = [e2string(x) for x in retrievals]
            picks = [e2string(x) for x in picks]
            ret = (ok, retrievals, picks)
        return ret
    
    def commit_movement(self, movementid):
        self._send("commit_movement %s" % (movementid))
        return self._read_json(220)
    
    def rollback_movement(self, movementid):
        self._send("rollback_movement %s" % (movementid))
        return self._read_json(220)
    
    def commit_retrieval(self, movementid):
        self._send("commit_retrieval %s" % (movementid))
        return self._read_json(220)
    
    def rollback_retrieval(self, movementid):
        self._send("rollback_retrieval %s" % (movementid))
        return self._read_json(220)
    
    def commit_pick(self, pickid):
        self._send("commit_pick %s" % (pickid))
        return self._read_json(220)
    
    def rollback_pick(self, pickid):
        self._send("rollback_pick %s" % (pickid))
        return self._read_json(220)
    
    def create_automatic_movements(self):
        self._send("create_automatic_movements")
        ret = self._read_json(220)
        ok, movements = ret
        ret = []
        for movement in movements:
            ok, movementid = movement
            ret.append(e2string(movementid))
        return ret
    
    def insert_pipeline(self, cid, orderlines, priority, customer, weigth, volume, attributes):
        """adds an order to the provisioningpipeline
        
        `CId' is a unique Id used by the client to refer to this Picking order, e.g. the "Lieferscheinnummer"
        or something similar. `Orderlines' is a list of Articles to
        provision. The List elements are tuples `{Quanity, Product, Attributes}' where Attributes contains
        arbitrary data for use at tha client side.
        The higher the `priority' the more likely it is, that the Order is processed early. If you want the
        scheduler to also consider day to deliver you have to encode that into priority. E.g.
        E.g. `NewPriority = Priority + 10 * max([(now() + 5 - order.day_to_deliver), 0])'.
        'Customer' is to aggregate shippments to the same customer. 'Weigth' and 'Volume' are the calculated
        total Weigth and Volume of the shippment and are used to make scheduling descisions.
        
        
        insert_pipeline(Id, [(20, 10106, {"auftragsposition": 1, "gewicht": 34567}),
                 (70, 14650, {"auftragsposition": 2, "gewicht": 35667}),
                 (30, 76500, {"auftragsposition": 3, "gewicht": 12367})],
                 28, "34566", 345000, 581.34,
                 {"auftragsnumer": "123432", "liefertermin": "2007-12-23"}).
        """
        
        newOrderlines = []
        for orderline in orderlines:
            newOrderlines.append((orderline[0], orderline[1], orderline[2].items()))
        parameters = (str(cid), newOrderlines, int(priority), unicode(customer).encode('utf-8'),
                           int(weigth), float(volume), attributes.items())
        self._send("insert_pipeline %s" % (simplejson.dumps(parameters)))
    
    
    def get_picklists(self):
        """returns one or more Picklists to be processed next.
        
        >>> get_picklist()
        [('p1195654200.622052', '40145201', 'AUSLAG', 1, {'liefertermin': '2007-11-12'},
             [('P1195654200.621917', '340059981000021932', 15, 092001', '83161', {})]),
         ('p1195654200.622053', '40145202', 'AUSLAG', 1, {'liefertermin': '2007-11-13'},
             [('P1195654200.621918', '340059981000021943', 4, '092002', '83161', {})])]
        
        """
        self._send("get_picklists")
        ret = self._read_json(220)
        if ret == 'nothing_available':
            return []
        out = []
        for data in ret:
            pickListId, cId, destination, attributes, parts, positions = data
            pickListId = e2string(pickListId)
            cId = e2string(cId)
            destination = e2string(destination)
            poslist = []
            for position in positions:
                (posId, nve, source, quantity, product, posattributes) = position
                posId = e2string(posId)
                nve = e2string(nve)
                source = e2string(source)
                product = e2string(product)
                poslist.append((posId, nve, source, quantity, product, attributelist2dict_str(posattributes)))
            out.append((pickListId, cId, destination, parts, attributelist2dict_str(attributes), poslist))
        return out
    
    
    def get_retrievallists(self):
        """returns one or more Retrievallists to be processed next.
        
        >>> get_retrievallists()
        [('r1195655518.977542', '40145183', 'AUSLAG', 2, {'liefertermin': '2007-11-19'}, 
            [('m1195655518.977156', '340059981000897650', '042802', '14695', {})])]
        """
        
        self._send("get_retrievallists")
        ret = self._read_json(220)
        if ret == 'nothing_available':
            return []
        out = []
        for data in ret:
            retrievalListId, cId, destination, attributes, parts, positions = data
            retrievalListId = e2string(retrievalListId)
            cId = e2string(cId)
            destination = e2string(destination)
            poslist = []
            for position in positions:
                (posId, nve, source, quantity, product, posattributes) = position
                posId = e2string(posId)
                nve = e2string(nve)
                source = e2string(source)
                product = e2string(product)
                poslist.append((posId, nve, source, product, attributelist2dict_str(posattributes)))
            out.append((retrievalListId, cId, destination, parts, attributelist2dict_str(attributes), poslist))
        return out
        
    
    def get_movementlist(self):
        self._send("get_movementlist")
        ret = self._read_json(220)
        if ret == 'nothing_available':
            return []
        ok, mIds = ret
        out = []
        for mId in mIds:
            out.append(e2string(mId))
        return out
    
    def commit_picklist(self, cId):
        self._send("commit_picklist %s" % (cId,))
        ret = self._read_json(220)
        return ret
    
    def commit_retrievallist(self, cId):
        self._send("commit_retrievallist %s" % (cId,))
        ret = self._read_json(220)
        return ret
    
    