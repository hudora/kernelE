#!/usr/bin/env python
# encoding: utf-8
"""
mypl_kerneladapter.py

Created by Maximillian Dornseif on 2007-10-12.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""


import unittest
import socket, uuid, simplejson, pickle, datetime

def e2string(data):
    # if we got a list of numbers turn it into a string
    if data and data[0] and type(data[0]) == type(17):
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
    ret = {}
    for name, value in l:
        if name in fixattnames:
            ret[name] = e2string(value)
        else:
            ret[name] = value
    return ret
    

class Kerneladapter:
    def __init__(self):
        self.host = 'localhost'
        self.port = 5711
        self.connected = False
    
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
        # print "<<<", data
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
        # print ">>>", line
        self.sock.send(line + '\n')
    
    def count_product(self, product):
        mui = product.replace(',','').replace('\n','').replace('\r','')
        self._send("count_product %s" % (product,))
        mengen, muis = self._read_json(220)
        return (mengen, [e2string(x) for x in muis])
    
    def count_products(self):
        self._send("count_products")
        ret = self._read_json(220)
        return [(e2string(product), fmenge, amenge, rmenge, pmenge) for (product, fmenge, amenge, rmenge, pmenge) in ret]
        #return (mengen, [e2string(x) for x in muis])
    
    def location_list(self):
        self._send("location_list")
        return [e2string(x) for x in self._read_json(220)]
    
    def location_info(self, name):
        self._send("location_info %s" % name)
        ok, d = self._read_json(220)
        d = attributelist2dict(d, ['name'])
        d['allocated_by'] = [e2string(x) for x in d['allocated_by']]
        d['reserved_for'] = [e2string(x) for x in d['reserved_for']]
        d['info'] = e2string(d['info'])
        return d
    
    def unit_info(self, name):
        self._send("unit_info %s" % name)
        ok, d = self._read_json(220)
        d = attributelist2dict(d, ['mui', 'product', 'location'])
        d['movements'] = [e2string(x) for x in d['movements']]
        d['picks'] = [e2string(x) for x in d['picks']]
        d['created_at'] = e2datetime(d['created_at'])
        return d
    
    def init_location(self, name, height=1950, floorlevel=False, preference=5, info='', attributes=[]):
        name = name.replace(',','').replace('\n','').replace('\r','')
        info = info.replace(',',' ').replace('\n','').replace('\r','')
        if not info: info = ' ' # the tokenizer used in kernelE can't handle 'foo,,bar', only 'foo, ,bar'
        # attributes are not implemented so far
        self._send("init_location %s,%d,%r,%d,%s,[]" % (name, height, floorlevel, preference, info))
        return self._read_code(220)
        
    def make_nve(self):
        self._send("make_nve")
        ret = self._read_json(220)
        return e2string(ret)
        
    def store_at_location(self, name, quantity, artnr, mui=None, height=1950):
        if mui == None:
            mui = "%s|%s|%s|%s" % (name, quantity, artnr, self.make_nve())
        name = name.replace(',','').replace('\n','').replace('\r','')
        artnr = artnr.replace(',','').replace('\n','').replace('\r','')
        mui = mui.replace(',','').replace('\n','').replace('\r','')
        self._send("store_at_location %s,%s,%d,%s,%d" % (name, mui, quantity, artnr, height))
        return self._read_code(220)
    
    def retrieve(self, mui):
        mui = mui.replace(',','').replace('\n','').replace('\r','')
        self._send("retrieve %s" % (mui,))
        return self._read_code(220)
        
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
        print ret
        ok, movements = ret
        ret = []
        for movement in movements:
            ok, movementid = movement
            ret.append(e2string(movementid))
        return ret
