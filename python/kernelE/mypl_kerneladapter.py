#!/usr/bin/env python
# encoding: utf-8
"""
mypl_kerneladapter.py

Created by Maximillian Dornseif on 2007-10-12.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

import sys
import os
import unittest
import socket, uuid, simplejson, pickle

def e2string(data):
    return ''.join([chr(x) for x in data])

def attributelist2dict(l, fixattnames):
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
    
    def _init_connection(self):
        """connects to server"""
        if not  self.connected:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.connect((self.host, self.port))
            self.connected = True
    
    def _read(self):
        data = self.sock.makefile().readline().strip()
        print "<<<", data
        if data.startswith("200 "):
            return self._read()
        return data
    
    def _read_json(self, code):
        data = self._read()
        codestr = "%d " % code
        if not data.startswith(codestr):
            raise RuntimeError, "unexpected reply: %r" % data
        data = data[len(codestr):]
        ok, reply = simplejson.loads(data)
        return reply
    
    def _read_code(self, code):
        data = self._read()
        codestr = "%d " % code
        if not data.startswith(codestr):
            raise RuntimeError, "unexpected reply: %r" % data
        return data
    
    def _send(self, line):
        self._init_connection()
        print ">>>", line
        self.sock.send(line + '\n')
    
    def location_list(self):
        self._send("location_list")
        return [e2string(x) for x in self._read_json(220)]
    
    def location_info(self, name):
        self._send("location_info %s" % name)
        return attributelist2dict(self._read_json(220), ['name'])
    
    def init_location(self, name, height=1950, floorlevel=False, preference=5, attributes=[]):
        name = name.replace(',','').replace('\n','').replace('\r','')
        # attributes are not implemented so far
        self._send("init_location %s,%d,%r,%d,[]" % (name, height, floorlevel, preference))
        return self._read_code(220)
        
    # % store_at_location(Locname, Mui, Quantity, Product, Height)
    def store_at_location(self, name, quantity, artnr, mui=None, height=1950):
        if mui == None:
            mui = str(uuid.uuid1())
        name = name.replace(',','').replace('\n','').replace('\r','')
        artnr = artnr.replace(',','').replace('\n','').replace('\r','')
        mui = mui.replace(',','').replace('\n','').replace('\r','')
        self._send("store_at_location %s,%s,%d,%s,%d" % (name, mui, quantity, artnr, height))
        return self._read_code(220)

        
    

if __name__ == '__main__':
    k = Kerneladapter()
    #k.location_list()
    print k.location_info("EINLAG")
    print k.init_location("010101", height=1950, floorlevel=True)
    print k.store_at_location("010101", 5, "65535")
    
    # sys.exit(0)
    # this only works with the correct PYTHONPATH
    from mofts.client import as400  
    softm = as400.MoftSconnection()
    plaetze = softm.get_belegteplaetze() + softm.get_freieplaetze()
    #pickle.dump(plaetze, open('paletze.pickle', 'w'))
    
    vorgaenge = softm.get_protokomissioniervorgaenge()
    #pickle.dump(vorgaenge, open('vorgaenge.pickle', 'w'))
    
    for platz in plaetze:
        if platz.endswith('01'):
            print k.init_location(platz, floorlevel=True)
        elif platz.isdigit():
            print k.init_location(platz, floorlevel=False)
        (artnr, menge) = softm.get_platzbestand(platz)
        if artnr and menge:
            print "storing", k.store_at_location(platz, menge, artnr)
