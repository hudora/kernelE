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
    if data and data[0] and type(data[0]) == type(17):
        return ''.join([chr(x) for x in data])
    return data

def attributelist2dict(l, fixattnames=[]):
    ret = {}
    print repr(l)
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
        # print "<<<", data
        if data.startswith("200 "):
            return self._read()
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
    
    def location_list(self):
        self._send("location_list")
        return [e2string(x) for x in self._read_json(220)]
    
    def location_info(self, name):
        self._send("location_info %s" % name)
        ok, d = self._read_json(220)
        d = attributelist2dict(d, ['name'])
        d['allocated_by'] = [e2string(x) for x in d['allocated_by']]
        d['reserved_for'] = [e2string(x) for x in d['reserved_for']]
        return d
    
    def init_location(self, name, height=1950, floorlevel=False, preference=5, attributes=[]):
        name = name.replace(',','').replace('\n','').replace('\r','')
        # attributes are not implemented so far
        self._send("init_location %s,%d,%r,%d,[]" % (name, height, floorlevel, preference))
        return self._read_code(220)
        
    def store_at_location(self, name, quantity, artnr, mui=None, height=1950):
        if mui == None:
            mui = "%s|%s|%s|%s" % (name, quantity, artnr, uuid.uuid1())
        name = name.replace(',','').replace('\n','').replace('\r','')
        artnr = artnr.replace(',','').replace('\n','').replace('\r','')
        mui = mui.replace(',','').replace('\n','').replace('\r','')
        self._send("store_at_location %s,%s,%d,%s,%d" % (name, mui, quantity, artnr, height))
        return self._read_code(220)
    
    def retrive(self, mui):
        mui = mui.replace(',','').replace('\n','').replace('\r','')
        self._send("retrive %s" % (mui,))
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
        
    def commit_pick(self, pickid):
        self._send("commit_pick %s" % (pickid))
        return self._read_json(220)
