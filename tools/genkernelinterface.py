#!/usr/bin/env python
# encoding: utf-8
"""
genkernelinterface.py - generiert Interface Funktionen in Erlang.

Created by Maximillian Dornseif on 2007-10-12.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

import sys
import os
import unittest



# Dies definiert der öffentliche API. Aus dem Code werden die diversen Schnittstellen erstellt
funcdefs = [
('mypl_db', 'init_location', 'Locname, Height, Floorlevel, Preference, Info, Attributes', []),
('mypl_db', 'location_info', 'Locname', []),
('mypl_db', 'location_list', '', []),
('mypl_db', 'store_at_location', 'Locname, Mui, Quantity, Product, Height', []),
('mypl_db', 'retrive', 'Mui', []),
('mypl_db', 'init_movement', 'Mui, Locname', []),
('mypl_db', 'init_movement_to_good_location', 'Mui', []),
('mypl_db', 'commit_movement', 'MovementId', []),
('mypl_db', 'rollback_movement', 'MovementId', []),
('mypl_db', 'init_pick', 'Quantity, Mui', []),
('mypl_db', 'commit_pick', 'PickId', []),
('mypl_db', 'rollback_pick', 'PickId', []),

('mypl_db_query', 'count_product', 'Product', []),
('mypl_db_query', 'count_products', '', []),

('mypl_provisioning', 'find_provisioning_candidates', 'Quantity, Product', []),
('mypl_provisioning', 'find_provisioning_candidates_multi', 'JsonList', []),
('mypl_provisioning', 'init_provisionings_multi', 'JsonList', []),

('mypl_movements', 'create_automatic_movements', '', []),
]

typemap = {
'Quantity':   'PositiveInteger',
'Height':     'PositiveInteger',
'Preference': 'PositiveInteger',
'Product':    'String',
'Locname':    'String',
'Mui':        'String',
'MovementId': 'String',
'PickId':     'String',
'Info':       'String',
'Floorlevel': 'Boolean',
'Attributes': 'Array',
'JsonList':   'JsonList',
}

out_genserver1 = ['%%%%%%%%%% autogenerated code follows']
out_genserver2 = ['\n%%%%%%%%%% call handlers (autogenerated) follow']
out_tcpserver = ['%%%%%%%%%% autogenerated code follows']
funclist = []
helptext = []
for funcdef in funcdefs:
    ns = {}
    ns['module'], ns['func'], ns['params'], ns['guards'] = funcdef
    ns['params'] = ns['params'].replace(' ', '')
    ns['func+params'] = ','.join([ns['func'], ns['params']])
    ns['guards'] = ','.join(ns['guards'])
    if ns['params']:
        ns['newparams'] = ','.join(["New"+x.strip() for x in ns['params'].split(',')])
    else:
        ns['newparams'] = ''
    if not ns['params']:
        parameters = newparameters = []
    else:
        parameters = [x.strip() for x in ns['params'].split(',')]
        newparameters = ["New"+x.strip() for x in ns['params'].split(',')]
    funclist.append('%s/%s' % (ns['func'], len(parameters)))
    helptext.append('%s %s' % (ns['func'], ','.join(parameters)))
    
    out_genserver1.append("""
%% implementation for API and backend for %(func)s
%(func)s(%(params)s) %(guards)s ->
     gen_server:call(?SERVER, {%(func)s, {%(params)s}}).""" % ns)
    out_genserver2.append("""
handle_call({%(func)s, {%(params)s}}, _From, State) ->
    Ret = %(module)s:%(func)s(%(params)s),
    {reply, Ret, State}""" % ns)
    
    # generating the code for our TCP server is much more komplex
    ret = []
    ret.append("""handle_command("%(func)s", _Parameters, State) ->
    %% implementation for %(func)s""" % ns)
    if ns['params'] and 'JsonList' not in ns['params']:
        ret.append("""    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [%(params)s] = Tokens""" % ns)
        for param in parameters:
            ns['param'] = param
            ns['converter'] = "convert%s" % typemap[param]
            ret.append("""    New%(param)s = %(converter)s(%(param)s)""" % ns)
    elif 'JsonList' in ns['params']:
        ret.append("""    {ok, NewJsonList, _} = rfc4627:decode(_Parameters)""")
    ret.append("""    {noreply, reply(220, rfc4627:encode(mypl_server:%(func)s(%(newparams)s)), reset_buffers(State))}""" % ns)
    out_tcpserver.append(',\n'.join(ret))

out_genserver1.insert(1, '%% -export([%s]).\n' % (','.join(funclist)))
fd = open('include/auto_genserverapi.hrl', 'w')
fd.write('\n'.join(out_genserver1))
fd.write(';\n'.join(out_genserver2))
fd.write('.\n')
fd.write('%%%%%%%%%% autogenerated code ends\n\n')
fd.close()

fd = open('include/auto_tcpapi.hrl', 'w')
fd.write(';\n'.join(out_tcpserver))
fd.write(';\n\nhandle_command("help", _Parameters, State) -> {noreply, reply(220, "Help follows:\n%s", reset_buffers(State))};' % '\n'.join(helptext))
fd.write('\nhandle_command("quit", _ClientDomain, State) -> {stop, normal, reply(201, "Goodbye", reset_buffers(State))};')
fd.write('\nhandle_command(Command, _Parameters, State) -> {noreply, reply(500, "Unsupported command " ++ Command, State)}.\n')
fd.write('\n%%%%%%%%%% autogenerated code ends\n')
fd.close()