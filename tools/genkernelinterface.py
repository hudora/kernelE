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

TIMEOUT = 9999 # timeout parameter in ms fuer gen_server:call/3

# Dies definiert der öffentliche API. Aus dem Code werden die diversen Schnittstellen erstellt
funcdefs = [
('mypl_db', 'backup', '', []),
('mypl_db', 'init_location', 'Locname, Height, Floorlevel, Preference, Info, Attributes', []),
('mypl_db', 'store_at_location', 'Locname, Mui, Quantity, Product, Height', []),
('mypl_db', 'retrieve', 'Mui', []),
('mypl_db', 'init_movement', 'Mui, Locname', []),
('mypl_db', 'init_movement_to_good_location', 'Mui', []),
('mypl_db', 'commit_movement', 'MovementId', []),
('mypl_db', 'rollback_movement', 'MovementId', []),
('mypl_db', 'commit_retrieval', 'MovementId', []),
('mypl_db', 'rollback_retrieval', 'MovementId', []),
('mypl_db', 'init_pick', 'Quantity, Mui', []),
('mypl_db', 'commit_pick', 'PickId', []),
('mypl_db', 'rollback_pick', 'PickId', []),

('mypl_db_query', 'count_product', 'Product', []),
('mypl_db_query', 'count_products', '', ['async']),
('mypl_db_query', 'unit_list', '', ['async']),
('mypl_db_query', 'unit_info', 'Mui', []),
('mypl_db_query', 'location_list', '', ['async']),
('mypl_db_query', 'location_info', 'Locname', []),
('mypl_db_query', 'movement_list', '', ['async']),
('mypl_db_query', 'movement_info', 'MovementId', []),
('mypl_db_query', 'pick_list', '', ['async']),
('mypl_db_query', 'pick_info', 'PickId', []),

('mypl_provisioning', 'find_provisioning_candidates', 'Quantity, Product', ['async']),
('mypl_provisioning', 'find_provisioning_candidates_multi', 'JsonList', ['async']),
('mypl_provisioning', 'init_provisionings_multi', 'JsonList', ['async']),

('mypl_provpipeline', 'insert_pipeline', 'JsonList', []),
('mypl_provpipeline', 'get_picklists', '', ['async']),
('mypl_provpipeline', 'get_retrievallists', '', ['async']),
('mypl_provpipeline', 'get_movementlist', '', ['async']),
('mypl_provpipeline', 'commit_picklist', 'CId', []),
('mypl_provpipeline', 'commit_retrievallist', 'CId', []),
('mypl_provpipeline', 'commit_movementlist', 'CId', []),
('mypl_provpipeline', 'is_provisioned', 'CId', []),
('mypl_provpipeline', 'provpipeline_list_new', '', ['async']),
('mypl_provpipeline', 'provpipeline_list_processing', '', ['async']),
('mypl_provpipeline', 'provpipeline_processing_list_all', '', ['async']),
('mypl_provpipeline', 'delete_pipeline', '', []),

('mypl_audit', 'get_articleaudit', 'Product', ['async']),
('mypl_audit', 'get_unitaudit', 'Mui', ['async']),

('mypl_abcserver', 'get_abc', '', ['async']),

('mypl_nveserver', 'make_oid', '', []),
('mypl_nveserver', 'make_nve', '', []),
]


typemap = {
'Quantity':   'PositiveInteger',
'Height':     'PositiveInteger',
'Preference': 'PositiveInteger',
'Priority':   'PositiveInteger',
'Weigth':     'PositiveInteger',
'Volume':     'PositiveInteger',
'Product':    'String',
'Locname':    'String',
'Mui':        'String',
'MovementId': 'String',
'PickId':     'String',
'CId':        'String',
'Info':       'String',
'Customer':   'String',
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
    ns = {'timeout': TIMEOUT}
    ns['module'], ns['func'], ns['params'], ns['options'] = funcdef
    ns['params'] = ns['params'].replace(' ', '')
    ns['func+params'] = ','.join([ns['func'], ns['params']])
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
    
    if 'async' in ns['options']:
        out_genserver1.append("""
%% implementation for API and backend for %(func)s
%(func)s(%(params)s) ->
    gen_server:call(?SERVER, {%(func)s, {%(params)s}}, %(timeout)d).""" % ns)

        out_genserver2.append("""
handle_call({%(func)s, {%(params)s}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, %(module)s:%(func)s(%(params)s)) end),
    {noreply, State, %(timeout)d}""" % ns)

    else:
        out_genserver1.append("""
%% implementation for API and backend for %(func)s
%(func)s(%(params)s) ->
    gen_server:call(?SERVER, {%(func)s, {%(params)s}}, %(timeout)d).""" % ns)

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