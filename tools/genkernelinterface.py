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

TIMEOUT = 39999 # timeout parameter in ms fuer gen_server:call/3

# Dies definiert der öffentliche API. Aus dem Code werden die diversen Schnittstellen erstellt
funcdefs = [
#('mypl_db', 'backup', '', []),
('mypl_db', 'init_location', 'Locname, Height, Floorlevel, Preference, Info, Attributes', []), # PUT location/name
('mypl_db', 'store_at_location', 'Locname, Mui, Quantity, Product, Height', []),
('mypl_db', 'store_at_location_multi', 'JsonList', []),

('mypl_db', 'retrieve', 'Mui', []),                  # obsolete
('mypl_db', 'init_movement', 'Mui, Locname', []),
('mypl_db', 'init_movement_to_good_location', 'Mui', []),
('mypl_db', 'commit_movement', 'MovementId', []),
('mypl_db', 'rollback_movement', 'MovementId', []),  # DELETE /movemnt/oid
('mypl_db', 'commit_retrieval', 'MovementId', []),   # obsolete
('mypl_db', 'rollback_retrieval', 'MovementId', []), # obsolete
('mypl_db', 'init_pick', 'Quantity, Mui', []),       # obsolete
('mypl_db', 'commit_pick', 'PickId', []),            # obsolete
('mypl_db', 'rollback_pick', 'PickId', []),          # obsolete
('mypl_db', 'correction', 'JsonList', []),
('mypl_db', 'update_unit', 'JsonList', []),          # POST /unit/mui

# TODO: move to http
('mypl_db_query',       'count_product', 'Product', []),  # GET /product/artnr
('mypl_db_query',       'count_products', '', ['async']), # GET /product
('mypl_db_query',       'unit_list', '', ['async']),      # GET /unit
('mypl_db_query',       'unit_info', 'Mui', ['async']),   # GET /unit/mui
('mypl_db_query',       'location_list', '', ['async']),  # GET /location
('mypl_db_query',       'location_info', 'Locname', []),  # GET /location/name
('mypl_db_query',       'movement_list', '', ['async']),  # GET /movement
('mypl_db_query',       'movement_info', 'MovementId', ['async']), # GET /movement/oid
('mypl_db_query',       'pick_list', '', ['async']),      # GET /pick
('mypl_db_query',       'pick_info', 'PickId', ['async']),# GET /pick/oid
('mypl_abcserver',      'get_abc', '', ['async']),        # GET /abc
('mypl_abcserver',      'get_abcclass', 'Product', ['async']), # obsolete
('mypl_requesttracker', 'dump_requests', '', []),         # GET /requesttracker
('mypl_statistics',     'statistics', '', ['async']),     # GET /statistics
('mypl_statistics',     'bewegungen', '', ['async']),

('mypl_prov_query',     'provpipeline_info', 'CId', []),
('mypl_prov_query',     'provpipeline_list_new', '', ['async']),
('mypl_prov_query',     'provpipeline_list_processing', '', ['async']),
('mypl_prov_query',     'provpipeline_list_prepared', '', ['async']),
('mypl_prov_query',     'provisioninglist_list', '', ['async']),
('mypl_prov_query',     'provisioninglist_info', 'CId', []),

('mypl_db_util', 'find_empty_location_nice', 'Height', []), # obsolete

# TODO: rename
('mypl_provpipeline', 'insert_pipeline', 'JsonList', []),
('mypl_provpipeline', 'commit_picklist', 'CId', []),
('mypl_provpipeline', 'commit_retrievallist', 'CId', []),
('mypl_provpipeline', 'get_picklists', '', ['async']),
('mypl_provpipeline', 'get_retrievallists', '', ['async']),
('mypl_provpipeline', 'get_movementlist', '', ['async']), # neu: POST /movement

('mypl_prov_special', 'delete_kommiauftrag', 'CId', []),
('mypl_prov_special', 'update_pipeline', 'JsonList', []),

('mypl_nveserver', 'make_oid', '', []), # obsolete
('mypl_nveserver', 'make_nve', '', []),


('mypl_volumes', 'feed_eap', 'Product, Prod_ve1, Prod_exportpackage, Export_pallet, Prod_x, Prod_y, Prod_z, Prod_g, Ve1_x, Ve1_y, Ve1_z, Ve1_g, Export_x, Export_y, Export_z, Export_g', []),
]


typemap = {
'Quantity':          'PositiveInteger',
'Height':            'PositiveInteger',
'Preference':        'PositiveInteger',
'Priority':          'PositiveInteger',
'Weigth':            'PositiveInteger',
'Volume':            'PositiveInteger',
'Prod_ve1':          'PositiveInteger',
'Prod_exportpackage':'PositiveInteger',
'Export_pallet':     'PositiveInteger',
'Prod_x':            'PositiveInteger',
'Prod_y':            'PositiveInteger',
'Prod_z':            'PositiveInteger',
'Prod_g':            'PositiveInteger',
'Ve1_x':             'PositiveInteger',
'Ve1_y':             'PositiveInteger',
'Ve1_z':             'PositiveInteger',
'Ve1_g':             'PositiveInteger',
'Export_x':          'PositiveInteger',
'Export_y':          'PositiveInteger',
'Export_z':          'PositiveInteger',
'Export_g':          'PositiveInteger',
'Product':           'String',
'Locname':           'String',
'Mui':               'String',
'MovementId':        'String',
'PickId':            'String',
'CId':               'String',
'Info':              'String',
'Customer':          'String',
'Type':              'String',
'Floorlevel':        'Boolean',
'Attributes':        'Array',
'JsonList':          'JsonList',
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