#!/usr/bin/env python
# encoding: utf-8
"""
picks.py

Created by Maximillian Dornseif on 2007-03-19.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

from sqlalchemy import Column, Table, Integer, String, DateTime, Boolean, CheckConstraint
from sqlalchemy import PassiveDefault, ForeignKey
from sqlalchemy import func

from kernel.metadataprovider import metadata

pick_table = Table('picks', metadata, 
    Column('id',                      Integer,    primary_key=True),
    Column('storage_location_id',     Integer,    ForeignKey("storage_locations.id")),
    Column('unit_load_mui',           Integer,    ForeignKey("unit_loads.mui")),
    Column('pick_list_id',            Integer,    ForeignKey("pick_lists.id")),
    Column('productkey',              String(30), default='', nullable=False),
    Column('quantity',                Integer,    default=None, nullable=True), # CheckConstraint('amount>0')
    Column('anbruch',                 Boolean,    default=False, nullable=False, index=True),
    Column('done',                    Boolean,    default=False, nullable=False, index=True),
    )

pick_list_table = Table('pick_lists', metadata,
    Column('id',                      Integer,    primary_key=True),
    Column('warehouse_id',            Integer,    ForeignKey('warehouses.id'), nullable=False),
    Column('auftragsnummer',          String(12), index=True),
    Column('kundennummer',            String(12), index=True),
    Column('lieferscheinnummer',      String(12), index=True),
    Column('komissionierbelegnummer', String(12), index=True),
    Column('status',                  String(12), default='', nullable=False, index=True),
    Column('prioritaet',              Integer,    CheckConstraint('prioritaet>=0 and prioritaet < 6'),
                                                  default=5, nullable=False),
    Column('done',                    Boolean,    default=False, nullable=False, index=True),
    )

class Pick(object):
    def __init__(self, picklist, unit_load, quantity):
        super(Pick, self).__init__()
        self.picklist = picklist
        self.unit_load = unit_load
        self.storage_location = unit_load.storage_location
        self.productkey = unit_load.productkey
        self.quantity = quantity
    
    def __unicode__(self):
        ret = u"%d x %s from %s" % (self.quantity, self.productkey, self.storage_location)
        if self.anbruch:
            ret += ', anbruch'
        return ret
    
class PickList(object):
    def __init__(self, warehouse=None):
        super(PickList, self).__init__()
        self.warehouse = warehouse
    
    def __unicode__(self):
        return u"PickList %d: %r" % (self.id, [unicode(x) for x in self.picks])
    

