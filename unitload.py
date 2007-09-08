#!/usr/bin/env python
# encoding: utf-8
"""
unitload.py

Created by Maximillian Dornseif on 2007-01-21.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

from sqlalchemy import Column, Table, Integer, String, DateTime, Boolean, CheckConstraint
from sqlalchemy import PassiveDefault, ForeignKey
from sqlalchemy import func

from kernel.metadataprovider import metadata
from kernel.movement import movement_table

unit_load_table = Table('unit_loads', metadata, 
    Column('mui', Integer, primary_key=True),
    Column('warehouse_id', Integer, ForeignKey('warehouses.id'), nullable=False),
    Column('storage_location_id', Integer, ForeignKey("storage_locations.id")),
    # help_text="Brutto height of the bin in mm."
    Column('height', Integer, default=2050), # CheckConstraint('height>0')
    # weight in g
    Column('weight', Integer, default=None, nullable=True), # CheckConstraint('height>0')
    Column('productkey', String(30), default='', nullable=False),
    # help_text="Text describing the content of the bin.")
    Column('content_description', String(80), default='', nullable=False),
    Column('note', String(), default='', nullable=False),
    Column('quantity', Integer, CheckConstraint('quantity>=0'), default=None, nullable=True),
    # help_text="Products in the bin"
    # quantity of goods to be fetched from this unit load - already deducted from quantity
    Column('open_pick_quantity', Integer, CheckConstraint('open_pick_quantity>=0'), default=0),
    # Bin does not exist anymore in our warehouses
    Column('disbanded', Boolean, default=False, nullable=False, index=True),
    # FIXME: ensure that disbanded bins have no associated storage slots
    Column('created_at', DateTime, PassiveDefault(func.current_timestamp()), nullable=False),
    Column('updated_at', DateTime, PassiveDefault(func.current_timestamp()), nullable=False),
            # onupdate=PassiveDefault(func.current_timestamp())),
    )

class UnitLoad(object):
    """Represents something we move in our Warehouse, e.g. a Pallett."""
    def __init__(self, mui=None, height=None, weight=None, productkey=None, content_description=None,
                 note=None, warehouse=None, storage_location=None, quantity=None):
        super(UnitLoad, self).__init__()
        self.mui = mui
        self.content_description = content_description
        self.height = height
        self.weight = weight
        self.productkey = productkey
        self.content_description = content_description
        self.note = note
        self.quantity = quantity
        if warehouse:
            self.warehouse = warehouse
        if storage_location:
            self.storage_location = storage_location
    
    def _get_is_moving(self):
        if len(self.open_movements) > 0:
            return True
        return False
    is_moving = property(_get_is_moving)
    
    def __unicode__(self):
        ret = [u"%r/%s/%r" % (self.mui, self.productkey, self.quantity)]
        return u' '.join(ret)
    
    def __repr__(self):
        ret = ["%r/%s/%r" % (self.mui, self.productkey, self.quantity)]
        if self.height:
            ret.append('%dmm' % (self.height,))
        if self.weight:
            ret.append('%dg' % (self.weight,))
        #if self.storage_location:
        #    ret.append("@%s" % (self.storage_location,))
        return ' '.join(ret)
    
    def _get_muistr(self):
        if not self.mui:
            # self.save()
            # TODO: how can we force to be sabed?
            pass
        return unicode(self.mui)
    muistr = property(_get_muistr)
    
