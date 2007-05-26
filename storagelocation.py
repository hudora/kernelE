#!/usr/bin/env python
# encoding: utf-8
"""
storagelocation.py

Created by Maximillian Dornseif on 2007-01-21.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

import datetime

from sqlalchemy import Column, Table, Integer, String, DateTime, Boolean, CheckConstraint
from sqlalchemy import PassiveDefault, ForeignKey, UniqueConstraint
from sqlalchemy import func

from kernel.metadataprovider import metadata

storage_location_table = Table('storage_locations', metadata, 
    Column('id', Integer, primary_key=True),
    Column('name', String(20)),
    Column('warehouse_id', Integer, ForeignKey('warehouses.id'), nullable=False),
    Column('description', String(80), default='', nullable=False),
    # help_text='Maximum height for bins in Slot in mm
    Column('height', Integer, default=2050, index=True), # CheckConstraint('height>0')
    # help_text='StorageSlot can hold more than one Bin')
    #Column('multi_bin', Boolean, default=False, index=True, nullable=False),
    # help_text='StorageLocation can be accessed without forklift.
    Column('is_floor_level', Boolean, default=False, index=True, nullable=False),
    # help_text='StorageSlot can be automaticalle allocated')
    Column('automatic_allocation', Boolean, default=True, index=True, nullable=False),
    Column('flush', Boolean, default=False, nullable=False), # clear as soon as possible
    # help_text='StorageSlots with a higher preference have a higher chance of getting allocated.
    # Should be between 0-9999.')
    Column('preference', Integer, CheckConstraint('preference>=0 AND preference< 10'), default=5, nullable=False), 
    # help_text='StorageSlot should be filled with this product if possible.')
    Column('preferred_productkey', String(30), default='', nullable=False),
    Column('last_movement', DateTime, PassiveDefault(func.current_timestamp()), nullable=False),
    Column('created_at', DateTime, PassiveDefault(func.current_timestamp()), nullable=False),
    Column('updated_at', DateTime, PassiveDefault(func.current_timestamp()), nullable=False,
            ),# onupdate=PassiveDefault(func.current_timestamp())),
              # there sems to be issues with SQL alchemy here
    UniqueConstraint('name', 'warehouse_id', name='unique_name_per_warehouse')
    )

class StorageLocation(object):
    """Defines a place where we can store UnitLoads. E.g. a spot on a shelf."""
    def __init__(self, name=None, description=None, height=None, is_floor_level=None,
                 automatic_allocation=None, preference=None, preferred_productkey=None,
                 warehouse=None):
        super(StorageLocation, self).__init__()
        self.name = name
        self.description = description
        self.height = height
        self.is_floor_level = is_floor_level
        self.automatic_allocation = automatic_allocation
        self.preference = preference
        self.preferred_productkey = preferred_productkey
        self.warehouse = warehouse
        self.last_movement = None
        # TODO: implement multi_bin
        self.multi_bin = False
        
    def __repr__(self):
        ret = ["%r/%s" % (self.id, self.name)]
        if self.is_floor_level:
            ret.append('floor')
        if self.automatic_allocation:
            ret.append('autoalloc')
        if self.preferred_productkey:
            ret.append('preferred=%s' % self.preferred_productkey)
        return ' '.join(ret)
    
    def _get_is_empty(self):
        """Check if a StorageLocation is empty."""
        return len(self.unit_loads) == 0
    is_empty = property(_get_is_empty)

    def _get_is_free(self):
        """Check if a StorageLocation is empty and no movements to here are under4way."""
        return len(self.unit_loads) == 0 and len(self.open_movements) == 0
    is_free = property(_get_is_free)
    
    def _get_unit_count(self):
        """Return the number of UnitLoads on theis StoragsSlot (typically 0 or 1)."""
        return len(self.unit_loads)
    unit_count = property(_get_unit_count)
    
    def store(self, unit_load, disable_checks=False):
        """Store a UnitLoad on this StorageSlot."""
        if not disable_checks:
            if unit_load.disbanded:
                raise RuntimeError, "tried to store disbanded unit_load %r on %r" % (unit_load, self)
            if unit_load.storage_location_id or unit_load.storage_location:
                raise RuntimeError, "tried to store unit_load %r on two locations (%r/%r)" % (unit_load,
                                    unit_load.storage_location, self)
            if unit_load.height > self.height:
                raise RuntimeError, "tried to store unit_load (%d mm) in slot beeing not high enough (%d mm)" % (
                                    unit_load.height, self.height)
            if not self.is_empty:
                raise RuntimeError, "tried to store more than one unit_load on %r" % self
        unit_load.warehouse_id = self.warehouse_id
        self.unit_loads.append(unit_load)
        self.last_movement = datetime.datetime.now()
        #self.log("%s stored" % unit_load, unit_load=unit_load)
        #unit_load.log("stored on %s" % self, storage_slot=self)
    
    def find(self, mui=None, productkey=None):
        if mui == None and productkey == None and self.unit_count == 1:
            return self.unit_loads[0]
        elif mui and productkey:
            raise RuntimeError, "tried to find by mui AND productkey at once from %r" % self
        elif mui:
            for unit in self.unit_loads:
                if unit.mui == mui:
                    return unit
        elif productkey:
            # FIXME: observe FIFO
            for unit in self.unit_loads:
                if unit.productkey == productkey:
                    return unit
            return None
        else:
            raise RuntimeError, ("tried to find without specifying mui or productkey from" + 
                                 " %r which contains more than one bin") % self
    
    def retrive(self, mui=None, productkey=None):
        ret = self.find(mui=mui, productkey=productkey)
        if not ret:
            raise RuntimeError, "tried to retrive mui=%r, productkey=%r from %s but nothing found" % (mui, 
                                productkey, self)
        ret.storage_location = None
        ret.storage_location_id = None
        # self.log("%r retrived" % ret, bin=ret)
        return ret

