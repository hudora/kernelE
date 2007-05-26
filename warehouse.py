#!/usr/bin/env python
# encoding: utf-8
"""
warehouse.py

Created by Maximillian Dornseif on 2007-01-21.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

from sqlalchemy import Column, Table, Integer, String, DateTime
from sqlalchemy import PassiveDefault
from sqlalchemy import  func, outerjoin, and_, asc, desc, select

from kernel.metadataprovider import metadata
from kernel.unitload import unit_load_table
from kernel.storagelocation import storage_location_table

warehouse_table = Table('warehouses', metadata, 
    Column('id', Integer, primary_key = True),
    Column('description', String(80), default='', nullable=False),
    Column('created_at', DateTime, PassiveDefault(func.current_timestamp()), nullable=False),
    Column('updated_at', DateTime, PassiveDefault(func.current_timestamp()), nullable=False,
            ),#onupdate=PassiveDefault(func.current_timestamp())),
    )

class Warehouse(object):
    """Represents a warehouse which is a collection of UnitLoads and StorageLocations."""
    def info(self):
        """Nice string representation of a Warehouse."""
        ret = ["Warehouse %d, %d slots, %d empty" % (self.id, self.location_count, self.empty_count)]
        for row in self.storage_locations:
            ret.append(str(row))
        return '\n'.join(ret)
    
    def _get_location_count(self):
        return storage_location_table.count(storage_location_table.c.warehouse_id==self.id).execute(
                                            ).fetchone()[0]
    location_count = property(_get_location_count)
    
    def _get_empty_count(self):
        return outerjoin(storage_location_table, unit_load_table).count(
                         and_(storage_location_table.c.warehouse_id==self.id,
                         storage_location_table.c.automatic_allocation==True,
                         unit_load_table.c.mui==None)).execute().fetchone()[0]
    empty_count = property(_get_empty_count)
    
    def _get_product_list(self):
        """Retun a list of all productkeys"""
        # TODO: unittest
        ret = select([unit_load_table.c.productkey],
                     and_(unit_load_table.c.warehouse_id==self.id,
                     unit_load_table.c.disbanded==False),
                     group_by=[unit_load_table.c.productkey]).execute().fetchall()
        return [x[0] for x in list(ret)]
    product_list = property(_get_product_list)
    
    def get_inventory(self):
        """Return a dict of {productkey: sum of ammounts}"""
        # TODO: unittest
        ret = select([unit_load_table.c.productkey, func.sum(unit_load_table.c.quantity)],
                     and_(unit_load_table.c.warehouse_id==self.id,
                     unit_load_table.c.disbanded==False),
                     group_by=[unit_load_table.c.productkey]).execute().fetchall()
        return dict(list([(x, int(y)) for x, y in ret]))
    
    def _get_product_locationcount(self):
        """return the number of storage location occupied by an productkey"""
        # TODO: unittest
        ret = select([unit_load_table.c.productkey, func.count(unit_load_table.c.storage_location_id)],
                     and_(unit_load_table.c.warehouse_id==self.id,
                     unit_load_table.c.disbanded==False),
                     group_by=[unit_load_table.c.productkey]).execute().fetchall()
        return dict(list([(x, int(y)) for x, y in ret]))
    product_locationcount = property(_get_product_locationcount)
    
    def get_product_location(self, productkey):
        """Returns a list of ids of StorageLocation containing productkey."""
        # TODO: unittest
        ret = select([storage_location_table.c.id, unit_load_table.c.quantity],
                     and_(unit_load_table.c.warehouse_id==self.id,
                          unit_load_table.c.disbanded==False,
                          unit_load_table.c.productkey==productkey),
                     from_obj=[unit_load_table.join(storage_location_table, 
                                        storage_location_table.c.id==unit_load_table.c.storage_location_id)]
                     ).execute().fetchall()
        return ret
    
    def find_empty_slot_id(self, unit_load=None):
        """Returns an empty slot suited for bin or None if no slot is available."""
        if not unit_load:
            query = outerjoin(storage_location_table, unit_load_table).select(
                              and_(storage_location_table.c.warehouse_id==self.id,
                              storage_location_table.c.automatic_allocation==True,
                              unit_load_table.c.mui==None),
                              order_by=[desc(storage_location_table.c.preference),
                                    asc(storage_location_table.c.last_movement)], limit=10)
            slots = list(query.execute().fetchall())
        else:
            # try if we find a fitting productkey
            query = outerjoin(storage_location_table, unit_load_table).select(
                              and_(storage_location_table.c.warehouse_id==self.id,
                              storage_location_table.c.automatic_allocation==True,
                              storage_location_table.c.height>=unit_load.height,
                              storage_location_table.c.preferred_productkey==unit_load.productkey,
                              unit_load_table.c.mui==None),
                              order_by=[desc(storage_location_table.c.preference),
                                        asc(storage_location_table.c.last_movement),
                                        desc(storage_location_table.c.height)], limit=10)
            slots = list(query.execute().fetchall())
            if not slots:
                # no ... then settle for any storage_location high enough
                query = outerjoin(storage_location_table, unit_load_table).select(
                                  and_(storage_location_table.c.warehouse_id==self.id,
                                  storage_location_table.c.automatic_allocation==True,
                                  storage_location_table.c.height>=unit_load.height,
                                  unit_load_table.c.mui==None),
                                  order_by=[desc(storage_location_table.c.preference),
                                            asc(storage_location_table.c.last_movement),
                                            desc(storage_location_table.c.height)], limit=10)
                slots = list(query.execute().fetchall())
        if slots:
            return slots[0][0]
        return None # nothing found
    
