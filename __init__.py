#!/usr/bin/env python
# encoding: utf-8
"""
kernel.py - myPL basic functionality. Originally a port of Django based 'models.py' to SQLalchemy.

Created by Maximillian Dornseif on 2006-12-26.
Copyright (c) 2006 HUDORA GmbH. All rights reserved.
"""

from sqlalchemy import  func, mapper, relation, create_engine, create_session, outerjoin, and_, or_, asc, desc
from sqlalchemy import DynamicMetaData, PassiveDefault, ForeignKey, UniqueConstraint

from kernel.metadataprovider import metadata, context
from kernel.unitload import UnitLoad, unit_load_table
from kernel.storagelocation import StorageLocation, storage_location_table
from kernel.movement import Movement, movement_table
from kernel.warehouse import Warehouse, warehouse_table
from kernel.pick import Pick, pick_table, PickList, pick_list_table
from kernel.Exec import WarehouseExec

UnitLoad_mapper = mapper(UnitLoad, unit_load_table,
    properties = {
        'open_movements': relation(Movement, backref='moving_unitloads',
                          primaryjoin=and_(unit_load_table.c.mui==movement_table.c.unit_load_id, 
                          movement_table.c.done==False, viewonly=True)),
        'picks':           relation(Pick, backref='unit_load'),
    })

StorageLocation_mapper = mapper(StorageLocation, storage_location_table,
    properties = {
        'unit_loads':     relation(UnitLoad, backref='storage_location'),
        'open_movements': relation(Movement,
                           primaryjoin=and_(or_(storage_location_table.c.id==movement_table.c.from_location_id,
                                                storage_location_table.c.id==movement_table.c.to_location_id, viewonly=True),
                           movement_table.c.done==False)),
        'movements':       relation(Movement,
                           primaryjoin=and_(storage_location_table.c.id==movement_table.c.from_location_id,
                                            storage_location_table.c.id==movement_table.c.to_location_id)),
        'picks':           relation(Pick, backref='storage_location'),
    })

Warehouse_mapper = mapper(Warehouse, warehouse_table,
    properties = {
       'storage_locations': relation(StorageLocation, cascade='all, delete-orphan', 
                                                      backref='warehouse'),
       'unit_loads':        relation(UnitLoad, cascade='all, delete-orphan', 
                                                      backref='warehouse'),
       'pick_lists':        relation(PickList, cascade='all, delete-orphan', 
                                                      backref='warehouse')
    })

Movement_mapper = mapper(Movement, movement_table,
    properties = {
        'from_location': relation(StorageLocation, 
                         primaryjoin=movement_table.c.from_location_id==storage_location_table.c.id,
                         backref='movements_out'),
        'to_location':   relation(StorageLocation, 
                         primaryjoin=movement_table.c.to_location_id==storage_location_table.c.id,
                         backref='movements_out'),
        'unit_load':     relation(UnitLoad, backref='movements'),
    })

PickList_mapper = mapper(PickList, pick_list_table,
    properties = {
        'picks':         relation(Pick, backref='pick_list'),
    })

Pick_mapper = mapper(Pick, pick_table)


#def setup_db(connection_string='sqlite:///:memory:'):
def setup_db(connection_string='postgres://hudora@localhost:5432/hudora'):
    """Create database connection."""
    global metadata
    #db = create_engine('postgres://postgres81@localhost:5432/testdatabase')
    engine = create_engine(connection_string) #, echo=True)
    metadata.connect(engine)
    metadata.create_all()
    # create default warehouse if needed
    if warehouse_table.count(warehouse_table.c.id==1).execute().fetchone()[0] < 1:
        warehouse_table.insert().execute(description='HUDORA MPL Remscheid')

setup_db()
