#!/usr/bin/env python
# encoding: utf-8
"""
movement.py

Created by Maximillian Dornseif on 2007-01-21.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

from sqlalchemy import Column, Table, Integer, String, DateTime, Boolean
from sqlalchemy import PassiveDefault, ForeignKey
from sqlalchemy import  func
from sqlalchemy.ext.selectresults import SelectResults

from kernel.metadataprovider import metadata

movement_table = Table('movements', metadata, 
    Column('id', Integer, primary_key=True),
    Column('from_location_id', Integer, ForeignKey("storage_locations.id"), nullable=False),
    Column('to_location_id', Integer, ForeignKey("storage_locations.id"), nullable=False),
    Column('unit_load_id', Integer, ForeignKey("unit_loads.mui"), nullable=False),
    Column('done', Boolean, default=False, nullable=False, index=True),
    Column('created_at', DateTime, PassiveDefault(func.current_timestamp()), nullable=False),
    Column('updated_at', DateTime, PassiveDefault(func.current_timestamp()), nullable=False),
    )

class Movement(object):
    # FIXME: enforce that there is only a single movement to a not multi-bin fixplatz
    def __init__(self, description=None, from_location=None, 
                 to_location=None, unit_load=None):
        super(Movement, self).__init__()
        self.description = description
        self.from_location = from_location
        self.to_location = to_location
        self.unit_load = unit_load
    
    def __repr__(self):
        if self.done:
            return "%d,DONE %s:%s->%s" % (self.id, self.unit_load, self.from_location, self.to_location) 
        return "%d %s:%s->%s" % (self.id, self.unit_load, self.from_location, self.to_location) 
    