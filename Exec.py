#!/usr/bin/env python
# encoding: utf-8
"""
exec.py

Created by Maximillian Dornseif on 2007-01-21.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

import unittest, datetime, logging

from sqlalchemy import create_session
from sqlalchemy.ext.selectresults import SelectResults

from kernel.metadataprovider import metadata, context
from kernel.storagelocation import StorageLocation, storage_location_table
from kernel.warehouse import Warehouse, warehouse_table
from kernel.unitload import UnitLoad, unit_load_table
from kernel.movement import Movement
from kernel.pick import Pick, PickList

from kernel.erp import get_palettenfaktor

logging.basicConfig(level=logging.INFO)
log = logging.getLogger('MyPL.kernel.Exec')

class WarehouseExecBase(object):
    """Collection of code to work with a Warehouse, its StorageLocations and UnitLoads."""
    def __init__(self, warehouse=None, warehouse_id=None):
        super(WarehouseExecBase, self).__init__()
        self.session = context.current
        if warehouse and warehouse_id:
            raise RuntimeError, 'Incorrect Parameters'
        if warehouse:
            self.warehouse = warehouse
        elif warehouse_id:
            self.warehouse = self.session.query(Warehouse).get(warehouse_id)
        else:
            self.warehouse = self.session.query(Warehouse).get(1)
        self.session.flush()
    
    def expire(self, *args):
        """Flushes session to the database and ensures that objects are up to date."""
        # if this is needed it indicates that there is something wrong with the programm design
        self.session.flush()
        for arg in args:
            self.session.expire(arg)
    
    def ensure_location_belongs_to_me(self, *args):
        """Takes one or more StorageLocations and checks if they belong to 'our' warehouse.
        
        If they belong to no warehouse they will be added to our warehouse."""
        for loc in args:
            if not loc.warehouse_id or (loc.warehouse_id != self.warehouse.id
                                        and loc.warehouse != self.warehouse):
                raise ValueError, "%r: tried to access location belonging to wrong warehouse (%r)" % (
                                    self, loc)
    
    def ensure_unit_belongs_to_me(self, *args):
        """Takes one or more UnitLoads as parameters and checks if they belong to 'our' warehouse.
        
        If they belong to no warehouse they will be added to our warehouse."""
        for unit in args:
            if not unit.warehouse_id or (unit.warehouse_id != self.warehouse.id
                                         and unit.warehouse != self.warehouse):
                raise ValueError, "%r: tried to access UnitLoad from wrong warehouse (%r|%r)" % (self,
                                 self.warehouse.id, unit.warehouse_id)
    
    def create_unit(self, flush=True, **kwargs):
        """Adds a new UnitLoad to our warehouse.
        
        Parameters are the same as in UnitLoad.__init__()."""
        
        if 'warehouse_id' in kwargs or 'warehouse' in kwargs:
            raise ValueError, "can't be called with an warehouse"
        kwargs['warehouse'] = self.warehouse
        unit = UnitLoad(**kwargs)
        if flush:
            self.expire(self.warehouse)
        return unit
    
    def create_location(self, flush=True, **kwargs):
        """Adds a new StorageLocation to our warehouse.
        
        Parameters are the same as in StorageLocation.__init__()."""
        
        if 'warehouse_id' in kwargs or 'warehouse' in kwargs:
            raise ValueError, "can't be called with an warehouse"
        kwargs['warehouse'] = self.warehouse
        loc = StorageLocation(**kwargs)
        if flush:
            self.expire(self.warehouse)
        return loc
    
    def get_storage_location(self, loc_id):
        query = self.session.query(StorageLocation)
        res = SelectResults(query)
        res = res.filter(storage_location_table.c.warehouse_id==self.warehouse.id).filter(
                         storage_location_table.c.id==loc_id)
        if len(list(res)) > 1:
            log.warn("Storage Location is not Unique! %r" % (list(res),))
        return list(res)[0]
    
    def get_storage_location_by_name(self, name):
        query = self.session.query(StorageLocation)
        res = SelectResults(query)
        res = res.filter(storage_location_table.c.warehouse_id==self.warehouse.id).filter(
                         storage_location_table.c.name==name)
        if len(list(res)) > 1:
            log.warn("Storage Location is not Unique! %r" % (list(res),))
        return list(res)[0]
    
    def store(self, storage_location, unit_load, flush=True):
        """Store a UnitLoad on this StorageLocation."""
        # TODO: add unittests - this is possibly broken
        if unit_load.disbanded:
            raise RuntimeError, "tried to store disbanded unit_load %r on %r" % (unit_load, storage_location)
        if unit_load.storage_location_id or unit_load.storage_location:
            raise RuntimeError, "tried to store unit_load %r on two locations (%r/%r)" % (unit_load,
                                unit_load.storage_location, storage_location)
        if unit_load.height > storage_location.height:
            raise RuntimeError, "tried to store unit_load (%d mm) in slot beeing not high enough (%d mm)" % (
                                unit_load.height, storage_location.height)
        if not storage_location.is_empty:
            raise RuntimeError, "tried to store more than one unit_load on %r" % storage_location
        unit_load.warehouse = storage_location.warehouse
        storage_location.unit_loads.append(unit_load)
        storage_location.last_movement = datetime.datetime.now()
        if flush:
            self.expire(self.warehouse, storage_location, unit_load)
        #self.log("%s stored" % unit_load, unit_load=unit_load)
        #unit_load.log("stored on %s" % self, storage_slot=self)
        
    def find_empty_slot(self, unit_load=None):
        # metadata.engine.echo = True
        row_id = self.warehouse.find_empty_slot_id(unit_load)
        # metadata.engine.echo = False
        if row_id:
            return self.session.query(StorageLocation).get(row_id) # get the Object instead of a raw id
        return None
        
    def insert_unit_load(self, unit_load, to_loc=None, flush=True):
        """Store a bin in an empty storage_location in the warehouse.
        
        If to_loc is not provided, find_empty_slot() is used to find an empty slot."""
        
        if not to_loc:
            to_loc = self.find_empty_slot(unit_load)
            if not to_loc:
                raise RuntimeError, "%r: no empty location in warehouse (better go sell something)" % self
        # we first compare ids instead of objects for preformance reasons
        self.ensure_location_belongs_to_me(to_loc)
        self.ensure_unit_belongs_to_me(unit_load)
        to_loc.store(unit_load)
        # self.log("successfully stored %s in %s" % (bin, to_slot), bin, to_slot)
        if flush:
            self.expire(self.warehouse, to_loc, unit_load)
        return to_loc
    
    def product_unit_loads(self, productkey):
        """Gets a list of all unit loads for a certain product."""
        query = self.session.query(UnitLoad)
        res = SelectResults(query)
        res = res.filter(unit_load_table.c.warehouse_id==self.warehouse.id).filter(
                         unit_load_table.c.productkey==productkey)
        res = res.order_by('created_at')
        return list(res)
    
    def product_count(self, productkey):
        query = self.session.query(UnitLoad)
        res = SelectResults(query)
        # TODO: ignore disbanded bins
        res = res.filter(unit_load_table.c.warehouse_id==self.warehouse.id).filter(
                         unit_load_table.c.productkey==productkey)
        ret = res.sum(unit_load_table.c.quantity)
        if not ret:
            return 0
        return ret

    def open_pick_count(self, productkey):
        query = self.session.query(UnitLoad)
        # TODO: ignore disbanded bins
        res = SelectResults(query)
        res = res.filter(unit_load_table.c.warehouse_id==self.warehouse.id).filter(
                         unit_load_table.c.productkey==productkey)
        ret = res.sum(unit_load_table.c.open_pick_quantity)
        if not ret:
            return 0
        return ret
    
    def inventory(self):
        """Get an inventory of the warehouse.
        
        Returns {'productkey1': 'quantity1', 'productkey2': 'quantity2'}."""
        return self.warehouse.get_inventory()
    
    def _find_fitting(self, items, quantity):
        # helper for find_product - IIRC this came from the python coockbook
        def _fittings(items, quantity):
            # generates permutations
            for i in xrange(len(items)):
                if items[i] == quantity:
                    yield [items[i]]
                if items[i] <= quantity:
                    for cc in _fittings(items[:i]+items[i+1:], quantity - items[i]):
                        yield [items[i]]+cc
        
        items.sort()
        items.reverse()
        if sum(items) < quantity: return []
        if sum(items) == quantity: return items
        for l in _fittings(items, quantity):
            if sum(l) == quantity:
                return l
        return []
    
    def find_product(self, productkey, quantity):
        """Find unit_loads of a product amountinging to quantity products."""
        bins = self.product_unit_loads(productkey)
        bindict = {}    # dict of available bin sizes and te actual unit loads
        binnumlist = [] # flattened version of bindict
        for bin in bins:
            if bin.quantity > quantity or not bin.quantity:
                # we dont't include that unit_load here, since it doesn't help (beeing to big or empty)
                continue
            if bin.open_pick_quantity:
                # we dont't touch unit|_loads with open picks
                continue
            if bin.quantity not in bindict:
                bindict[bin.quantity] = []
            bindict[bin.quantity].append(bin)
            binnumlist.append(bin.quantity)
        if quantity in bindict:
            # we have a direct fit, return the first fitting bin (which should be also the oldest one)
            return [bindict[quantity][0]]
        # now we have a knapsack like problem. urgs.
        # this is the right place to implement different policies - so far we don't
        ret = []
        for x in self._find_fitting(binnumlist, quantity):
            ret.append(bindict[x].pop(0))
        return ret


class MovementMixin(object):
    """Functins for moving goods inside the warehouse."""
    
    def teleport_unit(self, from_loc, to_loc, mui=None, productkey=None, flush=True):
        """Retrive a UnitLoad from a StorageLocation and store it on another.
        
        MUI and productkey can be used to distinguish UnitLoads on StorageLocations which can store
        several units. Note that this function acts in a virtual world where moving goods takes no time.
        In a real warehouse a UnitLoad is always "in flight" for some time (typically minutes, but sometimes
        hours or even days). Use initiate_movement() and commit_movement() for real world applications."""
        
        self.ensure_location_belongs_to_me(from_loc)
        self.ensure_location_belongs_to_me(to_loc)
        trans = self.session.create_transaction()
        unit = None
        try:
            self.expire(from_loc, to_loc)
            unit = from_loc.retrive(mui=mui, productkey=productkey)
            if unit.open_pick_quantity > 0:
                raise RuntimeError, "Tried to move UnitLoad with open Picks."
            self.expire(from_loc, to_loc)
            to_loc.store(unit)
            # self.log("successfully moved %r from %r to %r" % (bin, from_slot, to_slot),
            # bin, to_slot, from_slot)
        except Exception, msg:
            trans.rollback()
            # ensure the rollback is reflected in python objects
            # don't use self.refresh() here - the extra flush() and save() messes things up
            if unit:
                self.session.refresh(unit)
            raise
        trans.commit()
        return from_loc, to_loc, unit
    
    def merge_units(self, from_loc, to_loc, mui=None, productkey=None):
        """Retrive a UnitLoad from a StorageLocation and merge it with an UnitLoad stored it on another.
        
        MUI and productkey can be used to distinguish UnitLoads on StorageLocations which can store
        several units. Note that this function acts in a virtual world where moving goods takes no time."""
        self.ensure_location_belongs_to_me(from_loc)
        self.ensure_location_belongs_to_me(to_loc)
        trans = self.session.create_transaction()
        src_unit = None
        dst_unit = None
        try:
            src_unit = from_loc.retrive(mui=mui, productkey=productkey)
            if src_unit.open_pick_quantity > 0:
                raise RuntimeError, "Tried to move UnitLoad with open Picks."
            dst_unit = to_loc.find(productkey=src_unit.productkey)
            dst_unit.quantity = dst_unit.quantity + src_unit.quantity
            src_unit.disbanded = True
            src_unit.merged_into = dst_unit
            # self.log("successfully moved %r from %r to %r" % (bin, from_slot, to_slot),
            # bin, to_slot, from_slot)
        except:
            trans.rollback()
            # ensure the rollback is reflected in python objects
            # don't use self.refresh() here - the extra flush() and save() messes things up
            raise
        trans.commit()
    
    def movement_initiate(self, from_loc, to_loc, mui=None, productkey=None, maxmovements=500):
        # TODO
        #if Movement.objects.filter(finished_at__isnull=True).count() > maxmovements:
        #    raise RuntimeError,  "tried to initiate more than %i concurrent movements - that's to much" % (
        #    maxmovements)
        self.ensure_location_belongs_to_me(from_loc)
        self.ensure_location_belongs_to_me(to_loc)
        if from_loc == to_loc:
            raise RuntimeError, "%r: initiated a movement with identical source and destinations (%r)" % (
                                self, from_loc)
        # ensure no other movements are on their way
        if not from_loc.multi_bin:
            if from_loc.open_movements:
                raise RuntimeError, ("%r: tried to do movement from a location already engaged in"
                          + " another movement: (to=%s|%r)" % (self, from_loc, from_loc.open_movements))
        if not to_loc.multi_bin:
            if to_loc.open_movements:
                raise RuntimeError, ("%r: tried to do movement to a location already engaged in"
                          + " another movement: (to=%s|%r)" % (self, to_loc, to_loc.open_movements))
        # ensure from_slot is not empty and to_slot is empty
        if from_loc.is_empty:
            raise RuntimeError, "%r: tried to do movement from an empty location: (%r)" % (self, from_loc)
        if (not to_loc.multi_bin) and (not to_loc.is_empty):
            raise RuntimeError, "%r: tried to do movement to and NON-empty location: (%r)" % (self, to_loc)
        # ensure that bin is not active in any other movement
        unit = from_loc.find(mui=None, productkey=None)
        if unit.open_movements:
            raise RuntimeError, ("%r: tried to move UnitLoad %r which is already active in other"
                      + " movements %r" % (self, bin, unit.open_movements))
        if unit.open_pick_quantity > 0:
            raise RuntimeError, "Tried to move UnitLoad with open Picks."
        
        # HEYHO - we can actually initiate the movement
        movement = Movement(unit_load=unit, from_location=from_loc, to_location=to_loc)
        from_loc.last_movement = to_loc.last_movement = datetime.datetime.now()
        #self.log("successfully initiated movement %d of %r from %r to %r" % (movement.id,
        #         bin, from_slot, to_slot), bin, to_slot, from_slot)
        self.session.flush()
        self.expire(from_loc, to_loc, unit)
        return movement
    
    def movement_commit(self, movement):
        self.ensure_location_belongs_to_me(movement.from_location)
        self.ensure_location_belongs_to_me(movement.to_location)
        if movement.done:
            raise RuntimeError, "tried to commit an already committed movement (%r)" % (movement,)
        if movement.unit_load.open_pick_quantity > 0:
            raise RuntimeError, "Tried to move UnitLoad with open Picks."
        trans = self.session.create_transaction()
        try:
            self.teleport_unit(movement.from_location, movement.to_location, mui=movement.unit_load.mui)
            movement.done = True
        except:
            trans.rollback()
            # ensure the rollback is reflected in python objects
            # don't use self.refresh() here - the extra flush() and save() messes things up
            raise
        trans.commit()
        #self.log("successfully commited movement %d of %r from %r to %r" % (movement.id,
        #     movement.bin, movement.from_slot, movement.to_slot), movement.bin, movement.from_slot,
        #     movement.to_slot)
        self.session.flush()
        return movement

class OptimisationMixin(object):
    """Functions to optimize the placement of goods in the warehouse."""
    
    def propose_compacting_moves(self):
        """Propose moves with optimize space usage in the warehouse."""
        easymoves = []
        hardmoves = []
        # ensure everything is nice and clean
        self.session.flush()
        # TODO This is possibly broken
        self.session.refresh(self.warehouse)
        for l in self.warehouse.storage_locations:
            self.session.refresh(l)
        for artnr in sorted(self.warehouse.product_list):
            locs = self.warehouse.get_product_location(artnr)
            palettenfaktor = get_palettenfaktor(artnr)
            if not palettenfaktor:
                palettenfaktor = max([x[1] for x in locs]) # assume the biggest UnitLoad as palettenfaktor
            if len(locs) > 1:
                bestmove = None
                bestmove_overhead = 0xffffffffffff
                available_quantities = set([x[1] for x in locs])
                if len(available_quantities) == 1:
                    continue # nothing to reorder
                for small in sorted(available_quantities):
                    if small > palettenfaktor or small < 1:
                        break # we can't get better
                    for large_partner in sorted(available_quantities, reverse=True):
                        if large_partner <= small:
                            continue
                        if small + large_partner <= palettenfaktor:
                            overhead = palettenfaktor - (small + large_partner)
                            if overhead < bestmove_overhead:
                                bestmove_overhead = overhead
                                bestmove = (small, large_partner)
                if bestmove:
                    # we now have a suggestion to optimize: unite unit_loads from an bestmove[0] quantity
                    # with one of an bestmove[1] quantity - the resilt should be placed at floor level to make
                    # more easy
                    a_candidates = []
                    b_candidates = []
                    for loc_id, quantity in locs:
                        if quantity == bestmove[0]:
                            a_candidates.append(self.get_storage_location(loc_id))
                        if quantity == bestmove[1]:
                            b_candidates.append(self.get_storage_location(loc_id))
                    a_candidates_floor = [x for x in a_candidates if x.is_floor_level]
                    b_candidates_floor = [x for x in b_candidates if x.is_floor_level]
                    if a_candidates_floor and not b_candidates_floor:
                        # we move from B -> A
                        if b_candidates_floor:
                            # prefer to also move FROM floor level
                            source_candidates = b_candidates_floor
                        else:
                            source_candidates = b_candidates
                        destination_candidates = a_candidates_floor
                    else:
                        # we move from A -> B
                        if a_candidates_floor:
                            # prefer to also move FROM floor level
                            source_candidates = a_candidates_floor
                        else:
                            source_candidates = a_candidates
                        if b_candidates_floor:
                            destination_candidates = b_candidates_floor
                        else:
                            destination_candidates = b_candidates
                    
                    source = sorted(source_candidates, cmp=lambda x, y: cmp(x.name, y.name), reverse=True)[0]
                    destination = sorted(destination_candidates, cmp=lambda x, y: cmp(x.name, y.name))[0]
                    # TODO: avoid removing goods from fixplatz
                    # TODO: don't move UnitLoads with open Picks
                    if destination.is_floor_level:
                        easymoves.append((source, destination))
                    else:
                        hardmoves.append((source, destination))
        if easymoves:
            return easymoves
        return hardmoves
        
class PickingMixin(object):
    def choose_unitload_to_break_up(self, productkey, quantity, ignore=[]):
        """Select a unit load to take goods from."""
        query = self.session.query(UnitLoad)
        res = SelectResults(query)
        res = res.filter(unit_load_table.c.warehouse_id==self.warehouse.id).filter(
                         unit_load_table.c.productkey==productkey).filter(
                         unit_load_table.c.quantity>=quantity)
        res = res.order_by('created_at')
        candidates = list(res)
        preferred = []
        for candidate in candidates:
            # we prefer slots with preferred_productkey
            if (candidate.storage_location.preferred_productkey == productkey
                and candidate not in ignore):
                preferred.append(candidate)
        if not preferred: # nothing found so far
            for candidate in candidates:
                # fall back on UnitLoads placed on the ground
                if (candidate.storage_location.is_floor_level == True
                    and candidate not in ignore):
                    preferred.append(candidate)
        if not preferred:
            # that's not good - we have nothing to pick from
            log.info('nothing to pick from: %d x %s' % (qunatity, productkey))
            return None
        
        # TODO: we could further optimize choosing.
        return preferred[0]
    
    def generate_picklist(self, product_dict):
        """Generate a picklist if possible. Else return None.
        
        product_dict should be something like {'RatzFratz': 50, 'RatzFratz': 12}."""
        
        # first: check if we have sufficient quantities of this products in stock.
        for productkey, quantity in product_dict.items():
            if self.product_count(productkey) < quantity:
                return None # we can't fullfill this request for a picklist
        
        # second: try to find exact fitting unit loads
        unitloads = []
        anbruch = []
        anbruchpicks = []
        for productkey, quantity in product_dict.items():
            prodloads = self.find_product(productkey, quantity)
            # verify we got what we asked for
            prodsum = 0
            for unit in prodloads:
                prodsum += unit.quantity
                if unit.productkey != productkey:
                    raise RuntimeError, 'find_product() returned strange results - wrong products'
            if prodsum == 0:
                # we have to handle this product by breaking up an unit load, save it for later
                anbruch.append((productkey, quantity))
            elif prodsum != quantity:
                raise RuntimeError, 'find_product() returned strange results - wrong quantity %r' % (
                                    (productkey, quantity, prodsum, prodloads),)
            unitloads.extend(prodloads)
        
        # third: try to gather the goods by breaking up unit loads
        for productkey, quantity in anbruch:
            # try to get as many complete UnitLoads as possible
            candidates = self.product_unit_loads(productkey)
            # prefer storage slots which are meant to be flushed
            for unitload in candidates:
                if (unitload.quantity and unitload.quantity < quantity and unitload.storage_location.flush):
                    unitloads.append(unitload)
                    quantity -= unitload.quantity
                    candidates.remove(unitload)
            # prefer misplaced unit_loads
            for unitload in candidates:
                if (unitload.quantity and unitload.quantity < quantity
                        and unitload.storage_location.preferred_productkey
                        and unitload.storage_location.preferred_productkey != productkey):
                    unitloads.append(unitload)
                    quantity -= unitload.quantity
                    candidates.remove(unitload)
            # prefer unit_loads not on prefered storage_locations
            for unitload in candidates:
                if (unitload.quantity and unitload.quantity < quantity
                        and unitload.storage_location.preferred_productkey != productkey):
                    unitloads.append(unitload)
                    quantity -= unitload.quantity
                    candidates.remove(unitload)
            # take whatever we can get
            for unitload in candidates:
                if (unitload.quantity and unitload.quantity < quantity):
                    unitloads.append(unitload)
                    quantity -= unitload.quantity
                    candidates.remove(unitload)
            
            # now we have to break one up
            unit = self.choose_unitload_to_break_up(productkey, quantity, ignore=unitloads)
            if not unit:
                # unable to find something to pick from
                return None
            anbruchpicks.append((unit, quantity))
            
        # forth: we have exact fitting, generate picklist
        trans = self.session.create_transaction()
        try:
            picklist = PickList(warehouse=self.warehouse)
            for unitload in unitloads:
                pick = Pick(picklist, unitload, unitload.quantity)
                picklist.picks.append(pick) # this is redundant but helps SA to stay unconfused
                unitload.open_pick_quantity = unitload.quantity
                unitload.quantity = 0
            for unitload, quantity in anbruchpicks:
                pick = Pick(picklist, unitload, quantity)
                pick.anbruch = True
                picklist.picks.append(pick) # this is redundant but helps SA to stay unconfused
                unitload.open_pick_quantity = quantity
                unitload.quantity = unitload.quantity - quantity
        except Exception, msg:
            trans.rollback()
            raise
        trans.commit()
        return picklist
    

class WarehouseExec(WarehouseExecBase, MovementMixin, OptimisationMixin, PickingMixin):
    pass

#max_bins            = models.SmallIntegerField(default=1, help_text='StorageSlot can hold more than one Bin')
#depth                = models.PositiveIntegerField(default=1200, db_index=True, 
#                       help_text='Maximum depth for bins in Slot in mm')
# specific to fixplatz
#is_fixplatz          = models.BooleanField(default=False, db_index=True, blank=True, null=True, 
#                       help_text='StorageSlot should be used only for a vertain Product defined in
#                      "preferred_productkey".')
#maximum_products     = models.PositiveSmallIntegerField(blank=True, null=True, 
#                       help_text='Maximum number of Products which can be (automaticallly) stored 
#                       on this Fixplatz.')
#available_products   = models.PositiveSmallIntegerField(blank=True, null=True, default=None,
#                       help_text='Available number of Products on this Fixplatz.')

