-record(location, {name, description, height, floorlevel, preference, allocated_by, reserved_for}).
-record(unit_load, {mui, quantity, product, height, pick_quantity}).
-record(movement, {from_location, to_location, mui}).

