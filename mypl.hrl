% dies stellt einen Lagerplatz im Regal dar.
-record(location, {name,             % platznummer
                   height,           % platzhöhe in mm
                   floorlevel,       % kann der platz ohne Stapler bedient werden?
                   allocated_by,     % liste der muis, die diesen platz belegen
                   reserved_for,     % liste der muis, die auf dem weg zu diesem platz sind
                   preference,       % plätze mit höherer preference werden bevorzugt befüllt, sollte zwischen 0-9 liegen
                   description}).    % anmerkungen

% dies stellt eine palette dar
-record(unit, {mui,             % eindeutige Numer, z.B. NVE,
               quantity,        % einkeiten des produkts
               product,         % ArtNr
               height,          % Höhe in mm
               pick_quantity,
               created_at
               }). % TBD

-record(movement, {id, mui, from_location, to_location}).

-record(pick, {id,              % eindeutiger bezeichner
               picklist,        % dient der zusammenfassung von picks
               product,         % product das gepickt werden soll
               quantity,        % menge, die gepickt werden soll
               from_location    % ort von dem gepickt werden soll
               }). 

