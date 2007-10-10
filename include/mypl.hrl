% Record definitions

% dies stellt einen Lagerplatz im Regal dar.
-record(location, {
    name,             % platznummer
    height,           % platzhöhe in mm
    floorlevel,       % kann der platz ohne Stapler bedient werden?
    allocated_by,     % liste der muis, die diesen platz belegen
    reserved_for,     % liste der muis, die auf dem weg zu diesem platz sind
    preference,       % plätze mit höherer preference werden bevorzugt befüllt, sollte zwischen 0-9 liegen
    description,      % anmerkungen
    attributes        % Liste von attributen: [
                      % no_picks - von diesm Platz darf nicht komissioniert werden (z.B. EINLAG)
                      %            keine picks und retrievals
                      % ]
                   }).    

% dies stellt eine palette dar
-record(unit, {mui,             % eindeutige Numer, z.B. NVE,
               quantity,        % einkeiten des produkts
               product,         % ArtNr
               height,          % Höhe in mm
               pick_quantity,
               created_at
               }). % TBD


-record(movement, {id,
                   mui,
                   from_location,
                   to_location,
                   references   % list of tuples to be used by the client application, not used by the myPL kernel
                   }).


-record(pick, {id,              % eindeutiger bezeichner
               product,         % product das gepickt werden soll
               quantity,        % menge, die gepickt werden soll
               from_unit,       % unit von der gepickt werden soll
               created_at
               }). 


-record(picklist, {id,         % eindeutiger bezeichner
                   picks,      % list of picks belonging to this picklist
                   movements,  % list of movement/retrieval ids belonging to this picklist
                   priority,   % how urgend is this pick to be executed
                   references  % list of tuples to be used by the client application, not used by the myPL kernel
                   }).


% speichert alle Warenbewegungen zu Protokollzwecken
-record(articleaudit, {id,           % eindeutiger Bezeichner
                       quantity,     % Einkeiten des produkts
                       product,      % ArtNr
                       text,         % text describing the transaction
                       mui,          % bebuchte Unit
                       transaction,  % movement or pick ID
                       references,   % list of tuples to be used by the client application, not used by the myPL kernel
                       created_at
                   }).


% speichert alle Unitbewegungen zu Protokollzwecken
-record(unitaudit, {id,           % eindeutiger Bezeichner
                    mui,          % bebuchte Unit
                    quantity,     % Einkeiten des produkts
                    product,      % ArtNr
                    text,         % text describing the transaction
                    transaction,  % movement or pick ID
                    references,   % list of tuples to be used by the client application, not used by the myPL kernel
                    created_at
                   }).


% Trace utilities from adviserl

-ifdef(LOG_DEBUG).

-define(DEBUG(Msg, Params),   mypl_util:log(?MODULE, ?LINE, dbg, Msg, Params)).
-define(INFO(Msg, Params),    mypl_util:log(?MODULE, ?LINE, dbg, Msg, Params)).
-define(WARNING(Msg, Params), mypl_util:log(?MODULE, ?LINE, dbg, Msg, Params)).
-define(ERROR(Msg, Params, Exception), mypl_util:log(?MODULE, ?LINE, dbg, Msg, Params), throw(Exception)).

-else.

-define(DEBUG(Msg, Params),   true).
-define(INFO(Msg, Params),    mypl_util:log(?MODULE, ?LINE, inf, Msg, Params)).
-define(WARNING(Msg, Params), mypl_util:log(?MODULE, ?LINE, wrn, Msg, Params)).
-define(ERROR(Msg, Params, Exception), mypl_util:log(?MODULE, ?LINE, err, Msg, Params), throw(Exception)).

-endif.