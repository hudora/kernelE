% Record definitions

% dies stellt einen Lagerplatz im Regal dar.
-record(location, {
    name,             % platznummer
    height,           % platzhöhe in mm
    floorlevel,       % kann der platz ohne Stapler bedient werden?
    allocated_by,     % liste der muis, die diesen platz belegen
    reserved_for,     % liste der muis, die auf dem weg zu diesem platz sind
    preference,       % plätze mit höherer preference werden bevorzugt befüllt, sollte zwischen 0-9 liegen
    info,             % anmerkungen
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
               location,        % Zeiger auf den Location name
               attributes,
               created_at
               }). % TBD


-record(movement, {id,
                   mui,
                   from_location,
                   to_location,
                   created_at,
                   attributes   % list of tuples to be used by the client application
                   }).


-record(pick, {id,              % eindeutiger bezeichner
               product,         % product das gepickt werden soll
               quantity,        % menge, die gepickt werden soll
               from_unit,       % unit von der gepickt werden soll
               created_at,
               attributes
               %% attributes - please add
               }).

% keeps IDs to detect dupes in store_at_location_multi
-record(multistorage, 
    {id,
    muis,
    attributes,
    created_at}).
    

% reservation of a location - usually for a movement
-record(reservation, {id,
                     mui,
                     location,
                     reason,
                     attributes
                }).

% keep information about changes in stock ("Korrekturbuchungen")
-record(correction, 
    {id,
    old_quantity,
    product,
    mui,
    location,
    change_quantity,
    text,
    attributes,
    created_at}).
    

% orders to be provisioned
-record(provpipeline,
            {id,
             priority,          % the higher the number the higher the priority
             orderlines,
             weigth,
             volume,
             status,            % new, processing, provisioned
             tries,             % how often we tried to find a match for that pick
             provisioninglists, % retrievallists and picklists
             attributes         % propertylist
            }).


% list of the already given out provisioninglist entries
-record(provisioninglist,
            {id,
             type,
             provpipeline_id,
             destination,
             attributes,
             parts,
             provisionings
             % TODO:
             % find a better way to store provisionings
             % add created_at
             % add status
            }).

-record(pickpipeline,
            {id,
             provpipelineid,
             pickids,
             retrievalids
            }).

-record(retrievalpipeline, 
            {id,
             provpipelineid,
             retrievalids,
             pickids
            }).

-record(provpipeline_processing,
            {id,
             provpipelineid,
             retrievalids,
             pickids
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

% this is from eunit
-ifdef(NOTEST).
%% The plain assert macro should be defined to do nothing if this file
%% is included when testing is turned off.
-ifndef(assert).
-define(assert(BoolExpr),ok).
-endif.
-else.
%% The assert macro is written the way it is so as not to cause warnings
%% for clauses that cannot match, even if the expression is a constant.
-undef(assert).
-define(assert(BoolExpr),
	((fun () ->
	    case (BoolExpr) of
		true -> ok;
		__V -> erlang:error({assertion_failed,
				     [{module, ?MODULE},
				      {line, ?LINE},
				      {expression, (??BoolExpr)},
				      {expected, true},
				      {value, case __V of false -> __V;
						  _ -> {not_a_boolean,__V}
					      end}]})
	    end
	  end)())).
-endif.

% from eunit.hrl
%% This is mostly a convenience which gives more detailed reports.
%% Note: Guard is a guarded pattern, and can not be used for value.
-ifdef(NOTEST).
-define(assertMatch(Guard,Expr),ok).
-else.
-define(assertMatch(Guard, Expr),
	((fun () ->
	    case (Expr) of
		Guard -> ok;
		__V -> erlang:error({assertMatch_failed,
				     [{module, ?MODULE},
				      {line, ?LINE},
				      {expression, (??Expr)},
				      {expected, (??Guard)},
				      {value, __V}]})
	    end
	  end)())).
-endif.
-define(_assertMatch(Guard, Expr), ?_test(?assertMatch(Guard, Expr))).
