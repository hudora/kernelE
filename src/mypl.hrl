% bester Lagerplatz, dient zur Platzierung von A-Artikeln
-define(BESTLOCATION, "061301").
% schlechtester Lagerplatz, dient zur Platzierung von C-Artikeln
-define(WORSTLOCATION, "194503").



% Record definitions

% dies stellt einen Lagerplatz im Regal dar.
-record(location, {name :: nonempty_string(),           % platznummer
                   height :: pos_integer(),             % platzhöhe in mm
                   floorlevel :: boolean(),                % kann der platz ohne Stapler bedient werden?
                   allocated_by :: [nonempty_string()], % liste der muis, die diesen platz belegen
                   reserved_for :: [nonempty_string()], % liste der muis, die auf dem weg zu diesem platz sind
                   preference :: 0..9,                  % plätze mit höherer preference werden bevorzugt befüllt, sollte zwischen 0-9 liegen
                   info :: string(),                    % anmerkungen
                   attributes                           % Liste von attributen: [
                                                        % no_picks - von diesm Platz darf nicht komissioniert werden (z.B. EINLAG)
                                                        %            keine picks und retrievals
                                                        % ]
                   }).


% dies stellt eine palette dar
-record(unit, {mui :: nonempty_string(),             % eindeutige Numer, z.B. NVE,
               quantity :: non_neg_integer(),        % Einheiten des Produkts
               product :: nonempty_string(),         % ArtNr
               height :: nonempty_string(),          % Höhe in mm
               pick_quantity :: non_neg_integer(),
               location,                             % Zeiger auf den Location name
               attributes,
               created_at
               }).


-record(movement, {id :: nonempty_string(),
                   mui :: nonempty_string(),
                   from_location :: nonempty_string(),
                   to_location :: nonempty_string(),
                   created_at,
                   attributes   % list of tuples to be used by the client application
                   }).


-record(pick, {id :: nonempty_string(),        % eindeutiger bezeichner
               product :: nonempty_string(),   % product das gepickt werden soll
               quantity :: pos_integer(),      % menge, die gepickt werden soll
               from_unit :: nonempty_string(), % unit von der gepickt werden soll
               created_at,
               attributes
               }).

% keeps IDs to detect dupes in store_at_location_multi
-record(multistorage, {id :: nonempty_string(),
                      muis :: [nonempty_string()],
                      attributes,
                      created_at}).
    

% keep information about changes in stock ("Korrekturbuchungen")
-record(correction, {id :: nonempty_string(),
                     old_quantity :: 0|pos_integer(),
                     product :: nonempty_string(),
                     mui :: nonempty_string(),
                     location :: nonempty_string(),
                     change_quantity :: pos_integer(),
                     text :: string(),
                     attributes,
                     created_at}).
    

% orders to be provisioned
-record(provpipeline, {id :: nonempty_string(),
                       priority :: 0..9,         % the higher the number the higher the priority
                       orderlines,
                       weigth :: 0|pos_integer(),
                       volume :: float(),
                       status :: atom(),         % new, processing, provisioned
                       tries :: 0|pos_integer(), % how often we tried to find a match for that pick
                       provisioninglists,        % retrievallists and picklists
                       attributes                % propertylist
                      }).
    

% nachfolger von provpipeline
-record(kommiauftrag, {id :: nonempty_string(),
                       priority :: 0..9,         % the higher the number the higher the priority
                       orderlines,
                       retrieved,
                       status :: atom(),         % new, processing, provisioned
                       tries :: 0|pos_integer(), % how often we tried to find a match for that pick
                       provisioninglists,        % retrievallists and picklists
                       attributes                % propertylist
                      }).
    

% list of the already given out provisioninglist entries
-record(provisioninglist, {id :: nonempty_string(),
                           type :: atom(),
                           provpipeline_id :: nonempty_string(),
                           destination :: nonempty_string(),
                           attributes,
                           parts :: pos_integer(),
                           provisionings,
                           status :: atom(),
                           created_at
                          }).
    

% nachfolger von provisioninglist
-record(kommischein, {id :: nonempty_string(),
                      type :: atom(),
                      provpipeline_id :: nonempty_string(),
                      destination :: nonempty_string(),
                      attributes,
                      provisionings,
                      status :: atom(),
                      created_at
                    }).
    

-record(pickpipeline,
            {id :: nonempty_string(),
             provpipelineid :: nonempty_string(),
             pickids,
             retrievalids
            }).

-record(retrievalpipeline, 
            {id :: nonempty_string(),
             provpipelineid :: nonempty_string(),
             retrievalids,
             pickids
            }).

-record(provpipeline_processing,
            {id :: nonempty_string(),
             provpipelineid :: nonempty_string(),
             retrievalids,
             pickids
            }).

% archiviert units, movements und picks
-record(archive, {id :: nonempty_string(),  % eindeutiger Bezeichner
                  created_at,
                  archived_by :: atom(),    % wodurch wurde der Datensatz archiviert
                  type :: atom(),
                  body_id :: nonempty_string(),
                  body
                  }).


% Speichert die Logoistischen Stammdaten
-record(eap,{
    artnr,
    updated_at,
    prod_x,
    prod_y,
    prod_z,
    prod_vol,
    prod_g,
    prod_ve1,
    ve1_x,
    ve1_y,
    ve1_z,
    ve1_vol,
    ve1_g,
    prod_export_package,
    export_x,
    export_y,
    export_z,
    export_vol,
    export_g,
    export_pallet
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


% speichert alle Unitbewegungen zu Protokollzwecken - auch als provpipeline bekannt
-record(kommiauftragaudit, {id :: nonempty_string(),      % eindeutiger Bezeichner
                            komminr :: nonempty_string(), % aus softm
                            auftrnr :: string(),          % aus softm
                            customer :: string(),         % aus softm
                            text :: string(),             % text describing the transaction
                            transaction :: string(),      % kommischeinid
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
