%% @version 0.2
%% @copyright 2007 HUDORA GmbH
%% @author Maximillian Dornseif <md@hudora.de>
%% @doc myPL/kernel-E Volume Calcolation Engine
%%
%% This can calculate the volume of a shippment.
%%
%% @end

-module(mypl_volumes).

-export([run_me_once/0, feed_eap/16, partial/1, volume/1, weight/1, pallets/1, export_packages/1,
         partial_multi/1, volume_multi/1, weight_multi/1, pallets_multi/1, export_packages_multi/1,
         volume_proplist/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("include/mypl.hrl").

%% @doc should be run before mnesia is started for the first time.
run_me_once() ->
    mnesia:start(),
    % the main tables are kept in RAM with a disk copy for fallback
    mypl_db:init_table_info(mnesia:create_table(eap,    [{disc_copies, [node()]}, {attributes, record_info(fields, eap)}]), eap).
    

get_eap(ArtNr) ->
    case mnesia:dirty_read({eap, ArtNr}) of
        [] -> error_logger:warning_msg("No eAP for ~s~n", [ArtNr]), unknown_artnr;
        [Eap] -> Eap
    end.

feed_eap(ArtNr, Prod_ve1, Prod_export_package, Export_pallet, Prod_x, Prod_y, Prod_z, Prod_g,
     Ve1_x, Ve1_y, Ve1_z, Ve1_g, Export_x, Export_y, Export_z, Export_g) ->
    if Prod_x+Prod_y+Prod_z > 0 -> Prod_vol = Prod_x*Prod_y*Prod_z/1000/1000;
       true -> Prod_vol = 0 end,
    if Ve1_x+Ve1_y+Ve1_z > 0 -> Ve1_vol = Ve1_x*Ve1_y*Ve1_z/1000/1000;
       true -> Ve1_vol = 0 end,
    if Export_x+Export_y+Export_z > 0 -> Export_vol = Export_x*Export_y*Export_z/1000/1000;
       true -> Export_vol = 0 end,
    mnesia:dirty_write(#eap{artnr=ArtNr, updated_at=calendar:universal_time(), prod_x=Prod_x, prod_y=Prod_y,
    prod_z=Prod_z, prod_vol=Prod_vol, prod_g=Prod_g, prod_ve1=Prod_ve1, ve1_x=Ve1_x, ve1_y=Ve1_y, ve1_z=Ve1_z,
    ve1_vol=Ve1_vol, ve1_g=Ve1_g, prod_export_package=Prod_export_package,
    export_x=Export_x, export_y=Export_y, export_z=Export_z,
    export_vol=Export_vol, export_g=Export_g, export_pallet=Export_pallet}).
    

%% @doc Returns True if this Item does not result in an export_package to be opened.
partial({Quantity, ArtNr}) ->
    case get_eap(ArtNr) of
        unknown_artnr -> true;
        Eap -> (Quantity rem Eap#eap.prod_export_package) /= 0
    end.
    
%% @doc Retuns the volume of this item in liters
%% @TODO export_packagevolume
volume({Quantity, ArtNr}) ->
    case get_eap(ArtNr) of
        unknown_artnr -> 0;
        Eap -> Quantity * Eap#eap.prod_vol
    end.

%% @doc Retuns the weight of this item in g.
%% @TODO export_packageweight usw.
weight({Quantity, ArtNr}) ->
    case get_eap(ArtNr) of
        unknown_artnr -> 0;
        Eap -> Quantity * Eap#eap.prod_g
    end.

% def max_packstueck_weight(self):
%     """Returns the weigtht of the most heavy box for ths item."""
%     if self.menge >= self.produkte_pro_export_package:
%         return self.weight_pro_export_package
%     return (self.weight_pro_export_package / self.produkte_pro_export_package) * self.menge 

%% @doc Returns the number of pallets of this Item.
pallets({Quantity, ArtNr}) ->
    case get_eap(ArtNr) of
        unknown_artnr -> 0;
        Eap -> Quantity / (Eap#eap.prod_export_package * Eap#eap.export_pallet)
    end.

% def picks(self):
%     """Returns the number storage locations to be accessed to get the item."""
%     picks = self.pallets
%     # round up
%     if picks != int(picks):
%         picks = int(picks + 1)
%     return picks
% 

export_packages({Quantity, ArtNr}) ->
    case get_eap(ArtNr) of
        unknown_artnr -> 0;
        Eap -> Quantity / Eap#eap.prod_export_package
    end.

% def export_karton_weighte(self):
%     """Returns the weights of the estimated number of packages which will be shipped in gramms."""
%     menge = self.menge
%     ret = []
%     while menge:
%         if menge > self.produkte_pro_export_package:
%             ret.append(self.weight_pro_export_package)
%             menge -= self.produkte_pro_export_package
%         else:
%             ret.append(menge * self.einzelweight)
%             menge = 0
%     return ret
% 
% def packstuecke(self):
%     """Returns the absolute number of packages to fullfill this item as an integer.
%     
%     This could take into account that packages can be bundled etc.
%     """
%     
%     packstuecke = self.export_packages
%     # round up
%     if packstuecke != int(packstuecke):
%         packstuecke = int(packstuecke + 1)
%     return int(packstuecke)


none([Hd|Tail]) ->
    case Hd of
        true -> true;
        false -> none(Tail)
    end;
none([]) -> false.

partial_multi(L) ->
    none(lists:map(fun(X) -> partial(X) end, L)).
    

volume_multi(L) ->
    lists:sum(lists:map(fun(X) -> volume(X) end, L)).
    

weight_multi(L) ->
    lists:sum(lists:map(fun(X) -> weight(X) end, L)).
    

pallets_multi(L) ->
    lists:sum(lists:map(fun(X) -> pallets(X) end, L)).
    

export_packages_multi(L) ->
    lists:sum(lists:map(fun(X) -> export_packages(X) end, L)).

volume_proplist(L) ->
   [{volume,          volume_multi(L)},
    {partial,         partial_multi(L)},
    {weight,          weight_multi(L)},
    {pallets,         pallets_multi(L)},
    {export_packages, export_packages_multi(L)}].
    

% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).
    

init_tests() ->
feed_eap("14600", 0, 2, 35, 115, 730, 210, 3200, ve1_x, ve1_y, ve1_z, ve1_g, 225, 740, 215, 700),
    mnesia:dirty_write(#eap{artnr="77003",
        prod_x=460, prod_y=41, prod_z=450, prod_vol=460*41*450/1000/1000, prod_g=2330,
        export_x=480, export_y=280, export_z=480, export_vol=480*280*480/1000/1000,export_g=14780,
        prod_ve1=0,prod_export_package=6,export_pallet=18}),
    ok.
    

%%% @hidden
simple_test() ->
    run_me_once(),
    init_tests(),
    ?assertMatch(false  , partial({4, "14600"})),
    ?assertMatch(true   , partial({5, "14600"})),
    ?assertMatch(35.2590, volume({2, "14600"})),
    ?assertMatch(19200  , weight({6, "14600"})),
    ?assertMatch(224000 , weight({70, "14600"})),
    ?assertMatch(false  , partial({6, "77003"})),
    ?assertMatch(true   , partial({5, "77003"})),
    ?assertMatch(16.9740, volume({2, "77003"})),
    ?assertMatch(916.596, volume({108, "77003"})),
    ?assertMatch(13980  , weight({6, "77003"})),
    ?assertMatch(251640 , weight({108, "77003"})),
    ?assertMatch(35.0000, export_packages({70, "14600"})),
    ?assertMatch(18.0000, export_packages({108, "77003"})),
    ?assertMatch(1.0    , pallets({70, "14600"})),
    ?assertMatch(1.5    , pallets({162, "77003"})),
    ?assertMatch(2.0    , pallets({216, "77003"})),
    ok.
    

multi_test() ->
    run_me_once(),
    init_tests(),
    
    ?assertMatch(false  , partial_multi([{4, "14600"},{6, "77003"}])),
    ?assertMatch(true   , partial_multi([{5, "14600"},{6, "77003"}])),
    ?assertMatch(true   , partial_multi([{5, "14600"},{6, "77003"}, {5, "00000"}])),
    ?assertMatch(true   , partial_multi([{5, "14600"},{7, "77003"}])),
    ?assertMatch(86.1810, volume_multi([{2, "14600"},{6, "77003"}])),
    ?assertMatch(86.1810, volume_multi([{2, "14600"},{6, "77003"}, {5, "00000"}])),
    ?assertMatch(475640 , weight_multi([{108, "77003"},{70, "14600"}])),
    ?assertMatch(475640 , weight_multi([{108, "77003"},{70, "14600"}, {5, "00000"}])),
    ?assertMatch(53.0   , export_packages_multi([{108, "77003"},{70, "14600"}])),
    ?assertMatch(70.5   , export_packages_multi([{108, "77003"},{105, "14600"}, {5, "00000"}])),
    ?assertMatch(2.0    , pallets_multi([{108, "77003"},{70, "14600"}])),
    ?assertMatch(2.5    , pallets_multi([{162, "77003"},{70, "14600"}, {5, "00000"}])),
    volume_proplist([{108, "77003"},{70, "14600"}]),
    ok.
    

%%% @hidden
testrunner() ->
    simple_test(),
    multi_test(),
    ok.
    

-endif.
