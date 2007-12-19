-module(mypl_volumes).

% -export([aggregate/0]).

-include_lib("stdlib/include/qlc.hrl").
-include("include/mypl.hrl").

%% @doc should be run before mnesia is started for the first time.
run_me_once() ->
    mnesia:start(),
    % the main tables are kept in RAM with a disk copy for fallback
    mypl_db:init_table_info(mnesia:create_table(eap,    [{disc_copies, [node()]}, {attributes, record_info(fields, eap)}]), eap).
    

get_eap(ArtNr) ->
    case mnesia:dirty_read({eap, ArtNr}) of
        [] -> unknown_artnr;
        [Eap] -> Eap
    end.

%     """Returns True if this Item does not result in an export_package to be opened."""
anbruch({Quantity, ArtNr}) ->
    case get_eap(ArtNr) of
        unknown_artnr -> true;
        Eap -> (Quantity rem Eap#eap.prod_exportkarton) /= 0
    end.
    
%     """Retuns the volume of this item in liters"""
%     # TODO: Exportkartonvolumen
volumen({Quantity, ArtNr}) ->
    case get_eap(ArtNr) of
        unknown_artnr -> 0;
        Eap -> Quantity * Eap#eap.prod_vol
    end.

%     """Retuns the weight of this item in g."""
%     # TODO: Exportkartongewicht usw.
gewicht({Quantity, ArtNr}) ->
    case get_eap(ArtNr) of
        unknown_artnr -> 0;
        Eap -> Quantity * Eap#eap.prod_g
    end.

% def max_packstueck_gewicht(self):
%     """Returns the weigtht of the most heavy box for ths item."""
%     if self.menge >= self.produkte_pro_exportkarton:
%         return self.gewicht_pro_exportkarton
%     return (self.gewicht_pro_exportkarton / self.produkte_pro_exportkarton) * self.menge 
% 
%     """Returns the number of pallets of this Item."""
paletten({Quantity, ArtNr}) ->
    case get_eap(ArtNr) of
        unknown_artnr -> 0;
        Eap -> Quantity / Eap#eap.prod_exportkarton * Eap#eap.export_palette
    end.

% def picks(self):
%     """Returns the number storage locations to be accessed to get the item."""
%     picks = self.paletten
%     # round up
%     if picks != int(picks):
%         picks = int(picks + 1)
%     return picks
% 
export_kartons({Quantity, ArtNr}) ->
    case get_eap(ArtNr) of
        unknown_artnr -> 0;
        Eap -> Quantity / Eap#eap.prod_exportkarton
    end.

% def export_karton_gewichte(self):
%     """Returns the weights of the estimated number of packages which will be shipped in gramms."""
%     menge = self.menge
%     ret = []
%     while menge:
%         if menge > self.produkte_pro_exportkarton:
%             ret.append(self.gewicht_pro_exportkarton)
%             menge -= self.produkte_pro_exportkarton
%         else:
%             ret.append(menge * self.einzelgewicht)
%             menge = 0
%     return ret
% 
% def packstuecke(self):
%     """Returns the absolute number of packages to fullfill this item as an integer.
%     
%     This could take into account that packages can be bundled etc.
%     """
%     
%     packstuecke = self.export_kartons
%     # round up
%     if packstuecke != int(packstuecke):
%         packstuecke = int(packstuecke + 1)
%     return int(packstuecke)



% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).
    

none([Hd|Tail]) ->
    case Hd of
        true -> true;
        false -> none(Tail)
    end;
none([]) -> false.

anbruch_multi(L) ->
    none(lists:map(fun(X) -> anbruch(X) end, L)).
    
volumen_multi(L) ->
    lists:sum(lists:map(fun(X) -> volumen(X) end, L)).
    
gewicht_multi(L) ->
    lists:sum(lists:map(fun(X) -> gewicht(X) end, L)).
    

init_tests() ->
    mnesia:dirty_write(#eap{artnr="14600",
        prod_x=115, prod_y=730, prod_z=210, prod_vol=115*730*210/1000/1000, prod_g=3200,
        export_x=225, export_y=740, export_z=215, export_vol=225*740*215/1000/1000,export_g=700,
        prod_ve1=0,prod_exportkarton=2,export_palette=35}),
    mnesia:dirty_write(#eap{artnr="77003",
        prod_x=460, prod_y=41, prod_z=450, prod_vol=460*41*450/1000/1000, prod_g=2330,
        export_x=480, export_y=280, export_z=480, export_vol=480*280*480/1000/1000,export_g=14780,
        prod_ve1=0,prod_exportkarton=6,export_palette=18}),
    ok.
    
%%% @hidden
simple_test() ->
    run_me_once(),
    init_tests(),
    false   = anbruch({4, "14600"}),
    true    = anbruch({5, "14600"}),
    35.2590 = volumen({2, "14600"}),
    % 1234.07 = volumen({70, "14600"}),
    19200   = gewicht({6, "14600"}),
    224000  = gewicht({70, "14600"}),
    
    false   = anbruch({6, "77003"}),
    true    = anbruch({5, "77003"}),
    16.9740 = volumen({2, "77003"}),
    916.596 = volumen({108, "77003"}),
    13980   = gewicht({6, "77003"}),
    251640  = gewicht({108, "77003"}),
    ok.
    

multi_test() ->
    run_me_once(),
    init_tests(),
    
    false   = anbruch_multi([{4, "14600"},{6, "77003"}]),
    true    = anbruch_multi([{5, "14600"},{6, "77003"}]),
    true    = anbruch_multi([{5, "14600"},{6, "77003"}, {5, "00000"}]),
    true    = anbruch_multi([{5, "14600"},{7, "77003"}]),
    86.1810 = volumen_multi([{2, "14600"},{6, "77003"}]),
    86.1810 = volumen_multi([{2, "14600"},{6, "77003"}, {5, "00000"}]),
    % 1234.07 = volumen({70, "14600"}),
    475640  = gewicht_multi([{108, "77003"},{70, "14600"}]),
    475640  = gewicht_multi([{108, "77003"},{70, "14600"}, {5, "00000"}]),
    ok.
    

%%% @hidden
testrunner() ->
    simple_test(),
    multi_test(),
    ok.
    

-endif.
