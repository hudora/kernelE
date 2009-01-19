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
-include("mypl.hrl").

%% @doc should be run before mnesia is started for the first time.
run_me_once() ->
    mnesia:start(),
    % the main tables are kept in RAM with a disk copy for fallback
    mypl_db:init_table_info(mnesia:create_table(eap,
                                                [{disc_copies, [node()]},
                                                {attributes, record_info(fields, eap)}]), eap).
    

get_eap(ArtNr) ->
    case mnesia:dirty_read({eap, ArtNr}) of
        [] -> unknown_artnr;
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
        unknown_artnr ->
            true;
        Eap ->
            case Eap#eap.prod_export_package of
                0 ->
                    error_logger:warning_msg("No prod_export_package for ~s~n", [ArtNr]),
                    true;
                ProdExportPackage ->
                    (Quantity rem ProdExportPackage) /= 0
            end
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
        unknown_artnr -> 
            0;
        Eap -> 
            case Eap#eap.prod_export_package * Eap#eap.export_pallet of
                0 ->
                    error_logger:warning_msg("No prod_export_package or export_pallet for ~s~n", [ArtNr]),
                    0;
                _ProdExportPackage ->
                    Quantity / (Eap#eap.prod_export_package * Eap#eap.export_pallet)
            end
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
        Eap -> 
            case Eap#eap.prod_export_package of
                0 ->
                    error_logger:warning_msg("No prod_export_package for ~s~n", [ArtNr]),
                    0;
                _ProdExportPackage ->
                    Quantity / Eap#eap.prod_export_package
            end
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
    {anbruch,         partial_multi(L)},
    {weight,          weight_multi(L)},
    {paletten,        pallets_multi(L)},
    {export_packages, export_packages_multi(L)}
    % max_packstueck_gewicht
    % picks
    ].
    

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
    ?assertMatch([{volume, 2.150661e+03}, {anbruch, false}, {weight, 475640},
                  {paletten, 2.000000e+00}, {export_packages, 5.300000e+01}],
                 volume_proplist([{108, "77003"}, {70, "14600"}])),
    ok.
    

realworld_test() ->
    run_me_once(),
    init_tests(),
    
    lists:map(fun(X) -> mnesia:dirty_write(X) end,
    [{eap,"76232/HB",{{2008,2,8},{6,29,0}}, 220, 200,  80, 3.520000e+00,   275,  3,   0,   0,   0,            0,    0,  0,   0,   0,   0,            0,     0,   0},
     {eap,"76052/HB",{{2008,2,8},{6,29,0}}, 160, 160,  70, 1.792000e+00,    34,  6,   0,   0,   0,            0,    0,144, 440, 360, 560, 8.870400e+01,  5900,   7},
     {eap,"76008",   {{2008,2,8},{6,29,0}}, 140, 160,  30, 6.720000e-01,    58,  6, 100, 160, 180, 2.880000e+00,  394, 96, 300, 180, 560, 3.024000e+01,  7000,  24},
     {eap,"76237/HB",{{2008,2,8},{6,29,1}}, 180,  78,  55, 7.722000e-04,    65,  5,   0,   0,   0,            0,    0, 10, 200, 130, 300, 7.800000e+00,  1600, 133},
     {eap,"77010",   {{2008,2,8},{6,29,1}}, 230, 425,  52, 5.083000e+00,   386,  6,  52, 230, 425, 5.083000e-05, 2450, 12, 360, 410, 480, 7.084800e+01,  5100,  16},
     {eap,"76207/HB",{{2008,2,8},{6,29,0}}, 140, 195,  35, 9.555000e-01,    50,  8,   0,   0,   0,            0,    0,  0,   0,   0,   0,            0,     0,   0},
     {eap,"76249",   {{2008,2,8},{6,29,1}}, 170, 275,  40, 1.870000e+00,   378,  6, 190, 340, 280, 1.808800e+01, 2400, 36, 600, 240, 600, 8.640000e+01, 14900,  14},
     {eap,"76115",   {{2008,2,8},{6,29,0}}, 615, 455,  90, 2.518425e+01,  2673,  0,   0,   0,   0,            0,    0,  4, 385, 470, 640, 1.158080e+02, 11900,   9},
     {eap,"76015/HB",{{2008,2,8},{6,29,0}}, 240, 330,  70, 5.544000e+00,   280,  4,   0,   0,   0,            0,    0, 48, 425, 400, 490, 8.330000e+01, 13900,  16},
     {eap,"85040",   {{2008,2,8},{6,29,1}},  80, 440,   5, 1.760000e-01,    40,  6,  90, 450,  30, 1.215000e+00,  267, 96, 190, 130, 465, 1.148550e+01,  5250,  12},
     {eap,"76660",   {{2008,2,8},{6,29,1}}, 180, 180, 150, 4.860000e+00,   910,  4, 170, 380, 390, 2.519400e+01, 4500, 12, 410, 435, 560, 9.987600e+01, 14000,   8},
     {eap,"76236/HB",{{2008,2,8},{6,29,0}}, 180,  78,  55, 7.722000e-01,    65,  5,   0,   0,   0,            0,    0, 10, 200, 130, 310, 8.060000e+00,  1400, 133},
     {eap,"76228",   {{2008,2,8},{6,29,0}}, 600, 230, 180, 2.484000e+01,  1275,  2, 120, 360, 600, 2.592000e+01, 3200,  6, 440, 380, 610, 1.019920e+02, 10000,  12},
     {eap,"72000",   {{2008,2,8},{6,29,0}},  -1,  -1,  -1, 2.549750e-03,    -1, -1,  -1,  -1,  -1,           -1,   -1, -1,  -1,  -1,  -1, 1.387200e-01,    -1,  -1},
     {eap,"76404/HB",{{2008,2,8},{6,29,1}},   1,   1,   1, 1.009855e-05,     1,  1,   1,   1,   1,            1,    1,  1,   1,   1,   1, 5.222400e-05,     1,   1},
     {eap,"76081",   {{2008,2,8},{6,29,0}},   0,   0,   0,            0,     0,  0,   0,   0,   0,            0,    0,  0,   0,   0,   0,            0,     0,   0},
     {eap,"76010",   {{2008,2,8},{6,29,0}}, 180, 160,  50, 1.440000e+00,   190,  6, 160, 160, 260, 6.656000e+00, 1180, 72, 440, 405, 490, 8.731800e+01, 14500,   0},
     {eap,"76346/HB",{{2008,2,8},{6,29,1}}, 280, 210,  65, 3.822000e+00,   210,  4,   0,   0,   0,            0,    0, 24, 305, 230, 755, 5.296325e+01,     0,  25},
     {eap,"76275",   {{2008,2,8},{6,29,1}}, 182,  50, 110, 1.001000e+00,    33, 12, 280, 120, 340, 1.142400e+01,  500,144, 350, 580, 720,            0,  8300,   8},
     {eap,"76095",   {{2008,2,8},{6,29,0}}, 975, 285, 110, 3.056625e+01, 10800,  0,   0,   0,   0,            0,    0,  1, 110, 285,   0, 3.056625e+01, 10800,  28},
     {eap,"85030",   {{2008,2,8},{6,29,1}}, 238, 440,  10, 1.047200e+00,   120,  6, 255, 450,  55, 6.311250e+00,  768, 36, 260,   0, 465, 4.110600e+01,  5800,  24},
     {eap,"72106",   {{2008,2,8},{6,29,0}}, 355, 235,  70, 5.839750e+00,  1750,  0,   0,   0,   0,            0,    0,  6,   0, 375, 455, 4.095000e+01, 11100,  30},
     {eap,"76233",   {{2008,2,8},{6,29,0}}, 360, 230, 100, 8.280000e+00,   450,  2, 150, 260, 470, 1.833000e+01, 1200,  0, 460, 270, 490, 6.085800e+01,  7000,  18},
     {eap,"76212",   {{2008,2,8},{6,29,0}}, 285, 440,  90, 1.128600e+01,   350,  4, 190, 190, 440, 1.588400e+01,    0, 12, 540, 200, 580, 6.264000e+01,  5000,  20},
     {eap,"85020",   {{2008,2,8},{6,29,1}},  80, 440,  16, 5.632000e-01,    60,  6,  90, 110, 450,            0,  385, 36, 240, 285, 460, 3.146400e+01,  3038,  24},
     {eap,"76226",   {{2008,2,8},{6,29,0}}, 520, 230, 180, 2.152800e+01,  1175,  2, 120, 340,   0, 2.203200e+01, 3000,  6, 380, 340, 520, 6.718400e+01,  8500,  20},
     {eap,"71700",   {{2008,2,8},{6,29,0}}, 125, 171, 610, 1.303875e+01,  2100,  0,   0,   0,   0,            0,    0,  2, 175, 480, 640, 5.376000e+01,  4800,  19},
     {eap,"71537",   {{2008,2,8},{6,28,5}}, 185,  20, 320, 1.184000e+00,   115,  5,   0, 190, 330, 4.389000e+00,  575, 50, 310, 380, 420, 4.947600e+01,  7000,  20},
     {eap,"71400",   {{2008,2,8},{6,28,5}}, 127, 127, 127, 2.048383e+00,   120,  0, 415, 255, 510, 5.397075e+01, 4840, 26, 445, 290, 540, 6.968700e+01,  4900,  18},
     {eap,"76686",   {{2008,2,8},{6,29,1}}, 105, 185, 185, 3.593625e+00,     0,  6, 200, 235, 580, 2.726000e+01, 9500, 12, 265, 430, 605, 6.893975e+01, 20000,  20},
     {eap,"76209/HB",{{2008,2,8},{6,29,0}},  55,  55, 177, 5.354250e-01,    55,  6,   0,   0,   0,            0,    0, 72, 490, 210, 530, 5.453700e+01,  4600,  21},
     {eap,"85010",   {{2008,2,8},{6,29,1}},  80, 438,  10,            0,    44,  6,  90, 450, 667, 2.701350e+01,  267, 36, 195, 202, 460, 1.811940e+01,  2322,  34},
     {eap,"76266",   {{2008,2,8},{6,29,1}}, 151, 256,   0, 5.411840e-01,   190,  6, 120, 155, 260, 4.836000e+00, 1150, 96, 405, 415, 540, 9.076050e+01, 19900,  16},
     {eap,"76221/HB",{{2008,2,8},{6,29,0}}, 205, 305,  25, 1.563125e+00,   306,  6,   0,   0,   0,            0,    0,  0,   0,   0,   0,            0,     0,   0},
     {eap,"76213",   {{2008,2,8},{6,29,0}}, 210,   0,  80, 3.024000e+00,   150,  4, 170, 210, 230, 8.211000e+00,  900, 24, 440, 240, 520, 5.491200e+01,  5000,  28},
     {eap,"76263",   {{2008,2,8},{6,29,1}},   0, 256,  12, 4.638720e-01,   170,  6, 105, 155, 260, 4.231500e+00, 1400, 96, 285, 465, 670, 8.879175e+01, 16900,  12}]
    ),
    
    [{volume,10.5600}, {anbruch,true}, {weight,825}, {paletten,0}, {export_packages,0}]
        = volume_proplist([{3,"76232/HB"}]),
    
    % [{volume, 618.207}, {anbruch, true}, {weight, 63194}, {paletten, 6.52380}, {export_packages, 7.51667}]}
    [{volume, _}, {anbruch, true}, {weight, 63194}, {paletten, _}, {export_packages, _}]
        = volume_proplist([{3,"76232/HB"},{6,"76052/HB"},{6,"76008"},{5,"76237/HB"},{6,"77010"},{8,"76207/HB"},
                           {6,"76249"},{4,"76115"},{4,"76015/HB"},{6,"85040"},{4,"76660"},{5,"76236/HB"},
                           {2,"76228"},{4,"72000"},{2,"76404/HB"},{12,"76081"},{6,"76010"},{4,"76346/HB"},
                           {6,"76275"},{1,"76095"},{6,"85030"},{3,"72106"},{4,"76233"},{4,"76212"},{6,"85020"},
                           {2,"76226"},{2,"71700"},{5,"71537"},{26,"71400"},{6,"76686"},{6,"76209/HB"},
                           {6,"85010"},{6,"76266"},{6,"76221/HB"},{4,"76213"},{6,"76263"}]),
    ok.
    

%%% @hidden
testrunner() ->
    simple_test(),
    multi_test(),
    ok.
    

-endif.
