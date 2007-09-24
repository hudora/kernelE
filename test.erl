-module(test).
-compile(export_all).

-import(mypl).
-include("mypl.hrl").

testbefuellung([]) -> [];
testbefuellung(L) ->
    [H|T] = L,
    io:format("A ~w~n", [H]),
    {_Softmlocation, Artnr, Quantity} = H,
    {ok, _} = mypl:store(mypl:oid(), Quantity, Artnr, 1950),
    testbefuellung(T).
    
testbefuellung() ->
    T = [{"011302", "65140", 8},
 {"011303", "76295", 540},
 {"011402", "76238", 460},
 {"011602", "10130", 27},
 {"011702", "10108", 21},
 {"011802", "10131", 25},
 {"012002", "14556", 24},
 {"012102", "30970/EK", 38},
 {"012202", "76346", 203},
 {"012203", "10125/WK", 28},
 {"012302", "31936", 46},
 {"012402", "76347", 254},
 {"012403", "71572/XX", 560},
 {"012500", "66706", 400},
 {"012702", "14612/01", 27},
 {"012802", "10106", 30},
 {"012903", "EM83162", 33},
 {"013002", "14613/01", 7},
 {"013102", "14557", 1},
 {"013103", "71520", 1600},
 {"013202", "76223", 266},
 {"013203", "WK74500", 24},
 {"013402", "65209", 2},
 {"013500", "10125/WK", 5},
 {"013502", "65210", 17},
 {"013602", "65215", 4},
 {"013702", "22002/02", 141},
 {"013802", "14600/03", 11},
 {"013803", "71491/WW", 67},
 {"014302", "22006/02", 132},
 {"014303", "71572/XX", 600},
 {"014402", "76207", 1989},
 {"014502", "76208", 577},
 {"014602", "24500/01", 64},
 {"014702", "65131", 3},
 {"014802", "76212", 27},
 {"014803", "26901", 15},
 {"014902", "76232", 322},
 {"015100", "71508/XX", 60},
 {"015202", "66612", 101},
 {"015300", "EM12011", 78},
 {"015302", "14690/01", 34},
 {"015402", "76115", 13},
 {"021303", "10106", 30},
 {"021400", "71590/TR", 150},
 {"021403", "76062", 216},
 {"021500", "71512", 120},
 {"021502", "65320", 20},
 {"021503", "01003", 48},
 {"021602", "76102", 19},
 {"021603", "TR76120", 23},
 {"021700", "76263/WW", 77},
 {"021702", "66613", 62},
 {"021800", "30970/EK", 48},
 {"021802", "76099", 20},
 {"021803", "ED66705", 80},
 {"021900", "WK22005", 216},
 {"021902", "10106", 30},
 {"021903", "01013", 64},
 {"022000", "71508/XX", 59},
 {"022002", "01003", 48},
 {"022003", "10106", 30},
 {"022100", "12106", 64},
 {"022102", "76095", 25},
 {"022103", "30960", 48},
 {"022200", "01004", 10},
 {"022202", "76046/01", 179},
 {"022203", "10131", 30},
 {"022300", "30960", 48},
 {"022302", "76093", 87},
 {"022303", "10120", 24},
 {"022402", "11006", 40},
 {"022403", "71507", 60},
 {"022500", "30970/WK", 32},
 {"022502", "65138", 2},
 {"022503", "11006", 40},
 {"022600", "76208", 2160},
 {"022602", "76074", 72},
 {"022603", "01007", 70},
 {"022702", "10107", 8},
 {"022703", "01007", 70},
 {"022800", "WK11005", 40},
 {"022802", "74201", 111},
 {"022803", "01007", 70},
 {"022900", "01007", 70},
 {"022903", "66603", 300},
 {"023000", "71540", 960},
 {"023002", "57400/01", 139},
 {"023100", "14600/03", 70},
 {"023103", "65136/WK", 45},
 {"023202", "76151", 2626},
 {"023203", "ED66705", 30},
 {"023303", "30970/WK", 32},
 {"023400", "42230", 48},
 {"023402", "12520", 35},
 {"023500", "30970/EK", 48},
 {"023502", "71658", 3},
 {"023503", "30970/WK", 32},
 {"023600", "11007", 40},
 {"023602", "66802", 1802},
 {"023603", "71414", 14},
 {"023700", "42230", 48},
 {"023702", "71650", 11},
 {"023703", "71414", 14},
 {"023800", "35238", 24},
 {"023802", "76404", 122},
 {"023803", "65136/WK", 45},
 {"023900", "76062", 216},
 {"023902", "71671/WK", 96},
 {"024100", "71512", 120},
 {"024200", "30970/WK", 32},
 {"024300", "42230", 48},
 {"024303", "69000", 6},
 {"024400", "65140/EK", 4},
 {"024402", "71621", 12},
 {"024403", "WK22005", 216},
 {"024500", "65335", 9},
 {"024502", "76686", 64},
 {"024503", "EM12011", 16},
 {"024600", "92705/01", 44},
 {"024602", "71508", 53},
 {"024603", "71414", 14},
 {"024700", "01009", 70},
 {"024702", "71412", 9},
 {"024703", "IS77009", 74},
 {"024803", "62100/WK", 75},
 {"024900", "71414", 14},
 {"024902", "92725", 486},
 {"024903", "71507/XX", 110},
 {"025000", "71414", 14},
 {"025002", "92720", 660},
 {"025003", "71414", 14},
 {"025100", "71414", 14},
 {"025102", "92715", 643},
 {"025103", "10120", 24},
 {"025200", "76187", 72},
 {"025202", "92710", 62},
 {"025203", "71512", 120},
 {"025300", "66802", 2016},
 {"025303", "76121", 300},
 {"025400", "66613", 160},
 {"025403", "71512", 120},
 {"031303", "65315", 30},
 {"031400", "11007", 40},
 {"031402", "TR76120", 1},
 {"031403", "11022", 15},
 {"031500", "71414", 14},
 {"031502", "33637", 22},
 {"031503", "62100/WK", 120},
 {"031602", "65140", 12},
 {"031603", "WK74500", 24},
 {"031703", "71414", 14},
 {"031800", "WK31141", 48},
 {"031802", "14610/01", 32},
 {"031803", "71414", 14},
 {"031900", "WK11005", 40},
 {"031902", "10120", 18},
 {"031903", "41037", 48},
 {"032000", "12012", 48},
 {"032002", "76228", 53},
 {"032003", "71414", 14},
 {"032100", "10106", 30},
 {"032103", "65330", 14},
 {"032202", "76676", 177},
 {"032203", "71414", 14},
 {"032303", "65167", 96},
 {"032403", "71513", 22},
 {"032500", "66603", 300},
 {"032502", "11005", 7},
 {"032503", "71414", 14},
 {"032602", "76343", 172},
 {"032700", "11012", 24},
 {"032703", "41039", 48},
 {"032800", "WK57800", 600},
 {"032802", "76700", 15},
 {"032803", "71414", 14},
 {"032900", "14690/01", 36},
 {"032903", "14695", 36},
 {"033000", "65140/EK", 12},
 {"033002", "76128", 10},
 {"033003", "73200", 240},
 {"033100", "64100", 120},
 {"033202", "76233", 70},
 {"033203", "14695", 36},
 {"033300", "71414", 14},
 {"033303", "71414", 14},
 {"033403", "14695", 36},
 {"033503", "71414", 14},
 {"033600", "01104", 96},
 {"033603", "30960", 48},
 {"033700", "71516", 58},
 {"033703", "TR74080", 180},
 {"033802", "14612", 15},
 {"033803", "10106", 30},
 {"033900", "14695", 36},
 {"033903", "33639", 24},
 {"034200", "65330", 9},
 {"034300", "31470", 48},
 {"034303", "30960", 48},
 {"034402", "01104", 90},
 {"034403", "42239", 48},
 {"034500", "64100", 120},
 {"034502", "76069", 161},
 {"034503", "65172", 8},
 {"034600", "14890/01", 56},
 {"034603", "22002/02", 216},
 {"034700", "62100/WK", 120},
 {"034703", "65140/EK", 12},
 {"034900", "65330", 8},
 {"035000", "57100/01", 300},
 {"035002", "76022/01", 73},
 {"035003", "71572", 20},
 {"035100", "22006/02", 216},
 {"035103", "23107", 216},
 {"035200", "30960", 48},
 {"035203", "42233", 48},
 {"035300", "74500", 24},
 {"035302", "14890/01", 4},
 {"035303", "57400/01", 180},
 {"035400", "22006/02", 216},
 {"041302", "12104", 5},
 {"041303", "69000", 8},
 {"041400", "69000", 8},
 {"041403", "74500/TR", 24},
 {"041500", "62100/WK", 120},
 {"041603", "76700", 31},
 {"041700", "65192", 12},
 {"041702", "65330", 9},
 {"041703", "76000", 1728},
 {"041800", "71547", 60},
 {"041802", "64111", 25},
 {"041803", "62100/WK", 120},
 {"041900", "33641", 24},
 {"041903", "71512", 120},
 {"042000", "10106", 30},
 {"042002", "WK83162", 152},
 {"042003", "64100", 120},
 {"042100", "14600/03", 70},
 {"042102", "42239", 48},
 {"042202", "12557", 16},
 {"042203", "42430/WK", 48},
 {"042403", "61010", 378},
 {"042500", "65192", 12},
 {"042503", "31440", 48},
 {"042600", "72000", 52},
 {"042603", "10106", 30},
 {"042700", "WW71525", 84},
 {"042703", "74500/TR", 24},
 {"042800", "62100/WK", 120},
 {"042802", "76131", 19},
 {"042803", "65168", 14},
 {"042900", "10106", 30},
 {"042903", "10202", 18},
 {"043000", "33640", 24},
 {"043002", "33646", 5},
 {"043003", "42430/WK", 42},
 {"043100", "62100/WK", 120},
 {"043203", "65136/WK", 45},
 {"043300", "65136/WK", 45},
 {"043302", "65175", 11},
 {"043303", "33645", 24},
 {"043400", "10130", 30},
 {"043402", "01023", 199},
 {"043403", "65140/EK", 12},
 {"043502", "01022", 442},
 {"043503", "33643", 24},
 {"043602", "01020", 14},
 {"043603", "14891", 4},
 {"043700", "92705/01", 44},
 {"043703", "71580/XX", 439},
 {"043800", "92705/01", 44},
 {"043803", "66625", 96},
 {"044100", "14897", 348},
 {"044200", "65172", 8},
 {"044300", "14899", 2},
 {"044303", "74500/TR", 24},
 {"044400", "42233", 48},
 {"044403", "76344", 300},
 {"044500", "65138", 18},
 {"044502", "01004", 32},
 {"044503", "30970/EK", 8},
 {"044600", "64100", 120},
 {"044602", "01003", 1},
 {"044603", "14800/01", 56},
 {"044700", "84003", 80},
 {"044803", "66625", 96},
 {"044900", "65167", 96},
 {"044903", "84003", 80},
 {"045000", "84003", 80},
 {"045003", "71545", 384},
 {"045100", "33646", 24},
 {"045200", "10106", 30},
 {"045203", "65138", 18},
 {"045300", "71575", 20},
 {"045303", "12350", 70},
 {"045400", "74500/TR", 24},
 {"050103", "14613/01", 56},
 {"050200", "92700/01", 46},
 {"050300", "92700/01", 46},
 {"050303", "66625", 96},
 {"050400", "92700/01", 46},
 {"050403", "74500/TR", 24},
 {"050500", "92700/01", 46},
 {"050502", "74210", 8},
 {"050503", "71360", 10},
 {"050602", "71529", 27},
 {"050603", "WK76095", 28},
 {"050702", "42236", 44},
 {"050703", "62100/WK", 120},
 {"050802", "42233", 44},
 {"050803", "IS77069", 470},
 {"050900", "92700/01", 46},
 {"050902", "12106", 61},
 {"050903", "71570/WK", 8},
 {"051000", "92700/01", 46},
 {"051200", "30970/EK", 20},
 {"051300", "42236", 48},
 {"051302", "65315", 47},
 {"051303", "72113", 96},
 {"051400", "92700/01", 46},
 {"051402", "01013", 55},
 {"051403", "10106", 30},
 {"051500", "92700/01", 46},
 {"051503", "66603", 300},
 {"051602", "71416", 1},
 {"051603", "WK83162", 240},
 {"051700", "92700/01", 46},
 {"051702", "11012", 11},
 {"051703", "84003", 80},
 {"051802", "92705/01", 25},
 {"051803", "WK76084", 126},
 {"051902", "76234", 88},
 {"051903", "66625", 96},
 {"052002", "22005/02", 46},
 {"052003", "65209", 18},
 {"052102", "34636", 28},
 {"052103", "WK76095", 28},
 {"052202", "34637", 31},
 {"052203", "14690/01", 36},
 {"052302", "34639", 31},
 {"052303", "74500/TR", 24},
 {"052402", "33645", 10},
 {"052403", "76676", 240},
 {"052502", "12730", 58},
 {"052503", "WK74500", 24},
 {"052603", "74500/TR", 24},
 {"052702", "11021/01", 7},
 {"052703", "76062", 216},
 {"052800", "72000", 104},
 {"052900", "72000", 104},
 {"052902", "01105", 51},
 {"052903", "71492/XX", 90},
 {"053000", "72000", 104},
 {"053100", "72000", 104},
 {"053102", "66615", 20},
 {"053103", "66625", 96},
 {"053200", "72000", 104},
 {"053202", "66616", 114},
 {"053300", "72000", 104},
 {"053402", "76411/01", 117},
 {"053403", "71560/XX", 256},
 {"053502", "34638", 29},
 {"053503", "64100", 120},
 {"053602", "10011", 9},
 {"053603", "65138", 18},
 {"053702", "34640", 15},
 {"053803", "HU71529", 488},
 {"053903", "WK83161", 15},
 {"054200", "WK74500", 24},
 {"054302", "76126", 38},
 {"054400", "WK76095", 28},
 {"054402", "71414", 7},
 {"054403", "65136/WK", 45},
 {"054500", "69000", 8},
 {"054503", "71670/XX", 406},
 {"054600", "71414", 14},
 {"054602", "76116", 42},
 {"054603", "71416", 7},
 {"054700", "84003", 80},
 {"054702", "76104", 6},
 {"054703", "69000", 8},
 {"054803", "65140", 12},
 {"054900", "65140", 12},
 {"054902", "14605", 54},
 {"054903", "01020", 18},
 {"055000", "71416", 7},
 {"055002", "14897", 90},
 {"055003", "69000", 8},
 {"055100", "65136/WK", 24},
 {"055103", "76062", 216},
 {"055200", "71416", 7},
 {"055203", "66603", 300},
 {"055300", "WK76095", 28},
 {"055400", "69000", 8},
 {"055402", "22006", 60},
 {"055403", "10120", 24},
 {"055500", "74500/TR", 24},
 {"060102", "14555", 10},
 {"060103", "69000", 8},
 {"060200", "66603", 300},
 {"060202", "10106", 27},
 {"060203", "65140", 12},
 {"060300", "71400/00", 130},
 {"060302", "14800/01", 49},
 {"060303", "57400/01", 300},
 {"060400", "WK83161", 180},
 {"060402", "92700/01", 45},
 {"060403", "84018", 72},
 {"060500", "76346", 480},
 {"060502", "12550", 48},
 {"060503", "74500/TR", 24},
 {"060600", "65140", 12},
 {"060602", "76263", 624},
 {"060603", "71570/WK", 102},
 {"060700", "69000", 8},
 {"060702", "76249", 170},
 {"060703", "TR76143", 91},
 {"060802", "76156", 880},
 {"060803", "72115", 100},
 {"060900", "69000", 8},
 {"060902", "76345", 413},
 {"060903", "WK76095", 28},
 {"061000", "69000", 8},
 {"061100", "83161", 288},
 {"061200", "65136/WK", 45},
 {"061300", "71530/TR", 395},
 {"061302", "WK76095", 28},
 {"061303", "WK11005", 40},
 {"061400", "65136/WK", 45},
 {"061402", "31734", 9},
 {"061403", "84018", 72},
 {"061500", "76104", 54},
 {"061502", "31730", 29},
 {"061503", "74500/TR", 24},
 {"061602", "31470", 38},
 {"061603", "65136/WK", 45},
 {"061700", "66651", 2160},
 {"061702", "31460", 33},
 {"061703", "WK11005", 40},
 {"061800", "76062", 216},
 {"061802", "31450", 38},
 {"061803", "71670/XX", 270},
 {"061900", "65136/WK", 45},
 {"061902", "31440", 17},
 {"061903", "69000", 8},
 {"062000", "69000", 8},
 {"062003", "WK22005", 216},
 {"062100", "71530/TR", 400},
 {"062103", "69000", 8},
 {"062200", "WK76084", 120},
 {"062203", "76233", 108},
 {"062300", "10106", 30},
 {"062302", "31930", 43},
 {"062303", "WK76095", 28},
 {"062402", "31933", 40},
 {"062403", "30860", 48},
 {"062500", "76062", 120},
 {"062502", "76343", 576},
 {"062503", "66616", 160},
 {"062600", "69000", 8},
 {"062602", "31360", 27},
 {"062603", "69000", 8},
 {"062700", "69000", 8},
 {"062702", "31350", 24},
 {"062703", "84018", 72},
 {"062802", "31340", 6},
 {"062803", "01009", 70},
 {"062903", "14800/01", 56},
 {"063000", "83161", 288},
 {"063100", "74500/TR", 24},
 {"063103", "65175", 10},
 {"063203", "65136/WK", 45},
 {"063300", "69000", 8},
 {"063302", "76209", 491},
 {"063303", "WK76095", 28},
 {"063400", "65325", 18},
 {"063402", "76158", 65},
 {"063403", "WW71525", 90},
 {"063500", "69000", 8},
 {"063503", "65175", 10},
 {"063600", "65175", 10},
 {"063603", "84018", 72},
 {"063700", "65175", 10},
 {"063703", "65175", 10},
 {"063800", "74500/TR", 24},
 {"063802", "34641", 17},
 {"063803", "65136/WK", 45},
 {"063900", "12031/01", 240},
 {"063902", "34642", 20},
 {"063903", "66706", 400},
 {"064100", "76137", 72},
 {"064200", "65136/WK", 45},
 {"064300", "65175", 10},
 {"064302", "WK76095", 28},
 {"064303", "65136/WK", 45},
 {"064400", "66500", 336},
 {"064402", "71538", 263},
 {"064403", "76083", 150},
 {"064500", "30970", 32},
 {"064502", "41034", 55},
 {"064503", "76052", 1728},
 {"064600", "34639", 24},
 {"064602", "41038", 54},
 {"064603", "65136/WK", 45},
 {"064700", "WW71525", 90},
 {"064703", "76605/01", 3}],
    testbefuellung(T).
