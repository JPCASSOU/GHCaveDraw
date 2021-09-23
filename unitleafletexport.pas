unit UnitLeafletExport;
// 2020-02-11: Point de contrôle temporel

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics;

type

{ TLeafletExport }

  TLeafletExport = class
  private
    FCurrentIndentation: integer;
    FCurrentClassName: string;
    FCurrentProcName : string;
    FLeafletVarNameDispEntrances    : string;
    FLeafletVarNameDispCenterlines  : string;
    FLeafletVarNameDispStations     : string;
    FLeafletVarNameDispSymbols      : string;

    FDocTitle          : string;
    FP                 : TextFile;
    FMapWidth          : integer;
    FMapHeight         : integer;
    FCentroideLat      : double;
    FCentroideLon      : Double;
    // styles courants
    FCurrentLineWidth    : double;
    FCurrentLineColor    : TColor;
    FCurrentFillColor    : TColor;
    FCurrentLineOpacity  : double;
    FCurrentFillOpacity  : double;
    FListeLayers         : TStringList;

    FCurrentLayerName       : string;
    FCurrentLayerDescription: string;

    FCurrentTagString    : string;
    FCurrentObjectName   : string;

    procedure BeginJSFunction(const QProcName: string; const QParams: string='');
    procedure EndJSFunction();

    procedure BeginClassJSFunction(const QClassName, QMethod: string; const QParams: string='');
    procedure EndClassJSFunction();

    procedure BeginJSEventListener(const EventName: string; const Desc: string='');
    procedure EndJSEventListener();



    procedure jsFOR(const QVarCounter, QVarNb: string; const QStart: integer=0; const QTag: string=''; const QIndent: integer=4);
    procedure jsNEXT(const QVarCounter: string; const QTag: string=''; const QIndent: integer=4);
    procedure jsIF(const QCondition: string; const QTag: string=''; const QIndent: integer=4);
    procedure jsELSE(const QTag: string=''; const QIndent: integer=4);
    procedure jsENDIF(const QTag: string=''; const QIndent: integer=4);

    procedure jsSELECT_CASE(const QSelecteur: integer; const QTag: string=''; const QIndent: integer=4);
    procedure jsCASE(const QItem: integer; const QTag: string=''; const QIndent: integer=4);
    procedure jsBREAK(const QIndent: integer=4);
    procedure jsEND_SELECT(const QTag: string=''; const QIndent: integer=4);

    procedure JSSeparateur(const QMotif: Char='='; const nb: integer=80);
    procedure jsWEND(const QTag: string=''; const QIndent: integer=4);
    procedure jsWHILE(const QCondition: string; const QTag: string=''; const QIndent: integer=4);


  public
    function  Initialiser(const QFilename: string;
                          const QDocTitle: string;
                          const QMapWidth, QMapHeight: integer;
                          const QCentroideLon, QCentroideLat: Double): boolean;
    procedure Finaliser();

    procedure WriteLine(const S: string);
    procedure WrtLinFmt(const Fmt: String; const Args: array of const);


    procedure WriteHeader();
    procedure WriteFooter();

    procedure BeginConditionalSection(const B: boolean; const VariableName: string);
    procedure EndConditionalSection(const VariableName: string);

    procedure AddLayer(const L: string; const Desc: string);
    procedure setCurrentLayer(const Idx: integer);
    procedure DefineStylePoly(const LineWidth: double; const LineColor, FillColor: TColor; const LineOpacity, FillOpacity: byte);

    procedure AddPoint(const Lat, Lon, Alt: double; const Name: string; const Description: string);
    procedure BeginPolygon(const Name: string; const TagString: string);
    procedure AddVertex(const Lat, Lon, Alt: double; const IsLast: boolean);
    procedure EndPolygon();
    procedure BeginPolyLine(const Name: string; const TagString: string);
    procedure EndPolyLine();


    procedure DrawPoint(const Lat, Lon, Alt: double;
                        const Taille: double;
                        const Caption: string;
                        const Description: string);
    procedure DrawSegment(const Lat1, Lon1, Lat2, Lon2: double);
    function  getLeafletVarNameDispEntrances(): string;
    function  getLeafletVarNameDispCenterlines(): string;
    function  getLeafletVarNameDispStations(): string;
    function  getLeafletVarNameDispSymbols(): string;


end;

implementation
uses
  DGCDummyUnit;
const
  JS_FUNC_CreateMarker    = 'CreateMarker';
  JS_FUNC_CreateScrap     = 'CreateScrap';
  JS_FUNC_CreatePoint     = 'CreatePoint';
  JS_FUNC_CreateSegment   = 'CreateSegment';
const
  NB_LAYERS_FOND_CARTE    = 3;

  URL_LEAFLET_JS          = 'https://unpkg.com/leaflet@1.3.1/dist/leaflet.js';  //'http://cdn.leafletjs.com/leaflet/v0.7.7/leaflet.js';
  URL_LEAFLET_CSS         = 'https://unpkg.com/leaflet@1.3.1/dist/leaflet.css'; //'http://cdn.leafletjs.com/leaflet/v0.7.7/leaflet.css';
  //URL_GOOGLE_KEY          = 'https://maps.google.com/maps/api/js?key=AIzaSyCTIn22N0OJ6j4PjvTkqi8ROqN_XhKZUyQ';
  MIME_TEXT_JAVASCRIPT    = 'text/javascript';

  VAR_LAYER_OSM_MAPNIK       = 'LayerOsm';
  VAR_LAYER_OSM_TOPO         = 'LayerOsmTopo';
  VAR_LAYER_GOOGLE_SATELLITE = 'LayerGoogleSat';

  URL_TILES_OSM_MAPNIK       = 'https://{s}.tile.osm.org/{z}/{x}/{y}.png';
  URL_TILES_GOOGLE_SATELLITE = 'https://www.google.com/maps/vt?lyrs=s@189&gl=cn&x={x}&y={y}&z={z}';
  URL_TILES_OSM_TOPO         = 'https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png';



  NAMEREF_TITRE           = 'Titre';
  NAMEREF_MENU_LATERAL    = 'MenuLateral';
  NAMEREF_MAP             = 'MyMap';

  JS_FUNCTION_INITIALISER = 'Initialiser';

  TO_DECIMETERS = 10.00; // Optimisation: On génère des coordonnées entières en décimètres

// pour se rendre indépendant des unités de GHTopo
function FormatterNombreReel(const Value: double; const NbDecs: integer = 2): string;
var
  EWE: String;
begin
  EWE := Format('%%.%df', [NbDecs]);
  Result := Format(EWE, [Value]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
end;

function QColorToHTMLColor(const Couleur: TColor): string;
begin
  Result := Format('#%.2X%.2X%.2X', [Red(Couleur), Green(Couleur), Blue(Couleur)]);
end;
function ToJSCoord(const X: double): integer;
begin
  result := trunc(TO_DECIMETERS * X);
end;
//******************************************************************************
procedure TLeafletExport.WriteLine(const S: string); inline;
begin
  WriteLn(fp, S);
end;
procedure TLeafletExport.WrtLinFmt(const Fmt: String; const Args: array of const);
begin
  WriteLn(fp, Format(Fmt, Args));
end;
procedure TLeafletExport.JSSeparateur(const QMotif: Char = '='; const nb: integer = 80);
begin
  WriteLine('//' + StringOfChar(QMotif, nb));
end;

procedure TLeafletExport.BeginJSFunction(const QProcName: string; const QParams: string = '');
begin
  FCurrentProcName := QProcName;
  WrtLinFmt('// BeginJSFunction: *** ----- %s -- %s', [FCurrentClassName, FCurrentProcName]);
  WrtLinFmt('function %s(%s)', [FCurrentProcName, QParams]);
  WriteLine(      '{');
end;
procedure TLeafletExport.EndJSFunction();
begin
  WrtLinFmt('} // EndJSFunction %s ', [FCurrentProcName]);
  JSSeparateur('-', 60);
end;
procedure TLeafletExport.BeginClassJSFunction(const QClassName, QMethod: string; const QParams: string = '');
begin
  FCurrentClassName:= QClassName;
  FCurrentProcName := QMethod;
  WrtLinFmt('// BeginClassJSFunction: *** ----- %s -- %s', [FCurrentClassName, FCurrentProcName]);
  WrtLinFmt('  %s.%s = function(%s)', [FCurrentClassName, FCurrentProcName, QParams]);
  WriteLine(      '   {');
end;
procedure TLeafletExport.EndClassJSFunction();
begin
  WrtLinFmt('  } // EndClassJSFunction // %s.%s', [FCurrentClassName, FCurrentProcName]);
  JSSeparateur('-', 60);
end;
procedure TLeafletExport.BeginJSEventListener(const EventName: string; const Desc: string = '');
begin
  WrtLinFmt('  // Listener: %s - %s', [EventName, Desc]);
  WrtLinFmt('  %s.addEventListener("%s", function(e) {', ['window', EventName]);
end;
procedure TLeafletExport.EndJSEventListener();
begin
  WriteLine('  });');
  JSSeparateur('-', 60);
end;
//
procedure TLeafletExport.jsFOR(const QVarCounter, QVarNb: string; const QStart: integer = 0; const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%sfor (var %s = %d; %s < %s; %s++) // -- FOR %s', [WU, QVarCounter, QStart, QVarCounter, QVarNb, QVarCounter, QTag]);
  WrtLinFmt('%s{', [WU]);
end;
procedure TLeafletExport.jsNEXT(const QVarCounter: string; const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s} // -- NEXT %s ... %s', [WU, QVarCounter, QTag]);
end;
procedure TLeafletExport.jsWHILE(const QCondition: string; const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%swhile (%s) // -- WHILE %s', [WU, QCondition, QTag]);
  WrtLinFmt('%s{', [WU]);
end;
procedure TLeafletExport.jsWEND(const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s} // -- WEND %s', [WU, QTag]);
end;
procedure TLeafletExport.jsIF(const QCondition: string; const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%sif (%s) // %s', [WU, QCondition, QTag]);
  WrtLinFmt('%s{', [WU]);
end;
procedure TLeafletExport.jsELSE(const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s} else { // %s', [WU, QTag]);
end;
procedure TLeafletExport.jsENDIF(const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s} // %s', [WU, QTag]);
end;

procedure TLeafletExport.jsSELECT_CASE(const QSelecteur: integer; const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%sswitch(%d) { // %s', [WU, QTag]);
end;
procedure TLeafletExport.jsCASE(const QItem: integer; const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s  case %d: // %s', [WU, QItem, QTag]);
end;
procedure TLeafletExport.jsBREAK(const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s    break;', [WU]);
end;
procedure TLeafletExport.jsEND_SELECT(const QTag: string = ''; const QIndent: integer = 4);
var
  WU: String;
begin
  WU := StringOfChar(' ', QIndent);
  WrtLinFmt('%s} // switch %s', [WU, QTag]);
end;





//******************************************************************************


procedure TLeafletExport.AddLayer(const L: string; const Desc: string);
begin
  FListeLayers.Add(L + '|' + Desc);
end;

procedure TLeafletExport.setCurrentLayer(const Idx: integer);
var
  EWE: TStringArray;
begin
  EWE := FListeLayers[Idx].Split('|');
  FCurrentLayerName        := Trim(EWE[0]);
  FCurrentLayerDescription := Trim(EWE[1]);
end;



function TLeafletExport.Initialiser(const QFilename: string; const QDocTitle: string; const QMapWidth, QMapHeight: integer; const QCentroideLon, QCentroideLat: Double): boolean;
var
  i: Integer;
begin
  FCurrentIndentation := 0;
  Result := false;
  FCurrentClassName:= '';
  FCurrentProcName := '';
  FLeafletVarNameDispEntrances   := 'DoDisplayEntrances';
  FLeafletVarNameDispCenterlines := 'DoDisplayCenterlines';
  FLeafletVarNameDispStations    := 'DoDisplayStations';
  FLeafletVarNameDispSymbols     := 'DoDisplaySymbols';

  FDocTitle        := QDocTitle;
  FMapWidth        := QMapWidth;
  FMapHeight       := QMapHeight;
  FCentroideLat    := QCentroideLat;
  FCentroideLon    := QCentroideLon;
  FListeLayers := TStringList.Create;
  AssignFile(FP, QFilename);
  try
    ReWrite(fp);
    FListeLayers.Clear;
    AddLayer(VAR_LAYER_OSM_MAPNIK, 'OSM Mapnik');
    AddLayer(VAR_LAYER_OSM_TOPO,  'OSM TopoMap');
    AddLayer(VAR_LAYER_GOOGLE_SATELLITE, 'Google Satellite');

    result := true;
  except
    Exit(false);
  end;
end;



procedure TLeafletExport.Finaliser();
begin
  try
    CloseFile(FP);
    FListeLayers.Clear;
  finally
    FreeAndNil(FListeLayers);
  end;
end;


function TLeafletExport.getLeafletVarNameDispEntrances(): string;
begin
  result := FLeafletVarNameDispEntrances;
end;

function TLeafletExport.getLeafletVarNameDispCenterlines(): string;
begin
  result := FLeafletVarNameDispCenterlines;
end;

function TLeafletExport.getLeafletVarNameDispStations(): string;
begin
  result := FLeafletVarNameDispStations;
end;

function TLeafletExport.getLeafletVarNameDispSymbols(): string;
begin
  result := FLeafletVarNameDispSymbols;
end;



procedure TLeafletExport.DefineStylePoly(const LineWidth: double; const LineColor, FillColor: TColor; const LineOpacity, FillOpacity: byte);
begin
  FCurrentLineWidth   := LineWidth;
  FCurrentLineColor   := LineColor;
  FCurrentFillColor   := FillColor;
  FCurrentLineOpacity := 1.0 * LineOpacity / 256.0;
  FCurrentFillOpacity := 1.0 * FillOpacity / 256.0;
end;

procedure TLeafletExport.AddPoint(const Lat, Lon, Alt: double; const Name: string; const Description: string);
begin
  WrtLinFmt('          %s(%s, L, %s, %s, %s, "%s", "%s");', [JS_FUNC_CreateMarker, NAMEREF_MAP, FCurrentLayerName, FormatterNombreReel(Lat, 8), FormatterNombreReel(Lon, 8), Name, Description]);
end;

procedure TLeafletExport.AddVertex(const Lat, Lon, Alt: double; const IsLast: boolean);
var
  EWE: String;
begin
  if (IsLast) then EWE := ',' else EWE := ''; // Pas de IIF pour éviter dépendance avec GHTopo
  WrtLinFmt('             [%s, %s]%s', [FormatterNombreReel(Lat, 8), FormatterNombreReel(Lon, 8), EWE]);
end;

procedure TLeafletExport.BeginPolygon(const Name: string; const TagString: string);
begin
  FCurrentTagString  := TagString;
  FCurrentObjectName := Name;
  // function CreateScrap(QMyMap, QL, QNomScrap, QLineColor, QFillColor, QFillOpacity, QListeVertex)
  WrtLinFmt('      %s(%s, L, %s, "%s", ''%s'',  ''%s'', %s, [',
                   [JS_FUNC_CreateScrap,
                    NAMEREF_MAP,
                    FCurrentLayerName,
                    Name,
                    QColorToHTMLColor(FCurrentLineColor),
                    QColorToHTMLColor(FCurrentFillColor),
                    FormatterNombreReel(FCurrentFillOpacity)]);


end;
procedure TLeafletExport.EndPolygon();
begin
  WriteLine('      ]);');
end;
procedure TLeafletExport.BeginPolyLine(const Name: string; const TagString: string);
begin
  FCurrentTagString  := TagString;
  FCurrentObjectName := Name;
  WriteLine('      L.polyline([');
end;
procedure TLeafletExport.EndPolyLine();
var
  EWE: String;
begin
  EWE := FCurrentObjectName;
  WriteLine('       ], {');
  WrtLinFmt('          color:''%s'','      , [QColorToHTMLColor(FCurrentLineColor)]);
  WrtLinFmt('          weight:%s,'       , [FormatterNombreReel(FCurrentLineWidth)]);
  WrtLinFmt('          opacity:%s,'      , [FormatterNombreReel(FCurrentLineOpacity)]);
  WriteLine('        }');
  WrtLinFmt('      ).addTo(%s);', [FCurrentLayerName]);         //MAP_NAMEREF
end;
procedure TLeafletExport.WriteHeader();
const
  FMT001 = '       ''%s'': %s%s';
  VAR_baseLayers = 'baseLayers';
  VAR_overlays   = 'overlays';
  LARGEUR_PAGE_IN_POURCENT         = 100;
  LARGEUR_MENU_LATERAL_IN_POURCENT = 15;
  HAUTEUR_TITRE_IN_PIXELS = 36;
var
  URL_OSM_Contributors, WU: String;
  i: Integer;
  EWE: TStringArray;
begin
  try
    WriteLine('<!DOCTYPE html>');
    WrtLinFmt('<!-- Generated %s -->', [DateTimeToStr(Now)]);
    WriteLine('<HTML>');
    WriteLine('<HEAD>');
    WrtLinFmt('  <TITLE>%s</TITLE>', ['Report sur OpenStreetMap: ' + FDocTitle]);
    WrtLinFmt('  <META charset="%s" />', ['utf-8']);
    WrtLinFmt('  <META name="%s" content="width=%s, initial-scale=%s">', ['viewport', 'device-width', FormatterNombreReel(1.0)]);
    // Feuilles de Styles CSS
    WrtLinFmt('  <link rel="%s" href="%s" />', ['stylesheet', URL_LEAFLET_CSS]);
    WriteLine('<!-- Styles -->');
    WriteLine('<STYLE>');
      WrtLinFmt('  html, body { margin:%dpx; padding:%d; }', [0, 0]);
      // CSS de la barre de titre
      WrtLinFmt('       #%s{', [NAMEREF_TITRE]);  // titre
        WrtLinFmt('         %s:%s;'     , ['color', QColorToHTMLColor(clWhite)]);
        WrtLinFmt('         %s:%dpx %s;', ['font', 28, 'Arial']);
        WrtLinFmt('         %s:%s;'     , ['font-weight', 'bold']);
        WrtLinFmt('         %s:%dpx;'   , ['height', HAUTEUR_TITRE_IN_PIXELS]);
        WrtLinFmt('         %s:%s;'     , ['background', QColorToHTMLColor(clMaroon)]);
      WriteLine('       }');
      // CSS du menu latéral (inutilisé)
      (*
      WrtLinFmt('       #%s{', [NAMEREF_MENU_LATERAL]);
        WrtLinFmt('         %s:%s;'     , ['float', 'right']);
        WrtLinFmt('         %s:%s;'     , ['color', QColorToHTMLColor(clBlack)]);
        WrtLinFmt('         %s:%dpx %s;', ['font', 15, 'Arial']);
        WrtLinFmt('         %s:%s;'     , ['font-weight', 'bold']);
        WrtLinFmt('         %s:%s;'     , ['background', QColorToHTMLColor(clAqua)]);
        WrtLinFmt('         %s:%s;'     , ['border-style', 'none']);
        WrtLinFmt('         %s:%d%%;'   , ['width', LARGEUR_MENU_LATERAL_IN_POURCENT]);
        WrtLinFmt('         %s:%dpx;'   , ['height', FMapHeight - HAUTEUR_TITRE_IN_PIXELS]);
      WriteLine('       }');
      //*)
      // CSS de la carte
      WrtLinFmt('       #%s{', [NAMEREF_MAP]);
        WrtLinFmt('         %s:%s;', ['float', 'left']);
        //WrtLinFmt('         %s:%d%%;', ['width', LARGEUR_PAGE_IN_POURCENT - LARGEUR_MENU_LATERAL_IN_POURCENT]);
        WrtLinFmt('         %s:%d%%;', ['width', LARGEUR_PAGE_IN_POURCENT]);
        WrtLinFmt('         %s:%dpx;', ['height', FMapHeight - HAUTEUR_TITRE_IN_PIXELS]);
        WrtLinFmt('         %s:%dpx;'     , ['border', 0]);
        WrtLinFmt('         %s:%dpx;'     , ['margin', 0]);
      WriteLine('       }');
    WriteLine('</STYLE>');
    // Scripts Javascript
    WriteLine('<!-- Scripts JS -->');
    WrtLinFmt('  <script type="%s" src="%s"></script>', [MIME_TEXT_JAVASCRIPT, URL_LEAFLET_JS]);
    WrtLinFmt('  <script type="%s">', [MIME_TEXT_JAVASCRIPT]);
    // création de scraps
    BeginJSFunction(JS_FUNC_CreateScrap, 'QMyMap, QL, QLayer, QNomScrap, QLineColor, QFillColor, QFillOpacity, QListeVertex');

      WriteLine('      QL.polygon(QListeVertex, {');
      WriteLine('        color: QLineColor,');
      WriteLine('        weight: 1.00,');
      WriteLine('        opacity: 1.00,');
      WriteLine('        fillcolor: QFillColor,');
      WriteLine('        fillopacity: QFillOpacity');
      WriteLine('      }).bindPopup(QNomScrap).addTo(QLayer);');
	  EndJSFunction();
    // création de marqueurs
    WriteLine('    // Creation de marqueur');
    BeginJSFunction(JS_FUNC_CreateMarker, 'QMyMap, QL, QLayer, QLat, QLon, QCaption, QDesc');
      WriteLine('      QL.marker([QLat, QLon]).bindPopup(QCaption).addTo(QLayer);');
    EndJSFunction();
    // création de points topo (très lent)
    BeginJSFunction(JS_FUNC_CreatePoint, 'QMyMap, QL, QLayer, QTaille, QLat, QLon, QAlt, QCaption, QDesc');
       writeLine('        QL.circle([QLat, QLon], QTaille, {fillColor:''#FF0000'', color:''#FF0000'', weight:1.0, fillopacity:1.0}).addTo(QLayer);');
    EndJSFunction();
    // création de segments de centerlines (très lent)
    BeginJSFunction(JS_FUNC_CreateSegment, 'QMyMap, QL, QLayer, QLineColor, QLat1, QLon1, QLat2, QLon2');
      WriteLine('      QL.polyline([[QLat1, QLon1], [QLat2, QLon2]], {');
      WriteLine('        color:QLineColor,');
      WriteLine('        weight:1.00,');
      WriteLine('        opacity:1.00');
      WriteLine('      }).addTo(QLayer);');
    EndJSFunction();
    // Fonction JS d'initialisation
    WriteLine('    // Initialisation');
    BeginJSFunction(JS_FUNCTION_INITIALISER, '');

      WrtLinFmt('     var %s = %s;', [FLeafletVarNameDispEntrances  , 'true']);
      WrtLinFmt('     var %s = %s;', [FLeafletVarNameDispCenterlines, 'true']);
      WrtLinFmt('     var %s = %s;', [FLeafletVarNameDispStations   , 'true']);
      WrtLinFmt('     var %s = %s;', [FLeafletVarNameDispSymbols    , 'true']);

      WrtLinFmt('     var %s = L.map(''%s'').setView([%s, %s], %d);',
                            [NAMEREF_MAP, NAMEREF_MAP,
                             FormatterNombreReel(FCentroideLon, 8), FormatterNombreReel(FCentroideLat, 8),
                             15]); // OK
      // layer OSM Mapnik
      WrtLinFmt('     // %s', ['Layers de fonds de cartes']);
      WrtLinFmt('     var %s = L.tileLayer(''%s'',', [VAR_LAYER_OSM_MAPNIK, URL_TILES_OSM_MAPNIK]);  // OK
      WriteLine('       {');
        URL_OSM_Contributors := Format('<a href="%s">%s</a> %s', ['http://osm.org/copyright', 'OpenStreetMap', 'contributors']);
        WrtLinFmt('         attribution: ''&copy; %s'', ', [URL_OSM_Contributors]);
        WrtLinFmt('         maxZoom: %d', [21]);
      WriteLine('       });');

      // layer OSM Topo
      WrtLinFmt('     // %s', ['Layers de fonds de cartes']);
      WrtLinFmt('     var %s = L.tileLayer(''%s'',', [VAR_LAYER_OSM_TOPO, URL_TILES_OSM_TOPO]);  // OK
      WriteLine('       {');
        URL_OSM_Contributors := Format('<a href="%s">%s</a> %s', ['http://osm.org/copyright', 'OpenStreetMap', 'contributors']);
        WrtLinFmt('         attribution: ''&copy; %s'', ', [URL_OSM_Contributors]);
        WrtLinFmt('         maxZoom: %d', [21]);
      WriteLine('       });');
      // layer Google
       WrtLinFmt('     var %s = L.tileLayer(''%s'',', [VAR_LAYER_GOOGLE_SATELLITE, URL_TILES_GOOGLE_SATELLITE]);  // OK
      WriteLine('       {');
        WrtLinFmt('         attribution:''%s'',', ['Google']);
        WrtLinFmt('         maxZoom: %d', [21]);
      WriteLine('       });');
      // et on ajoute les layers de fond de carte
      WrtLinFmt('     %s.addLayer(%s);', [NAMEREF_MAP, VAR_LAYER_OSM_MAPNIK]);
      WrtLinFmt('     %s.addLayer(%s);', [NAMEREF_MAP, VAR_LAYER_OSM_TOPO]);
      WrtLinFmt('     %s.addLayer(%s);', [NAMEREF_MAP, VAR_LAYER_GOOGLE_SATELLITE]);
      WrtLinFmt('     // %s', ['Layers de cavité']);
      for i := NB_LAYERS_FOND_CARTE to FListeLayers.Count - 1 do
      begin
        EWE := FListeLayers[i].Split('|');
        WrtLinFmt('     var %s = new L.LayerGroup();', [Trim(EWE[0])]);
        WrtLinFmt('     %s.addLayer(%s);', [NAMEREF_MAP, Trim(EWE[0])]);
      end;

      // layers de fonds de cartes
      WrtLinFmt('     // %s', ['Layers de fonds de cartes']);
      WrtLinFmt('     var %s = {', [VAR_baseLayers]);
      for i := 0 to NB_LAYERS_FOND_CARTE - 1 do
      begin
        EWE := FListeLayers[i].Split('|');
        WU  := Format('        "%s": %s', [Trim(EWE[1]), Trim(EWE[0])]);
        if (i < (FListeLayers.Count - 1)) then WU := WU + ',';
        WriteLine(WU);
      end;
      WriteLine('     };');
      WrtLinFmt('     // %s', ['Layers de cavité']);
      WrtLinFmt('     var %s = {', [VAR_overlays]);
      for i := NB_LAYERS_FOND_CARTE to FListeLayers.Count - 1 do
      begin
        EWE := FListeLayers[i].Split('|');
        WU  := Format('        "%s": %s', [Trim(EWE[1]), Trim(EWE[0])]);
        if (i < (FListeLayers.Count - 1)) then WU := WU + ',';
        WriteLine(WU);
      end;
      WriteLine('     };');
      WrtLinFmt('     L.control.layers(%s, %s).addTo(%s);', [VAR_baseLayers, VAR_overlays, NAMEREF_MAP]);
      WrtLinFmt('     L.control.scale().addTo(%s); // %s', [NAMEREF_MAP, 'Ajout de l''échelle']); // map.addLayer(osmLayer);
      WriteLine('    //**----- BEGIN DATA -----**');
      Exit;
  except
  end;
end;
procedure TLeafletExport.WriteFooter();  // ok
begin
  WriteLine('        //**----- END OF DATA -----**');
  EndJSFunction(); ///WrtLinFmt('     } // **** End of %s function', [JS_FUNCTION_INITIALISER]);
  JSSeparateur('+', 132);
  WriteLine('  </SCRIPT>');
  WriteLine('</HEAD>');
  WriteLine('<!-- Mise en page du document -->');
  WrtLinFmt('<BODY onLoad="%s()">', [JS_FUNCTION_INITIALISER]);
    WrtLinFmt('  <div id="%s">%s</div>', [NAMEREF_TITRE, 'Report sur carte OSM']);   // Le titre
    WrtLinFmt('  <div id="%s"></div>'  , [NAMEREF_MAP]);                             // La carte
    //WrtLinFmt('  <div id="%s">', [NAMEREF_MENU_LATERAL]);                            // Le menu latéral (inutilisé)
      // Les éléments du menu ici
    WriteLine('</div>');
  WriteLine('</BODY>');
  WriteLine('</HTML>');
end;

procedure TLeafletExport.BeginConditionalSection(const B: boolean; const VariableName: string);
var
  EWE: String;
begin
  if (B) then EWE := VariableName else EWE := '!' + VariableName;
  WrtLinFmt('      if (%s)', [EWE]);
  WriteLine('      {');
end;

procedure TLeafletExport.EndConditionalSection(const VariableName: string);
begin
  WrtLinFmt('      }; // if (%s)', [VariableName]);
end;

procedure TLeafletExport.DrawPoint(const Lat, Lon, Alt: double; const Taille: double; const Caption: string; const Description: string);
begin
  WrtLinFmt('          %s(%s, L, %s, %s, %s, %s, %s, "%s", "%s");',
                              [JS_FUNC_CreatePoint,
                               NAMEREF_MAP, FCurrentLayerName,
                               FormatterNombreReel(Taille, 2),
                               FormatterNombreReel(Lat, 8), FormatterNombreReel(Lon, 8), FormatterNombreReel(Alt, 3),
                               Caption, Description]);
end;

procedure TLeafletExport.DrawSegment(const Lat1, Lon1, Lat2, Lon2: double);
begin
   WrtLinFmt('          %s(%s, L, %s, ''%s'', %s, %s, %s, %s);',
                              [JS_FUNC_CreateSegment,
                               NAMEREF_MAP, FCurrentLayerName,
                               QColorToHTMLColor(FCurrentLineColor),
                               FormatterNombreReel(Lat1, 8), FormatterNombreReel(Lon1, 8),
                               FormatterNombreReel(Lat2, 8), FormatterNombreReel(Lon2, 8)]);
end;


end.

