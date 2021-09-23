unit UnitExportDXF2;
// Export DXF
{$mode delphi}

interface

uses
  UnitDXFObjects,
  Classes, SysUtils, Graphics;
type

{ TDXFExport2 }

 TDXFExport2 = class
 strict private
   FpFilename         : TextFile;
   procedure GenererTables();

 private
   FListeLayers       : TListeLayers;
   FLineTypeList      : TLineTypeList;
   FTextStyleList     : TTextStyleList;
   FFileName          : string;
   FDocTitle          : string;
   FLineColor         : TColor;
   FCoinBasGauche     : TDXFPoint3Df;
   FCoinHautDroit     : TDXFPoint3Df;
   procedure  BeginTables();
   procedure  BeginTable(const tabName: string; const NbElements: integer);
   procedure  EndTable();
   procedure  EndTables();
   procedure  BeginBlocksSections();
   procedure  EndBlockSection();
   procedure  BeginEntities();
   procedure  EndEntities();
   // listes des couches

   // liste des types de lignes
   function  GetDxfLineType(const Idx: integer): TDxfLineType;
   function  GetNbDxfLineTypes(): integer;
   // liste des styles de texte
   function  GetDxfTextStyle(const Idx: integer): TDxfTextStyle;
   function  GetNbDxfTextStyles(): integer;
 public
   function  Initialiser(const QFilename: string; const QDocTitle: string;
                         const X1, Y1, Z1, X2, Y2, Z2: double): boolean;
   procedure Finaliser();
   // liste des couches
   procedure AddLayer(const QLayerName: string; const QAcadColorIndex: integer; const QLineType: string = ACAD_LINESTYLE_CONTINUOUS); overload;
   function  GetLayer(const Idx: integer): TDxfLayer;
   function  GetNbLayers(): integer;
   function  GetLayerByName(const QLayerName: string): TDxfLayer;
   // liste des types de lignes
   procedure AddDxfLineType(const typeName: string; const VectDashLen: TDXFArrayOfDouble; const linePattern: string);
   // liste des styles de texte
   procedure AddDxfTextStyle(const textStyle, fontFileName: string; const fontHeight: double);
   //*****************************************
   function  BeginDXF(): boolean;
     procedure Line(const L: TDxfLine);
     procedure Polyline(const P: TDxfPolyline);
     procedure Circle(const C: TDxfCircle);
     procedure Texte(const T: TDxfDataText);
   procedure EndDXF();
end;
 //*****************************************************************************
implementation
uses
  DGCDummyUnit;
function TDXFExport2.Initialiser(const QFilename: string; const QDocTitle: string;
                                 const X1, Y1, Z1, X2, Y2, Z2: double): boolean;
var
  ArrD : TDXFArrayOfDouble;
begin
  result := false;
  FFileName := QFilename;
  FDocTitle := QDocTitle;
  ArrD.Empty();
  FListeLayers       := TListeLayers.Create;
  FLineTypeList      := TLineTypeList.Create;
  FTextStyleList     := TTextStyleList.Create;
  try
    FListeLayers.ClearListe();
    FLineTypeList.ClearListe();
    FTextStyleList.ClearListe();

    FCoinBasGauche.setFrom(X1, Y1, Z1);
    FCoinHautDroit.setFrom(X2, Y2, Z2);
    // couches minimales
    AddLayer('0'              , 7 {white}     , ACAD_LINESTYLE_CONTINUOUS); //must have!
    AddLayer('HIDDEN_YELLOW'  , 2 {2 = yellow}, ACAD_LAYER_LineType_HIDDEN);
    AddLayer('CENTER_RED'     , 1 {red}       , ACAD_LAYER_LineType_CENTER);
    AddLayer('DOT_GREEN'      , 3 {green}     , ACAD_LAYER_LineType_DOT);
    AddLayer('DASHDOT_CYAN'   , 4 {cyan}      , ACAD_LAYER_LineType_DASHDOT);
    AddLayer('DIVIDE_BLUE'    , 5 {blue}      , ACAD_LAYER_LineType_DIVIDE);
    AddLayer('BORDER_MAGENTA' , 6 {magenta}   , ACAD_LAYER_LineType_BORDER);
    AddLayer('CONTINUOUS_GRAY', 8 {GRAY}      , ACAD_LINESTYLE_CONTINUOUS);

    // styles de ligne minimaux
    ArrD.setValues([1.00]);
    AddDxfLineType(ACAD_LINESTYLE_CONTINUOUS    , ArrD, '____________');
    ArrD.setValues([0.25,-0.125]);
    AddDxfLineType(ACAD_LAYER_LineType_HIDDEN   , ArrD, '__ __ __ ');
    ArrD.setValues([1.25,-0.25, 0.25, -0.25]);
    AddDxfLineType(ACAD_LAYER_LineType_CENTER   , ArrD, '____ _ ____ _ ');
    ArrD.setValues([0.0, -0.25]);
    AddDxfLineType(ACAD_LAYER_LineType_DOT      , ArrD, '. . . . . ');
    ArrD.setValues([0.5, -0.25, 0.0, -0.25]);
    AddDxfLineType(ACAD_LAYER_LineType_DASHDOT  , ArrD, '__ . __ . ');
    ArrD.setValues([0.5, -0.25, 0.0, -0.25, 0.0, -0.25]);
    AddDxfLineType(ACAD_LAYER_LineType_DIVIDE   , ArrD, '____ . . ____ . . ');
    ArrD.setValues([0.5, -0.25, 0.5, -0.25, 0.0, -0.25]);
    AddDxfLineType(ACAD_LAYER_LineType_BORDER   , ArrD, '__ __ . __ __ . ');
    // styles de texte minimaux
    AddDxfTextStyle('DEFAULT'   , 'ARIAL.TTF'{'isocpeur.ttf'}, 0.0 {no fixed!});
    AddDxfTextStyle('ISOCPEUR'  , 'isocpeur.ttf', 0.0 {no fixed!});
    result := true;
  except
    ;;
  end;
end;
procedure TDXFExport2.Finaliser();
begin
  try
    FListeLayers.ClearListe();
    FLineTypeList.ClearListe();
    FTextStyleList.ClearListe();
  finally
    FreeAndNil(FListeLayers);
    FreeAndNil(FLineTypeList);
    FreeAndNil(FTextStyleList);
  end;
end;




procedure TDXFExport2.AddLayer(const QLayerName: string; const QAcadColorIndex: integer; const QLineType: string = ACAD_LINESTYLE_CONTINUOUS);
var
  L: TDxfLayer;
begin
  L.setFrom(QLayerName, QAcadColorIndex, QLineType);
  FListeLayers.AddElement(L);
end;

function TDXFExport2.GetLayer(const Idx: integer): TDxfLayer;
begin
  result := FListeLayers.GetElement(Idx);
end;

function TDXFExport2.GetNbLayers(): integer;
begin
  result := FListeLayers.GetNbElements();
end;

function TDXFExport2.GetLayerByName(const QLayerName: string): TDxfLayer;
var
  Nb, i: Integer;
  EWE: String;
begin
  Nb := self.GetNbLayers(); // Nb forcément non nul: des couches ont été créées dans Initialiser()
  EWE := LowerCase(QLayerName);
  for i := 0 to Nb-1 do
  begin
    Result := self.GetLayer(i);
    if (EWE = LowerCase(Result.LayerName)) then exit(Result);
  end;
  Result := self.GetLayer(0);  // sinon retourne la couche 0
end;

procedure TDXFExport2.AddDxfLineType(const typeName: string; const VectDashLen: TDXFArrayOfDouble; const linePattern: string);
var
  LT: TDxfLineType;
begin
  LT.setFrom(typeName, VectDashLen, linePattern);
  FLineTypeList.AddElement(LT);
end;

function TDXFExport2.GetDxfLineType(const Idx: integer): TDxfLineType;
begin
  result := FLineTypeList.GetElement(Idx);
end;

function TDXFExport2.GetNbDxfLineTypes(): integer;
begin
  result := FLineTypeList.GetNbElements();
end;

procedure TDXFExport2.AddDxfTextStyle(const textStyle, fontFileName: string; const fontHeight: double);
var
  TS: TDxfTextStyle;
begin
  TS.setFrom(textStyle, fontFileName, fontHeight);
  FTextStyleList.AddElement(TS);
end;

function TDXFExport2.GetDxfTextStyle(const Idx: integer): TDxfTextStyle;
begin
  result := FTextStyleList.GetElement(Idx);
end;

function TDXFExport2.GetNbDxfTextStyles(): integer;
begin
  result := FTextStyleList.GetNbElements();
end;

procedure TDXFExport2.BeginTables();
begin
  WriteDXFIdxKeyStringValue (FpFilename,   0, 'SECTION');
  WriteDXFIdxKeyStringValue (FpFilename,   2, 'TABLES');
end;

procedure TDXFExport2.EndTables();
begin
  WriteDXFIdxKeyStringValue (FpFilename,   0, 'ENDSEC');
end;

procedure TDXFExport2.BeginBlocksSections();
begin
  WriteDXFIdxKeyStringValue (FpFilename,   0, 'SECTION');
  WriteDXFIdxKeyStringValue (FpFilename,   2, 'BLOCKS');
end;

procedure TDXFExport2.EndBlockSection();
begin
  WriteDXFIdxKeyStringValue (FpFilename,   0, 'ENDSEC');
end;

procedure TDXFExport2.BeginTable(const tabName: string; const NbElements: integer);
var
  acdbTable: string;
begin
  if (CompareText(tabName, 'LTYPE'))    = 0 then acdbTable:= 'AcDbLTypeTable';
  if (CompareText(tabName, 'STYLE'))    = 0 then acdbTable:= 'AcDbStyleTable';
  if (CompareText(tabName, 'LAYER'))    = 0 then acdbTable:= 'AcDbLayerTable';
  if (CompareText(tabName, 'DIMSTYLE')) = 0 then acdbTable:= 'AcDbDimStyleTable';
  if (CompareText(tabName, 'UCS'))      = 0 then acdbTable:= 'AcDbUcsTable';
  WriteDXFIdxKeyStringValue (FpFilename,   0, 'TABLE');
  WriteDXFIdxKeyStringValue (FpFilename,   2, tabName);
  WriteDXFIdxKeyIntegerValue(FpFilename,  70, NbElements);
  WriteDXFIdxKeyStringValue (FpFilename, 100, 'AcDbSymbolTable');
  WriteDXFIdxKeyStringValue (FpFilename, 100, acdbTable);
end;

procedure TDXFExport2.EndTable();
begin
  WriteDXFIdxKeyStringValue (FpFilename,   0, 'ENDTAB');
end;
function TDXFExport2.BeginDXF(): boolean;
var
  PositionPoint0: TDXFPoint3Df;
  //'$DIMASZ' '$DIMTSZ' '$DIMGAP'  '$DIMEXO'  '$DIMDLI' '$DIMDLE' '$DIMEXE' '$DIMTXT'
  procedure WrtAcadVer();
  begin
    WriteDXFIdxKeyStringValue(FpFilename,   9, '$ACADVER');
    WriteDXFIdxKeyStringValue(FpFilename,   1, ACAD_VERSION);
  end;

  procedure WrtDimFloat(const DVAR: string; const DV: double);
  begin
    WriteDXFIdxKeyStringValue(FpFilename,   9, DVAR);
    WriteDXFIdxKeyFloatValue (FpFilename,  40, DV);
  end;
  procedure WrtDimInt(const DVAR: string; const DV: integer);
  begin
    WriteDXFIdxKeyStringValue(FpFilename,   9, DVAR);
    WriteDXFIdxKeyFloatValue (FpFilename,  70, DV);
  end;
  procedure WrtLimVar(const KVAR: string; const QX, QY: double);
  begin
    WriteDXFIdxKeyStringValue(FpFilename,   9, KVAR);
    WriteDXFIdxKeyFloatValue(FpFilename,   10, QX);
    WriteDXFIdxKeyFloatValue(FpFilename,   20, QY);
  end;
  procedure WrtDimAssoc();
  begin
    WriteDXFIdxKeyStringValue(FpFilename,   9, '$DIMASSOC');
    WriteDXFIdxKeyIntegerValue(FpFilename, 280, 0); //2 - associative
     {0 = Creates exploded dimensions; there is no association
     1 = Creates non-associative dimension objects; the elements
         of the dimension are formed into a single object, and if the
         definition point on the object moves, then the dimension
         value is updated
     2 = Creates associative dimension objects; the elements of
         the dimension are formed into a single object and one or
         more definition points of the dimension are coupled with
         association points on geometric objects};
  end;

begin
  result := false;
  AssignFile(FpFilename, FFileName);
  try
    ReWrite(FpFilename);
    WriteDXFIdxKeyStringValue(FpFilename, 999, 'Generator: GHCaveDraw');
    WriteDXFIdxKeyStringValue(FpFilename, 999, FDocTitle);
    WriteDXFIdxKeyStringValue(FpFilename,   0, 'SECTION');
    WriteDXFIdxKeyStringValue(FpFilename,   2, 'HEADER');
    WrtAcadVer();
    WrtDimFloat('$DIMASZ', 0.1800);
    WrtDimFloat('$DIMTSZ', 0.0 );  {DIMTSZ Specifies the size of oblique strokes drawn instead of arrowheads for linear,  radius, and diameter dimensioning  0 = No ticks}
    WrtDimFloat('$DIMGAP', 0.625); {0.6250 = metric} {imperial= 0.0900}
    WrtDimFloat('$DIMEXO', 0.0625);
    WrtDimFloat('$DIMDLI', 0.38);
    WrtDimFloat('$DIMDLE', 0.00);
    WrtDimFloat('$DIMEXE', 0.1800);
    WrtDimFloat('$DIMTXT', 0.1800);
    WrtDimInt('$DIMTXTDIRECTION', 0);
    WrtDimInt('$DIMTIH'         , 0);
    WrtDimInt('$DIMTAD'         , 1); // 1 = Metric
    WrtDimInt('$DIMCLRD'        , 256);
    WrtDimInt('$DIMCLRE'        , 256);
    WrtDimInt('$DIMCLRT'        , 256);
    WrtDimInt('$DIMASO'         , 0);    //0 = Draw individual entities      1 = Create associative dimensioning
    WrtDimAssoc();
    WrtDimInt('$DIMSHO'   , 0);     //1 = Recompute dimensions while dragging ;  0 = Drag original image
    WrtDimInt('$DIMLUNIT' , 2);     // DIMLUNIT 70 Sets units for all dimension types except Angular:   // 1 = Scientific; 2 = Decimal; 3 = Engineering; 4 = Architectural; 5 = Fractional; 6 = Windows desktop}
    WrtDimInt('$DIMDEC'   , 2);     // 2 = metric 4 = Imperial
    WrtDimInt('$DIMADEC'  , 2);
    WriteDXFIdxKeyStringValue(FpFilename,   9, '$INSBASE');
    PositionPoint0.setFrom(0.5 * (FCoinHautDroit.X + FCoinBasGauche.X),
                           0.5 * (FCoinHautDroit.Y + FCoinBasGauche.Y),
                           0.0); // provisoire (non prise en compte des altitudes)
    PositionPoint0.toDXF(FpFilename);
    WrtLimVar('$EXTMIN', FCoinBasGauche.X, FCoinBasGauche.Y);
    WrtLimVar('$EXTMAX', FCoinHautDroit.X, FCoinHautDroit.Y);
    //WrtLimVar('$LINMIN', FCoinBasGauche.X, FCoinBasGauche.Y);
    //WrtLimVar('$LINMAX', FCoinHautDroit.X, FCoinHautDroit.Y);
    WriteDXFIdxKeyStringValue(FpFilename,   0, 'ENDSEC');
    /// Les tables
    GenererTables();
    BeginEntities();
    result := True;
  except
  end;
end;

procedure TDXFExport2.Polyline(const P: TDxfPolyline);
begin
  P.ToDXF(FpFilename);
end;

procedure TDXFExport2.Circle(const C: TDxfCircle);
begin
  C.ToDXF(FpFilename);
end;
procedure TDXFExport2.Line(const L: TDxfLine);
begin
  L.ToDXF(FpFilename);
end;
procedure TDXFExport2.Texte(const T: TDxfDataText);
begin
  T.ToDXF(FpFilename);
end;

procedure TDXFExport2.EndDXF();
begin
  EndEntities();
  try
    WriteDXFIdxKeyStringValue(FpFilename, 0, 'EOF');
  finally
    Closefile(FpFilename);
  end;
end;

procedure TDXFExport2.GenererTables();
var
  i: Integer;
begin
  BeginTables();
    BeginTable('LTYPE', getNbDxfLineTypes());
      for i := 0 to getNbDxfLineTypes() - 1 do getDxfLineType(i).ToDXF(FpFilename);
    EndTable();
    BeginTable('STYLE', getNbDxfTextStyles());  //follow 'count' table
      for i := 0 to getNbDxfTextStyles() - 1 do getDxfTextStyle(i).ToDXF(FpFilename);
    EndTable();
    BeginTable('LAYER', GetNbLayers()); //follow 'count' table
      for i := 0 to  GetNbLayers() - 1 do GetLayer(i).toDXF(FpFilename);
    EndTable();
  EndTables();
end;


procedure TDXFExport2.BeginEntities();
begin
  WriteDXFIdxKeyStringValue(FpFilename, 0, 'SECTION');
  WriteDXFIdxKeyStringValue(FpFilename, 2, 'ENTITIES');
end;

procedure TDXFExport2.EndEntities();
begin
  WriteDXFIdxKeyStringValue(FpFilename, 0, 'ENDSEC');
end;


end.
