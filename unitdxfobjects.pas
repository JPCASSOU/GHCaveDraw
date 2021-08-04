unit UnitDXFObjects;

{$mode delphi}

interface

uses
  UnitListesSimplesWithGeneriques,
  Classes, SysUtils;
//////////////////////////////////////////////////////////////////////////////////////////
// constantes
const ACAD_VERSION                   = 'AC1006';
const ACAD_LINESTYLE_CONTINUOUS      = 'CONTINUOUS';
const ACAD_TEXT_STYLENAME_GENERIC    = 'GENERIC';
const ACAD_LAYER_LineType_HIDDEN     = 'HIDDEN';
const ACAD_LAYER_LineType_CENTER     = 'CENTER';
const ACAD_LAYER_LineType_DOT        = 'DOT';
const ACAD_LAYER_LineType_DASHDOT    = 'DASHDOT';
const ACAD_LAYER_LineType_DIVIDE     = 'DIVIDE';
const ACAD_LAYER_LineType_BORDER     = 'BORDER';

const ACAD_DATABASE_VAR_AcDbEntity   = 'AcDbEntity';

//////////////////////////////////////////////////////////////////////////////////////////
// énumérations
type TDxfCommonCode = (
  TEXT1      = 1,
  TEXT2      = 3, {only for MTEXT}
  STYLE_NAME = 7, {Text style}
  X1 = 10,
  X2 = 11,
  X3 = 12,
  X4 = 13,

  Y1 = 20,
  Y2 = 21,
  Y3 = 22,
  Y4 = 23,

  Z1 = 30,
  Z2 = 31,
  Z3 = 32,
  Z4 = 33,
  THICKNESS          = 39,
  SIZE               = 40,       {Radius, Height, ...}
  ANGLE1             = 50,       {start angle, rotation, inclination, ...}
  ANGLE2             = 51,       {end angle}
  FOLLOW_FLAG        = 66,      {only PolyLine}
  OPENED_CLOSED_FLAG = 70,  {PolyLine and LWPolyLine}
  COUNT_VERTICE       = 90);   {only LWPolyLine}


type TDXFColor = (
  DXFByBlock   = 0,
  DXFRed       = 1,
  DXFYellow    = 2,
  DXFGreen     = 3,
  DXFCyan      = 4,
  DXFBlue      = 5,
  DXFMagenta   = 6,
  DXFWhite     = 7,
  DXFGray      = 8,
  DXFBrown     = 15,
  DXFltRed     = 23,
  DXFltGreen   = 121,
  DXFltCyan    = 131,
  DXFltBlue    = 163,
  DXFltMagenta = 221,
  DXFBlack     = 250,
  DXFltGray    = 252,
  DXFByLayer   = 256);


type  TDxfTextStyleCode = (
      PRIMARY_FILE_NAME = 3,
      FIXED_HEIGHT      = 40,
      WIDTH_FACTOR      = 41,
      LAST_HEIGHT_USED  = 42,
      TEXT_ANGLE        = 50,
      STANDARD_FLAG     = 70,
      GENERATION_FLAGS = 71);

type TDxfLineTypeCode = (
      ASCII_LINE_PATTERN      =  3,
      SUM_DASH_ELEMENT_LENGTH = 40,
      DASH_ELEMENT_LENGTH     = 49,
      ALIGNMENT               = 72,
      DASH_ELEMENT_COUNT      = 73);
type  TDxfDimStyleCode = (
      ARROW_SIZE                        = 41,
      OFFSET_EXTENSION_LINE_FROM_ORIGIN = 42,
      EXTENSION_LINE_SIZE_PASSING       = 44,    // Extension line size after Dimension line
      LINE_SIZE_PASSING                 = 46,    // Dimension line size after Extension line
      DIMSTANDARD_FLAGS                 = 70,    //default: 0
      POSITION_TEXT_INSIDE_EXTENSION_LINES = 73,    //position of dimension text inside the extension lines
      VERTICAL_TEXT_LOCATION            = 77,    // Vertical Text Placement
      TEXT_HEIGHT                       = 140,   //Text height
      OFFSET_FROM_DIMENSION_LINE_TO_TEXT  = 147,  // Offset from dimension line to text
      LINE_AND_ARROW_HEAD_COLOR           = 176,  // Dimension line & Arrow heads color
      EXTENSION_LINE_COLOR                = 177,  // Extension line color
      TEXT_COLOR                          = 178);
type TDxfLayerCode = (LINE_TYPE = 6,
                      COLOR     = 62);

type TPolygoneOrPolyline = (tpPOLYGON, tpPOLYLINE);
///////////////////////////////////////////////////////////////////////////////////////////
  // structures de base
type TDXFPoint3Df = record
  X: double;
  Y: double;
  Z: double;
  procedure Empty();
  procedure setFrom(const QX, QY, QZ: double);
  procedure toDXF(var fp: TextFile; const DoSetZAtZero: boolean = false);
end;
type  TArrayOfTDXFPoint3Df = array of TDXFPoint3Df;

type TDXFArrayOfDouble =  record
  M: array of double;
  procedure Empty();
  procedure setValues(const AV: array of double);
  function  getNbValues(): integer;
  function  getValue(const Idx: integer): double;
  procedure setValue(const Idx: integer; const V: double);
  procedure addValue(const V: double);
end;
//////////////////////////////////////////////////////////////////////////////////////////
// styles
type  TDxfTextStyle = record
  TextStyleName           : string;
  FixedHeight             : double;
  WidthFactor             : double;
  GenerationFlags         : integer;
  LastHeightUsed          : double;
  PrimaryFileName         : string;
  TextAngle               : double ;
  DxfCode                 : TDxfTextStyleCode;
  function ToDXF(var fp: TextFile): boolean;
  procedure setFrom(const textStyle, fontFileName: string; const fontHeight: double);
end;

type  TDxfLineType = record
  LineTypeName            : String;
  AsciiLinePatern         : string; // '__ __ __ __') ; '__' {Plus}; ' ' {minus}
  Alignment               : integer;  //"A"
  DashElementCount        : integer;  // Number of linetype elements, ex up: 2
  SUMDashElementLength    : Double; //Sum Abs DASH_ELEMENT_LENGTH: Abs(DashElementLength:=v[0])+ Abs(DashElementLength:=v[1]);
  DashElementLength       : Double;   //repeat patern: DashElementLength:=v[0]; DashElementLength:=v[1]; etc..
  VectorDashElementLength : TDXFArrayOfDouble; //alternate numbers: "+" or "-" where:  '__' :plus  ' ':minus
  DxfCode                 : TDxfLineTypeCode;
  function  ToDXF(var fp: TextFile): boolean;
  procedure setFrom(const typeName: string; const VectDashLen: TDXFArrayOfDouble; const linePattern: string);
end;
type  {TDxfDimStyle}
  TDxfDimStyle = record
	DimStyleName                              : string;                           // DimStyle Name
	DimStandardFlags                          : integer;                      //0
  {DIMCLRD} LineAndArrowHeadColor           : integer;
  {DIMDLE}  LineSizePassing                 : Double;                // Dimension line size after Extensionline
  {DIMCLRE} ExtensionLineColor              : integer;          // Extension line color
  {DIMEXE}  ExtensionLinePassing            : Double;           // Extension line size after Dimension  line
  {DIMEXO}  OffsetExtensionLineFromOrigin   : Double;  // Offset from origin
  {DIMASZ}  ArrowSize                       : Double;
            ArrowWidth                      : Double;
  {DIMCLRT} TextColor                       : integer;
  {DIMTXT}  TextHeight                      : Double;
  {DIMTAD}  VerticalTextLocation            : integer;
  {DIMTIH}  PositionTextInsideExtensionLines: integer;
  {DIMGAP}  OffsetFromDimensionLineToText   : Double;   // Gap/offset from dimension line to text
  DxfCode                                   : TDxfDimStyleCode;
  function  ToDXF(var fp: TextFile): boolean;
  procedure setFrom(styleName: string; const arrwSize, ArrwWidth: double; const color: integer; const txtHeight: double);
end;
//////////////////////////////////////////////////////////////////////////////////////////
// couches
type TDxfLayer = record
  LayerName  : string;
  LineType   : string;  //CONTINUOUS.. or Dashed.. or Hidden ... etc
  Color      : integer;
  DxfCode    : TDxfLayerCode;
  procedure setFrom(const QLayername: string; const QDxfColor: integer; const QLineType: string = ACAD_LINESTYLE_CONTINUOUS);
  function toDXF(var fp: TextFile): boolean;
end;
//////////////////////////////////////////////////////////////////////////////////////////
// Listes couches, styles
type TListeLayers = class(TListeSimple<TDxfLayer>)
  private
  public
end;
type TLineTypeList = class(TListeSimple<TDxfLineType>)
  private
  public
end;
type TTextStyleList = class(TListeSimple<TDxfTextStyle>)
  private
  public
end;
//////////////////////////////////////////////////////////////////////////////////////////
// Entités
type TDxfPoint = record     // point
  Position     : TDXFPoint3Df;
  Thickness    : double;
  DxfLayer     : TDxfLayer;
  DxfCommonCode: TDxfCommonCode;
  function  ToDXF(var fp: TextFile): boolean;
  procedure setFrom(const QDxfLayer: TDxfLayer; const QX1, QY1, QZ1: double; const QThickness: double; const QDxfCommonCode: TDxfCommonCode);
end;

type TDxfLine = record       // segment
  Extr1, Extr2 : TDXFPoint3Df;
  Thickness    : double;
  DxfLayer     : TDxfLayer;
  DxfCommonCode: TDxfCommonCode;
  function  ToDXF(var fp: TextFile): boolean;
  procedure setFrom(const QDxfLayer: TDxfLayer; const QX1, QY1, QZ1, QX2, QY2, QZ2: double; const QThickness: double; const QDxfCommonCode: TDxfCommonCode);
end;

type TDxfPolyline = record   // polyligne TODO: Généraliser en 3D
  Start        : TDXFPoint3Df;
  Thickness    : double;
  DxfLayer     : TDxfLayer;
  DxfCommonCode: TDxfCommonCode;
  FollowFlag   : integer; //set to 1!
  ListeVertex  : TArrayOfTDXFPoint3Df;
  ClosedPoly   : boolean;
  Use3DPoly    : boolean;

  function  ToDXF(var fp: TextFile): boolean;
  procedure setFrom(const QDxfLayer: TDxfLayer;
                    const QCentroid: TDXFPoint3Df;
                    const QClosedPoly: boolean;
                    const QVertexCapacity: integer;
                    const QUse3DPoly: boolean = false); overload;

  procedure setFrom(const QDxfLayer: TDxfLayer;
                    const QCentroid: TDXFPoint3Df;
                    const QThickness: double;
                    const QDxfCommonCode: TDxfCommonCode;
                    const QClosedPoly: boolean;
                    const QVertexCapacity: integer;
                    const QUse3DPoly: boolean = false);
  procedure setVertex(const Idx: integer; const V: TDXFPoint3Df); overload;
  procedure setVertex(const Idx: integer; const QX, QY, QZ: double); overload;
  function  getNbVertex(): integer;
end;

type  TDxfCircle = record         // cercle
  Position     : TDXFPoint3Df;
  Thickness    : double;
  DxfLayer     : TDxfLayer;
  DxfCommonCode: TDxfCommonCode;
  Radius       : double;
  function ToDXF(var fp: TextFile): boolean;
  procedure setFrom(const QDxfLayer: TDxfLayer; const QX1, QY1, QZ1, QRadius: double; const QThickness: double; const QDxfCommonCode: TDxfCommonCode);
end;
// texte
type  TDxfDataText = record     { TDxfDataText }
  Position     : TDXFPoint3Df;
  Thickness    : double;
  Height       : double;
  AngleRot     : double;
  DxfLayer     : TDxfLayer;
  DxfCommonCode: TDxfCommonCode;
  TextStyleName: string;
  Caption      : string;
  procedure setFrom(const QDxfLayer: TDxfLayer;
                    const QX1, QY1, QZ1: double;
                    const QThickness: double;
                    const QDxfCommonCode: TDxfCommonCode;
                    const QTextStyleName: string;
                    const QHeight: double;
                    const QAngleRot: double;
                    const QText: string);
  function ToDXF(var fp: TextFile): boolean;
end;
// TODO: 3D face (triangle 3D)
type

{ TDxf3DFace }

  TDxf3DFace = record     { TDxfDataText }
  DxfLayer     : TDxfLayer;
  vA: TDXFPoint3Df;
  vB: TDXFPoint3Df;
  vC: TDXFPoint3Df;
  procedure setFrom(const QDxfLayer: TDxfLayer; const qA, qB, qC: TDXFPoint3Df); overload;
end;


//////////////////////////////////////////////////////////////////////////////////////////
// utilitaires DXF
procedure WriteDXFIdxKeyIntegerValue(var fp: TextFile; const K: integer; const V: integer);
procedure WriteDXFIdxKeyFloatValue(var fp: TextFile; const K: integer; const V: double; const NbDecimals: integer = 2);
procedure WriteDXFIdxKeyStringValue(var fp: TextFile; const K: integer; const V: string);
procedure WriteDXF_ACAD_DATABASE_VAR(var fp: TextFile; const AcV: string);

implementation
uses
  DGCDummyUnit;
// couple clé-valeur DXF:
// Ligne 1: Clé                  10
// Ligne 2: Valeur                123.456
procedure WriteDXFIdxKeyIntegerValue(var fp: TextFile; const K: integer; const V: integer);
begin
  WriteLn(fp, Format('%d', [K]));
  WriteLn(fp, Format('  %d', [V]));
end;

procedure WriteDXFIdxKeyFloatValue(var fp: TextFile; const K: integer; const V: double; const NbDecimals: integer = 2);
var
  EWE: String;
begin
  WriteLn(fp, Format('%d', [K]));
  EWE := Format(' %%.%df', [NbDecimals]);
  EWE := Format(EWE, [V]);
  EWE := StringReplace(EWE, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
  WriteLn(fp, EWE);
end;

procedure WriteDXFIdxKeyStringValue(var fp: TextFile; const K: integer; const V: string);
begin
  WriteLn(fp, Format('%d', [K]));
  WriteLn(fp, V);
end;

procedure WriteDXF_ACAD_DATABASE_VAR(var fp: TextFile; const AcV: string);
begin
  WriteDXFIdxKeyStringValue (fp, 100, AcV);
end;

{ TDxf3DFace }

procedure TDxf3DFace.setFrom(const QDxfLayer: TDxfLayer; const qA, qB, qC: TDXFPoint3Df);
begin
  self.DxfLayer := QDxfLayer;
  self.vA := qA;
  self.vB := qB;
  self.vC := qC;

end;


//******************************************************************************
{ TDXFPoint3Df }

procedure TDXFPoint3Df.Empty();
begin
  self.setFrom(0.0, 0.0, 0.0);
end;

procedure TDXFPoint3Df.setFrom(const QX, QY, QZ: double);
begin
  self.X := QX;
  self.Y := QY;
  self.Z := QZ;
end;

procedure TDXFPoint3Df.toDXF(var fp: TextFile; const DoSetZAtZero: boolean = false);
begin
  //if (DoSetZAtZero) then Pos
  WriteDXFIdxKeyFloatValue(fp, 10, Self.X, 2);
  WriteDXFIdxKeyFloatValue(fp, 20, Self.Y, 2);
  WriteDXFIdxKeyFloatValue(fp, 30, Self.Z, 2);
end;
//************************************
procedure TDXFArrayOfDouble.Empty();
begin
  SetLength(self.M, 0);
end;

procedure TDXFArrayOfDouble.setValues(const AV: array of double);
var
  i, n: Integer;
begin
  n := length(AV);
  setLength(self.M, n);
  if (n = 0) then exit;
  for i := 0 to n - 1 do self.setValue(i, AV[i]);
end;

function TDXFArrayOfDouble.getNbValues(): integer;
begin
  result := length(self.M);
end;

function TDXFArrayOfDouble.getValue(const Idx: integer): double;
begin
  Result := self.M[Idx];
end;

procedure TDXFArrayOfDouble.setValue(const Idx: integer; const V: double);
begin
  self.M[Idx] := V;
end;

procedure TDXFArrayOfDouble.addValue(const V: double);
var
  n: Integer;
begin
  n := length(self.M);
  setLength(self.M, n+1);
  self.setValue(n, V);
end;
//**********************************************
{ TDxfTextStyle }

function TDxfTextStyle.ToDXF(var fp: TextFile): boolean;
begin
  Result := false;
  try
    WriteDXFIdxKeyStringValue(fp, 0, 'STYLE');
    WriteDXFIdxKeyStringValue(fp, 2, TextStyleName);
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfTextStyleCode.GENERATION_FLAGS), GenerationFlags);
    WriteDXFIdxKeyFloatValue(fp, Ord(TDxfTextStyleCode.FIXED_HEIGHT), FixedHeight, 2);
    WriteDXFIdxKeyFloatValue(fp, Ord(TDxfTextStyleCode.LAST_HEIGHT_USED), LastHeightUsed, 2);
    WriteDXFIdxKeyFloatValue(fp, Ord(TDxfTextStyleCode.TEXT_ANGLE), TextAngle, 2);
    WriteDXFIdxKeyFloatValue(fp, Ord(TDxfTextStyleCode.WIDTH_FACTOR), WidthFactor, 2);
    WriteDXFIdxKeyStringValue(fp, Ord(TDxfTextStyleCode.PRIMARY_FILE_NAME), PrimaryFileName);
    WriteDXFIdxKeyStringValue(fp, 100, 'AcDbSymbolTableRecord');
    WriteDXFIdxKeyStringValue(fp, 100, 'AcDbTextStyleTableRecord');
    WriteDXFIdxKeyStringValue(fp,  70, '0'); // 70 or 64
    Result := True;
  except
    ;;
  end;
end;

procedure TDxfTextStyle.setFrom(const textStyle, fontFileName: string; const fontHeight: double);
begin
  if (textStyle = '') then TextStyleName:= ACAD_TEXT_STYLENAME_GENERIC else TextStyleName:= textStyle;
  self.FixedHeight      := fontHeight;    //no fixed = 0.
  self.WidthFactor      := 1;
  self.GenerationFlags  := 0;
  self.LastHeightUsed   := 1.0;
  self.TextAngle        := 0;
  if (fontFileName = '') then PrimaryFileName:= 'TXT' else PrimaryFileName:= fontFileName;
end;
//************************************************
{ TDxfLineType }

function TDxfLineType.ToDXF(var fp: TextFile): boolean;
var
  i, Nb: Integer;
begin
  Result := false;
  try
    WriteDXFIdxKeyStringValue(fp, 0, 'LTYPE');
    WriteDXFIdxKeyStringValue(fp, 2, 'LineTypeName');
    WriteDXFIdxKeyStringValue(fp, 100, 'AcDbSymbolTableRecord');
    WriteDXFIdxKeyStringValue(fp, 100, 'AcDbLinetypeTableRecord');
    WriteDXFIdxKeyStringValue(fp, Ord(TDxfLineTypeCode.ASCII_LINE_PATTERN), 'AsciiLinePatern');
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfLineTypeCode.ALIGNMENT), Alignment);
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfLineTypeCode.DASH_ELEMENT_COUNT), DashElementCount);
    WriteDXFIdxKeyFloatValue(fp, Ord(TDxfLineTypeCode.SUM_DASH_ELEMENT_LENGTH), SUMDashElementLength, 3);
    Nb:= VectorDashElementLength.getNbValues();
    if (Nb > 0) then
    begin
      for i := 0 to Nb - 1 do  WriteDXFIdxKeyFloatValue(fp, Ord(TDxfLineTypeCode.DASH_ELEMENT_LENGTH), VectorDashElementLength.getValue(i), 3);
    end;
    WriteDXFIdxKeyStringValue(fp, 70, '0');
    Result := True;
  except
    ;;
  end;
end;

procedure TDxfLineType.setFrom(const typeName: string; const VectDashLen: TDXFArrayOfDouble; const linePattern: string);
var
  i: Integer;
begin
  self.LineTypeName:= typeName;
  self.AsciiLinePatern:= linePattern;
  self.Alignment:= 65; //'A'
  self.SUMDashElementLength := 0;
  //if an empty array is passed, then High(..) returns -1
  self.DashElementCount:= VectDashLen.getNbValues();
  self.VectorDashElementLength := VectDashLen;
end;
//*****************************************
function TDxfDimStyle.ToDXF(var fp: TextFile): boolean;
var
  i: Integer;
begin
  Result := false;
  try
    WriteDXFIdxKeyStringValue(fp,   0, 'DIMSTYLE');
    WriteDXFIdxKeyStringValue(fp, 100, 'AcDbSymbolTableRecord');
    WriteDXFIdxKeyStringValue(fp, 100, 'AcDbDimStyleTableRecord');
    WriteDXFIdxKeyStringValue(fp,   2, DimStyleName);
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfDimStyleCode.DIMSTANDARD_FLAGS)                 , DimStandardFlags);
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfDimStyleCode.VERTICAL_TEXT_LOCATION)            , VerticalTextLocation);
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfDimStyleCode.LINE_AND_ARROW_HEAD_COLOR)         , LineAndArrowHeadColor);
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfDimStyleCode.EXTENSION_LINE_COLOR)              , ExtensionLineColor);
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfDimStyleCode.TEXT_COLOR)                        , TextColor);
    WriteDXFIdxKeyFloatValue  (fp, Ord(TDxfDimStyleCode.ARROW_SIZE)                        , ArrowSize);
    WriteDXFIdxKeyFloatValue  (fp, Ord(TDxfDimStyleCode.LINE_SIZE_PASSING)                 , LineSizePassing);
    WriteDXFIdxKeyFloatValue  (fp, Ord(TDxfDimStyleCode.EXTENSION_LINE_SIZE_PASSING)       , ExtensionLinePassing);
    WriteDXFIdxKeyFloatValue  (fp, Ord(TDxfDimStyleCode.OFFSET_EXTENSION_LINE_FROM_ORIGIN) , OffsetExtensionLineFromOrigin);
    WriteDXFIdxKeyFloatValue  (fp, Ord(TDxfDimStyleCode.OFFSET_FROM_DIMENSION_LINE_TO_TEXT), OffsetFromDimensionLineToText);
    WriteDXFIdxKeyFloatValue  (fp, Ord(TDxfDimStyleCode.TEXT_HEIGHT)                       , TextHeight);
    //autocad need!
    for i := 3 to 7 do WriteDXFIdxKeyStringValue(fp, i, '');
    WriteDXFIdxKeyFloatValue(fp,  40,  1.00); // DIMSCALE
    WriteDXFIdxKeyFloatValue(fp,  43,  0.38); // DIMDLI {metric} {0.38 = imperial}
    WriteDXFIdxKeyFloatValue(fp,  45,  0.00); // DIMRND
    WriteDXFIdxKeyFloatValue(fp,  47,  0.00); // DIMTP
    WriteDXFIdxKeyFloatValue(fp,  48,  0.00); // DIMTM
    WriteDXFIdxKeyFloatValue(fp, 141,  0.09); // DIMCEN
    WriteDXFIdxKeyFloatValue(fp, 142,  0.00); // DIMTSZ
    WriteDXFIdxKeyFloatValue(fp, 143, 25.40); // DIMALTF
    WriteDXFIdxKeyFloatValue(fp, 144,  1.00); // DIMLFAC
    WriteDXFIdxKeyFloatValue(fp, 145,  0.00); // DIMTVP
    WriteDXFIdxKeyFloatValue(fp, 146,  1.00); // DIMTFAC
    WriteDXFIdxKeyIntegerValue(fp, 71 , 0); //   WriteLn(fp, '71');    WriteLn(fp, '0');
    WriteDXFIdxKeyIntegerValue(fp, 72 , 0); //   WriteLn(fp, '72');     WriteLn(fp, '0');
    WriteDXFIdxKeyIntegerValue(fp, 73 , 1); //    WriteLn(fp, '73');    WriteLn(fp, '1');
    WriteDXFIdxKeyIntegerValue(fp, 74 , 1); //      WriteLn(fp, '74');   WriteLn(fp, '1');
    WriteDXFIdxKeyIntegerValue(fp, 75 , 0); //     WriteLn(fp, '75');        WriteLn(fp, '0');
    WriteDXFIdxKeyIntegerValue(fp, 76 , 0); //     WriteLn(fp, '76');    WriteLn(fp, '0');
    WriteDXFIdxKeyIntegerValue(fp, 78 , 0); //     WriteLn(fp, '78');      WriteLn(fp, '0');
    WriteDXFIdxKeyIntegerValue(fp,170 , 0); //      WriteLn(fp, '170');     WriteLn(fp, '0');
    WriteDXFIdxKeyIntegerValue(fp,171 , 2); //      WriteLn(fp, '171');   WriteLn(fp, '2');
    WriteDXFIdxKeyIntegerValue(fp,172 , 0); //       WriteLn(fp, '172');  WriteLn(fp, '0');
    WriteDXFIdxKeyIntegerValue(fp,173 , 0); //     WriteLn(fp, '173');   WriteLn(fp, '0');
    WriteDXFIdxKeyIntegerValue(fp,174 , 0); //     WriteLn(fp, '174');     WriteLn(fp, '0');
    WriteDXFIdxKeyIntegerValue(fp,175 , 0); //    WriteLn(fp, '175');     WriteLn(fp, '0');
    Result := True;
  except
    ;;
  end;
end;

procedure TDxfDimStyle.setFrom(styleName: string; const arrwSize, ArrwWidth: double; const color: integer; const txtHeight: double);
begin
  DimStandardFlags:= 0;
  if (styleName = '') or (Pos(styleName, ACAD_TEXT_STYLENAME_GENERIC) > 0 )  then
  begin
    DimStyleName:= ACAD_TEXT_STYLENAME_GENERIC;
    {DIMCLRD} LineAndArrowHeadColor:= 256;
    {DIMDLE}  LineSizePassing:= 0.0; //0:for arrow; OR <>0: for tickes/strok...line size after Extensionline
    {DIMCLRE} ExtensionLineColor:=  256;
    {DIMEXE}  ExtensionLinePassing:= 0.1800;   {or 1.2500 metric}  // Extension line size after Dimension line
    {DIMEXO}  OffsetExtensionLineFromOrigin:= 0.0625;    //distance from extension line from baseline/shape
    {DIMASZ}  ArrowSize:=  0.1800;
    ArrowWidth:= 0.0625;
    {DIMCLRT} TextColor:= 256;
    {DIMTXT}  TextHeight:= 0.1800; {imperial 2.5 = metric}
    {DIMTIH}  PositionTextInsideExtensionLines:= 1; {1= force horizontal  0=metric}
    {DIMTAD}  VerticalTextLocation:= 0; {imperial} {1=metric}  //Place text above the dimension line
    {DIMGAP}  OffsetFromDimensionLineToText:= 0.0900;{0.6250 = metric} {imperial=0.0900};  //Gape from dimension line to text
  end  else
  begin
    DimStyleName:= styleName;
    {DIMCLRD} LineAndArrowHeadColor:= color;
    {DIMDLE}  LineSizePassing:= 0.0;
    {DIMCLRE} ExtensionLineColor:= color;
    {DIMEXE}  ExtensionLinePassing:= 0.1800;
    {DIMEXO}  OffsetExtensionLineFromOrigin:= 0.0625;
    {DIMASZ}  ArrowSize:=  arrwSize;
    ArrowWidth:= arrwWidth;
    {DIMCLRT} TextColor:= color;
    {DIMTXT}  TextHeight:= txtHeight;
    {DIMTIH}  PositionTextInsideExtensionLines:= 1; {1= force horizontal}
    {DIMTAD}  VerticalTextLocation:= 0;
    {DIMGAP}  OffsetFromDimensionLineToText:= 0.0900; {0.6250 or 0.0900};
  end;
end;
//*********************************************
{ TDxfLayer }
procedure TDxfLayer.setFrom(const QLayername: string; const QDxfColor: integer; const QLineType: string);
begin
   LayerName  := QLayername;
   Color      := QDxfColor;
   LineType   := QLineType;
end;

function TDxfLayer.toDXF(var fp: TextFile): boolean;
begin
  result := false;
  try
    WriteDXFIdxKeyStringValue(fp,   0, 'LAYER');
    WriteDXF_ACAD_DATABASE_VAR(fp, 'AcDbSymbolTableRecord'); //    WriteDXFIdxKeyStringValue(fp, 100, 'AcDbSymbolTableRecord');
    WriteDXF_ACAD_DATABASE_VAR(fp, 'AcDbLayerTableRecord');  //    WriteDXFIdxKeyStringValue(fp, 100, 'AcDbLayerTableRecord');
    WriteDXFIdxKeyStringValue(fp,   2, self.LayerName);
    WriteDXFIdxKeyIntegerValue(fp, 70, 0);
    WriteDXFIdxKeyIntegerValue(fp, 62, self.Color);
    WriteDXFIdxKeyStringValue(fp,   6, self.LineType);
    result := True;
  except
    ;;
  end;
end;
//******************************************************************************
// Entités
function TDxfPoint.ToDXF(var fp: TextFile): boolean;
begin
  Result := false;
  try
    WriteDXFIdxKeyStringValue (fp,   0, 'POINT');
    WriteDXF_ACAD_DATABASE_VAR(fp, ACAD_DATABASE_VAR_AcDbEntity); //   WriteDXFIdxKeyStringValue (fp, 100, ACAD_DATABASE_VAR_AcDbEntity);
    WriteDXFIdxKeyStringValue (fp,   8, 'DxfLayer.LayerName');
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfLayerCode.COLOR)         , DxfLayer.Color);              // 62
    WriteDXFIdxKeyStringValue (fp , Ord(TDxfLayerCode.LINE_TYPE)     , DxfLayer.LineType);
    WriteDXFIdxKeyFloatValue (fp  , Ord(TDxfCommonCode.THICKNESS)    , Thickness);
    WriteDXF_ACAD_DATABASE_VAR(fp, 'AcDbPoint'); //WriteDXFIdxKeyStringValue(fp, 100, 'AcDbPoint');
    self.Position.toDXF(fp);
    Result:= True;
  except
    ;;
  end;
end;

procedure TDxfPoint.setFrom(const QDxfLayer: TDxfLayer; const QX1, QY1, QZ1: double; const QThickness: double; const QDxfCommonCode: TDxfCommonCode);
begin
  self.Position.setFrom(QX1, QY1, QZ1);
  self.Thickness  := QThickness;
  self.DxfLayer   := QDxfLayer;
  self.DxfCommonCode := QDxfCommonCode;
  self.DxfLayer.LineType   := 'BYLAYER';
end;
//*************************************
function TDxfLine.ToDXF(var fp: TextFile): boolean;
begin
  Result := false;
  try
    WriteDXFIdxKeyStringValue(fp,   0, 'LINE');
    WriteDXF_ACAD_DATABASE_VAR(fp, ACAD_DATABASE_VAR_AcDbEntity); //   WriteDXFIdxKeyStringValue (fp, 100, ACAD_DATABASE_VAR_AcDbEntity);
    WriteDXFIdxKeyStringValue(fp,   8, self.DxfLayer.LayerName);
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfLayerCode.COLOR), DxfLayer.Color);
    WriteDXFIdxKeyStringValue(fp, Ord(TDxfLayerCode.LINE_TYPE), DxfLayer.LineType);
    WriteDXFIdxKeyFloatValue(fp, Ord(TDxfCommonCode.THICKNESS), Thickness);
    WriteDXF_ACAD_DATABASE_VAR(fp, 'AcDbLine'); //WriteDXFIdxKeyStringValue(fp, 100, 'AcDbLine');
    Extr1.toDXF(fp);
    Extr2.toDXF(fp);
    Result := True;
  except
    ;;
  end;
end;
procedure TDxfLine.setFrom(const QDxfLayer: TDxfLayer; const QX1, QY1, QZ1, QX2, QY2, QZ2: double; const QThickness: double; const QDxfCommonCode: TDxfCommonCode);
begin
  self.Extr1.setFrom(QX1, QY1, QZ1);
  self.Extr2.setFrom(QX2, QY2, QZ2);
  self.Thickness  := QThickness;
  self.DxfLayer   := QDxfLayer;
  self.DxfCommonCode := QDxfCommonCode;
  self.DxfLayer.LineType   := 'BYLAYER';
end;
//*************************************
procedure TDxfPolyline.setFrom(const QDxfLayer: TDxfLayer;
                               const QCentroid: TDXFPoint3Df;
                               const QClosedPoly: boolean;
                               const QVertexCapacity: integer;
                               const QUse3DPoly: boolean = false);
begin
  self.setFrom(QDxfLayer,
               QCentroid,
               0.00,
               TDxfCommonCode.THICKNESS,
               QClosedPoly,
               QVertexCapacity,
               QUse3DPoly);

end;

procedure TDxfPolyline.setFrom(const QDxfLayer: TDxfLayer;
                               const QCentroid: TDXFPoint3Df;
                               const QThickness: double;
                               const QDxfCommonCode: TDxfCommonCode;
                               const QClosedPoly: boolean;
                               const QVertexCapacity: integer;
                               const QUse3DPoly: boolean = false);
begin
  self.Start := QCentroid;

  self.DxfLayer   := QDxfLayer;
  self.Thickness  := QThickness;
  self.FollowFlag := 1;   //set to 1!
  self.DxfCommonCode := QDxfCommonCode;
  self.DxfLayer.LineType   := 'BYLAYER';
  SetLength(self.ListeVertex, QVertexCapacity);
  self.ClosedPoly    := QClosedPoly;
  self.Use3DPoly     := QUse3DPoly;
end;

procedure TDxfPolyline.setVertex(const Idx: integer; const V: TDXFPoint3Df);
begin
  self.ListeVertex[Idx] := V;
end;

procedure TDxfPolyline.setVertex(const Idx: integer; const QX, QY, QZ: double);
var
  V: TDXFPoint3Df;
begin
  V.setFrom(QX, QY, QZ);
  self.setVertex(Idx, V);
end;

function TDxfPolyline.getNbVertex(): integer;
begin
  result := length(self.ListeVertex);
end;
function TDxfPolyline.ToDXF(var fp: TextFile): boolean;
var
  n, i: Integer;
  VX: TDXFPoint3Df;
  QType2D3DPoly, QType2D3DVertex: String;
begin
  Result := false;
  n := self.getNbVertex();
  if (n < 2) then exit; // moins de 2 sommets = poly invalide, on sort
  if (self.Use3DPoly) then QType2D3DPoly   := 'AcDb3dPolyline' else QType2D3DPoly := 'AcDb2dPolyline';
  if (self.Use3DPoly) then QType2D3DVertex := 'AcDb3dVertex'   else QType2D3DPoly := 'AcDb2dVertex';
  try
    WriteDXFIdxKeyStringValue (fp,   0                             , 'POLYLINE');
    WriteDXF_ACAD_DATABASE_VAR(fp, ACAD_DATABASE_VAR_AcDbEntity); //   WriteDXFIdxKeyStringValue (fp, 100, ACAD_DATABASE_VAR_AcDbEntity);
    WriteDXFIdxKeyStringValue (fp,   8                             , DxfLayer.LayerName);
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfLayerCode.COLOR)        , DxfLayer.Color);
    WriteDXFIdxKeyStringValue (fp, Ord(TDxfLayerCode.LINE_TYPE)    , DxfLayer.LineType);
    WriteDXF_ACAD_DATABASE_VAR(fp, QType2D3DPoly); //WriteDXFIdxKeyStringValue (fp, 100, QType2D3DPoly);
    WriteDXFIdxKeyStringValue (fp, Ord(TDxfCommonCode.OPENED_CLOSED_FLAG), BoolToStr(ClosedPoly, '1', '0'));
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfCommonCode.FOLLOW_FLAG) , FollowFlag);

    for i := 0 to n - 1 do
    begin
      VX := ListeVertex[i];
      WriteDXFIdxKeyStringValue (fp,   0, 'VERTEX');
      WriteDXF_ACAD_DATABASE_VAR(fp, ACAD_DATABASE_VAR_AcDbEntity); //   WriteDXFIdxKeyStringValue (fp, 100, ACAD_DATABASE_VAR_AcDbEntity);
      WriteDXFIdxKeyStringValue (fp,   8, DxfLayer.LayerName);
      WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfLayerCode.COLOR)        , DxfLayer.Color);
      WriteDXFIdxKeyStringValue (fp, Ord(TDxfLayerCode.LINE_TYPE)    , DxfLayer.LineType);
      WriteDXFIdxKeyFloatValue  (fp, Ord(TDxfCommonCode.THICKNESS)   , Thickness, 2);
      WriteDXF_ACAD_DATABASE_VAR(fp, 'AcDbVertex'); //WriteDXFIdxKeyStringValue (fp, 100, 'AcDbVertex');
      //WriteDXF_ACAD_DATABASE_VAR(fp, QType2D3DVertex);  //WriteDXFIdxKeyStringValue (fp, 100, QType2D3DVertex);
      VX.toDXF(fp);
    end;
    WriteDXFIdxKeyStringValue(fp, 0, 'SEQEND');
    Result := True;
  except
    ;;
  end;
end;

//************************************************
{ TDxfCircle }
function TDxfCircle.ToDXF(var fp: TextFile): boolean;
begin
  Result := false;
  try
    WriteDXFIdxKeyStringValue(fp,   0, 'CIRCLE');
    WriteDXF_ACAD_DATABASE_VAR(fp, ACAD_DATABASE_VAR_AcDbEntity); // writeDXFIdxKeyStringValue(fp, 100, ACAD_DATABASE_VAR_AcDbEntity);
    WriteDXFIdxKeyStringValue(fp,   8, DxfLayer.LayerName);

    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfLayerCode.COLOR)        , DxfLayer.Color);
    WriteDXFIdxKeyStringValue (fp, Ord(TDxfLayerCode.LINE_TYPE)    , DxfLayer.LineType);
    WriteDXFIdxKeyFloatValue  (fp, Ord(TDxfCommonCode.THICKNESS)   , Thickness);

    WriteDXF_ACAD_DATABASE_VAR(fp, 'AcDbCircle'); //WriteDXFIdxKeyStringValue (fp, 100, 'AcDbCircle');
    Position.toDXF(fp);
    WriteDXFIdxKeyFloatValue  (fp, Ord(TDxfCommonCode.SIZE)        , Radius);
    Result := True;
  except
    ;;
  end;
end;

procedure TDxfCircle.setFrom(const QDxfLayer: TDxfLayer; const QX1, QY1, QZ1, QRadius: double; const QThickness: double; const QDxfCommonCode: TDxfCommonCode);
begin
  self.Position.setFrom(QX1, QY1, QZ1);
  self.Radius     := QRadius;
  self.Thickness  := QThickness;
  self.DxfLayer   := QDxfLayer;
  self.DxfCommonCode := QDxfCommonCode;
  self.DxfLayer.LineType   := 'BYLAYER';
end;
//*************************************************
procedure TDxfDataText.setFrom(const QDxfLayer: TDxfLayer;
                               const QX1, QY1, QZ1: double;
                               const QThickness: double;
                               const QDxfCommonCode: TDxfCommonCode;
                               const QTextStyleName: string;
                               const QHeight: double;
                               const QAngleRot: double;
                               const QText: string);
begin
  self.Position.setFrom(QX1, QY1, QZ1);
  self.Thickness  := QThickness;
  self.DxfLayer   := QDxfLayer;
  self.DxfCommonCode       := QDxfCommonCode;
  self.DxfLayer.LineType   := 'BYLAYER';        // DxfLayer.Color = 256;
  self.TextStyleName := QTextStyleName; // = 'GENERIC'
  self.Height        := QHeight;
  self.AngleRot      := QAngleRot;
  self.Caption       := QText;
end;

function TDxfDataText.ToDXF(var fp: TextFile): boolean;
begin
  Result := false;
  try
    WriteDXFIdxKeyStringValue (fp,   0, 'TEXT');
    WriteDXF_ACAD_DATABASE_VAR(fp, ACAD_DATABASE_VAR_AcDbEntity); // WriteDXFIdxKeyStringValue (fp, 100, ACAD_DATABASE_VAR_AcDbEntity);
    WriteDXFIdxKeyStringValue (fp,   8, DxfLayer.LayerName);
    WriteDXFIdxKeyIntegerValue(fp, Ord(TDxfLayerCode.COLOR)        , DxfLayer.Color);
    WriteDXFIdxKeyStringValue (fp, Ord(TDxfLayerCode.LINE_TYPE)    , DxfLayer.LineType);
    WriteDXF_ACAD_DATABASE_VAR(fp, 'AcDbText'); //WriteDXFIdxKeyStringValue (fp, 100, 'AcDbText');
    Position.toDXF(fp);
    WriteDXFIdxKeyStringValue (fp, Ord(TDxfCommonCode.TEXT1)       , Caption);
    WriteDXFIdxKeyStringValue (fp, Ord(TDxfCommonCode.STYLE_NAME)  , TextStyleName);
    WriteDXFIdxKeyFloatValue  (fp, Ord(TDxfCommonCode.SIZE)        , Height);
    WriteDXFIdxKeyFloatValue  (fp, Ord(TDxfCommonCode.ANGLE1)      , AngleRot);
    Result := True;
  except
    ;;
  end;
end;
end.

