unit GeneralFunctions;
{$INCLUDE CompilationParameters.inc}
// 11/04/2015: Portage vers LINUX: Adaptations OK
// 17/06/2015: Nouvelle version de TracerRotatedTexte() qui utilise la
//             propriété Orientation LOL !!!
// 06/08/2015: IntersectRectangles() déboguée.


interface
uses
  GHCD_Types
  {$IFDEF LANGUAGE_FRENCH}
  , UnitMessages_fr
  {$ENDIF}
  , SysUtils
  , dateutils
  , Math
  , types
  , Forms     // pour EnvoyerFenetreVersMoniteur()
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  , Graphics
  , LCLType
  , BGRABitmap
  , BGRABitmapTypes
  , BGRACanvas
  , BGRAPhongTypes
  , BGRAGradients
  ;

// pseudo-instruction vide (équivalente au pass du Python)
// à utiliser pour une meilleure lecture de certains algos
procedure pass; inline;
// affichages de contrôle
procedure AfficherMessage(const Msg: string);
procedure AfficherMessageErreur(const Msg: string);
procedure AfficherMessageErreurBasepointNotFound(const BS: TIDBaseStation);

// envoyer une fenêtre vers un moniteur donné
procedure EnvoyerFenetreVersMoniteur(const MyFenetre: TForm; const NoMoniteur: integer; const X, Y, W, H: integer);
// conversion sécurisée UTF et texte GHCD
function GetResourceString(const RS: string): string;
function _AnsiToLCLStr(const S: string): string; inline;
function _LCLStrToAnsi(const S: string): string; inline;

function GetGHCaveDrawDirectory(): string;
function GetGHCaveDrawApplicationName(): string;
function GetGHCaveDrawVersion(): string; inline;
function GetIconesTexturesDirectory(): string;
function GetGHCaveDrawVersionAndDateBuild(): string;

// affectation rapide de valeurs (pour éviter des affectations multiples ds le code)
//function MakeTPoint2Df(const QX, QY: double): TPoint2Df;
//function MakeTPoint3Df(const QX, QY, QZ: double): TPoint3Df;
function MakeTPoint(const QX, QY: integer): TPoint;
function MakeTPoint2DfWithGroupeAndBaseStation(const BP: TBaseStation; const GP: TGroupeEntites; const OffX, OffY: double): TPoint2Df;
function MakeTPoint2DfWithBaseStationOnly(const BP: TBaseStation; const OffX, OffY: double): TPoint2Df; inline;
function MakeTRect(const Left, Top, Right, Bottom: Integer): TRect;
//function MakeTRect2Df(const QX1, QY1, QX2, QY2: double): TRect2Df;

// ifs immédiats
function IIF(const Condition: boolean; const V1, V2: integer): integer; overload; inline;
function IIF(const Condition: boolean; const V1, V2: Extended): Extended; overload; inline;
function IIF(const Condition: boolean; const V1, V2: boolean): boolean; overload; inline;
function IIF(const Condition: boolean; const V1, V2: String): String; overload; inline;
function IIF(const Condition: boolean; const V1, V2: TColor): TColor; overload; inline;

// extraction de paramètres d'une chaine
function ShortSplit(const Str: string; const Sep: string):TGHShortStringArray;
function Split(const Str: string; const Sep: string):TGHStringArray;
function SplitEx(const Str: string; const Seps: array of Char; const DoRemoveQuotes: boolean): TStringArray;
function ExtractStringsToTStringArray(Separators, WhiteSpace: TSysCharSet; const MyStr: string): TGHStringArray;
function DecoupeChaineEspacesGuillemets(const S: string; const DoRemoveQuotes: boolean): TGHStringArray;
function ChooseString(const Index: integer; const Strs: array of string): string;
function IndexOfString(const S: string; const Strs: array of string; const IsEqual: boolean = false): integer;


// échange de deux valeurs
procedure Swap(var V1, V2: integer); overload;
procedure Swap(var V1, V2: Double); overload;
procedure Swap(var V1, V2: string); overload;

// valeurs dans intervalles
function IsInRange(const Value: Extended; const MinValue, MaxValue: Extended): Boolean; overload; inline;
function IsInRange(const Value: integer; const MinValue, MaxValue: integer): Boolean; overload; inline;

// fonctions de dates
function StrToDateTimeDef(const QDate: string; const DateDefault: TDateTime): TDateTime;
function DateSqlToTDateTime(const S: string): TDateTime;
function DateTimeToDateSql(const QD: TDateTime): string;  // encodage d'une date en format SQL

// fonctions math additionnelles
function Hypot3D(const DX, DY, DZ: Double): Double; inline;
function RealMin(const a, b: double): double; inline;
function RealMax(const a, b: double): double; inline;


procedure GetBearingInc(const dx, dy, dz: double; var Dist, Az, Inc: double; const fUB, fUC: Double); // retourne la longueur, direction et pente pour dx, dy, dz
function  GetAzimut(const dx, dy: Double; const Unite: double): double;                               // retourne un azimut
function  GetAngleBissecteur(const X1, Y1, X2, Y2: double): double;                                   // calcul de l'angle moyen de deux segments, avec la bonne orientation
function  ArcTan2InDegrees(const x, y: double): double;                                               // retourne un angle en degrés dans le bon quadrant et sur [0; 360[
function  ProposerEquidistance(const C1, C2: TPoint3Df): Double;                                      // Suggestion d'une équidistance en fonction de l'etendue totale du réseau

// formatage de nombres
function ensureConvertLongOrLatToXML(const Value: extended): string;                                  // formatage des nombres pour KML et OSM
function ensureConvertAltitudeToXML(const Value: extended): string;
// convertir en nombre des valeurs de type 123 456 789.10 ou 1245-45785
function ConvertirEnNombreReel(const S: string; const Default: double): double;
// échappement des caractères spéciaux pour MySQL et XML
// même utilité que la fonction mysqli_real_escape_string() du PHP -> on en reprend le même nom.
function mysqli_real_escape_string(const S: string): string;
function XML_real_escape_string(const S: string): string;

// fonctions vectorielles
function ProduitVectoriel(const Vect1, Vect2: TPoint3Df; const Normalized: Boolean): TPoint3Df;
function GetConstantVector2D(const Value: double): TPoint2Df; inline;
function GetConstantVector3D(const Value: double): TPoint3Df; inline;

// Tests d'intersections et de points
function IntersectRectangles(const R1, R2: TRect2Df): boolean; inline;
function PointIsInRectangle(const PT: TPoint2Df; const R1: TRect2Df): boolean; inline;
function IsInBoundingBox(const XX, YY: Double; const BB: TBoundingBox): boolean; inline;
function CCW(const P0, P1, P2: TPoint2Df): integer;
function Intersect(const D1, D2: TDroite): boolean;    // function Intersect: intersection de deux droites
function PointDansPolygone(const QX, QY: double; const LstSommets: TArrayPoints2Df): boolean;

// fonctions de couleurs
function KMLColor(const R, G, B, A: byte): string; inline;
function ColorToHTMLColor(const Couleur: TColor): string;
function ColorHTMLToColor(const CouleurHTML: string): TColor;
function GetFloatRValue(const C: TColor): double;
function GetFloatGValue(const C: TColor): double;
function GetFloatBValue(const C: TColor): double;
function Acad2RGB(const n : integer) : tColor; // palette par défaut d'Autocad
function RGB2Acad(const C: TColor) : Byte;
function RGB2HTMLColor(const C: TColor): string;
function GetColorByScale(const V, QMin, QMax: double; const ColDefault: TColor; const AC: array of TColor): TColor;     // échelle de couleurs

// fonctions TOPOROBOT
{$IFDEF TIDBASEPOINT_AS_TEXT}
{$ELSE}
function ExtractNumSerieFromTIDBaseStation(const B: TIDBaseStation): integer;
function SetIDStationFromSerSt(const qSer, qSt: integer): TIDBaseStation;
{$ENDIF TIDBASEPOINT_AS_TEXT}

// fonctions de textures
function CreateSandTexture (tx,ty: integer): TBGRABitmap;    // texture de sable
function CreateStoneTexture(tx,ty: integer): TBGRABitmap;    // texture de pierre
function CreateWaterTexture(tx,ty: integer): TBGRABitmap;    // texture de flotte

function Interp256(const value1,value2,position: integer): integer; inline; overload;
function Interp256(const color1,color2: TBGRAPixel; const position: integer): TBGRAPixel; inline; overload;

// fonction pour les textes GHCD
function InterpreterTexteAnnotation(const STR: string; const NbC: integer; const Station: TBaseStation): string;
function ProposerTextAlignment(const dx, dy: double): byte;
function AttributsFontePas2GHCD(const FA: TFontStyles): integer;
function AttributsFonteGHCD2Pas2(const FA: Integer): TFontStyles;

// description des styles
function GetDescStylePolygone(const SP: TNatureObjetPolygone): string; inline;
function GetDescStyleLigne(const SL: TNatureObjetLigne): string; inline;
function GetDescStyleTexte(const ST: TNatureObjetTexte): string; inline;
function GetDescStylePolyligne(const SC: TNatureObjetCourbe): string; inline;
function GetDescStyleCourbe(const SC: TNatureObjetCourbe): string; inline;
function GetDescNatureSymbole(const SC: TNatureObjetSymbole): string; inline;

function SelectSVGStylePolygone(const SP: TNatureObjetPolygone): string;
function SelectSVGStyleLigne(const SL: TNatureObjetLigne): string;
function SelectSVGStyleTexte(const ST: TNatureObjetTexte): string;
function SelectSVGStyleCourbe(const SC: TNatureObjetCourbe): string;

// calcul d'une courbe de Bézier - Retourne un tableau de points
function CalcBezierCurve(const P0, P1, P2, P3: TPoint2Df; const NbSubdivs: integer): TArrayPoints2Df;

// fonctions issues de TDocumentDessin
function GenererLigneBasepoint(const QIdx: integer; const BP: TBaseStation): string;
function GenererLigneSimpleLigne(const QIdx: integer; const L: TSimpleLigne): string;
function GenererLigneSymbole(const QIdx: integer; const Obj: TSymbole): string;
function GenererLigneTexte(const QIdx: integer; const Obj: TTextObject): string;
function GenererLigneArcCourbe(const QIdx: integer; const A: TArcCourbe): string;
function GenererLigneGroupe(const QIdx: integer; const Grp: TGroupeEntites): string;
function GenererLigneVertexPolygon(const QIdx: integer; const V: TVertexPolygon): string;
function GenererLigneEnteteScrap(const QIdx: integer; const S: TScrap): string;
function GenererLigneEnteteCourbe(const QIdx: integer; const C: TCourbe): string;
function GenererLigneEntetePolyligne(const QIdx: integer; const PL: TPolyLigne): string;
function GenererLigneEntetePolygone(const QIdx: integer; const PG: TPolygone): string;
function GenererLigneEndScrap: string;
function GenererLigneEndCourbe: string;
function GenererLigneEndPolyligne: string;
function GenererLigneEndPolygone: string;

// rotation d'un ensemble de points autour de leur centre
function RotatePoint(const Angle: double; const PT: TPoint2Df): TPoint2Df;
function RotateSetPoints(const Angle: double;
                         const SetOfPoints: TArrayPoints2Df;
                         const QBasePoint: TPoint2Df): TArrayPoints2Df;
function GetOffsetCentrage(const MC: integer; const L, H: double): TPoint2Df;


// fonctions diverses pour tests
function GetLlanfairPG(): string;

// formattage de nombres
function FormatterNombreOOo(const Value: extended; const NbDecs: integer; const IgnoreNulls: boolean): string;
// extraction de la liste des groupes d'une chaine
function StrToArrayOfIdxGroupes(const S: string): TArrayOfIdxGroupes;
function ArrayOfIdxGroupesToStr(const A: TArrayOfIdxGroupes): string;
function AddToArrayOfIdxGroupes(var A: TArrayOfIdxGroupes; const Idx: TIDGroupeEntites): boolean;

function CompareIDStations(const I1, I2: TIDBaseStation): boolean;
// Station valide (= n'est pas une visée radiante)
function IsValidBaseStation(const BS: TBaseStation): boolean;

// distance sur grand cercle entre deux points (Lon, Lat)
function HaversineDist(const Lat1, Lon1, Lat2, Lon2:double):double;
//******************************************************************************
//******************************************************************************
implementation
{$IFDEF OPENCAVEMAP_ADMIN}
uses
  OCMMainWindow;
{$ELSE}
uses
  frmJournal;
{$ENDIF}
procedure AfficherMessage(const Msg: string);
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF OPENCAVEMAP_ADMIN}
    with OCMFrontal.lsbMessages do
    begin
      Items.Add(TimeToStr(Now) + ': ' + Msg);
      if (Count > 500) then Items.Delete(0);
      ItemIndex := Count - 1;
      Refresh;
    end;
    {$ELSE}
    with dlgProcessing.lsbMessages do
    begin
      Items.Add(TimeToStr(Now) + ': ' + Msg);
      if (Count > 500) then Items.Delete(0);
      ItemIndex := Count - 1;
      Refresh;
      Application.ProcessMessages;
    end;
    {$ENDIF}
  {$ENDIF} // {$IFDEF MSWINDOWS}
  {$IFDEF LINUX}
     WriteLn(Msg); // message dans le terminal depuis lequel GHCaveDraw est lancé
  {$ENDIF}
end;

procedure AfficherMessageErreur(const Msg: string);
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF OPENCAVEMAP_ADMIN}
    with OCMFrontal.lsbMessages do
    begin
      Items.Add(TimeToStr(Now) + ': ' + Msg);
      if (Count > 500) then Items.Delete(0);
      ItemIndex := Count - 1;
      Refresh;
    end;
    {$ELSE}
    with dlgProcessing.memoMsgsErreur do
    begin
      Lines.Add(Msg);
      if (Lines.Count > 2000) then Lines.Delete(0);
      Refresh;
    end;
    {$ENDIF} // {$IFDEF MSWINDOWS}
  {$ENDIF}
  {$IFDEF LINUX}
     WriteLn(Msg); // message dans le terminal depuis lequel GHCaveDraw est lancé
  {$ENDIF}
end;

procedure AfficherMessageErreurBasepointNotFound(const BS: TIDBaseStation);
begin
  {$IfDef TIDBASEPOINT_AS_TEXT}
  AfficherMessageErreur(Format('Point %s non trouvé', [BS]));
  {$ELSE}
  AfficherMessageErreur(Format('Point %d non trouvé', [BS]));
  {$ENDIF TIDBASEPOINT_AS_TEXT}
end;

function GetResourceString(const RS: string): string;
begin
  Result := RS; //AnsiToUtf8(RS);
end;

function GetGHCaveDrawDirectory(): string;
begin
  {$IFDEF MSWINDOWS}
  Result := ExtractFilePath(ParamStr(0));
  {$ENDIF}
  {$IFDEF LINUX}
  Result := ExtractFilePath(ParamStr(0));
  {$ENDIF}
end;

// Envoyer une fenêtre vers le moniteur N à la position X, Y:
// Envoie au dernier moniteur si NoMoniteur = -1
procedure EnvoyerFenetreVersMoniteur(const MyFenetre: TForm; const NoMoniteur: integer; const X, Y, W, H: integer);
var
  NbScrs: Integer;
  MyMonitor: TMonitor;
  ZonesEcran: TRect;
  i, Q, BBL: Integer;
begin
  NbScrs := screen.MonitorCount;
  Q := NoMoniteur;
  AfficherMessage(Format('EnvoyerFenetreVersMoniteur: %d (%d moniteurs) - %s', [NoMoniteur, NbScrs, MyFenetre.Caption]));
  try
    if (NbScrs = 0) then Exit;
    if (Q = -1) then Q := NbScrs - 1;
    BBL := 0;
    for i := 0 to Q do
    begin
      MyMonitor := screen.Monitors[i];
      ZonesEcran := MyMonitor.BoundsRect;
      BBL := BBL + ZonesEcran.Left;
    end;
    MyFenetre.Top   := Y;
    MyFenetre.Left  := BBL + X;
    MyFenetre.Width := W;
    MyFenetre.Height:= H;
  except
    pass;
  end;
  //}
end;


// ifs immédiats
function IIF(const Condition: boolean; const V1, V2: integer): integer; overload; inline;
begin
  if (Condition) then Result := V1 else Result := V2;
end;
function IIF(const Condition: boolean; const V1, V2: Extended): Extended; overload; inline;
begin
  if (Condition) then Result := V1 else Result := V2;
end;
function IIF(const Condition: boolean; const V1, V2: boolean): boolean; overload; inline;
begin
  if (Condition) then Result := V1 else Result := V2;
end;
function IIF(const Condition: boolean; const V1, V2: String): String; overload; inline;
begin
  if (Condition) then Result := V1 else Result := V2;
end;
function IIF(const Condition: boolean; const V1, V2: TColor): TColor; overload; inline;
begin
  if (Condition) then Result := V1 else Result := V2;
end;



function Hypot3D(const DX, DY, DZ: Double): Double; inline;
begin
  Result:=Sqrt(dx*dx + dy*dy + dz*dz);
end;

function RealMin(const a, b: double): double; inline;
begin
  if (a < b) then Result := a else Result := b;
end;

function RealMax(const a, b: double): double; inline;
begin
  if (a > b) then Result := a else Result := b;
end;

// valeurs dans intervalles
function IsInRange(const Value: Extended; const MinValue, MaxValue: Extended): Boolean; overload; inline;
begin
  Result := ((Value >= MinValue) and (Value <= MaxValue));
end;

function IsInRange(const Value: integer; const MinValue, MaxValue: integer): Boolean; overload; inline;
begin
  Result := ((Value >= MinValue) and (Value <= MaxValue));
end;
// extraction de paramètres d'une chaine
// récupération de données d'une ligne de texte

function ShortSplit(const Str: string; const Sep: string): TGHShortStringArray;
var
  pn   : integer;
  ps   : integer;
  S    : string;
begin
  for pn:=0 to High(Result) do Result[pn] := '';
  S:=Str;
  ps:=0;
  try
    pn:=0;
    repeat
     if pn>High(Result) then Break;
     ps:=Pos(Sep, S);
     Result[pn]:=Trim(Copy(s,0, ps-1));
     Inc(pn);
     s:=Copy(s, 1+ps, Length(s));
    until ps=0;
    Result[pn-1]:=Trim(s);
  except
  end;
end;
function Split(const Str: string; const Sep: string):TGHStringArray;
var
  pn   : integer;
  ps   : integer;
  S    : string;
begin
  for pn:=0 to High(Result) do Result[pn]:='';
  S:=Str;
  ps:=0;
  try
    pn:=0;
    repeat
     if pn>High(Result) then Break;
     ps:=Pos(Sep, S);
     Result[pn]:=Trim(Copy(s,0, ps-1));
     Inc(pn);
     s:=Copy(s, 1+ps, Length(s));
    until ps=0;
    Result[pn-1]:=Trim(s);
  except
  end;
end;
//*)
function SplitEx(const Str: string; const Seps: array of Char; const DoRemoveQuotes: boolean): TStringArray;
var
  n, i: Integer;
  EWE: String;
begin
  Result := Str.Split(Seps, '"', '"', TStringSplitOptions.ExcludeEmpty);
  n := length(Result);
  if (n = 0) then exit;
  if (DoRemoveQuotes) then
  begin
    for i := 0 to n-1 do
    begin
      EWE := Result[i];
      Result[i] := EWE.DeQuotedString('"');
    end;
  end;
end;

// calcul d'une courbe de Bézier - Retourne un tableau de points
function CalcBezierCurve(const P0, P1, P2, P3: TPoint2Df;
                         const NbSubdivs: integer): TArrayPoints2Df;
var
  i: integer;
  PasT, t : double;
  A, B, C, D, Q1, Q2, Q3, T2, T3: double;

begin
  PasT := 1/NbSubdivs;
  SetLength(Result, 1+NbSubdivs);
  t := 0;
  // coefs
  for i:= 0 to High(Result) do
  begin
    Q1 := 1 - t;
    Q2 := Q1 * Q1;
    Q3 := Q2 * Q1;
    T2 := t * t;
    T3 := T2 * t;
    A := Q3; //Q*Q*Q;
    B := 3 * Q2 * t;
    C := 3 * Q1 * T2; //T * T;
    D := T3; //T * T * T;
    Result[i].X := A * P0.X + B * P1.X + C * P2.X + D * P3.X;
    Result[i].Y := A * P0.Y + B * P1.Y + C * P2.Y + D * P3.Y;
    t += PasT;
  end;
end;


// Suggestion d'une équidistance en fonction de l'etendue totale du réseau
function ProposerEquidistance(const C1, C2: TPoint3Df): Double;
var
  d: double;
begin
  Result:=50.00;
  try
    d := 0.10 * Hypot3D(C2.X - C1.X,
                        C2.Y - C1.Y,
                        C2.Z - C1.Z);

    Result := d;
    if IsInRange(d,    0.00,   10.00) then Result:=10.00;
    if IsInRange(d,   10.00,   25.00) then Result:=25.00;
    if IsInRange(d,   25.00,   50.00) then Result:=50.00;
    if IsInRange(d,   50.00,  100.00) then Result:=100.00;
    if IsInRange(d,  100.00,  200.00) then Result:=200.00;
    if IsInRange(d,  200.00,  250.00) then Result:=250.00;
    if IsInRange(d,  250.00,  500.00) then Result:=500.00;
    if IsInRange(d,  500.00, 1000.00) then Result:=1000.00;
    if IsInRange(d, 1000.00, 2000.00) then Result:=2000.00;
    if IsInRange(d, 2000.00, 5000.00) then Result:=5000.00;
    if IsInRange(d, 5000.00,10000.00) then Result:=10000.00;
    if IsInRange(d,10000.00,20000.00) then Result:=20000.00;
  except
    Result:=50.00;
  end;
end;
// produit vectoriel
function ProduitVectoriel(const Vect1, Vect2: TPoint3Df;
                          const Normalized: Boolean):TPoint3Df;
var
  r: Extended;
begin
  Result.X := Vect1.Y*Vect2.Z-Vect1.Z*Vect2.Y;
  Result.Y := Vect1.Z*Vect2.X-Vect1.X*Vect2.Z;
  Result.Z := Vect1.X*Vect2.Y-Vect1.Y*Vect2.X;
  if (Normalized) then
  begin
    r := sqrt(Sqr(Result.x)+sqr(Result.y)+sqr(Result.z))+1e-12;
    Result.X := Result.x / r;
    Result.y := Result.y / r;
    Result.z := Result.z / r;
  end;
end;
// calcul de l'angle moyen de deux segments, avec la bonne orientation
function GetAngleBissecteur(const X1, Y1, X2, Y2: double): double;
var
  V1, V2, W: TPoint3Df;
begin
  // vecteur V1           vecteur V2        vecteur w
  V1.X:=X1;               V2.X:=X2;         W.X :=0;
  V1.Y:=Y1;               V2.Y:=Y2;         W.Y :=0;
  V1.Z:=0;                V2.Z:=0;          W.Z :=1;
  // produits vectoriels
  v1 := ProduitVectoriel(v1,w,True);
  v2 := ProduitVectoriel(v2,w,True);
  //composition vectorielle
  w.x:=v1.x+v2.X;
  w.y:=v1.y+v2.Y;
  w.z:=v1.z+v2.z;
  // angles
  Result := ArcTan2(w.y+1e-12, w.x+1e-12);
end;

// échange de deux valeurs
procedure Swap(var V1, V2: integer); overload;
var Tmp: integer;
begin
  Tmp:=V1;
  V1:=V2;
  V2:=Tmp;
end;
procedure Swap(var V1, V2: Double); overload;
var Tmp: Double;
begin
  Tmp := V1;
  V1 := V2;
  V2  := Tmp;
end;
procedure Swap(var V1, V2: string); overload;
var Tmp: string;
begin
  Tmp := V1;
  V1  := V2;
  V2  := Tmp;
end;
// détecter si un point est dans une BoundingBox
function IsInBoundingBox(const XX, YY: Double; const BB: TBoundingBox): boolean;
begin
  Result := (IsInRange(XX, BB.C1.X, BB.C2.X) AND IsInRange(YY, BB.C1.Y, BB.C2.Y));
end;

// Interprétation d'une annotation
function InterpreterTexteAnnotation(const STR: string; const NbC: integer; const Station: TBaseStation): string;
var
  ppp : integer;
  Dist, Az, Inc: double;
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  {$ELSE}
  qTRB: TToporobotIDStation;
  {$ENDIF TIDBASEPOINT_AS_TEXT}

  function zdar(const qStr, qCode, qReplace: string): string;
  var
    PPPP: SizeInt;
  begin
    Result := qStr;
    PPPP := Pos(qCode, LowerCase(qStr));
    if (PPPP > 0) then begin
      System.Delete(Result, PPPP, 2);
      System.Insert(qReplace, Result, PPPP);
    end;
  end;
begin
  Result := STR;
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  {$ELSE}
  qTRB := Station.getToporobotIDStation(); // GetToporobotIDStation(Station);
  {$ENDIF TIDBASEPOINT_AS_TEXT}

  // Longueur, azimut, pente
  GetBearingInc(Station.PosStation.X - Station.PosExtr0.X,
                Station.PosStation.Y - Station.PosExtr0.Y,
                Station.PosStation.Z - Station.PosExtr0.Z,
                Dist, Az, Inc,
                360.00, 360.00);
  // ID station

  {$IFDEF TIDBASEPOINT_AS_TEXT}
  Result := zdar(Result, '%s', Station.IDStation);
  {$ELSE}
  Result := zdar(Result, '%s', qTRB.aIDTerrain);
  Result := zdar(Result, '%i', Format('[%d.%d]', [qTRB.aSerie, qTRB.aStation]));
  {$ENDIF TIDBASEPOINT_AS_TEXT}

  // coordonnées
  Result := zdar(Result, '%x', Format('X = %.0n m',[Station.PosStation.X]));
  Result := zdar(Result, '%y', Format('Y = %.0n m',[Station.PosStation.Y]));
  Result := zdar(Result, '%z', Format('Alt %.0n m',[Station.PosStation.Z]));
  // longueur, az, pente
  Result := zdar(Result, '%l', Format('L=%.1fm',[Dist]));
  Result := zdar(Result, '%a', Format('A=%.1fd',[Az]));
  Result := zdar(Result, '%p', Format('P=%.1fd',[Inc]));
  //---------------
  if ((0 = Length(Result)) or (NbC = -1)) then Exit(Result);
  if (Length(STR) > NbC) then Result := Copy(Result, 1, NbC);
end;
// index d'un tableau de chaines
function IndexOfString(const S: string; const Strs: array of string; const IsEqual: boolean = false): integer;
var
  i: integer;
  C: boolean;
begin
  Result := -1;
  for i := Low(Strs) to High(Strs) do
  begin
    if (IsEqual) then C := (LowerCase(S) = LowerCase(Strs[i]))
                 else C := (Pos(S, Strs[i]) > 0);
    if (C) then Exit(i);
  end;
end;
// choisir une chaine en fonction d'une valeur
function ChooseString(const Index: integer; const Strs: array of string): string;
begin
  if (Index < 0) or (Index > High(Strs)) then Exit(Format('** Erroneous index: %d **',[Index]));
  Result := Strs[Index];
end;


{$IFDEF TIDBASEPOINT_AS_TEXT}
{$ELSE}
function SetIDStationFromSerSt(const qSer, qSt: integer): TIDBaseStation;
begin
  Result := qSer * DIV_SR_CENTERLINE + qSt  * 10;
end;
{$ENDIF TIDBASEPOINT_AS_TEXT}
// retourne la longueur, direction et pente pour dx, dy, dz
procedure GetBearingInc(const dx, dy, dz: double;
                        var Dist, Az, Inc: double;
                        const fUB, fUC: Double);
var
  dp: Double;
begin;
  dp  := Hypot(dx, dy);
  Dist:= Hypot(dp,dz);
  Inc := ArcTan2(dz, dp) * 0.5 * fUC / pi;
  Az  := GetAzimut(dx,dy, fUB);
end;
// retourne un azimut
function GetAzimut(const dx, dy: Double; const Unite: double): double;
const TWO_PI = 2 * PI;
var
  a: double;
  procedure MiouMiou();
  begin
    if (a < 0) then a += TWO_PI;
  end;

begin
  a := ArcTan2(dy, dx+1e-12);
  MiouMiou();
  a := 0.50 * PI - a;
  MiouMiou();
  Result := a * 0.50 * Unite / PI;
end;
// intersection de deux rectangles
function IntersectRectangles(const R1, R2: TRect2Df): boolean; inline;
begin
  Result := (R1.X1 <= R2.X2) and (R1.X2 >= R2.X1) and (R1.Y1 <= R2.Y2) and (R1.Y2 >= R2.Y1);
end;
// point dans rectangle
function PointIsInRectangle(const PT: TPoint2Df; const R1: TRect2Df): boolean; inline;
begin
  Result := (IsInRange(PT.X, R1.X1, R1.X2) AND IsInRange(PT.Y, R1.Y1, R1.Y2));
end;

// rotation d'un ensemble de points autour de leur centre
// angle en degrés
function RotatePoint(const Angle: double; const PT: TPoint2Df): TPoint2Df;
var
  ca, sa: double;
begin
  sincos(Angle * PI_180, sa, ca);
  Result.X := ca * PT.X - sa * PT.Y;
  Result.Y := sa * PT.X + ca * PT.Y;
end;
function RotateSetPoints(const Angle: double;
                         const SetOfPoints: TArrayPoints2Df;
                         const QBasePoint: TPoint2Df): TArrayPoints2Df;
var
  ca, sa: double;
  i, n : integer;
  PC: TPoint2Df;
  function QTransform(const PP: TPoint2Df): TPoint2Df;
  var
    PT, PQ: TPoint2Df;
  begin
    PT.setFrom(PP.X - PC.X, PP.Y - PC.Y);                         // réduction au centre
    PQ.setFrom(ca * PT.X - sa * PT.Y, sa * PT.X + ca * PT.Y);     // rotation locale (/!\ ne pas réutiliser la variable PT sinon effet de bord)
    Result.setFrom(PQ.X + PC.X, PQ.Y + PC.Y);                     // et on remet en repère général
  end;
begin
  // dimensionnement du tableau résultat
  n := 1 + High(SetOfPoints);
  SetLength(Result, n);
  PC := QBasePoint;
  sincos(Angle * PI_180, sa, ca);
  for i:= 0 to n - 1 do Result[i] := QTransform(SetOfPoints[i]);
end;


// centrage de la photo
function GetOffsetCentrage(const MC: integer; const L, H: double): TPoint2Df;
begin
  Result.X := 0.00;
  Result.Y := 0.00;
  case MC of
    0,1: ;   // coin bas gauche
    2: begin // coin bas droit
         Result.X := - L;
       end;
    3: begin // coin haut gauche
         Result.Y := - H;
       end;
    4: begin // coin haut droit
         Result.X := - L;
         Result.Y := - H;
       end;
  end;
end;
// vecteurs de mêmes composantes
function GetConstantVector2D(const Value: double): TPoint2Df;
begin
  Result.setFrom(Value, Value);
end;
function GetConstantVector3D(const Value: double): TPoint3Df;
begin
  Result.setFrom(Value, Value, Value);
end;


// routines de couleurs
//****************************************************************************
// fonctions de couleurs
function GetFloatRValue(const C: TColor): double;
begin
  Result := Red(C) / 256;
end;
function GetFloatGValue(const C: TColor): double;
begin
  Result := Green(C) / 256;
end;
function GetFloatBValue(const C: TColor): double;
begin
  Result := Blue(C) / 256;
end;

function RGB2Acad(const C: TColor): Byte;
var drmin,dgmin,dbmin,
    CouleurR,
    CouleurG,
    CouleurB,
    ACcolor,
    ACcolorR,
    ACcolorG,
    ACcolorB,
    dColor,
    res    : integer;
begin
    Result:=0;

    dRmin := 99999999;
    dGmin := 99999999;
    dBmin := 99999999;

    CouleurR := Red(C);
    CouleurG := Green(C);
    CouleurB := Blue(C);
    for res := 1 to 255 do
    begin
      ACcolor := Acad2RGB(res);
      ACcolorR := Red(ACcolor);
      ACcolorG := Green(ACcolor);
      ACcolorB := Blue(ACcolor);
      dColor := abs(ACcolorR-CouleurR)+
                abs(ACcolorG-CouleurG)+
                abs(ACcolorB-CouleurB);
      if (dColor < dRmin) then
      begin
        dRmin := dColor;
        result := res;
      end;
    end;
end;

function Acad2RGB(const n : integer) : tColor; // palette par défaut d'Autocad
var r,g,b,
    c,d,u : integer;
    C1 : Tcolor;  // RGB(r, g ,b)
    const StandCol : array[0..9] of tcolor =
        (clBlack ,clRed,$0001E1F3{clYellow},$0000C800{clLime},$00FDE302{clAqua},
         clBlue,clFuchsia,clBlack,clGray,clLtGray);
    const FullPalete : array[0..23] of tcolor =
        ($0000FF,$0040FF,$0080FF,$00C0FF,
         $00FFFF,$00FFC0,$00FF80,$00FF40,
         $00FF00,$40FF00,$80FF00,$C0FF00,
         $FFFF00,$FFC000,$FF8000,$FF4000,
         $FF0000,$FF0040,$FF0080,$FF00C0,
         $FF00FF,$C000FF,$8000FF,$4000FF);//..$0000FF (retour au début)
begin
    c := n mod 256; // au cas ou ?
    if c<10 then C1 := StandCol[c]
    else begin
        d := ((c-10) div 10);// 0..23
        // Couleur de base à corriger
        C1 :=FullPalete[d mod 24];
        // Correction:--------------------------------
        d := c div 10; // dizaines
        u := c-d*10; // unités
        // séparation des couleurs RGB
        b := (C1 and $FF0000) shr 16;
        g := (C1 and $00FF00) shr 8;
        r := (C1 and $0000FF);
        //Plus clair pour les impairs
        if ((u div 2)*2<>u) then
        begin
          b := b + ((255-b) div 2);
          g := g + ((255-g) div 2);
          r := r + ((255-r) div 2);
        end;
        // Plus foncé si u grand
        b := b*4 div (u+4);
        g := g*4 div (u+4);
        r := r*4 div (u+4);
        // Couleur corrigée:---------------------------
        C1 := RGBToColor(r,g,b);
    end;
    result := C1;
end;

function RGB2HTMLColor(const C: TColor): string;
var
  qr, qg, qb: byte;
begin
  qr := Red(C);
  qg := Green(C);
  qb := Blue(C);
  Result := Format('#%.2X%.2X%.2X',[qr, qg, qb]);

end;
//************************************************************************
// retourne 1 si P0P1 x P0P2 pointe vers le haut
function CCW(const P0, P1, P2: TPoint2Df): integer;
var
  dx1, dx2, dy1, dy2: Double;
  cp1, cp2: double;
begin
  dx1 := P1.x - P0.X;  dy1 := P1.Y - P0.Y;
  dx2 := P2.x - P0.X;  dy2 := P2.Y - P0.Y;

  cp1 := dx1 * dy2;
  cp2 := dy1 * dx2;

  if (CP1 > CP2) then begin Result:=  1; Exit; End;
  if (CP1 < CP2) then begin Result:= -1; Exit; End;
  if ( ((dx1 * dx2) < 0) OR
       ((dy1 * dy2) < 0)
     ) then begin Result := -1; Exit; End;
  if (Sqr(dx1) + Sqr(dy1)) >= (Sqr(dx2) + Sqr(dy2))
       then begin Result := 0; Exit; End;

end;
// function Intersect: intersection de deux droites
function Intersect(const D1, D2: TDroite): boolean;
begin
  Result := (
              (CCW(D1.PT1, D1.PT2, D2.PT1) * CCW(D1.PT1, D1.PT2, D2.PT2) <=0)
            ) AND
            (
              (CCW(D2.PT1, D2.PT2, D1.PT1) * CCW(D2.PT1, D2.PT2, D1.PT2) <=0)
            )
end;

// point dans zone  -- Test unitaire OK
function PointDansPolygone(const QX, QY: double; const LstSommets: TArrayPoints2Df): boolean;
var
  i, j, NbPts     : integer;
  CountIntersects : integer;
  LP, LT          : TDroite; // demi-droite partant du point (X, Y);
begin
  //AfficherMessage(' -- Dans PointDansPolygone()');
  Result := False;
  CountIntersects := 0;
  j := 0;
  LT.PT1.X := QX;
  LT.PT1.Y := QY;
  LT.PT2.X := 1E15;
  LT.PT2.Y := LT.PT1.Y;

  NbPts  := 1 + High(LstSommets);
  for i:=0 to NbPts-1 do begin
    LP.PT1 := LstSommets[i];
    LP.PT2 := LstSommets[i];
    if Intersect(LT, LP) then
      Continue;
    LP.PT2 := LstSommets[j];
    j := i;
    if(Intersect(LP,LT)) then Inc(CountIntersects);
  end;
  Result := (CountIntersects mod 2) > 0;
end;

function StrToDateTimeDef(const QDate: string; const DateDefault: TDateTime): TDateTime;
begin
  try
    Result := StrToDateTime(QDate);
  except
    Result := Now();
  end;
end;



// conversion sécurisée UTF et texte GHCD
function _AnsiToLCLStr(const S: string): string; inline;
begin
  //Result := AnsiToUtf8(S); // pour Lazarus < 1.6
  Result := system.AnsiToUtf8(S); // pour Lazarus < 1.6

end;
function _LCLStrToAnsi(const S: string): string; inline;
begin
  //Result := Utf8ToAnsi(S); // pour Lazarus < 1.6
  Result := System.Utf8ToAnsi(S); // pour Lazarus < 1.6
end;
// conversion d'attributs de fontes
//TFontStyle = (fsBold, fsItalic, fsStrikeOut, fsUnderline);
function AttributsFontePas2GHCD(const FA: TFontStyles): integer;
begin
  Result := Integer(FA);
end;
function AttributsFonteGHCD2Pas2(const FA: Integer): TFontStyles;
begin
  result := TFontStyles(FA);
end;
//******************************************************************************
// choix des styles d'objets (SVG, ODG)
function SelectSVGStylePolygone(const SP: TNatureObjetPolygone): string;
begin
  case SP of
    nopDEFAULT         : Result := STYLE_POLYGONE_DEFAUT;
    nopLAC             : Result := STYLE_POLYGONE_LAC;
    nopARGILE          : Result := STYLE_POLYGONE_ARGILE;
    nopSABLE           : Result := STYLE_POLYGONE_SABLE;
    nopEBOULIS         : Result := STYLE_POLYGONE_EBOULIS;
    nopGALETS          : Result := STYLE_POLYGONE_GALETS;
    nopNEIGE           : Result := STYLE_POLYGONE_NEIGE;
    nopSILHOUETTE      : Result := STYLE_POLYGONE_SILHOUETTE;
    nopGROS_BLOC       : Result := STYLE_POLYGONE_GROS_BLOC;
    nopGOUR            : Result := STYLE_POLYGONE_GOUR;
    nopSIPHON          : Result := STYLE_POLYGONE_SIPHON;
    nopARGILES_GALETS  : Result := STYLE_POLYGONE_VARVES;
    nopCHEMINS         : Result := STYLE_POLYGONE_CHEMIN;
  else
    Result := STYLE_POLYGONE_DEFAUT;
  end;
end;
function SelectSVGStyleLigne(const SL: TNatureObjetLigne): string;
begin
  case SL of
   nolDEFAULT      : Result := STYLE_LIGNE_DEFAULT;
   nolFLECHE       : Result := STYLE_LIGNE_FLECHE;
   nolSUITE_RESEAU : Result := STYLE_LIGNE_SUITE_RESEAU;
   nolFRACTURE     : Result := STYLE_LIGNE_FRACTURE;
   nolPENTE        : Result := STYLE_LIGNE_PENTE;
  else
    Result := STYLE_LIGNE_DEFAULT;
  end;
end;
function SelectSVGStyleTexte(const ST: TNatureObjetTexte): string;
begin
  case ST of
    notDEBUG         : Result := STYLE_TEXTE_DEBUG;
    notTITRES        : Result := STYLE_TEXTE_TITRES;
    notSOUS_TITRES   : Result := STYLE_TEXTE_SOUS_TITRES;
    notCOTATION      : Result := STYLE_TEXTE_COTATION;
    notTEXTE1        : Result := STYLE_TEXTE_ORDINAIRE_1;
    notTEXTE2        : Result := STYLE_TEXTE_ORDINAIRE_2;
    notLIEU_DIT      : Result := STYLE_TEXTE_LIEU_DIT;
  else
    Result := STYLE_TEXTE_DEFAULT;
  end;
end;
function SelectSVGStyleCourbe(const SC: TNatureObjetCourbe): string;
begin
  case SC of
    nocDEFAULT        : Result := STYLE_COURBE_DEFAULT;
    nocPAROI          : Result := STYLE_COURBE_PAROIS;
    nocPAROIS_CACHEE  : Result := STYLE_COURBE_PAROIS_CACHEES;
    nocECOULEMENT     : Result := STYLE_COURBE_ECOULEMENTS;
    nocLIGNES_PENTE   : Result := STYLE_COURBE_PENTES;
    nocRESSAUT        : Result := STYLE_COURBE_RESSAUTS;
    nocMARCHE         : Result := STYLE_COURBE_MINI_RESSAUTS;
    nocSURPLOMB       : Result := STYLE_COURBE_SURPLOMB;
    nocCHENAL_VOUTE   : Result := STYLE_COURBE_CHENAL;
  else
    Result := STYLE_COURBE_DEFAULT;
  end;
end;
function ColorToHTMLColor(const Couleur: TColor): string;
var
  R, V, B: byte;
begin
  R := Red(Couleur);
  V := Green(Couleur);
  B := Blue(Couleur);
  Result := Format('#%.2X%.2X%.2X', [R, V, B]);
end;
function ColorHTMLToColor(const CouleurHTML: string): TColor;
var
  R, V, B: byte;
  WU: String;
begin
  WU := Trim(CouleurHTML);
  WU[1] := '$';
  Result := StringToColor(WU);
end;
function MakeTPoint2Df(const QX, QY: double): TPoint2Df;
begin
  Result.X := QX;
  Result.Y := QY;
end;

function MakeTPoint3Df(const QX, QY, QZ: double): TPoint3Df;
begin
  Result.X := QX;
  Result.Y := QY;
  Result.Z := QZ;
end;
function MakeTPoint(const QX, QY: integer): TPoint;
begin
  Result.X := QX;
  Result.Y := QY;
end;
// crée un TPoint2Df en tenant compte des décalages de groupes
function MakeTPoint2DfWithGroupeAndBaseStation(const BP: TBaseStation; const GP: TGroupeEntites; const OffX, OffY: double): TPoint2Df;
var
  DG: TPoint2Df;
begin
  if (GP.DecalageActif) then DG := MakeTPoint2Df(GP.Decalage.X, GP.Decalage.Y) else DG := MakeTPoint2Df(0.00, 0.00);
  Result := MakeTPoint2Df(BP.PosStation.X + DG.X + OffX,
                          BP.PosStation.Y + DG.Y + OffY);
end;
// crée un TPoint2Df sans tenir compte des décalages de groupes (utilisé pour les BoundingBox des objets)
function MakeTPoint2DfWithBaseStationOnly(const BP: TBaseStation; const OffX, OffY: double): TPoint2Df;
begin
  Result := MakeTPoint2Df(BP.PosStation.X + OffX, BP.PosStation.Y + OffY);
end;

function MakeTRect(const Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Top     := Top;
  Result.Left    := Left;
  Result.Bottom  := Bottom;
  Result.Right   := Right;
end;


function KMLColor(const R, G, B, A: byte): string; inline;
begin
  Result := Format('<color>%.2X%.2X%.2X%.2X</color>',[A, B, G, R]);
end;
// formatage des nombres pour KML et OSM
function ensureConvertLongOrLatToXML(const Value: extended): string;
begin
  Result := Format('%.10f', [Value]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
end;
// formatage des nombres pour KML et OSM
function ensureConvertAltitudeToXML(const Value: extended): string;
begin
  Result := Format('%.2f', [Value]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
end;
// convertir en nombre des valeurs de type 123 456 789.10 ou 1245-45785
function ConvertirEnNombreReel(const S: string; const Default: double): double;
var
  st: String;
  m1: double;
  i : integer;
  c : char;
  procedure S666(const QC: char);
  begin
    if (st[1] = '-') then m1 := -1.00;
    if (st[1] = QC) then system.Delete(st, 1, 1);
  end;
begin
  try
    Result := Default;
    m1 := 1.00;
    st := Trim(s);  // on nettoie la chaîne
    if (st = '') then Exit(Default);
    // on vire les guillemets
    st := StringReplace(st, '"', ' ', [rfIgnoreCase, rfReplaceAll]);
    S666('''');       // on vire l'apostrophe de début
    S666('-');        // sauvegarde du signe
    S666('+');        // on vire le '+' de tête
    // balayage avec nettoyage et remplacement du point décimal
    ST := StringReplace(ST, '.', DefaultFormatSettings.DecimalSeparator, [rfIgnoreCase, rfReplaceAll]);
    ST := StringReplace(ST, ',', DefaultFormatSettings.DecimalSeparator, [rfIgnoreCase, rfReplaceAll]);
    // on ne retient que les chiffres
    result := StrToFloat(ST) * m1;
  except
    Result := Default;
  end;
end;

// convertir en nombre des valeurs de type 123 456 789.10 ou 1245-45785
(*
function ConvertirEnNombreReel(const S: string; const Default: Extended): Extended;
var
  st: String;
  m1: double;
  tt: string;
  i : integer;
  c : char;
begin
  try
    m1 := 1.00;
    // on nettoie la chaîne
    st := Trim(s);
    // on vire les guillemets
    st := StringReplace(st, '"', ' ', [rfIgnoreCase, rfReplaceAll]);
    st := Trim(S);
    // on vire l'apostrophe de début
    if (st[1] = '''') then system.Delete(st, 1, 1);
    // sauvegarde du signe
    if (st[1] = '-') then
    begin
      m1 := -1;
      System.Delete(st, 1, 1);
    end;
    // on vire le '+'
    if (st[1] = '+') then System.Delete(st, 1, 1);
    // balayage avec nettoyage et remplacement du point décimal
    tt := '';
    for i := 1 to Length(st) do
    begin
      c := st[i];
      if (c in ['0' .. '9', '.', ',']) then
      begin
        //if (c = DefaultFormatSettings.ThousandSeparator) then c := ''; // on vire le séparateur de milliers
        if (c = ',') then c := DefaultFormatSettings.DecimalSeparator;
        if (c = '.') then c := DefaultFormatSettings.DecimalSeparator;
        tt := tt + c;
      end;
    end;
    //AfficherMessage(tt);
    // on ne retient que les chiffres
    result := StrToFloat(tt) * m1;
  except
    Result := Default;
  end;
end;
//*)
// choix des styles d'objets (SVG, ODG)
function GetDescStylePolygone(const SP: TNatureObjetPolygone): string; inline;
begin
  Result := ListeStylesPolygones[Ord(SP)];
end;
function GetDescStyleLigne(const SL: TNatureObjetLigne): string; inline;
begin
  Result := ListeStylesLignes[Ord(SL)];
end;
function GetDescStyleTexte(const ST: TNatureObjetTexte): string; inline;
begin
  Result := ListeStylesTextes[Ord(ST)];
end;
function GetDescStyleCourbe(const SC: TNatureObjetCourbe): string; inline;
begin
  Result := ListeStylesCourbes[Ord(SC)];
end;
function GetDescStylePolyligne(const SC: TNatureObjetCourbe): string; inline;
begin
  Result := GetDescStyleCourbe(SC);
end;
function GetDescNatureSymbole( const SC: TNatureObjetSymbole) : string; inline;
begin
  Result := ListeNatureSymboles[Ord(SC)];
end;
//******************************************************************************
// échelle de couleurs
function GetColorByScale(const V, QMin, QMax: double; const ColDefault: TColor; const AC: array of TColor): TColor;
var
  i, n: Integer;
  Step: Extended;
begin
  Result := ColDefault;
  n := High(AC);
  if (V < QMin) then Exit;
  if (V > QMax) then Exit;
  Step := (QMax - QMin) / n;
  for i := 0 to n do
  begin
    if (IsInRange(V, QMin + Step * i, QMin + Step * (i + 1))) then
    begin
      Result := AC[i];
      exit;
    end;
  end;

end;
// pour les textures
function Interp256(const value1,value2,position: integer): integer; inline; overload;
begin
  result := (value1*(256-position) + value2*position) shr 8;
end;
function Interp256(const color1,color2: TBGRAPixel; const position: integer): TBGRAPixel; inline; overload;
begin
  result.red   := Interp256(color1.red,color2.red, position);
  result.green := Interp256(color1.green,color2.green, position);
  result.blue  := Interp256(color1.blue,color2.blue, position);
  result.alpha := Interp256(color1.alpha,color2.alpha, position);
end;

function CreateSandTexture(tx, ty: integer): TBGRABitmap;
const
  blurSize     = 5;
  tile_overlap = 4;
var
  QTileOverlap : LongInt;
  WU      : TRect;
  temp    : TBGRABitmap;
  phong   : TPhongShading;
begin
  QTileOverlap := 4;
  // GetPart crée également l'objet Temp
  result := CreateCyclicPerlinNoiseMap(128, 128, 0.01, 0.01, 0.07000);
  // Bumped procedural texture
  WU := MakeTRect(-tile_overlap,-tile_overlap,tx+tile_overlap,ty+tile_overlap);
  temp:= result.GetPart(WU) as TBGRABitmap;
  BGRAReplace(temp,temp.FilterBlurRadial(blurSize,rbFast));
  try
    phong := TPhongShading.Create;
    phong.LightSourceDistanceFactor := 0;
    phong.LightDestFactor           := 0;
    phong.LightSourceIntensity      := 150;
    phong.LightPositionZ            := 80;
    phong.LightColor := BGRA(255, 255, 255, 0);

    phong.NegativeDiffusionFactor   := 0.30;
    phong.SpecularIndex             := 20;
    phong.AmbientFactor            := 0.400;
    phong.Draw(Result, Temp, 20, -blurSize,-blurSize, BGRA(241, 207, 0, 255));
  finally
    FreeAndNil(phong);
    FreeAndNil(temp);
  end;
end;

function CreateClayTexture(tx,ty: integer): TBGRABitmap;
const
  blurSize     = 5;
  tile_overlap = 4;
var
  QTileOverlap : LongInt;
  WU      : TRect;
  temp    : TBGRABitmap;
  phong   : TPhongShading;
begin
  QTileOverlap := 4;
  // GetPart crée également l'objet Temp
  result := CreateCyclicPerlinNoiseMap(128, 128,  0.08, 0.08, 0.21000);
  // Bumped procedural texture
  WU := MakeTRect(-tile_overlap,-tile_overlap,tx+tile_overlap,ty+tile_overlap);
  temp:= result.GetPart(WU) as TBGRABitmap;
  BGRAReplace(temp,temp.FilterBlurRadial(blurSize,rbFast));
  try
    phong := TPhongShading.Create;
    phong.LightSourceDistanceFactor := 0;
    phong.LightDestFactor           := 0;
    phong.LightSourceIntensity      := 150;
    phong.LightPositionZ            := 80;
    phong.LightColor := BGRA(255, 255, 255, 0);

    phong.NegativeDiffusionFactor   := 0.30;
    phong.SpecularIndex             := 20;
    phong.AmbientFactor            := 0.400;
    phong.Draw(Result, Temp, 20, -blurSize,-blurSize, BGRA(191, 132, 70, 255));
  finally
    FreeAndNil(phong);
    FreeAndNil(temp);
  end;
end;

// texture de pierre
function CreateStoneTexture(tx,ty: integer): TBGRABitmap;
 var
   temp: TBGRABitmap;
   phong: TPhongShading;
   WU: TRECT;
 begin
   result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,0.6);
   // GetPart crée également l'objet Temp
   WU := MakeTRect(-2,-2,tx+2,ty+2);
   temp:= result.GetPart(WU) as TBGRABitmap;
   phong := TPhongShading.Create;
   try
     phong.LightSourceDistanceFactor := 0;
     phong.LightDestFactor := 0;
     phong.LightSourceIntensity := 100;
     phong.LightPositionZ := 100;
     phong.NegativeDiffusionFactor := 0.3;
     phong.AmbientFactor := 0.5;
     phong.Draw(result,temp,30,-2,-2,BGRA(170,170,170));
   finally
     FreeAndNil(phong);
     FreeAndNil(temp);
   end;
 end;


// texture eau
function CreateWaterTexture(tx,ty: integer): TBGRABitmap;
const blurSize = 5;
var
  temp: TBGRABitmap;
  phong: TPhongShading;
  QRect: TRect;
begin
  result := CreateCyclicPerlinNoiseMap(tx,ty,1,1,2.6);
  QRect := MakeTRect(-blurSize, -blurSize, tx + blurSize, ty + blurSize);
  temp   := result.GetPart(QRect) as TBGRABitmap;
  BGRAReplace(temp,temp.FilterBlurRadial(blurSize,rbFast));
  try
    phong := TPhongShading.Create;
    phong.LightSourceDistanceFactor := 0;
    phong.LightDestFactor := 0;
    phong.LightSourceIntensity := 150;
    phong.LightPositionZ := 80;
    phong.LightColor := BGRA(105,233,240);
    phong.NegativeDiffusionFactor := 0.3;
    phong.SpecularIndex := 20;
    phong.AmbientFactor := 0.4;
    phong.Draw(result,temp,20,-blurSize,-blurSize,BGRA(28,139,166));
  finally
    FreeAndNil(phong);
    FreeAndNil(temp);
  end;
end;
function GetGHCaveDrawApplicationName(): string;
begin
  Result := 'GHCaveDraw';
end;

function GetGHCaveDrawVersion(): string; inline;
begin
  // Versions: 2          Début du projet, specs posées et architecture de la BDD OK (fin 2008)
  //           2.7        Moteur de tracé des objets graphiques OK; feuilles de styles (2009)
  //           2.71       Editeur graphique opérationnel, centre d'impression, export SVG (2009 .. 2015)
  //           2.718      Support des scraps et des images, génération des atlas, génération de topos as images de grande taille (2015-)
  //           2.7182     Sections transversales (2016-)
  Result := Format('%.4f', [exp(1)]);
end;

function GetIconesTexturesDirectory(): string;
begin
  result := GetGHCaveDrawDirectory() + 'Icones' + PathDelim;
end;

procedure pass();
begin
  ;
end;

function GetGHCaveDrawVersionAndDateBuild(): string;
var
  WU: LongInt;
  EWE: TDateTime;
  YYYY, MM, DD, HH, MN, SS, MS: Word;
begin
  WU := FileAge(ParamStr(0));
  EWE := FileDateToDateTime(WU);
  DecodeDateTime(EWE, YYYY, MM, DD, HH, MN, SS, MS);
  Result := Format(rsGHCD_VERSION, [exp(1), DD, MM, YYYY, HH, MN]);  //_AnsiToLCLStr(rsGHTOPOVERSION);
end;
// parsage de texte
//==============================================================================
// version de la fonction ExtractStrings() mais envoie le résultat dans un TStringArray au lieu d'un TStringList
function ExtractStringsToTStringArray(Separators, WhiteSpace: TSysCharSet;
                                      const MyStr: string): TGHStringArray;
var
  b, c : pchar;
  quoted : char;
  IdxResultArray: Integer;
  procedure SkipWhitespace;
  begin
    while (c^ in Whitespace) do inc (c);
  end;

  procedure AddString;
  var
    l : integer;
    s : string;
  begin
    l := c-b;
    if (l > 0) then
    begin
      setlength(s, l);
      move (b^, s[1],l);
      //Strings.Add (s);
      Result[IdxResultArray] := s;
      IdxResultArray += 1;
    end;
  end;
begin
  IdxResultArray := 0;
  c := PChar(MyStr);
  Quoted := #0;
  //Separators := Separators + [#13, #10] - ['''','"'];
  Separators := Separators + [#13, #10] - ['"'];
  SkipWhitespace;
  b := c;
  while (c^ <> #0) do
  begin
    if (c^ = Quoted) then
    begin
      if ((c+1)^ = Quoted) then
        inc (c)
      else
        Quoted := #0
    end
    //else if (Quoted = #0) and (c^ in ['''','"']) then
    else if (Quoted = #0) and (c^ in ['"']) then
      Quoted := c^;
    if (Quoted = #0) and (c^ in Separators) then
    begin
      AddString;
      inc (c);
      SkipWhitespace;
      b := c;
    end
    else
      inc (c);
  end;
  if (c <> b) then AddString;
end;



function DecoupeChaineEspacesGuillemets(const S: string; const DoRemoveQuotes: boolean): TGHStringArray; inline;
var
  i: Integer;
  function RemoveQuote(const S: string): string;
  begin
    Result := StringReplace(S, '"', '', [rfReplaceAll]);
  end;
begin
  Result := ExtractStringsToTStringArray([' '], [], S);
  if (DoRemoveQuotes) then
  begin
    for i := 0 to high(Result) do Result[i] := RemoveQuote(Result[i]);
  end;
end;

function ExtractNumSerieFromTIDBaseStation(const B: TIDBaseStation): integer;
begin
  {$IFDEF TIDBASEPOINT_AS_TEXT}
    Result := -1;
  {$ELSE}
    Result := Abs(B) div MULTIPLICATEUR_SERIE; // 100000;
  {$ENDIF TIDBASEPOINT_AS_TEXT}
end;
//*)

// fonctions issues de TDocumentDessin
//===========================================================================

function GenererLigneBasepoint(const QIdx: integer; const BP: TBaseStation): string;
begin
  Result := (Format(FMT_BASE_STS,
                       [BP.IDStation,
                        BP.IDTerrain,
                        BP.TypeStation, BP.Couleur,
                        BP.PosExtr0.X,  BP.PosExtr0.Y, BP.PosExtr0.Z,
                        BP.PosStation.X, BP.PosStation.Y, BP.PosStation.Z,
                        BP.PosPG.X, BP.PosPG.Y, BP.PosPG.Z,
                        BP.PosPD.X, BP.PosPD.Y, BP.PosPD.Z,
                        BP.IDTerrain
                       ]));
end;
function GenererLigneSimpleLigne(const QIdx: integer; const L: TSimpleLigne): string;
begin
  Result := Format('    line '+ #9 + '%d' + #9 + '%d' + #9 + '%d' + //ID, Groupe, Style
                                #9 + FORMAT_BASEPOINT + #9 + '%.2f' + #9 + '%.2f' + #9 + '%.2f' +
                                #9 + FORMAT_BASEPOINT + #9 + '%.2f' + #9 + '%.2f' + #9 + '%.2f' +
                                #9 + '%s',
                       [QIdx, L.IDGroupe, L.IDStyleLigne,
                        L.IDBaseStExt1, L.OffsetExtr1.X, L.OffsetExtr1.Y, L.OffsetExtr1.Z,
                        L.IDBaseStExt2, L.OffsetExtr2.X, L.OffsetExtr2.Y, L.OffsetExtr2.Z,
                        DateTimeToStr(Now)
                       ]);
end;

function GenererLigneSymbole(const QIdx: integer; const Obj: TSymbole): string;
begin
  Result := Format('    ponctualobject '+ #9 + '%d' + #9 + '%d' + #9 + '%d' +  #9 + '%d' + //IDXGroupe, TypeObject, Couleur,
                                #9 + FORMAT_BASEPOINT + #9 + '%.2f' + #9 + '%.2f' + #9 + '%.2f' + // IDBase, Offset
                                #9 + '%.2f' + #9 + '%.2f' + #9 + '%.2f' +
                                #9 +'%s' + // texte
                                #9 + '%d' + #9 + '%d' + #9 + '%s',
                       [QIdx, Obj.IDGroupe, Obj.TypeObject, Obj.Couleur,
                        Obj.IDBaseStation, Obj.Offset.X, Obj.Offset.Y, Obj.Offset.Z,
                        Obj.AngleRot, Obj.ScaleX, Obj.ScaleY,
                        Obj.TagTexte,
                        Obj.UnTag, IIF(Obj.PhotoDisplayed, 1, 0),
                        DateTimeToStr(Now)
                       ]);
end;
function GenererLigneTexte(const QIdx: integer; const Obj: TTextObject): string;
begin
  Result := Format('    text '+ #9 + '%d' + #9 + '%d' + #9 + '%d' + //ID, Groupe, Style
                                #9 + FORMAT_BASEPOINT + #9 + '%.2f' + #9 + '%.2f' + #9 + '%.2f' +
                                #9 + '%d' + #9 + '%d'+ #9 + '%s' + #9 + '%s',
                       [QIdx, Obj.IDGroupe, Obj.IDStyleTexte,
                        Obj.IDBaseSt, Obj.Offset.X, Obj.Offset.Y, Obj.Offset.Z,
                        Obj.Alignment, Obj.MaxLength, Obj.Text,
                        DateTimeToStr(Now)
                       ])

end;

function GenererLigneArcCourbe(const QIdx: integer; const A: TArcCourbe): string;
begin
  Result := Format(ENTITYARCBEZIER,
                        [A.IDStationP1,
                         A.OffsetP1.X, A.OffsetP1.Y, A.OffsetP1.Z,
                         A.TangP1.X, A.TangP1.Y, -1.00,
                         A.IDStationP2,
                         A.OffsetP2.X, A.OffsetP2.Y, A.OffsetP2.Z,
                         A.TangP2.X, A.TangP2.Y, -2.00
                        ]);
end;

function GenererLigneGroupe(const QIdx: integer; const Grp: TGroupeEntites): string;
begin
  Result := Format('  groupe'+ #9 +'%d' + #9 +
                       '%s' + #9 + '%d' + #9 +
                       FMT_COORDS + #9 +
                       '%d' + #9 + '%s' +
                       #9 + '%d' +
                       #9 + '%.2f' +
                       #9 + '%s', [
                         GRP.IDGroupeEntites,
                         GRP.NomGroupe,
                         GRP.CouleurGroupe,
                         GRP.Decalage.X,
                         GRP.Decalage.Y,
                         GRP.Decalage.Z,
                         0,
                         DateTimeToStr(GRP.DateLastModif),
                         IIF(GRP.DecalageActif, 1, 0),
                         GRP.ZOrder,
                         GRP.Filtres
                       ]);
end;
function GenererLigneVertexPolygon(const QIdx: integer; const V: TVertexPolygon): string;
begin
  Result := Format(ENTITYVERTEXPOLYGON,
                        [V.IDStation,
                         V.Offset.X, V.Offset.Y, V.Offset.Z
                        ]);
end;
function GenererLigneEnteteScrap(const QIdx: integer; const S: TScrap): string;
begin
  Result := Format('  ' + SCRAP + FMT_SCRAP, [QIdx, S.IDGroupe,
                                                   Red(S.Couleur), Green(S.Couleur), Blue(S.Couleur),
                                                   S.Opacite,
                                                   S.Nom]);
end;

function GenererLigneEnteteCourbe(const QIdx: integer; const C: TCourbe): string;
begin
  Result := Format('  ' + ENTITYCURVE + FMT_CURVE, [QIdx, C.IDGroupe, C.IDStyleCourbe, IIF(C.Closed, 1, 0)]);
end;

function GenererLigneEntetePolyligne(const QIdx: integer; const PL: TPolyLigne): string;
begin
  Result := Format('  ' + ENTITYPOLYLINE + FMT_POLYLINE, [QIdx, PL.IDGroupe, PL.IDStylePolyLine, IIF(PL.Closed, 1, 0)]);
end;

function GenererLigneEntetePolygone(const QIdx: integer; const PG: TPolygone): string;
begin
  Result := Format('  ' + ENTITYPOLYGON + FMT_POLYGON, [QIdx, PG.IDGroupe, PG.IDStylePolygone]);

end;

function GenererLigneEndScrap: string;
begin
  Result := '  ' + ENDSCRAP;
end;

function GenererLigneEndCourbe: string;
begin
  Result := '  ' + ENDENTITYCURVE;
end;

function GenererLigneEndPolyligne: string;
begin
  Result := '  ' + ENDENTITYPOLYLINE;
end;

function GenererLigneEndPolygone: string;
begin
  Result := '  ' + ENDENTITYPOLYGON;
end;

// échappement des guillemets et autres. Même nom que la fonction php.
// OK pour les guillemets "
function mysqli_real_escape_string(const S: string): string;
begin
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
end;

function XML_real_escape_string(const S: string): string;
begin
  result := S;
end;


function DateSqlToTDateTime(const S: string): TDateTime;
var
  AAAA, MM, JJ: word;
  HH, MN, SS, MS: word;
begin
  //1234567890123456789
  //2016-08-15 01:02:03
  AAAA := StrToIntDef(Copy(S, 1, 4), 2000);
  MM   := StrToIntDef(Copy(S, 6, 2), 01);
  JJ   := StrToIntDef(Copy(S, 9, 2), 01);
  HH   := StrToIntDef(Copy(S, 12, 2), 00);
  MN   := StrToIntDef(Copy(S, 15, 2), 00);
  SS   := StrToIntDef(Copy(S, 18, 2), 00);
  MS   := 0;
  Result := EncodeDateTime(AAAA, MM, JJ, HH, MN, SS, MS);
end;

// encodage d'une date en format SQL
function DateTimeToDateSql(const QD: TDateTime): string;
const
  SQL_DATE_HEURE_FORMAT = '%.4d-%.2d-%.2d %.2d:%.2d:%.2d';
var
  QAnnee, QMois, QJour, QHeure, QMinute, QSeconde, QMilliseconde: word;
begin
  DecodeDate(QD, QAnnee, QMois, QJour);
  DecodeTime(QD, QHeure, QMinute, QSeconde, QMilliseconde);
  Result := Format(SQL_DATE_HEURE_FORMAT,
                   [QAnnee, QMois, QJour,
                    QHeure, QMinute, QSeconde
                   ]);
end;
function GetLlanfairPG(): string;
begin
  Result := UpperCase('Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch');
end;
// proposer un alignement de texte en prolongement de la ligne d'attache
// retourne l'alignement aka disposition clavier numérique
// /!\ artan2() retourne une valeur entre -pi et +pi
function ProposerTextAlignment(const dx, dy: double): byte;
var
  QAngle: double;
begin
  Result := 0;
  QAngle := radtodeg(arctan2(dy, dx));

  if      (InRange(QAngle,    0.00,   10.00))   then Result := 4
  else if (InRange(QAngle,   10.00,   80.00))   then Result := 1
  else if (InRange(QAngle,   80.00,  100.00))   then Result := 2
  else if (InRange(QAngle,  100.00,  170.00))   then Result := 3
  else if (InRange(QAngle,  170.00,  180.00))   then Result := 6
  else if (InRange(QAngle,  -10.00,    0.00))   then Result := 4
  else if (InRange(QAngle,  -80.00,  -10.00))   then Result := 7
  else if (InRange(QAngle, -100.00,  -80.00))   then Result := 8
  else if (InRange(QAngle, -170.00, -100.00))   then Result := 9
  else if (InRange(QAngle, -180.00, -170.00))   then Result := 6
  else Result := 1;
  AfficherMessageErreur(Format('dx: %.2f, dy: %.2f, Angle: %.2f, Align: %d', [dx, dy, QAngle, Result]));
end;

function ArcTan2InDegrees(const x, y: double): double;
begin
  Result := radtodeg(arctan2(y, x));
  if (Result < 0) then Result := Result + 360.00;
  if (Result > 360.00) then Result := Result - 360.00;
end;

// formatage OOo des nombres
function FormatterNombreOOo(const Value: extended; const NbDecs: integer; const IgnoreNulls: boolean): string;
var
  EWE: String;
begin
  EWE := Format('%%.%df', [NbDecs]);
  // remplacer les points par des virgules
  Result := Format(EWE, [Value]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, ',', [rfReplaceAll, rfIgnoreCase]);
  if (IgnoreNulls and SameValue(Value, 0)) then Result := '';
end;
function StrToArrayOfIdxGroupes(const S: string): TArrayOfIdxGroupes;
var
  EWE: TStringArray;
  i: Integer;
  QIdx: LongInt;
begin
  EWE := SplitEx(Trim(S), [';'], true);
  Setlength(Result, 0);
  for i := 0 to High(EWE) do
  begin
    QIdx := StrToIntDef(EWE[i], -1);
    if (QIdx >= 0) then
    begin
      Setlength(Result, Length(Result) + 1);
      Result[High(Result)] := QIdx;
    end;
  end;
end;
function ArrayOfIdxGroupesToStr(const A: TArrayOfIdxGroupes): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(A) do Result += Format('%d;',[A[i]]);
end;

function AddToArrayOfIdxGroupes(var A: TArrayOfIdxGroupes; const Idx: TIDGroupeEntites): boolean;
var
  i, n: Integer;
  procedure QExchange(const n1, n2: integer);
  var
    tmp: TIDGroupeEntites;
  begin
    tmp := A[n1];
    A[n1] := A[n2];
    A[n2] := tmp;
  end;
  procedure QSort(const lidx, ridx: integer);
  var
    k, e, mid: integer;
    Q1, Q2: TIDGroupeEntites;
  begin
    if (lidx >= ridx) then Exit;
    mid := (lidx + ridx) div 2;
    QExchange(lidx, ridx);
    e:=lidx;
    for k:=lidx+1 to ridx do
    begin
      Q1 := A[k];
      Q2 := A[lidx];
      if (Q1 < Q2)  then
      begin
        Inc(e);
        QExchange(e, k);
      end;
    end;
    QExchange(lidx, e);
    QSort(lidx, e-1);
    QSort(e+1, ridx);
  end;
begin
  Result := false;
  if (Idx < 0) then exit;
  // vérifier si le groupe existe déjà
  n := Length(A);
  for i := 0 to n-1 do if (Idx = A[i]) then exit;
  try
    SetLength(A, n + 1);
    A[High(A)] := Idx;
    QSort(0, High(A));
    Result := True;
  except

  end;
end;

function CompareIDStations(const I1, I2: TIDBaseStation): boolean;
begin
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  Result := (I1 = I2);
  {$ELSE}
  Result := (abs(I1) = abs(I2));
  {$ENDIF TIDBASEPOINT_AS_TEXT}
end;

function IsValidBaseStation(const BS: TBaseStation): boolean;
begin
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  Result := (BS.IDStation <> '');
  {$ELSE}
  Result := (BS.IDStation > 0);
  {$ENDIF TIDBASEPOINT_AS_TEXT}
end;
// distance sur grand cercle entre deux points (Lon, Lat)
function HaversineDist(const Lat1, Lon1, Lat2, Lon2: double):double;
const diameter = 2 * 6372.8;
var   dx, dy, dz: double;
  QLat1, QLon1, QLon2: float;
begin
  QLat1    := degtorad(Lat1 - Lat2);
  QLon1    := degtorad(Lon1);
  QLon2    := degtorad(Lon2);

  dz     := sin(QLon1) - sin(QLon2);
  dx     := cos(QLat1) * cos(QLon1) - cos(QLon2);
  dy     := sin(QLat1) * cos(QLon1);
  Result := arcsin(sqrt(sqr(dx) + sqr(dy) + sqr(dz)) / 2) * diameter;
end;



end.
