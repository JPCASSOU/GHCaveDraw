unit unitTherionScrap;

{$INCLUDE CompilationParameters.inc}

// Génération d'un scrap THERION pour un groupe donné
// En attente de compréhension du mécanisme interne des scraps de Therion

// 19/12/2014:
interface

uses
  SysUtils
  , Classes
  , UnitDocDessin
  , UnitCenterlinesBasePoints
  , UnitListesSimplesWithGeneriques
  , GeneralFunctions
  , GHCD_Types
  {$IFDEF LANGUAGE_FRENCH}
    , UnitMessages_fr
  {$ENDIF}
  ;
type TLocalScrapCoordinates = record
  X : double;
  Y : double;
end;



(*point 763 746 station -name 2@second
point 702 430 station -name 2@first
point 352 469 station -name 1@first
point 675 585 air-draught -orientation 240 -scale large
//*)
type TTh2ScrapBasepoint = record
  StationName: string;
  Coordinates: TLocalScrapCoordinates;
end;
type TListeBasePointScrap = TListeSimple<TTh2ScrapBasepoint>;
type

{ TTherionScrap }

 TTherionScrap = class
  private
    FTH2FileName: string;
    FScrapName: string;
    FMyDocDessin: TDocumentDessin;
    FListeMorphingBasePoints: TListeBasePointScrap;
    // pointeur sur les centerlines, pour éviter les lourdeurs d'écritures
    FPtrCenterlines: TCenterLines;
    // groupe d'objets
    FGroupeObjets: TGroupeEntites;
    // bounding-box du groupe
    FBoundingBox: TBoundingBox;
    // conteneur de sortie: utilisé pour affichage du script sur un TMemo, ou dans un fichier
    FTamponSortie: TStringList;

    procedure AddBasePointScrap(const BP: TTh2ScrapBasepoint); overload;
    procedure AddBasePointScrap(const BPName: string; const BPX, BPY: double); overload;
    // construction du script
    procedure AddLineBuffer(const S: string);
    function  ConvertGCSCoordsToLocalScrapCoords(const QX, QY: double): TLocalScrapCoordinates; overload;
    function  ConvertGCSCoordsToLocalScrapCoords(const BP: TBaseStation; const QdX, QdY: double): TLocalScrapCoordinates; overload;
    function GetTherionTypeLigne(const T: TNatureObjetCourbe): string;
  public
    function  Initialiser(const FD: TDocumentDessin; const TH2FileName: string): boolean;
    procedure Finaliser();
    procedure SetGroupe(const Grp: TGroupeEntites);
    procedure MakeSectionBasePoints();
    procedure RecenserBasePointsScrap();
    procedure BeginTH2File;
    procedure BeginScrap(const QTag: string);
    procedure EndScrap();
    procedure EndTH2File();
    // objets
    procedure MakeCourbe(const QCourbe: TCourbe);
    procedure MakePolygone(const QPolygone: TPolygone);
    procedure MakePolyLigne(const QPolyLigne: TPolyLigne);
    procedure MakeSymbole(const MySymbole: TSymbole);
    procedure MakeTextObject(const MyTexte: TTextObject);

    // récupérer le tampon de sortie
    function GetTamponSortie(): TStringList;
end;
implementation

function MakeTLocalScrapCoordinates(const QX, QY: double): TLocalScrapCoordinates;
begin
  Result.X := QX;
  Result.Y := QY;
end;
function MakeTTh2ScrapBasepoint(const QX, QY: double; const SN: string): TTh2ScrapBasepoint;
begin
  Result.Coordinates.X := QX;
  Result.Coordinates.Y := QY;
  Result.StationName   := SN;
end;

{ TTherionScrap }



function TTherionScrap.Initialiser(const FD: TDocumentDessin; const TH2FileName: string): boolean;
begin
  FTH2FileName := TH2FileName;
  FScrapName := '';
  FMyDocDessin := FD;
  FMyDocDessin.CalcBoundingBoxAllGroupes(); // on recalcule toutes les bounding box
  FPtrCenterlines := FMyDocDessin.GetPtrCenterlines();
  FListeMorphingBasePoints := TListeBasePointScrap.Create;
  FTamponSortie            := TStringList.Create;
  FListeMorphingBasePoints.ClearListe();
end;

procedure TTherionScrap.SetGroupe(const Grp: TGroupeEntites);
begin
  FGroupeObjets := Grp;
  FBoundingBox := FGroupeObjets.BoundingBox;
end;

procedure TTherionScrap.AddBasePointScrap(const BPName: string; const BPX, BPY: double);
begin
  AddBasePointScrap(MakeTTh2ScrapBasepoint(BPX, BPY, BPName));
end;

procedure TTherionScrap.AddBasePointScrap(const BP: TTh2ScrapBasepoint);
begin
  FListeMorphingBasePoints.AddElement(BP);
end;

procedure TTherionScrap.AddLineBuffer(const S: string);
begin
  FTamponSortie.Add(S);
end;

procedure TTherionScrap.BeginScrap(const QTag: string);
begin
  FScrapName := Format('Groupe%d', [FGroupeObjets.IDGroupeEntites]);
  AddLineBuffer('');
  AddLineBuffer(Format('  # Scrap for Groupe: %d - %s', [FGroupeObjets.IDGroupeEntites, FGroupeObjets.NomGroupe]));
  AddLineBuffer(format('  scrap %s', [FScrapName]));
  (*
  scrap s1 -author 2004 "Therion team"
point 763 746 station -name 2@second
point 702 430 station -name 2@first
point 352 469 station -name 1@first
point 675 585 air-draught -orientation 240 -scale large
line wall -close on
287 475
281 354 687 331 755 367
981 486 846 879 683 739
476 561 293 611 287 475
endline
endscrap
  //*)
end;

procedure TTherionScrap.EndScrap;
begin
  AddLineBuffer(format('  endscrap # %s', [FScrapName]));
end;


procedure TTherionScrap.MakeSectionBasePoints();
var
  EWE: TTh2ScrapBasepoint;
  n: LongInt;
  i: Integer;
begin
  n := FListeMorphingBasePoints.GetNbElements();
  AddLineBuffer(format('    # Basepoints list: %d stations', [n]));
  if (n = 0) then Exit;
  for i := 0 to n - 1 do
  begin
    EWE := FListeMorphingBasePoints.GetElement(i);
    AddLineBuffer(Format('    point %f %f station -name %s', [EWE.Coordinates.X, EWE.Coordinates.Y, EWE.StationName]));
  end;
end;

function TTherionScrap.GetTherionTypeLigne(const T: TNatureObjetCourbe): string;
begin
  case T of
    nocDEFAULT           : Result := '';
    nocPAROI             : Result := 'wall';
    nocPAROIS_CACHEE     : ;
    nocECOULEMENT        : ;
    nocLIGNES_PENTE      : ;
    nocRESSAUT           : ;
    nocSURPLOMB          : ;
    nocCHENAL_VOUTE      : ;
    nocMARCHE            : ;
    nocPAROI_INCERTAINE  : ;
    nocMUR_MACONNE       : ;
  else
  end;
end;
procedure TTherionScrap.MakePolyLigne(const QPolyLigne: TPolyLigne);
var
  i: Integer;
  EWE: TVertexPolygon;
  PP: TLocalScrapCoordinates;
  BP: TBaseStation;
begin
  AddLineBuffer(format('      line %s -close on', [GetTherionTypeLigne(QPolyLigne.IDStylePolyLine)]));
  AddLineBuffer('        smooth off');
  //line rock-border -close on -place bottom

  //235.0 -899.5
  //253.5 -902.5
  for i := 0 to High(QPolyLigne.Sommets) do
  begin
    EWE := QPolyLigne.Sommets[i];
    if (FMyDocDessin.GetBasePointByIndex(EWE.IDStation, BP)) then
    begin
      PP  := ConvertGCSCoordsToLocalScrapCoords(BP, EWE.Offset.X, EWE.Offset.Y);
      AddLineBuffer(Format('      %.3f %.3f', [PP.X, PP.Y]));
    end;
  end;
  AddLineBuffer('      endline');
end;

procedure TTherionScrap.MakePolygone(const QPolygone: TPolygone);
var
  i: Integer;
  EWE: TVertexPolygon;
  PP: TLocalScrapCoordinates;
  BP: TBaseStation;
begin
  AddLineBuffer('      line ');
  AddLineBuffer('        smooth off');
  //line rock-border -close on -place bottom
  //235.0 -899.5
  //253.5 -902.5
  for i := 0 to High(QPolygone.Sommets) do
  begin
    EWE := QPolygone.Sommets[i];
    if (FMyDocDessin.GetBasePointByIndex(EWE.IDStation, BP)) then
    begin
      PP  := ConvertGCSCoordsToLocalScrapCoords(BP, EWE.Offset.X, EWE.Offset.Y);
      AddLineBuffer(Format('      %.3f %.3f', [PP.X, PP.Y]));
    end;
  end;
  AddLineBuffer('      endline');
end;

procedure TTherionScrap.MakeCourbe(const QCourbe: TCourbe);
var
  i: Integer;
  BP: TBaseStation;
  AC: TArcCourbe;
  PP1, PP2, PC2, PC1: TLocalScrapCoordinates;
begin
  AddLineBuffer('      line -close off');

  //line rock-border -close on -place bottom
  //235.0 -899.5
  //253.5 -902.5
  AC := QCourbe.Arcs[0];
  if (FMyDocDessin.GetBasePointByIndex(AC.IDStationP1, BP)) then
  begin
    PP1  := ConvertGCSCoordsToLocalScrapCoords(BP, AC.OffsetP1.X, AC.OffsetP1.Y);
    AddLineBuffer(Format('          %.3f %.3f', [PP1.X, PP1.Y]));
  end;
  AddLineBuffer('        smooth on');
  for i := 0 to High(QCourbe.Arcs) do
  begin
    AC := QCourbe.Arcs[i];
    if (FMyDocDessin.GetBasePointByIndex(AC.IDStationP2, BP)) then
    begin
      PP1  := ConvertGCSCoordsToLocalScrapCoords(BP, AC.OffsetP1.X, AC.OffsetP1.Y);
      PC1  := ConvertGCSCoordsToLocalScrapCoords(BP, AC.OffsetP1.X + AC.TangP1.X,
                                                     AC.OffsetP1.Y + AC.TangP1.Y);
      PC2  := ConvertGCSCoordsToLocalScrapCoords(BP, AC.OffsetP2.X + AC.TangP2.X,
                                                     AC.OffsetP2.Y + AC.TangP2.Y);
      PP2  := ConvertGCSCoordsToLocalScrapCoords(BP, AC.OffsetP2.X, AC.OffsetP2.Y);

      AddLineBuffer(Format('          %.3f %.3f', [PP2.X, PP2.Y]));
    end;
  end;
  AddLineBuffer('      endline');
end;


procedure TTherionScrap.MakeSymbole(const MySymbole: TSymbole);
var
  BP: TBaseStation;
  PP: TLocalScrapCoordinates;
  beuh: String;
begin
  //point 312.0 -807.0 stalagmite
  if (FMyDocDessin.GetBasePointByIndex(MySymbole.IDBaseStation, BP)) then
  begin
    case MySymbole.TypeObject of
      nosPHOTO            : beuh := '';
      nosENTREE           : beuh := '';
      nosPOINT_TOPO       : beuh := '';
      nosPOINT_FIXE       : beuh := '';
      nosCORRESPONDANCE   : beuh := '';
      nosFISTULEUSE       : beuh := '';
      nosCONCRETION_PAROI : beuh := '';
      nosEXCENTRIQUES     : beuh := 'helictite';
      nosSTALACTITES      : beuh := 'stalactite';
      nosCOLONNES         : beuh := '';
      nosSTALAGMITES      : beuh := 'stalagmite';
      nosCRISTAUX         : beuh := '';
      nosFRACTURES        : beuh := '';
      nosCUPULES          : beuh := '';
      nosZEFF             : beuh := '';
      nosARRIVEE_EAU      : beuh := '';
      nosPERTE            : beuh := '';
      nosDESOB            : beuh := '';
    else
      beuh := '';
    end;
    PP  := ConvertGCSCoordsToLocalScrapCoords(BP, MySymbole.Offset.X, MySymbole.Offset.Y);
    AddLineBuffer(Format('      %.3f %.3f %s', [PP.X, PP.Y, beuh]));
  end;
end;

procedure TTherionScrap.MakeTextObject(const MyTexte: TTextObject);
var
  BP: TBaseStation;
  PP: TLocalScrapCoordinates;
  beuh: String;
begin
  // point 451.0 -159.0 station -name 4
  if (FMyDocDessin.GetBasePointByIndex(MyTexte.IDBaseSt, BP)) then
  begin
    PP  := ConvertGCSCoordsToLocalScrapCoords(BP, MyTexte.Offset.X, MyTexte.Offset.Y);
    //AddLineBuffer(Format('      %.3f %.3f %s', [PP.X, PP.Y, beuh]));
  end;
end;


function TTherionScrap.ConvertGCSCoordsToLocalScrapCoords(const QX, QY: double): TLocalScrapCoordinates;
begin
  Result.X  := QX - FBoundingBox.C1.X;
  Result.Y  := QY - FBoundingBox.C1.Y;
end;

function TTherionScrap.ConvertGCSCoordsToLocalScrapCoords(const BP: TBaseStation; const QdX, QdY: double): TLocalScrapCoordinates;
begin
  Result.X  := (BP.PosStation.X + QdX) - FBoundingBox.C1.X;
  Result.Y  := (BP.PosStation.Y + QdY) - FBoundingBox.C1.Y;
end;

procedure TTherionScrap.RecenserBasePointsScrap();
const
  FMT_SER_PT = 's%d_%d'; // s1_0
var
  n, i: Integer;
  EWE: TBaseStation;
  QAT: TToporobotIDStation;
  WU : TTh2ScrapBasepoint;
  PP : TLocalScrapCoordinates;
begin
  pass;
  {
  n := FPtrCenterlines.GetNbBasePoints();
  for i := 0 to n-1 do
  begin
    EWE := FPtrCenterlines.GetBasePoint(i);
    if (IsInBoundingBox(EWE.PosStation.X, EWE.PosStation.Y, FBoundingBox)) then
    begin
      if (EWE.IDStation < 0) then Continue;
      QAT := GetToporobotIDStation(EWE);
      PP := ConvertGCSCoordsToLocalScrapCoords(EWE.PosStation.X, EWE.PosStation.Y);
      WU := MakeTTh2ScrapBasepoint(PP.X, PP.Y, Format(FMT_SER_PT, [QAT.aSerie, QAT.aStation]));
      AddBasePointScrap(WU);
    end;
  end;
  //}
end;


function TTherionScrap.GetTamponSortie: TStringList;
begin
  Result := FTamponSortie;
end;

procedure TTherionScrap.Finaliser;
begin
  try
    FListeMorphingBasePoints.ClearListe();
    FTamponSortie.Clear;
  finally
    FreeAndNil(FListeMorphingBasePoints);//       FListeMorphingBasePoints.Free;
    FreeAndNil(FTamponSortie);//       FTamponSortie.Free;
  end;
end;

procedure TTherionScrap.BeginTH2File;
begin
  AddLineBuffer('encoding  utf-8');
  AddLineBuffer(Format('# Filename: %s - created %s', [FTH2FileName, DateTimeToDateSql(Now())]));
  AddLineBuffer('language ' + rsCODE_ISO_COUNTRY);
end;

procedure TTherionScrap.EndTH2File;
begin
  AddLineBuffer('# End of file: ' + FTH2FileName);
end;


end.

