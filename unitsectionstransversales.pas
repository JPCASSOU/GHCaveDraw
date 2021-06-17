unit unitSectionsTransversales;
// sections transversales
// 07/10/2015: Démarrage


{$INCLUDE CompilationParameters.inc}
interface
uses
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr
  {$ENDIF}
  , GHCD_Types
  , Graphics
  , UnitListesSimplesWithGeneriques
  , Clipper
  //, UnitGraphismesProvisoires
  , math
  , FastGEO // pour calcul de distance
  , GeneralFunctions
  , UnitDocDessin
  , Classes, SysUtils;

type TViseeRayonnante = record
  Longueur: double;
  Azimut  : double;
  Pente   : double;
  dx      : double;
  dy      : double;
  dz      : double;
  dp      : double;
  AsSection: boolean;
end;
type TArrayViseesRayonnantes = array of TViseeRayonnante;
 // 'hérisson' des visées radiantes depuis un point topo
type THerissonViseesRadiantes = class
  private  // point topo de base
    FDocuTopo : TDocumentDessin;
    FBasePoint: TBaseStation;
    FAngleOrientationSection: double;

    FListeViseesRayonnantes: TListeSimple<TViseeRayonnante>;


    procedure MakeTViseesRayonnantesFromLRUD(const BP: TBaseStation; out L, R, U, D: TViseeRayonnante);
    procedure PurgerListeViseesRayonnantes(); inline;
    function  ExtractViseesRayonnantesForBP(): boolean;
    procedure AddViseeRayonnante(const VR: TViseeRayonnante); inline;

    function  MakeTViseeRayonnante(const BP: TBaseStation): TViseeRayonnante;
  public
    function  Initialiser(const FD: TDocumentDessin;
                          const BS: TBaseStation;
                          const AngleOrientationSection: double): boolean;
    procedure Finaliser();
    function  getBaseStation(): TBaseStation;
    function  getAngleOrientationSection(): double;
    procedure setAngleOrientationSection(const q: double);

    function  getViseeRayonnante(const Idx: integer): TViseeRayonnante; inline;
    procedure PutViseeRayonnante(const Idx: integer; V: TViseeRayonnante); inline;
    function  getNbViseesRayonnantes(): integer;  inline;
    procedure CalcProjPZVisee(const r, Az, phi: double; out PP, PZ: double);
    procedure CalcProjsPZAndSetMiniMaxi();
end;

// Objet CrossSection
type TCrossSection = class
  private
    FBaseStation       : TBaseStation;
    FIDGroupe          : TIDGroupeEntites;
    FDecalageXY        : TPoint2Df; // décalage XY entre le point topo et le centre de la section transversale
    FListeCourbes      : TListeSimple<TCrossSectionCourbe>;
    FListePolygones    : TListeSimple<TCrossSectionPolygone>;
    FListeSimpleLignes : TListeSimple<TCrossSectionSimpleLigne>;
    FListeTextes       : TListeSimple<TCrossSectionTexte>;
    // listes des styles d'objets
    FListeLinesStyles   : TListeSimple<TStyleLigne>;
    FListeCurveStyles   : TListeSimple<TStyleCourbe>;
    FListePolygonStyles : TListeSimple<TStylePolygone>;
    FListeTextStyles    : TListeSimple<TStyleTexte>;

    // scrap englobant
    FScrapEnglobant     : TCrossSectionPolygone;

    procedure PurgerListeStyles();
    procedure SetBaseStation(const B: TBaseStation);
    procedure SetDefaultStyles();
    procedure ViderListes();

  public
    function  Initialiser(const B: TBaseStation): boolean;
    procedure SetIDGroupe(const G: TIDGroupeEntites);
    procedure SetDecalageXY(const QX, QY: double);
    procedure Finaliser();
    function  Serialiser(out LS: TStringList): boolean;
    function  Analyser(const LS: TStringList; out NumeroLigneEnErreur: integer; out MessageErreur: string): boolean;

    function  getScrapEnglobant(): TCrossSectionPolygone;
    function  getNbCourbes(): integer;        inline;
    function  getNbPolygones(): integer;      inline;
    function  getNbSimplesLignes(): integer;  inline;
    function  getNbTextes(): Integer;         inline;

    procedure addCourbe(const Q: TCrossSectionCourbe)              ; inline;
    procedure addPolygone(const Q: TCrossSectionPolygone)          ; inline;
    procedure addSimpleLigne(const Q: TCrossSectionSimpleLigne)    ; inline;
    procedure addSimpleLigneByValues(const QIDStyle: TNatureObjetLigne; const QExtr1X, QExtr1Y, QExtr2X, QExtr2Y: double);
    procedure addTexte(const Q: TCrossSectionTexte)                ; inline;
    procedure addTexteByValues(const QIDStyle: TNatureObjetTexte; const QAlign: byte; const QX, QY: double; const QText: string);


    function  getCourbe(const Idx: integer): TCrossSectionCourbe              ; inline;
    function  getPolygone(const Idx: integer): TCrossSectionPolygone          ; inline;
    function  getSimpleLigne(const Idx: integer): TCrossSectionSimpleLigne    ; inline;
    function  getTexte(const Idx: integer): TCrossSectionTexte                ; inline;

    procedure putCourbe(const Idx: integer; const Q: TCrossSectionCourbe)              ; inline;
    procedure putPolygone(const Idx: integer; const Q: TCrossSectionPolygone)          ; inline;
    procedure putSimpleLigne(const Idx: integer; const Q: TCrossSectionSimpleLigne)    ; inline;
    procedure putTexte(const Idx: integer; const Q: TCrossSectionTexte)                ; inline;

    procedure RemoveCourbe(const Idx: integer)             ; inline;
    procedure RemovePolygone(const Idx: integer)           ; inline;
    procedure RemoveSimpleLigne(const Idx: integer)        ; inline;
    procedure RemoveTexte(const Idx: integer)              ; inline;

    function  FindIdxNearCourbeToXY(const QXY: TPoint2Df): integer;
    function  FindIdxNearPolygoneToXY(const QXY: TPoint2Df): integer;
    function  FindIdxNearSimpleLigneToXY(const QXY: TPoint2Df): integer;
    function  FindIdxNearTexteToXY(const QXY: TPoint2Df): integer;

    procedure AddStylePolygone(const LS: TStylePolygone); inline;
    function  GetStylePolygone(const Idx: integer): TStylePolygone; inline;
    procedure PutStylePolygone(const Idx: integer; const LS: TStylePolygone); inline;
    function  GetNbStylesPolygones(): integer; inline;

    procedure AddStyleCourbe(const LS: TStyleCourbe); inline;
    function  GetStyleCourbe(const Idx: integer): TStyleCourbe; inline;
    procedure PutStyleCourbe(const Idx: integer; const LS: TStyleCourbe); inline;
    function  GetNbStylesCourbe(): integer; inline;

    procedure AddStyleTexte(const LS: TStyleTexte);
    function  GetNbStylesTexte: integer;
    function  GetStyleTexte(const Idx: integer): TStyleTexte;
    procedure PutStyleTexte(const Idx: integer; const LS: TStyleTexte);

    procedure AddStyleLigne(const LS: TStyleLigne);
    function  GetNbStylesLigne: integer;
    function  GetStyleLigne(const Idx: integer): TStyleLigne;
    procedure PutStyleLigne(const Idx: integer; const LS: TStyleLigne);
    // crée un scrap englobant avec les courbes
    function  MakeScrapEnglobant(): boolean;
    // clipping par le scrap
    function  ClippingByScrapEnglobant(): boolean;
end;


type  TOperationsCSG = class(TList)
  private
    FNatureObjetPolygone0: TNatureObjetPolygone;
    FNatureObjetPolygone1: TNatureObjetPolygone;

    FPath0: TArrayPoints2Df;
    FPath1: TArrayPoints2Df;
    FPolygoneResult: TCrossSectionPolygone;

    function  MakeTGeoPolygon2D(const QPath: TArrayPoints2Df): TGeoPolygon2D;
    function  MakeTPathFromPoly(const P: TArrayPoints2Df; out QPath: TPath): boolean;
  public
    function  Initialise(): boolean;
    procedure Finalise();

    procedure SetPoly0(const P: TCrossSectionPolygone);
    procedure SetPoly1(const P: TCrossSectionPolygone);


    function  GetPolygoneResult(): TCrossSectionPolygone;
    // opérations booléennes
    function  ExecuteCSG(const ClipType: TClipType): boolean;
end;

function IntersectTVertexPolygonArray(const Poly1, Poly2: TArrayPoints2Df): boolean;

implementation
uses DGCDummyUnit;


const QMULTIPLICATEUR = 100;
function IntersectTVertexPolygonArray(const Poly1, Poly2: TArrayPoints2Df): boolean;
var
  Poly1Trailer, i, j, Poly2Trailer: Integer;
  P1, P2, P3, P4: TGeoPoint2D;
begin
  Result := False;
  if (Length(Poly1) < 3) or (Length(Poly2) < 3) then exit;
  Poly1Trailer := Length(Poly1) - 1;
  for i := 0 to Length(Poly1) - 1 do
  begin
    Poly2Trailer := Length(Poly2) - 1;
    for j := 0 to Length(Poly2) - 1 do
    begin
      P1.x := Poly1[i].X;
      P1.y := Poly1[i].Y;
      P2.x := Poly1[Poly1Trailer].X;
      P2.y := Poly1[Poly1Trailer].Y;
      P3.x := Poly2[j].X;
      P3.y := Poly2[j].Y;
      P4.x := Poly2[Poly2Trailer].X;
      P4.y := Poly2[Poly2Trailer].Y;
      if (FastGEO.Intersect(P1, P2, P3, P4)) then Exit(True);
      Poly2Trailer := j;
    end;
    Poly1Trailer := i;
  end;
  end;




(* Class: THerissonViseesRadiantes *)

procedure THerissonViseesRadiantes.AddViseeRayonnante(const VR: TViseeRayonnante);
begin
  FListeViseesRayonnantes.AddElement(VR);
end;


function THerissonViseesRadiantes.getViseeRayonnante(const Idx: integer): TViseeRayonnante;
begin
  Result := FListeViseesRayonnantes.GetElement(Idx);
end;

function THerissonViseesRadiantes.ExtractViseesRayonnantesForBP(): boolean;
var
  i, Nb: Integer;
  EWE: TBaseStation;
  LL, RR, UU, DD, VR: TViseeRayonnante;
begin
  AfficherMessage(Format('%s.ExtractViseesRayonnantesForBP(): %d', [ClassName, FBasePoint.IDStation]));
  Result := false;
  PurgerListeViseesRayonnantes();

  // ajout des LRUD, antennes par défaut
  MakeTViseesRayonnantesFromLRUD(FBasePoint, LL, RR, UU, DD);
  AddViseeRayonnante(LL);
  AddViseeRayonnante(RR);
  AddViseeRayonnante(UU);
  AddViseeRayonnante(DD);
  // extraction des visées radiantes
  Nb := FDocuTopo.GetNbBaseStations();
  //AfficherMessageErreur(Format('-- Scan des %d basepoints', [FDocuTopo.GetNbBaseStations()]));
  for i := 1 to Nb - 1 do
  begin
    EWE := FDocuTopo.GetBaseStation(i);
    if (CompareIDStations(EWE.IDStation, FBasePoint.IDStation)) then AddViseeRayonnante(MakeTViseeRayonnante(EWE));

  end;
end;


procedure THerissonViseesRadiantes.Finaliser();
begin
  try
    FListeViseesRayonnantes.ClearListe();

  finally
    //FListeViseesRayonnantes.Free;
  end;
end;

function THerissonViseesRadiantes.getAngleOrientationSection: double;
begin
  result := FAngleOrientationSection;
end;

procedure THerissonViseesRadiantes.setAngleOrientationSection(const q: double);
begin
 FAngleOrientationSection := q;
end;


function THerissonViseesRadiantes.getBaseStation: TBaseStation;
begin
  Result := FBasePoint;
end;


function THerissonViseesRadiantes.getNbViseesRayonnantes: integer;
begin
  Result := FListeViseesRayonnantes.GetNbElements();
end;



function THerissonViseesRadiantes.Initialiser(const FD: TDocumentDessin;
                                              const BS: TBaseStation;
                                              const AngleOrientationSection: double): boolean;
begin
  result := false;
  FBasePoint := BS;
  FAngleOrientationSection := AngleOrientationSection;
  // init du scrap englobant
  FListeViseesRayonnantes := TListeSimple<TViseeRayonnante>.Create;
  FListeViseesRayonnantes.ClearListe();
  try
    FDocuTopo  := FD;
    ExtractViseesRayonnantesForBP();
    CalcProjsPZAndSetMiniMaxi();
    result := true;
  except
  end;
end;

function THerissonViseesRadiantes.MakeTViseeRayonnante(const BP: TBaseStation): TViseeRayonnante;
begin
  Result.dx       := BP.PosStation.X - BP.PosExtr0.X;
  Result.dy       := BP.PosStation.Y - BP.PosExtr0.Y;
  Result.dz       := BP.PosStation.Z - BP.PosExtr0.Z;
  Result.dp       := hypot(Result.dx, Result.dy);
  Result.Longueur := hypot(Result.dp, Result.dz);
  Result.Pente    := radtodeg(ArcTan2(Result.dz, Result.dp));
  Result.Azimut   := GetAzimut(Result.dx,Result.dy, 360.00);
  result.AsSection := false;

end;
(*
procedure THerissonViseesRadiantes.MakeTViseesRayonnantesFromLRUD(const BP: TBaseStation; out L, R, U, D: TViseeRayonnante);
begin
  L.dx := BP.PosPG.X - BP.PosStation.X;
  L.dy := BP.PosPG.Y - BP.PosStation.Y;
  L.dz := 0.00;
  L.dp := Hypot(L.dx, L.dy);
  L.Longueur := L.dp;
  L.Azimut   := GetAzimut(L.dx, L.dy, 360.00);
  L.Pente    := 0.00;
  L.AsSection := true;
  R.dx := BP.PosPD.X - BP.PosStation.X;
  R.dy := BP.PosPD.Y - BP.PosStation.Y;
  R.dz := 0.00;
  R.dp := Hypot(R.dx, R.dy);
  R.Longueur := R.dp;
  R.Azimut   := GetAzimut(R.dx, R.dy, 360.00);
  R.Pente    := 0.00;
  R.AsSection := true;
  U.dx        := 0.00;
  U.dy        := abs(BP.PosPD.Z - BP.PosStation.Z);
  U.dz        := 0.00;
  U.Longueur := U.dy;
  U.Azimut   := 0.00;
  U.Pente    := 90.00;
  D.dx        := 0.00;
  D.dy        := abs(BP.PosPG.Z - BP.PosStation.Z);
  D.dz        := 0.00;
  D.Longueur := U.dy;
  D.Azimut   := 0.00;
  D.Pente    := -90.00;
end;
//*)

procedure THerissonViseesRadiantes.MakeTViseesRayonnantesFromLRUD(const BP: TBaseStation; out L, R, U, D: TViseeRayonnante);
  function MiouMiou(const qdx, qdy, qdz: double; const qPente: double; const qAsSection: boolean = true): TViseeRayonnante;
  begin
    Result.dx := qdx;
    Result.dy := qdy;
    Result.dz := qdz;
    Result.dp := Hypot(qdx, qdy);
    Result.Longueur := Hypot(Result.dp, qdz);
    Result.Azimut   := GetAzimut(qdx, qdy, 360.00);
    Result.Pente    := qPente;
    Result.AsSection := qAsSection;
  end;
begin
  L := MiouMiou(BP.PosPG.X - BP.PosStation.X, BP.PosPG.Y - BP.PosStation.Y, 0.00, 0.00, True);
  R := MiouMiou(BP.PosPD.X - BP.PosStation.X, BP.PosPD.Y - BP.PosStation.Y, 0.00, 0.00, True);
  U := MiouMiou(0.00, abs(BP.PosPD.Z - BP.PosStation.Z), 0.00,  90.00, True);
  D := MiouMiou(0.00, abs(BP.PosPG.Z - BP.PosStation.Z), 0.00, -90.00, True);
end;
//*)

procedure THerissonViseesRadiantes.PurgerListeViseesRayonnantes;
begin
  FListeViseesRayonnantes.ClearListe();
end;

procedure THerissonViseesRadiantes.PutViseeRayonnante(const Idx: integer; V: TViseeRayonnante);
begin
  FListeViseesRayonnantes.PutElement(Idx, V);
end;


procedure THerissonViseesRadiantes.CalcProjPZVisee(const r, Az, phi: double; out PP, PZ: double);
var
  incl, azc, PH: double;
  PX, PY, ca, sa: ValReal;
begin
  incl := degtorad(phi);
  azc  := degtorad(Az);
  PH   := r * cos(incl);
  PZ   := r * sin(incl);
  PX   := PH * cos(azc);
  PY   := PH * sin(azc);
  // rotation
  ca := cos(degtorad(FAngleOrientationSection));
  sa := sin(degtorad(FAngleOrientationSection));

  // X  =   x * cos(a)  + y * sin(a)
  // Y  =  -x * sin(a)  + y * cos(a)
  PP := PX * ca + PY * sa;
end;


procedure THerissonViseesRadiantes.CalcProjsPZAndSetMiniMaxi();
var
  dx, dy, PP, PZ: Double;
  n, i: Integer;
  V: TViseeRayonnante;
begin
  n := getNbViseesRayonnantes();
  for i := 0 to n - 1 do
  begin
    V := getViseeRayonnante(i);
    CalcProjPZVisee(V.Longueur, V.Azimut, V.Pente, PP, PZ);
    //AfficherMessageErreur(Format('%d: %.2f, %.2f, %.2f, %.3f, %.3f', [i, V.Longueur, V.Azimut, V.Pente, PP, PZ]));
  end;
end;
//******************************************************************************
(* Class: TCrossSection                                                       *)

function TCrossSection.Initialiser(const B: TBaseStation): boolean;
begin
  result := false;
  FBaseStation       := B;
  FDecalageXY.X      := 0.00; FDecalageXY.Y      := 0.00;
  FListeCourbes      := TListeSimple<TCrossSectionCourbe>.Create;
  FListePolygones    := TListeSimple<TCrossSectionPolygone>.Create;
  FListeSimpleLignes := TListeSimple<TCrossSectionSimpleLigne>.Create;
  FListeTextes       := TListeSimple<TCrossSectionTexte>.Create;

  FListeCurveStyles   := TListeSimple<TStyleCourbe>.Create;
  FListePolygonStyles := TListeSimple<TStylePolygone>.Create;
  FListeLinesStyles   := TListeSimple<TStyleLigne>.Create;
  FListeTextStyles    := TListeSimple<TStyleTexte>.Create;
  // le scrap englobant
  FScrapEnglobant.IDStylePolygone := nopSCRAP;
  SetLength(FScrapEnglobant.Vertexes, 0);
  try
    FListeCourbes.ClearListe();
    FListePolygones.ClearListe();
    FListeSimpleLignes.ClearListe();
    FListeTextes.ClearListe();

    SetDefaultStyles();
    Result := true;
  except

  end;

end;
procedure TCrossSection.SetBaseStation(const B: TBaseStation);
begin
  FBaseStation := B;
end;

procedure TCrossSection.SetDecalageXY(const QX, QY: double);
begin
  FDecalageXY.X := QX; FDecalageXY.Y := QY;
end;

procedure TCrossSection.ViderListes();
begin
  FListeCourbes.ClearListe();
  FListePolygones.ClearListe();
  FListeSimpleLignes.ClearListe();
  FListeTextes.ClearListe();
end;

procedure TCrossSection.Finaliser();
begin
  try
    ViderListes();
    PurgerListeStyles();
    SetLength(FScrapEnglobant.Vertexes, 0);
  finally
    (*

    FreeAndNil(FListeCourbes);// FListeCourbes.Free;
    FreeAndNil(FListePolygones);// FListePolygones.Free;
    FreeAndNil(FListeSimpleLignes);// FListeSimpleLignes.Free;
    FreeAndNil(FListeTextes);// FListeTextes.Free;

    FreeAndNil(FListeCurveStyles);// FListeCurveStyles.Free;
    FreeAndNil(FListePolygonStyles);// FListePolygonStyles.Free;
    FreeAndNil(FListeLinesStyles);// FListeLinesStyles.Free;
    FreeAndNil(FListeTextStyles);// FListeTextStyles.Free;

    //*)
  end;
end;

function TCrossSection.FindIdxNearCourbeToXY(const QXY: TPoint2Df): integer;
var
  n, i, s, NbP: Integer;
  R : double;
  EWE: TCrossSectionCourbe;
  V1, V2: TPoint2Df;

  S1, S2, S3: TGeoSegment2D;
  AC: TCrossSectionArcCourbe;
  procedure QQ(const QS: TGeoSegment2D; const QIdx: integer);
  var
    QD: TGeoFloat;
    QP: TGeoPoint2D;
  begin
    QP.x  := QXY.X;
    QP.y  := QXY.Y;
    QD := Distance(QP, QS);
    if (QD < R) then
    begin
      R := QD;
      Result := i;
    end;
  end;
begin
  Result := -1;
  //AfficherMessage(format('%s.FindIdxNearCourbeToXY(%.3f, %.3f)', [ClassName, QXY.X, QXY.Y]));
  R := 1E20;
  n := getNbCourbes();
  if (n = 0) then Exit;

  for i := 0 to n - 1  do
  begin
    EWE := getCourbe(i);
    NbP := high(EWE.Arcs);
    for s := 0 to NbP do
    begin
      AC := EWE.Arcs[s];

      S1[1].x := AC.CoordsP1.X;
      S1[1].y := AC.CoordsP1.Y;
      S1[2].x := AC.CoordsP1.X + AC.TangP1.X;
      S1[2].y := AC.CoordsP1.Y + AC.TangP1.Y;

      S2[1].x := S1[2].x;
      S2[1].y := S1[2].y;
      S2[2].x := AC.CoordsP2.X + AC.TangP2.X;
      S2[2].y := AC.CoordsP2.Y + AC.TangP2.Y;

      S3[1].x := S2[2].x;
      S3[1].y := S2[2].y;
      S3[2].x := AC.CoordsP2.X;
      S3[2].y := AC.CoordsP2.Y;

      QQ(S1, i);
      QQ(S2, i);
      QQ(S3, i);
    end;
  end;
end;
function TCrossSection.FindIdxNearPolygoneToXY(const QXY: TPoint2Df): integer;
var
  n, i, NbP, s: Integer;
  R : double;
  EWE: TCrossSectionPolygone;
  QS   : TGeoSegment2D;
  V1, V2: TPoint2Df;
  QP: TGeoPoint2D;
  DD: TGeoFloat;
begin
  Result := -1;
  //AfficherMessage(format('%s.FindIdxNearPolygoneToXY(%.3f, %.3f)', [ClassName, QXY.X, QXY.Y]));
  R := 1E20;
  n := getNbPolygones();
  if (n = 0) then Exit;
  QP.x  := QXY.X; QP.y  := QXY.Y;
  for i := 0 to n - 1 do
  begin
    EWE := getPolygone(i);
    NbP := high(EWE.Vertexes);
    for s := 1 to NbP do
    begin
      V1 := EWE.Vertexes[s - 1];
      V2 := EWE.Vertexes[s];
      QS[1].x := V1.x;     QS[1].y := V1.y;
      QS[2].x := V2.x;     QS[2].y := V2.y;
      DD := Distance(QP, QS);
      if (DD < R) then
      begin
        R := DD;
        Result := i;
      end;
    end;
  end;
end;
function TCrossSection.FindIdxNearSimpleLigneToXY(const QXY: TPoint2Df): integer;
var
  n, i : Integer;
  R, DD: double;
  EWE  : TCrossSectionSimpleLigne;
  QP   : TGeoPoint2D;
  QS   : TGeoSegment2D;
begin
  Result := -1;
  //AfficherMessage(format('%s.FindIdxNearSimpleLigneToXY(%.3f, %.3f)', [ClassName, QXY.X, QXY.Y]));
  R := 1E20;
  n := getNbSimplesLignes();
  if (n = 0) then Exit;
  QP.x  := QXY.X; QP.y  := QXY.Y;
  for i := 0 to n - 1 do
  begin
    EWE := getSimpleLigne(i);
    QS[1].x := EWE.Extr1.X    ; QS[1].y := EWE.Extr1.Y;
    QS[2].x := EWE.Extr2.X    ; QS[2].y := EWE.Extr2.Y;
    DD := Distance(QP, QS);
    if (DD < R) then
    begin
      R := DD;
      Result := i;
    end;
  end;
end;
function TCrossSection.FindIdxNearTexteToXY(const QXY: TPoint2Df): integer;
var
  n, i: Integer;
  R, dist : double;
  EWE: TCrossSectionTexte;
begin
  Result := -1;
  //AfficherMessage(format('%s.FindIdxNearTexteToXY(%.3f, %.3f)', [ClassName, QXY.X, QXY.Y]));
  R := 1E20;
  n := getNbTextes();
  if (n = 0) then Exit;
  for i := 0 to n - 1 do
  begin
    EWE  := getTexte(i);
    dist := Hypot(EWE.PosX - QXY.X, EWE.PosY - QXY.Y);
    if (dist < R) then
    begin
      R := dist;
      Result := i;
    end;
  end;
end;



procedure TCrossSection.SetIDGroupe(const G: TIDGroupeEntites);
begin
  FIDGroupe := G;
end;

function TCrossSection.getNbCourbes(): integer;
begin
  Result := FListeCourbes.GetNbElements();
end;

function TCrossSection.getNbPolygones(): integer;
begin
  Result := FListePolygones.GetNbElements();
end;

function TCrossSection.getNbSimplesLignes(): integer;
begin
  Result := FListeSimpleLignes.GetNbElements();
end;





function TCrossSection.getNbTextes(): Integer;
begin
  Result := FListeTextes.GetNbElements();
end;

procedure TCrossSection.addCourbe(const Q: TCrossSectionCourbe);
begin
  FListeCourbes.AddElement(Q);
end;

procedure TCrossSection.addPolygone(const Q: TCrossSectionPolygone);
begin
  FListePolygones.AddElement(Q);
end;

procedure TCrossSection.addSimpleLigne(const Q: TCrossSectionSimpleLigne);
begin
  FListeSimpleLignes.AddElement(Q);
end;

procedure TCrossSection.addSimpleLigneByValues(const QIDStyle: TNatureObjetLigne; const QExtr1X, QExtr1Y, QExtr2X, QExtr2Y: double);
var
  EWE: TCrossSectionSimpleLigne;
begin
  EWE.IDStyleLigne := QIDStyle;
  EWE.Extr1.X      := QExtr1X;
  EWE.Extr1.Y      := QExtr1Y;
  EWE.Extr2.X      := QExtr2X;
  EWE.Extr2.Y      := QExtr2Y;
  addSimpleLigne(EWE);
end;




procedure TCrossSection.addTexte(const Q: TCrossSectionTexte);
begin
  FListeTextes.AddElement(Q);
end;

procedure TCrossSection.addTexteByValues(const QIDStyle: TNatureObjetTexte; const QAlign: byte; const QX, QY: double; const QText: string);
var
  EWE: TCrossSectionTexte;
begin
  EWE.IDStyleTexte := QIDStyle;
  EWE.Alignment    := QAlign;
  EWE.PosX         := QX;
  EWE.PosY         := QY;
  EWE.Text         := QText;
  addTexte(EWE);
end;

function TCrossSection.Analyser(const LS: TStringList; out NumeroLigneEnErreur: integer; out MessageErreur: string): boolean;
type TTypeDeSubsection = (ttsNONE, ttsCOURBES, ttsPOLYGONES, ttsLIGNES, ttsTEXTES);
var
  EWE: TGHStringArray;
  i, n, nb, m, j: Integer;
  WU: String;
  CrossSectionFound: boolean;
  YaScrapEnglobant, YaCourbe, YaPolygone, YaSimpleLigne, YaTexte: Boolean;
  MyCourbe: TCrossSectionCourbe;
  TypeDeSubsection : TTypeDeSubsection;
  ACDC: TCrossSectionArcCourbe;
  MyPolygone: TCrossSectionPolygone;
  MySimpleLigne: TCrossSectionSimpleLigne;
  MySimpleTexte: TCrossSectionTexte;
  VX: TPoint2Df;
  lol: float;
  function HasKeywordOccurred(const QRow: TGHStringArray; const QKeyword: string): boolean; inline;
  begin
    Result := (Trim(EWE[0]) = QKeyword);
  end;

begin
  MessageErreur:= '';
  NumeroLigneEnErreur := -1;
  CrossSectionFound := false;
  AfficherMessage(Format('%s.Analyser(): %d lines', [ClassName, LS.Count]));
  SetLength(FScrapEnglobant.Vertexes, 0);
  self.ViderListes();
  YaScrapEnglobant := false;
  YaCourbe         := false;
  YaPolygone       := false;
  YaSimpleLigne    := false;
  YaTexte          := false;
  for i := 0 to LS.Count - 1 do
  begin
    WU := Trim(LS.Strings[i]);
    if (WU = '') then
    begin
      AfficherMessage(Format('%d: Cette ligne est vide', [i]));
      Continue;
    end;
    if (WU[1] = '#') then
    begin
      AfficherMessage(Format('%d: Cette ligne est un commentaire', [i]));
      Continue;
    end;
    EWE := Split(WU, #9);
    //#Parameters: IDCrossSection, IDGroupe, IDBasePoint, OffsetX, OffsetY
    //   crosssection	1	0	100130	-23.250	13.780
    if (HasKeywordOccurred(EWE, CROSS_SECTION)) then
    begin
      TypeDeSubsection := ttsNONE;
      CrossSectionFound := True;
      AfficherMessage(Format('%d: CrossSection found', [i]));
      self.FIDGroupe := StrToInt64Def(EWE[2], 0);
      {$IFDEF TIDBASEPOINT_AS_TEXT}
      FBaseStation.IDStation := Trim(EWE[3]);
      {$ELSE}
      FBaseStation.IDStation := StrToInt64Def(EWE[3], 0);
      {$ENDIF TIDBASEPOINT_AS_TEXT}

      SetDecalageXY(ConvertirEnNombreReel(EWE[4], 0.00),
                    ConvertirEnNombreReel(EWE[5], 0.00));
    end;
    if (CrossSectionFound) then
    begin
      if (HasKeywordOccurred(EWE, CROSS_SECTION_COURBES)) then
      begin
        TypeDeSubsection := ttsCOURBES;
        AfficherMessage(Format('%d: Courbes section found', [i]));
        YaCourbe := false;
      end;
      if (HasKeywordOccurred(EWE, END_CROSS_SECTION_COURBES)) then
      begin
        TypeDeSubsection := ttsNONE;
        AfficherMessage(Format('%d: End Courbes section found', [i]));
        YaCourbe := false; // ya plus courbe
      end;
      if (HasKeywordOccurred(EWE, CROSS_SECTION_POLYGONES)) then
      begin
        TypeDeSubsection := ttsPOLYGONES;
        AfficherMessage(Format('%d: Polygones section found', [i]));
      end;
      if (HasKeywordOccurred(EWE, END_CROSS_SECTION_POLYGONES)) then
      begin
        TypeDeSubsection := ttsNONE;
        AfficherMessage(Format('%d: End Polygones section found', [i]));
      end;
      if (HasKeywordOccurred(EWE, CROSS_SECTION_LINES)) then
      begin
        TypeDeSubsection := ttsLIGNES;
        AfficherMessage(Format('%d: SimplesLines section found', [i]));
      end;
      if (HasKeywordOccurred(EWE, END_CROSS_SECTION_LINES)) then
      begin
        TypeDeSubsection := ttsNONE;
        AfficherMessage(Format('%d: End SimplesLines section found', [i]));
      end;
      if (HasKeywordOccurred(EWE, CROSS_SECTION_TEXTES)) then
      begin
        TypeDeSubsection := ttsTEXTES;
        AfficherMessage(Format('%d: Texts section found', [i]));
      end;
      if (HasKeywordOccurred(EWE, END_CROSS_SECTION_TEXTES)) then
      begin
        TypeDeSubsection := ttsNONE;
        AfficherMessage(Format('%d: End Texts section found', [i]));
      end;
      // les courbes
      if (TypeDeSubsection = ttsCOURBES) then
      begin
        if (HasKeywordOccurred(EWE, CROSS_SECTION_A_COURBE)) then
        begin
          YaCourbe := True;
          AfficherMessage(Format('%d: Courbe found', [i]));
          MyCourbe.IDStyleCourbe := TNatureObjetCourbe(strToIntDef(EWE[1], 0));
          SetLength(MyCourbe.Arcs, 0);
        end;
        // arc de courbe
        if (YaCourbe AND HasKeywordOccurred(EWE, CROSS_SECTION_ARC_COURBE)) then
        begin
          YaCourbe := True;
          AfficherMessage(Format('-- %d: Arc courbe found', [i]));
          SetLength(MyCourbe.Arcs, 1 + Length(MyCourbe.Arcs));
          n := High(MyCourbe.Arcs);
          ACDC := MyCourbe.Arcs[n];
          ACDC.CoordsP1.setFrom(ConvertirEnNombreReel(EWE[1], 0.00), ConvertirEnNombreReel(EWE[2], 0.00));
          ACDC.TangP1.setFrom(ConvertirEnNombreReel(EWE[3], 0.00), ConvertirEnNombreReel(EWE[4], 0.00));
          ACDC.TangP2.setFrom(ConvertirEnNombreReel(EWE[5], 0.00), ConvertirEnNombreReel(EWE[6], 0.00));
          ACDC.CoordsP2.setFrom(ConvertirEnNombreReel(EWE[7], 0.00), ConvertirEnNombreReel(EWE[8], 0.00));
          MyCourbe.Arcs[n] := ACDC;
        end;
        if (HasKeywordOccurred(EWE, END_CROSS_SECTION_A_COURBE)) then
        begin
          AfficherMessage(Format('%d: End Courbe found = add to ', [i]));
          if (YaCourbe) then
          begin
            self.addCourbe(MyCourbe);
            YaCourbe := false;
          end;
        end;
      end;
      // les polygones
      if (TypeDeSubsection = ttsPOLYGONES) then
      begin
        // CSPolybezier	1	1
        if (HasKeywordOccurred(EWE, CROSS_SECTION_A_POLYGONE)) then
        begin
          YaPolygone := True;
          AfficherMessage(Format('%d: Polygone found', [i]));
          MyPolygone.IDStylePolygone := TNatureObjetPolygone(strToIntDef(EWE[1], 0));
          SetLength(MyPolygone.Vertexes, 0);
        end;
        // sommet de polygone
        // CSVertex	-1.561	-0.714
        if (YaPolygone AND HasKeywordOccurred(EWE, CROSS_SECTION_POLY_VERTEX)) then
        begin
          YaPolygone := True;
          AfficherMessage(Format('-- %d: Arc courbe found', [i]));
          SetLength(MyPolygone.Vertexes, 1 + Length(MyPolygone.Vertexes));
          n   := High(MyPolygone.Vertexes);
          VX  := MyPolygone.Vertexes[n];
          VX.setFrom(ConvertirEnNombreReel(EWE[1], 0.00), ConvertirEnNombreReel(EWE[2], 0.00));
          MyPolygone.Vertexes[n] := VX;
        end;
        if (HasKeywordOccurred(EWE, END_CROSS_SECTION_A_POLYGONE)) then
        begin
          AfficherMessage(Format('%d: End Courbe found = add to ', [i]));
          if (YaPolygone) then
          begin
            self.addPolygone(MyPolygone);
            YaPolygone := false;
          end;
        end;
      end;
      // les lignes
      (*
      CSLines
        CSSimpleLine	1	-12.255	-9.881	11.022	3.274
        CSSimpleLine	4	-2.854	-3.250	1.001	6.660
        CSSimpleLine	1	-18.255	3.325	8.022	0.274
        CSSimpleLine	1	-1.666	-1.881	12.022	0.365
      endCSLines
      //*)

      if (TypeDeSubsection = ttsLIGNES) then
      begin
        if (HasKeywordOccurred(EWE, CROSS_SECTION_SIMPLELINE)) then
        begin
          AfficherMessage(Format('%d: Simple line found ', [i]));
          MySimpleLigne.IDStyleLigne := TNatureObjetLigne(strToIntDef(EWE[1], 0));
          MySimpleLigne.Extr1.setFrom(ConvertirEnNombreReel(EWE[2], 0.00),
                                      ConvertirEnNombreReel(EWE[3], 0.00));
          MySimpleLigne.Extr2.setFrom(ConvertirEnNombreReel(EWE[4], 0.00),
                                      ConvertirEnNombreReel(EWE[5], 0.00));
          // ici, on teste si la ligne est de longueur nulle,
          // ce qui a autant de sens qu'un soutien-gorge 95C chez Jane Brikin
          lol := Hypot(MySimpleLigne.Extr2.X - MySimpleLigne.Extr1.X,
                       MySimpleLigne.Extr2.Y - MySimpleLigne.Extr1.Y);
          if (lol > 0.001) then
          begin
            self.addSimpleLigne(MySimpleLigne);
          end;
        end;
      end;


      // les textes
      (*CSTexts
        CSSimpleText	3  7	-4.020	-1.050	miaou
        CSSimpleText	1  7	-3.060	1.050	fatche
        CSSimpleText	4  0	-8.470	11.980	blaireau
      endCSTexts
    //*)
      if (TypeDeSubsection = ttsTEXTES) then
      begin
        if (HasKeywordOccurred(EWE, CROSS_SECTION_SIMPLETEXT)) then
        begin
          AfficherMessage(Format('%d: Simple text found ', [i]));
          MySimpleTexte.IDStyleTexte := TNatureObjetTexte(strToIntDef(EWE[1], 0));
          MySimpleTexte.Alignment    := strToIntDef(EWE[2], 0);
          MySimpleTexte.PosX         := ConvertirEnNombreReel(EWE[3], 0.00) ;
          MySimpleTexte.PosY         := ConvertirEnNombreReel(EWE[4], 0.00) ;
          MySimpleTexte.Text         := Trim(EWE[5]);
          // ici, on teste si le texte est vide
          // ce qui est aussi utile qu'un pansement sur une jambe de bois
          if (MySimpleTexte.Text <> '') then self.addTexte(MySimpleTexte);
        end;
      end;
    end;
    if (HasKeywordOccurred(EWE, END_CROSS_SECTION)) then
    begin
      CrossSectionFound := false;
      AfficherMessage(Format('%d: EndCrossSection found', [i]));
    end;
  end;
  //****************************************************************************
  // traitement des erreurs de fermeture de balises
  //if (CrossSectionFound) then


  //****************************************************************************
  // contrôle
  AfficherMessageErreur('===================================');
  nb := getNbCourbes();
  AfficherMessageErreur(Format('%d courbes', [nb]));
  if (nb > 0) then
  begin
    for i := 0 to Nb -1 do
    begin
      MyCourbe := getCourbe(i);
      m := length(MyCourbe.Arcs);
      AfficherMessageErreur(Format('Courbe %d: style: %d; n=%d', [i, MyCourbe.IDStyleCourbe, m]));
      for j := 0 to m - 1 do
      begin
        ACDC := MyCourbe.Arcs[j];
        WU := Format('  P1 = %.3f, %.3f - T1 = %.3f, %.3f - T2 = %.3f, %.3f - P2 = %.3f, %.3f',
                     [ACDC.CoordsP1.X, ACDC.CoordsP1.Y,
                      ACDC.TangP1.X, ACDC.TangP1.Y,
                      ACDC.TangP2.X, ACDC.TangP2.Y,
                      ACDC.CoordsP1.X, ACDC.CoordsP1.Y
                     ]);
        AfficherMessageErreur(WU);
      end;
    end;
  end;
  nb := getNbPolygones();
  AfficherMessageErreur(Format('%d polygones', [nb]));
  if (nb > 0) then
  begin
    for i := 0 to Nb -1 do
    begin
      MyPolygone := getPolygone(i);
      m := length(MyPolygone.Vertexes);
      AfficherMessageErreur(Format('polygone %d: style: %d; n=%d', [i, MyPolygone.IDStylePolygone, m]));
      for j := 0 to m - 1 do
      begin
        VX := MyPolygone.Vertexes[j];
        WU := Format('  P1 = %.3f, %.3f', [VX.X, VX.Y]);
        AfficherMessageErreur(WU);
      end;
    end;
  end;
  nb := getNbSimplesLignes();
  AfficherMessageErreur(Format('%d simples lignes', [nb]));
  if (nb > 0) then
  begin
    for i := 0 to Nb -1 do
    begin
      MySimpleLigne := getSimpleLigne(i);
      AfficherMessageErreur(Format('SLigne %d: style: %d; x1=%.3f; y1=%.3f; x2=%.3f; y2=%.3f; ',
                                   [i, MySimpleLigne.IDStyleLigne,
                                    MySimpleLigne.Extr1.X, MySimpleLigne.Extr1.Y,
                                    MySimpleLigne.Extr2.X, MySimpleLigne.Extr2.Y]));
    end;
  end;
  nb := getNbTextes();
  AfficherMessageErreur(Format('%d textes', [nb]));
  if (nb > 0) then
  begin
    for i := 0 to Nb -1 do
    begin
      MySimpleTexte := getTexte(i);
      AfficherMessageErreur(Format('STexte %d: style: %d; align=%d; x=%.3f; y=%.3f; text="%s"',
                                   [i, MySimpleTexte.IDStyleTexte, MySimpleTexte.Alignment,
                                    MySimpleTexte.PosX, MySimpleTexte.PosY,
                                    MySimpleTexte.Text]));
    end;
  end;
end;

function TCrossSection.getScrapEnglobant(): TCrossSectionPolygone;
begin
  Result := FScrapEnglobant;
end;

function TCrossSection.getCourbe(const Idx: integer): TCrossSectionCourbe;
begin
  Result := FListeCourbes.GetElement(Idx);
end;

function TCrossSection.getPolygone(const Idx: integer): TCrossSectionPolygone;
begin
  Result := FListePolygones.GetElement(Idx);
end;

function TCrossSection.getSimpleLigne(const Idx: integer): TCrossSectionSimpleLigne;
begin
  Result := FListeSimpleLignes.GetElement(Idx);
end;





function TCrossSection.getTexte(const Idx: integer): TCrossSectionTexte;
begin
  Result := FListeTextes.GetElement(Idx);
end;

procedure TCrossSection.putCourbe(const Idx: integer; const Q: TCrossSectionCourbe);
begin
  FListeCourbes.PutElement(Idx, Q);
end;

procedure TCrossSection.putPolygone(const Idx: integer; const Q: TCrossSectionPolygone);
begin
  FListePolygones.PutElement(Idx, Q);
end;

procedure TCrossSection.putSimpleLigne(const Idx: integer; const Q: TCrossSectionSimpleLigne);
begin
  FListeSimpleLignes.PutElement(Idx, Q);
end;




procedure TCrossSection.putTexte(const Idx: integer; const Q: TCrossSectionTexte);
begin
  FListeTextes.PutElement(Idx, Q);
end;

procedure TCrossSection.RemoveCourbe(const Idx: integer);
begin
  FListeCourbes.RemoveElement(Idx);
end;

procedure TCrossSection.RemovePolygone(const Idx: integer);
begin
  FListePolygones.RemoveElement(Idx);
end;

procedure TCrossSection.RemoveSimpleLigne(const Idx: integer);
begin
  FListeSimpleLignes.RemoveElement(Idx);
end;

procedure TCrossSection.RemoveTexte(const Idx: integer);
begin
  FListeTextes.RemoveElement(Idx);
end;


function TCrossSection.Serialiser(out LS: TStringList): boolean;
var
  n, i: Integer;
  CC: TCrossSectionCourbe;
  PP: TCrossSectionPolygone;
  LL: TCrossSectionSimpleLigne;
  TT: TCrossSectionTexte;
  QAT: string;
  procedure QCommentaire(const Indent: integer; const QS: string);
  begin
    LS.Add(StringOfChar(' ', Indent) + '#' + QS);
  end;
  function NbRealToStr(const V: double): string;
  begin
    Result := Format('%.3f', [V]);
  end;
  procedure QBeginSection(const Indent: integer; const QS: string; const Args: array of string);
  var
    EWE: String;
    n, i: Integer;
  begin
    EWE := StringOfChar(' ', Indent) +  QS;
    n := Length(Args);
    if (n > 0) then
    begin
      for i := 0 to n - 1 do EWE := EWE + #9 + Args[i];
    end;
    LS.Add(EWE);
  end;

  procedure QEndSection(const Indent: integer; const QS: string);
  begin
    LS.Add(StringOfChar(' ', Indent) +  QS);
  end;
  procedure QLnCourbe(const Indent: integer; const C: TCrossSectionCourbe);
  var
    nv, j: Integer;
    AC: TCrossSectionArcCourbe;
    WU: String;
  begin
    nv := 1 + High(C.Arcs);
    if (nv = 0) then Exit;
    QBeginSection(Indent, CROSS_SECTION_A_COURBE, [Format('%d', [C.IDStyleCourbe])]);
    for j := 0 to nv - 1 do
    begin
      AC := C.Arcs[j];
      WU := StringOfChar(' ', Indent + 2) + CROSS_SECTION_ARC_COURBE + #9;
      WU := WU + Format('%.3f' + #9 + '%.3f' + #9 +
                        '%.3f' + #9 + '%.3f' + #9 +
                        '%.3f' + #9 + '%.3f' + #9 +
                        '%.3f' + #9 + '%.3f',
                   [AC.CoordsP1.X, AC.CoordsP1.Y,
                    AC.TangP1.X, AC.TangP1.Y,
                    AC.TangP2.X, AC.TangP2.Y,
                    AC.CoordsP2.X, AC.CoordsP2.Y
                   ]);
      LS.Add(WU);
    end;

    QEndSection(Indent, END_CROSS_SECTION_A_COURBE);
  end;
  procedure QLnPolygone(const Indent: integer; const C: TCrossSectionPolygone);
  var
    nv, j: Integer;
    VX: TPoint2Df;
    WU: String;
  begin
    nv := 1 + High(C.Vertexes);
    if (nv = 0) then Exit;
    QBeginSection(Indent, CROSS_SECTION_A_POLYGONE, [Format('%d', [C.IDStylePolygone])]);
    for j := 0 to nv - 1 do
    begin
      VX := C.Vertexes[j];
      WU := StringOfChar(' ', Indent + 2) + CROSS_SECTION_POLY_VERTEX + #9;
      WU := WU + Format('%.3f' + #9 + '%.3f',
                   [VX.X, VX.Y]);
      LS.Add(WU);
    end;
    QEndSection(Indent, END_CROSS_SECTION_A_POLYGONE);
  end;
  procedure QLnScrapEnglobant(const Indent: integer);
  var
    nv, j: Integer;
    VX: TPoint2Df;
    WU: String;
  begin
    nv := 1 + High(FScrapEnglobant.Vertexes);
    if (nv = 0) then Exit;
    for j := 0 to nv - 1 do
    begin
      VX := FScrapEnglobant.Vertexes[j];
      WU := StringOfChar(' ', Indent + 2) + CROSS_SECTION_VERTEX_SCRAP_ENGLOBANT + #9;
      WU := WU + Format('%.3f' + #9 + '%.3f', [VX.X, VX.Y]);
      LS.Add(WU);
    end;
  end;
  procedure QLnTexte(const Indent: integer; const T: TCrossSectionTexte);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', Indent) + CROSS_SECTION_SIMPLETEXT + #9;
    WU := WU + Format('%d' + #9 + '%d' + #9 + '%.3f' + #9 + '%.3f' + #9 +'%s',
                     [T.IDStyleTexte, T.Alignment, T.PosX, T.PosY, T.Text]);
    LS.Add(WU);

  end;
  procedure QLnSimpleLigne(const Indent: integer; const L: TCrossSectionSimpleLigne);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', Indent) + CROSS_SECTION_SIMPLELINE + #9;
    WU := WU + Format('%d' + #9 + '%.3f' + #9 + '%.3f' + #9 + '%.3f' + #9 + '%.3f',
                     [L.IDStyleLigne, L.Extr1.X, L.Extr1.Y, L.Extr2.X, L.Extr2.Y]);
    LS.Add(WU);
  end;
begin
  LS.Clear;
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  QAT := FBaseStation.IDStation;
  {$ELSE}
  QAT := GetToporobotIDStationAsString(FBaseStation, True);
  {$ENDIF TIDBASEPOINT_AS_TEXT}
  QCommentaire(2, Format('Cross-section for station: %s', [QAT]));
  QCommentaire(2, 'Parameters: IDCrossSection, IDGroupe, IDBasePoint, OffsetX, OffsetY');
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  QAT := FBaseStation.IDStation;
  {$ELSE}
  QAT := Inttostr(FBaseStation.IDStation);
  {$ENDIF TIDBASEPOINT_AS_TEXT}
  QBeginSection(2, CROSS_SECTION, [IntToStr(1), IntToStr(FIDGroupe), QAT, NbRealToStr(FDecalageXY.X), NbRealToStr(FDecalageXY.Y)]);
    // scrap englobant
    n := Length(FScrapEnglobant.Vertexes);
    if (n > 0) then
    begin
      QBeginSection(4, CROSS_SECTION_SCRAP_ENGLOBANT, ['']);
        QLnScrapEnglobant(4);
      QEndSection(4, END_CROSS_SECTION_SCRAP_ENGLOBANT);
    end;

    n := getNbCourbes();
    if (n > 0) then
    begin
      QBeginSection(4, CROSS_SECTION_COURBES, [Format('%d', [n])]);
      for i := 0 to n - 1 do QLnCourbe(6, getCourbe(i));
      QEndSection(4, END_CROSS_SECTION_COURBES);
    end;
    n := getNbPolygones();
    if (n > 0) then
    begin
      QBeginSection(4, CROSS_SECTION_POLYGONES, [Format('%d', [n])]);
      for i := 0 to n - 1 do QLnPolygone(6, getPolygone(i));
      QEndSection(4, END_CROSS_SECTION_POLYGONES);
    end;
    n := getNbSimplesLignes();
    if (n > 0) then
    begin
      QBeginSection(4, CROSS_SECTION_LINES, [Format('%d', [n])]);
      for i := 0 to n - 1 do QLnSimpleLigne(6, getSimpleLigne(i));
      QEndSection(4, END_CROSS_SECTION_LINES);
    end;
    n := getNbTextes();
    if (n > 0) then
    begin
      QBeginSection(4, CROSS_SECTION_TEXTES, [Format('%d', [n])]);
      for i := 0 to n - 1 do QLnTexte(6, getTexte(i));
      QEndSection(4, END_CROSS_SECTION_TEXTES);
    end;
  QEndSection(2, END_CROSS_SECTION);
end;
//******************************************************************************
procedure TCrossSection.AddStylePolygone(const LS: TStylePolygone);
begin
  FListePolygonStyles.AddElement(LS);
end;
function TCrossSection.GetNbStylesPolygones(): integer;
begin
  Result := FListePolygonStyles.GetNbElements();
end;

function TCrossSection.GetStylePolygone(const Idx: integer): TStylePolygone; inline;
begin
  Result := FListePolygonStyles.GetElement(Idx);
end;
procedure TCrossSection.PutStylePolygone(const Idx: integer; const LS: TStylePolygone);
begin
  FListePolygonStyles.PutElement(Idx, LS);
end;

procedure TCrossSection.AddStyleCourbe(const LS: TStyleCourbe);
begin
  FListeCurveStyles.AddElement(LS);
end;
function TCrossSection.GetStyleCourbe(const Idx: integer): TStyleCourbe;
begin
  Result := FListeCurveStyles.GetElement(Idx);
end;

procedure TCrossSection.PutStyleCourbe(const Idx: integer; const LS: TStyleCourbe);
begin
  FListeCurveStyles.PutElement(Idx, LS);
end;
function TCrossSection.GetNbStylesCourbe(): integer;
begin
  Result := FListeCurveStyles.GetNbElements();
end;


procedure TCrossSection.AddStyleTexte(const LS: TStyleTexte);
begin
  FListeTextStyles.AddElement(LS);
end;
function TCrossSection.GetStyleTexte(const Idx: integer): TStyleTexte;
begin
  Result := FListeTextStyles.GetElement(Idx);
end;

procedure TCrossSection.PutStyleTexte(const Idx: integer; const LS: TStyleTexte);
begin
  FListeTextStyles.PutElement(Idx, LS);
end;
function TCrossSection.GetNbStylesTexte: integer;
begin
  Result := FListeTextStyles.GetNbElements();
end;


procedure TCrossSection.AddStyleLigne(const LS: TStyleLigne);
begin
  FListeLinesStyles.AddElement(LS);
end;
function TCrossSection.GetStyleLigne(const Idx: integer): TStyleLigne;
begin
  Result := FListeLinesStyles.GetElement(Idx);
end;

procedure TCrossSection.PutStyleLigne(const Idx: integer; const LS: TStyleLigne);
begin
  FListeLinesStyles.PutElement(Idx, LS);
end;

function TCrossSection.MakeScrapEnglobant(): boolean;
const
   NB_SUBDIVS_COURBES = 10;
var
  NbC, c, a, p: Integer;
  MyPolyProvisoire: TListeSimple<TPoint2Df>;
  MyCourbe: TCrossSectionCourbe;
  MyArc: TCrossSectionArcCourbe;
  P1, P4, P2, P3, QV: TPoint2Df;
  PtsCourbes: TArrayPoints2Df;
begin
  Result := false;
  NbC := self.getNbCourbes();
  AfficherMessage(Format('%s.MakeScrapEnglobant(): %d courbes', [ClassName, NbC]));
  if (0 = NbC) then Exit; // Aucune courbe ? --> [ ];
  MyPolyProvisoire := TListeSimple<TPoint2Df>.Create;
  try
    MyPolyProvisoire.ClearListe();
    for c := 0 to NbC - 1 do
    begin
      MyCourbe := getCourbe(c);
      for a := 0 to High(MyCourbe.Arcs) do
      begin
        MyArc := MyCourbe.Arcs[a];
        P1 := MyArc.CoordsP1;
        P4 := MyArc.CoordsP2;
        P2.setFrom(P1.X + MyArc.TangP1.X, P1.Y + MyArc.TangP1.Y);
        P3.setFrom(P4.X + MyArc.TangP2.X, P4.Y + MyArc.TangP2.Y);
        PtsCourbes := CalcBezierCurve(P1, P2, P3, P4, NB_SUBDIVS_COURBES);
        for p := 0 to High(PtsCourbes) do MyPolyProvisoire.AddElement(PtsCourbes[p]);
      end;
    end;
    //MyPolyProvisoire.LoadCrossSectionCourbe();
    NbC := MyPolyProvisoire.GetNbElements();
    SetLength(FScrapEnglobant.Vertexes, NbC);
    for p := 0 to NbC - 1 do
    begin
      QV := MyPolyProvisoire.GetElement(p);
      FScrapEnglobant.Vertexes[p] := QV;
    end;
    AfficherMessage(Format('Scrap englobant: %d sommets', [Length(FScrapEnglobant.Vertexes)]));
    Result := (NbC > 0);
  finally
    FreeAndNil(MyPolyProvisoire);
  end;
end;



function TCrossSection.GetNbStylesLigne: integer;
begin
  Result := FListeLinesStyles.GetNbElements();
end;


//******************************************************************************
//******************************************************************************
// GESTION DES STYLES
//******************************************************************************
procedure TCrossSection.PurgerListeStyles();
begin
  FListeTextStyles.ClearListe();
  FListeCurveStyles.ClearListe();
  FListeLinesStyles.ClearListe();
  FListePolygonStyles.ClearListe();
end;
// définition des styles par défaut
// TODO: Procédure d'ajout d'un nouveau type d'objet: Incrémenter de 1 la constante NB_CUR_STY
procedure TCrossSection.SetDefaultStyles();
const
  LONG_BARB  = 0.2;
  // seuil de visibilite
  SEUIL_TOUJOURS_VISIBLE = 1E-9;
  SEUIL_PPAUX_DETAILS    = 1 / 5000;
  SEUIL_TOUS_DETAILS     = 1 / 1000;
var
  Q, i: integer;
  procedure QSetStyleLigne(const id: integer;
                           const svgs : string;
                           const descs: string;
                           const LC: TColor;
                           const LW: integer;
                           const LPW: double;
                           const LS: TPenStyle;
                           const SeuilDeVisibilite: double);
  var
    WU: TStyleLigne;
  begin
    WU.IDStyle        := id;
    WU.NameSVGStyle   := svgs;
    WU.DescStyle      := descs;
    WU.LineColor      := LC;
    WU.LineWidth      := LW;
    WU.PrintLineWidth := LPW;
    WU.LineStyle      := LS;
    WU.SeuilVisibilite:= SeuilDeVisibilite;
    AddStyleLigne(WU);
  end;
  //*)
  procedure QSetStyleCourbe(const id: integer;
                           const svgs : string;
                           const descs: string;
                           const LC: TColor;
                           const LW: integer;
                           const LPW: double;
                           const LS: TPenStyle;
                           const SM: boolean;
                           const BB: TBarbule;
                           const LB: double;
                           const SeuilDeVisibilite: double);
  var
    WU: TStyleCourbe;
  begin
    WU.IDStyle        := id;
    WU.NameSVGStyle   := svgs;
    WU.DescStyle      := descs;
    WU.LineColor      := LC;
    WU.LineWidth      := LW;
    WU.PrintLineWidth := LPW;
    WU.LineStyle      := LS;
    WU.Smooth         := SM;
    WU.Barbules       := BB;
    WU.LongBarbules   := LB;
    WU.SeuilVisibilite:= SeuilDeVisibilite;
    AddStyleCourbe(WU);
  end;
  procedure QSetStylePolygone(const id: integer;
                         const svgs : string;
                         const descs: string;
                         const LW: integer;
                         const LC: TColor;
                         const LS: TPenStyle;
                         const FC: TColor;
                         const PS: byte;
                         const SeuilDeVisibilite: double);
  var
    WU: TStylePolygone;
  begin

    WU.IDStyle     := id;
    WU.NameSVGStyle:= svgs;
    WU.DescStyle   := descs;
    WU.LineWidth   := LW;
    WU.LineColor   := LC;
    WU.LineStyle   := LS;
    WU.FillColor   := FC;
    WU.Style       := PS;
    WU.SeuilVisibilite := SeuilDeVisibilite;
    AddStylePolygone(WU);
  end;
  procedure QSetStyleTexte(const id: integer;
                           const svgs : string;
                           const descs: string;
                           const FN: string;
                           const FPH: double;
                           const FC: TColor;
                           const FS: TFontStyles;
                           const SeuilDeVisibilite: double);
  var
    WU : TStyleTexte;
  begin
    with WU do
    begin
      IDStyle       := ID;
      NameSVGStyle  := svgs;
      DescStyle     := descs;
      FontName      := FN;
      FontPrnHeight := FPH;
      FontColor     := FC;
      FontStyle     := FS;
      SeuilVisibilite := SeuilDeVisibilite;
    end;
    AddStyleTexte(WU);
  end;
  procedure QSetStyleSymbole(const id: integer;
                            const svgs : string;
                            const descs: string;
                            const OC: TColor;
                            const SeuilDeVisibilite: double);
  var
    WU: TStyleSymboles;
  begin
    with WU do
    begin
      IDStyle       := id;
      NameSVGStyle  := svgs;
      DescStyle     := descs;
      Color         := OC;
      SeuilVisibilite := SeuilDeVisibilite;
    end;
    //AddStyleSymbole(WU);
  end;
begin
  AfficherMessage(Format('%s.SetDefaultStyles()', [ClassName]));
  PurgerListeStyles();
  // lignes
  QSetStyleLigne(0, LowerCase('LINE-DEFAULT')          , ListeStylesLignes[0], clBlack    , 0, 0.03, psSolid, SEUIL_TOUS_DETAILS);
  QSetStyleLigne(1, LowerCase('LINE-FLECHES')          , ListeStylesLignes[1], clBlack    , 0, 0.03, psSolid, SEUIL_TOUS_DETAILS);
  QSetStyleLigne(2, LowerCase('LINE-FRACTURES')        , ListeStylesLignes[2], clBlue     , 0, 0.10, psSolid, SEUIL_PPAUX_DETAILS);
  QSetStyleLigne(3, LowerCase('LINE-SUITE-RESEAUX')    , ListeStylesLignes[3], clRed      , 0, 0.05, psSolid, SEUIL_PPAUX_DETAILS);
  QSetStyleLigne(4, LowerCase('LINE-PENTES')           , ListeStylesLignes[4], clGray     , 0, 0.03, psSolid, SEUIL_TOUS_DETAILS);
  // courbes
  QSetStyleCourbe(0, LowerCase('CURVE-DEFAULT')        , ListeStylesCourbes[0], clBlack    , 0, 0.10, psSolid, True, tbNONE, LONG_BARB, SEUIL_TOUJOURS_VISIBLE);
  QSetStyleCourbe(1, LowerCase('CURVE-WALLS')          , ListeStylesCourbes[1], clMaroon   , 2, 0.25, psSolid, True, tbNONE, LONG_BARB, SEUIL_TOUJOURS_VISIBLE);
  QSetStyleCourbe(2, LowerCase('CURVE-HIDDEN_WALLS')   , ListeStylesCourbes[2], clGray     , 2, 0.15, psSolid, True, tbNONE, LONG_BARB, SEUIL_TOUJOURS_VISIBLE);
  QSetStyleCourbe(3, LowerCase('CURVE-FLOODINGS')      , ListeStylesCourbes[3], clBlue     , 3, 0.35, psSolid, True, tbNONE, LONG_BARB, SEUIL_TOUJOURS_VISIBLE);
  QSetStyleCourbe(4, LowerCase('CURVE-PENTES')         , ListeStylesCourbes[4], clGray     , 0, 0.03 , psSolid, True, tbNONE, LONG_BARB, SEUIL_TOUS_DETAILS);
  QSetStyleCourbe(5, LowerCase('CURVE-RESSAUTS')       , ListeStylesCourbes[5], clMaroon   , 0, 0.03 , psSolid, True, tbRESSAUT, LONG_BARB, SEUIL_PPAUX_DETAILS);
  QSetStyleCourbe(6, LowerCase('CURVE-SURPLOMBS')      , ListeStylesCourbes[6], clGreen    , 0, 0.03 , psSolid, True, tbSURPLOMB, LONG_BARB, SEUIL_PPAUX_DETAILS);
  QSetStyleCourbe(7, LowerCase('CURVE-CHENAL_VOUTE')   , ListeStylesCourbes[7], clNavy     , 0, 0.03 , psSolid, True, tbCHENAL_VOUTE, LONG_BARB, SEUIL_TOUS_DETAILS);
  QSetStyleCourbe(8, LowerCase('CURVE-MINI-RESSAUTS')  , ListeStylesCourbes[8], clBlue     , 0, 0.03 , psSolid, True, tbMINI_RESSAUT, LONG_BARB / 2, SEUIL_TOUS_DETAILS);
  QSetStyleCourbe(9, LowerCase('CURVE-PAROI-INCERTAINE'), ListeStylesCourbes[9], clMaroon  , 0, 0.25 , psDash, True, tbNONE, LONG_BARB, SEUIL_TOUJOURS_VISIBLE);
  // polygones
  QSetStylePolygone( 0, LowerCase('POLYGON-DEFAULT')       , ListeStylesPolygones[0] , 0, clBlack  , psSolid, clSilver, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 1, LowerCase('POLYGON-LAKE')          , ListeStylesPolygones[1] , 0, clNavy   , psSolid, clBlue  , 0, SEUIL_PPAUX_DETAILS);
  QSetStylePolygone( 2, LowerCase('POLYGON-ARGILE')        , ListeStylesPolygones[2] , 0, clMaroon , psSolid, clMaroon, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 3, LowerCase('POLYGON-SABLE')         , ListeStylesPolygones[3] , 0, clMaroon , psSolid, clMaroon, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 4, LowerCase('POLYGON-BLOCKS')        , ListeStylesPolygones[4] , 0, clMaroon , psSolid, clMaroon, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 5, LowerCase('POLYGON-GALETS')        , ListeStylesPolygones[5] , 0, clMaroon , psSolid, clMaroon, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 6, LowerCase('POLYGON-NEIGE')         , ListeStylesPolygones[6] , 0, clMaroon , psSolid, clMaroon, 0, SEUIL_PPAUX_DETAILS);
  QSetStylePolygone( 7, LowerCase('POLYGON-SILHOUETTES')   , ListeStylesPolygones[7] , 0, clSilver , psSolid, clSilver, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 8, LowerCase('POLYGON-GROS-BLOC')     , ListeStylesPolygones[8] , 0, clBlack  , psSolid, clSilver, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 9, LowerCase('POLYGON-GOUR')          , ListeStylesPolygones[9] , 0, clBlue   , psSolid, clAqua  , 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone(10, LowerCase('POLYGON-SIPHON')        , ListeStylesPolygones[10], 0, clNavy   , psSolid, clNavy  , 0, SEUIL_PPAUX_DETAILS);
  QSetStylePolygone(11, LowerCase('POLYGON-ARGILE-GALETS') , ListeStylesPolygones[11], 0, clMaroon , psSolid, clMaroon, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone(12, LowerCase('POLYGON-PARCOURS')      , ListeStylesPolygones[12], 0, clMaroon , psSolid, clYellow, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone(13, LowerCase('POLYGON-MASQUE')        , ListeStylesPolygones[13], 0, clAqua   , psSolid, clAqua  , 0, SEUIL_TOUS_DETAILS);
  // textes
  QSetStyleTexte( 0, LowerCase('TEXT-DEBUG')               , ListeStylesTextes[0], 'Arial',  1.5, clRed, [], SEUIL_TOUS_DETAILS);
  QSetStyleTexte( 1, LowerCase('TEXT-TITRES')              , ListeStylesTextes[1], 'Arial',  8.0, clBlack, [fsBold, fsUnderline], SEUIL_TOUJOURS_VISIBLE);
  QSetStyleTexte( 2, LowerCase('TEXT-SOUS-TITRES')         , ListeStylesTextes[2], 'Arial',  6.0, clBlack, [fsBold, fsItalic], SEUIL_PPAUX_DETAILS);
  QSetStyleTexte( 3, LowerCase('TEXT-COTATION')            , ListeStylesTextes[3], 'Arial',  2.5, clBlue, [], SEUIL_TOUS_DETAILS);
  QSetStyleTexte( 4, LowerCase('TEXT-ORDINAIRE1')          , ListeStylesTextes[4], 'Arial',  1.5, clBlack, [], SEUIL_TOUS_DETAILS);
  QSetStyleTexte( 5, LowerCase('TEXT-ORDINAIRE2')          , ListeStylesTextes[5], 'Arial',  1.2, clBlack, [fsItalic], SEUIL_TOUS_DETAILS);
  QSetStyleTexte( 6, LowerCase('TEXT-LIEUDIT')             , ListeStylesTextes[6], 'Arial',  1.0, clBlack, [fsItalic, fsUnderline], SEUIL_TOUS_DETAILS);

  // objets ponctuels
  QSetStyleSymbole( 0, LowerCase('OBJECT-PHOTO')       , ListeNatureSymboles[0] , clBlack, SEUIL_PPAUX_DETAILS);              // Photo
  QSetStyleSymbole( 1, LowerCase('OBJECT-ENTRANCE')    , ListeNatureSymboles[1] , clBlue, SEUIL_TOUJOURS_VISIBLE);               // Entrée
  QSetStyleSymbole( 2, LowerCase('OBJECT-STATION')     , ListeNatureSymboles[2] , clRed, SEUIL_TOUS_DETAILS);                // Point topo
  QSetStyleSymbole( 3, LowerCase('OBJECT-FIXPOINT')    , ListeNatureSymboles[3] , clGreen, SEUIL_TOUS_DETAILS);              // Point fixe''
  QSetStyleSymbole( 4, LowerCase('OBJECT-GRP-RELATION'), ListeNatureSymboles[4] , clGray, SEUIL_TOUS_DETAILS);               // Correspondance entre groupes
  QSetStyleSymbole( 5, LowerCase('OBJECT-FISTULEUSES') , ListeNatureSymboles[5] , clBlack, SEUIL_TOUS_DETAILS);              // Fistuleuses'
  QSetStyleSymbole( 6, LowerCase('OBJECT-CONCRETIONS') , ListeNatureSymboles[6] , clBlack, SEUIL_TOUS_DETAILS);              // Concrétions de paroi'
  QSetStyleSymbole( 7, LowerCase('OBJECT-HELICTITES')  , ListeNatureSymboles[7] , clBlack, SEUIL_TOUS_DETAILS);              // Excentriques
  QSetStyleSymbole( 8, LowerCase('OBJECT-STALACTITES') , ListeNatureSymboles[8] , clBlack, SEUIL_TOUS_DETAILS);              // Stalactites
  QSetStyleSymbole( 9, LowerCase('OBJECT-COLUMNS')     , ListeNatureSymboles[9] , clBlack, SEUIL_TOUS_DETAILS);              // Colonnes
  QSetStyleSymbole(10, LowerCase('OBJECT-STALAGMITES') , ListeNatureSymboles[10], clBlack, SEUIL_TOUS_DETAILS);              // Stalagmites
  QSetStyleSymbole(11, LowerCase('OBJECT-ARAGONITES')  , ListeNatureSymboles[11], clBlack, SEUIL_TOUS_DETAILS);              // Cristaux d'aragonite
  QSetStyleSymbole(12, LowerCase('OBJECT-FAILLES')     , ListeNatureSymboles[12], clBlack, SEUIL_TOUS_DETAILS);
  QSetStyleSymbole(13, LowerCase('OBJECT-CUPULES')     , ListeNatureSymboles[13], clBlack, SEUIL_TOUS_DETAILS);
  QSetStyleSymbole(14, LowerCase('OBJECT-ZEFF')        , ListeNatureSymboles[14], clBlack, SEUIL_TOUS_DETAILS);
  QSetStyleSymbole(15, LowerCase('OBJECT-ARRIVEE-EAU') , ListeNatureSymboles[15], clBlack, SEUIL_TOUS_DETAILS);
  QSetStyleSymbole(16, LowerCase('OBJECT-PERTE')       , ListeNatureSymboles[16], clBlack, SEUIL_TOUS_DETAILS);
  QSetStyleSymbole(17, LowerCase('OBJECT-DESOB')       , ListeNatureSymboles[17], clBlack, SEUIL_TOUS_DETAILS);
  AfficherMessage('SetDefaultStyles OK');
end;

function TCrossSection.ClippingByScrapEnglobant(): boolean;
var
  OperationsBooleennes: TOperationsCSG;
  NbPoly, i: Integer;
  MyPolygone: TCrossSectionPolygone;
  QBackupStyleScrap: TNatureObjetPolygone;
  QIDStylePolygone : TNatureObjetPolygone;

begin
  result := false;


  NbPoly := getNbPolygones();
  AfficherMessage(Format('%s.ClippingByScrapEnglobant: %d polygones', [classname, NbPoly]));
  AfficherMessageErreur(Format('%s.ClippingByScrapEnglobant: %d polygones', [classname, NbPoly]));
  if (0 = NbPoly) then Exit(false);
  OperationsBooleennes := TOperationsCSG.Create;
  try
    if (OperationsBooleennes.Initialise()) then
    begin
       QBackupStyleScrap := FScrapEnglobant.IDStylePolygone;
       OperationsBooleennes.SetPoly0(FScrapEnglobant);
       for i := NbPoly - 1 downto 0 do
       begin
         MyPolygone := getPolygone(i);
         if (IntersectTVertexPolygonArray(FScrapEnglobant.Vertexes, MyPolygone.Vertexes)) then
         begin
           QIDStylePolygone := MyPolygone.IDStylePolygone;
           OperationsBooleennes.SetPoly1(MyPolygone);
           if (OperationsBooleennes.ExecuteCSG(ctIntersection)) then
           begin
             MyPolygone := OperationsBooleennes.GetPolygoneResult();
             putPolygone(i, MyPolygone);
           end;
         end
         else
         begin
           AfficherMessageErreur(Format('Suppression du polygone hors limites %d', [i]));
           RemovePolygone(i);
         end;

       end;
       FScrapEnglobant.IDStylePolygone := QBackupStyleScrap;
       Result := True;

    end;
    OperationsBooleennes.Finalise();
  finally
    FreeAndNil(OperationsBooleennes);
  end;

end;


{Class: TOperationsCSG }

function TOperationsCSG.Initialise(): boolean;
begin
  result := false;
  try
    AfficherMessage(Format('%s.Initialise', [ClassName]));
    result := True;
  except
  end;

end;

procedure TOperationsCSG.Finalise();
begin
  pass;
end;

procedure TOperationsCSG.SetPoly0(const P: TCrossSectionPolygone);
begin
  FNatureObjetPolygone0 := P.IDStylePolygone;
  FPath0                := P.Vertexes;
end;

procedure TOperationsCSG.SetPoly1(const P: TCrossSectionPolygone);
begin
  FNatureObjetPolygone1 := P.IDStylePolygone;
  FPath1                := P.Vertexes;
end;




function TOperationsCSG.MakeTGeoPolygon2D(const QPath: TArrayPoints2Df): TGeoPolygon2D;
begin

end;

function TOperationsCSG.MakeTPathFromPoly(const P: TArrayPoints2Df; out QPath: TPath): boolean;
var
  i, n: Integer;
begin
  result := false;
  try
    n := Length(P);
    if (n = 0) then Exit(false);

    Setlength(QPath, n);
    for i := 0 to n - 1 do
    begin
      // /!\ Clipper travaille sur des entiers: on passe tout en centimètres
      QPath[i].X := Round(QMULTIPLICATEUR * P[i].X);
      QPath[i].Y := Round(QMULTIPLICATEUR * P[i].Y);
    end;
    Result := true;
  except
  end;
end;

function TOperationsCSG.GetPolygoneResult(): TCrossSectionPolygone;
begin
  Result := FPolygoneResult;
end;


function TOperationsCSG.ExecuteCSG(const ClipType: TClipType): boolean;
var
  CP: TClipper;
  PP0, PP1: TArrayPoints2Df;
  QPath0, QPath1, MyPathResult: TPath;
  ResultatUnion: TPaths;
  NbVtxResult, i: Integer;
  Xo, Yo: Extended;
begin
  Result := false;
  if (IntersectTVertexPolygonArray(FPath0, FPath1))   then
  begin
    AfficherMessage('-- Chemins 1 et 2 se coupent');
    // ici, on va appeler les fonctions Clipper
    CP := TClipper.Create();
    try
      if (MakeTPathFromPoly(FPath0, QPath0) AND MakeTPathFromPoly(FPath1, QPath1)) then
      begin
        CP.AddPath(QPath0, ptSubject, True);
        CP.AddPath(QPath1, ptClip, True);
        if (CP.Execute(ClipType, ResultatUnion)) then
        begin
          MyPathResult := ResultatUnion[0];
          NbVtxResult  := 1 + high(MyPathResult);
          SetLength(FPolygoneResult.Vertexes, NbVtxResult);
          FPolygoneResult.IDStylePolygone := FNatureObjetPolygone1;
          // et on repasse tout en mètres
          for i := 0 to NbVtxResult - 1 do
          begin
            FPolygoneResult.Vertexes[i].X := MyPathResult[i].X / QMULTIPLICATEUR;
            FPolygoneResult.Vertexes[i].Y := MyPathResult[i].Y / QMULTIPLICATEUR;
          end;  // for i := 0 to NbVtxResult - 1 do
          Result := True;
        end;    // if (CP.Execute(ctIntersection, ResultatUnion)) then
      end; // if MakeTPathFromPoly(PP0, QPath0) AND MakeTPathFromPoly(PP1, QPath1)
    finally
      FreeAndNil(CP);//CP.Free;
    end;
  end

end;

end.
