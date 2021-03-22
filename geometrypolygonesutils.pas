unit GeometryPolygonesUtils;
{$INCLUDE CompilationParameters.inc}

// Union de scraps
// Utilise Clipper (http://www.angusj.com/delphi/clipper.php#code)
// 29/07/2015: Support des scraps, polygones; opérations booléennes
// 30/07/2015: Fusion de polylignes et de courbes (dans UnitGraphismesProvisoires)
//             Génération de scraps depuis une liste de courbes orientées
// 25/08/2015: Plusieurs polygones peuvent être traités, au lieu de deux
// 14/04/2016: Procédure de fusion de courbes complètement réécrite
// 29/05/2019: Suite à 3 ans de RETEX, où seuls deux objets étaient traités:
//             TOperationsCSGClipping n'est plus une classe TList
//             les deux polygones à traiter vont dans deux membres privés,
//             qui ne sont affectés qu'une seule fois, dans Initialiser().
// 31/08/2020: Pointage temporel
interface

uses
  GHCD_Types,
  Graphics,
  GeneralFunctions,
  UnitListesSimplesWithGeneriques,
  UnitDocDessin,
  UnitGraphismesProvisoires,
  FastGEO,
  clipper,
  Classes, SysUtils;
type TOperationsCSGClipping = class
  private
    FPath0: TVertexPolygonArray;
    FPath1: TVertexPolygonArray;

    FMyDocDessin: TDocumentDessin;
    FVertexPolygonArrayResult : TVertexPolygonArray;
    function  IntersectTVertexPolygonArray(const Poly1, Poly2: TVertexPolygonArray): boolean;
    function  MakeTGeoPolygon2D(const PolyVertex: TVertexPolygonArray): TGeoPolygon2D;
    function  MakeTPathFromPoly(const P: TVertexPolygonArray): TPath;
  public
    procedure Initialise(const FD: TDocumentDessin; const P0, P1: TVertexPolygonArray);
    procedure Finalise();
    function  GetVertexPolygonArrayResult: TVertexPolygonArray;
    function  IntersectionVertexPolygonArray(): boolean; inline;
    function  GetNearBasePointIdx(const qX, qY: double): TIDBaseStation;    // recherche le point le plus proche de (X, Y)
    function  ExecuteCSG(const ClipType: TClipType): boolean;               // opérations booléennes
end;

// fonctions utilitaires
// fusion d'objets avec opérations booléennes (vue en plan)
function MergeScraps(const FD: TDocumentDessin; const Obj1, Obj2: TScrap; const OpBool: byte; out MergedObj: TScrap): integer;
function MergePolygones(const FD: TDocumentDessin; const Obj1, Obj2: TPolygone; const OpBool: byte; out MergedObj: TPolygone): integer;
function MergePolylignes(const FD: TDocumentDessin; const Obj1, Obj2: TPolyLigne; const OpBool: byte; out MergedObj: TPolyLigne): integer;

// concaténation de courbes et lignes
function ConcatenerCourbes(const FD: TDocumentDessin;
                           const ArrIdxCourbes: TArrayIdxObjets;
                           out MergedObj: TCourbe): boolean;
function ConcatenerPolylignes(const FD: TDocumentDessin;
                              const ArrIdxPoly: TArrayIdxObjets;
                              out MergedObj: TPolyligne): boolean;
// génération de scrap depuis deux objets
function GenererUnScrapDepuisCourbes(const FD: TDocumentDessin;
                                     const ArrCourbes: TArrayOfTCourbe;
                                     const IdxGrp: TIDGroupeEntites;
                                     out   MergedObj: TScrap): integer;
function GenererUnPolygoneDepuisCourbes(const FD: TDocumentDessin;
                                     const ArrCourbes: TArrayOfTCourbe;
                                     const IdxGrp: TIDGroupeEntites;
                                     out   MergedObj: TPolygone): integer;
function GenererUnScrapDepuisPolylignes(const FD: TDocumentDessin;
                                        const ArrPolylignes: TArrayOfTPolyligne;
                                        const IdxGrp: TIDGroupeEntites;
                                        out MergedObj: TScrap): integer;
// Fractionnement d'un objet ligne ou courbe en deux
function CasserUneCourbe(const C: TCourbe; const Idx: integer; out RC1, RC2: TCourbe): boolean;
function CasserUnePolyligne(const C: TPolyLigne; const Idx: integer; out RC1, RC2: TPolyLigne): boolean;

// Fonctions vides
function GenererUnPolygoneDepuisIntersectedCourbes(const FD: TDocumentDessin;
                                        const ArrCourbes: TArrayOfTCourbe;
                                        const IdxGrp: TIDGroupeEntites;
                                        out   MergedObj: TVertexPolygonArray): integer;




// (P0+Q0)*(1-t)^3+3*(P1+Q1)*t*(1-t)^2+3*(P2+Q2)*(1-t)*t^2+(P3+Q3)*t^3

implementation
const MULTIPLICATEUR_UNITES = 100.00;  // passage en centimètres

function IntersectionOfBezierArcs(const A0, A1: TBezierArc; out Pts: TArrayPoints2Df): boolean;
var
  PP0, PP1, PP2, PP3: TPoint2Df;
  PQ0, PQ1, PQ2, PQ3: TPoint2Df;
begin
  Result := false;
  // premier arc
  PP0 := A0.PT1;
  PP3 := A0.PT2;
  PP1 := MakeTPoint2Df(PP0.X + A0.Tgt1.X, PP0.Y + A0.Tgt1.Y);
  PP2 := MakeTPoint2Df(PP1.X + A0.Tgt2.X, PP1.Y + A0.Tgt2.Y);
  // deuxième arc
  PQ0 := A1.PT1;
  PQ3 := A1.PT2;
  PQ1 := MakeTPoint2Df(PQ0.X + A1.Tgt1.X, PQ0.Y + A1.Tgt1.Y);
  PQ2 := MakeTPoint2Df(PQ1.X + A1.Tgt2.X, PQ1.Y + A1.Tgt2.Y);



  //(P0+Q0)*(1-t)^3+3*(P1+Q1)*t*(1-t)^2+3*(P2+Q2)*(1-t)*t^2+(P3+Q3)*t^3
  // avec XCas:
  //expand((P0+Q0)*(1-t)^3+3*(P1+Q1)*t*(1-t)^2+3*(P2+Q2)*(1-t)*t^2+(P3+Q3)*t^3)
  // --> 3Q1(-t)2t+3P1(-t)2t+Q3t3-3Q2t3+3Q2t2-6Q1t2+3Q1t+3Q0(-t)2-3Q0t+P3t3-3P2t3+3P2t2-6P1t2+3P1t+3P0(-t)2-3P0t+ (Q0+P0 )(-t)3+Q0+P0

  // simplify((P0+Q0)*(1-t)^3+3*(P1+Q1)*t*(1-t)^2+3*(P2+Q2)*(1-t)*t^2+(P3+Q3)*t^3)
  // Termes en t3: (-1 P0) t^3  (+3 P1) t^3  (-3 P2) t^3  (+1 P3) t^3  (-1 Q0) t^3  (3 Q1) t^3  (-3 Q2) t^3   (+Q3) t^3
  // Termes en t2: (+3 P0) t^2  (-6 P1) t^2  (+3 P2) t^2  (+3 Q0) t^2  (-6 Q1) t^2  (3 Q2) t^2
  // Termes en t : (-3 P0) t    (+3 P1) t    (-3 Q0) t    (+3 Q1) t
  // Termes en t0: (+1 P0)      (+1 Q0)

end;





function MergePolyScrap(const FD: TDocumentDessin; const PP1, PP2: TVertexPolygonArray; const OpBool: byte; out Merged: TVertexPolygonArray): integer;
var
  OpsCSG: TOperationsCSGClipping;
  OP : TClipType;
  PPP: TPoint2Df;
  WU : TIDBaseStation;
begin
  OP := TClipType(OpBool);
  Result := errMERGE_SCRAPS_ANY_ERROR;
  OpsCSG := TOperationsCSGClipping.Create;
  try
    OpsCSG.Initialise(FD, PP1, PP2);
    PPP := MakeTPoint2Df(403480, 3091131);
    WU := OpsCSG.GetNearBasePointIdx(PPP.X, PPP.Y);

    if (OpsCSG.IntersectionVertexPolygonArray()) then
    begin
      //AfficherMessage('-- Scraps 1 et 2 se coupent');
      if (OpsCSG.ExecuteCSG(OP)) then
      begin
        Merged := OpsCSG.GetVertexPolygonArrayResult();
        Result := errMERGE_SCRAPS_OK;
      end;
    end
    else
    begin
      Result := errMERGE_SCRAPS_NO_INTERSECT;
      //AfficherMessage('-- Scraps 1 et 2 sont disjoints');
    end;
    OpsCSG.Finalise();
  finally
    FreeAndNil(OpsCSG);//OpsCSG.Free;
  end;

end;

function MergeScraps(const FD: TDocumentDessin; const Obj1, Obj2: TScrap; const OpBool: byte; out MergedObj: TScrap): integer;
var

  G1: TGroupeEntites;
  G2: TGroupeEntites;
  OP: TClipType;
begin
  //AfficherMessage(Format('Merge scraps 0: (%s) and 1: (%s)', [Obj1.Nom, Obj2.Nom]));
  // il est interdit de fusionner deux scraps appartenant à deux groupes différents
  G1 := FD.GetGroupeByIDGroupe(Obj1.IDGroupe);
  G2 := FD.GetGroupeByIDGroupe(Obj2.IDGroupe);
  if (G2.IDGroupeEntites <> G1.IDGroupeEntites) then exit(errMERGE_SCRAPS_GROUPES_MISMATCH);
  MergedObj := Obj1;
  MergedObj.Couleur := clBlue;
  MergedObj.Nom     := Obj1.Nom + '+' + Obj2.Nom;
  Result := MergePolyScrap(FD, Obj1.Sommets, Obj2.Sommets, OpBool, MergedObj.Sommets);
end;
function MergePolygones(const FD: TDocumentDessin; const Obj1, Obj2: TPolygone; const OpBool: byte; out MergedObj: TPolygone): integer;
var
  PP1, PP2: TVertexPolygonArray;
  OpsCSG: TOperationsCSGClipping;
  PPP: TPoint2Df;
  G1: TGroupeEntites;
  G2: TGroupeEntites;
  OP: TClipType;
begin
  Result := errMERGE_POLYGONES_ANY_ERROR;
  OP := TClipType(OpBool);
  // il est interdit de fusionner deux polygones appartenant à deux groupes différents
  G1 := FD.GetGroupeByIDGroupe(Obj1.IDGroupe);
  G2 := FD.GetGroupeByIDGroupe(Obj2.IDGroupe);
  if (G2.IDGroupeEntites <> G1.IDGroupeEntites) then exit(errMERGE_POLYGONES_GROUPES_MISMATCH);
  MergedObj := Obj1;
  PP1 := Obj1.Sommets;
  PP2 := Obj2.Sommets;
  Result := MergePolyScrap(FD, Obj1.Sommets, Obj2.Sommets, OpBool, MergedObj.Sommets);
end;

function MergePolylignes(const FD: TDocumentDessin; const Obj1, Obj2: TPolyLigne; const OpBool: byte; out MergedObj: TPolyLigne): integer;
var
  PP1, PP2: TVertexPolygonArray;
  OpsCSG: TOperationsCSGClipping;
  WU: TIDBaseStation;
  PPP: TPoint2Df;
   G1: TGroupeEntites;
  G2: TGroupeEntites;
  OP: TClipType;
begin
  Result := errMERGE_POLYLIGNES_ANY_ERROR;
  OP := TClipType(OpBool);
  // il est interdit de fusionner deux polygones appartenant à deux groupes différents
  G1 := FD.GetGroupeByIDGroupe(Obj1.IDGroupe);
  G2 := FD.GetGroupeByIDGroupe(Obj2.IDGroupe);
  if (G2.IDGroupeEntites <> G1.IDGroupeEntites) then exit(errMERGE_POLYGONES_GROUPES_MISMATCH);

  MergedObj := Obj1;

  PP1 := Obj1.Sommets;
  PP2 := Obj2.Sommets;

  OpsCSG := TOperationsCSGClipping.Create;
  try
    OpsCSG.Initialise(FD, PP1, PP2);
    //OpsCSG.ListerLesPaths();
    PPP := MakeTPoint2Df(403480, 3091131);
    WU := OpsCSG.GetNearBasePointIdx(PPP.X, PPP.Y);
    if (OpsCSG.IntersectionVertexPolygonArray()) then
    begin
      AfficherMessage('-- polygones 1 et 2 se coupent');
      if (OpsCSG.ExecuteCSG(OP)) then
      begin
        MergedObj.Sommets := OpsCSG.GetVertexPolygonArrayResult();
        Result := errMERGE_SCRAPS_OK;
      end;
    end
    else
    begin
      Result := errMERGE_SCRAPS_NO_INTERSECT;
      AfficherMessage('-- polygones 1 et 2 sont disjoints');
    end;
    OpsCSG.Finalise();
  finally
    FreeAndNil(OpsCSG);//OpsCSG.Free;
  end;

end;

//*****************************************************************************************
function ConcatenerCourbes(const FD: TDocumentDessin;
                           const ArrIdxCourbes: TArrayIdxObjets;
                           out   MergedObj: TCourbe): boolean;
var
  Nb, i, QNbArcs, a, n: Integer;
  QListeArcs: TListeSimple<TArcCourbe>;
  CC, CC0: TCourbe;
  MyArc: TArcCourbe;
  EWE: Int64;
begin
  Result := false;
  Nb := 1 + high(ArrIdxCourbes);
  AfficherMessage(Format('ConcatenerCourbes: %d', [Nb]));
  if (Nb < 2) then Exit;
  // attraper les attributs de la 1ère courbe
  MergedObj := FD.GetCourbe(ArrIdxCourbes[0]);
  // liste provisoire d'arcs
  QListeArcs := TListeSimple<TArcCourbe>.Create;
  try
    QListeArcs.ClearListe();
    for i := 0 to Nb -1 do
    begin
      EWE := ArrIdxCourbes[i];
      CC := FD.GetCourbe(EWE);
      // création des arcs de liaison entre courbes n et n-1 (sauf la première)
      // avec pour paramètres:
      // PT 1 = dernier point de la courbe précédente (ID station + offset)
      // PT 2 = premier point de la courvbe courante  (ID station + offset)
      // PC1  = petit prolongement de la tangente à PT1
      // PC2  = petit prolongement de la tangente à PT2

      if (i > 0) then
      begin
        CC0 := FD.GetCourbe(ArrIdxCourbes[i - 1]);
        n := High(CC0.Arcs);
        MyArc.IDStationP1 :=  CC0.Arcs[n].IDStationP2;
        MyArc.OffsetP1    :=  CC0.Arcs[n].OffsetP2;
        MyArc.TangP1.X    := -0.05 * CC0.Arcs[n].TangP2.X;
        MyArc.TangP1.Y    := -0.05 * CC0.Arcs[n].TangP2.Y;
        MyArc.TangP1.Z    := -0.05 * CC0.Arcs[n].TangP2.Z;
        MyArc.IDStationP2 :=  CC.Arcs[0].IDStationP1;
        MyArc.OffsetP2    :=  CC.Arcs[0].OffsetP1;
        MyArc.TangP2.X    := -0.05 * CC.Arcs[0].TangP1.X;
        MyArc.TangP2.Y    := -0.05 * CC.Arcs[0].TangP1.Y;
        MyArc.TangP2.Z    := -0.05 * CC.Arcs[0].TangP1.Z;
        QListeArcs.AddElement(MyArc);
      end;

      QNbArcs := 1 + high(CC.Arcs);
      if (QNbArcs = 0) then Continue;
      for a := 0 to QNbArcs - 1 do QListeArcs.AddElement(CC.Arcs[a]);

    end;
    Nb := QListeArcs.GetNbElements();
    if (Nb = 0) then Exit;
    SetLength(MergedObj.Arcs, Nb);
    for i := 0 to Nb - 1 do
    begin
      MyArc := QListeArcs.GetElement(i);
      MergedObj.Arcs[i] := MyArc;
    end;
    // controle
    for i := 0 to Nb - 1 do
    begin
      MyArc := MergedObj.Arcs[i];
      AfficherMessage(Format('%d: %d to %d', [i, MyArc.IDStationP1, MyArc.IDStationP2]));
    end;
    // retourner le résultat: OK si Nb > 0
    Result := (Nb > 0);
    QListeArcs.ClearListe();
  finally
    FreeAndNil(QListeArcs);
  end;
end;
// TODO: ConcatenerPolylignes est Non testé
function ConcatenerPolylignes(const FD: TDocumentDessin; const ArrIdxPoly: TArrayIdxObjets; out MergedObj: TPolyligne): boolean;
var
  QListeVertex : TListeSimple<TVertexPolygon>;
  Nb, i, QNbV, a: Integer;
  EWE: Int64;
  PP: TPolyLigne;
  MyVertex: TVertexPolygon;
begin
  Result := false;
  Nb := 1 + high(ArrIdxPoly);
  AfficherMessage(Format('ConcatenerPolylignes: %d', [Nb]));
  if (Nb < 2) then Exit;
  // attraper les attributs de la 1ère courbe
  MergedObj := FD.GetPolyligne(ArrIdxPoly[0]);
  // liste provisoire d'arcs
  QListeVertex := TListeSimple<TVertexPolygon>.Create;
  try
    QListeVertex.ClearListe();
    for i := 0 to Nb -1 do
    begin
      EWE := ArrIdxPoly[i];
      AfficherMessage(Format('---> Polyligne %d', [EWE]));
      PP := FD.GetPolyligne(EWE);

      QNbV := 1 + high(PP.Sommets);
      if (QNbV = 0) then Continue;
      for a := 0 to QNbV - 1 do QListeVertex.AddElement(PP.Sommets[a]);
    end;
    Nb := QListeVertex.GetNbElements();
    if (Nb = 0) then Exit;
    SetLength(MergedObj.Sommets, Nb);
    for i := 0 to Nb - 1 do
    begin
      MyVertex := QListeVertex.GetElement(i);
      MergedObj.Sommets[i] := MyVertex;
    end;
    // controle
    for i := 0 to Nb - 1 do
    begin
      MyVertex := MergedObj.Sommets[i];
      AfficherMessage(Format('%d: %d', [i, MyVertex.IDStation]));
    end;
    // retourner le résultat: OK si Nb > 0
    Result := (Nb > 0);
    QListeVertex.ClearListe();
  finally
    FreeAndNil(QListeVertex);
  end;

end;

function GenererUnScrapDepuisCourbes(const FD: TDocumentDessin;
                                     const ArrCourbes: TArrayOfTCourbe;
                                     const IdxGrp: TIDGroupeEntites;
                                     out   MergedObj: TScrap): integer;
var
  CP: TCourbePolygoneProvisoire;
  i, n: Integer;
  CC: TCourbe;
begin
  Result := errGEN_SCRAP_FROM_COURBES_ANY_ERROR;
  CP := TCourbePolygoneProvisoire.Create;
  try
    CP.SetDocDessin(FD);
    n := High(ArrCourbes);
    if (n < 1) then Exit;
    CP.LoadCourbe(ArrCourbes[0], false);
    for i := 1 to n do
    begin
      CC := ArrCourbes[i];
      AfficherMessageErreur(Format('%d: Courbe - grp = %d', [i, CC.IDGroupe]));
      if (IdxGrp = CC.IDGroupe) then CP.LoadCourbe(CC, True);
    end;
    MergedObj.IDGroupe := IdxGrp;
    if (CP.GenerateScrap(MergedObj)) then
    begin
      MergedObj.Nom := 'Scrap_from_courbe';
      MergedObj.MarkToDelete := false;
      MergedObj.Couleur := clSilver;
      MergedObj.Opacite := 128;
      MergedObj.LastModified := Now;
      Result := errGEN_SCRAP_FROM_COURBES_OK;
    end;
    CP.ClearVertex;
  finally
    FreeAndNil(CP);//CP.Free;
  end;
end;
function GenererUnPolygoneDepuisCourbes(const FD: TDocumentDessin;
                                     const ArrCourbes: TArrayOfTCourbe;
                                     const IdxGrp: TIDGroupeEntites;
                                     out   MergedObj: TPolygone): integer;
var
  CP: TCourbePolygoneProvisoire;
  i, n: Integer;
  CC: TCourbe;
begin
  Result := errGEN_SCRAP_FROM_COURBES_ANY_ERROR;
  CP := TCourbePolygoneProvisoire.Create;
  try
    CP.SetDocDessin(FD);
    n := High(ArrCourbes);
    if (n < 1) then Exit;
    CP.LoadCourbe(ArrCourbes[0], false);
    for i := 1 to n do
    begin
      CC := ArrCourbes[i];
      if (IdxGrp = CC.IDGroupe) then CP.LoadCourbe(CC, True);
    end;
    if (CP.GeneratePolygon(MergedObj)) then
    begin
      MergedObj.IDGroupe := IdxGrp;
      MergedObj.MarkToDelete := false;
      MergedObj.IDStylePolygone := nopCHEMINS;
      MergedObj.LastModified := Now;
    end;
    CP.ClearVertex;
    Result := errGEN_POLYGONE_FROM_COURBES_OK;
  finally
    FreeAndNil(CP);//CP.Free;
  end;
end;


// TODO: A tester
function CasserUneCourbe(const C: TCourbe; const Idx: integer; out RC1, RC2: TCourbe): boolean;
var
  Nb: Integer;
  function MakeCC(const QStart, QEnd: integer; var RC: TCourbe): boolean;
  var
    QListe : TListeSimple<TArcCourbe>;
    EWE: TArcCourbe;
    n, i: Integer;
  begin
    result := false;
    QListe := TListeSimple<TArcCourbe>.Create;
    try
      try
        QListe.ClearListe();
        n := 1 + (QEnd - QStart);
        SetLength(RC.Arcs, n);
        for i := QStart to QEnd do
        begin
          EWE := C.Arcs[i + QStart];
          RC.Arcs[i] := EWE;
        end;
        result := True;
      except
      end;
      QListe.ClearListe();
    finally
      FreeAndNil(QListe);//QListe.Free;
    end;
  end;
begin
  AfficherMessage(' ---- CasserUneCourbe' );
  try
    Result := false;
    Nb := High(C.Arcs);
    if (Nb < 2) then Exit;
    if (Idx = 0) then Exit;
    if (Idx > Nb) then Exit;
    // recopier les attributs dans les courbes résultats
    RC1 := C;
    Result := MakeCC(0, Idx, RC1);
    RC2 := C;
    Result := MakeCC(Idx, Nb, RC2);
  except
  end;
end;

function CasserUnePolyligne(const C: TPolyLigne; const Idx: integer; out RC1, RC2: TPolyLigne): boolean;
var
  Nb: Integer;

begin
  Result := false;
  Nb := length(C.Sommets);
  if (Nb < 3) then Exit;
  if (Idx = 0) then Exit;
  if (Idx > (Nb-1)) then Exit;
  RC1 := C;          // recopier les attributs dans les courbes résultats
end;

// générer un scrap ou un polygone depuis plusieurs courbes:
//
//      |C4                        |C3
//      |                          |
//-C1-----------------------------------           +------------------------+
//      |                          |               |                        |
//      |                          |               |                        |
//      |   ZONE A POLYGONALISER   |       =       |    POLYGONE            |
//      |                          |               |                        |
//      |                          |               |                        |
//-C2-----------------------------------           +------------------------+
//      |                          |
//      |                          |
function GenererUnPolygoneDepuisIntersectedCourbes(const FD: TDocumentDessin;
                                                   const ArrCourbes: TArrayOfTCourbe;
                                                   const IdxGrp    : TIDGroupeEntites;
                                                   out   MergedObj : TVertexPolygonArray): integer;
begin
  Result := errGEN_SCRAP_OR_POLYGONE_FROM_COURBES_ANY_ERROR;
  //CP := TCourbePolygoneProvisoire.Create;
  try

  finally
    //CP.Free;
  end;
end;
//*****************************************************************************************
{ TOperationsCSGClipping }
//******************************************************************************
function TOperationsCSGClipping.MakeTGeoPolygon2D(const PolyVertex: TVertexPolygonArray): TGeoPolygon2D;
var
  i, n: Integer;
  P: TVertexPolygon;
  BP: TBaseStation;
begin
  n := High(PolyVertex);
  SetLength(Result, n+1);
  for i := 0 to n do
  begin
    P := PolyVertex[i];
    if (FMyDocDessin.GetBasePointByIndex(P.IDStation, BP)) then
    begin
      Result[i].x := BP.PosStation.X + P.Offset.X;
      Result[i].y := BP.PosStation.Y + P.Offset.Y;
    end;
  end;
end;
//******************************************************************************
procedure TOperationsCSGClipping.Initialise(const FD: TDocumentDessin; const P0, P1: TVertexPolygonArray);
begin
  AfficherMessage(Format('%s.Initialise', [ClassName]));
  FMyDocDessin := FD;
  FPath0 := P0;
  FPath1 := P1;
end;
procedure TOperationsCSGClipping.Finalise();
begin
  pass;
end;

function TOperationsCSGClipping.GetVertexPolygonArrayResult: TVertexPolygonArray;
begin
  Result := FVertexPolygonArrayResult;
end;

function TOperationsCSGClipping.IntersectTVertexPolygonArray(const Poly1, Poly2: TVertexPolygonArray): boolean;
var
  I            : Integer;
  J            : Integer;
  Poly1Trailer : Integer;
  Poly2Trailer : Integer;
  BP1, BP2     : TBaseStation;
  BP3, BP4     : TBaseStation;
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
      if (FMyDocDessin.GetBasePointByIndex(Poly1[i].IDStation, BP1)            AND
          FMyDocDessin.GetBasePointByIndex(Poly1[Poly1Trailer].IDStation, BP2) AND
          FMyDocDessin.GetBasePointByIndex(Poly2[j].IDStation, BP3)            AND
          FMyDocDessin.GetBasePointByIndex(Poly2[Poly2Trailer].IDStation, BP4)) then
      begin
        P1.x := BP1.PosStation.X + Poly1[i].Offset.X;
        P1.y := BP1.PosStation.Y + Poly1[i].Offset.Y;
        P2.x := BP2.PosStation.X + Poly1[Poly1Trailer].Offset.X;
        P2.y := BP2.PosStation.Y + Poly1[Poly1Trailer].Offset.Y;
        P3.x := BP3.PosStation.X + Poly2[j].Offset.X;
        P3.y := BP3.PosStation.Y + Poly2[j].Offset.Y;
        P4.x := BP4.PosStation.X + Poly2[Poly2Trailer].Offset.X;
        P4.y := BP4.PosStation.Y + Poly2[Poly2Trailer].Offset.Y;
        if (FastGEO.Intersect(P1, P2, P3, P4)) then Exit(True);
        Poly2Trailer := j;
      end;
    end;
    Poly1Trailer := i;
  end;
end;

//  DONE: Cette fonction n'utilise que les basepoints des chemins
//        (aucun risque d'attrapper des points non filtrés)
function TOperationsCSGClipping.GetNearBasePointIdx(const qX, qY: double): TIDBaseStation;
var
  Dist: Extended;
  ResultIDBasePoint: TIDBaseStation;
  i, n: Integer;
  procedure EWE(const P: TVertexPolygonArray);
  var
    i, n   : Integer;
    BP     : TBaseStation;
    FX, FY : Extended;
    WU     : Extended;
  begin
    n := High(P);
    for i := 0 to n do
    begin
      // ceci montre qu'on ne travaille que sur les Basepoints des polygones
      if (FMyDocDessin.GetBasePointByIndex(P[i].IDStation, BP)) then
      begin
        FX := qX - (BP.PosStation.X + P[i].Offset.X);
        FY := qY - (BP.PosStation.Y + P[i].Offset.Y);
        WU := FX*FX + FY*FY;
        if (WU < Dist) then
        begin
          Dist := WU;
          ResultIDBasePoint := BP.IDStation;
        end;
      end;
    end;
  end;
begin
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  Result := '';
  {$ELSE}
  Result := -1;
  {$ENDIF TIDBASEPOINT_AS_TEXT}

  Dist := INFINITE;
  EWE(FPath0);
  EWE(FPath1);
  Result := ResultIDBasePoint;
end;




function TOperationsCSGClipping.IntersectionVertexPolygonArray(): boolean;
begin
  Result := IntersectTVertexPolygonArray(FPath0, FPath1);
end;

function TOperationsCSGClipping.MakeTPathFromPoly(const P: TVertexPolygonArray): TPath;
var
  i, n: Integer;
  BP: TBaseStation;
begin
  n := High(P);
  Setlength(Result, n+1);
  for i := 0 to n do
  begin
    if (FMyDocDessin.GetBasePointByIndex(P[i].IDStation, BP)) then
    begin
      // /!\ Clipper travaille sur des entiers: on passe tout en centimètres
      Result[i].X := Round(MULTIPLICATEUR_UNITES * (BP.PosStation.X + P[i].Offset.X));
      Result[i].Y := Round(MULTIPLICATEUR_UNITES * (BP.PosStation.Y + P[i].Offset.Y));
    end;
  end;
end;

function TOperationsCSGClipping.ExecuteCSG(const ClipType: TClipType): boolean;
var
  CP: TClipper;
  ResultatUnion: TPaths;
  MyPathResult: TPath;
  NbVtxResult: Integer;
  i: Integer;
  Xo,  Yo: Extended;
  IdxBP: TIDBaseStation;
  BasePoint0: TBaseStation;
begin
  Result := false;
  if (self.IntersectionVertexPolygonArray()) then
  begin
    AfficherMessage('-- Chemins 1 et 2 se coupent');
    // ici, on va appeler les fonctions Clipper
    CP := TClipper.Create();
    try
      CP.AddPath(MakeTPathFromPoly(FPath0), ptSubject, True);
      CP.AddPath(MakeTPathFromPoly(FPath1), ptClip, True);
      if (CP.Execute(ClipType, ResultatUnion)) then
      begin
        MyPathResult := ResultatUnion[0];
        NbVtxResult  := 1 + high(MyPathResult);
        SetLength(FVertexPolygonArrayResult, NbVtxResult);
        for i := 0 to NbVtxResult - 1 do
        begin
          Xo := MyPathResult[i].X / MULTIPLICATEUR_UNITES; // et on repasse tout en mètres
          Yo := MyPathResult[i].Y / MULTIPLICATEUR_UNITES;

          IdxBP := self.GetNearBasePointIdx(Xo, Yo);
          if (FMyDocDessin.GetBasePointByIndex(IdxBP, BasePoint0)) then
          begin
            FVertexPolygonArrayResult[i].IDStation := BasePoint0.IDStation;
            FVertexPolygonArrayResult[i].Offset.X := Xo - BasePoint0.PosStation.X;
            FVertexPolygonArrayResult[i].Offset.Y := Yo - BasePoint0.PosStation.Y;
          end;
        end;  // for i := 0 to NbVtxResult - 1 do
        Result := True;
      end;    // if (CP.Execute(ctIntersection, ResultatUnion)) then
    finally
      FreeAndNil(CP);//CP.Free;
    end;
  end
end;

function GenererUnScrapDepuisPolylignes(const FD: TDocumentDessin;
                                        const ArrPolylignes: TArrayOfTPolyligne;
                                        const IdxGrp: TIDGroupeEntites;
                                        out MergedObj: TScrap): integer;
var
  CP: TCourbePolygoneProvisoire;
  i, n: Integer;
  CC: TPolyLigne;
begin
  Result := errGEN_SCRAP_FROM_POLYLIGNES_ANY_ERROR;
  CP := TCourbePolygoneProvisoire.Create;
  try
    CP.SetDocDessin(FD);
    n := High(ArrPolylignes);
    if (n < 1) then Exit;
    CP.LoadPolyligne(ArrPolylignes[0], false);
    for i := 1 to n do
    begin
      //if (ArrCourbes[i].IDGroupe <> ArrCourbes[0].IDGroupe) then Continue;
      CC := ArrPolylignes[i];
      AfficherMessageErreur(Format('%d: Polyligne - grp = %d', [i, CC.IDGroupe]));
      if (IdxGrp = CC.IDGroupe) then CP.LoadPolyligne(CC, True);
    end;
    if (CP.GenerateScrap(MergedObj)) then
    begin
      MergedObj.Nom := 'Scrap_from_polyline';
      MergedObj.MarkToDelete := false;
      MergedObj.Couleur := clSilver;
      MergedObj.Opacite := 128;
      MergedObj.LastModified := Now;
    end;
    CP.ClearVertex;
    Result := errGEN_SCRAP_FROM_COURBES_OK;
  finally
    FreeAndNil(CP);//CP.Free;
  end;
end;

end.

