unit UnitGraphismesProvisoires;
{$INCLUDE CompilationParameters.inc}
// 25/02/2015: Début du développement de GenerateCurveByCloudPoints()
// 27/02/2015: GenerateCurveByCloudPoints() OK (méthode du 1 point sur N)
// 14/04/2015: Petit bug corrigé dans GeneratePolyligne()
// 22/06/2015: Purge des vertex (eg: vertex trop proches)
// 30/06/2015: Insertion de vertex
// 30/07/2015: Fusion de polylignes et de courbes (dans UnitGraphismesProvisoires)
// 06/09/2016: Les fonctions GeneratePolyLine(), GenerateCourbe(), GeneratePolygone() et GenerateScrap() retournent
//             désormais un booléen à tester par l'appelant + le résultat en 'out'
interface
uses
  SysUtils
  , Classes
  , UnitListesSimplesWithGeneriques
  , Graphics
  , GHCD_Types
  , Math
  , GeneralFunctions
  , UnitDocDessin
  ;
// structure provisoire de stockage des courbes et polygones
type TCourbePolygoneProvisoire = class(TListeSimple<TVertexCourbe>)
     procedure ClearVertex(); inline;
     procedure AddVertex(const V : TVertexCourbe); inline;
     procedure InsertVertex(const Idx: integer; const V: TVertexCourbe); inline;
     procedure DeleteVertex(const Idx: integer); inline;
     procedure DeleteLastVertex(); inline;


     function  GetVertex(const Idx: integer): TVertexCourbe; inline;
     procedure PutVertex(const Idx: integer; const V: TVertexCourbe); inline;

     procedure SetIDGroupe(const Id: TIDGroupeEntites);
     procedure SetIDStyleCourbe(const Id: TNatureObjetCourbe);
     procedure SetIDStylePolygone(const Id: TNatureObjetPolygone);


     function  GenerateCourbe(const ByDefault: boolean;
                              const QIdxStart, QIdxEnd: integer;
                              out   CResult: TCourbe): boolean;
     function  GeneratePolyLine(out P: TPolyLigne): boolean;
     function  GeneratePolygon(out P: TPolygone): boolean;
     function  GenerateScrap(out P: TScrap): boolean;
     // charger une courbe
     procedure LoadCourbe(const CV: TCourbe; const DoAppend: boolean);
     // charger une polyligne
     procedure LoadPolyligne(const PV: TPolyLigne; const DoAppend: boolean);
     // charger un polygone
     procedure LoadPolygone(const PV: TPolygone);
     // charger un scrap
     procedure LoadScrap(const PV: TScrap);
     // charger une courbe de sections transversale
     //function LoadCrossSectionCourbe(const CV: TCrossSectionCourbe; const DoAppend: boolean): boolean;
     // recherche un point d'après un couple de coordonnées
     function GetIDVertexByXY(const X, Y: double): integer;
     // recherche un point ou ses tangentes d'après un couple de coordonnées
     // résultat: ID Vertex * 10 + p
     // avec p = 0 si point lui-même
     //      p = 1 si tangente gauche
     //      p = 2 si tangente droite
     function GetIDVertexTgtsByXY(const X, Y: double): Integer;
     // inverser la liste des sommets
     procedure StripVertex();
     // ajouter un sommet au voisinage de la souris
     function AddVertexNearToPoint(const X, Y: double): boolean;
     // générer une courbe d'après une suite de points
     function  GenerateCurveBySuiteDePoints(const CP: TArrayPoints2Df;
                                            const QBasePointLocked: boolean;
                                            const QIDGroupeEditing: integer;
                                            const QIDStyleCurveEditing: TNatureObjetCourbe;
                                            const Nb_Pts_Skipped: integer): boolean;
     // fusion et fission de courbes et polylignes
     // génère deux TArrayPoints2Df à partir de la liste des vertex
     // fusionne (= en fait ajoute les vertex de) un TArrayPoints2Df
     //function FissionnerCourbe(const IdxPt: integer; out S1, S2: TCourbe): boolean;
     //function FissionnerPolyligne(const IdxPt: integer; out S1, S2: TPolyLigne): boolean;
     function FusionnerTArrayPoints2Df(const S1: TArrayVertexCourbe): boolean;
   private
     FDocDessin   : TDocumentDessin; // pour les points de base
     FMyScrap     : TScrap;
     FMyPolyligne : TPolyLigne;
     FMyPolygone  : TPolygone;
     FIDGroupe    : TIDGroupeEntites;
     FIDStyleCourbeEditing  : TNatureObjetCourbe;
     FIDStylePolygoneEditing: TNatureObjetPolygone;
     function ExtractXYFromTVertexCourbe(const QV: TVertexCourbe): TPoint2Df;
   public
     procedure SetDocDessin(const FD: TDocumentDessin);
     function  GetNbVertex(): integer; inline;
     procedure PurgerVertex(const Seuil: double);
     function  GetLongueurDeveloppee(const B: boolean; const QX, QY: double): double;
     function  GetPerimetreAndAire(out Perimetre, Aire: double): boolean;

end;

// pour les sections transversales
type TCrossSectionPolyProvisoire = class(TListeSimple<TCrossSectionVertexCourbe>)
     procedure ClearVertex(); inline;
     procedure AddVertex(const V : TCrossSectionVertexCourbe); inline;
     procedure AddVertexByXY(const QX, QY: double);
     procedure DeleteVertex(const Idx: integer); inline;
     procedure DeleteLastVertex(); inline;


     function  GetVertex(const Idx: integer): TCrossSectionVertexCourbe; inline;
     procedure PutVertex(const Idx: integer; const V: TCrossSectionVertexCourbe); inline;
     function  GenererCourbe(const ByDefault: boolean; out C: TCrossSectionCourbe): boolean;
     function  GenererPolygone(out P: TCrossSectionPolygone): boolean;
     // charger une courbe
     procedure LoadCourbe(const CV: TCrossSectionCourbe; const DoAppend: boolean);
     // charger un polygone
     procedure LoadPolygone(const PV: TCrossSectionPolygone);
     // recherche un point d'après un couple de coordonnées
     function GetIDVertexByXY(const X, Y: double): integer;
     // recherche un point ou ses tangentes d'après un couple de coordonnées
     // résultat: ID Vertex * 10 + p
     // avec p = 0 si point lui-même
     //      p = 1 si tangente gauche
     //      p = 2 si tangente droite
     // inverser la liste des sommets
     procedure StripVertex();
     // ajouter un sommet au voisinage de la souris
     function AddVertexNearToPoint(const X, Y: double): boolean;

   private
     FMyPolygone  : TCrossSectionPolygone;
     FMyCourbe    : TCrossSectionCourbe;
   public
     function  GetNbVertex(): integer;
     procedure PurgerVertex(const Seuil: double);
end;

//------------------------------------------------------------------------------
function PurgerScrap(const FD: TDocumentDessin; var P: TScrap): boolean;
function PurgerCourbe(const FD: TDocumentDessin; var P: TCourbe): boolean;
function PurgerPolygone(const FD: TDocumentDessin; var P: TPolygone): boolean;
function PurgerPolyligne(const FD: TDocumentDessin; var P: TPolyLigne): boolean;


implementation
//------------------------------------------------------------------------------
function PurgerPolygone(const FD: TDocumentDessin; var P: TPolygone): boolean;
var
  CP: TCourbePolygoneProvisoire;
begin
  result := false;
  CP:= TCourbePolygoneProvisoire.Create;
  try
    CP.SetDocDessin(FD);
    CP.LoadPolygone(P);
    CP.PurgerVertex(DIST_MERGE_VERTEX);
    Result := CP.GeneratePolygon(P);
  finally
    FreeAndNil(CP);//     CP.Free;
  end;
end;
function PurgerPolyligne(const FD: TDocumentDessin; var P: TPolyLigne): boolean;
var
  CP: TCourbePolygoneProvisoire;
begin
  result := false;
  CP:= TCourbePolygoneProvisoire.Create;
  try
    CP.SetDocDessin(FD);
    CP.LoadPolyligne(P, false);
    CP.PurgerVertex(DIST_MERGE_VERTEX);
    Result := CP.GeneratePolyLine(P);
  finally
    FreeAndNil(CP);//     CP.Free;
  end;
end;


function PurgerScrap(const FD: TDocumentDessin; var P: TScrap): boolean;
var
  CP: TCourbePolygoneProvisoire;
begin
  result := false;
  CP:= TCourbePolygoneProvisoire.Create;
  try
    CP.SetDocDessin(FD);
    CP.LoadScrap(P);
    CP.PurgerVertex(DIST_MERGE_VERTEX);
    Result := CP.GenerateScrap(P);
  finally
    FreeAndNil(CP);//     CP.Free;
  end;
end;
function PurgerCourbe(const FD: TDocumentDessin; var P: TCourbe): boolean;
var
  CP: TCourbePolygoneProvisoire;
begin
  result := false;
  CP:= TCourbePolygoneProvisoire.Create;
  try
    CP.SetDocDessin(FD);
    CP.LoadCourbe(P, false);
    CP.PurgerVertex(DIST_MERGE_VERTEX);
    Result := CP.GenerateCourbe(True, -1, -1, P);
  finally
    FreeAndNil(CP);//     CP.Free;
  end;
end;

{ TCrossSectionPolyProvisoire }

procedure TCrossSectionPolyProvisoire.AddVertex(const V: TCrossSectionVertexCourbe);
begin
  self.AddElement(V);
end;

procedure TCrossSectionPolyProvisoire.AddVertexByXY(const QX, QY: double);

var
  V: TCrossSectionVertexCourbe;
begin
  V.Position := MakeTPoint2Df(QX, QY);
  V.TangDroite := MakeTPoint2Df(0.00, 0.00);
  V.TangGauche := MakeTPoint2Df(0.00, 0.00);
  self.AddVertex(V);
end;

function TCrossSectionPolyProvisoire.AddVertexNearToPoint(const X, Y: double): boolean;
begin
  result := false;
end;

procedure TCrossSectionPolyProvisoire.ClearVertex;
begin
  self.ClearListe();
end;

procedure TCrossSectionPolyProvisoire.DeleteLastVertex;
begin
  self.RemoveLastElement();
end;

procedure TCrossSectionPolyProvisoire.DeleteVertex(const Idx: integer);
begin
  self.RemoveElement(Idx);
end;

function TCrossSectionPolyProvisoire.GenererCourbe(const ByDefault: boolean; out C: TCrossSectionCourbe): boolean;
var
  QNbPts, i: Integer;
  AB , A0, A1: TCrossSectionArcCourbe;
  R1, R2: float;
  AngleTg2, AngleTg1: ValReal;
  V1, V2: TCrossSectionVertexCourbe;
  DeltaX, DeltaY: Double;
begin
  AfficherMessageErreur(Format('%s.GenerateCourbe: %d vertex', [ClassName, GetNbVertex()]));
  Result := false;

  (*
  // contrôle des vertex
  QNbPts := GetNbVertex();

  if (0 = QNbPts) then exit;
  for i := 0 to QNbPts-1 do
  begin
    V1 := GetVertex(i);
    AfficherMessageErreur(Format('--> %d: %.2f, %.2f', [i, V1.Position.X, V1.Position.Y]));
  end;
  //*)

  PurgerVertex(DIST_MERGE_VERTEX);
  QNbPts := GetNbVertex();
   if (QNbPts <= 1) then
   begin
     AfficherMessage('--> ** La courbe ne comporte pas de segment. Reinitialisee **');
     self.ClearVertex();
     Exit;
   end;
   // fixation du nombre d'arcs = nb de sommets - 1
   SetLength(C.Arcs, 1);
   DeltaX := 0.00; DeltaY := 0.00;
   //AfficherMessage(IntToStr(QNbPts));
   SetLength(C.Arcs, QNbPts-1);
   // TODO: C'est dans ce secteur qu'il faut gérer le mode ByDefault
   for i := 1 to QNbPts-1 do
   begin
     V1 := GetVertex(i-1);
     V2 := GetVertex(i);
     AB.CoordsP1 := V1.Position;
     AB.CoordsP2 := V2.Position;
     DeltaX := V2.Position.X - V1.Position.X;
     DeltaY := V2.Position.Y - V1.Position.Y;
     if (ByDefault) then
     begin
       AB.TangP1.X := DeltaX / 3;
       AB.TangP1.Y := DeltaY / 3;
       R1 := Hypot(DeltaX, DeltaY) / 3;

       AB.TangP2.X := -AB.TangP1.X;
       AB.TangP2.Y := -AB.TangP1.Y;
       C.Arcs[i-1] := AB;
       // angle
       AngleTg1 := AngleTg2;
     end else
     begin
       AB.TangP1 := V1.TangDroite;
       AB.TangP2 := V2.TangGauche;
       C.Arcs[i-1] := AB;
     end;
   end;
   // calcul des tangentes (construction par défaut)
   if (ByDefault) then
   begin
     for i := 1 to High(C.Arcs)  do
     begin
       //C.Arcs[i];
       A0 := C.Arcs[i-1];
       A1 := C.Arcs[i];

       R1 := Hypot(A0.TangP2.X, A0.TangP2.Y);
       AngleTg2 := GetAngleBissecteur(-A0.TangP2.X, -A0.TangP2.Y,
                                       A1.TangP1.X,  A1.TangP1.Y) - PI_2;
       C.Arcs[i-1].TangP2.X := R1 * cos(AngleTg2);
       C.Arcs[i-1].TangP2.Y := R1 * sin(AngleTg2);

       R2 := Hypot(A1.TangP1.X, A1.TangP1.Y);

       AngleTg1 := AngleTg2 + PI;
       C.Arcs[i].TangP1.X := R2 * cos(AngleTg1);
       C.Arcs[i].TangP1.Y := R2 * sin(AngleTg1);
     end;
  end;
  C.IDStyleCourbe := FMyCourbe.IDStyleCourbe;
  //******************************************************
  Result := True;
end;



function TCrossSectionPolyProvisoire.GenererPolygone(out P: TCrossSectionPolygone): boolean;
var
  QNbPts, i: Integer;
  VC: TCrossSectionVertexCourbe;
begin
  AfficherMessageErreur(Format('%s.GenererPolygone: %d vertex', [ClassName, GetNbVertex()]));
  Result := false;
  QNbPts := GetNbVertex();
  (*
  // contrôle des vertex

  if (0 = QNbPts) then exit;
  for i := 0 to QNbPts-1 do
  begin
    VC := GetVertex(i);
    AfficherMessageErreur(Format('--> %d: %.2f, %.2f', [i, VC.Position.X, VC.Position.Y]));
  end;
  //*)
  SetLength(P.Vertexes, 0);
  SetLength(P.Vertexes, QNbPts);
  for i:=0 to High(P.Vertexes) do
  begin
    VC := GetVertex(i);
    P.Vertexes[i] := MakeTPoint2Df(VC.Position.X, VC.Position.Y);
  end;
  P.IDStylePolygone := FMyPolygone.IDStylePolygone;
  Result := true;
end;



function TCrossSectionPolyProvisoire.GetIDVertexByXY(const X, Y: double): integer;
var
  d2, r2: double;
  i, n: Integer;
  VC: TCrossSectionVertexCourbe;
begin
  Result := -1;
  n := GetNbVertex();
  if (n = 0) then Exit;
  r2 := INFINITE;
  for i := 0 to n-1 do
  begin
    VC := GetVertex(i);
    d2 := Hypot(VC.Position.X - X, VC.Position.Y - Y);
    if (d2 < r2) then
    begin
      Result := i;
      r2     := d2;
    end;
  end;
end;

function TCrossSectionPolyProvisoire.GetNbVertex: integer;
begin
  result := self.GetNbElements();
end;

function TCrossSectionPolyProvisoire.GetVertex(const Idx: integer): TCrossSectionVertexCourbe;
begin
  result := GetElement(Idx);
end;



procedure TCrossSectionPolyProvisoire.LoadCourbe(const CV: TCrossSectionCourbe; const DoAppend: boolean);
var
  n, i: Integer;
  V1: TCrossSectionVertexCourbe;
begin
  AfficherMessageErreur(Format('%s.LoadCourbe', [ClassName]));
  FMyCourbe  := CV;
  if (not DoAppend) then self.ClearVertex();
  n := High(CV.Arcs);
  if (n < 0) then Exit;

  //AfficherMessage(Format('Arc %d: %f %f - %f %f',[0, CV.Arcs[0].TangP1.X, CV.Arcs[0].TangP1.Y, CV.Arcs[0].TangP2.X, CV.Arcs[0].TangP2.Y ]));
  // premier sommet
  V1.Position.X := CV.Arcs[0].CoordsP1.X;
  V1.Position.Y := CV.Arcs[0].CoordsP1.Y;
  V1.TangGauche.X:= -CV.Arcs[0].TangP1.X;
  V1.TangGauche.Y:= -CV.Arcs[0].TangP1.Y;
  V1.TangDroite  := CV.Arcs[0].TangP1;
  self.AddVertex(V1);
  for i := 1 to n do
  begin
    V1.Position.X := CV.Arcs[i-1].CoordsP2.X;
    V1.Position.Y := CV.Arcs[i-1].CoordsP2.Y;
    V1.TangGauche:= CV.Arcs[i-1].TangP2;
    V1.TangDroite:= CV.Arcs[i].TangP1;
    AddVertex(V1);
  end;
  // dernier vertex
  V1.Position.X := CV.Arcs[n].CoordsP2.X;
  V1.Position.Y := CV.Arcs[n].CoordsP2.Y;
  V1.TangGauche := CV.Arcs[n].TangP2; //
  V1.TangDroite := MakeTPoint2Df(0.0, 0.0);
  AddVertex(V1);
  // contrôle
  (*
  AfficherMessageErreur('Points de la courbe');
  n := GetNbVertex();


  for i:=0 to n-1 do
  begin
    V1 := GetVertex(i);
    AfficherMessageErreur(Format('%d: %f %f - G=(%f %f), D=(%f %f)',
                       [i,
                        V1.Position.X, V1.Position.Y,
                        V1.TangGauche.X, V1.TangGauche.Y,
                        V1.TangDroite.X, V1.TangDroite.Y
                       ]));
  end;
  //*)
end;

procedure TCrossSectionPolyProvisoire.LoadPolygone(const PV: TCrossSectionPolygone);
var
  n, i: Integer;
  V1: TCrossSectionVertexCourbe;
begin
  AfficherMessageErreur(Format('%s.LoadPolygone', [ClassName]));
  FMyPolygone  := PV;
  self.ClearVertex();
  n := length(PV.Vertexes);
  if (n = 0) then Exit;
  for i:= 0 to n - 1 do
  begin
    V1.Position.X   := PV.Vertexes[i].X;
    V1.Position.Y   := PV.Vertexes[i].Y;
    V1.TangGauche.X := 0.00;
    V1.TangGauche.Y := 0.00;
    V1.TangDroite   := V1.TangGauche;
    AddVertex(V1);
  end;
  // contrôle
  (*
  for i:=0 to n-1 do
  begin
    V1 := GetVertex(i);
    AfficherMessageErreur(Format('%d: %f %f', [i, V1.Position.X, V1.Position.Y]));
  end;
  //*)
end;

procedure TCrossSectionPolyProvisoire.PurgerVertex(const Seuil: double);
var
  Delta: double;
  i: Integer;
  V0, V1: TCrossSectionVertexCourbe;
begin
  AfficherMessageErreur(Format('%s.PurgerVertex: seuil = %.3f, %d vertex', [ClassName, Seuil, GetNbVertex()]));

  for i := GetNbVertex() - 1 downto 1 do // //while (i < GetNbVertex()) do
  begin
    V0 := GetVertex(i-1);
    V1 := GetVertex(i);
    Delta := Hypot(V1.Position.X - V0.Position.X,
                   V1.Position.Y - V0.Position.Y);
    if (Delta < Seuil) then DeleteVertex(i);
  end;
end;

procedure TCrossSectionPolyProvisoire.PutVertex(const Idx: integer; const V: TCrossSectionVertexCourbe);
begin
  self.PutElement(Idx, V);
end;


procedure TCrossSectionPolyProvisoire.StripVertex;
begin
  pass;
end;


//******************************************************************************
// structure provisoire de stockage des courbes
// Reset = Purge de la structure
procedure TCourbePolygoneProvisoire.ClearVertex();
begin
  self.ClearListe();
end;

procedure TCourbePolygoneProvisoire.DeleteLastVertex();
begin
  self.RemoveLastElement();
end;

procedure TCourbePolygoneProvisoire.AddVertex(const V : TVertexCourbe);
begin
  self.AddElement(V);
end;


procedure TCourbePolygoneProvisoire.DeleteVertex(const Idx: integer);
begin
  self.RemoveElement(Idx);
end;



function TCourbePolygoneProvisoire.GetVertex(const Idx: integer): TVertexCourbe;
begin
  Result := self.GetElement(Idx);
end;

procedure TCourbePolygoneProvisoire.InsertVertex(const Idx: integer; const V: TVertexCourbe);
begin
  self.InsertElement(Idx, V);
end;

procedure TCourbePolygoneProvisoire.PutVertex(const Idx: integer; const V: TVertexCourbe);
begin
  self.PutElement(Idx, V);
end;

procedure TCourbePolygoneProvisoire.SetDocDessin(const FD: TDocumentDessin);
begin
  FDocDessin := FD;
end;



procedure TCourbePolygoneProvisoire.SetIDGroupe(const Id: TIDGroupeEntites);
begin
  FIDGroupe := Id;
end;

procedure TCourbePolygoneProvisoire.SetIDStyleCourbe(const Id: TNatureObjetCourbe);
begin
  FIDStyleCourbeEditing := Id;
end;

procedure TCourbePolygoneProvisoire.SetIDStylePolygone(const Id: TNatureObjetPolygone);
begin
  FIDStylePolygoneEditing := Id;
end;



function TCourbePolygoneProvisoire.GeneratePolygon(out P: TPolygone): boolean;
var
  i: integer;
  VC: TVertexCourbe;
  VP: TVertexPolygon;
  FNbPts: Integer;
begin
  Result := false;
  PurgerVertex(DIST_MERGE_VERTEX);
  FNbPts := GetNbVertex;
  if (FNbPts <= 1) then begin
    AfficherMessage('--> ** Le polygone ne comporte pas de segment. Reinitialisee **');
    self.ClearVertex;
    Exit;
  end;
  P.IDGroupe := FIDGroupe;
  P.IDStylePolygone := FIDStylePolygoneEditing;
  SetLength(P.Sommets, 0);
  SetLength(P.Sommets, FNbPts);
  for i:=0 to High(P.Sommets) do
  begin
    VC := GetVertex(i);
    VP.IDStation := VC.IDStation;
    VP.Offset    := VC.Offset;
    P.Sommets[i] := VP;
  end;
  Result := true;
end;
procedure TCourbePolygoneProvisoire.LoadCourbe(const CV: TCourbe; const DoAppend: boolean);
var
  i, n: integer;
  V1: TVertexCourbe;
begin
  //AfficherMessage(Format('%s.LoadCourbe', [ClassName]));
  FIDGroupe               := CV.IDGroupe;
  FIDStyleCourbeEditing   := CV.IDStyleCourbe;
  FIDStylePolygoneEditing := nopDEFAULT;
  if (not DoAppend) then self.ClearVertex();
  if (High(CV.Arcs) < 0) then Exit;
  n := High(CV.Arcs);
  if (n = 0) then Exit;
  //AfficherMessage(Format('Arc %d: %f %f - %f %f',[0, CV.Arcs[0].TangP1.X, CV.Arcs[0].TangP1.Y, CV.Arcs[0].TangP2.X, CV.Arcs[0].TangP2.Y ]));
  // premier sommet
  V1.IDStation   :=  CV.Arcs[0].IDStationP1;
  V1.Offset      :=  CV.Arcs[0].OffsetP1;
  V1.TangGauche.X:= -CV.Arcs[0].TangP1.X;
  V1.TangGauche.Y:= -CV.Arcs[0].TangP1.Y;
  V1.TangGauche.Z:= -CV.Arcs[0].TangP1.Z;

  V1.TangDroite  := CV.Arcs[0].TangP1;
  self.AddVertex(V1);
  for i := 1 to n do
  begin
    V1.IDStation := CV.Arcs[i-1].IDStationP2;
    V1.Offset    := CV.Arcs[i-1].OffsetP2;
    V1.TangGauche:= CV.Arcs[i-1].TangP2;
    V1.TangDroite:= CV.Arcs[i].TangP1;
    AddVertex(V1);
  end;
  // dernier vertex
  V1.IDStation := CV.Arcs[n].IDStationP2;
  V1.Offset    := CV.Arcs[n].OffsetP2;
  V1.TangGauche:= CV.Arcs[n].TangP2; //
  V1.TangDroite:= GetConstantVector3D(0.00); //
  AddVertex(V1);
  // contrôle
  (*
  AfficherMessage('Points de la courbe');
  FNbPts := GetNbVertex();

  for i:=0 to FNbPts-1 do
  begin
    V1 := GetVertex(i);
    AfficherMessage(Format('%d: ID %d (%f %f) - G=(%f %f), D=(%f %f)',
                       [i, V1.IDStation,
                        V1.Offset.X, V1.Offset.Y,
                        V1.TangGauche.X, V1.TangGauche.Y,
                        V1.TangDroite.X, V1.TangDroite.Y
                       ]));
  end;
  FMyCourbe := CV;
  //*)
end;

procedure TCourbePolygoneProvisoire.LoadPolyligne(const PV: TPolyLigne; const DoAppend: boolean);
var
  i, n: integer;
  V1: TVertexCourbe;
  FNbPts: Integer;
begin
  FIDGroupe := PV.IDGroupe;
  FIDStyleCourbeEditing   := PV.IDStylePolyLine;
  FIDStylePolygoneEditing := nopDEFAULT;
  if (not DoAppend) then // DoAppend = Ajout à la courbe existante
  begin                  // sinon, on crée un nouvel objet
    self.ClearVertex;
    FMyPolyligne := PV;
  end;
  n := High(PV.Sommets);
  if (n = 0) then Exit;
  for i:= 0 to n  do
  begin
    V1.IDStation := PV.Sommets[i].IDStation;
    V1.Offset    := PV.Sommets[i].Offset;
    AddVertex(V1);
  end;
  // contrôle
  (*
  FNbPts := GetNbVertex();
  for i:=0 to FNbPts-1 do
  begin
    V1 := GetVertex(i);
    AfficherMessage(Format('%d: ID %d (%f %f)',
                       [i, V1.IDStation,
                        V1.Offset.X, V1.Offset.Y
                       ]));
  end;
  //*)
end;

procedure TCourbePolygoneProvisoire.LoadScrap(const PV: TScrap);
var
  i, n: integer;
  V1: TVertexCourbe;
  FNbPts: Integer;
begin
  FIDGroupe := PV.IDGroupe;
  FIDStyleCourbeEditing   := nocDEFAULT;
  FIDStylePolygoneEditing := nopDEFAULT;
  FMyScrap := PV;
  //AfficherMessage(Format('%s.LoadScrap()', [ClassName]));
  n := High(PV.Sommets);
  for i:= 0 to n  do
  begin
    V1.IDStation := PV.Sommets[i].IDStation;
    V1.Offset    := PV.Sommets[i].Offset;
    AddVertex(V1);
  end;
  // contrôle
  (*
  FNbPts := GetNbVertex();
  for i:=0 to FNbPts-1 do V1 := GetVertex(i);

  begin

    AfficherMessage(Format('%d: ID %d (%f %f)',
                       [i, V1.IDStation,
                        V1.Offset.X, V1.Offset.Y
                       ]));
  end;
  //*)
end;
(*
function TCourbePolygoneProvisoire.LoadCrossSectionCourbe(const CV: TCrossSectionCourbe; const DoAppend: boolean): boolean;
begin
  Result := false;
  AfficherMessage(Format('%s.LoadCrossSectionCourbe', [ClassName]));
  FIDGroupe               := -1;
  FIDStyleCourbeEditing   := -1;
  if (not DoAppend) then self.ClearVertex();
  n := High(CV.Arcs);
  if (High(CV.Arcs) <= 0) then Exit;
  // premier sommet
  V1.IDStation   := CV.Arcs[0].IDStationP1;
  V1.Offset      := CV.Arcs[0].OffsetP1;
  V1.TangGauche.X:= -CV.Arcs[0].TangP1.X;
  V1.TangGauche.Y:= -CV.Arcs[0].TangP1.Y;
  V1.TangGauche.Z:= -CV.Arcs[0].TangP1.Z;

  V1.TangDroite  := CV.Arcs[0].TangP1;
  self.AddVertex(V1);
  for i := 1 to n do
  begin
    V1.IDStation := CV.Arcs[i-1].IDStationP2;
    V1.Offset    := CV.Arcs[i-1].OffsetP2;
    V1.TangGauche:= CV.Arcs[i-1].TangP2;
    V1.TangDroite:= CV.Arcs[i].TangP1;
    AddVertex(V1);
  end;
  // dernier vertex
  V1.IDStation := CV.Arcs[n].IDStationP2;
  V1.Offset    := CV.Arcs[n].OffsetP2;
  V1.TangGauche:= CV.Arcs[n].TangP2; //
  V1.TangDroite:= GetConstantVector3D(0.00); //
  AddVertex(V1);
  // contrôle
  (*
  AfficherMessage('Points de la courbe');
  FNbPts := GetNbVertex();

  for i:=0 to FNbPts-1 do
  begin
    V1 := GetVertex(i);
    AfficherMessage(Format('%d: ID %d (%f %f) - G=(%f %f), D=(%f %f)',
                       [i, V1.IDStation,
                        V1.Offset.X, V1.Offset.Y,
                        V1.TangGauche.X, V1.TangGauche.Y,
                        V1.TangDroite.X, V1.TangDroite.Y
                       ]));
  end;
  FMyCourbe := CV;
end;
//*)
procedure TCourbePolygoneProvisoire.PurgerVertex(const Seuil: double);
var
  i , n: integer;
  V0: TVertexCourbe;
  V1: TVertexCourbe;
  Delta: double;
  VoV1Confondus: Boolean;
begin
  // supprimer les vertex trop proches l'un de l'autre
  n := GetNbVertex();
  if (0 = n) then exit;
  for i := n - 1 downto 1 do // //while (i < GetNbVertex()) do
  begin
    V0 := GetVertex(i-1);
    V1 := GetVertex(i);
    Delta := Hypot(V1.Offset.X - V0.Offset.X, V1.Offset.Y - V0.Offset.Y);
    VoV1Confondus := (Delta < Seuil);
    if ((V0.IDStation = V1.IDStation) and VoV1Confondus) then  DeleteVertex(i);
  end;
end;
function TCourbePolygoneProvisoire.ExtractXYFromTVertexCourbe(const QV: TVertexCourbe): TPoint2Df;
 var
   BP: TBaseStation;
 begin
   Result := MakeTPoint2Df(0.00, 0.00);
   if (not FDocDessin.GetBasePointByIndex(QV.IDStation, BP)) then exit;
   Result := MakeTPoint2Df(BP.PosStation.X + QV.Offset.X, BP.PosStation.Y + QV.Offset.Y);
 end;

function TCourbePolygoneProvisoire.GetLongueurDeveloppee(const B: boolean; const QX, QY: double): double;
var
  V: TVertexCourbe;
  PP0, PP1: TPoint2Df;
  Nb, i: Integer;
begin
  Result := 0.00;
  Nb := GetNbVertex();
  if (Nb = 0) then exit(0.00);
  V := GetVertex(0);  // premier point
  PP0 := ExtractXYFromTVertexCourbe(V);
  if (Nb > 1) then
  begin
    for i := 1 to Nb - 1 do
    begin
      V  := GetVertex(i);
      PP1 := ExtractXYFromTVertexCourbe(V);
      Result += Hypot(PP1.X - PP0.X, PP1.Y - PP0.Y);
      PP0 := PP1;
    end;
  end;
  if (B) then Result += Hypot(QX - PP0.X, QY - PP0.Y); // courbe fermée ?
end;

function TCourbePolygoneProvisoire.GetPerimetreAndAire(out Perimetre, Aire: double): boolean;
var
  i, j, Nb: Integer;
  PtI, PtJ: TPoint2Df;
begin
  result := false;
  Aire      := 0.0;
  Perimetre := 0.00;
  Nb := GetNbVertex();
  if (Nb < 3) then Exit;
  j := Nb - 1;
  for i := 0 to Nb - 1 do
  begin
    PtI := ExtractXYFromTVertexCourbe(GetVertex(i));
    PtJ := ExtractXYFromTVertexCourbe(GetVertex(j));
    Aire        += (PtJ.X + PtI.X) * (PtJ.Y - PtI.Y);
    Perimetre   += Hypot(PtJ.X - PtI.X, PtJ.Y - PtI.Y);
    j := i;
  end;
  Aire := Abs(Aire) * 0.5;
  result := true;
end;

// charger un polygone
procedure TCourbePolygoneProvisoire.LoadPolygone(const PV: TPolygone);
var
  i, n: integer;
  V1: TVertexCourbe;
  FNbPts: Integer;
begin
  FIDGroupe := PV.IDGroupe;
  FIDStyleCourbeEditing   := nocDEFAULT;
  FIDStylePolygoneEditing := PV.IDStylePolygone;
  FMyPolygone := PV;
  n := High(PV.Sommets);
  for i:= 0 to n  do
  begin
    V1.IDStation := PV.Sommets[i].IDStation;
    V1.Offset    := PV.Sommets[i].Offset;
    AddVertex(V1);
  end;
end;

// génère une courbe par défaut d'après un zigzag
function TCourbePolygoneProvisoire.GenerateCourbe(const ByDefault: boolean;
                                                  const QIdxStart, QIdxEnd: integer;
                                                  out   CResult: TCourbe): boolean;
var
  i  : integer;
  V1   : TVertexCourbe;
  V2   : TVertexCourbe;
  //BS1, BS2     : TBaseStation;
  AB, A0, A1 : TArcCourbe;
  Q1, Q2, Delta: TPoint2Df;
  //X1, Y1, X2, Y2, dx, dY: double;
  AngleTg1, AngleTg2: double;
  R1, R2: double;
  errCode: integer;
  QNbPts, IdxStart, IdxEnd, IdxTableau: Integer;
begin
  Result := false;
  PurgerVertex(DIST_MERGE_VERTEX);
  QNbPts := GetNbVertex();
  //AfficherMessage(Format('%s.GenerateCourbe: (%d points) (%d stations)',[ClassName, QNbPts, FDocDessin.GetNbBaseStations]));
  if (QNbPts <= 1) then
  begin
    AfficherMessage('--> ** La courbe ne comporte pas de segment. Reinitialisee **');
    self.ClearVertex();
    Exit;
  end;
  SetLength(CResult.Arcs, 1);
  Delta := MakeTPoint2Df(0.0, 0.0);
  SetLength(CResult.Arcs, QNbPts-1);
  // TODO: C'est dans ce secteur qu'il faut gérer le mode ByDefault
  for i := 1 to QNbPts-1 do
  begin
    V1 := GetVertex(i-1);
    V2 := GetVertex(i);
    // arc de courbe
    AB.IDStationP1 := V1.IDStation;
    AB.OffsetP1    := V1.Offset;

    AB.IDStationP2 := V2.IDStation;
    AB.OffsetP2    := V2.Offset;

    FDocDessin.GetCoordsGCS(AB.IDStationP1, FIDGroupe, V1.Offset, Q1, errCode);
    if (ErrCode = -1) then Exit;
    FDocDessin.GetCoordsGCS(AB.IDStationP2, FIDGroupe, V2.Offset, Q2, errCode);
    if (ErrCode = -1) then Exit;
    Delta.X := Q2.X - Q1.X;
    Delta.Y := Q2.Y - Q1.Y;
    if (ByDefault) then
    begin
      AB.TangP1.X := Delta.X / 3;
      AB.TangP1.Y := Delta.Y / 3;
      R1 := Hypot(Delta.X, Delta.Y) / 3;

      AB.TangP2.X := -AB.TangP1.X;
      AB.TangP2.Y := -AB.TangP1.Y;
      CResult.Arcs[i-1] := AB;
      // angle
      AngleTg1 := AngleTg2;
    end else begin
      AB.TangP1 := V1.TangDroite;
      AB.TangP2 := V2.TangGauche;
      CResult.Arcs[i-1] := AB;
    end;
  end;
  // calcul des tangentes (construction par défaut)
  if (ByDefault) then
  begin
    for i := 1 to High(CResult.Arcs)  do begin
      A0 := CResult.Arcs[i-1];
      A1 := CResult.Arcs[i];

      R1 := Hypot(A0.TangP2.X, A0.TangP2.Y);
      AngleTg2 := GetAngleBissecteur(-A0.TangP2.X, -A0.TangP2.Y,
                                      A1.TangP1.X,  A1.TangP1.Y) - PI_2;
      CResult.Arcs[i-1].TangP2.X := R1 * cos(AngleTg2);
      CResult.Arcs[i-1].TangP2.Y := R1 * sin(AngleTg2);

      R2 := Hypot(A1.TangP1.X, A1.TangP1.Y);

      AngleTg1 := AngleTg2 + PI;
      CResult.Arcs[i].TangP1.X := R2 * cos(AngleTg1);
      CResult.Arcs[i].TangP1.Y := R2 * sin(AngleTg1);
    end;
  end;
  CResult.IDGroupe      := FIDGroupe;
  CResult.IDStyleCourbe := FIDStyleCourbeEditing;
  CResult := FDocDessin.CalcBoundingBoxCourbe(CResult);
  //******************************************************
  Result := True;
end;

function TCourbePolygoneProvisoire.GeneratePolyLine(out P: TPolyLigne): boolean;
const
  TOL_FERMETURE = 0.30;
var
  VC, PE0, PE1: TVertexCourbe;
  VP: TVertexPolygon;
  i: Integer;
  FNbPts, n: Integer;
  BP0, BP1: TBaseStation;
  PS0, PS1: TPoint2Df;
  QDelta: float = 0.00;
begin
  Result := false;
  PurgerVertex(DIST_MERGE_VERTEX);
  // détruire le dernier sommet
  FNbPts := GetNbVertex();
  if (FNbPts <= 1) then
  begin
    AfficherMessage('--> ** La polyligne ne comporte pas de segment. Reinitialisee **');
    self.ClearVertex();
    Exit(false);;
  end;
  P.IDGroupe        := FIDGroupe;
  P.IDStylePolyLine := FIDStyleCourbeEditing;
  P.Closed          := false;
  SetLength(P.Sommets, 0);
  SetLength(P.Sommets, GetNbVertex());
  n := High(P.Sommets);
  for i:= 0 to n do
  begin
    VC := GetVertex(i);
    VP.IDStation := VC.IDStation;
    VP.Offset    := VC.Offset;
    P.Sommets[i] := VP;
  end;

  // si les deux points d'extrémité de la polyligne sont proches, on suppose l'objet fermé
  // valable si l'objet comporte plus de 2 sommets
  if (FNbPts > 2) then
  begin
    PE0 := GetVertex(0);
    PE1 := GetVertex(n);
    try
      if (FDocDessin.GetBasePointByIndex(PE0.IDStation, BP0) AND
          FDocDessin.GetBasePointByIndex(PE1.IDStation, BP1)) then
      begin
        PS0 := MakeTPoint2Df(BP0.PosStation.X + PE0.Offset.X,
                             BP0.PosStation.Y + PE0.Offset.Y);
        PS1 := MakeTPoint2Df(BP1.PosStation.X + PE1.Offset.X,
                             BP1.PosStation.Y + PE1.Offset.Y);
        QDelta := hypot(PS1.X - PS0.X, PS1.Y - PS0.Y);
        P.Closed := (TOL_FERMETURE > QDelta);
      end;
    except
      P.Closed := false;
    end;
  end;
  P := FDocDessin.CalcBoundingBoxPolyligne(P);
  Result := True;
end;

function TCourbePolygoneProvisoire.GenerateScrap(out P: TScrap): boolean;
var
  i: integer;
  VC: TVertexCourbe;
  VP: TVertexPolygon;
  FNbPts: Integer;
begin
  Result := false;
  // purger vertex
  PurgerVertex(DIST_MERGE_VERTEX);
  FNbPts := GetNbVertex();
  if (FNbPts <= 1) then
  begin
    AfficherMessage('--> ** Le scrap ne comporte pas de segment. Reinitialisee **');
    self.ClearVertex();
    Exit(false);
  end;
  P := FMyScrap;
  SetLength(P.Sommets, 0);
  SetLength(P.Sommets, FNbPts);
  for i:=0 to High(P.Sommets) do
  begin
    VC := GetVertex(i);
    VP.IDStation := VC.IDStation;
    VP.Offset    := VC.Offset;
    P.Sommets[i] := VP;
  end;
  P := FDocDessin.CalcBoundingBoxScrap(P);
  Result := True;
end;


// recherche un point d'après un couple de coordonnées
function TCourbePolygoneProvisoire.GetIDVertexByXY(const X, Y: double): integer;
var
  BS: TBaseStation;
  V : TVertexCourbe;
  X1, Y1: double;
  R, R666: double;
  i: integer;
  FNbPts: Integer;
begin
  Result := -1;
  R666 := INFINITE;
  FNbPts := GetNbVertex();
  for i:= 0 to FNbPts - 1 do begin
    V := GetVertex(i);
    if (FDocDessin.GetBasePointByIndex(V.IDStation, BS)) then
    begin
      // passage en coordonnées absolues
      X1 := BS.PosStation.X + V.Offset.X;
      Y1 := BS.PosStation.Y + V.Offset.Y;
      R := Sqr(X - X1) + Sqr(Y - Y1);
      if (R < R666) then
      begin
        Result := i;
        R666   := R;
      end;
    end;
  end;
end;

// recherche un point ou ses tangentes d'après un couple de coordonnées
// résultat: ID Vertex * 10 + p
// Poignées de dimensionnement:
// avec p = 0 si point lui-même
//      p = 1 si tangente gauche
//      p = 2 si tangente droite
// Fonctionnement OK
function TCourbePolygoneProvisoire.GetIDVertexTgtsByXY(const X, Y: double): Integer;
var
  BS: TBaseStation;
  V : TVertexCourbe;

  R, R666: double;
  i: integer;
  IDX: integer;  // index du vertex
  FNbPts: Integer;
  procedure SetIdx(const XX, YY: double; const ii, Q: integer);
  begin
    R := Sqr(X - ((BS.PosStation.X + V.Offset.X) + XX)) +
         Sqr(Y - ((BS.PosStation.Y + V.Offset.Y) + YY));
    if (R < R666) then
    begin
      Result := ii * 10 + Q;
      R666   := R;
    end;
  end;
begin
  Result := -1;
  R666 := 1E24;
  FNbPts := GetNbVertex();
  if (0 = FNbPts) then exit;
  for i:= 0 to FNbPts - 1 do
  begin
    V := GetVertex(i);
    if (FDocDessin.GetBasePointByIndex(V.IDStation, BS)) then
    begin
      SetIdx(0.0, 0.0, i, 0);                          // point
      SetIdx(V.TangGauche.X, V.TangGauche.Y, i, 1);    // tangente gauche
      SetIdx(V.TangDroite.X, V.TangDroite.Y, i, 2);    // tangente droite
    end;
  end;
end;

function TCourbePolygoneProvisoire.GetNbVertex(): integer;
begin
  Result := self.Count;
end;

// inverser les vertex
procedure TCourbePolygoneProvisoire.StripVertex();
var
  i, j: integer;
  VR: array of TVertexCourbe;
  V1, V2: TVertexCourbe;
  FNbPts: Integer;
begin
  FNbPts := GetNbVertex();
  if (0 = FNbPts) then exit;
  SetLength(VR, FNbPts);
  for i:= 0 to FNbPts - 1 do
  begin
    j := FNbPts - i - 1;
    V1 := self.GetVertex(i);
    V2.IDStation := V1.IDStation;
    V2.Offset    := V1.Offset;
    // DONE: inverser les signes des tangentes ?
    V2.TangDroite.X := V1.TangGauche.X;
    V2.TangDroite.Y := V1.TangGauche.Y;
    V2.TangDroite.Z := V1.TangGauche.Z;
    V2.TangGauche.X := V1.TangDroite.X;
    V2.TangGauche.Y := V1.TangDroite.Y;
    V2.TangGauche.Z := V1.TangDroite.Z;
    VR[j] := V2;
  end;
  self.ClearVertex();
  for i:= 0 to High(VR) do self.AddVertex(VR[i]);
  SetLength(VR, 0);
end;

function TCourbePolygoneProvisoire.GenerateCurveBySuiteDePoints(const CP: TArrayPoints2Df;
                                                                const QBasePointLocked: boolean;
                                                                const QIDGroupeEditing: integer;
                                                                const QIDStyleCurveEditing: TNatureObjetCourbe;
                                                                const Nb_Pts_Skipped: integer): boolean;
var
  i, QNb: Integer;
  EWE: TPoint2Df;
  procedure MiouMiou(const Miou: TPoint2Df);
  var
    V: TVertexCourbe;
    WU, BST: TBaseStation;
  begin
    BST := FDocDessin.GetNearBasepoint(Miou.X, Miou.Y, WU, QBasePointLocked);
    V.IDStation := BST.IDStation;
    V.Offset := MakeTPoint3Df(Miou.X - BST.PosStation.X,
                              Miou.Y - BST.PosStation.y,
                                       BST.PosStation.Z);
    self.AddVertex(V);
  end;
begin
  Result := false;
  self.ClearVertex;
  QNb := 1 + High(CP);
  //AfficherMessage(self.ClassName, Format('GenerateCurveBySuiteDePoints: %d', [QNb]));
  if (QNb > 2) then
  begin
    try
      // méthode de base: on prend 1 vertex sur N + les premiers et derniers vertex;
      EWE := CP[0];     // premier point
      MiouMiou(EWE);
      for i := 0 to QNb - 2 do   // vertex intermédiaires
      begin
        EWE := CP[i];
        if (Nb_Pts_Skipped > 2) then
        begin
          if ((i mod NB_PTS_SKIPPED) = 0) then MiouMiou(EWE);
        end
        else
          MiouMiou(EWE);
      end;
      EWE := CP[QNb - 1];  // dernier point
      MiouMiou(EWE);
      // purge ?
      PurgerVertex(0.25);
      Result := True;
    except
      pass;
    end;
  end;
end;
//******************************************************************************
function TCourbePolygoneProvisoire.AddVertexNearToPoint(const X, Y: double): boolean;
var
  EWE : TVertexCourbe;
  WU: Integer;
  BP: TBaseStation;
  IdxMax: Integer;
begin
  Result := False;
  try
    WU  := self.GetIDVertexByXY(X, Y);                // attrapper l'Idx du vertex le plus proche
    if (WU < 0) then WU := 0;
    IdxMax := self.Count - 1;
    if (WU > IdxMax) then WU := IdxMax;
    EWE := self.GetVertex(WU);                            // et attrapper le vertex correspondant
    if (FDocDessin.GetBasePointByIndex(EWE.IDStation, BP)) then    // attrapper le basepoint du vertex
    begin
      BP := FDocDessin.GetNearBasepoint(X, Y, BP, false);   // rechercher le basepoint le plus proche
      EWE.IDStation := BP.IDStation;
      EWE.Offset := MakeTPoint3Df(X - BP.PosStation.X,      // calculer l'offset
                                  Y - BP.PosStation.Y,
                                  BP.PosStation.Z);
      InsertVertex(WU, EWE);                                // insérer le vertex
      Result := True;                                       // valeur de retour
    end;
  except
  end;
end;
//******************************************************************************

function TCourbePolygoneProvisoire.FusionnerTArrayPoints2Df(const S1: TArrayVertexCourbe): boolean;
var
  i, n, h1, h2: Integer;
  Vo, V1, V2: TVertexCourbe;
  Po, P1, P2: TBaseStation;
  D1, D2: double;
  NearExtremiteIsPT1, Q2, Q1, Q3: Boolean;
begin
  Result := false;
  try
    n := High(S1);
    if (n < 0) then Exit;
    // on recherche l'extrémité la plus proche du polygone courant
    Vo := GetVertex(self.Count - 1);                      // vertex de fin de la courbe courante
    V1 := S1[0];                                          // extrémité 1 de la courbe à ajouter
    V2 := S1[n];                                          // extrémité 2 de cette courbe
    Q1 := FDocDessin.GetBasePointByIndex(Vo.IDStation, Po);     // basepoint fin courbe courante
    Q2 := FDocDessin.GetBasePointByIndex(V1.IDStation, P1);     // basepoint extrémité 1 de la courbe à ajouter
    Q3 := FDocDessin.GetBasePointByIndex(V2.IDStation, P2);     // basepoint extrémité 2 de cette courbe
    if (Q1 AND Q2 AND Q3) then
    begin
      D1 := Sqr(P1.PosStation.X - Po.PosStation.X) +
            Sqr(P1.PosStation.Y - Po.PosStation.Y);         // calcul des distances
      D2 := Sqr(P2.PosStation.X - Po.PosStation.X) +
            Sqr(P2.PosStation.Y - Po.PosStation.Y);
      NearExtremiteIsPT1 := (D1 < D2);                      // l'extrémite 1 est la plus proche ?
      // si P1 est plus proche, on ajoute à la suite sinon on ajoute en strippant S1
      if (NearExtremiteIsPT1) then begin
        for i := 0 to n do self.AddVertex(S1[i]);
      end else begin
        for i := n downto 0 do self.AddVertex(S1[i]);
      end;
      Result := True;
    end;
  except
  end;
end;
end.

