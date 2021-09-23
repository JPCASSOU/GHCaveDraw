unit UnitFindObjects;
// Recherche d'objets dans un document de dessin
// 27/07/2015: Routines de recherche entièrement réécrites
// 13/10/2015: Bug dans la sélection des courbes fixé
// 30/08/2020: Pointage temporel
{$mode delphi}

interface
uses
  GHCD_Types,
  GeneralFunctions,
  UnitDocDessin,
  math,
  GeometryPolygonesUtils,
  Classes, SysUtils;
type

{ TFindingObjects }

 TFindingObjects = class(TList)
  strict private
    procedure ScanVertexArray(const QIdxShape: int64; const QGroupe: TGroupeEntites; const VARR: TArrayVertexPolygon; const X, Y: double; var DistCourante: double);
    function  GroupeIsEditable(const GP: TGroupeEntites): boolean;
    //function  GetSecuredGroupeByIDGroupe(const IdxGp: TIDGroupeEntites);
  private
    FDocDessin             : TDocumentDessin;
    FOperationsCSGClipping : TOperationsCSGClipping;
    FNbElementsFound       : integer;
    FIDNearest             : int64;
    procedure AddElementFound(const IDElement: Int64);
    procedure FindNearestIdxCourbeToXY(const QX, QY: double);
    procedure FindNearestIdxLigneToXY(const X, Y: double);
    procedure FindNearestIdxPolygoneToXY(const X, Y: double);
    procedure FindNearestIdxPolyligneToXY(const X, Y: double);
    procedure FindNearestIdxScrapToXY(const X, Y: double);
    procedure FindNearestIdxSymboleToXY(const X, Y: double);
    procedure FindNearestIdxTexteToXY(const X, Y: double);
  public
    function  Initialise(const FD: TDocumentDessin): boolean;
    procedure Resetter();
    function  GetNbElementsFound(): integer;
    function  FindObject(const X, Y: double; const ModeSelection: TModeSelectionEntites): Int64;
    function  GetIDNearest(): Int64;
    function  GetIdxElementFound(const Idx: Integer): Int64;
    procedure Finalise();
    function  FindObjectsInWindow(const ModeSelection: TModeSelectionEntites;
                                  const PS: TArrayVertexPolygon): integer;
end;
implementation
uses
  DGCDummyUnit;
const
  RAYON_CAPTURE = 10.00;
  SQ_RAYON_CAPTURE_MAXIMAL = RAYON_CAPTURE * RAYON_CAPTURE;

// pour les scraps, polylignes et polygones
procedure TFindingObjects.ScanVertexArray(const QIdxShape: int64;
                                          const QGroupe: TGroupeEntites;
                                          const VARR: TArrayVertexPolygon;
                                          const X, Y: double;
                                          var   DistCourante: double);
var
  V: TVertexPolygon;
  a: Integer;
  P0: TPoint2Df;
  errCode: integer;
  q: Extended;
begin
  for a := 0 to High(VARR) do
  begin
    V := VARR[a];
    FDocDessin.GetCoordsGCS(V.IDStation, QGroupe.IDGroupeEntites, V.Offset, P0, errCode);
    if (ErrCode = -1) then Continue;
    q := Sqr(P0.X - X) + sqr(P0.Y - Y );
    if (q < DistCourante) then
    begin
      DistCourante := q;
      FIDNearest := QIdxShape;
    end;
  end;
end;

function TFindingObjects.GroupeIsEditable(const GP: TGroupeEntites): boolean;
begin
  if (0 = GP.IDGroupeEntites) then exit(true); // Le groupe 0 est toujours éditable.
  //AfficherMessageErreur(Format('Groupe: %d - %s ; Visible: %s', [GP.IDGroupeEntites, GP.NomGroupe, booltostr(GP.Visible)]));
  Result := GP.Visible; // OR (not(GP.Locked)));
end;



function TFindingObjects.Initialise(const FD: TDocumentDessin): boolean;
begin
  Result := false;
  try
    FDocDessin := FD;
    FOperationsCSGClipping := TOperationsCSGClipping.Create;
    self.Resetter;
    Result := True;
  except
  end;
end;

procedure TFindingObjects.Resetter();
var
  i: integer;
begin
  if (Count = 0) then Exit; // AfficherMessage('--> Aucune entite')
  for i:=0 to self.Count - 1 do Dispose(self.Items[i]); // désallocation des entités
  self.Clear;
  FNbElementsFound := 0;
  FIDNearest       := -1;
end;

function TFindingObjects.GetNbElementsFound(): integer;
begin
  Result := self.Count;
end;

procedure TFindingObjects.Finalise;
begin
  self.Resetter();
  try
    FOperationsCSGClipping.Finalise();
  finally
    FreeAndNil(FOperationsCSGClipping);//     FOperationsCSGClipping.Free;
  end;
end;


procedure TFindingObjects.AddElementFound(const IDElement: Int64);
var
  pN: ^Int64;
begin
  try
    New(pN);
    pN^ := IDElement;
    Self.Add(pN);
  finally
  end;
end;
function TFindingObjects.GetIdxElementFound(const Idx: Integer): Int64;
var
  pN: ^Int64;
begin
  pN := self.Items[Idx];
  Result := pN^;
end;
//------------------------------------------------------------------------------
procedure  TFindingObjects.FindNearestIdxCourbeToXY(const QX, QY: double);
var
  i, j: Integer;
  C: TCourbe;
  NI: Int64;
  NbObj: Integer;
  MyDistCourante: double;
  GP: TGroupeEntites;
  QNbC: Integer;
  // bug fixé.
  procedure ScanCourbe(const QIdxCourbe: int64;
                       const QC: TCourbe;
                       const QGroupe: TGroupeEntites;
                       var QDistanceCourante: double);
  var
    P: TBaseStation;
    a, s: integer;
    q2: double; // carré de la distance
    Bezier: TArrayPoints2Df;
    P1, P2, PC1, PC2: TPoint2Df;
    errCode: integer;

    Arc: TArcCourbe;
    procedure Test(const PP: TPoint2Df);
    var
      QDist: float;
    begin
      QDist:= hypot(QX - PP.X, QY - PP.Y);
      if (QDist < QDistanceCourante) then
      begin
        QDistanceCourante := QDist;
        FIDNearest := QIdxCourbe;
      end;
    end;
  begin
    for a := 0 to QC.getNbArcs() - 1 do //High(QC.Arcs) do
    begin
      Arc := QC.getArc(a); //Arcs[a];
      //PC1 := MakeTPoint2Df(B.PT1.X + B.Tgt1.X, B.PT1.Y + B.Tgt1.Y);
      //PC2 := MakeTPoint2Df(B.PT2.X + B.Tgt2.X, B.PT2.Y + B.Tgt2.Y);
      FDocDessin.GetCoordsGCS(Arc.IDStationP1, QGroupe.IDGroupeEntites, ARC.OffsetP1, P1, errCode);
      if (ErrCode = -1) then Continue;
      // les tangentes sont exprimées par rapport au point de base associé
      PC1.setFrom(P1.X + ARC.TangP1.X, P1.Y + ARC.TangP1.Y);
      FDocDessin.GetCoordsGCS(Arc.IDStationP2, QGroupe.IDGroupeEntites, ARC.OffsetP2, P2, errCode);
      PC2.setFrom(P2.X + ARC.TangP2.X, P2.Y + ARC.TangP2.Y);
      if (ErrCode = -1) then Continue;
      Test(P1); Test(PC1); Test(PC2); Test(P2);
      Bezier := CalcBezierCurve(P1, PC1, PC2, P2, 100);
      for s := 0 to High(Bezier) do Test(Bezier[s]);
    end;
  end;
begin
  FIDNearest := 0;
  MyDistCourante := INFINITE;
  AfficherMessageErreur('Balayage');
  self.Resetter;
  NbObj := FDocDessin.GetNbCourbes();
  if (0 = NbObj) then Exit;
  for i := 0 to NbObj - 1 do
  begin
    C := FDocDessin.GetCourbe(i);
    GP := FDocDessin.GetGroupeByIDGroupe(C.IDGroupe);
    if (GroupeIsEditable(GP)) then
    begin
      if (IsInBoundingBox(QX, QY, C.BoundingBox)) then AddElementFound(i);    // on ajoute
    end;
  end;
  if (self.Count = 0) then Exit;

  QNbC := self.Count;
  for i := 0 to QNbC - 1  do
  begin
    NI := self.GetIdxElementFound(i);
    //AfficherMessageErreur(format('Scan courbe: %d', [NI]));
    C  := FDocDessin.GetCourbe(NI);
    GP := FDocDessin.GetGroupeByIDGroupe(C.IDGroupe);
    ScanCourbe(NI, C, GP, MyDistCourante);
  end;
end;

procedure TFindingObjects.FindNearestIdxLigneToXY(const X, Y: double);
var
  i, NbObj: Integer;
  SC: TSimpleLigne;
  GP: TGroupeEntites;
  DistCourante : double;
  NI: Int64;
  P1, P2: TPoint2Df;
  errCode: integer;
  qd: Extended;
begin
  self.Resetter;
  NbObj := FDocDessin.GetNbSimpleLignes();
  if (0 = NbObj) then Exit;
  for i := 0 to NbObj - 1 do
  begin
    SC := FDocDessin.GetSimpleLigne(i);
    GP := FDocDessin.GetGroupeByIDGroupe(SC.IDGroupe);
    if (GroupeIsEditable(GP)) then
    begin
      if (IsInBoundingBox(X, Y, SC.BoundingBox)) then AddElementFound(i);    // on ajoute
    end;
  end;
  if (self.Count = 0) then Exit;
  DistCourante := INFINITE;
  for i := self.Count - 1 downto 0 do
  begin
    NI := self.GetIdxElementFound(i);
    SC  := FDocDessin.GetSimpleLigne(NI);
    GP := FDocDessin.GetGroupeByIDGroupe(SC.IDGroupe);
    FDocDessin.GetCoordsGCS(SC.IDBaseStExt1, GP.IDGroupeEntites, SC.OffsetExtr2, P1, errCode);
    qd := sqr(X - P1.X) + sqr(Y - P1.Y);
    FDocDessin.GetCoordsGCS(SC.IDBaseStExt2, GP.IDGroupeEntites, SC.OffsetExtr2, P2, errCode);
    if (qd < DistCourante) then
    begin
      DistCourante := qd;
      FIDNearest := NI;
    end;
    qd := sqr(X - P2.X) + sqr(Y - P2.Y);
    if (qd < DistCourante) then
    begin
      DistCourante := qd;
      FIDNearest := NI;
    end;
  end;
end;

procedure TFindingObjects.FindNearestIdxScrapToXY(const X, Y: double);
var
  i, NbObj: Integer;
  SC: TScrap;
  GP: TGroupeEntites;
  DistCourante : double;
  NI: Int64;
begin
  AfficherMessageErreur('FindNearestIdxScrapToXY()');
  DistCourante := INFINITE;
  self.Resetter;
  NbObj := FDocDessin.GetNbScraps();
  if (0 = NbObj) then Exit;
  for i:=0 to NbObj - 1 do
  begin
    SC := FDocDessin.GetScrap(i);
    GP := FDocDessin.GetGroupeByIDGroupe(SC.IDGroupe);
    if (GroupeIsEditable(GP)) then
    begin
      if (IsInBoundingBox(X, Y, SC.BoundingBox)) then AddElementFound(i);    // on ajoute
    end;
  end;
  if (self.Count = 0) then Exit;

  for i := self.Count - 1 downto 0 do
  begin
    NI := self.GetIdxElementFound(i);
    SC  := FDocDessin.GetScrap(NI);
    GP := FDocDessin.GetGroupeByIDGroupe(SC.IDGroupe);
    ScanVertexArray(NI, GP, SC.Sommets, X, Y, DistCourante);
  end;
end;
procedure TFindingObjects.FindNearestIdxPolyligneToXY(const X, Y: double);
var
  i, NbObj: Integer;
  GP: TGroupeEntites;
  DistCourante : double;
  NI: Int64;
  PC: TPolyLigne;
begin
  AfficherMessageErreur('FindNearestIdxPolyligneToXY()');
  DistCourante := INFINITE;
  self.Resetter;
  NbObj := FDocDessin.GetNbPolylignes();
  if (0 = NbObj) then Exit;
  for i := 0 to NbObj - 1 do
  begin
    PC := FDocDessin.GetPolyligne(i);
    GP := FDocDessin.GetGroupeByIDGroupe(PC.IDGroupe);
    if (GroupeIsEditable(GP)) then
    begin
      if (IsInBoundingBox(X, Y, PC.BoundingBox)) then AddElementFound(i);    // on ajoute
    end;
  end;
  if (self.Count = 0) then Exit;
  for i := self.Count - 1 downto 0 do
  begin
    NI := self.GetIdxElementFound(i);
    PC  := FDocDessin.GetPolyligne(NI);
    GP := FDocDessin.GetGroupeByIDGroupe(PC.IDGroupe);
    ScanVertexArray(NI, GP, PC.Sommets, X, Y, DistCourante);
  end;
end;
procedure TFindingObjects.FindNearestIdxPolygoneToXY(const X, Y: double);
var
  i, NbObj: Integer;
  GP: TGroupeEntites;
  DistCourante : double;
  NI: Int64;
  PC: TPolygone;
begin
  AfficherMessageErreur('FindNearestIdxPolygoneToXY()');
  self.Resetter();
  NbObj := FDocDessin.GetNbPolygones();
  if (0 = NbObj) then Exit;
  for i:=0 to NbObj - 1 do
  begin
    PC := FDocDessin.GetPolygone(i);
    GP := FDocDessin.GetGroupeByIDGroupe(PC.IDGroupe);

    if (GroupeIsEditable(GP)) then
    begin
      if (IsInBoundingBox(X, Y, PC.BoundingBox)) then AddElementFound(i);    // on ajoute
    end;
  end;
  AfficherMessageErreur(Format('List of founds: %d', [self.Count]));
  if (self.Count = 0) then Exit;
  DistCourante := INFINITE;
  for i := self.Count - 1 downto 0 do
  begin
    NI := self.GetIdxElementFound(i);
    PC  := FDocDessin.GetPolygone(NI);
    GP := FDocDessin.GetGroupeByIDGroupe(PC.IDGroupe);
    ScanVertexArray(NI, GP, PC.Sommets, X, Y, DistCourante);
  end;
end;

procedure TFindingObjects.FindNearestIdxSymboleToXY(const X, Y: double);
var
  i, NbObj: Integer;
  SC: TSymbole;
  GP: TGroupeEntites;
  DistCourante : double;
  NI: Int64;
  P1: TPoint2Df;
  errCode: integer;
  qd: Extended;
begin
  self.Resetter;
  NbObj := FDocDessin.GetNbSymboles();
  if (0 = NbObj) then Exit;
  DistCourante := INFINITE;
  for i:=0 to NbObj - 1 do
  begin
    SC := FDocDessin.GetSymbole(i);
    GP := FDocDessin.GetGroupeByIDGroupe(SC.IDGroupe);
    if (GroupeIsEditable(GP)) then
    begin
      FDocDessin.GetCoordsGCS(SC.IDBaseStation, GP.IDGroupeEntites, SC.Offset, P1, errCode);
      qd := sqr(X - P1.X) + sqr(Y - P1.Y);
      if ((qd < SQ_RAYON_CAPTURE_MAXIMAL) and (qd < DistCourante)) then
      begin
        DistCourante := qd;
        FIDNearest := i;
      end;
    end;
  end;
end;
procedure TFindingObjects.FindNearestIdxTexteToXY(const X, Y: double);
var
  i, NbObj: Integer;
  SC: TTextObject;
  GP: TGroupeEntites;
  DistCourante : double;
  NI: Int64;
  P1: TPoint2Df;
  errCode: integer;
  qd: Extended;
begin
  self.Resetter;
  NbObj := FDocDessin.GetNbTextes();
  if (NbObj = 0) then Exit;
  DistCourante := INFINITE;
  for i:=0 to NbObj - 1 do
  begin
    SC := FDocDessin.GetTexte(i);
    GP := FDocDessin.GetGroupeByIDGroupe(SC.IDGroupe);
    if (GroupeIsEditable(GP)) then
    begin
      FDocDessin.GetCoordsGCS(SC.IDBaseStation, GP.IDGroupeEntites, SC.Offset, P1, errCode);
      qd := sqr(X - P1.X) + sqr(Y - P1.Y);
      if ((qd < SQ_RAYON_CAPTURE_MAXIMAL) and (qd < DistCourante)) then
      begin
        DistCourante := qd;
        FIDNearest := i;
      end;
    end;
  end;
end;


// recherche d'éléments
function  TFindingObjects.FindObject(const X,Y: double; const ModeSelection: TModeSelectionEntites): Int64;
begin
  FIDNearest        := -1;
  Result            := -1; // valeur conventionnelle pour élément non trouvé
  case ModeSelection of
    mseNONE: Exit;
    mseSCRAPS     : FindNearestIdxScrapToXY(X, Y);
    mseCOURBES    : FindNearestIdxCourbeToXY(X, Y);
    msePOLYLIGNES : FindNearestIdxPolyligneToXY(X, Y);
    msePOLYGONES  : FindNearestIdxPolygoneToXY(X, Y);
    mseLIGNES     : FindNearestIdxLigneToXY(X, Y);
    mseSYMBOLES   : FindNearestIdxSymboleToXY(X, Y);
    mseTEXTES     : FindNearestIdxTexteToXY(X, Y);
    mseIMAGES     : pass;
  else
    begin
      Result := -1;
      AfficherMessage('--> No object found');
    end;
  end;
end;

function TFindingObjects.GetIDNearest(): Int64;
begin
  Result := FIDNearest;
end;
// retourne la liste des éléments capturés par un polygone de sélection

function TFindingObjects.FindObjectsInWindow(const ModeSelection: TModeSelectionEntites;
                                             const PS: TArrayVertexPolygon): integer;

  function NbScrapsFound(): integer;
  var
    i, Nb: Integer;
    MyScrap: TScrap;
  begin
    Result := 0;
    Nb := FDocDessin.GetNbScraps();
    if (Nb = 0) then Exit;
    for i := 0 to Nb - 1 do
    begin
      MyScrap := FDocDessin.GetScrap(i);
      FOperationsCSGClipping.Initialise(FDocDessin, MyScrap.Sommets, PS);
    end;
  end;
begin
  Result := 0;
  case ModeSelection of
    mseSCRAPS     : Result := NbScrapsFound;
    mseCOURBES    : ;
    msePOLYLIGNES : ;
    msePOLYGONES  : ;
    mseLIGNES     : ;
    mseSYMBOLES   : ;
    mseTEXTES     : ;
  end;
end;

end.

