unit CadreHierarchieObjets;
// See https://wiki.freepascal.org/VirtualTreeview_Example_for_Lazarus
// https://documentation.help/VirtualTreeview/Features_htm.html
// Getting started: My first
// https://documentation.help/VirtualTreeview/GettingStartedE_html.html

{$mode delphi}

interface

uses
  GHCD_Types,
  GeneralFunctions,
  UnitDocDessin,
  Classes, SysUtils, Forms, Dialogs, Controls, StdCtrls, VirtualTrees;
type TTypeNode = (tnUNKNOWN,
                  tnROOT, tnGROUPE,
                  tnSECTION_SCRAPS, tnSECTION_COURBES, tnSECTION_POLYLINES, tnSECTION_POLYGONES, tnSECTION_SIMPLE_LINES, tnSECTION_SYMBOLS, tnSECTION_TEXTES,
                  tnOBJECT_SCRAP, tnOBJECT_COURBE, tnOBJECT_POLYLINE, tnOBJECT_POLYGONE, tnOBJECT_SIMPLE_LINE, tnOBJECT_SYMBOL, tnOBJECT_TEXTE
                  );
type    { TTreeData }
  PTreeData = ^TTreeData;
  TTreeData = record
    Level      : cardinal;
    TypeNode   : TTypeNode;
    InternalIndex: integer; // index interne de l'objet
    Name   : string;
    Column1: String;
    Column2: String;
    Column3: String;
    procedure Empty();
    procedure setFrom(const QLevel: integer; const QTypeNode: TTypeNode; const QInternalIdx: integer; const QName: string; const C1, C2, C3: string);
    function DebugString(): string;
    function DescTTypeNode(): string;
  end;

type

  { TCdrHierarchieObjets }

  TCdrHierarchieObjets = class(TFrame)
    Button1: TButton;
    ImageList1: TImageList;
    lbNodeInfo: TStaticText;
    VirtualStringTree1: TVirtualStringTree;
    procedure Button1Click(Sender: TObject);
    procedure VirtualStringTree1Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VirtualStringTree1FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VirtualStringTree1NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  private
    FDocumentDessin: TDocumentDessin;
    procedure RemplirTreeView();
  public
    function Initialiser(const FD: TDocumentDessin): boolean;

  end;

implementation
uses DGCDummyUnit;

{$R *.lfm}

{ TTreeData }

procedure TTreeData.Empty();
begin
  self.setFrom(0, tnUNKNOWN, -1, '', '', '', '');
end;

procedure TTreeData.setFrom(const QLevel: integer; const QTypeNode: TTypeNode; const QInternalIdx: integer; const QName: string; const C1, C2, C3: string);
begin
  self.Level    := QLevel;
  self.TypeNode := QTypeNode;
  self.InternalIndex := QInternalIdx;
  self.Name     := QName;
  self.Column1  := C1;
  self.Column2  := C2;
  self.Column3  := C3;

end;
Vols et/ou perte récurrents et quasi-systématique de courriers importants:

- 360 euros en chèques vacances, jamais reçus (entre novembre 2020 et maintenant)
- Un avis d'infraction port du masque, constaté par la police, jamais reçu (émis par le Trésor Public Amendes en septembre 2020. Contravention pré-payée et dossier clôturé)
- Une carte d'assurance spéléo, jamais reçue (janvier 2021)
- Un courrier-leurre envoyé à moi-même et contenant un chèque de 1 euro: jamais reçu. Un deuxième courrier-leurre déposé en même temps et contenant un formulaire de mathématiques est bien arrivé
- Un paiement par chèque envoyé le 15/09/2021 par un cabinet notarial de Cadillac: toujours pas reçu.
function TTreeData.DebugString(): string;
begin
  Result := format('Level: %d - V1 = %s,  V2 = %s,  V3 = %s ',
                  [self.Level,
                   self.Column1,
                   self.Column2,
                   self.Column3
                  ]);
end;

function TTreeData.DescTTypeNode(): string;
begin
  Result := '--';
  case self.TypeNode of
    tnUNKNOWN               : Result := '--';
    tnROOT                  : Result := 'Noeud racine';
    tnGROUPE                : Result := 'Groupe';
    tnSECTION_SCRAPS        : Result := 'Section SCRAPS';
    tnSECTION_COURBES       : Result := 'Section COURBES';
    tnSECTION_POLYLINES     : Result := 'Section POLYLIGNES';
    tnSECTION_POLYGONES     : Result := 'Section POLYGONES';
    tnSECTION_SIMPLE_LINES  : Result := 'Section LIGNES';
    tnSECTION_SYMBOLS       : Result := 'Section SYMBOLES';
    tnSECTION_TEXTES        : Result := 'Section TEXTES';
    tnOBJECT_SCRAP          : Result := 'Scrap';
    tnOBJECT_COURBE         : Result := 'Courbe';
    tnOBJECT_POLYLINE       : Result := 'Polyligne';
    tnOBJECT_POLYGONE       : Result := 'Polygone';
    tnOBJECT_SIMPLE_LINE    : Result := 'Ligne simple';
    tnOBJECT_SYMBOL         : Result := 'Symbole';
    tnOBJECT_TEXTE          : Result := 'Texte';
  end;
end;

{ TCdrHierarchieObjets }

procedure TCdrHierarchieObjets.RemplirTreeView();
var
  RootNode         : PVirtualNode;
  pNodeGroupe      : PVirtualNode;
  pScrapsOfGroupe  : PVirtualNode;
  i, NbGroupes: Integer;
  MyGroupe: TGroupeEntites;
  pGroupeSectionScraps       : PVirtualNode;
  pGroupeSectionCourbes      : PVirtualNode;
  pGroupeSectionPolylines    : PVirtualNode;
  pGroupeSectionPolygones    : PVirtualNode;
  pGroupeSectionSimpleLines  : PVirtualNode;
  pGroupeSectionSymboles     : PVirtualNode;
  pGroupeSectionTextes       : PVirtualNode;


  Data: PTreeData;


  function QCreateASectionOfGroupe(var pMyNodeGroupe, pMyGroupeSection: PVirtualNode; const QTypeSection: TTypeNode; const QInternalIdx: integer; const QSectionName: string): boolean;
  var
    L: Cardinal;
  begin
    result := false;

    pMyGroupeSection := VirtualStringTree1.AddChild(pMyNodeGroupe);
    L := VirtualStringTree1.GetNodeLevel(pMyGroupeSection);
    Data := VirtualStringTree1.GetNodeData(pMyGroupeSection);
    Data^.setFrom(L,
                  QTypeSection,
                  QInternalIdx,
                  QSectionName,
                  Format('%s', [QSectionName]),
                  '',
                  '');
  end;
  procedure QCreateNodesScrapOfGroupe(var pSection: PVirtualNode; const AGroupe: TGroupeEntites);
  var
    s, Nb, NbVertex: Integer;
    MyScrap: TScrap;
    pMyScrap: PVirtualNode;
    L: Cardinal;

  begin
    Nb := FDocumentDessin.GetNbScraps();
    if (0 = Nb) then exit;
    for s := 0 to Nb - 1 do
    begin
      MyScrap  := FDocumentDessin.GetScrap(s);
      NbVertex := MyScrap.getNbVertex();
      if (AGroupe.IDGroupeEntites = MyScrap.IDGroupe) then
      begin
        pMyScrap := VirtualStringTree1.AddChild(pSection);
        L := VirtualStringTree1.GetNodeLevel(pMyScrap);
        Data := VirtualStringTree1.GetNodeData(pMyScrap);
        Data^.setFrom(L,
                      tnOBJECT_SCRAP,
                      s,
                      MyScrap.Nom,
                      Format('%d', [NbVertex]),
                      '',
                      '');
      end;
    end;
  end;
  procedure QCreateNodesCourbesOfGroupe(var pSection: PVirtualNode; const AGroupe: TGroupeEntites);
  var
    s, Nb, NbArcs: Integer;
    pMyCourbe: PVirtualNode;
    L: Cardinal;
    MyCourbe: TCourbe;
  begin
    Nb := FDocumentDessin.GetNbCourbes();
    if (0 = Nb) then exit;
    for s := 0 to Nb - 1 do
    begin
      MyCourbe  := FDocumentDessin.GetCourbe(s);
      NbArcs := MyCourbe.getNbArcs();
      if (AGroupe.IDGroupeEntites = MyCourbe.IDGroupe) then
      begin
        pMyCourbe := VirtualStringTree1.AddChild(pSection);
        L := VirtualStringTree1.GetNodeLevel(pMyCourbe);
        Data := VirtualStringTree1.GetNodeData(pMyCourbe);
        Data^.setFrom(L,
                      tnOBJECT_COURBE,
                      s,
                      Format('Courbe%d', [s]),
                      Format('%d', [NbArcs]),
                      '',
                      '');
      end;
    end;
  end;

  procedure QCreateNodesPolylinesOfGroupe(var pSection: PVirtualNode; const AGroupe: TGroupeEntites);
  var
    s, Nb, NbVertex: Integer;
    MyPolyLine : TPolyLigne;
    pMyPolyline: PVirtualNode;
    L: Cardinal;
  begin
    Nb := FDocumentDessin.GetNbPolylignes();
    if (0 = Nb) then exit;
    for s := 0 to Nb - 1 do
    begin
      MyPolyLine  := FDocumentDessin.GetPolyligne(s);
      NbVertex := MyPolyLine.getNbVertex();
      if (AGroupe.IDGroupeEntites = MyPolyLine.IDGroupe) then
      begin
        pMyPolyline := VirtualStringTree1.AddChild(pSection);
        L := VirtualStringTree1.GetNodeLevel(pMyPolyline);
        Data := VirtualStringTree1.GetNodeData(pMyPolyline);
        Data^.setFrom(L,
                      tnOBJECT_POLYLINE,
                      s,
                      Format('Polyline%d', [s]),
                      Format('%d', [NbVertex]),
                      '',
                      '');
      end;
    end;

  end;
  procedure QCreateNodesPolygonesOfGroupe(var pSection: PVirtualNode; const AGroupe: TGroupeEntites);
  var
    s, Nb, NbVertex: Integer;
    MyPolygone : TPolygone;
    pMyPolygone: PVirtualNode;
    L: Cardinal;
  begin
    Nb := FDocumentDessin.GetNbPolygones();
    if (0 = Nb) then exit;
    for s := 0 to Nb - 1 do
    begin
      MyPolygone  := FDocumentDessin.GetPolygone(s);
      NbVertex := MyPolygone.getNbVertex();
      if (AGroupe.IDGroupeEntites = MyPolygone.IDGroupe) then
      begin
        pMyPolygone := VirtualStringTree1.AddChild(pSection);
        L := VirtualStringTree1.GetNodeLevel(pMyPolygone);
        Data := VirtualStringTree1.GetNodeData(pMyPolygone);
        Data^.setFrom(L,
                      tnOBJECT_POLYGONE,
                      s,
                      Format('Polygone%d', [s]),
                      Format('%d', [NbVertex]),
                      '',
                      '');
      end;
    end;

  end;

  procedure QCreateNodesSymbolesOfGroupe(var pSection: PVirtualNode; const AGroupe: TGroupeEntites);
  var
    s, Nb: Integer;
    MySymbole : TSymbole;
    pMySymbole: PVirtualNode;
    L: Cardinal;
    BP: TBaseStation;
  begin
    Nb := FDocumentDessin.GetNbSymboles();
    if (0 = Nb) then exit;
    for s := 0 to Nb - 1 do
    begin
      MySymbole  := FDocumentDessin.GetSymbole(s);
      FDocumentDessin.GetBasePointByIndex(MySymbole.IDBaseStation, BP);
      if (AGroupe.IDGroupeEntites = MySymbole.IDGroupe) then
      begin
        pMySymbole := VirtualStringTree1.AddChild(pSection);
        L := VirtualStringTree1.GetNodeLevel(pMySymbole);
        Data := VirtualStringTree1.GetNodeData(pMySymbole);
        Data^.setFrom(L,
                      tnOBJECT_SYMBOL,
                      s,
                      Format('Symbole%d near to %s', [s, BP.getToporobotIDStationAsString()]),
                      Format('%d', [Ord(MySymbole.TypeObject)]),
                      '',
                      '');
      end;
    end;
  end;

  procedure QCreateNodesTextesOfGroupe(var pSection: PVirtualNode; const AGroupe: TGroupeEntites);
  var
    s, Nb: Integer;
    MyTexte : TTextObject;
    pMyTexte: PVirtualNode;
    L: Cardinal;
    BP: TBaseStation;
  begin
    Nb := FDocumentDessin.GetNbTextes();
    if (0 = Nb) then exit;
    for s := 0 to Nb - 1 do
    begin
      MyTexte  := FDocumentDessin.GetTexte(s);
      FDocumentDessin.GetBasePointByIndex(MyTexte.IDBaseStation, BP);
      if (AGroupe.IDGroupeEntites = MyTexte.IDGroupe) then
      begin
        pMyTexte := VirtualStringTree1.AddChild(pSection);
        L := VirtualStringTree1.GetNodeLevel(pMyTexte);
        Data := VirtualStringTree1.GetNodeData(pMyTexte);
        Data^.setFrom(L,
                      tnOBJECT_TEXTE,
                      s,
                      Format('Texte%d near to %s', [s, BP.getToporobotIDStationAsString()]),
                      MyTexte.Text,
                      '',
                      '');
      end;
    end;
  end;
  procedure QAddAndDefineColumn(const QWidth: integer; const QCaption: string);
  var
    MyColumn: TVirtualTreeColumn;
  begin
    MyColumn := VirtualStringTree1.Header.Columns.Add;
    //MyColumn.Options:=;
    MyColumn.Width       := QWidth;
    MyColumn.Text        := QCaption;

  end;
begin

  VirtualStringTree1.Clear;
  VirtualStringTree1.Header.Columns.Clear();
  QAddAndDefineColumn(320, 'Name');
  QAddAndDefineColumn(400, 'Type de noeud');
  QAddAndDefineColumn(100, 'Idx');

  //QAddAndDefineColumn(100, 'Column2');
  //QAddAndDefineColumn(400, 'Column3');
  //QAddAndDefineColumn(100, 'Column4');







  AfficherMessageErreur('0002');
  RootNode := VirtualStringTree1.AddChild(nil);
  if (VirtualStringTree1.AbsoluteIndex(RootNode) > -1) then
  begin
    Data := VirtualStringTree1.GetNodeData(RootNode);
    AfficherMessageErreur('0003');
    Data^.setFrom(VirtualStringTree1.GetNodeLevel(RootNode), tnROOT, -1, 'Cavité', '', '', '');
    AfficherMessageErreur('0004');
    // groupes
    NbGroupes := FDocumentDessin.GetNbGroupes();
    AfficherMessageErreur('0005');
    for i := 0 to NbGroupes - 1 do
    begin
      MyGroupe := FDocumentDessin.GetGroupe(i);
      pNodeGroupe := VirtualStringTree1.AddChild(RootNode);
      Data := VirtualStringTree1.GetNodeData(pNodeGroupe);
      Data.setFrom(VirtualStringTree1.GetNodeLevel(pNodeGroupe),
                   tnGROUPE,
                   i,
                   MyGroupe.NomGroupe,
                   Format('%.2f', [MyGroupe.ZOrder]),
                   '',
                   '');
      //VirtualStringTree1.Expanded[pNodeGroupe] := True;
      // ajout des sections (scraps)
      AfficherMessageErreur('0007+'+inttostr(i));

      QCreateASectionOfGroupe(pNodeGroupe, pGroupeSectionScraps      , tnSECTION_SCRAPS      , -1, 'Scraps');
        QCreateNodesScrapOfGroupe(pGroupeSectionScraps, MyGroupe);

      QCreateASectionOfGroupe(pNodeGroupe, pGroupeSectionCourbes     , tnSECTION_COURBES     , -1, 'Courbes');
        QCreateNodesCourbesOfGroupe(pGroupeSectionCourbes, MyGroupe);

      QCreateASectionOfGroupe(pNodeGroupe, pGroupeSectionPolylines   , tnSECTION_POLYLINES   , -1, 'Polylignes');
        QCreateNodesPolylinesOfGroupe(pGroupeSectionPolylines, MyGroupe);

      QCreateASectionOfGroupe(pNodeGroupe, pGroupeSectionPolygones   , tnSECTION_POLYGONES   , -1, 'Polygones');
        QCreateNodesPolygonesOfGroupe(pGroupeSectionPolygones, MyGroupe);
      QCreateASectionOfGroupe(pNodeGroupe, pGroupeSectionSimpleLines , tnSECTION_SIMPLE_LINES, -1, 'Lignes simples');

      QCreateASectionOfGroupe(pNodeGroupe, pGroupeSectionSymboles    , tnSECTION_SYMBOLS     , -1, 'Symboles');
        QCreateNodesSymbolesOfGroupe(pGroupeSectionSymboles, MyGroupe);

      QCreateASectionOfGroupe(pNodeGroupe, pGroupeSectionTextes      , tnSECTION_TEXTES      , -1, 'Textes');
        QCreateNodesTextesOfGroupe(pGroupeSectionTextes, MyGroupe);


    end;
    AfficherMessageErreur('0008');
  end;
  (*

   if VST.AbsoluteIndex(XNode) > -1 then
  begin
    Data := VST.GetNodeData(Xnode);
    Data^.Column0 := 'One ' + IntToStr(Rand);
    Data^.Column1 := 'Two ' + IntToStr(Rand + 10);
    Data^.Column2 := 'Three ' + IntToStr(Rand - 10);
  end;
  //*)
end;

procedure TCdrHierarchieObjets.Button1Click(Sender: TObject);
begin
  VirtualStringTree1.FullExpand();
end;

procedure TCdrHierarchieObjets.VirtualStringTree1Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  VirtualStringTree1.Refresh;
end;

procedure TCdrHierarchieObjets.VirtualStringTree1FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTreeData;
begin
  Data := VirtualStringTree1.GetNodeData(Node);
  if (Assigned(Data)) then Data.setFrom(0, tnUNKNOWN, -1, '', '', '', '');
end;

procedure TCdrHierarchieObjets.VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PTreeData;
begin
  Data := VirtualStringTree1.GetNodeData(Node);
  case Data.TypeNode of
    tnUNKNOWN                 : ImageIndex := -1;
    tnROOT                    : ImageIndex :=  0;
    tnGROUPE                  : ImageIndex :=  1;
    tnSECTION_SCRAPS          : ImageIndex :=  2;
    tnSECTION_COURBES         : ImageIndex :=  3;
    tnSECTION_POLYLINES       : ImageIndex :=  4;
    tnSECTION_POLYGONES       : ImageIndex :=  5;
    tnSECTION_SIMPLE_LINES    : ImageIndex :=  6;
    tnSECTION_SYMBOLS         : ImageIndex :=  7;
    tnSECTION_TEXTES          : ImageIndex :=  8;
    (*tnOBJECT_SCRAP            : ;
    tnOBJECT_COURBE           : ;
    tnOBJECT_POLYLINE         : ;
    tnOBJECT_POLYGONE         : ;
    tnOBJECT_SIMPLE_LINE      : ;
    tnOBJECT_SYMBOL           : ;
    tnOBJECT_TEXTE            : ;
    //*)
  else
    ImageIndex := -1;
  end;

  if (Data.Level in [0, 1]) then ImageIndex := Data.Level;
end;

procedure TCdrHierarchieObjets.VirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TCdrHierarchieObjets.VirtualStringTree1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: PTreeData;
begin
  Data := VirtualStringTree1.GetNodeData(Node);
  case Column of
    0: CellText := format('%d - %s', [Data^.Level, Data^.Name]);
    1: CellText := Data^.DescTTypeNode();
    2: CellText := IIF(Data^.InternalIndex >= 0, format('%d', [Data^.InternalIndex]), '');
  else
    pass;
    //1: CellText := Data^.Column1;
    //2: CellText := Data^.Column2;
    //3: CellText := Data^.Column3;
  end;
end;

procedure TCdrHierarchieObjets.VirtualStringTree1NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  MyNode : PTreeData;
begin
  MyNode := VirtualStringTree1.GetNodeData(HitInfo.HitNode);
  lbNodeInfo.Caption := MyNode.DebugString();
end;


function TCdrHierarchieObjets.Initialiser(const FD: TDocumentDessin): boolean;
begin
  Result := false;
  FDocumentDessin := FD;
  AfficherMessageErreur('0001');
  RemplirTreeView();
  result := True;
end;

end.

