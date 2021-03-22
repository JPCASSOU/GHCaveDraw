unit frmObjetsOfGroupe;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$IFDEF LANGUAGE_FRENCH}
   UnitMessages_fr
  {$ENDIF}
  {$IFDEF LINUX}
  , Types
  {$ENDIF}
  , GHCD_Types
  , GeneralFunctions
  , LCLType
  , UnitListesSimplesWithGeneriques
  , UnitDocDessin
  , CallDialogsGHCaveDraw
  , Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls;
type TNatureObject = (odUNKNOWN, ogSCRAP, ogCOURBE, ogPOLYLINE, ogLINE, ogPOLYGONE, ogSYMBOLE, ogTEXTE);
type TObjectOfGroupe = record
  Nature      : TNatureObject;
  InternalIdx : integer;
  Couleur     : TColor;
  Attributs   : string;
end;
type  TdlgObjetsOfGroupe = class(TForm)
    BitBtn1: TBitBtn;
    btnSupprimerObjet: TButton;
    hcColonnes: THeaderControl;
    ImageList1: TImageList;
    lsbObjets: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    lbIdGroupe: TStaticText;
    lbNomGroupe: TStaticText;
    procedure btnSupprimerObjetClick(Sender: TObject);
    procedure lsbObjetsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  private
    FListeObjets   : TListeSimple<TObjectOfGroupe>;
    FDocDessin     : TDocumentDessin;
    FCurrentGroupe : TGroupeEntites;
    procedure ListerLesObjets();
    function RecenserLesObjets(): boolean;
  public
    function Initialiser(const MyDocDessin: TDocumentDessin; const QIdx: TIDGroupeEntites): boolean;
    procedure Finaliser();
  end;

var
  dlgObjetsOfGroupe: TdlgObjetsOfGroupe;

implementation
uses DGCDummyUnit;

{$R *.lfm}

{ TdlgObjetsOfGroupe }
function DescribeTypeObject(const T: TNatureObject): string;
begin
  result := ChooseString(Ord(T), ['---', 'Scrap', 'Courbe', 'Polyligne', 'Ligne', 'Polygone', 'Symbole', 'Texte']);
end;

function TdlgObjetsOfGroupe.RecenserLesObjets(): boolean;
var
  i, Nb: Integer;
  MyScrap: TScrap;
  EWE : TObjectOfGroupe;
  MyCourbe: TCourbe;
  MyPolyline: TPolyLigne;
  MySimpleLine: TSimpleLigne;
  MySymbole: TSymbole;
  MyTexte: TTextObject;
  MyPolygone: TPolygone;
begin
  result := false;
  FListeObjets.ClearListe();
  Nb := FDocDessin.GetNbScraps();
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      MyScrap := FDocDessin.GetScrap(i);
      if (FCurrentGroupe.IDGroupeEntites = MyScrap.IDGroupe) then
      begin
        EWE.Nature      := ogSCRAP;
        EWE.InternalIdx := i;
        EWE.Couleur     := MyScrap.Couleur;
        EWE.Attributs   := MyScrap.Nom;
        FListeObjets.AddElement(EWE);
      end;
    end;
  end;
  Nb := FDocDessin.GetNbCourbes();
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      MyCourbe := FDocDessin.GetCourbe(i);
      if (FCurrentGroupe.IDGroupeEntites = MyCourbe.IDGroupe) then
      begin
        EWE.Nature      := ogCOURBE;
        EWE.InternalIdx := i;
        EWE.Couleur     := clBlack;
        EWE.Attributs   := Format('%d arcs', [Length(MyCourbe.Arcs)]);
        FListeObjets.AddElement(EWE);
      end;
    end;
  end;
  Nb := FDocDessin.GetNbPolylignes();
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      MyPolyline := FDocDessin.GetPolyligne(i);
      if (FCurrentGroupe.IDGroupeEntites = MyPolyline.IDGroupe) then
      begin
        EWE.Nature      := ogPOLYLINE;
        EWE.InternalIdx := i;
        EWE.Couleur     := clBlack;
        EWE.Attributs   := Format('%d sommets', [Length(MyPolyline.Sommets)]);
        FListeObjets.AddElement(EWE);
      end;
    end;
  end;
  Nb := FDocDessin.GetNbSimpleLignes();
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      MySimpleLine := FDocDessin.GetSimpleLigne(i);
      if (FCurrentGroupe.IDGroupeEntites = MySimpleLine.IDGroupe) then
      begin
        EWE.Nature      := ogLINE;
        EWE.InternalIdx := i;
        EWE.Couleur     := clBlack;
        EWE.Attributs   := ChooseString(Ord(MySimpleLine.IDStyleLigne), ['---', 'Fleche', 'A poursuivre', 'Fracture', 'Ligne de plus grande pente', 'Pendage']);
        FListeObjets.AddElement(EWE);
      end;
    end;
  end;
  Nb := FDocDessin.GetNbPolygones();
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      MyPolygone := FDocDessin.GetPolygone(i);
      if (FCurrentGroupe.IDGroupeEntites = MyPolygone.IDGroupe) then
      begin
        EWE.Nature      := ogPOLYGONE;
        EWE.InternalIdx := i;
        EWE.Couleur     := clBlack;
        EWE.Attributs   := Format('%d sommets', [Length(MyPolygone.Sommets)]);
        FListeObjets.AddElement(EWE);
      end;
    end;
  end;
  Nb := FDocDessin.GetNbSymboles();
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      MySymbole := FDocDessin.GetSymbole(i);
      if (FCurrentGroupe.IDGroupeEntites = MySymbole.IDGroupe) then
      begin
        EWE.Nature      := ogSYMBOLE;
        EWE.InternalIdx := i;
        EWE.Couleur     := clBlack;
        EWE.Attributs   := ''; //ChooseString(Ord(MySymbole.TypeObject), ['---', 'Fleche', 'A poursuivre', 'Fracture', 'Ligne de plus grande pente', 'Pendage']);
        FListeObjets.AddElement(EWE);
      end;
    end;
  end;
  Nb := FDocDessin.GetNbTextes();
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      MyTexte := FDocDessin.GetTexte(i);
      if (FCurrentGroupe.IDGroupeEntites = MyTexte.IDGroupe) then
      begin
        EWE.Nature      := ogTEXTE;
        EWE.InternalIdx := i;
        EWE.Couleur     := clBlack;
        EWE.Attributs   := MyTexte.Text;
        FListeObjets.AddElement(EWE);
      end;
    end;
  end;
  result := True;
end;

procedure TdlgObjetsOfGroupe.lsbObjetsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
  mg  = 1;
var
  QObj : TObjectOfGroupe;
  procedure DessineRectCouleur(const TB: integer; const W: integer;const Coul: TColor; const bg: TColor);
  var
    QR: TRect;
  begin
    lsbObjets.Canvas.Pen.Style   := psSolid;
    lsbObjets.Canvas.Brush.Style := bsSolid;
    lsbObjets.Canvas.Brush.Color := Coul;
    lsbObjets.Canvas.Pen.Color   := clBlack;
    QR.Left   := ARect.Left   + TB + 3;
    QR.Top    := ARect.Top    + 1;
    QR.Bottom := ARect.Bottom - 1;
    QR.Right  := QR.Left + W - 4;
    lsbObjets.Canvas.Rectangle(QR);
    lsbObjets.Canvas.Brush.Color := bg;
  end;
  procedure DessineFiletColonne(const TB: integer);
  begin
    lsbObjets.Canvas.Pen.Color   := clSilver; // pour les filets
    lsbObjets.Canvas.MoveTo(TB, ARect.Top);
    lsbObjets.Canvas.LineTo(TB, ARect.Bottom);
  end;
  procedure DessineItem(const bg,tc: TColor);
  VAR
    HS: THeaderSection;
    QR: TRect;
  begin
    lsbObjets.Canvas.Brush.Color := bg;
    lsbObjets.Canvas.Font.Color  := tc;
    lsbObjets.Canvas.FillRect(ARect);
    HS := hcColonnes.Sections.Items[0];  // affiché
    ImageList1.Draw(lsbObjets.Canvas, HS.Left + 4, ARect.Top, Ord(QObj.Nature));
    lsbObjets.Canvas.TextOut(HS.Left + 28, ARect.Top, DescribeTypeObject(QObj.Nature));

    //lsbObjets.Canvas.TextOut(HS.Left + 4, ARect.Top,  Format('%d',[MyGRP.NombreObjets]));
    HS := hcColonnes.Sections.Items[1];  // affiché
    DessineFiletColonne(HS.Left - Q4);
    lsbObjets.Canvas.TextOut(HS.Left + 4, ARect.Top, Format('%d', [QObj.InternalIdx]));

    HS := hcColonnes.Sections.Items[2];  // affiché
    DessineFiletColonne(HS.Left - Q4);
    lsbObjets.Canvas.TextOut(HS.Left + 4, ARect.Top, QObj.Attributs);

  end;
begin
  try
    QObj := FListeObjets.GetElement(Index);
    DessineItem(clWhite, clBlack);
    if (odSelected in state) then
    begin
      lsbObjets.Canvas.brush.color := clBlue;
      DessineItem(clBlue, clWhite);
    end;
  except

  end;
end;

procedure TdlgObjetsOfGroupe.btnSupprimerObjetClick(Sender: TObject);
var
  Nb: LongInt;
  MyObject: TObjectOfGroupe;
begin
  Nb := FListeObjets.GetNbElements();
  if (0 = Nb) then Exit;
  Nb := lsbObjets.ItemIndex;
  MyObject := FListeObjets.GetElement(Nb);
  if (QuestionOuiNon(Format('Supprimer %s %d', [DescribeTypeObject(MyObject.Nature), MyObject.InternalIdx]))) then
  begin
    case MyObject.Nature of
      odUNKNOWN  : exit;
      ogSCRAP    : FDocDessin.DeleteScrap(MyObject.InternalIdx);
      ogCOURBE   : FDocDessin.DeleteCourbe(MyObject.InternalIdx);
      ogPOLYLINE : FDocDessin.DeletePolyligne(MyObject.InternalIdx);
      ogLINE     : FDocDessin.DeleteSimpleLigne(MyObject.InternalIdx);
      ogPOLYGONE : FDocDessin.DeletePolygone(MyObject.InternalIdx);
      ogSYMBOLE  : FDocDessin.DeleteSymbole(MyObject.InternalIdx);
      ogTEXTE    : FDocDessin.DeleteTexte(MyObject.InternalIdx);
    end;
    RecenserLesObjets();
    ListerLesObjets();
  end;
end;

procedure TdlgObjetsOfGroupe.ListerLesObjets();
var
  i, Nb: LongInt;
  EWE : TObjectOfGroupe;
begin
  lsbObjets.Clear;
  Nb := FListeObjets.GetNbElements();
  if (0 = Nb) then Exit;
  for i := 0 to Nb - 1 do
  begin
    EWE := FListeObjets.GetElement(i);
    lsbObjets.Items.Add(Format('%d, %d, %s', [ EWE.Nature, EWE.InternalIdx, EWE.Attributs ]));
  end;
  lsbObjets.ItemIndex := 0;
end;

function TdlgObjetsOfGroupe.Initialiser(const MyDocDessin: TDocumentDessin; const QIdx: TIDGroupeEntites): boolean;
begin
  result := false;
  try
    FDocDessin     := MyDocDessin;
    FCurrentGroupe := MyDocDessin.GetGroupeByIDGroupe(QIdx);
    self.Caption   := Format('Objets du groupe %d - %s', [FCurrentGroupe.IDGroupeEntites, FCurrentGroupe.NomGroupe]);
    lbIdGroupe.Caption  := IntToStr(FCurrentGroupe.IDGroupeEntites);
    lbNomGroupe.Caption := FCurrentGroupe.NomGroupe;
    FListeObjets   := TListeSimple<TObjectOfGroupe>.Create;
    if (RecenserLesObjets()) then
    begin
      ListerLesObjets();
      exit(True);
    end;
  except
  end;
end;

procedure TdlgObjetsOfGroupe.Finaliser();
begin
  try
    FListeObjets.ClearListe();
  finally
    FreeAndNil(FListeObjets);
  end;
end;

end.

