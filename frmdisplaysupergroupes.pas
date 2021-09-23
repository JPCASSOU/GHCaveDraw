unit frmDisplaySuperGroupes;
// Pour éviter des dépendances croisées, ne pas utiliser TCadreListeGroupes

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
  , CallDialogsGHCaveDraw
  , LCLType
  , UnitDocDessin, CdrListeGroupes, CadreListboxGroupes,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, PairSplitter, Types;

type

{ TdlgDisplaySuperGroupes }

 TdlgDisplaySuperGroupes = class(TForm)
    btnAddGrpsAtCurrSuperGroupe: TButton;
    btnAddSuperGroupe: TButton;
    btnClearListeGroupesOfSG: TButton;
    btnModifyCurrSuperGroupe: TButton;
    btnSupprimerSuperGroupe: TButton;
    btnRemoveGroupe: TButton;
    btnMarquerDepuisSupergroupe: TButton;
    btnGroupeDepuisTexte: TButton;
    btnSuperGroupeColor: TColorButton;
    btnLstGrpsToText: TButton;
    CdrListBoxGroupes1: TCdrListBoxGroupes;
    editNomSupergroupe: TEdit;
    grbxCurrentSuperGroupe: TGroupBox;
    hcSupergroupes: THeaderControl;
    hcGroupesLies: THeaderControl;
    Label2: TLabel;
    lbNbGroupesAssocies: TLabel;
    lbNbSuperGroupes: TLabel;
    lsbGroupesOfSuperGroupe: TListBox;
    lsbSuperGroupes: TListBox;
    PairSplitter1: TPairSplitter;
    PairSplitter2: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    StaticText1: TStaticText;
    procedure btnAddGrpsAtCurrSuperGroupeClick(Sender: TObject);
    procedure btnAddSuperGroupeClick(Sender: TObject);
    procedure btnClearListeGroupesOfSGClick(Sender: TObject);
    procedure btnGroupeDepuisTexteClick(Sender: TObject);
    procedure btnLstGrpsToTextClick(Sender: TObject);
    procedure btnMarquerDepuisSupergroupeClick(Sender: TObject);
    procedure btnModifyCurrSuperGroupeClick(Sender: TObject);
    procedure btnSupprimerSuperGroupeClick(Sender: TObject);
    procedure btnRemoveGroupeClick(Sender: TObject);
    procedure lsbGroupesOfSuperGroupeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbSuperGroupesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbSuperGroupesSelectionChange(Sender: TObject; User: boolean);
  private
    FDocDessin: TDocumentDessin;
    FCurrentIdxSuperGroupe: integer;
    procedure ListerLesSuperGroupes(const IdxGr: integer);
    procedure ModifyGroupe(const Idx: TIDGroupeEntites);
    procedure SetSuperGroupeInForm();
    function  GetSuperGroupeFromForm(): TSuperGroupe;
  public
    function  Initialiser(const MyDocDessin: TDocumentDessin): boolean;
    procedure Finaliser();
  end;

var
  dlgDisplaySuperGroupes: TdlgDisplaySuperGroupes;

implementation
uses DGCDummyUnit;

{$R *.lfm}
const FMT_NB_GROUPES_ASSOCIES = '%d groupes associés';

{ TdlgDisplaySuperGroupes }

function TdlgDisplaySuperGroupes.Initialiser(const MyDocDessin: TDocumentDessin): boolean;
begin
  result := false;
  try
    FCurrentIdxSuperGroupe := 0;
    FDocDessin := MyDocDessin;
    ListerLesSuperGroupes(0);
    result := CdrListBoxGroupes1.Initialiser(MyDocDessin,
                                             FCurrentIdxSuperGroupe,
                                             True,
                                             nil,
                                             ModifyGroupe);
  except
    pass;
  end;
end;

procedure TdlgDisplaySuperGroupes.Finaliser();
begin
  pass;
end;

procedure TdlgDisplaySuperGroupes.ModifyGroupe(const Idx: TIDGroupeEntites);
var
  QDoSetThisGroupeAsCurrent: boolean;
  GP: TGroupeEntites;
begin
  if (Idx >= 0) then
  begin
    GP := FDocDessin.GetGroupe(Idx);
    if (EditerGroupe(GP, medMODIF, '', QDoSetThisGroupeAsCurrent)) then
    begin
      FDocDessin.PutGroupe(Idx, GP);
      FDocDessin.TrierGroupesByZOrder();
    end;
  end;
  CdrListBoxGroupes1.ListerLesGroupes(Idx);
end;
procedure TdlgDisplaySuperGroupes.SetSuperGroupeInForm();
var
  MySG: TSuperGroupe;
  procedure QDispListeGroupes();
  var
    n, i: Integer;
    MyGrp: TGroupeEntites;
    QIdxGrp: TIDGroupeEntites;
  begin
    lsbGroupesOfSuperGroupe.Clear;
    lbNbGroupesAssocies.Caption := format(FMT_NB_GROUPES_ASSOCIES, [lsbGroupesOfSuperGroupe.Count]);
    n := MySG.getNbGroupes();//length(MySG.ListeGroupes);
    //ShowMessageFmt('%d groupes', [n]);
    if (n = 0) then exit;
    for i := 0 to n - 1 do
    begin
      QIdxGrp := MySG.ListeGroupes.GetElement(i); // [i];
      if (QIdxGrp >= 0) then
      begin
        MyGrp := FDocDessin.GetGroupeByIDGroupe(QIdxGrp);
        lsbGroupesOfSuperGroupe.Items.Add(Format('%d', [MyGrp.IDGroupeEntites]));
      end;
    end;
    lbNbGroupesAssocies.Caption := format(FMT_NB_GROUPES_ASSOCIES, [lsbGroupesOfSuperGroupe.Count]);
  end;
begin
  grbxCurrentSuperGroupe.Caption := format('Supergroupe #%d', [FCurrentIdxSuperGroupe]);
  MySG := FDocDessin.GetSuperGroupe(FCurrentIdxSuperGroupe);
  btnSuperGroupeColor.ButtonColor := MySG.Couleur;
  editNomSupergroupe.Text := MySG.NomSuperGroupe;
  StaticText1.Caption :=  MySG.ListeGroupes.toString(); //ArrayOfIdxGroupesToStr(MySG.ListeGroupes);
  QDispListeGroupes();
end;

function TdlgDisplaySuperGroupes.GetSuperGroupeFromForm(): TSuperGroupe;
  function QExtractGroupes(): string;
  var
    i, n: Integer;
    EWE: TGHStringArray;
    q: LongInt;
  begin
    Result := '';
    n := lsbGroupesOfSuperGroupe.Count;
    if (0 = n) then exit;
    for i := 0 to n - 1 do
    begin
      q := StrToIntDef(lsbGroupesOfSuperGroupe.Items[i], -1);
      if (q > 0) then Result += Format('%d;', [q]);
    end;
  end;
begin
  Result.Couleur        := btnSuperGroupeColor.ButtonColor;
  Result.NomSuperGroupe := Trim(editNomSupergroupe.Text);
  Result.ListeGroupes.fromString(QExtractGroupes()); //  := StrToArrayOfIdxGroupes(QExtractGroupes());
end;

procedure TdlgDisplaySuperGroupes.ListerLesSuperGroupes(const IdxGr: integer);
var
  n, i: Integer;
  MySuperGroupe: TSuperGroupe;
begin
  FCurrentIdxSuperGroupe := IdxGr;
  lsbSuperGroupes.Visible                      := false;
  lsbSuperGroupes.Clear;

  n := FDocDessin.GetNbSuperGroupes();
  for i := 0 to n-1 do
  begin
    MySuperGroupe := FDocDessin.GetSuperGroupe(i);
    lsbSuperGroupes.Items.Add(''); // Item redessiné par le composant
  end;
  lsbSuperGroupes.ItemIndex := IdxGr;
  lsbSuperGroupes.Visible   := true;
  lbNbSuperGroupes.Caption  := Format('%d super-groupes', [n]);
  SetSuperGroupeInForm();
end;


procedure TdlgDisplaySuperGroupes.lsbSuperGroupesSelectionChange(Sender: TObject; User: boolean);
begin
  FCurrentIdxSuperGroupe := lsbSuperGroupes.ItemIndex;
  SetSuperGroupeInForm();
end;


procedure TdlgDisplaySuperGroupes.btnAddSuperGroupeClick(Sender: TObject);
var
  MySG: TSuperGroupe;
  n: Integer;
  EWE: String;
begin
  EWE := format('Supergroupe_%d', [FDocDessin.GetNbSuperGroupes()]);
  if (InputQuery('Ajout d''un supergroupe', 'Nom', EWE)) then
  begin
    MySG.Couleur            := clYellow;
    MySG.NomSuperGroupe     := Trim(EWE);
    if (MySG.NomSuperGroupe = '') then exit;
    MySG.Displayed          := true;
    MySG.ListeGroupes.Empty();

    FDocDessin.AddSuperGroupe(MySG);
    n := FDocDessin.GetNbSuperGroupes() - 1;
    ListerLesSuperGroupes(n);
    SetSuperGroupeInForm();
  end;
end;

procedure TdlgDisplaySuperGroupes.btnClearListeGroupesOfSGClick(Sender: TObject);
begin
  if (not QuestionOuiNon('Vider la liste')) then exit;
  lsbGroupesOfSuperGroupe.Clear;
  lbNbGroupesAssocies.Caption := format(FMT_NB_GROUPES_ASSOCIES, [lsbGroupesOfSuperGroupe.Count]);
end;

procedure TdlgDisplaySuperGroupes.btnGroupeDepuisTexteClick(Sender: TObject);
var
  EWE: String;
  LS: TArrayOfIdxGroupes;
  MySG: TSuperGroupe;
  i, Nb: Integer;
  MyGRP: TGroupeEntites;
begin
  EWE := '';
  if (not InputQuery('Groupes depuis texte', 'Liste (sép: ;)', EWE)) then exit;

  LS.fromString(EWE);// := StrToArrayOfIdxGroupes(EWE);
  Nb := LS.GetNbElements(); //length(LS);

  if (0 = Nb) then exit;
  MySG := FDocDessin.GetSuperGroupe(FCurrentIdxSuperGroupe);
  for i := 0 to Nb - 1 do
  begin
    if (FDocDessin.ExtractGroupeFromIdxGroupe(LS.GetElement(i), MyGRP)) then MySG.ListeGroupes.AddElementAndSort(MyGRP.IDGroupeEntites);//AddToArrayOfIdxGroupes(MySG.ListeGroupes, MyGRP.IDGroupeEntites);
  end;
  FDocDessin.PutSuperGroupe(FCurrentIdxSuperGroupe, MySG);
  SetSuperGroupeInForm();
end;

procedure TdlgDisplaySuperGroupes.btnLstGrpsToTextClick(Sender: TObject);
var
  MySG: TSuperGroupe;
  EWE: String;
begin
  MySG := FDocDessin.GetSuperGroupe(FCurrentIdxSuperGroupe);
  EWE  := MySG.ListeGroupesToText();
  AfficherMessageErreur(EWE);
  EWE := InputBox('Liste des groupes', 'Groupes', EWE);
end;

procedure TdlgDisplaySuperGroupes.btnMarquerDepuisSupergroupeClick(Sender: TObject);
var
  NbGroupesOfSG, g: Integer;
  MySG: TSuperGroupe;
  procedure MiouMiou(const Miou: TIDGroupeEntites);
  var
    NbGroupes, i: Integer;
    MyGRP: TGroupeEntites;
  begin
    NbGroupes := FDocDessin.GetNbGroupes();
    for i := 0 to NbGroupes - 1 do
    begin
      MyGRP := FDocDessin.GetGroupe(i);
      if (Miou = MyGRP.IDGroupeEntites) then
      begin
        MyGRP.Visible := True;
        FDocDessin.PutGroupe(i, MyGRP);
      end;
    end;
  end;
begin
  MySG := FDocDessin.GetSuperGroupe(FCurrentIdxSuperGroupe);

  NbGroupesOfSG := MySG.ListeGroupes.GetNbElements();
  if (NbGroupesOfSG = 0) then exit;
  FDocDessin.MarquerVisibiliteOfAllGroupes(false);
  for g := 0 to NbGroupesOfSG - 1 do MiouMiou(MySG.ListeGroupes.GetElement(g));
  CdrListBoxGroupes1.ListerLesGroupes(0);
end;

procedure TdlgDisplaySuperGroupes.btnModifyCurrSuperGroupeClick(Sender: TObject);
var
  MyGrp: TSuperGroupe;
begin
  MyGrp := GetSuperGroupeFromForm();
  FDocDessin.PutSuperGroupe(FCurrentIdxSuperGroupe, MyGRP);
  ListerLesSuperGroupes(FCurrentIdxSuperGroupe);
end;

procedure TdlgDisplaySuperGroupes.btnSupprimerSuperGroupeClick(Sender: TObject);
begin
  pass;
end;

procedure TdlgDisplaySuperGroupes.btnRemoveGroupeClick(Sender: TObject);
var
  MySuperGRP: TSuperGroupe;
  i, n: Integer;
  IdxGrp: TIDGroupeEntites;
  MyGroupe: TGroupeEntites;
begin
  MySuperGRP := FDocDessin.GetSuperGroupe(FCurrentIdxSuperGroupe);
  AfficherMessageErreur(MySuperGRP.ListeGroupesToText());
  if (MySuperGRP.getNbGroupes() = 0) then exit;
  i := lsbGroupesOfSuperGroupe.ItemIndex;
  IdxGrp := MySuperGRP.ListeGroupes.GetElement(i);
  MyGroupe := FDocDessin.GetGroupeByIDGroupe(IdxGrp);
  if (QuestionOuiNon(Format('Supprimer le groupe %d: %d - %s', [i, MyGroupe.IDGroupeEntites, MyGroupe.NomGroupe]))) then
  begin
    MySuperGRP.RemoveGroupe(i);
    AfficherMessageErreur(MySuperGRP.ListeGroupesToText());
    // enregistrement pour le lsbGroupesOfSuperGroupe.OnDrawItem
    FDocDessin.PutSuperGroupe(FCurrentIdxSuperGroupe, MySuperGRP);
    // puis récupération
    MySuperGRP := FDocDessin.GetSuperGroupe(FCurrentIdxSuperGroupe);
    n := MySuperGRP.getNbGroupes();
    lbNbGroupesAssocies.Caption := format(FMT_NB_GROUPES_ASSOCIES, [n]);
    // relister
    lsbGroupesOfSuperGroupe.Clear;

    if (0 = n) then exit;
    for i := 0 to n - 1 do lsbGroupesOfSuperGroupe.Items.add(format('%d', [MySuperGRP.ListeGroupes.GetElement(i)]));
    lsbGroupesOfSuperGroupe.ItemIndex := 0;
  end;
end;

procedure TdlgDisplaySuperGroupes.lsbGroupesOfSuperGroupeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
  mg  = 1;
var
  MySuperGRP: TSuperGroupe;
  MyGRP: TGroupeEntites;
  procedure DessineCoche(const TB: integer; const IsChecked: boolean; const bg: TColor);
  var
    QR: TRect;
    H : Integer;
  begin
    lsbGroupesOfSuperGroupe.Canvas.Pen.Style   := psSolid;
    lsbGroupesOfSuperGroupe.Canvas.Brush.Style := bsSolid;
    lsbGroupesOfSuperGroupe.Canvas.Pen.Color   := clBlack;
    QR.Left   := ARect.Left   + TB + 2;
    QR.Top    := ARect.Top    + 1;
    QR.Bottom := ARect.Bottom - 1;
    H := QR.Bottom - QR.Top;
    QR.Right  := QR.Left + H;
    lsbGroupesOfSuperGroupe.Canvas.Brush.Color := clWhite;
    if (IsChecked) then lsbGroupesOfSuperGroupe.Canvas.Brush.Color := clRed;
    lsbGroupesOfSuperGroupe.Canvas.Rectangle(QR);
    lsbGroupesOfSuperGroupe.Canvas.Brush.Color := bg;
  end;
  procedure DessineRectCouleur(const TB: integer; const W: integer;const Coul: TColor; const bg: TColor);
  var
    QR: TRect;
  begin
    lsbGroupesOfSuperGroupe.Canvas.Pen.Style   := psSolid;
    lsbGroupesOfSuperGroupe.Canvas.Brush.Style := bsSolid;
    lsbGroupesOfSuperGroupe.Canvas.Brush.Color := Coul;
    lsbGroupesOfSuperGroupe.Canvas.Pen.Color   := clBlack;
    QR.Left   := ARect.Left   + TB + 3;
    QR.Top    := ARect.Top    + 1;
    QR.Bottom := ARect.Bottom - 1;
    QR.Right  := QR.Left + W - 4;
    lsbGroupesOfSuperGroupe.Canvas.Rectangle(QR);
    lsbGroupesOfSuperGroupe.Canvas.Brush.Color := bg;
  end;
  procedure DessineFiletColonne(const TB: integer);
  begin
    lsbGroupesOfSuperGroupe.Canvas.Pen.Color   := clSilver; // pour les filets
    lsbGroupesOfSuperGroupe.Canvas.MoveTo(TB, ARect.Top);
    lsbGroupesOfSuperGroupe.Canvas.LineTo(TB, ARect.Bottom);
  end;
  procedure DessineItem(const bg,tc: TColor);
  VAR
    HS: THeaderSection;
    QR: TRect;
  begin
    lsbGroupesOfSuperGroupe.Canvas.Brush.Color := bg;
    lsbGroupesOfSuperGroupe.Canvas.Font.Color  := tc;
    lsbGroupesOfSuperGroupe.Canvas.FillRect(ARect);
    HS := hcGroupesLies.Sections.Items[0];
    lsbGroupesOfSuperGroupe.Canvas.TextOut(HS.Left + 4, ARect.Top, Format('%d', [MyGRP.IDGroupeEntites]));
    HS := hcGroupesLies.Sections.Items[1];  // affiché
    DessineFiletColonne(HS.Left - Q4);
    DessineRectCouleur(HS.Left - Q4, HS.Width, MyGRP.CouleurGroupe, bg);
    HS := hcGroupesLies.Sections.Items[2];  // nom
    DessineFiletColonne(HS.Left - Q4);
    lsbGroupesOfSuperGroupe.Canvas.TextOut(HS.Left + 4, ARect.Top,  MyGRP.NomGroupe);
  end;
begin
  MySuperGRP := FDocDessin.GetSuperGroupe(FCurrentIdxSuperGroupe);
  if (MySuperGRP.getNbGroupes() = 0) then exit;
  try
    MyGRP := FDocDessin.GetGroupeByIDGroupe(MySuperGRP.ListeGroupes.GetElement(Index));
    if (odSelected in state) then DessineItem(clBlue     , clWhite)
                             else DessineItem(clWhite    , clBlack);
  except
    pass;
  end;
end;



procedure TdlgDisplaySuperGroupes.lsbSuperGroupesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
  mg  = 1;
var
  MySuperGRP: TSuperGroupe;
  procedure DessineCoche(const TB: integer; const IsChecked: boolean; const bg: TColor);
  var
    QR: TRect;
    H : Integer;
  begin
    lsbSuperGroupes.Canvas.Pen.Style   := psSolid;
    lsbSuperGroupes.Canvas.Brush.Style := bsSolid;
    lsbSuperGroupes.Canvas.Pen.Color   := clBlack;

    QR.Left   := ARect.Left   + TB + 2;
    QR.Top    := ARect.Top    + 1;
    QR.Bottom := ARect.Bottom - 1;
    H := QR.Bottom - QR.Top;
    QR.Right  := QR.Left + H;
    lsbSuperGroupes.Canvas.Brush.Color := clWhite;
    if (IsChecked) then lsbSuperGroupes.Canvas.Brush.Color := clRed;
    lsbSuperGroupes.Canvas.Rectangle(QR);
    lsbSuperGroupes.Canvas.Brush.Color := bg;
  end;
  procedure DessineRectCouleur(const TB: integer; const W: integer;const Coul: TColor; const bg: TColor);
  var
    QR: TRect;
  begin
    lsbSuperGroupes.Canvas.Pen.Style   := psSolid;
    lsbSuperGroupes.Canvas.Brush.Style := bsSolid;
    lsbSuperGroupes.Canvas.Brush.Color := Coul;
    lsbSuperGroupes.Canvas.Pen.Color   := clBlack;
    QR.Left   := ARect.Left   + TB + 3;
    QR.Top    := ARect.Top    + 1;
    QR.Bottom := ARect.Bottom - 1;
    QR.Right  := QR.Left + W - 4;
    lsbSuperGroupes.Canvas.Rectangle(QR);
    lsbSuperGroupes.Canvas.Brush.Color := bg;
  end;
  procedure DessineFiletColonne(const TB: integer);
  begin
    lsbSuperGroupes.Canvas.Pen.Color   := clSilver; // pour les filets
    lsbSuperGroupes.Canvas.MoveTo(TB, ARect.Top);
    lsbSuperGroupes.Canvas.LineTo(TB, ARect.Bottom);
  end;
  procedure DessineItem(const bg,tc: TColor);
  VAR
    HS: THeaderSection;
    QR: TRect;
  begin
    lsbSuperGroupes.Canvas.Brush.Color := bg;
    lsbSuperGroupes.Canvas.Font.Color  := tc;
    lsbSuperGroupes.Canvas.FillRect(ARect);
    HS := hcSupergroupes.Sections.Items[0];
    lsbSuperGroupes.Canvas.TextOut(HS.Left + 4, ARect.Top, Format('%d', [Index]));
    HS := hcSupergroupes.Sections.Items[1];  // affiché
    DessineFiletColonne(HS.Left - Q4);
    DessineRectCouleur(HS.Left - Q4, HS.Width, MySuperGRP.Couleur, bg);
    HS := hcSupergroupes.Sections.Items[2];  // nom
    DessineFiletColonne(HS.Left - Q4);
    lsbSuperGroupes.Canvas.TextOut(HS.Left + 4, ARect.Top,  format('%d', [MySuperGRP.getNbGroupes()]));
    HS := hcSupergroupes.Sections.Items[3];  // nom
    DessineFiletColonne(HS.Left - Q4);
    lsbSuperGroupes.Canvas.TextOut(HS.Left + 4, ARect.Top,  MySuperGRP.NomSuperGroupe);
  end;
begin
  try
    MySuperGRP := FDocDessin.GetSuperGroupe(Index);
    if (odSelected in state) then DessineItem(clBlue     , clWhite)
                             else DessineItem(clWhite    , clBlack);
  except
    pass;
  end;
end;



procedure TdlgDisplaySuperGroupes.btnAddGrpsAtCurrSuperGroupeClick(Sender: TObject);
var
  i, Nb: Integer;
  MyGrp: TGroupeEntites;
  MySG : TSuperGroupe;
  EWE: String;
begin
  Nb := FDocDessin.GetNbGroupes();
  if (0 = Nb) then exit;
  // récupération pour modification
  MySG := FDocDessin.GetSuperGroupe(FCurrentIdxSuperGroupe);
  for i := 0 to Nb - 1 do
  begin
    if (CdrListBoxGroupes1.IsSelectedItem(i)) then
    begin
      MyGrp := FDocDessin.GetGroupe(i);
      MySG.ListeGroupes.AddElementAndSort(MyGrp.IDGroupeEntites);
    end;
  end;
  // sauvegarde des modifs
  FDocDessin.PutSuperGroupe(FCurrentIdxSuperGroupe, MySG);
  SetSuperGroupeInForm();
end;

end.
