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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, PairSplitter;

type

{ TdlgDisplaySuperGroupes }

 TdlgDisplaySuperGroupes = class(TForm)
    btnAddGrpsAtCurrSuperGroupe: TButton;
    btnAddSuperGroupe: TButton;
    btnClearListeGroupesOfSG: TButton;
    btnModifyCurrSuperGroupe: TButton;
    btnSupprimerSuperGroupe: TButton;
    Button1: TButton;
    btnMarquerDepuisSupergroupe: TButton;
    btnGroupeDepuisTexte: TButton;
    CdrListBoxGroupes1: TCdrListBoxGroupes;
    editNomSupergroupe: TEdit;
    grbxCurrentSuperGroupe: TGroupBox;
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
    procedure btnMarquerDepuisSupergroupeClick(Sender: TObject);
    procedure btnModifyCurrSuperGroupeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
    n := length(MySG.ListeGroupes);
    if (n = 0) then exit;
    for i := 0 to n - 1 do
    begin
      QIdxGrp := MySG.ListeGroupes[i];
      if (QIdxGrp >= 0) then
      begin
        MyGrp := FDocDessin.GetGroupeByIDGroupe(QIdxGrp);
        lsbGroupesOfSuperGroupe.Items.Add(Format('%d: %s', [MyGrp.IDGroupeEntites, MyGrp.NomGroupe]));
      end;
    end;
    lbNbGroupesAssocies.Caption := format(FMT_NB_GROUPES_ASSOCIES, [lsbGroupesOfSuperGroupe.Count]);
  end;
begin
  grbxCurrentSuperGroupe.Caption := format('Supergroupe #%d', [FCurrentIdxSuperGroupe]);
  MySG := FDocDessin.GetSuperGroupe(FCurrentIdxSuperGroupe);

  editNomSupergroupe.Text := MySG.NomSuperGroupe;
  StaticText1.Caption := ArrayOfIdxGroupesToStr(MySG.ListeGroupes);
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
      EWE := split(lsbGroupesOfSuperGroupe.Items[i], ':');
      q := StrToIntDef(EWE[0], -1);
      if (q >= 0) then Result += Format('%d;', [q]);
    end;
  end;
begin
  Result.NomSuperGroupe := Trim(editNomSupergroupe.Text);
  Result.ListeGroupes   := StrToArrayOfIdxGroupes(QExtractGroupes());
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
    lsbSuperGroupes.Items.Add(MySuperGroupe.NomSuperGroupe);
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
    MySG.NomSuperGroupe := Trim(EWE);
    if (MySG.NomSuperGroupe = '') then exit;
    SetLength(MySG.ListeGroupes, 0);
    MySG.Displayed      := true;
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

  LS := StrToArrayOfIdxGroupes(EWE);
  Nb := length(LS);
  if (0 = Nb) then exit;
  MySG := FDocDessin.GetSuperGroupe(FCurrentIdxSuperGroupe);
  for i := 0 to Nb - 1 do
  begin
    if (FDocDessin.ExtractGroupeFromIdxGroupe(LS[i], MyGRP)) then AddToArrayOfIdxGroupes(MySG.ListeGroupes, MyGRP.IDGroupeEntites);
  end;
  FDocDessin.PutSuperGroupe(FCurrentIdxSuperGroupe, MySG);
  SetSuperGroupeInForm();
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
  NbGroupesOfSG := length(MySG.ListeGroupes);
  if (NbGroupesOfSG = 0) then exit;
  FDocDessin.MarquerVisibiliteOfAllGroupes(false);
  for g := 0 to NbGroupesOfSG - 1 do MiouMiou(MySG.ListeGroupes[g]);
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

procedure TdlgDisplaySuperGroupes.Button1Click(Sender: TObject);
begin
  if (lsbGroupesOfSuperGroupe.Count = 0) then exit;
  lsbGroupesOfSuperGroupe.Items.Delete(lsbGroupesOfSuperGroupe.ItemIndex);
  lbNbGroupesAssocies.Caption := format(FMT_NB_GROUPES_ASSOCIES, [lsbGroupesOfSuperGroupe.Count]);
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
      AddToArrayOfIdxGroupes(MySG.ListeGroupes, MyGrp.IDGroupeEntites);
    end;
  end;
  // sauvegarde des modifs
  FDocDessin.PutSuperGroupe(FCurrentIdxSuperGroupe, MySG);
  SetSuperGroupeInForm();
end;

end.
