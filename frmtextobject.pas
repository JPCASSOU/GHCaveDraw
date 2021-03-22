// 02/02/2014: Callback pour répercussion des modifs dans l'affichage du plan
unit frmTextObject;

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GHCD_Types,
  GeneralFunctions,
  UnitDocDessin,
  Classes, SysUtils, FileUtil, curredit, Forms,
  Controls, Graphics, Dialogs, Buttons, StdCtrls;


type

  { TdlgTextObject }

  TdlgTextObject = class(TForm)
    BitBtn1: TBitBtn;
    btnPresetStationLongAzPente1: TButton;
    Button1: TButton;
    btnPresetStationLongAzPente: TButton;
    Button2: TButton;
    btnApply: TButton;
    Button3: TButton;
    cmbGroupes: TComboBox;
    cmbStyles: TComboBox;
    cmbPresets: TComboBox;
    editIDBaseStation: TEdit;
    editOffsetX: TCurrencyEdit;
    editOffsetY: TCurrencyEdit;
    editOffsetZ: TCurrencyEdit;
    editTexte: TEdit;
    grbxAlignement: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lbCoordsPointInsertion: TStaticText;
    lbOffset: TLabel;
    lbGroupes: TLabel;
    lbOffset1: TLabel;
    lbOffset2: TLabel;
    lbStyles: TLabel;
    lbTexte: TLabel;
    lbBaseStation: TLabel;
    rbLI7: TRadioButton;
    rbLI4: TRadioButton;
    rbLI1: TRadioButton;
    rbLI2: TRadioButton;
    rbLI5: TRadioButton;
    rbLI8: TRadioButton;
    rbLI3: TRadioButton;
    rbLI6: TRadioButton;
    rbLI9: TRadioButton;
    lbCoordsBaseStation: TStaticText;
    lbIdxTextObj: TStaticText;
    procedure BitBtn1Click(Sender: TObject);
    procedure btnPresetStationLongAzPente1Click(Sender: TObject);
    procedure btnPresetStationLongAzPenteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);

    procedure cmbPresetsChange(Sender: TObject);

  private
    { private declarations }
    FDocumentDessin  : TDocumentDessin;
    FCurrentIdxTexte : integer;
    FModeEdition     : TModeEdition;
    FObjetTexte      : TTextObject;
    FAbsolutePos     : TPoint3Df;
    FProcRepercute   : TProcRefreshVues;
    function GetDataFromForm(out SB: TTextObject): boolean;
    function GetAlignement(): integer;
    procedure SetBtnAlignement(const A: byte);
  public
    { public declarations }
    procedure InitCaptions();
    procedure Initialiser(const MyDocuDessin: TDocumentDessin;
                          const IdxTextObj  : integer;
                          const ModeEdition : TModeEdition;
                          const Currgroupe  : TIDGroupeEntites;
                          const TypeTexte   : TNatureObjetTexte;
                          const ObjetTexte  : TTextObject;
                          const QAbsolutePos: TPoint3Df;
                          const ProcRepercute   : TProcRefreshVues);

    function  GetTextObjectFromForm(): TTextObject;
  end;

var
  dlgTextObject: TdlgTextObject;

implementation
uses DGCDummyUnit;

{$R *.lfm}

{ TdlgTextObject }
procedure TdlgTextObject.InitCaptions;
begin
  lbGroupes.Caption         := rsDLG_TXT_GROUPES;
  lbStyles.Caption          := rsDLG_TXT_STYLE;
  lbTexte.Caption           := rsDLG_TXT_TEXTE;
  lbBaseStation.Caption     := rsDLG_TXT_IDBASESTATION;
  lbOffset.Caption          := rsDLG_TXT_OFFSET;
  grbxAlignement.Caption    := rsDLG_TXT_ALIGNEMENT;
end;

procedure TdlgTextObject.Initialiser(const MyDocuDessin: TDocumentDessin;
                                     const IdxTextObj  : integer;
                                     const ModeEdition: TModeEdition;
                                     const Currgroupe  : TIDGroupeEntites;
                                     const TypeTexte   : TNatureObjetTexte;
                                     const ObjetTexte  : TTextObject;
                                     const QAbsolutePos: TPoint3Df;
                                     const ProcRepercute : TProcRefreshVues);
var
  GRP: TGroupeEntites;
  i: Integer;
  ST: TStyleTexte;
  EWE: TBaseStation;
begin

  // fonction de rappel
  FProcRepercute := ProcRepercute;

  FAbsolutePos := QAbsolutePos;
  lbCoordsPointInsertion.Caption:= Format('X = %.2f, Y = %.2f', [FAbsolutePos.X, FAbsolutePos.Y]);
  self.Caption    := rsDLG_TXT_TITRE;

  InitCaptions;
  FDocumentDessin := MyDocuDessin;

  FObjetTexte     := ObjetTexte;
  // index du texte courant
  FModeEdition := ModeEdition;
  if (FModeEdition = medMODIF) then  FCurrentIdxTexte := IdxTextObj
                               else  FCurrentIdxTexte := -1;
  lbIdxTextObj.Caption := Format('ID = %d', [FCurrentIdxTexte]);
  // groupes
  cmbGroupes.Clear;
  for i := 0 to FDocumentDessin.GetNbGroupes - 1 do
  begin
    GRP := FDocumentDessin.GetGroupe(i);
    cmbGroupes.Items.Add(Format('%d - %s', [GRP.IDGroupeEntites, GRP.NomGroupe]));
  end;
  // styles
  cmbStyles.Clear;
  for i := 0 to FDocumentDessin.GetNbStylesTexte() - 1 do
  begin
    ST := FDocumentDessin.GetStyleTexte(i);
    cmbStyles.Items.Add(ST.DescStyle);
  end;
  case FModeEdition of
    medCREATION:
      begin
        cmbGroupes.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(Currgroupe);
        cmbStyles.ItemIndex  := 0;
        cmbStyles.ItemIndex  := Ord(TypeTexte);
        // en mode création, calcul de l'offset de la station de base
        EWE := FDocumentDessin.GetNearBasepoint(QAbsolutePos.X, QAbsolutePos.Y, EWE, False);
        {$IFDEF TIDBASEPOINT_AS_TEXT}
        editIDBaseStation.Text      := EWE.IDStation;
        {$ELSE}
        editIDBaseStation.Text      := GetToporobotIDStationAsString(EWE);
        {$ENDIF TIDBASEPOINT_AS_TEXT}


        editOffsetX.Value := QAbsolutePos.X - EWE.PosStation.X;
        editOffsetY.Value := QAbsolutePos.Y - EWE.PosStation.Y;
        editOffsetZ.Value := 0.00;
        SetBtnAlignement(1);
      end;
    medMODIF:
      begin
        if (FDocumentDessin.GetBasePointByIndex(FObjetTexte.IDBaseSt, EWE)) then
        begin
          {$IFDEF TIDBASEPOINT_AS_TEXT}
          editIDBaseStation.Text       := EWE.IDStation;
          {$ELSE}
          editIDBaseStation.Text      := GetToporobotIDStationAsString(EWE);
          {$ENDIF TIDBASEPOINT_AS_TEXT}
          cmbGroupes.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(FObjetTexte.IDGroupe);

          cmbStyles.ItemIndex  := Ord(FObjetTexte.IDStyleTexte);

          if (FDocumentDessin.GetBasePointByIndex(FObjetTexte.IDBaseSt, EWE)) then
          begin
            editOffsetX.Value := FObjetTexte.Offset.X;
            editOffsetY.Value := FObjetTexte.Offset.Y;
            editOffsetZ.Value := 0.00;

            lbCoordsBaseStation.Caption:= format('X = %.2f, Y = %.2f', [EWE.PosStation.X, EWE.PosStation.Y]);
            SetBtnAlignement(FObjetTexte.Alignment);
          end;
        end;
      end;
  end;


  // texte
  editTexte.Text := FObjetTexte.Text;
end;


procedure TdlgTextObject.btnPresetStationLongAzPenteClick(Sender: TObject);
begin
  editTexte.Text := editTexte.Text + '%s %l %a %p';
end;

procedure TdlgTextObject.Button1Click(Sender: TObject);
var
  EWE: TCaption;
  BP: TBaseStation;
begin
  EWE := Trim(editIDBaseStation.Text);

  if (not FDocumentDessin.FindBasePoint(EWE, BP)) then
  begin
    ShowMessagefmt(rsIDSTATION_NOT_FOUND, [EWE]);
    exit;
  end;
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  editIDBaseStation.Text       := BP.IDStation;
  {$ELSE}
  editIDBaseStation.Text       := GetToporobotIDStationAsString(BP);
  {$ENDIF TIDBASEPOINT_AS_TEXT}
end;

procedure TdlgTextObject.Button2Click(Sender: TObject);
begin
  ShowMessage(rsDLG_TXT_HINT);
end;

procedure TdlgTextObject.Button3Click(Sender: TObject);
begin
  editTexte.Text := editTexte.Text + '%s';
end;



procedure TdlgTextObject.btnApplyClick(Sender: TObject);
var
  EWE: TTextObject;
begin
  EWE := GetTextObjectFromForm();
  case FModeEdition of
    medCREATION : FDocumentDessin.AddTexte(EWE, True);
    medMODIF    : FDocumentDessin.PutTexte(FCurrentIdxTexte, EWE);
  end;
  // si on était en mode création, on bascule en mode édition afin de pouvoir modifier sans fermer la boite de dialogue
  if (FModeEdition = medCREATION) then
  begin
    FModeEdition := medMODIF;
    FCurrentIdxTexte := FDocumentDessin.GetNbTextes() - 1;
    lbIdxTextObj.Caption := Format('%d', [FCurrentIdxTexte]);
  end;
  if (assigned(FProcRepercute)) then FProcRepercute();
end;

procedure TdlgTextObject.cmbPresetsChange(Sender: TObject);
begin
  editTexte.Text := cmbPresets.Text;
end;

procedure TdlgTextObject.btnPresetStationLongAzPente1Click(Sender: TObject);
begin
  editTexte.Text := editTexte.Text + ' %z';
end;

procedure TdlgTextObject.BitBtn1Click(Sender: TObject);
begin
  self.Close;
end;

function TdlgTextObject.GetDataFromForm(out SB: TTextObject): boolean;
var
  EWE: TCaption;
  BP: TBaseStation;
begin
  result := false;
  EWE := Trim(editIDBaseStation.Text);
  if (not FDocumentDessin.FindBasePoint(EWE, BP)) then
  begin
    ShowMessagefmt(rsIDSTATION_NOT_FOUND, [EWE]);
    exit;
  end;
  SB.IDBaseSt     := BP.IDStation;
  SB.IDGroupe     := FDocumentDessin.GetIndexGroupeByInternalIdx(cmbGroupes.ItemIndex);
  SB.Text         := Trim(editTexte.Text);
  SB.IDStyleTexte := TNatureObjetTexte(cmbStyles.ItemIndex);
  SB.Offset.X     := editOffsetX.Value;
  SB.Offset.Y     := editOffsetY.Value;
  SB.Offset.Z     := editOffsetZ.Value;
  SB.Alignment    := GetAlignement;
  SB.LastModified := now();
  SB.MaxLength    := Length(SB.Text);
  result := True;
end;

function TdlgTextObject.GetAlignement: integer;
var
  i: Integer;
begin
  Result := 0;
  if (rbLI1.Checked) then result := 1;
  if (rbLI2.Checked) then result := 2;
  if (rbLI3.Checked) then result := 3;
  if (rbLI4.Checked) then result := 4;
  if (rbLI5.Checked) then result := 5;
  if (rbLI6.Checked) then result := 6;
  if (rbLI7.Checked) then result := 7;
  if (rbLI8.Checked) then result := 8;
  if (rbLI9.Checked) then result := 9;
end;

procedure TdlgTextObject.SetBtnAlignement(const A: byte);
var
  i: Integer;
  C: TComponent;
begin
  rbLI1.Checked := False;
  rbLI2.Checked := False;
  rbLI3.Checked := False;
  rbLI4.Checked := False;
  rbLI5.Checked := False;
  rbLI6.Checked := False;
  rbLI7.Checked := False;
  rbLI8.Checked := False;
  rbLI9.Checked := False;
  case A of
    0:;
    1: rbLI1.Checked := True;
    2: rbLI2.Checked := True;
    3: rbLI3.Checked := True;
    4: rbLI4.Checked := True;
    5: rbLI5.Checked := True;
    6: rbLI6.Checked := True;
    7: rbLI7.Checked := True;
    8: rbLI8.Checked := True;
    9: rbLI9.Checked := True;
  end;
end;




function TdlgTextObject.GetTextObjectFromForm(): TTextObject;
begin
  GetDataFromForm(FObjetTexte);
  Result := FObjetTexte;
end;

end.


