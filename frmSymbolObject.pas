// 02/02/2014: Callback pour répercussion des modifs dans l'affichage du plan
unit frmSymbolObject;

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GHCD_Types,
  GeneralFunctions,
  UnitDocDessin,
  CallDialogsGHCaveDraw,
  Classes, SysUtils, math,
  FileUtil, curredit, Forms,
  Controls, Graphics, Dialogs, Buttons, StdCtrls, EditBtn;

type

  { TdlgSymbolObject }

  TdlgSymbolObject = class(TForm)
    btnClose: TBitBtn;
    btnApply: TBitBtn;
    Button1: TButton;
    chkPhotoDisplayed: TCheckBox;
    cmbGroupes: TComboBox;
    cmbTypeObjet: TComboBox;
    editIDBaseStation: TEdit;
    editTag: TCurrencyEdit;
    editAngleRot: TCurrencyEdit;
    editOffsetX: TCurrencyEdit;
    editScaleX: TCurrencyEdit;
    editScaleY: TCurrencyEdit;
    editOffsetY: TCurrencyEdit;
    editTagTexte: TFileNameEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbAngleRot: TLabel;
    lbDimensions: TLabel;
    lbTagTexte: TLabel;
    lbCoordsPointInsertion: TStaticText;
    lbOffset: TLabel;
    lbGroupes: TLabel;
    lbBaseStation: TLabel;
    lbCoordsBaseStation: TStaticText;
    lbTypeObjet: TLabel;
    lbIdxCurrentSymbole: TStaticText;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cmbTypeObjetChange(Sender: TObject);
  private
    { private declarations }
    FDocumentDessin: TDocumentDessin;
    FModeEdition   : TModeEdition;
     FObjetPonctuel : TSymbole;
    FAbsolutePos   : TPoint3Df;
    FCurrentIdxSymbole: integer;
    // callback de répercussion des modifs
    FProcRepercuteModifs: TProcRefreshVues;
    function  GetDataFromForm(out SB: TSymbole): boolean;
    procedure SetEditLHByAngle(const A: double);
  public
    { public declarations }
    procedure InitCaptions;
    procedure Initialiser(const MyDocuDessin: TDocumentDessin;
                          const QIdxSymbole: integer;
                          const ModeEdition : TModeEdition;
                          const CurrgroupeIdx  : TIDGroupeEntites;
                          const QTypeDeSymbole: TNatureObjetSymbole;
                          const OP: TSymbole;

                          const QAbsolutePos: TPoint3Df;
                          const ProcRepercute: TProcRefreshVues);

    function GetSymbolObject: TSymbole;

  end;

var
  dlgTextObject: TdlgSymbolObject;

implementation
uses DGCDummyUnit;

{$R *.lfm}
{ TdlgSymbolObject }
procedure TdlgSymbolObject.InitCaptions;
var
  i: Integer;
begin
  self.Caption              := rsDLG_SYM_TITRE;
  lbGroupes.Caption         := rsDLG_SYM_GROUPES;
  lbTagTexte.Caption        := rsDLG_SYM_TAGTEXTE;
  lbBaseStation.Caption     := rsDLG_SYM_IDBASESTATION;
  lbOffset.Caption          := rsDLG_SYM_OFFSET;
  lbTypeObjet.Caption       := rsDLG_SYM_TYPEOBJET;
  chkPhotoDisplayed.Caption := rsDLG_SYM_PHOTODISPLAYED;
  lbDimensions.Caption      := rsDLG_SYM_DIMENSIONS;
  lbAngleRot.Caption        := rsDLG_SYM_ANGLEROT;
  // combo types d'objets
  RemplirComboTypesSymboles(cmbTypeObjet, 0);
  (*
  cmbTypeObjet.Clear;
  for i:=0 to high(ListeNatureSymboles) do
  begin
    cmbTypeObjet.Items.add(ListeNatureSymboles[i]);
  end;
  //*)
end;
procedure TdlgSymbolObject.Initialiser(const MyDocuDessin: TDocumentDessin;
                                       const QIdxSymbole: integer;
                                       const ModeEdition: TModeEdition;
                                       const CurrGroupeIdx  : TIDGroupeEntites;
                                       const QTypeDeSymbole: TNatureObjetSymbole;
                                       const OP: TSymbole;
                                       const QAbsolutePos: TPoint3Df;
                                       const ProcRepercute: TProcRefreshVues);
var
  GRP: TGroupeEntites;
  i: Integer;
  EWE: TBaseStation;
  zdar: Double;
begin
  FAbsolutePos    := QAbsolutePos;
  FProcRepercuteModifs := ProcRepercute;
  lbCoordsPointInsertion.Caption:= Format('X = %.2f, Y = %.2f', [FAbsolutePos.X, FAbsolutePos.Y]);
  self.Caption    := rsDLG_TXT_TITRE;

  InitCaptions;
  FModeEdition    := ModeEdition;
  FDocumentDessin := MyDocuDessin;
  // index du symbole courant
  FModeEdition := ModeEdition;
  FCurrentIdxSymbole := QIdxSymbole;
  if (FModeEdition = medMODIF) then
    FCurrentIdxSymbole := QIdxSymbole
  else
    FCurrentIdxSymbole := -1;
  FObjetPonctuel  := OP;
  lbIdxCurrentSymbole.Caption := Format('ID =%d', [FCurrentIdxSymbole]);



  // groupes
  cmbGroupes.Clear;
  for i := 0 to FDocumentDessin.GetNbGroupes - 1 do
  begin
    GRP := FDocumentDessin.GetGroupe(i);
    cmbGroupes.Items.Add(Format('%d - %s', [GRP.IDGroupeEntites, GRP.NomGroupe]));
  end;
  case FModeEdition of
    medCREATION:
      begin
        cmbGroupes.ItemIndex       := FDocumentDessin.GetInternalIdxGroupe(CurrGroupeIdx);
        cmbTypeObjet.ItemIndex     := Ord(QTypeDeSymbole);
        editTagTexte.Text          := '';
        chkPhotoDisplayed.Checked  := false;
        // en mode création, calcul de l'offset de la station de base
        EWE := FDocumentDessin.GetNearBasepoint(QAbsolutePos.X, QAbsolutePos.Y, EWE, False);
        {$IFDEF TIDBASEPOINT_AS_TEXT}
        editIDBaseStation.Text      := EWE.IDStation;
        {$ELSE}
        editIDBaseStation.Text      := EWE.GetToporobotIDStationAsString();

        {$ENDIF TIDBASEPOINT_AS_TEXT}
        editOffsetX.Value    := QAbsolutePos.X - EWE.PosStation.X;
        editOffsetY.Value    := QAbsolutePos.Y - EWE.PosStation.Y;

        editScaleX.Value     := 1.00;
        editScaleY.Value     := 1.00;
        editAngleRot.Value   := 0.00;

        editTag.AsInteger    := 0;

        // proposition d'angle pour les entrées
        if (QTypeDeSymbole = nosENTREE) then
        begin
          zdar := ArcTan2InDegrees(editOffsetX.Value, editOffsetY.Value);
          editAngleRot.Value := zdar;
        end;
      end;
    medMODIF:
      begin // point topo de base et décalage
        if (FDocumentDessin.GetBasePointByIndex(FObjetPonctuel.IDBaseStation, EWE)) then
        begin
          {$IFDEF TIDBASEPOINT_AS_TEXT}
          editIDBaseStation.Text       := EWE.IDStation;
          {$ELSE}
          editIDBaseStation.Text      := EWE.GetToporobotIDStationAsString();
          {$ENDIF TIDBASEPOINT_AS_TEXT}
          cmbGroupes.ItemIndex       := FDocumentDessin.GetInternalIdxGroupe(FObjetPonctuel.IDGroupe);
          cmbTypeObjet.ItemIndex     := Ord(FObjetPonctuel.TypeObject);
          editTagTexte.Text          := AnsiToUtf8(FObjetPonctuel.TagTexte);
          chkPhotoDisplayed.Checked  := FObjetPonctuel.PhotoDisplayed;
          editOffsetX.Value          := FObjetPonctuel.Offset.X;
          editOffsetY.Value          := FObjetPonctuel.Offset.Y;
          editScaleX.Value           := FObjetPonctuel.ScaleX;
          editScaleY.Value           := FObjetPonctuel.ScaleY;
          editAngleRot.Value         := FObjetPonctuel.AngleRot;
          editTag.AsInteger          := FObjetPonctuel.UnTag;
          lbCoordsBaseStation.Caption:= format('X = %.2f, Y = %.2f', [EWE.PosStation.X, EWE.PosStation.Y]);
        end;
      end;
  end;
end;

procedure TdlgSymbolObject.cmbTypeObjetChange(Sender: TObject);
var
  A: float;
begin
  // presets de dimensions d'objets
  case TNatureObjetSymbole(cmbTypeObjet.ItemIndex) of
    nosCOLONNES:
      begin
        editScaleX.Value := 0.50;
        editScaleY.Value := 1.50;
      end;
    nosSTALACTITES, nosSTALAGMITES:
      begin
        editScaleX.Value := 0.50;
        editScaleY.Value := 1.00;
      end;
    nosPOINT_FIXE:
      begin
        editScaleX.Value := 0.40;
        editScaleY.Value := 0.40;
      end;
    nosPOINT_TOPO:
      begin
        editScaleX.Value := 0.30;
        editScaleY.Value := 0.30;
      end;
    nosCRISTAUX:
      begin
        editScaleX.Value := 0.30;
        editScaleY.Value := 0.30;
      end;
    nosFISTULEUSE:
      begin
        editScaleX.Value := 1.50;
        editScaleY.Value := 0.75;
      end;
    nosEXCENTRIQUES:
      begin
        editScaleX.Value := 0.35;
        editScaleY.Value := 0.20;
      end;
    nosENTREE:
      begin
        editScaleX.Value := 1.20;
        editScaleY.Value := 0.50;
        // proposition d'angle de rotation
        A := ArcTan2InDegrees(editOffsetX.Value, editOffsetY.Value);
        editAngleRot.Value := A;
      end;
    nosCONCRETION_PAROI:
      begin
        editScaleX.Value := 1.60;
        editScaleY.Value := 0.60;
      end;
    else
      begin
        editScaleX.Value := 1.00;
        editScaleY.Value := 1.00;
      end;
  end;
end;
procedure TdlgSymbolObject.btnApplyClick(Sender: TObject);
var
  EWE: TSymbole;
begin
  if (GetDataFromForm(EWE)) then
  begin
    case FModeEdition of
      medCREATION : FDocumentDessin.AddSymbole(EWE, True);
      medMODIF    : FDocumentDessin.PutSymbole(FCurrentIdxSymbole, EWE);
    end;
    // si on était en mode création, on bascule en mode édition afin de pouvoir modifier sans fermer la boite de dialogue
    if (FModeEdition = medCREATION) then
    begin
      FModeEdition := medMODIF;
      FCurrentIdxSymbole := FDocumentDessin.GetNbSymboles() - 1;
      lbIdxCurrentSymbole.Caption := Format('%d', [FCurrentIdxSymbole]);
    end;
    if (assigned(FProcRepercuteModifs)) then FProcRepercuteModifs();
  end
  else
  begin

  end;
end;

procedure TdlgSymbolObject.btnCloseClick(Sender: TObject);
begin
  self.Close;
end;

procedure TdlgSymbolObject.Button1Click(Sender: TObject);
var
  EWE: string;
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
  editIDBaseStation.Text      := BP.GetToporobotIDStationAsString();
  {$ENDIF TIDBASEPOINT_AS_TEXT}
end;

function TdlgSymbolObject.GetDataFromForm(out SB: TSymbole): boolean;
var
  BP: TBaseStation;
  EWE: TCaption;
begin
  result := false;
  EWE := Trim(editIDBaseStation.Text);
  if (not FDocumentDessin.FindBasePoint(EWE, BP)) then
  begin
    ShowMessagefmt(rsIDSTATION_NOT_FOUND, [EWE]);
    exit;
  end;
  SB.IDGroupe      := FDocumentDessin.GetIndexGroupeByInternalIdx(cmbGroupes.ItemIndex);
  SB.IDBaseStation := BP.IDStation;

  SB.TypeObject    := TNatureObjetSymbole(cmbTypeObjet.ItemIndex);
  SB.TagTexte      := editTagTexte.Text;
  SB.Offset.X      := editOffsetX.Value;
  SB.Offset.Y      := editOffsetY.Value;
  SB.Offset.Z      := 0.00;
  SB.AngleRot      := editAngleRot.Value;
  SB.ScaleX        := editScaleX.Value;
  SB.ScaleY        := editScaleY.Value;
  SB.LastModified  := now();
  SB.MarkToDelete  := false;
  SB.UnTag         := editTag.AsInteger;
  result := True;
end;

// pour les symboles Faille uniquement: définit L et H
// Angle dans le sens horaire
procedure TdlgSymbolObject.SetEditLHByAngle(const A: double);
var
  Ang : float;
  WU  : float;
  EWE : TNatureObjetSymbole;
  Q69: Boolean;
begin
  EWE := TNatureObjetSymbole(cmbTypeObjet.ItemIndex);
  Q69 := (EWE = nosFRACTURES) OR
         (EWE = nosZEFF);
  if (Q69) then
  begin
    Ang := degtorad(90 - A);
    WU  := hypot(editScaleX.Value, editScaleY.Value);
    editScaleX.Value := WU * cos(Ang);
    editScaleY.Value := WU * sin(Ang);
  end;
end;

function TdlgSymbolObject.GetSymbolObject: TSymbole;
begin
  GetDataFromForm(FObjetPonctuel);
  Result := FObjetPonctuel;
end;

end.


