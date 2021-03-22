unit frmGroupes;

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GHCD_Types,
  GeneralFunctions,
  CallDialogsGHCaveDraw,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, Graphics,
  Dialogs, Buttons, StdCtrls;

type

  { TdlgGroupe }

  TdlgGroupe = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    chkDoSetGrpAsCurrent: TCheckBox;
    chkDecalageActif: TCheckBox;
    chkVisible: TCheckBox;
    editFiltres: TEdit;
    editZOrder: TCurrencyEdit;
    editDecalageX: TCurrencyEdit;
    editDecalageY: TCurrencyEdit;
    editDecalageZ: TCurrencyEdit;
    editIDGroupe: TCurrencyEdit;
    editNomGroupe: TEdit;
    Label1: TLabel;
    lbNomGroupe: TLabel;
    btnCouleurGroupe: TStaticText;
    lbAltitudeGroupe: TLabel;
    lbFiltres: TLabel;
    lbDateLastModification: TLabel;
    lbLastGrpModif: TStaticText;
    lbDecalageGroupe: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure btnCouleurGroupeClick(Sender: TObject);
  private
    { private declarations }
    FGroupe: TGroupeEntites;
    FModeEdition : TModeEdition;
  public
    { public declarations }
    procedure Initialise(const G: TGroupeEntites; const ME: TModeEdition; const QFiltres: string);
    function  GetGroupe: TGroupeEntites;
    function  GetDoSetGrpAsCurrent(): boolean;
  end;

var
  dlgGroupe: TdlgGroupe;

implementation
uses DGCDummyUnit;

{$R *.lfm}

{ TdlgGroupe }


procedure TdlgGroupe.btnCouleurGroupeClick(Sender: TObject);
begin
  btnCouleurGroupe.Color := ChooseColor(btnCouleurGroupe.Color);
end;

function TdlgGroupe.GetDoSetGrpAsCurrent: boolean;
begin
  Result := chkDoSetGrpAsCurrent.Checked;
end;

procedure TdlgGroupe.BitBtn1Click(Sender: TObject);
begin

end;

procedure TdlgGroupe.Initialise(const G: TGroupeEntites;
                                const ME: TModeEdition;
                                const QFiltres: string);
  procedure MiouMiou(const Miou: boolean);
  begin
    editZOrder.Visible           := Miou;
    lbAltitudeGroupe.Visible     := Miou;
    chkDoSetGrpAsCurrent.Visible := Miou;
	end;
begin
  FGroupe := G;
  FModeEdition := ME;
  //chkLocked.Caption            := GetResourceString(rsDLG_GRP_CHK_LOCKED);
  chkVisible.Caption           := GetResourceString(rsDLG_GRP_CHK_VISIBLE);
  lbDecalageGroupe.Caption     := GetResourceString(rsDLG_GRP_LB_DECALAGE);
  lbNomGroupe.Caption          := GetResourceString(rsDLG_GRP_LB_NOM_GROUPE);
  chkDecalageActif.Caption     := GetResourceString(rsDLG_GRP_CHK_DECALAGE_ACTIF);
  lbFiltres.Caption            := GetResourceString(rsDLG_GRP_FILTRES);
  lbAltitudeGroupe.Caption     := GetResourceString(rsDLG_GRP_ALTITUDE);
  chkDoSetGrpAsCurrent.Caption := GetResourceString(rsDLG_GRP_DO_SET_CURR_GRP);
  // en général, quand on crée un groupe, c'est pour l'utiliser immédiatement
  // en mode création, on arme cette chkbox
  chkDoSetGrpAsCurrent.Checked := (ME = medCREATION);

  editIDGroupe.Enabled   := False; // ID de groupe non éditable
  lbDateLastModification.Caption:= GetResourceString(rsDLG_GRP_DATE_LAST_MODIF);
  case FModeEdition of
    medCREATION:
      begin
        self.Caption := GetResourceString(rsDLG_GRP_CAPTION_CREATION);
        editIDGroupe.AsInteger := -1;
        editIDGroupe.Visible   := False; // ID de groupe invisible en mode création
        editNomGroupe.Text  := rsMISC_NOUVEAU_GROUPE;
        editDecalageX.Value := 0.00;
        editDecalageY.Value := 0.00;
        editDecalageZ.Value := 0.00;
        chkVisible.Checked  := True;
        //chkLocked.Checked   := False;
        chkDecalageActif.Checked := true;
        editFiltres.Text    := QFiltres;
        btnCouleurGroupe.Color := clGray;
        lbLastGrpModif.Caption := DateTimeToStr(Now());
        editZOrder.Value       := 0.00;
        MiouMiou(false);

      end;
    medMODIF:
      begin
        self.Caption := GetResourceString(rsDLG_GRP_CAPTION_MODIF);
        editIDGroupe.AsInteger := FGroupe.IDGroupeEntites;
        editNomGroupe.Text     := FGroupe.NomGroupe;
        editDecalageX.Value    := FGroupe.Decalage.X;
        editDecalageY.Value    := FGroupe.Decalage.Y;
        editDecalageZ.Value    := FGroupe.Decalage.Z;
        //chkLocked.Checked         := FGroupe.Locked;
        chkVisible.Checked        := FGroupe.Visible;
        chkDecalageActif.Checked  := FGroupe.DecalageActif;
        btnCouleurGroupe.Color := FGroupe.CouleurGroupe;
        lbLastGrpModif.Caption := DateTimeToStr(FGroupe.DateLastModif);
        editZOrder.Value       := FGroupe.ZOrder;
        editFiltres.Text       := FGroupe.Filtres;
        MiouMiou(True);
      end;
  end;
end;

function TdlgGroupe.GetGroupe: TGroupeEntites;
begin
  FGroupe.IDGroupeEntites := editIDGroupe.AsInteger;
  //FGroupe.IDSuperGroupe   := 0; // DONE: Implémenter supergroupes
  FGroupe.NomGroupe       := Trim(editNomGroupe.Text);
  FGroupe.Decalage.X      := editDecalageX.Value;
  FGroupe.Decalage.Y      := editDecalageY.Value;
  FGroupe.Decalage.Z      := editDecalageZ.Value;
  //FGroupe.Locked          := chkLocked.Checked;
  FGroupe.Visible         := chkVisible.Checked;
  FGroupe.DecalageActif   := chkDecalageActif.Checked;
  FGroupe.CouleurGroupe   := btnCouleurGroupe.Color;
  if (FModeEdition = medCREATION) then FGroupe.DateLastModif := Now();
  FGroupe.ZOrder          := editZOrder.Value;
  FGroupe.Filtres         := Trim(editFiltres.Text);
  Result := FGroupe;
end;

end.

