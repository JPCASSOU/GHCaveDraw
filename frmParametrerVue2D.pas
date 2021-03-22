unit frmParametrerVue2D;

{$INCLUDE CompilationParameters.inc}

interface

uses
  SysUtils
   , Classes
   {$IFDEF LANGUAGE_FRENCH}
     , UnitMessages_fr
   {$ENDIF}
   , GHCD_Types
   , GeneralFunctions
   , FileUtil, curredit, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons;

type

  { TdlgParametrerVue2D }

  TdlgParametrerVue2D = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnMainGrdColor: TColorButton;
    btnScrapColorDefault: TColorButton;
    btnSecGrdColor: TColorButton;
    chkDispBBXGroupes: TCheckBox;
    chkDispCenterLine: TCheckBox;
    chkDispEchelleNord: TCheckBox;
    chkDispIDStations: TCheckBox;
    chkDispImages: TCheckBox;
    chkDispMainGrid: TCheckBox;
    chkDispPenteSuperieureA: TCheckBox;
    chkDispPhotos: TCheckBox;
    chkDispQuadrilles: TCheckBox;
    chkDispScraps: TCheckBox;
    chkDispSecGrid: TCheckBox;
    chkDispTexteDebug: TCheckBox;
    chkDispTextes: TCheckBox;
    chkDoDrawScrapsMonochromes: TCheckBox;
    chkListerPOI: TCheckBox;
    cmbTypeQuadrillage: TComboBox;
    btnBackgroundColor: TColorButton;
    editLimiteDispPente: TCurrencyEdit;
    editMainGrdSpacing: TCurrencyEdit;
    editSecGrdSpacing: TCurrencyEdit;
    editTailleCroix: TCurrencyEdit;
    editTailleEchelle: TCurrencyEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    pnlEchelleNord: TPanel;
    pnlDispCenterlines: TPanel;
    pnlScrapsParDefaut: TPanel;
    pnlEchelleNord2: TPanel;
    pnlQuadrillage: TPanel;
    procedure chkDispCenterLineChange(Sender: TObject);
    procedure chkDispEchelleNordChange(Sender: TObject);
    procedure chkDispPenteSuperieureAChange(Sender: TObject);
    procedure chkDispQuadrillesChange(Sender: TObject);
    procedure chkDispScrapsChange(Sender: TObject);
    procedure chkDispTextesChange(Sender: TObject);
  private

    procedure InitCaptions();
    procedure PutParams2DInForm(const FP: TParamsVue2D);

  public
    function Initialiser(const FP: TParamsVue2D): boolean;
    function GetParams2DFromForm(): TParamsVue2D;
  end;

var
  dlgParametrerVue2D: TdlgParametrerVue2D;

implementation

{$R *.lfm}

{ TdlgParametrerVue2D }

procedure TdlgParametrerVue2D.chkDispQuadrillesChange(Sender: TObject);
begin
  pnlQuadrillage.Enabled := chkDispQuadrilles.Checked;
end;

procedure TdlgParametrerVue2D.chkDispScrapsChange(Sender: TObject);
begin
  pnlScrapsParDefaut.Enabled := chkDispScraps.Checked;
end;

procedure TdlgParametrerVue2D.chkDispTextesChange(Sender: TObject);
begin

end;



procedure TdlgParametrerVue2D.chkDispCenterLineChange(Sender: TObject);
begin
  pnlDispCenterlines.Enabled := chkDispCenterLine.Checked;
end;

procedure TdlgParametrerVue2D.chkDispEchelleNordChange(Sender: TObject);
begin
  pnlEchelleNord.Enabled := chkDispEchelleNord.Checked;
end;

procedure TdlgParametrerVue2D.chkDispPenteSuperieureAChange(Sender: TObject);
begin

end;

function TdlgParametrerVue2D.Initialiser(const FP: TParamsVue2D): boolean;
begin
  result := false;
  InitCaptions();
  PutParams2DInForm(FP);
  result := True;
end;

procedure TdlgParametrerVue2D.InitCaptions();
begin
  chkDispMainGrid.Caption         := GetResourceString(rsDLG_ATLAS_CHK_DISP_MAIN_GRID);
  chkDispSecGrid.Caption          := GetResourceString(rsDLG_ATLAS_CHK_DISP_SEC_GRID);
  chkListerPOI.Caption            := GetResourceString(rsDLG_ATLAS_CHK_DISP_LISTE_POI);
  chkDispBBXGroupes.Caption       := GetResourceString(rsCHK_DISP_BOUNDING_BOX);
  chkDispCenterLine.Caption       := GetResourceString(rsCHK_DISP_CENTERLINE);
  chkDispIDStations.Caption       := GetResourceString(rsCHK_DISP_ID_STATIONS);
  chkDispQuadrilles.Caption       := GetResourceString(rsCHK_DISP_QUADRILLES);
  chkDispEchelleNord.Caption      := GetResourceString(rsCHK_DISP_ECHELLE);
  chkDispTextes.Caption           := GetResourceString(rsCHK_DISP_TEXTES);
  chkDispPhotos.Caption           := GetResourceString(rsCHK_DISP_PHOTOS);
  chkDispScraps.Caption           := GetResourceString(rsCHK_DISP_SCRAPS);
  chkDispTexteDebug.Caption       := GetResourceString(rsCHK_DISP_TEXTE_DEBUG);
  chkDispPenteSuperieureA.Caption := GetResourceString(rsCHK_DISP_PENTES_SUP_A);
end;

procedure TdlgParametrerVue2D.PutParams2DInForm(const FP: TParamsVue2D);
begin
  chkDispTexteDebug.Checked     := (tedDEBUG_TEXTS     in FP.ElementsDrawn);
  chkDispEchelleNord.Checked    := (tedECHELLE_NORD    in FP.ElementsDrawn);
  chkDispQuadrilles.Checked     := (tedQUADRILLES      in FP.ElementsDrawn);
  chkDispCenterLine.Checked     := (tedCENTERLINES     in FP.ElementsDrawn);
  chkDispIDStations.Checked     := (tedIDSTATIONS      in FP.ElementsDrawn);
  chkDispTextes.Checked         := (tedTEXTES          in FP.ElementsDrawn);
  chkDispPhotos.Checked         := (tedCADRES_PHOTO    in FP.ElementsDrawn);
  chkDispScraps.Checked         := (tedSCRAPS          in FP.ElementsDrawn);
  chkDispImages.Checked         := (tedIMAGES          in FP.ElementsDrawn);
  chkDispPenteSuperieureA.Checked  := (tedDISP_PENTES  in FP.ElementsDrawn);
  cmbTypeQuadrillage.ItemIndex := Ord(FP.GrdTypeQuadrillage);
  editMainGrdSpacing.Value     := FP.GrdSpcMainGrid;
  editSecGrdSpacing.Value      := FP.GrdSpcSecGrid;
  btnMainGrdColor.ButtonColor  := FP.GrdMainGridColor;
  btnSecGrdColor.ButtonColor   := FP.GrdSecGridColor;
  chkDispMainGrid.Checked      := FP.GrdDispMainGrid;
  chkDispSecGrid.Checked       := FP.GrdDispSecGrid;
  editTailleCroix.Value        := FP.GrdCrossSize;
  btnBackgroundColor.ButtonColor      := FP.BackGroundColor;
  editLimiteDispPente.Value           := FP.PenteLimiteDisp;
  chkDoDrawScrapsMonochromes.Checked  := FP.DoDrawScrapsMonochromes;
  editTailleEchelle.Value             := FP.TailleEchelle;

end;

function TdlgParametrerVue2D.GetParams2DFromForm(): TParamsVue2D;
begin
  if (chkDispTexteDebug.Checked)     then Result.ElementsDrawn := Result.ElementsDrawn + [tedDEBUG_TEXTS]
                                     else Result.ElementsDrawn := Result.ElementsDrawn - [tedDEBUG_TEXTS];
  if (chkDispEchelleNord.Checked)    then Result.ElementsDrawn := Result.ElementsDrawn + [tedECHELLE_NORD]
                                     else Result.ElementsDrawn := Result.ElementsDrawn - [tedECHELLE_NORD];
  if (chkDispQuadrilles.Checked)     then Result.ElementsDrawn := Result.ElementsDrawn + [tedQUADRILLES]
                                     else Result.ElementsDrawn := Result.ElementsDrawn - [tedQUADRILLES];
  if (chkDispCenterLine.Checked)     then Result.ElementsDrawn := Result.ElementsDrawn + [tedCENTERLINES]
                                     else Result.ElementsDrawn := Result.ElementsDrawn - [tedCENTERLINES];
  if (chkDispIDStations.Checked)     then Result.ElementsDrawn := Result.ElementsDrawn + [tedIDSTATIONS]
                                     else Result.ElementsDrawn := Result.ElementsDrawn - [tedIDSTATIONS];
  if (chkDispTextes.Checked)         then Result.ElementsDrawn := Result.ElementsDrawn + [tedTEXTES]
                                     else Result.ElementsDrawn := Result.ElementsDrawn - [tedTEXTES];
  if (chkDispPhotos.Checked)         then Result.ElementsDrawn := Result.ElementsDrawn + [tedCADRES_PHOTO]
                                     else Result.ElementsDrawn := Result.ElementsDrawn - [tedCADRES_PHOTO];
  if (chkDispScraps.Checked)         then Result.ElementsDrawn := Result.ElementsDrawn + [tedSCRAPS]
                                     else Result.ElementsDrawn := Result.ElementsDrawn - [tedSCRAPS];
  if (chkDispImages.Checked)         then Result.ElementsDrawn := Result.ElementsDrawn + [tedIMAGES]
                                     else Result.ElementsDrawn := Result.ElementsDrawn - [tedIMAGES];
  if (chkDispPenteSuperieureA.Checked)  then Result.ElementsDrawn := Result.ElementsDrawn + [tedDISP_PENTES]
                                        else Result.ElementsDrawn := Result.ElementsDrawn - [tedDISP_PENTES];

  Result.BackGroundColor    := btnBackgroundColor.ButtonColor;
  Result.GrdCrossSize       := editTailleCroix.Value;
  Result.GrdDispMainGrid    := chkDispMainGrid.Checked;
  Result.GrdDispSecGrid     := chkDispSecGrid.Checked;
  Result.GrdMainGridColor   := btnMainGrdColor.ButtonColor;
  Result.GrdSecGridColor    := btnSecGrdColor.ButtonColor;
  Result.GrdTypeQuadrillage := TTypeQuadrillage(cmbTypeQuadrillage.ItemIndex);
  Result.GrdSpcMainGrid     := editMainGrdSpacing.Value;
  Result.GrdSpcSecGrid      := editSecGrdSpacing.Value;
  Result.PenteLimiteDisp    := editLimiteDispPente.Value;
  Result.DoDrawScrapsMonochromes := chkDoDrawScrapsMonochromes.Checked;
  Result.TailleEchelle           := editTailleEchelle.Value;
end;

end.

procedure TdlgParametrageAtlas.InitCaptions;
begin
  lbDossierDestination.Caption    := GetResourceString(rsDLG_ATLAS_LB_DOSSIER_DEST);
  // tailles d'écran
  grbxEcrans.Caption              := GetResourceString(rsDLG_ATLAS_GRBX_TAILLE_ECRAN);
  lbEcransUsuels.Caption          := GetResourceString(rsDLG_ATLAS_USUALLY_RESOLS);
  // options de l'atlas
  grbxOptionsAtlas.Caption        := GetResourceString(rsDLG_ATLAS_GRBX_OPTIONS_ATLAS);
  lbDimensionsImageTopo.Caption   := GetResourceString(rsDLG_ATLAS_LB_TAILLE_IMAGE);
  chkCopyright.Caption            := GetResourceString(rsDLG_ATLAS_CHK_DISP_COPYRIGHT);
  editMentionsCopyright.Text      := MENTION_CC_BY_SA;
  // options de tracé
  grbxOptionsDessin.Caption       := GetResourceString(rsDLG_ATLAS_GRBX_OPTIONS_TRACE);

  // checkbox d'affichage


  // notes de copyright
  editMentionsCopyright.Clear;
  editMentionsCopyright.Items.Add('CC-BY-SA JPCassou');
  editMentionsCopyright.Items.Add('CC-BY-SA GRASLourdes');
  editMentionsCopyright.Items.Add('CC-BY-SA GESAEysines');
  editMentionsCopyright.Items.Add('CC0 JPCassou');
  editMentionsCopyright.Items.Add('CC-BY-NC-SA JPCassou');
  editMentionsCopyright.ItemIndex := 0;
end;




chkDispMainGrid.Caption         := GetResourceString(rsDLG_ATLAS_CHK_DISP_MAIN_GRID);
 chkDispSecGrid.Caption          := GetResourceString(rsDLG_ATLAS_CHK_DISP_SEC_GRID);


 // tailles d'écran
 grbxEcrans.Caption              := GetResourceString(rsDLG_ATLAS_GRBX_TAILLE_ECRAN);
 lbEcransUsuels.Caption          := GetResourceString(rsDLG_ATLAS_USUALLY_RESOLS);
 // options de l'atlas
 grbxOptionsAtlas.Caption        := GetResourceString(rsDLG_ATLAS_GRBX_OPTIONS_ATLAS);
 lbDimensionsImageTopo.Caption   := GetResourceString(rsDLG_ATLAS_LB_TAILLE_IMAGE);
 chkListerPOI.Caption            := GetResourceString(rsDLG_ATLAS_CHK_DISP_LISTE_POI);
 chkCopyright.Caption            := GetResourceString(rsDLG_ATLAS_CHK_DISP_COPYRIGHT);
 editMentionsCopyright.Text      := MENTION_CC_BY_SA;
 // options de tracé
 grbxOptionsDessin.Caption       := GetResourceString(rsDLG_ATLAS_GRBX_OPTIONS_TRACE);

 // checkbox d'affichage
 chkDispBBXGroupes.Caption       := GetResourceString(rsCHK_DISP_BOUNDING_BOX);
 chkDispCenterLine.Caption       := GetResourceString(rsCHK_DISP_CENTERLINE);
 chkDispIDStations.Caption       := GetResourceString(rsCHK_DISP_ID_STATIONS);
 chkDispQuadrilles.Caption       := GetResourceString(rsCHK_DISP_QUADRILLES);
 chkDispEchelleNord.Caption      := GetResourceString(rsCHK_DISP_ECHELLE);
 chkDispTextes.Caption           := GetResourceString(rsCHK_DISP_TEXTES);
 chkDispPhotos.Caption           := GetResourceString(rsCHK_DISP_PHOTOS);
 chkDispScraps.Caption           := GetResourceString(rsCHK_DISP_SCRAPS);
 chkDispTexteDebug.Caption       := GetResourceString(rsCHK_DISP_TEXTE_DEBUG);
 chkDispPenteSuperieureA.Caption := GetResourceString(rsCHK_DISP_PENTES_SUP_A);

chkDispTexteDebug.Caption := rsCHK_DRAW_TEXTE_DEBUG;
  chkDispIDStations.Caption := rsCHK_DRAW_ID_STATIONS;
   // checkbox d'affichage
  chkDispBBXGroupes.Caption       := GetResourceString(rsCHK_DISP_BOUNDING_BOX);
  chkDispCenterLine.Caption       := GetResourceString(rsCHK_DISP_CENTERLINE);
  chkDispIDStations.Caption       := GetResourceString(rsCHK_DISP_ID_STATIONS);
  chkDispQuadrilles.Caption       := GetResourceString(rsCHK_DISP_QUADRILLES);
  chkDispEchelleNord.Caption      := GetResourceString(rsCHK_DISP_ECHELLE);
  chkDispTextes.Caption           := GetResourceString(rsCHK_DISP_TEXTES);
  chkDispPhotos.Caption           := GetResourceString(rsCHK_DISP_PHOTOS);
  chkDispScraps.Caption           := GetResourceString(rsCHK_DISP_SCRAPS);
  chkDispTexteDebug.Caption       := GetResourceString(rsCHK_DISP_TEXTE_DEBUG);
  chkDispPenteSuperieureA.Caption := GetResourceString(rsCHK_DISP_PENTES_SUP_A);
