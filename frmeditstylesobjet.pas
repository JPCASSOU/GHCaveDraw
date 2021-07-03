unit frmEditStylesObjet;
// Editeur de styles d'objets
// Statut: OK
// 02/02/2014: Possibilité d'ajouter des styles + sauvegrd/restauration
//             Fonction de rappel permettant d'actualiser le rendu des styles sur le plan
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GHCD_Types,
  GeneralFunctions,
  UnitDocDessin,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, Graphics,
  Dialogs, Buttons, StdCtrls, ComCtrls, Spin, ExtCtrls, CallDialogsGHCaveDraw;

type

  { TdlgEditStylesObjets }

  TdlgEditStylesObjets = class(TForm)
    BitBtn1: TBitBtn;
    btnAddStyleCourbe: TBitBtn;
    btnAddStyleLigne: TBitBtn;
    btnAddStylePolygone: TBitBtn;
    btnAddStyleTexte: TBitBtn;
    btnOpenStyles: TBitBtn;
    btnSaveStyles: TBitBtn;
    btnRestoreStylesLignes: TBitBtn;
    btnApplyStylesCourbes: TBitBtn;
    btnApplyStylesLignes: TBitBtn;
    btnApplyStylesPolygones: TBitBtn;
    btnApplyStylesTextes: TBitBtn;
    btnCouleurCourbe: TStaticText;
    btnCouleurLigne: TStaticText;
    btnCouleurPolygone: TStaticText;
    btnRestoreStylesCourbes: TBitBtn;
    btnRestoreStylesLignes1: TBitBtn;
    btnRestoreStylesLignes2: TBitBtn;
    btnStyleTexteAttrs: TStaticText;
    cmbTypesBarbules: TComboBox;
    cmbTypesStylesSheet: TComboBox;
    editSeuilVisibiliteTexte: TCurrencyEdit;
    editLigneWidth: TSpinEdit;
    editPrintLigneWidth: TFloatSpinEdit;
    editSeuilVisibiliteCourbe: TCurrencyEdit;
    editSeuilVisibiliteLigne: TCurrencyEdit;
    editSeuilVisibilitePolygone: TCurrencyEdit;
    editStyTxtFontPRNSize: TCurrencyEdit;
    editDescStyleCourbe: TEdit;
    editDescStyleLigne: TEdit;
    editDescStylePolygone: TEdit;
    editDescStyleTexte: TEdit;
    editSVGStyleCourbe: TEdit;
    editLongBarbules: TCurrencyEdit;
    editSVGStyleLigne: TEdit;
    editSVGStylePolygone: TEdit;
    editSVGStyleTexte: TEdit;
    editPrintCourbeWidth: TFloatSpinEdit;
    grbxStyleCourbe: TGroupBox;
    grbxStyleLigne: TGroupBox;
    grbxStylePolygone: TGroupBox;
    grbxStyleTexte: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lbSeuilTexteHint: TLabel;
    lbSeuilCourbeHint: TLabel;
    lbSeuilLigneHint: TLabel;
    lbSeuilPolygoneHint: TLabel;
    lbSeuilVisibiliteTexte: TLabel;
    lbSeuilVisibiliteCourbe: TLabel;
    lbSeuilVisibiliteLigne: TLabel;
    lbSeuilVisibilitePolygone: TLabel;
    lbTexteAttributs: TLabel;
    lbTexteHauteurPrn: TLabel;
    lbTailleBarbules: TLabel;
    lbTypeBarbules: TLabel;
    lbLigneCouleur: TLabel;
    lbCourbeCouleur: TLabel;
    lbLigneLargeurEcran: TLabel;
    lbCourbeLargeurPrn: TLabel;
    lbCourbeLargeurEcran: TLabel;
    lbLigneLargeurPrn: TLabel;
    lbDescStyleCourbe: TLabel;
    lbDescStyleTexte: TLabel;
    lbDescStylePolygone: TLabel;
    lbDescStyleLigne: TLabel;
    lbSVGStyleCourbe: TLabel;
    lbSVGStyleTexte: TLabel;
    lbSVGStylePolygone: TLabel;
    lbSVGStyleLigne: TLabel;
    lsbStylesCourbe: TListBox;
    lsbStylesLignes: TListBox;
    lsbStylesTextes: TListBox;
    lsbStylesPolygones: TListBox;
    PageControl1: TPageControl;
    editCourbeWidth: TSpinEdit;
    pnlFillPolygone: TPanel;
    tabshtLignes: TTabSheet;
    tabshtPolygones: TTabSheet;
    tabshtTextes: TTabSheet;
    tabshtCourbes: TTabSheet;
    procedure btnAddStyleCourbeClick(Sender: TObject);
    procedure btnAddStyleLigneClick(Sender: TObject);
    procedure btnAddStylePolygoneClick(Sender: TObject);
    procedure btnAddStyleTexteClick(Sender: TObject);
    procedure btnApplyStylesCourbesClick(Sender: TObject);
    procedure btnApplyStylesLignesClick(Sender: TObject);
    procedure btnApplyStylesPolygonesClick(Sender: TObject);
    procedure btnApplyStylesTextesClick(Sender: TObject);
    procedure btnCouleurLigneClick(Sender: TObject);
    procedure btnCouleurPolygoneClick(Sender: TObject);
    procedure btnOpenStylesClick(Sender: TObject);
    procedure btnRestoreStylesCourbesClick(Sender: TObject);
    procedure btnRestoreStylesLignes1Click(Sender: TObject);
    procedure btnRestoreStylesLignes2Click(Sender: TObject);
    procedure btnRestoreStylesLignesClick(Sender: TObject);
    procedure btnSaveStylesClick(Sender: TObject);
    procedure btnStyleTexteAttrsClick(Sender: TObject);
    procedure btnCouleurCourbeClick(Sender: TObject);
    procedure lsbStylesCourbeClick(Sender: TObject);
    procedure lsbStylesLignesClick(Sender: TObject);
    procedure lsbStylesPolygonesClick(Sender: TObject);
    procedure lsbStylesTextesClick(Sender: TObject);

  private
    { private declarations }
    FDocumentDessin: TDocumentDessin;
    FStyleCourbe   : TStyleCourbe;
    FStyleLigne    : TStyleLigne;
    FStylePolygone : TStylePolygone;
    FStyleTexte    : TStyleTexte;

    // fonction de rappel pour répercussion du style
    FProcRefreshStyles: TProcRefreshStyles;
    procedure RefreshListeStylesCourbes(const Idx: integer);
    procedure RefreshListeStylesLignes(const Idx: integer);
    procedure RefreshListeStylesPolygones(const Idx: integer);
    procedure RefreshListeStylesSymboles(const Idx: integer);
    procedure RefreshListeStylesTextes(const Idx: integer);


  public
    { public declarations }
    procedure Initialise(const MD: TDocumentDessin; const PF: TProcRefreshStyles);
    function  GetAStyleCourbe: TStyleCourbe;
    procedure PutAStyleCourbeInForm(const MS: TStyleCourbe);
    function  GetAStyleLigne: TStyleLigne;
    procedure PutAStyleLigneInForm(const MS: TStyleLigne);
    function  GetAStylePolygone: TStylePolygone;
    procedure PutAStylePolygoneInForm(const MS: TStylePolygone);
    function  GetAStyleTexte: TStyleTexte;
    procedure PutAStyleTexteInForm(const MS: TStyleTexte);

  end;

var
  dlgEditStylesObjets: TdlgEditStylesObjets;

implementation

{$R *.lfm}

{ TdlgEditStylesObjets }
const FACT_ECHELLE_STD = 10000;




procedure TdlgEditStylesObjets.Initialise(const MD: TDocumentDessin; const PF: TProcRefreshStyles);
var
  i: Integer;
  procedure Toto666(const S: string); inline;
  begin
    cmbTypesStylesSheet.Items.Add(GetResourceString(S));
  end;
begin
  // titre
  self.Caption := rsDLG_STYLES_TITRE;
  // type de feuilles de styles
  cmbTypesStylesSheet.Clear;
  Toto666(rsDLG_STYLES_TYPES_STYLESHEET_STD);
  Toto666(rsDLG_STYLES_TYPES_STYLESHEET_01);
  Toto666(rsDLG_STYLES_TYPES_STYLESHEET_02);
  cmbTypesStylesSheet.ItemIndex := 0;
  // doc de dessin
  FDocumentDessin := MD;
  // fonction de rappel pour actualisation
  FProcRefreshStyles := PF;
  // combos et captions
  cmbTypesBarbules.Clear;
  for i := 0 to High(ListeTypesBarbules) do
    cmbTypesBarbules.Items.Add(ListeTypesBarbules[i]);
  tabshtCourbes.Caption          := rsDLG_STYLES_TAB_STYLE_COURBES;
    grbxStyleCourbe.Caption      := '';
    lbDescStyleCourbe.Caption    := rsDLG_STYLES_LB_DESC_STYLE_OBJET;
    lbSVGStyleCourbe.Caption     := rsDLG_STYLES_LB_SVG_STYLE_OBJET;
    lbSeuilVisibiliteCourbe.Caption := rsDLG_STYLES_LB_DESC_SEUIL_VISIBLE;
    lbSeuilCourbeHint.Caption       := rsDLG_STYLES_LB_DESC_SEUIL_HINT;

    lbCourbeCouleur.Caption      := rsDLG_STYLES_LB_COULEUR_OBJET;
    lbCourbeLargeurEcran.Caption := rsDLG_STYLES_LB_LINEWIDTH_OBJET;
    lbCourbeLargeurPrn.Caption   := rsDLG_STYLES_LB_PRNLINEWIDTH_OBJET;
    lbTypeBarbules.Caption       := rsDLG_STYLES_LB_BARBULES_TYPE;
    lbTailleBarbules.Caption     := rsDLG_STYLES_LB_BARBULES_TAILLE;

  tabshtLignes.Caption           := rsDLG_STYLES_TAB_STYLE_LIGNES;
    grbxStyleLigne.Caption       := '';
    lbSVGStyleLigne.Caption      := rsDLG_STYLES_LB_DESC_STYLE_OBJET;
    lbDescStyleLigne.Caption     := rsDLG_STYLES_LB_SVG_STYLE_OBJET;
    lbSeuilVisibiliteLigne.Caption := rsDLG_STYLES_LB_DESC_SEUIL_VISIBLE;
    lbSeuilLigneHint.Caption       := rsDLG_STYLES_LB_DESC_SEUIL_HINT;

    lbLigneCouleur.Caption       := rsDLG_STYLES_LB_COULEUR_OBJET;
    lbLigneLargeurEcran.Caption  := rsDLG_STYLES_LB_LINEWIDTH_OBJET;
    lbLigneLargeurPrn.Caption    :=  rsDLG_STYLES_LB_PRNLINEWIDTH_OBJET;
  tabshtPolygones.Caption        := rsDLG_STYLES_TAB_STYLE_POLYGONES;
    grbxStylePolygone.Caption    := '';
    lbSVGStylePolygone.Caption   := rsDLG_STYLES_LB_DESC_STYLE_OBJET;
    lbDescStylePolygone.Caption  := rsDLG_STYLES_LB_SVG_STYLE_OBJET;
    lbSeuilVisibilitePolygone.Caption := rsDLG_STYLES_LB_DESC_SEUIL_VISIBLE;
    lbSeuilPolygoneHint.Caption       := rsDLG_STYLES_LB_DESC_SEUIL_HINT;
  //tabShtSymboles.Caption    := rsDLG_STYLES_TAB_STYLE_SYMBOLES;
  tabshtTextes.Caption           := rsDLG_STYLES_TAB_STYLE_TEXTES;
    grbxStyleTexte.Caption       := '';
    lbSVGStyleTexte.Caption      := rsDLG_STYLES_LB_DESC_STYLE_OBJET;
    lbDescStyleTexte.Caption     := rsDLG_STYLES_LB_SVG_STYLE_OBJET;
    lbSeuilVisibiliteTexte.Caption    := rsDLG_STYLES_LB_DESC_SEUIL_VISIBLE;
    lbSeuilTexteHint.Caption          := rsDLG_STYLES_LB_DESC_SEUIL_HINT;

    lbTexteHauteurPrn.Caption    := rsDLG_STYLES_LB_TEXTE_PRN_HAUTEUR;
    lbTexteAttributs.Caption     := rsDLG_STYLES_LB_TEXTE_ATTRS;
  PageControl1.ActivePageIndex := 0;


  // listes
  RefreshListeStylesCourbes(0);
  RefreshListeStylesLignes(0);
  RefreshListeStylesPolygones(0);
  RefreshListeStylesSymboles(0);
  RefreshListeStylesTextes(0);
end;

procedure TdlgEditStylesObjets.RefreshListeStylesCourbes(const Idx: integer);
var
  i: Integer;
  EWE: TStyleCourbe;
begin
  lsbStylesCourbe.Clear;
  for i := 0 to FDocumentDessin.GetNbStylesCourbe() - 1 do
  begin
    EWE := FDocumentDessin.GetStyleCourbe(i);
    lsbStylesCourbe.Items.Add(EWE.DescStyle);
  end;
  EWE := FDocumentDessin.GetStyleCourbe(Idx);
  PutAStyleCourbeInForm(EWE);
  lsbStylesCourbe.ItemIndex := Idx;
end;
procedure TdlgEditStylesObjets.RefreshListeStylesLignes(const Idx: integer);
var
  i: Integer;
  EWE: TStyleLigne;
begin
  lsbStylesLignes.Clear;
  for i := 0 to FDocumentDessin.GetNbStylesLigne() - 1 do
  begin
    EWE := FDocumentDessin.GetStyleLigne(i);
    lsbStylesLignes.Items.Add(EWE.DescStyle);
  end;
  EWE := FDocumentDessin.GetStyleLigne(Idx);
  PutAStyleLigneInForm(EWE);
  lsbStylesLignes.ItemIndex := Idx;
end;

procedure TdlgEditStylesObjets.RefreshListeStylesPolygones(const Idx: integer);
var
  i: Integer;
  EWE: TStylePolygone;
begin
  lsbStylesPolygones.Clear;
  for i := 0 to FDocumentDessin.GetNbStylesPolygones() - 1 do
  begin
    EWE := FDocumentDessin.GetStylePolygone(i);
    lsbStylesPolygones.Items.Add(EWE.DescStyle);
  end;
  EWE := FDocumentDessin.GetStylePolygone(Idx);
  PutAStylePolygoneInForm(EWE);
  lsbStylesPolygones.ItemIndex := Idx;
end;

procedure TdlgEditStylesObjets.RefreshListeStylesSymboles(const Idx: integer);
begin

end;

procedure TdlgEditStylesObjets.RefreshListeStylesTextes(const Idx: integer);
var
  i: Integer;
  EWE: TStyleTexte;
begin
  lsbStylesTextes.Clear;
  for i := 0 to FDocumentDessin.GetNbStylesTexte() - 1 do
  begin
    EWE := FDocumentDessin.GetStyleTexte(i);
    lsbStylesTextes.Items.Add(EWE.DescStyle);
  end;
  EWE := FDocumentDessin.GetStyleTexte(Idx);
  PutAStyleTexteInForm(EWE);
  lsbStylesTextes.ItemIndex := Idx;
end;

function TdlgEditStylesObjets.GetAStyleCourbe: TStyleCourbe;
begin
  Result := FStyleCourbe;
  Result.NameSVGStyle    := Trim(editSVGStyleCourbe.Text);
  Result.DescStyle       := Trim(editDescStyleCourbe.Text);
  Result.SeuilVisibilite := editSeuilVisibiliteCourbe.Value;

  Result.LineColor       := btnCouleurCourbe.Color;
  result.LineWidth       := editCourbeWidth.Value;
  Result.PrintLineWidth  := editPrintCourbeWidth.Value;
  Result.LongBarbules    := editLongBarbules.Value;
  result.Barbules        := TBarbule(cmbTypesBarbules.ItemIndex);
end;
procedure TdlgEditStylesObjets.PutAStyleCourbeInForm(const MS: TStyleCourbe);
begin
  FStyleCourbe := MS;  // variable pour sauvegrd des propriétés
  editDescStyleCourbe.Text        := FStyleCourbe.DescStyle;
  editSVGStyleCourbe.Text         := FStyleCourbe.NameSVGStyle;
  editSeuilVisibiliteCourbe.Value := FStyleCourbe.SeuilVisibilite;
  btnCouleurCourbe.Color          := FStyleCourbe.LineColor;
  editCourbeWidth.Value           := FStyleCourbe.LineWidth;
  editPrintCourbeWidth.Value      := FStyleCourbe.PrintLineWidth;
  cmbTypesBarbules.ItemIndex      := ord(FStyleCourbe.Barbules);
  editLongBarbules.Value          := FStyleCourbe.LongBarbules;

end;

function TdlgEditStylesObjets.GetAStyleLigne: TStyleLigne;
begin
  Result := FStyleLigne;
  Result.DescStyle           := Trim(editDescStyleLigne.Text);
  Result.NameSVGStyle        := Trim(editSVGStyleLigne.Text);
  Result.SeuilVisibilite     := editSeuilVisibiliteLigne.Value;


  Result.LineColor           := btnCouleurLigne.Color;
  Result.LineWidth           := editLigneWidth.Value;
  Result.PrintLineWidth      := editPrintLigneWidth.Value;
end;

procedure TdlgEditStylesObjets.PutAStyleLigneInForm(const MS: TStyleLigne);
begin
  FStyleLigne := MS;
  editDescStyleLigne.Text        := FStyleLigne.DescStyle;
  editSVGStyleLigne.Text         := FStyleLigne.NameSVGStyle;
  editSeuilVisibiliteLigne.Value := FStyleLigne.SeuilVisibilite;

  btnCouleurLigne.Color      := FStyleLigne.LineColor;
  editLigneWidth.Value       := FStyleLigne.LineWidth;
  editPrintLigneWidth.Value  := FStyleLigne.PrintLineWidth;
end;

function TdlgEditStylesObjets.GetAStylePolygone: TStylePolygone;
begin
  Result := FStylePolygone;
  Result.DescStyle           := Trim(editDescStylePolygone.Text);
  Result.NameSVGStyle        := Trim(editSVGStylePolygone.Text);
  Result.SeuilVisibilite     := editSeuilVisibilitePolygone.Value;

  Result.LineColor           := btnCouleurLigne.Color;
end;

procedure TdlgEditStylesObjets.PutAStylePolygoneInForm(const MS: TStylePolygone);
begin
  FStylePolygone := MS;
  editDescStylePolygone.Text        := FStylePolygone.DescStyle;
  editSVGStylePolygone.Text         := FStylePolygone.NameSVGStyle;
  editSeuilVisibilitePolygone.Value := FStylePolygone.SeuilVisibilite;
  btnCouleurPolygone.Color   := FStylePolygone.FillColor;
  //pnlFillPolygone.Canvas.Brush.Style := FStylePolygone.;
end;

function TdlgEditStylesObjets.GetAStyleTexte: TStyleTexte;
begin
  Result := FStyleTexte;

  Result.DescStyle       := Trim(editDescStyleTexte.Text);
  Result.NameSVGStyle    := Trim(editSVGStyleTexte.Text);
  Result.SeuilVisibilite := editSeuilVisibiliteTexte.Value;

  Result.FontColor       := btnStyleTexteAttrs.Font.Color;
  //Result.FontHeight      := 10; //btnStyleTexteAttrs.Font.Height;
  Result.FontStyle       := btnStyleTexteAttrs.Font.Style;
  Result.FontPrnHeight   := editStyTxtFontPRNSize.Value;
end;

procedure TdlgEditStylesObjets.PutAStyleTexteInForm(const MS: TStyleTexte);
begin
  FStyleTexte := MS;
  editDescStyleTexte.Text          := FStyleTexte.DescStyle;
  editSVGStyleTexte.Text           := FStyleTexte.NameSVGStyle;
  editSeuilVisibiliteTexte.Value   := FStyleTexte.SeuilVisibilite;

  btnStyleTexteAttrs.Font.Color := FStyleTexte.FontColor;
  btnStyleTexteAttrs.Font.Style := FStyleTexte.FontStyle;
  btnStyleTexteAttrs.Font.Height:= 10; //FStyleTexte.FontHeight;
  editStyTxtFontPRNSize.Value   := FStyleTexte.FontPrnHeight;
end;



//------------------------------------------------------------------------------
procedure TdlgEditStylesObjets.btnCouleurCourbeClick(Sender: TObject);
begin
  btnCouleurCourbe.Color := ChooseColor(btnCouleurCourbe.Color);
end;

procedure TdlgEditStylesObjets.btnApplyStylesCourbesClick(Sender: TObject);
var
  Idx: Integer;
  EWE: TStyleCourbe;
begin
  Idx := lsbStylesCourbe.ItemIndex;
  EWE := GetAStyleCourbe();
  FDocumentDessin.PutStyleCourbe(Idx, EWE);
  // contrôle
  //EWE := FDocumentDessin.GetStyleCourbe(Idx);
  //PutAStyleCourbeInForm(EWE);
  RefreshListeStylesCourbes(Idx);
  // callback de répercussion
  if (Assigned(FProcRefreshStyles)) then FProcRefreshStyles();
end;

procedure TdlgEditStylesObjets.btnAddStyleCourbeClick(Sender: TObject);
var
  EWE: TStyleCourbe;
begin
  EWE := GetAStyleCourbe();
  FDocumentDessin.AddStyleCourbe(EWE);
  RefreshListeStylesCourbes(0);
end;

procedure TdlgEditStylesObjets.btnAddStyleLigneClick(Sender: TObject);
var
  EWE: TStyleLigne;
begin
  EWE := GetAStyleLigne();
  FDocumentDessin.AddStyleLigne(EWE);
  RefreshListeStylesLignes(0);
end;

procedure TdlgEditStylesObjets.btnAddStylePolygoneClick(Sender: TObject);
var
  EWE: TStylePolygone;
begin
  EWE := GetAStylePolygone();
  FDocumentDessin.AddStylePolygone(EWE);
  RefreshListeStylesPolygones(0);
end;

procedure TdlgEditStylesObjets.btnAddStyleTexteClick(Sender: TObject);
var
  EWE: TStyleTexte;
begin
  EWE := GetAStyleTexte();
  FDocumentDessin.AddStyleTexte(EWE);
  RefreshListeStylesTextes(0);
end;

procedure TdlgEditStylesObjets.btnApplyStylesLignesClick(Sender: TObject);
var
  Idx: Integer;
  EWE: TStyleLigne;
begin
  Idx := lsbStylesLignes.ItemIndex;
  EWE := GetAStyleLigne();
  FDocumentDessin.PutStyleLigne(Idx, EWE);
  RefreshListeStylesLignes(Idx);
end;

procedure TdlgEditStylesObjets.btnApplyStylesPolygonesClick(Sender: TObject);
var
  Idx: Integer;
  EWE: TStylePolygone;
begin
  Idx := lsbStylesPolygones.ItemIndex;
  EWE := GetAStylePolygone();
  FDocumentDessin.PutStylePolygone(Idx, EWE);
  RefreshListeStylesPolygones(Idx);
end;

procedure TdlgEditStylesObjets.btnApplyStylesTextesClick(Sender: TObject);
var
  Idx: Integer;
  EWE: TStyleTexte;
begin
  Idx := lsbStylesTextes.ItemIndex;
  EWE := GetAStyleTexte();
  FDocumentDessin.PutStyleTexte(Idx, EWE);
  RefreshListeStylesTextes(Idx);
end;

procedure TdlgEditStylesObjets.btnCouleurLigneClick(Sender: TObject);
begin
  btnCouleurLigne.Color := ChooseColor(btnCouleurLigne.Color);
end;

procedure TdlgEditStylesObjets.btnCouleurPolygoneClick(Sender: TObject);
begin
  btnCouleurPolygone.Color := ChooseColor(btnCouleurPolygone.Color);
end;

procedure TdlgEditStylesObjets.btnOpenStylesClick(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
begin
  if (DoDialogOpenFilename('Fichiers de styles (*.sty)|*.sty|Tous (*.*)|*.*', QFileName)) then
  begin
    if (FDocumentDessin.LoadStylesFromFile(QFileName)) then
    begin

    end;
  end;
end;

procedure TdlgEditStylesObjets.btnRestoreStylesCourbesClick(Sender: TObject);
begin
  PutAStyleCourbeInForm(FStyleCourbe);
end;

procedure TdlgEditStylesObjets.btnRestoreStylesLignes1Click(Sender: TObject);
begin
  PutAStylePolygoneInForm(FStylePolygone);
end;

procedure TdlgEditStylesObjets.btnRestoreStylesLignes2Click(Sender: TObject);
begin
  PutAStyleTexteInForm(FStyleTexte);
end;

procedure TdlgEditStylesObjets.btnRestoreStylesLignesClick(Sender: TObject);
begin
  PutAStyleLigneInForm(FStyleLigne);
end;

procedure TdlgEditStylesObjets.btnSaveStylesClick(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
begin
  if (DoDialogSaveFilename('Fichiers de styles (*.sty)|*.sty|Tous (*.*)|*.*', '.sty', QFileName)) then
  begin
    FDocumentDessin.SaveStylesInFile(QFileName);
  end;
end;

procedure TdlgEditStylesObjets.btnStyleTexteAttrsClick(Sender: TObject);
var
  WU: TFontDialog;
begin
  WU      := TFontDialog.Create(Application);
  try
    WU.Font := btnStyleTexteAttrs.Font;
    if (WU.Execute) then btnStyleTexteAttrs.Font := WU.Font;
  finally
    FreeAndNil(WU);//WU.Free;
  end;
end;

procedure TdlgEditStylesObjets.lsbStylesCourbeClick(Sender: TObject);
var
  EWE: TStyleCourbe;
begin
  try
    EWE := FDocumentDessin.GetStyleCourbe(lsbStylesCourbe.ItemIndex);
    PutAStyleCourbeInForm(EWE);
  except
  end;
end;

procedure TdlgEditStylesObjets.lsbStylesLignesClick(Sender: TObject);
var
  EWE: TStyleLigne;
begin
  try
    EWE := FDocumentDessin.GetStyleLigne(lsbStylesLignes.ItemIndex);
    PutAStyleLigneInForm(EWE);
  except
  end;
end;

procedure TdlgEditStylesObjets.lsbStylesPolygonesClick(Sender: TObject);
var
  EWE: TStylePolygone;
begin
  try
    EWE := FDocumentDessin.GetStylePolygone(lsbStylesPolygones.ItemIndex);
    PutAStylePolygoneInForm(EWE);
  except
  end;
end;

procedure TdlgEditStylesObjets.lsbStylesTextesClick(Sender: TObject);
var
  EWE: TStyleTexte;
begin
  try
    EWE := FDocumentDessin.GetStyleTexte(lsbStylesTextes.ItemIndex);
    PutAStyleTexteInForm(EWE);
  except
  end;
end;



end.

