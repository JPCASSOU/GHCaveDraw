unit frmParametrageAtlas;

{$INCLUDE CompilationParameters.inc}

interface

uses
  GHCD_Types
  , dateutils
  , GeneralFunctions
  , CallDialogsGHCaveDraw
  , UnitDocDessin
  {$IFDEF LANGUAGE_FRENCH}
    , UnitMessages_fr
  {$ENDIF}
  ,UnitAtlasHTML
  , Classes, SysUtils, FileUtil, curredit, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, EditBtn, ExtCtrls, ComCtrls;

type TResolEcran = record
  width: integer;
  height: integer;
  description: string;
end;

type

  { TdlgParametrageAtlas }

  TdlgParametrageAtlas = class(TForm)
    btnClose: TBitBtn;
    btnGenererAtlas: TButton;
    chkCopyright: TCheckBox;
    cmbResolutionEcrans: TComboBox;
    editMentionsCopyright: TComboBox;
    editDescResolEcran: TEdit;
    editImgTopoHeightPX: TCurrencyEdit;
    editEtendueTopoX: TCurrencyEdit;
    editMonitorWidth: TCurrencyEdit;
    editDossierDestination: TDirectoryEdit;
    editMonitorHeight: TCurrencyEdit;
    editImgTopoWidthPX: TCurrencyEdit;
    grbxEcrans: TGroupBox;
    grbxOptionsAtlas: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbEtapeTravail: TLabel;
    lbProgress: TLabel;
    lbDimensionsImageTopo: TLabel;
    lbEcransUsuels: TLabel;
    lbDossierDestination: TLabel;
    pnlProgression: TPanel;
    ProgressBar1: TProgressBar;
    procedure btnGenererAtlasClick(Sender: TObject);
    procedure cmbResolutionEcransSelect(Sender: TObject);
    procedure editTailleCroixChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FDocDessin : TDocumentDessin;
    FTexturesPolygones: TTexturesPolygonObject;
    FParamsVue2D: TParamsVue2D;
    FElementsDessines : TELementsDrawn;
    FOutputAtlas: TGenerateurAtlasHTML;
     // variables pour le suivi de la progression
    FProgressionTotal: integer;
    FProgressionDone : integer;
    procedure AfficherProgressionGenerationAtlas(const Etape: string; const Min, Max, Position: integer);
    function DoAbortAtlas: boolean;
    procedure GenererUnAtlas(const QDossierDest: string;
                             const QTailleCarresEnPixelX: integer;
                             const QTailleCarresEnPixelY: integer;
                             const QEtendueCarreEnM: double;
                             const DoDrawCopyright: boolean;
                             const StrCopyright: string;
                             const FP: TParamsVue2D);
    procedure ListerEcransUsuels();
    procedure PutResolEcranInForm(const E: TResolEcran);
    procedure InitCaptions();
    function  MakeTResolEcranFromStr(const s: string): TResolEcran;

  public
    { public declarations }
    procedure Initialiser(const FD: TDocumentDessin;
                          const TP: TTexturesPolygonObject;
                          const PG: TParamsVue2D);
  end;

var
  dlgParametrageAtlas: TdlgParametrageAtlas;

implementation

{$R *.lfm}

{ TdlgParametrageAtlas }


procedure TdlgParametrageAtlas.btnGenererAtlasClick(Sender: TObject);
var
  QDossierDestAtlas: string;
begin
  btnGenererAtlas.Enabled := false;
  btnClose.Enabled        := false;
  pnlProgression.Visible  := true;
  try
    QDossierDestAtlas := editDossierDestination.Directory;
    GenererUnAtlas(QDossierDestAtlas,
                   editImgTopoWidthPX.AsInteger,
                   editImgTopoHeightPX.AsInteger,
                   editEtendueTopoX.Value,
                   chkCopyright.Checked, Trim(editMentionsCopyright.Text),
                   FParamsVue2D);

  finally
    btnGenererAtlas.Enabled := true;
    btnClose.Enabled        := true;
    pnlProgression.Visible  := false;
  end;
end;

procedure TdlgParametrageAtlas.cmbResolutionEcransSelect(Sender: TObject);
var
  WU: String;
begin
  WU  := cmbResolutionEcrans.Items[cmbResolutionEcrans.ItemIndex];
  PutResolEcranInForm(MakeTResolEcranFromStr(WU));
end;

procedure TdlgParametrageAtlas.editTailleCroixChange(Sender: TObject);
begin

end;

procedure TdlgParametrageAtlas.FormCreate(Sender: TObject);
begin

end;

procedure TdlgParametrageAtlas.Initialiser(const FD: TDocumentDessin;
                                           const TP: TTexturesPolygonObject;
                                           const PG: TParamsVue2D);
begin

  FProgressionTotal     := 0;
  FProgressionDone      := 0;
  FDocDessin            := FD;
  FTexturesPolygones    := TP;
  FParamsVue2D          := PG;
  editDossierDestination.Directory := GetGHCaveDrawDirectory();
  InitCaptions();
  ListerEcransUsuels();
  pnlProgression.Visible := false;
end;
procedure TdlgParametrageAtlas.ListerEcransUsuels();
const
  FMT_ECRANS_RESOL = '%d x %d: %s';
var
  i: Integer;
  M: TMonitor;
  MM: TResolEcran;
  procedure addEcran(const QW, QH: integer; const Desc: string);
  var
    WU: Integer;
    EWE: String;
  begin
    WU  := cmbResolutionEcrans.Items.Count;
    EWE := GetResourceString(Format(FMT_ECRANS_RESOL, [QW, QH, Desc]));
    cmbResolutionEcrans.Items.Add(EWE);
  end;
begin
  cmbResolutionEcrans.Clear;
  // les écrans courants
  for i:= 0 to Screen.MonitorCount - 1 do
  begin
    M := Screen.Monitors[i];
    cmbResolutionEcrans.Items.Add(GetResourceString(Format(FMT_ECRANS_RESOL, [M.Width, M.Height, BoolToStr(M.Primary, 'Primary', 'Auxiliary')])));
  end;
  // autres résolutions usuelles
  addEcran(2560, 2048, 'HDTV');
  addEcran(2560, 1440, 'HDTV');
  addEcran(2048, 1536, 'QXGA 4/3 ratio');
  addEcran(1920, 1080, 'HD 1080');
  addEcran(1600, 1200, 'UXGA 4/3 ratio');
  addEcran(1400, 1050, 'SXGA+ 4/3 ratio');
  addEcran(1280, 1024, 'SXGA 4/3 ratio');
  addEcran(1440, 900,  'WXGA aka Acer eMachines');
  addEcran(1152, 864, 'SXGA 4/3 ratio');
  addEcran(1024, 768, 'XGA Antiquity 4/3 ratio');
  addEcran( 800, 600, 'SVGA Prehistoric monitor');
  addEcran( 640, 480, 'VGA minimal');
  // résolutions démentielles
  addEcran(16384, 12288, 'H2XGA (Hex Hex Extended Graphics Array');
  // personnalisé
  MM := MakeTResolEcranFromStr(cmbResolutionEcrans.Items[0]);
  PutResolEcranInForm(MM);

  cmbResolutionEcrans.ItemIndex := 0;
end;

function TdlgParametrageAtlas.MakeTResolEcranFromStr(const s: string): TResolEcran;
var
  EWE: TGHStringArray;
  WU: String;
begin
  EWE := split(S, ':');
  WU  := Trim(EWE[0]);
  Result.description := Trim(EWE[1]);
  EWE := split(WU, 'x');
  Result.width  := StrToIntDef(Trim(EWE[0]), -1);
  Result.height := StrToIntDef(Trim(EWE[1]), -1);
end;


procedure TdlgParametrageAtlas.PutResolEcranInForm(const E: TResolEcran);
begin
  editMonitorWidth.AsInteger   := E.width;
  editMonitorHeight.AsInteger  := E.height;
  editDescResolEcran.Text      := E.description;
  // propal de taille des carrés: image carrée
  editImgTopoHeightPX.AsInteger := E.height - 270;
  editImgTopoWidthPX.AsInteger  := E.width  - 320;
end;



//------------------------------------------------------------------------------
// générer un atlas dans le dossier sélectionné
procedure TdlgParametrageAtlas.GenererUnAtlas(const QDossierDest: string;
                                              const QTailleCarresEnPixelX: integer;
                                              const QTailleCarresEnPixelY: integer;
                                              const QEtendueCarreEnM: double;
                                              const DoDrawCopyright: boolean;
                                              const StrCopyright: string;
                                              const FP: TParamsVue2D);
var
  QDossierAtlas: String;

begin
  AfficherMessage(Format('%s.GenererUnAtlas(%s)', [ClassName, QDossierDest]));
  // créer le dossier contenant l'atlas
  QDossierAtlas := QDossierDest + PathDelim + 'Atlas001';
  ForceDirectories(QDossierAtlas);
  FOutputAtlas := TGenerateurAtlasHTML.Create;
  try
    if (FOutputAtlas.Initialise(FDocDessin, QDossierAtlas, DoDrawCopyright, StrCopyright, FParamsVue2D)) then
    begin
      FOutputAtlas.SetProcDoAbort(DoAbortAtlas);
      FOutputAtlas.SetProcAfficherProgression(AfficherProgressionGenerationAtlas);
      FOutputAtlas.SetDimensionsCarres(QTailleCarresEnPixelX, QTailleCarresEnPixelY, QEtendueCarreEnM);
      FOutputAtlas.SetTexturespolygones(FTexturesPolygones);


      FOutputAtlas.CalcNbCarres();
      Application.ProcessMessages;
      FOutputAtlas.GenererMainPage();
      Application.ProcessMessages;
      FOutputAtlas.GenererLesPagesAtlas();
      Application.ProcessMessages;
      ShowMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
    end;
  finally
    FOutputAtlas.Free;
  end;
end;

function TdlgParametrageAtlas.DoAbortAtlas(): boolean;
begin
  Result := QuestionOuiNon('La première page de l''atlas est correcte ?');
end;

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
  // notes de copyright
  editMentionsCopyright.Clear;
  editMentionsCopyright.Items.add(format('(c) %d JPCassou', [YearOf(Now())]));
  editMentionsCopyright.Items.Add('CC-BY-SA JPCassou');
  editMentionsCopyright.Items.Add('CC0 JPCassou');
  editMentionsCopyright.Items.Add('CC-BY-NC-SA JPCassou');
  editMentionsCopyright.Items.Add('CC-BY-SA GRASLourdes');
  editMentionsCopyright.Items.Add('CC-BY-SA GESAEysines');
  editMentionsCopyright.ItemIndex := 0;
end;
procedure TdlgParametrageAtlas.AfficherProgressionGenerationAtlas(const Etape: string; const Min, Max, Position: integer);
begin
  lbProgress.Caption := Format('Page: %d / %d', [Position+1, Max]);
  lbEtapeTravail.Caption := Etape;
  ProgressBar1.Min       := 0;
  ProgressBar1.Max       := Max;
  ProgressBar1.Position  := 1+Position;
end;

end.

