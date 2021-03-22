unit dlgEditeurSectionTransversale;

{$INCLUDE CompilationParameters.inc}

interface

uses
  GHCD_Types,
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GeneralFunctions, math, unitSectionsTransversales, UnitDocDessin,
  CadreGHCDEditor,
  cdrSectionTransversale,
  CallDialogsGHCaveDraw,
  Classes, SysUtils, FileUtil, curredit, SynEdit,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ExtCtrls, ActnList, ComCtrls, Menus, LCLType, PairSplitter, ColorBox, CheckLst;

type

  { TfrmEditeurSections }

  TfrmEditeurSections = class(TForm)
    acCourbe: TAction;
    acPanDown: TAction;
    acPanLeft: TAction;
    acPanRight: TAction;
    acPanUp: TAction;
    acPolygone: TAction;
    acReset: TAction;
    acSelectCourbe: TAction;
    acSelectPolygone: TAction;
    acSelectSimpleLigne: TAction;
    acSelectTexte: TAction;
    acSimpleLigne: TAction;
    acTexte: TAction;
    acOpenSection: TAction;
    acSaveSection: TAction;
    acNouvelleSection: TAction;
    acSerialiser: TAction;
    acParser: TAction;
    acHelp: TAction;
    acQuit: TAction;
    ActionList1: TActionList;
    acValidateCourbe: TAction;
    acValidateCourbePolygone: TAction;
    acZoomMoins: TAction;
    acZoomPlus: TAction;
    acZoomTout: TAction;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CadreSectionTransversale1: TCadreSectionTransversale;
    CdrGHCDTextEditor1: TCdrGHCDTextEditor;
    cmbGroupes: TComboBox;
    editAngleOrientation: TCurrencyEdit;
    editBasePointID: TEdit;
    editDemiOuverture: TCurrencyEdit;
    editIdxGroupe: TCurrencyEdit;
    editOffsetX: TCurrencyEdit;
    editOffsetY: TCurrencyEdit;
    imglstMenus: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbBasePointIDTerrain: TStaticText;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuAide: TMenuItem;
    mnuFichier: TMenuItem;
    PageControl1: TPageControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    popupCourbePoly: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    StaticText1: TStaticText;
    memoHerisson: TSynEdit;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    procedure acCourbeExecute(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    procedure acNouvelleSectionExecute(Sender: TObject);
    procedure acOpenSectionExecute(Sender: TObject);
    procedure acParserExecute(Sender: TObject);
    procedure acQuitExecute(Sender: TObject);
    procedure acSaveSectionExecute(Sender: TObject);
    procedure acSerialiserExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);

  private
    { private declarations }
    FMyDocTopo              : TDocumentDessin;
    FHerissonViseesRadiantes: THerissonViseesRadiantes;
    FCurrentCrossSection    : TCrossSection;

    procedure AnalyserEtTracerSection();
    procedure InitCaptionsMenus();
    function  initSectionVierge(): boolean;
    procedure ListerGroupes();
    procedure Serialiser();
  public
    { public declarations }
    function Initialiser(const FD: TDocumentDessin; const BP: TBaseStation; const QOffsetX, QOffsetY: double): boolean;
    procedure Finaliser();
  end;

var
  frmEditeurSections: TfrmEditeurSections;

implementation
uses DGCDummyUnit;

{$R *.lfm}

{ TfrmEditeurSections }



procedure TfrmEditeurSections.acHelpExecute(Sender: TObject);
begin
  pass;
end;

procedure TfrmEditeurSections.acCourbeExecute(Sender: TObject);
begin

end;

procedure TfrmEditeurSections.acNouvelleSectionExecute(Sender: TObject);
begin
  if (not (QuestionOuiNon(GetResourceString(rsEDITOR_SECTIONS_MSG_BEFORE_RESET_SECTION)))) then Exit;
  initSectionVierge();
  self.Serialiser();
  PageControl1.ActivePageIndex := 0;
end;

procedure TfrmEditeurSections.acOpenSectionExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
begin
  QFileName := '';
  if (DoDialogOpenFilename('Fichiers texte|*.txt', QFileName)) then
  begin
    initSectionVierge();
    (*
    memoHerisson.Lines.LoadFromFile(QFileName);
    AnalyserEtTracerSection();
    //*)
  end;
end;

procedure TfrmEditeurSections.acParserExecute(Sender: TObject);
begin
  AnalyserEtTracerSection();
  PageControl1.ActivePageIndex := 0;
end;

procedure TfrmEditeurSections.acQuitExecute(Sender: TObject);
begin
  self.Close;
end;

procedure TfrmEditeurSections.acSaveSectionExecute(Sender: TObject);
var
  QFilename: TStringDirectoryFileName;
begin
  QFilename := Format('Section_' + FORMAT_BASEPOINT + '.txt', [FHerissonViseesRadiantes.getBaseStation().IDStation]);
  if (DoDialogSaveFilename('Fichiers texte|*.txt|Tous|*.*', '.txt', QFilename)) then
  begin
    Serialiser();
    CdrGHCDTextEditor1.SaveToFile(QFilename);
  end;
end;

procedure TfrmEditeurSections.acSerialiserExecute(Sender: TObject);
begin
  self.Serialiser();
  PageControl1.ActivePageIndex := 0;
end;

procedure TfrmEditeurSections.Button1Click(Sender: TObject);
begin

end;

procedure TfrmEditeurSections.Button3Click(Sender: TObject);
var
  LeHerisson: THerissonViseesRadiantes;
  n, i: Integer;
  BP: TBaseStation;
  VV: TViseeRayonnante;
  procedure MiouMiou(const Miou: string);
  begin
    memoHerisson.Lines.Add(Miou);
  end;
begin
  LeHerisson := CadreSectionTransversale1.GetHerisson();
  memoHerisson.Lines.Clear;
  n := LeHerisson.getNbViseesRayonnantes();
  BP := LeHerisson.getBaseStation();
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  MiouMiou(Format('Basepoint: %s - %s',  [BP.IDStation, BP.IDTerrain]));
  {$ELSE}
  MiouMiou(Format('Basepoint: %d - %s',  [BP.IDStation, BP.IDTerrain]));
  {$ENDIF TIDBASEPOINT_AS_TEXT}
  MiouMiou(Format('X = %.3f, Y = %.3f, Z = %.3f', [BP.PosStation.X, BP.PosStation.Y, BP.PosStation.Z]));
  MiouMiou(format('Angle rotation de la section = %.2f', [LeHerisson.getAngleOrientationSection()]));
  MiouMiou(format('====== %d ===========', [n]));
  if (n > 0) then
  begin
    for i := 0 to n - 1 do
    begin
      VV := LeHerisson.getViseeRayonnante(i);
      MiouMiou(format('-- %d: %.3f, %.3f, %.3f', [i, VV.Longueur, VV.Azimut, VV.Pente]));
    end;
  end;
end;

procedure TfrmEditeurSections.AnalyserEtTracerSection();
var
  LS: TStringList;
  QMessageErreur: string;
  i, n, QNumeroLigneEnErreur: Integer;
begin
  try
    LS := TStringList.Create;
    LS.Clear;
    n := CdrGHCDTextEditor1.GetNbLines();
    if (n = 0) then Exit;
    for i := 0 to n - 1 do LS.Add(CdrGHCDTextEditor1.GetALine(i));
    FCurrentCrossSection.Analyser(LS, QNumeroLigneEnErreur, QMessageErreur);
    CadreSectionTransversale1.Redessin();
  finally
    FreeAndNil(LS); //LS.Free;
  end;
  PageControl1.ActivePageIndex := 0;
  //*)
end;






function TfrmEditeurSections.initSectionVierge(): boolean;
var
  w: Boolean;
begin
  //BP := FMyDocTopo.FindBasePoint();
  result := false;
  FCurrentCrossSection.SetIDGroupe(editIdxGroupe.AsInteger);
  FCurrentCrossSection.SetDecalageXY(0.0, 0.0);
  // on ajoute quelques objets ici
  //FCurrentCrossSection.addSimpleLigneByValues(nolFRACTURE, -12.255, -9.881, 11.022, 3.274);
  //FCurrentCrossSection.addTexteByValues(notCOTATION, 0, -8.47,  -5.81, GetLlanfairPG());
  w := CadreSectionTransversale1.CdrDrwInitialiser(FHerissonViseesRadiantes);
  CadreSectionTransversale1.SetSectionTransversale(FCurrentCrossSection);
  CadreSectionTransversale1.SetViewLimits(-10, -10, 10, 10);
  result := true;
end;


procedure TfrmEditeurSections.Serialiser();
var
  LS: TStringList;
begin
  LS := TStringList.Create;
  LS.Clear;
  try
    FCurrentCrossSection.Serialiser(LS);
    CdrGHCDTextEditor1.SetLines(LS);
    LS.Clear;
  finally
    FreeAndNil(LS); //LS.Free;
  end;
end;
procedure TfrmEditeurSections.InitCaptionsMenus();
  procedure S777(const WU: TAction; const CaptionHint: string);
  begin
    WU.Caption  := GetResourceString(CaptionHint);
    WU.Hint     := GetResourceString(CaptionHint);
  end;
begin
  mnuFichier.Caption    := GetResourceString(rsMNU_FILE);
    S777(acNouvelleSection  , rsEDITOR_SECTIONS_MNU_NOUVEAU);
    S777(acOpenSection      , rsEDITOR_SECTIONS_MNU_OPEN);
    S777(acSaveSection      , rsEDITOR_SECTIONS_MNU_SAVE);
    S777(acSerialiser       , rsEDITOR_SECTIONS_MNU_SERIALISE);
    S777(acParser           , rsEDITOR_SECTIONS_MNU_PARSE);
    S777(acQuit             , rsEDITOR_SECTIONS_MNU_QUIT);
  mnuAide.Caption       := GetResourceString(rsMNU_AIDE);
    S777(acHelp             , rsEDITOR_SECTIONS_MNU_HELP);
end;

function TfrmEditeurSections.Initialiser(const FD: TDocumentDessin; const BP: TBaseStation; const QOffsetX, QOffsetY: double): boolean;
var
  EWE: TBaseStation;
  WU : TToporobotIDStation;
  Ang: double;
begin
  Result := False;
  self.Caption := GetResourceString(rsEDITOR_SECTIONS_MAIN_TITLE);
  FMyDocTopo := FD;
  InitCaptionsMenus;
  FHerissonViseesRadiantes := THerissonViseesRadiantes.Create;
  FCurrentCrossSection     := TCrossSection.Create;
  //try
    FHerissonViseesRadiantes.Initialiser(FD, BP, 0.00);
    FCurrentCrossSection.Initialiser(BP);
    FCurrentCrossSection.SetDecalageXY(QOffsetX, QOffsetY);
    editOffsetX.Value := QOffsetX;
    editOffsetY.Value := QOffsetY;
    EWE := FHerissonViseesRadiantes.getBaseStation();
    Ang := FHerissonViseesRadiantes.getAngleOrientationSection();
    editAngleOrientation.Value := Ang;
    {$IFDEF TIDBASEPOINT_AS_TEXT}
    self.Caption := Format('Section transversale: %s - Orientation: %.0f', [EWE.IDStation, Ang]);
    {$ELSE}
    WU  := GetToporobotIDStation(EWE);
    editBasePointID.Text         := GetToporobotIDStationAsString(BP, false);
    self.Caption := Format('Section transversale: %d.%d (%s) - Orientation: %.0f', [WU.aSerie, WU.aStation, WU.aIDTerrain, Ang]);
    {$ENDIF TIDBASEPOINT_AS_TEXT}
    lbBasePointIDTerrain.Caption := BP.IDTerrain;
    ListerGroupes();
    // éditeur de code GHCaveDraw;
    Result := CdrGHCDTextEditor1.Initialiser(True);
  //except
  //end;
end;


procedure TfrmEditeurSections.Finaliser();
begin
  AfficherMessage(Format('%s.Finaliser', [ClassName]));
  try
    FHerissonViseesRadiantes.Finaliser();
    FCurrentCrossSection.Finaliser();
  finally
    FreeAndNil(FHerissonViseesRadiantes);
    FreeAndNil(FCurrentCrossSection);
  end;
end;

procedure TfrmEditeurSections.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := false;
  if (QuestionOuiNon(GetResourceString(rsEDITOR_SECTIONS_MSG_BEFORE_RESET_SECTION))) then CanClose := true;
end;

procedure TfrmEditeurSections.FormShow(Sender: TObject);
begin
  initSectionVierge();
  self.Serialiser();
  PageControl1.ActivePageIndex := 0;
end;


procedure TfrmEditeurSections.ListerGroupes;
var
  i, Nb: Integer;
  GRP: TGroupeEntites;
begin
  Nb := FMyDocTopo.GetNbGroupes();
  cmbGroupes.Clear;
  for i := 0 to Nb - 1 do
  begin
    GRP := FMyDocTopo.GetGroupe(i);
    cmbGroupes.Items.Add(Format('%d - %s', [GRP.IDGroupeEntites, GRP.NomGroupe]));
  end;
  cmbGroupes.ItemIndex := 0;
end;
end.


