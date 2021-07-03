unit frmExportToSIG;
{$INCLUDE CompilationParameters.inc}

interface

uses
  GHCD_Types,
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GeneralFunctions,
  ConvertisseurJPC,
  UnitDocDessin,
  CallDialogsGHCaveDraw,
  Classes, SysUtils, FileUtil, curredit,
  Forms,
  Controls, Graphics, Dialogs, Buttons, EditBtn, StdCtrls, ExtCtrls, ComCtrls
  ;

type

  { TdlgExportSIG }

  TdlgExportSIG = class(TForm)
    BitBtn1: TBitBtn;
    btnColorSilhouette: TColorButton;
    btnDoExport: TButton;
    btnChooseSystemeEPSG: TButton;
    chkScrapsFilled: TCheckBox;
    chkgbxFormatsOutput: TCheckGroup;
    chkWithMetadata: TCheckBox;
    chkWithPOI: TCheckBox;
    chkWithCenterlines: TCheckBox;
    chkWithEntrances: TCheckBox;
    chkDoUseDefaultStyle: TCheckBox;
    editFileName: TFileNameEdit;
    editMapHeight: TCurrencyEdit;
    editMapWidth: TCurrencyEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lbEtape: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlProgression: TPanel;
    ProgressBar1: TProgressBar;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    trkBarTransparence: TTrackBar;
    procedure btnChooseSystemeEPSGClick(Sender: TObject);
    procedure btnDoExportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FMyDocDessin: TDocumentDessin;
    //FConversionSystem: TConversionSysteme; // le convertisseur est contenu dans TDocumentDessin
    procedure AfficherProgression(const Etape: string; const Min, Max, Position: integer);
    procedure ListerCoordsSystems();
  public
    { public declarations }
    function Initialiser(const FD: TDocumentDessin): boolean;
  end;

var
  dlgExportSIG: TdlgExportSIG;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}

procedure TdlgExportSIG.AfficherProgression(const Etape: string; const Min, Max, Position: integer);
begin
  lbEtape.Caption := Etape;
  lbEtape.Refresh;
  ProgressBar1.Min := Min;
  ProgressBar1.Max := Max;
  ProgressBar1.Position := Position;
end;

procedure TdlgExportSIG.btnDoExportClick(Sender: TObject);
var
  WithExportGIS: TWithExportGIS;
begin
  pnlProgression.Visible := true;
  WithExportGIS := [];
  if (chkWithCenterlines.Checked) then WithExportGIS := WithExportGIS + [gisWITH_CENTERLINES];
  if (chkWithEntrances.Checked)   then WithExportGIS := WithExportGIS + [gisWITH_ENTRANCES];
  if (chkWithPOI.Checked)         then WithExportGIS := WithExportGIS + [gisWITH_POI];
  if (chkWithMetadata.Checked)    then WithExportGIS := WithExportGIS + [gisWITH_METADATA];
  FMyDocDessin.SetProcAfficherProgression(self.AfficherProgression);
  if (chkgbxFormatsOutput.Checked[0]) then
    FMyDocDessin.GenererCarteLeaflet(ChangeFileExt(editFileName.FileName, '.htm'),
                                             editMapWidth.AsInteger, editMapHeight.AsInteger,
                                             chkDoUseDefaultStyle.Checked,
                                             chkScrapsFilled.Checked,
                                             btnColorSilhouette.ButtonColor,
                                             255 - trkBarTransparence.Position,
                                             WithExportGIS);
  if (chkgbxFormatsOutput.Checked[1]) then
    FMyDocDessin.ExporterScrapsToKML(ChangeFileExt(editFileName.FileName, '.kml'),
                                     chkDoUseDefaultStyle.Checked,
                                     chkScrapsFilled.Checked,
                                     btnColorSilhouette.ButtonColor,
                                     255 - trkBarTransparence.Position,
                                     WithExportGIS);
  if (chkgbxFormatsOutput.Checked[2]) then
    FMyDocDessin.ExporterScrapsToGeoJSON(ChangeFileExt(editFileName.FileName, '.json'),
                                            chkDoUseDefaultStyle.Checked,
                                            chkScrapsFilled.Checked,
                                            btnColorSilhouette.ButtonColor,
                                            255 - trkBarTransparence.Position,
                                            WithExportGIS);
  {$IFDEF DXF_SUPPORT}
  if (chkgbxFormatsOutput.Checked[3]) then
    FMyDocDessin.ExporterScrapsToDXF(self,
                                     ChangeFileExt(editFileName.FileName, '.dxf'),
                                            chkDoUseDefaultStyle.Checked,
                                            chkScrapsFilled.Checked,
                                            btnColorSilhouette.ButtonColor,
                                            255 - trkBarTransparence.Position,
                                            WithExportGIS);

  {$ENDIF DXF_SUPPORT}
  FMyDocDessin.SetProcAfficherProgression(nil);
  ShowMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
  pnlProgression.Visible := False;
end;

procedure TdlgExportSIG.FormCreate(Sender: TObject);
begin

end;



procedure TdlgExportSIG.btnChooseSystemeEPSGClick(Sender: TObject);
var
  MyEPSG: TLabelSystemesCoordsEPSG;
  EWE: TCodeEPSG;
  FC: TConversionSysteme;

begin
  FC := FMyDocDessin.GetConvertisseurCoordonnees();
  if (SelectCoordinatesSystem(FC, MyEPSG)) then
  begin
    EWE := MyEPSG.CodeEPSG;
    FMyDocDessin.SetCodeEPSG(EWE);
    // acquittement: Récupération depuis le document, et non réutilisation de SC
    EWE := FMyDocDessin.GetCodeEPSG();
    MyEPSG := FC.GetEPSGSystemeFromCodeEPSG(EWE);
    btnChooseSystemeEPSG.Caption := Format('%d: %s', [MyEPSG.CodeEPSG, MyEPSG.Nom]);

  end;
end;





function TdlgExportSIG.Initialiser(const FD: TDocumentDessin): boolean;
var
  FC: TConversionSysteme;
begin
  Result := false;
  pnlProgression.Visible := false;
  FMyDocDessin       := FD;
  FC  := FD.GetConvertisseurCoordonnees();
  ListerCoordsSystems();
  try

    btnDoExport.Caption             := GetResourceString(rsMISC_BTN_DO_EXPORT_SIG);
    chkDoUseDefaultStyle.Caption    := GetResourceString(rsMISC_CHK_USE_DEFAULT_STYLE);
    editMapWidth.AsInteger  := Screen.Width - 60;
    editMapHeight.AsInteger := Screen.Height - 200;


    editFileName.FileName      := 'scraps';
    editFileName.DialogOptions := editFileName.DialogOptions + [ofOverwritePrompt];
    editFileName.InitialDir    := GetGHCaveDrawDirectory();
    editFileName.Filter        := 'Map files|*.htm;*.kml;*.json';
    chkgbxFormatsOutput.Checked[0] := True;
    chkgbxFormatsOutput.Checked[1] := True;
    chkgbxFormatsOutput.Checked[2] := True;
    chkgbxFormatsOutput.Checked[3] := {$IFDEF DXF_SUPPORT}True{$else}False{$ENDIF DXF_SUPPORT};

    chkDoUseDefaultStyle.Checked   := True;
    btnColorSilhouette.ButtonColor := clBlue;
    trkBarTransparence.Position    := 64;
    result := True;
  except
  end;
end;





procedure TdlgExportSIG.ListerCoordsSystems();
var
  EWE: TCodeEPSG;
  MyEPSG: TLabelSystemesCoordsEPSG;
  FC: TConversionSysteme;
begin
  FC := FMyDocDessin.GetConvertisseurCoordonnees();
  EWE := FMyDocDessin.GetCodeEPSG();
  MyEPSG := FC.GetEPSGSystemeFromCodeEPSG(EWE);
  btnChooseSystemeEPSG.Caption := Format('%d: %s', [MyEPSG.CodeEPSG, MyEPSG.Nom]);
end;


end.

