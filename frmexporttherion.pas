unit frmExportTherion;
// Export vers Therion

{$INCLUDE CompilationParameters.inc}

// 19/12/2014:
interface

uses
  SysUtils
  , Classes
  , GeneralFunctions
  , GHCD_Types
  , UnitDocDessin
  , unitTherionScrap
  {$IFDEF LANGUAGE_FRENCH}
    , UnitMessages_fr
  {$ENDIF}
  , Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls, CheckLst, ComCtrls,
  Buttons, FileCtrl, SynEdit, curredit, Types;



type

  { TdlgExportToTherion }

  TdlgExportToTherion = class(TForm)
    BitBtn1: TBitBtn;
    btnForegroundColor: TColorButton;
    Button1: TButton;
    chkInsertGrid: TCheckBox;
    chkInsertCenterlines: TCheckBox;
    chkGenererFichierLOX: TCheckBox;
    chkInsertGrid1: TCheckBox;
    chkInsertLegend: TCheckBox;
    chkUseColorScraps: TCheckBox;
    chkGenererPDF: TCheckBox;
    chkGenererAtlasPDF: TCheckBox;
    chklsbGroupes: TCheckListBox;
    btnBackgroundColor: TColorButton;
    editDirectoryDest: TDirectoryEdit;
    editGridSpacing: TCurrencyEdit;
    editAngleRotation: TCurrencyEdit;
    editScaleSize: TCurrencyEdit;
    Label6: TLabel;
    Layout1: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lsbFichiersCenterlines: TFileListBox;
    Label1: TLabel;
    lsbGroupesFichiersTH2: TListBox;
    PageControl1: TPageControl;
    editTHConfig: TSynEdit;
    SynEdit1: TSynEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tabShtTHConfig: TTabSheet;
    tabShtGroupes: TTabSheet;
    tabshtGeneral: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure editDirectoryDestChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  private
    { private declarations }
    FMyDocDessin: TDocumentDessin;

    procedure ExportGroupeToTherionScrap(const Grp: TGroupeEntites; const TH2FileName: string);
    procedure ListerFichiersTH2();
    procedure ListerGroupes();
  public
    { public declarations }
    function Initialiser(const FD: TDocumentDessin): boolean;
  end;

var
  dlgExportToTherion: TdlgExportToTherion;

implementation

{$R *.lfm}


//******************************************************************************
{ TdlgExportToTherion }
procedure TdlgExportToTherion.ListerGroupes();
var
  n, i: Integer;
  MyGrp: TGroupeEntites;
begin
  n := FMyDocDessin.GetNbGroupes();
  if (n = 0) then Exit;
  chklsbGroupes.Clear;
  for i := 0 to n - 1 do
  begin
    MyGrp := FMyDocDessin.GetGroupe(i);
    chklsbGroupes.Items.Add(format('%d: %s', [MyGrp.IDGroupeEntites, MyGrp.NomGroupe]));
  end;
  chklsbGroupes.ItemIndex := 0;
end;
procedure TdlgExportToTherion.ListerFichiersTH2();
var
  n, i: Integer;
  MyGrp: TGroupeEntites;
begin
  n := FMyDocDessin.GetNbGroupes();
  if (n = 0) then Exit;
  lsbGroupesFichiersTH2.Clear;
  for i := 0 to n - 1 do
  begin
    MyGrp := FMyDocDessin.GetGroupe(i);
    lsbGroupesFichiersTH2.Items.Add(format('TherionScrap_%d_Groupe_%d.th2', [i, MyGrp.IDGroupeEntites]));
  end;
  lsbGroupesFichiersTH2.ItemIndex := 0;
end;

procedure TdlgExportToTherion.editDirectoryDestChange(Sender: TObject);
begin
  //lsbFichiersCenterlines.Directory := editDirectoryDest.Directory;
end;

procedure TdlgExportToTherion.Button1Click(Sender: TObject);
var
  MyGroupe: TGroupeEntites;
  TH2: String;
begin
  MyGroupe := FMyDocDessin.GetGroupe(chklsbGroupes.ItemIndex);
  TH2 := lsbGroupesFichiersTH2.Items[chklsbGroupes.ItemIndex];
  ExportGroupeToTherionScrap(MyGroupe, TH2);
end;

procedure TdlgExportToTherion.PageControl1Change(Sender: TObject);
begin

end;

procedure TdlgExportToTherion.TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin

end;

function TdlgExportToTherion.Initialiser(const FD: TDocumentDessin): boolean;
begin
  Result := false;
  FMyDocDessin := FD;
  try
    editDirectoryDest.Directory := GetGHCaveDrawDirectory();
    ListerGroupes();
    ListerFichiersTH2;
  except
  end;
  result := True;
end;

procedure TdlgExportToTherion.ExportGroupeToTherionScrap(const Grp: TGroupeEntites; const TH2FileName: string);
var
  MyTherionScrap: TTherionScrap;
  LS: TStringList;
  n, i: Integer;
  CC: TCourbe;
begin
  AfficherMessage(format('%s.ExportGroupeToTherionScrap(): %d - %s', [ClassName, Grp.IDGroupeEntites, Grp.NomGroupe]));
  MyTherionScrap := TTherionScrap.Create;
  try
    MyTherionScrap.Initialiser(FMyDocDessin, TH2FileName);
    MyTherionScrap.SetGroupe(Grp);
    // recenser les basepoints
    MyTherionScrap.RecenserBasePointsScrap();

    //..................
    MyTherionScrap.BeginTH2File();
     MyTherionScrap.BeginScrap('');
      MyTherionScrap.MakeSectionBasePoints();
      // export des courbes
      n := FMyDocDessin.GetNbCourbes();
      if (n > 0) then
      begin
        for i := 0 to n - 1 do
        begin
          CC := FMyDocDessin.GetCourbe(i);
          if (CC.IDGroupe = Grp.IDGroupeEntites) then MyTherionScrap.MakeCourbe(CC);
        end;
      end;
     MyTherionScrap.EndScrap();
    MyTherionScrap.EndTH2File();
    // attraper et lister le tampon de sortie
    LS := MyTherionScrap.GetTamponSortie();
    n := LS.Count;
    SynEdit1.Clear;
    if (n > 0) then
    begin
      for i := 0 to n - 1 do SynEdit1.Lines.Add(LS.Strings[i]);
    end;
    //-------------------------
    MyTherionScrap.Finaliser();
  finally
    FreeAndNil(MyTherionScrap);//MyTherionScrap.Free;
  end;

end;
{

source demo-jaskyna

layout demo-jaskyna-header

  map-header 0 0 nw

  #language cz
  #language sk
  #language en
  language fr

  legend off #on

  colour map-fg [80 80 80]
  #colour map-bg [70 90 70]
  symbol-hide group all
  symbol-show line wall
  symbol-show line survey
  rotate 30 # angle de rotation

  grid bottom

  grid-size 5 5 5 m

  scale-bar 10 m



  code metapost

  def l_survey_cave (expr p) =
    draw p withpen PenD withcolor (0,0,1);
  enddef;

endlayout

export map -layout demo-jaskyna-header -output cave_03.pdf
}

end.

