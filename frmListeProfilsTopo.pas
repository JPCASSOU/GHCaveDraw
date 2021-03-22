unit frmListeProfilsTopo;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  //UnitListesSimplesWithGeneriques,
  ToporobotClasses2012,
  unitProfilTopo,
  UnitClasseMaillage,
  CadreProfilsTopo,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, PairSplitter, StdCtrls, ComCtrls, Types, LCLType;

type

  { TdlgLesProfilsTopo }

  TdlgLesProfilsTopo = class(TForm)
    BitBtn1: TBitBtn;
    btnModifierProfil: TButton;
    btnSupprimerProfil: TButton;
    btnSaveProfils: TButton;
    btnLoadProfils: TButton;
    CdrProfilTopo1: TCdrProfilTopo;
    HeaderControl1: THeaderControl;
    lbNbProfilsTopo: TLabel;
    lsbProfilsTopoMaillage: TListBox;
    mnlMaillages: TPanel;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    procedure btnLoadProfilsClick(Sender: TObject);
    procedure btnSaveProfilsClick(Sender: TObject);
    procedure btnSupprimerProfilClick(Sender: TObject);
    procedure btnModifierProfilClick(Sender: TObject);
    procedure lsbProfilsTopoMaillageDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbProfilsTopoMaillageSelectionChange(Sender: TObject; User: boolean);
  private
    FMyMaillage: TMaillage;
    FDocTopo   : TToporobotStructure2012;
    procedure DisplayProfil(const P: TProfilTopo);
    procedure ListerProfilsMaillages();
  public
    function  Initialiser(const DT: TToporobotStructure2012; const M: TMaillage): boolean;
    procedure Finaliser();

  end;

var
  dlgLesProfilsTopo: TdlgLesProfilsTopo;

implementation

{$R *.lfm}
uses
  CallDialogsStdVersion
  ;

{ TdlgLesProfilsTopo }
function TdlgLesProfilsTopo.Initialiser(const DT: TToporobotStructure2012; const M: TMaillage): boolean;
begin
  result := false;
  lsbProfilsTopoMaillage.ItemIndex := -1;
  try
    FMyMaillage := M;
    FDocTopo    := DT;
    ListerProfilsMaillages();
    //lsbProfilsTopoMaillage.ItemIndex := 0;
    //PP := FMyMaillage.GetProfilTopo(0);
    //DisplayProfil(PP);

    result := True;
  except
  end;
end;

procedure TdlgLesProfilsTopo.DisplayProfil(const P: TProfilTopo);
begin
  //CdrProfilTopo1.Finaliser();
  CdrProfilTopo1.Initialiser(FDocTopo, FMyMaillage, P);
end;

procedure TdlgLesProfilsTopo.Finaliser();
begin
  pass;
end;

procedure TdlgLesProfilsTopo.ListerProfilsMaillages();
var
  i, Nb: Integer;
  P: TProfilTopo;
begin
  Nb := FMyMaillage.GetNbProfilsTopo();
  lbNbProfilsTopo.Caption := Format('%d profils', [Nb]);
  lsbProfilsTopoMaillage.Clear;
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    P := FMyMaillage.GetProfilTopo(i);
    lsbProfilsTopoMaillage.Items.Add(P.ProfilName);
  end;
  lsbProfilsTopoMaillage.ItemIndex := 0;

end;

procedure TdlgLesProfilsTopo.lsbProfilsTopoMaillageDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
  mg  = 1;
var
  PF: TProfilTopo;
  procedure DessineFiletColonne(const TB: integer); inline;
  begin
    lsbProfilsTopoMaillage.Canvas.Line(TB, ARect.Top, TB, ARect.Bottom);
  end;
  procedure DessineItem(const bg,tc: TColor);
  VAR
    HS: THeaderSection;
    R : TRect;
  begin
    with lsbProfilsTopoMaillage do
    begin
      Canvas.FillRect(ARect);
      canvas.Pen.Color:=clSilver;
      Canvas.Font.Color :=tc;

      HS := HeaderControl1.Sections.Items[0];  // ID réseau
      canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[Index]));

      HS := HeaderControl1.Sections.Items[1];  // Couleur réseau
      DessineFiletColonne(HS.Left - Q4);
      canvas.Pen.Color   := clBlack;
      canvas.Brush.Color := PF.ProfilColor; // couleur du profil
      r.Left   := HS.Left;
      r.Right  := HS.Right - 8;
      r.Top    := ARect.Top + mg;
      r.Bottom := ARect.Bottom - mg;
      canvas.Rectangle(R);
      Canvas.Brush.Color := bg;
      canvas.Pen.Color   := clSilver;

      HS := HeaderControl1.Sections.Items[2];  // Nom profil
      DessineFiletColonne(HS.Left - Q4);
      Canvas.Font.Color := tc;
      canvas.TextOut(HS.Left + 4, ARect.Top+1, PF.ProfilName);
    end;
  end;
begin
  try
    PF := FMyMaillage.GetProfilTopo(Index);
    with lsbProfilsTopoMaillage do
    begin
      canvas.brush.color := clWhite;
      DessineItem(clWhite, clBlack);
      //affichage lorsque la ligne est sélectionnée
      if (odSelected in state) then
      begin
        canvas.brush.color:=clBlue;
        DessineItem(clBlue, clWhite);
      end;
    end;
  except
  end;
end;

procedure TdlgLesProfilsTopo.btnSupprimerProfilClick(Sender: TObject);
var
  n: Integer;
begin
  n := lsbProfilsTopoMaillage.ItemIndex;
  if (n < 0) then Exit;
  if (GHTopoQuestionOuiNon('Supprimer le profil')) then
  begin
    FMyMaillage.RemoveProfilTopo(n);
    ListerProfilsMaillages();
  end;
end;

procedure TdlgLesProfilsTopo.btnSaveProfilsClick(Sender: TObject);
var
  QFilename: TStringDirectoryFilename;
  QIdxFilter: integer;
begin
  QFilename := 'Profils_001.csv';
  if (DoDialogSaveFile(rsCSV_FILE_FILTER, '.csv', QFilename, QIdxFilter)) then
  begin
    FMyMaillage.SaveProfilsToFile(QFilename);
  end;
end;

procedure TdlgLesProfilsTopo.btnLoadProfilsClick(Sender: TObject);
var
  QFilename: TStringDirectoryFilename;
begin
  QFilename := 'Profils_001.csv';
  if (DoDialogOpenFile(rsCSV_FILE_FILTER, '.csv', QFilename)) then
  begin
    FMyMaillage.LoadProfilsFromFile(QFilename);
    ListerProfilsMaillages();
  end;
end;

procedure TdlgLesProfilsTopo.btnModifierProfilClick(Sender: TObject);
var
  n: Integer;
  PP: TProfilTopo;
begin
  n := lsbProfilsTopoMaillage.ItemIndex;
  if (n < 0) then Exit;
  PP := CdrProfilTopo1.GetProfilTopo();
  FMyMaillage.PutProfilTopo(n, PP);
  ListerProfilsMaillages();
end;

procedure TdlgLesProfilsTopo.lsbProfilsTopoMaillageSelectionChange(Sender: TObject; User: boolean);
var
  PP: TProfilTopo;
  nb: Integer;
begin
  nb := FMyMaillage.GetNbProfilsTopo();
  if (nb > 0) then
  begin
    PP := FMyMaillage.GetProfilTopo(lsbProfilsTopoMaillage.ItemIndex);
    DisplayProfil(PP);
  end;
end;


end.

