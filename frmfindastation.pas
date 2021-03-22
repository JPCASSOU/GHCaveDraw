unit frmFindAStation;

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  Common,
  ToporobotClasses2012, StructuresDonnees,
  LCLType, Clipbrd;

type

  { TdlgFindAStation }

  TdlgFindAStation = class(TForm)
    btnOK: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    chkDoMatchExact: TCheckBox;
    editFindWhat: TEdit;
    lbPrompt: TLabel;
    lsbIDsTerrain: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure lsbIDsTerrainDblClick(Sender: TObject);
  private
    { private declarations }
    FDocTopo: TToporobotStructure2012;
    procedure ListerIDsTerrain();
  public
    { public declarations }
    function Initialiser(const QDC: TToporobotStructure2012): boolean;
    function GetFindWhat(): string;
    function GetDoMatchExact(): boolean;
  end;

var
  dlgFindAStation: TdlgFindAStation;

implementation

{$R *.lfm}

procedure TdlgFindAStation.Button1Click(Sender: TObject);
const
  NB_COLONNES_PAR_PAGE : integer = 3;
var
  Clipboard: TClipboard;
  Nb, i: Integer;
  EWE: TToporobotIDStation;
  WU: String;
begin
  Clipboard := TClipboard.Create(ctClipboard);
  try
    Clipboard.Clear;
    Nb := FDocTopo.GetNbLabelTerrain;
    if (Nb = 0) then Exit;
    WU := '';
    for i := 0 to Nb - 1 do
    begin
      EWE := FDocTopo.GetLabelTerrain(i);
      WU  := WU + Format('%d.%d', [EWE.aSerie, EWE.aStation]) + #9 +
             EWE.aIDTerrain + #9 + #9;
      if ((i+1) mod NB_COLONNES_PAR_PAGE) = 0 then WU := WU + #13#10;
    end;
    AfficherMessageErreur(WU);
    Clipboard.AsText := WU;
  finally

  end;
end;

{ TdlgFindAStation }
function TdlgFindAStation.GetDoMatchExact: boolean;
begin
  result := chkDoMatchExact.Checked;
end;

function TdlgFindAStation.GetFindWhat: string;
begin
  result := Trim(editFindWhat.Text);
end;

function TdlgFindAStation.Initialiser(const QDC: TToporobotStructure2012): boolean;
begin
  Result := false;
  self.Caption            := GetResourceString(rsDLG_FIND_STATION_TITLE);
  lbPrompt.Caption        := GetResourceString(rsDLG_FIND_STATION_PROMPT);
  chkDoMatchExact.Caption := GetResourceString(rsDLG_FIND_STATION_DO_MATCH);
  try
    FDocTopo := QDC;
    ListerIDsTerrain();
    Result := True;
  except
  end;
end;

procedure TdlgFindAStation.ListerIDsTerrain();
var
  Nb, i: Integer;
  EWE: TToporobotIDStation;
begin
  try
    lsbIDsTerrain.Clear;
    Nb := FDocTopo.GetNbLabelTerrain;
    if (Nb = 0) then Exit;
    for i := 0 to Nb - 1 do
    begin
      EWE := FDocTopo.GetLabelTerrain(i);
      lsbIDsTerrain.Items.Add(Format('%d.%d: %s', [EWE.aSerie, EWE.aStation, EWE.aIDTerrain]));
    end;
  except
  end;
end;

procedure TdlgFindAStation.lsbIDsTerrainDblClick(Sender: TObject);
var
  EWE: String;
  p: SizeInt;
begin
  EWE := Trim(lsbIDsTerrain.Items[lsbIDsTerrain.ItemIndex]);
  p := pos(':', EWE);
  if (p > 0) then EWE := Trim(Copy(EWE, p+1, length(EWE)));
  editFindWhat.Text := EWE;
end;

end.

