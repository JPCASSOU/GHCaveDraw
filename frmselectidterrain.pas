unit frmSelectIDTerrain;

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue

  StructuresDonnees,
  Common,
  ToporobotClasses2012,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TdlgSelectIDTerrain }

  TdlgSelectIDTerrain = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    editIDTerrainToFind: TEdit;
    Label1: TLabel;
    lsbIDsTerrain: TListBox;
    procedure editIDTerrainToFindChange(Sender: TObject);
    procedure lsbIDsTerrainClick(Sender: TObject);
  private
    { private declarations }
    FDocTopo: TToporobotStructure2012;
    FCurrentIdxIDTerrain: integer;
  public
    { public declarations }
    function Initialiser(const DC: TToporobotStructure2012): boolean;
    function GetIDXTerrainFound: integer;
  end;

var
  dlgSelectIDTerrain: TdlgSelectIDTerrain;

implementation

{$R *.lfm}

{ TdlgSelectIDTerrain }


procedure TdlgSelectIDTerrain.editIDTerrainToFindChange(Sender: TObject);
var
  EWE: String;
  p: Integer;
begin
  EWE := Trim(editIDTerrainToFind.Text);
  p := FDocTopo.FindIdxIDTerrainByText(EWE);
  if (p = -1) then editIDTerrainToFind.Color := clRed
              else editIDTerrainToFind.Color := clWhite;
  Label1.Caption := Format('%d', [p]);
  if (p >= 0) then lsbIDsTerrain.ItemIndex := p;
  FCurrentIdxIDTerrain := p;
end;

procedure TdlgSelectIDTerrain.lsbIDsTerrainClick(Sender: TObject);
begin
  FCurrentIdxIDTerrain := lsbIDsTerrain.ItemIndex;
end;

function TdlgSelectIDTerrain.Initialiser(const DC: TToporobotStructure2012): boolean;
var
  i, Nb: Integer;
  EWE: TToporobotIDStation;
begin
  Result := False;
  FCurrentIdxIDTerrain := -1;
  FDocTopo := DC;
  try
    lsbIDsTerrain.Clear;
    Nb := FDocTopo.GetNbLabelTerrain;
    if (Nb = 0) then Exit;
    for i := 0 to Nb - 1 do
    begin
      EWE := FDocTopo.GetLabelTerrain(i);
      lsbIDsTerrain.Items.Add(Format('%d.%d: %s', [EWE.aSerie, EWE.aStation, EWE.aIDTerrain]));
    end;
    Result := True;
  except
  end;
end;

function TdlgSelectIDTerrain.GetIDXTerrainFound: integer;
begin
  Result := FCurrentIdxIDTerrain;
end;

end.

