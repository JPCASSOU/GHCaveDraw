unit frmSelectCoordsSystem;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, Buttons,
  ConvertisseurJPC, GHCD_Types, Types, LCLType;

type

  { TdlgSelectionCoordsSystem }

  TdlgSelectionCoordsSystem = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    hcColsTitres: THeaderControl;
    lsbCoordsSystems: TListBox;
    procedure lsbCoordsSystemsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  private
    FConversionSysteme: TConversionSysteme;
  public
    function Initialiser(const FC: TConversionSysteme): boolean;
    function GetSelectedEPSGSysCoords(): TLabelSystemesCoordsEPSG;
  end;

var
  dlgSelectionCoordsSystem: TdlgSelectionCoordsSystem;

implementation

{$R *.lfm}

{ TdlgSelectionCoordsSystem }

procedure TdlgSelectionCoordsSystem.lsbCoordsSystemsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
var
  q: string;
  CC: TLabelSystemesCoordsEPSG;
  // bg = couleur de fond; tc= couleur du texte
  procedure DessineFiletColonne(const TB: integer); inline;
  begin
    lsbCoordsSystems.Canvas.Line(TB, ARect.Top, TB, ARect.Bottom);
  end;
  procedure DessineItem(const bg,tc: TColor);
  VAR
    //TB: integer;
    HS: THeaderSection;
    QR: TRect;
    ColorItemSelected: TColor;
  begin
    with lsbCoordsSystems do
    begin
      Canvas.FillRect(ARect);
      HS := hcColsTitres.Sections.Items[0];
      Canvas.Brush.Color := bg;
      Canvas.Font.Color  := tc;
      canvas.Pen.Color:=clSilver;
      canvas.TextOut(HS.Left + 4  , ARect.Top+1, Format('%d', [CC.CodeEPSG]));

      HS := hcColsTitres.Sections.Items[1];
      Canvas.Brush.Color:=bg;
      canvas.Pen.Color:=clSilver;
      DessineFiletColonne(HS.Left - Q4);
      canvas.TextOut(HS.Left + 4  , ARect.Top+1, CC.Nom);
    end;
  end;

begin
  CC := FConversionSysteme.GetCodeEPSGNomSysteme(Index);
  if (odSelected in state) then DessineItem(clBlue, clWhite)
                           else DessineItem(clwhite, clBlack);
end;

function TdlgSelectionCoordsSystem.Initialiser(const FC: TConversionSysteme): boolean;
var
  Nb, i: Integer;
  EWE: TLabelSystemesCoordsEPSG;
begin
  result := false;
  try
    FConversionSysteme := FC;
    Nb := FConversionSysteme.GetNbSystemes();
    lsbCoordsSystems.Clear;
    for i := 0 to Nb - 1 do
    begin
      EWE := FConversionSysteme.GetCodeEPSGNomSysteme(i);
      lsbCoordsSystems.Items.Add(format('%d - %s', [EWE.CodeEPSG, EWE.Nom]));
    end;
    lsbCoordsSystems.ItemIndex := 0;
    result := true;
  except

  end;
end;

function TdlgSelectionCoordsSystem.GetSelectedEPSGSysCoords(): TLabelSystemesCoordsEPSG;
begin
  Result := FConversionSysteme.GetCodeEPSGNomSysteme(lsbCoordsSystems.ItemIndex);
end;


end.

