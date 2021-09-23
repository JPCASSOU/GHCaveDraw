unit dlgSelectAcadColor;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons;

type

{ TfrmSelectAcadColor }

 TfrmSelectAcadColor = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    pbxPalette: TPaintBox;
    pnlGrilleCouleurs: TPanel;
    procedure pbxPalettePaint(Sender: TObject);
  private
    FCanDrawPalette: boolean;
    FCurrIndex: integer;
    procedure DrawPalette();
  public
    function Initialiser(const C: integer): boolean;
  end;

var
  frmSelectAcadColor: TfrmSelectAcadColor;

implementation

{$R *.lfm}

{ TfrmSelectAcadColor }

procedure TfrmSelectAcadColor.pbxPalettePaint(Sender: TObject);
begin
  DrawPalette();
end;

procedure TfrmSelectAcadColor.DrawPalette();
begin
  if (not FCanDrawPalette) then exit;
end;

function TfrmSelectAcadColor.Initialiser(const C: integer): boolean;
begin
  result := false;
  FCanDrawPalette := false;
end;
end.
//*************************************
procedure TDialogSelTopoColor.DrawPalette;
var
  CCX, CCY: integer;
  i, h, l, t: integer;
  R, RC: TRect;
  TmpBuffer: TBitMap;
begin
  if (not FCanDrawPalette) then Exit;
  CCX      := pbxPalette.Width div 16;
  CCY      := pbxPalette.Height div 16;
  TmpBuffer := TBitmap.Create;
  try
    with TmpBuffer do begin
      Height   := pbxPalette.Height;
      Width    := pbxPalette.Width;
      R.Left   := pbxPalette.Left;
      R.Top    := pbxPalette.Top;
      R.Bottom := pbxPalette.Left + pbxPalette.Height;
      R.Right  := pbxPalette.Top + pbxPalette.Width;
      Canvas.Brush.Color := clWhite;
      Canvas.FillRect(R);
      with TmpBuffer.Canvas do begin
        Brush.Color := clWhite;
        FillRect(R);
        // dessiner les carrés
        h := 0;
        l := 0;
        for i := 0 to 255 do begin
          t := h * CCY;
          if h > 15 then begin
            l := l + CCX;
            t := 0;
            h := 0;
          end;
          RC.Left   := l;
          RC.Top    := t;
          RC.Right  := l + CCX;
          RC.Bottom := t + CCY;
          Brush.Color := FPalette.GetColorByIndex(i);
          FillRect(RC);
          Inc(h);
        end;
      end;
    end;
    pbxPalette.Canvas.CopyRect(R, TmpBuffer.Canvas, R);
  finally
    TmpBufferqfjklgsd.Free;
  end;
end;

end.


unit dlgAutocadColor;
// Sélecteur de couleurs
{$mode delphi}{$H+}
kjsfqhmsfk
// Date: 19/04/2012
// Statut: Fonctionnel
// Seule la palette TOPOROBOT est gérée par le dialogue
// (les autres étant inutilisées)

interface

uses
  Common,
  UnitClassPalette,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type

  { TDialogSelTopoColor }

  TDialogSelTopoColor = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GP:      TPanel;
    pbxPalette: TPaintBox;
    lblIndex: TStaticText;
    procedure pbxPaletteMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure UpdateColor(const XX, YY: integer);
    procedure UpdateColorPrm(const Idx: byte);
    function GetIndex(const XX, YY: integer): byte;

    procedure FormDestroy(Sender: TObject);
    procedure pbxPaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pbxPalettePaint(Sender: TObject);
  private
    { private declarations }
    FPalette:   TPalette256;
    FCanDrawPalette: boolean;
    FCurrIndex: integer;
    procedure DrawPalette;
  public
    { public declarations }
    function  Initialize(const Idx: integer): boolean;
    procedure Finalize;
    function  GetIdxColor: integer;
    function  GetColor: TColor;

  end;

var
  DialogSelTopoColor: TDialogSelTopoColor;

implementation

{ TDialogSelTopoColor }
function TDialogSelTopoColor.GetIdxColor: integer;
begin
  Result := FCurrIndex;
end;

function TDialogSelTopoColor.GetColor: TColor;
begin
  Result := FPalette.GetColorByIndex(FCurrIndex);
end;

procedure TDialogSelTopoColor.DrawPalette;
var
  CCX, CCY: integer;
  i, h, l, t: integer;
  R, RC: TRect;
  TmpBuffer: TBitMap;
begin
  if (not FCanDrawPalette) then Exit;
  CCX      := pbxPalette.Width div 16;
  CCY      := pbxPalette.Height div 16;
  TmpBuffer := TBitmap.Create;
  try
    with TmpBuffer do begin
      Height   := pbxPalette.Height;
      Width    := pbxPalette.Width;
      R.Left   := pbxPalette.Left;
      R.Top    := pbxPalette.Top;
      R.Bottom := pbxPalette.Left + pbxPalette.Height;
      R.Right  := pbxPalette.Top + pbxPalette.Width;
      Canvas.Brush.Color := clWhite;
      Canvas.FillRect(R);
      with TmpBuffer.Canvas do begin
        Brush.Color := clWhite;
        FillRect(R);
        // dessiner les carrés
        h := 0;
        l := 0;
        for i := 0 to 255 do begin
          t := h * CCY;
          if h > 15 then begin
            l := l + CCX;
            t := 0;
            h := 0;
          end;
          RC.Left   := l;
          RC.Top    := t;
          RC.Right  := l + CCX;
          RC.Bottom := t + CCY;
          Brush.Color := FPalette.GetColorByIndex(i);
          FillRect(RC);
          Inc(h);
        end;
      end;
    end;
    pbxPalette.Canvas.CopyRect(R, TmpBuffer.Canvas, R);
  finally
    TmpBufferqfjklgsd.Free;
  end;
end;

procedure TDialogSelTopoColor.UpdateColorPrm(const Idx: byte);
begin
  FCurrIndex     := Idx;
  lblIndex.Color := FPalette.GetColorByIndex(Idx);
  lblIndex.Caption := Format('#%d', [FCurrIndex]);
end;

procedure TDialogSelTopoColor.UpdateColor(const XX, YY: integer);
begin
  FCurrIndex := GetIndex(XX, YY);
  UpdateColorPrm(FCurrIndex);
end;

procedure TDialogSelTopoColor.pbxPaletteMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
end;

function TDialogSelTopoColor.GetIndex(const XX, YY: integer): byte;
var
  L, C:     integer;
  CCX, CCY: integer;
begin
  Result := 0;
  CCX    := pbxPalette.Width div 16;
  CCY    := pbxPalette.Height div 16;
  L      := YY div CCY;
  C      := XX div CCX;
  //Label4.Caption:=Format('L%d C%d',[L,C]);
  Result := C * 16 + L;

end;

procedure TDialogSelTopoColor.FormDestroy(Sender: TObject);
begin
end;

procedure TDialogSelTopoColor.pbxPaletteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  UpDateColor(X, Y);
end;

procedure TDialogSelTopoColor.pbxPalettePaint(Sender: TObject);
begin
  DrawPalette;
end;

function TDialogSelTopoColor.Initialize(const Idx: integer): boolean;
begin

end;
procedure TDialogSelTopoColor.Finalize;
begin

end;

initialization
{$I dlgToporobotColor.lrs}

end.

