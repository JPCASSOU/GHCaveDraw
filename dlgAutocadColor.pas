unit dlgAutocadColor;

{$mode delphi}{$H+}

interface

uses
  math,
  UnitClassPalette,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, StdCtrls;

type TdlgSelectAutocadColor = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pbxPalette: TPaintBox;
    pnlGP: TPanel;
    lblIndex: TStaticText;
    procedure FormShow(Sender: TObject);
    procedure pbxPaletteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbxPalettePaint(Sender: TObject);
  private
    { private declarations }
    FPalette:   TPalette256;
    FCanDrawPalette: boolean;
    FCurrIndex: integer;
    procedure DrawPalette();
    procedure UpdateColor(const XX, YY: integer);
    procedure UpdateColorPrm(const Idx: byte);
    function  GetIndex(const XX, YY: integer): byte;
    function  MakeLC(out QL, QT: integer):boolean;
  public
    { public declarations }
    function  Initialiser(const Idx: integer): boolean;
    procedure Finaliser();
    function  GetIdxColor(): integer;
    function  GetColor(): TColor;
  end;

var
  dlgSelectAutocadColor: TdlgSelectAutocadColor;

implementation

{$R *.lfm}

{ TdlgSelectAutocadColor }
procedure TdlgSelectAutocadColor.DrawPalette();
var
  CCX, CCY: integer;
  i, h, l, t: integer;
  R, RC: TRect;
  TmpBuffer: TBitMap;
begin
  if (not FCanDrawPalette) then Exit;
  CCX      := pbxPalette.Width  div 16;
  CCY      := pbxPalette.Height div 16;
  TmpBuffer := TBitmap.Create;
  try
    TmpBuffer.Height   := pbxPalette.Height;
    TmpBuffer.Width    := pbxPalette.Width;
    R.Left   := pbxPalette.Left;
    R.Top    := pbxPalette.Top;
    R.Bottom := pbxPalette.Left + pbxPalette.Height;
    R.Right  := pbxPalette.Top + pbxPalette.Width;
    TmpBuffer.Canvas.Pen.Width := 0;
    TmpBuffer.Canvas.Pen.Color := clBlack;
    TmpBuffer.Canvas.Brush.Color := clWhite;
    TmpBuffer.Canvas.FillRect(R);
    TmpBuffer.Canvas.Brush.Color := clWhite;
    TmpBuffer.Canvas.FillRect(R);
    // dessiner les carrés
    h := 0;
    l := 0;
    for i := 0 to 255 do
    begin
      t := h * CCY;
      if h > 15 then
      begin
        l := l + CCX;
        t := 0;
        h := 0;
      end;
      RC.Left   := l;
      RC.Top    := t;
      RC.Right  := l + CCX;
      RC.Bottom := t + CCY;
      TmpBuffer.Canvas.Brush.Color := FPalette.GetColorByIndex(i);
      TmpBuffer.Canvas.FillRect(RC);
      Inc(h);
    end;
    if (self.MakeLC(l, t)) then  // dessin du rectangle couleur sélectionné
    begin
      TmpBuffer.Canvas.Pen.Width := 3;
      TmpBuffer.Canvas.Brush.Color := FPalette.GetColorByIndex(FCurrIndex);
      RC.Left   := l * CCX;
      RC.Top    := t * CCY;
      RC.Right  := (1 + l) * CCX;
      RC.Bottom := (1 + t) * CCY;
      TmpBuffer.Canvas.Rectangle(RC);
    end;
    pbxPalette.Canvas.CopyRect(R, TmpBuffer.Canvas, R);
  finally
    FreeAndNil(TmpBuffer);//TmpBuffer.Free;
  end;
end;
function TdlgSelectAutocadColor.Initialiser(const Idx: integer): boolean;
begin
  self.Caption := 'Palette AutoCAD';
  FCanDrawPalette := False;
  FCurrIndex := Idx;
  FPalette := TPalette256.Create;
  //FPalette.GenerateTOPOROBOTPalette;
  FPalette.GenerateACADPalette();
  FCanDrawPalette := True;
  pbxPalette.Invalidate;
  lblIndex.Caption := IntToStr(FCurrIndex);
end;

procedure TdlgSelectAutocadColor.pbxPaletteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  self.UpDateColor(X, Y);
end;

procedure TdlgSelectAutocadColor.pbxPalettePaint(Sender: TObject);
begin
  self.DrawPalette();
end;

procedure TdlgSelectAutocadColor.UpdateColor(const XX, YY: integer);
begin
  FCurrIndex := GetIndex(XX, YY);
  UpdateColorPrm(FCurrIndex);
end;

procedure TdlgSelectAutocadColor.UpdateColorPrm(const Idx: byte);
begin
  FCurrIndex       := Idx;
  lblIndex.Color   := FPalette.GetColorByIndex(Idx);
  lblIndex.Caption := Format('#%d', [FCurrIndex]);
  pbxPalette.Invalidate;
end;

procedure TdlgSelectAutocadColor.Finaliser();
begin
  try
    ;;
  finally
    FreeAndNil(FPalette);//FPalette.Free;
  end;
end;



procedure TdlgSelectAutocadColor.FormShow(Sender: TObject);
begin
  self.Position := poScreenCenter;
  self.Width    := Min(Screen.Width  - 16, Min(self.Width , 800));
  self.Height   := Min(Screen.Height - 40, Min(self.Height, 600));
end;

function TdlgSelectAutocadColor.GetColor(): TColor;
begin
  Result := FPalette.GetColorByIndex(FCurrIndex);
end;

function TdlgSelectAutocadColor.GetIdxColor(): integer;
begin
  Result := FCurrIndex;
end;

function TdlgSelectAutocadColor.GetIndex(const XX, YY: integer): byte;
var
  CCX, CCY, L, C: Integer;
begin
  Result := 0;
  CCX    := pbxPalette.Width div 16;
  CCY    := pbxPalette.Height div 16;
  L      := YY div CCY;
  C      := XX div CCX;
  Result := C * 16 + L;
end;

function TdlgSelectAutocadColor.MakeLC(out QL, QT: integer): boolean;
begin
  result := false;
  if (FCurrIndex < 0) then Exit;
  QT := FCurrIndex mod 16;
  QL := FCurrIndex div 16;
  result := true;
end;

end.

