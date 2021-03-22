unit dlgColorDialog;

{$mode delphi}{$H+}

interface

uses
  Common,
  Graphics, Classes, SysUtils, FileUtil, LResources, Forms, Controls,
  Dialogs, Buttons, StdCtrls;

type

  { TfrmColorDialog }

  TfrmColorDialog = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    sclR: TScrollBar;
    sclG: TScrollBar;
    sclB: TScrollBar;
    StaticText1: TStaticText;
    StaticText10: TStaticText;
    StaticText11: TStaticText;
    StaticText12: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    lbSampleColor: TStaticText;
    StaticText9: TStaticText;
    procedure FormShow(Sender: TObject);
    procedure sclBChange(Sender: TObject);
    procedure sclGChange(Sender: TObject);
    procedure sclRChange(Sender: TObject);
    procedure StaticText10Click(Sender: TObject);
    procedure StaticText11Click(Sender: TObject);
    procedure StaticText12Click(Sender: TObject);
    procedure StaticText1Click(Sender: TObject);
    procedure StaticText2Click(Sender: TObject);
    procedure StaticText3Click(Sender: TObject);
    procedure StaticText4Click(Sender: TObject);
    procedure StaticText5Click(Sender: TObject);
    procedure StaticText6Click(Sender: TObject);
    procedure StaticText7Click(Sender: TObject);
    procedure StaticText8Click(Sender: TObject);
    procedure StaticText9Click(Sender: TObject);
  private
    { private declarations }
    FCurrColor: TColor;

  public
    { public declarations }
    procedure SetColor(const C: TColor);
    function  GetColor: TColor;
  end; 

var
  frmColorDialog: TfrmColorDialog;

implementation

procedure TfrmColorDialog.StaticText1Click(Sender: TObject);
begin
  SetColor(StaticText1.Color);
end;

procedure TfrmColorDialog.FormShow(Sender: TObject);
begin
  SetColor(FCurrColor);
end;

procedure TfrmColorDialog.sclBChange(Sender: TObject);
begin
   SetColor(RGB(sclR.Position, sclB.Position, sclG.Position));
end;

procedure TfrmColorDialog.sclGChange(Sender: TObject);
begin
  SetColor(RGB(sclR.Position, sclB.Position, sclG.Position));
end;

procedure TfrmColorDialog.sclRChange(Sender: TObject);
begin
  SetColor(RGB(sclR.Position, sclB.Position, sclG.Position));
end;

procedure TfrmColorDialog.StaticText10Click(Sender: TObject);
begin
  SetColor(StaticText10.Color);
end;

procedure TfrmColorDialog.StaticText11Click(Sender: TObject);
begin
  SetColor(StaticText11.Color);
end;

procedure TfrmColorDialog.StaticText12Click(Sender: TObject);
begin
  SetColor(StaticText12.Color);
end;

procedure TfrmColorDialog.StaticText2Click(Sender: TObject);
begin
  SetColor(StaticText2.Color);
end;

procedure TfrmColorDialog.StaticText3Click(Sender: TObject);
begin
  SetColor(StaticText3.Color);
end;

procedure TfrmColorDialog.StaticText4Click(Sender: TObject);
begin
  SetColor(StaticText4.Color);
end;

procedure TfrmColorDialog.StaticText5Click(Sender: TObject);
begin
  SetColor(StaticText5.Color);
end;

procedure TfrmColorDialog.StaticText6Click(Sender: TObject);
begin
  SetColor(StaticText6.Color);
end;

procedure TfrmColorDialog.StaticText7Click(Sender: TObject);
begin
  SetColor(StaticText7.Color);
end;

procedure TfrmColorDialog.StaticText8Click(Sender: TObject);
begin
  SetColor(StaticText8.Color);
end;

procedure TfrmColorDialog.StaticText9Click(Sender: TObject);
begin
  SetColor(StaticText9.Color);
end;

procedure TfrmColorDialog.SetColor(const C: TColor);
var R,G,B: byte;
begin
  FCurrColor := C;
  lbSampleColor.Color:= C;

  R := Red(C);
  G := Green((C);
  B := Blue(C);
  //showmessagefmt('%d %d %d',[R,G,B]);
  (*
  sclR.Position := R;
  sclG.Position := G;
  sclB.Position := B;
  //*)

end;

function TfrmColorDialog.GetColor: TColor;
begin
  result := lbSampleColor.Color;
end;

initialization
//{$I dlgColorDialog.lrs}

end.

