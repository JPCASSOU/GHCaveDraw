unit dlgSetTexteAttributs;
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees, Classes, SysUtils, FileUtil, curredit, Forms, Controls,
  Graphics, Dialogs, StdCtrls, EditBtn, Buttons;

type

  { TfrmSelectTexteAttributs }

  TfrmSelectTexteAttributs = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnFontColor: TColorButton;
    btnBackColor: TColorButton;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkUnderline: TCheckBox;
    editHauteurTexte: TCurrencyEdit;
    editFontName: TEdit;
    editAngleRot: TCurrencyEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
  private
    { private declarations }

  public
    { public declarations }
    procedure SetTexteAttributs(const AT: TTexteAttributs);
    function  GetTexteAttributs: TTexteAttributs;
  end;

var
  frmSelectTexteAttributs: TfrmSelectTexteAttributs;

implementation

{$R *.lfm}

{ TfrmSelectTexteAttributs }

procedure TfrmSelectTexteAttributs.SetTexteAttributs(const AT: TTexteAttributs);
begin
  RadioButton1.Checked     := True;
  editFontName.Text        := AT.FontName;
  btnFontColor.ButtonColor := AT.FontColor;
  btnBackColor.ButtonColor := AT.BackColor;
  editHauteurTexte.Value   := AT.HauteurTexte;
  editAngleRot.AsInteger   := AT.AngleRot;
  chkBold.Checked      := (fsBold      in AT.FontStyle);
  chkItalic.Checked    := (fsItalic    in AT.FontStyle);
  chkUnderline.Checked := (fsUnderline in AT.FontStyle);

  case AT.Position of
    1: RadioButton1.Checked := True;
    2: RadioButton2.Checked := True;
    3: RadioButton3.Checked := True;
    4: RadioButton4.Checked := True;
    5: RadioButton5.Checked := True;
    6: RadioButton6.Checked := True;
    7: RadioButton7.Checked := True;
    8: RadioButton8.Checked := True;
    9: RadioButton9.Checked := True;
  else
    RadioButton1.Checked := True;
  end;


end;

function TfrmSelectTexteAttributs.GetTexteAttributs: TTexteAttributs;
begin
  Result.FontName  := Trim(editFontName.Text);
  result.FontColor := btnFontColor.ButtonColor;
  Result.BackColor := btnBackColor.ButtonColor;
  Result.HauteurTexte := editHauteurTexte.Value;
  Result.AngleRot     := editAngleRot.AsInteger;
  Result.FontStyle := [];
  if (chkBold.Checked)      then Result.FontStyle := Result.FontStyle + [fsBold];
  if (chkItalic.Checked)    then Result.FontStyle := Result.FontStyle + [fsItalic];
  if (chkUnderline.Checked) then Result.FontStyle := Result.FontStyle + [fsUnderline];
  if      (RadioButton1.Checked = True) then Result.Position := 1
  else if (RadioButton2.Checked = True) then Result.Position := 2
  else if (RadioButton3.Checked = True) then Result.Position := 3
  else if (RadioButton4.Checked = True) then Result.Position := 4
  else if (RadioButton5.Checked = True) then Result.Position := 5
  else if (RadioButton6.Checked = True) then Result.Position := 6
  else if (RadioButton7.Checked = True) then Result.Position := 7
  else if (RadioButton8.Checked = True) then Result.Position := 8
  else if (RadioButton9.Checked = True) then Result.Position := 9
  else Result.Position := 1;
end;

end.

