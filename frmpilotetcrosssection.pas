unit frmPiloteTCrossSection;

{$mode objfpc}{$H+}

interface

uses
  GHCD_Types,
  QUnitCrossSection,
  Classes, SysUtils, FileUtil, curredit, SynEdit, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    editIDGroupe: TCurrencyEdit;
    SynEdit1: TSynEdit;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  SC: TCrossSection;
  B: TBaseStation;
  LS: TStringList;
  n, i: Integer;
  MyTexte: TCSTexte;
begin
  B.IDStation := 10010020;
  B.IDTerrain := 'LLANFAIRPWLL';
  B.PosStation.X := 403460.00;
  B.PosStation.Y := 3089600.00;
  B.PosStation.Z := 540.00;

  LS := TStringList.Create;
  LS.Clear;
  SC := TCrossSection.Create();
  try
    SC.Initialiser(B);
    SC.SetIDGroupe(editIDGroupe.AsInteger);
    SC.SetDecalageXY(-23.25, 13.78);
    // on ajoute quelques objets ici
    SC.addSimpleLigneByValues(1, -12.255, -9.881, 11.022, 3.274);
    SC.addSimpleLigneByValues(4,  -2.854, -3.250,  1.001, 6.660);
    SC.addSimpleLigneByValues(1, -18.255,  3.325,  8.022, 0.274);
    SC.addSimpleLigneByValues(1,  -1.666, -1.881, 12.022, 0.365);




    SC.addTexteByValues(3, -4.02, -1.05, 'miaou');
    SC.addTexteByValues(1, -3.06,  1.05, 'fatche');
    SC.addTexteByValues(4, -8.47,  11.98, 'blaireau');



    SC.Serialiser(LS);
    SC.Finaliser();
  finally
    SC.Free;
  end;

  // peupler le memo
  SynEdit1.ClearAll;

  n := LS.Count;
  for i := 0 to n - 1 do
  begin
    SynEdit1.Lines.add(LS.Strings[i]);
  end;
  LS.Clear;
  LS.Free;
end;

end.

