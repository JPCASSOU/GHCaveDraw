unit dlgDistanceEntreStations;
// 17/01/2014: Nouveau dialogue Distance
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees, Common,
  UnitEntitesExtended,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls;
type TStationDepartOuArrivee = (sdaDEPART, sdaARRIVEE);
type TModeRechercheStation = (rstBY_SER_ST, rstBY_ID_LITTERAL);
type

  { TfrmDistanceStations }

  TfrmDistanceStations = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    editIDLitteralSt1: TEdit;
    editIDLitteralSt2: TEdit;
    editSt1: TCurrencyEdit;
    editSt2: TCurrencyEdit;
    editPt1: TCurrencyEdit;
    editPt2: TCurrencyEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbDistanceXY: TStaticText;
    lbAzimut: TStaticText;
    lbInclinaison: TStaticText;
    lbXStation1: TStaticText;
    lbXStation2: TStaticText;
    lbDistanceXYZ: TStaticText;
    lbDX: TStaticText;
    lbYStation1: TStaticText;
    lbYStation2: TStaticText;
    lbDY: TStaticText;
    lbZStation1: TStaticText;
    lbZStation2: TStaticText;
    lbDZ: TStaticText;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private
    { private declarations }
    FBDDEntites: TBDDEntites;
    FStation1: TEntiteEtendue;
    FStation2: TEntiteEtendue;
    procedure CalcEtAfficheDistance(const QST1, QST2: TEntiteEtendue);
    function  RechercheStation(const S, P: integer; const LB: string; const Mode: TModeRechercheStation; const Q: TStationDepartOuArrivee): boolean;
  public
    { public declarations }
    procedure SetBDDEntites(const BD: TBDDEntites);
    procedure SetStations(const S1, S2: TEntiteEtendue);
  end;

var
  frmDistanceStations: TfrmDistanceStations;

implementation

{$R *.lfm}

{ TfrmDistanceStations }

procedure TfrmDistanceStations.Button1Click(Sender: TObject);
begin
  if (RechercheStation(editSt1.AsInteger, editPt1.AsInteger, editIDLitteralSt1.Text, rstBY_SER_ST, sdaDEPART)) then CalcEtAfficheDistance(FStation1, FStation2);
end;

procedure TfrmDistanceStations.Button2Click(Sender: TObject);
begin
  if (RechercheStation(editSt2.AsInteger, editPt2.AsInteger, editIDLitteralSt2.Text, rstBY_SER_ST, sdaARRIVEE)) then CalcEtAfficheDistance(FStation1, FStation2);
end;

procedure TfrmDistanceStations.Button3Click(Sender: TObject);
begin
  if (RechercheStation(editSt1.AsInteger, editPt1.AsInteger, editIDLitteralSt1.Text, rstBY_ID_LITTERAL, sdaDEPART)) then CalcEtAfficheDistance(FStation1, FStation2);
end;

procedure TfrmDistanceStations.Button4Click(Sender: TObject);
begin
  if (RechercheStation(editSt2.AsInteger, editPt2.AsInteger, editIDLitteralSt2.Text, rstBY_ID_LITTERAL, sdaARRIVEE)) then CalcEtAfficheDistance(FStation1, FStation2);
end;

procedure TfrmDistanceStations.FormShow(Sender: TObject);
begin
  self.Caption  := 'Distance';
  self.Position := poScreenCenter;
end;

procedure TfrmDistanceStations.Label1Click(Sender: TObject);
begin

end;


procedure TfrmDistanceStations.CalcEtAfficheDistance(const QST1, QST2: TEntiteEtendue);
const
  FMT_COORDS = '%.3f';
var
  dx, dy, dz, dp: Double;
  qL, qAZ, qP: double;
begin
  dx := (QST2.Une_Station_2_X - QST1.Une_Station_2_X);
  dy := (QST2.Une_Station_2_Y - QST1.Une_Station_2_Y);
  dz := (QST2.Une_Station_2_Z - QST1.Une_Station_2_Z);
  dp   := Hypot2D(dx, dy);
  //dist := Hypot3D(dx, dy, dz);
  GetBearingInc(dx, dy, dz, qL, qAZ, qP, 360.00, 360.00);

  editSt1.AsInteger := QST1.Entite_Serie;
  editPt1.AsInteger := QST1.Entite_Station;
  editSt2.AsInteger := QST2.Entite_Serie;
  editPt2.AsInteger := QST2.Entite_Station;
  editIDLitteralSt1.Text := QST1.oIDLitteral;
  editIDLitteralSt2.Text := QST2.oIDLitteral;
  lbXStation1.Caption    := Format(FMT_COORDS, [QST1.Une_Station_2_X]);
  lbYStation1.Caption    := Format(FMT_COORDS, [QST1.Une_Station_2_Y]);
  lbZStation1.Caption    := Format(FMT_COORDS, [QST1.Une_Station_2_Z]);

  lbXStation2.Caption    := Format(FMT_COORDS, [QST2.Une_Station_2_X]);
  lbYStation2.Caption    := Format(FMT_COORDS, [QST2.Une_Station_2_Y]);
  lbZStation2.Caption    := Format(FMT_COORDS, [QST2.Une_Station_2_Z]);

  lbDX.Caption           := Format(FMT_COORDS, [dx]);
  lbDY.Caption           := Format(FMT_COORDS, [dy]);
  lbDZ.Caption           := Format(FMT_COORDS, [dz]);

  lbDistanceXY.Caption   := Format(FMT_COORDS, [dp]);
  lbDistanceXYZ.Caption  := Format(FMT_COORDS, [qL]);

  lbAzimut.Caption       := Format('%.3f°', [qAZ]); ;
  lbInclinaison.Caption  := Format('%.3f°', [qP]);
end;

function TfrmDistanceStations.RechercheStation(const S, P: integer; const LB: string;
                                               const Mode: TModeRechercheStation;
                                               const Q: TStationDepartOuArrivee): boolean;
var
  EWE: TEntiteEtendue;
  WU: Boolean;
begin
  Result := False;
  try
    case Mode of
      rstBY_SER_ST       : WU := FBDDEntites.GetEntiteFromSerSt(S, P, EWE);
      rstBY_ID_LITTERAL  : WU := FBDDEntites.GetEntiteFromCle(false, LB, EWE);
    end;
    if (not WU) then Exit;
    case Q of
      sdaDEPART : FStation1 := EWE;
      sdaARRIVEE: FStation2 := EWE;
    end;
    Result := True;
  except
  end;
end;

procedure TfrmDistanceStations.SetBDDEntites(const BD: TBDDEntites);
begin
  FBDDEntites := BD;
end;

procedure TfrmDistanceStations.SetStations(const S1, S2: TEntiteEtendue);
begin
  FStation1 := S1;
  FStation2 := S2;
  CalcEtAfficheDistance(S1, S2);
end;

end.

