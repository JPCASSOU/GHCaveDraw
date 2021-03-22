unit frmDistanceEntreDeuxPoints;
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  StructuresDonnees,
  ToporobotClasses2012,
  UnitEntitesExtended, UnitObjetSerie, CadreListeNearStations,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons;

type

  { TdlgDistanceBetweenTwoStations }

  TdlgDistanceBetweenTwoStations = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    CdrListeNearestStations1: TCdrListeNearestStations;
    chkDoPerformAdditionalAction: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbDistance: TStaticText;
    lbAzimut: TStaticText;
    lbPente: TStaticText;
    lbSerieOfStation1: TStaticText;
    lbSerieOfStation2: TStaticText;
    lbXStation2: TStaticText;
    lbDeltaX: TStaticText;
    lbYStation1: TStaticText;
    lbYStation2: TStaticText;
    lbDeltaY: TStaticText;
    lbZStation1: TStaticText;
    lbZStation2: TStaticText;
    lbDeltaZ: TStaticText;
    lbStation1: TStaticText;
    lbXStation1: TStaticText;
    lbStation2: TStaticText;
    lbIDStation1: TStaticText;
    lbIDStation2: TStaticText;
    Panel1: TPanel;
    lbDescroAction: TStaticText;
    procedure btnOKClick(Sender: TObject);
  private
    FDocTopo   : TToporobotStructure2012;
    FBDDEntites: TBDDEntites;
    FStation1, FStation2: TBaseStation;
    procedure PutStation1InForm(const S: TBaseStation);
    procedure PutStation2InForm(const S: TBaseStation);
    procedure PutDistsInForm(const S2: TBaseStation);

  public
    function Initialiser(const FD: TToporobotStructure2012;
                         const FE: TBDDEntites;
                         const S1, S2: TBaseStation;
                         const DescriptionAction: string;
                         const CaptionAdditionalAction: string): boolean;
    function DoAllowAdditionalAction(): boolean;
    procedure Finaliser();
  end;

var
  dlgDistanceBetweenTwoStations: TdlgDistanceBetweenTwoStations;

implementation
const
  FMT_COORDS = '%.3f';
  FMT_ANGLES = '%.6f';

{$R *.lfm}

{ TdlgDistanceBetweenTwoStations }

procedure TdlgDistanceBetweenTwoStations.btnOKClick(Sender: TObject);
begin

end;

procedure TdlgDistanceBetweenTwoStations.PutStation1InForm(const S: TBaseStation);
var
  QIdxNameSpace, QNoSerie: integer;
begin
  DecomposeNumeroSerie(S.Entite_Serie, QIdxNameSpace, QNoSerie);
  lbStation1.caption   := Format('%d.%d@%d', [QNoSerie, S.Entite_Station, QIdxNameSpace]);
  lbIDStation1.Caption := S.IDTerrain;
  lbXStation1.Caption  := Format(FMT_COORDS, [S.PosStation.X]);
  lbYStation1.Caption  := Format(FMT_COORDS, [S.PosStation.Y]);
  lbZStation1.Caption  := Format(FMT_COORDS, [S.PosStation.Z]);
end;

procedure TdlgDistanceBetweenTwoStations.PutStation2InForm(const S: TBaseStation);
var
  QIdxNameSpace, QNoSerie: integer;
begin
  DecomposeNumeroSerie(S.Entite_Serie, QIdxNameSpace, QNoSerie);
  lbStation2.caption   := Format('%d.%d@%d', [QNoSerie, S.Entite_Station, QIdxNameSpace]);
  lbIDStation2.Caption := S.IDTerrain;
  lbXStation2.Caption  := Format(FMT_COORDS, [S.PosStation.X]);
  lbYStation2.Caption  := Format(FMT_COORDS, [S.PosStation.Y]);
  lbZStation2.Caption  := Format(FMT_COORDS, [S.PosStation.Z]);
end;

procedure TdlgDistanceBetweenTwoStations.PutDistsInForm(const S2: TBaseStation);
var
  dx, dy, dz, QDist, QAz, QPente: Double;
begin
  dx := S2.PosStation.X - FStation1.PosStation.X;
  dy := S2.PosStation.Y - FStation1.PosStation.Y;
  dz := S2.PosStation.Z - FStation1.PosStation.Z;
  GetBearingInc(dx, dy, dz, QDist, QAz, QPente, 360.00, 360.00);
  lbDeltaX.Caption := Format(FMT_COORDS, [dx]);
  lbDeltaY.Caption := Format(FMT_COORDS, [dy]);
  lbDeltaZ.Caption := Format(FMT_COORDS, [dz]);

  lbDistance.Caption := Format(FMT_COORDS, [QDist]);
  lbAzimut.Caption   := Format(FMT_ANGLES, [QAz]);
  lbPente.Caption    := Format(FMT_ANGLES, [QPente]);
end;

function TdlgDistanceBetweenTwoStations.Initialiser(const FD: TToporobotStructure2012;
                                                    const FE: TBDDEntites;
                                                    const S1, S2: TBaseStation;
                                                    const DescriptionAction: string;
                                                    const CaptionAdditionalAction: string): boolean;

var
  QQ1, QQ2: Boolean;
  MySerieOfStation1, MySerieOfStation2: TObjSerie;
  QIdx1, QIdx2: integer;

  procedure S666(const L: TStaticText; const SR: TObjSerie);
  begin
    L.Caption := Format('%d - %s', [SR.GetNumeroDeSerie(), SR.GetNomSerie()]);
  end;
begin
  result := False;
  // description de l'action (calcul de distance, accrochage d'une série)
  lbDescroAction.Caption := DescriptionAction;
  FDocTopo    := FD;
  FBDDEntites := FE;
  FStation1   := S1;
  FStation2   := S2;
  QQ1 := FDocTopo.GetSerieByNumeroSerie(FStation1.Entite_Serie, MySerieOfStation1, QIdx1);
  QQ2 := FDocTopo.GetSerieByNumeroSerie(FStation2.Entite_Serie, MySerieOfStation2, QIdx2);

  S666(lbSerieOfStation1, MySerieOfStation1);
  S666(lbSerieOfStation2, MySerieOfStation2);
  PutStation1InForm(FStation1);
  PutStation2InForm(FStation2);
  PutDistsInForm(FStation2);
  // action additionnelle facultative
  (*
  Bjr à vous,

Ma boîte mail est actuellement saturée de spams. La mention SPELEOLOGIE est insuffisante
Pour y mettre fin, j'ai mis en place un système de codes GUID, à rappeler dans tous les messages.

Voici le vôtre: 5D078743-4D18-47B2-A721-2AAEEF794E07

Sans ce GUID, tous vos messages seront détruits au niveau serveur sans être lus et il faudra passer par un 0899 à 2.99 euros/appel pour obtenir un GUID

Utiliser de préférence la fonction Répondre.

  //*)
  chkDoPerformAdditionalAction.Visible := false;
  if (CaptionAdditionalAction <> '') then
  begin
    chkDoPerformAdditionalAction.Caption := CaptionAdditionalAction;
    chkDoPerformAdditionalAction.Checked := True;
    chkDoPerformAdditionalAction.Visible := True;
  end;
  // cadre near stations - A initialiser avec la station d'extrémité S2
  CdrListeNearestStations1.Initialiser(FBDDEntites, FStation2, false, 30.00);
  result := true;
end;

function TdlgDistanceBetweenTwoStations.DoAllowAdditionalAction(): boolean;
begin
  result := chkDoPerformAdditionalAction.Checked;
end;

procedure TdlgDistanceBetweenTwoStations.Finaliser();
begin
  CdrListeNearestStations1.Finaliser();
end;

end.

