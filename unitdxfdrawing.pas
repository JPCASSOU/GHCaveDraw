unit UnitDXFDrawing;

{$INCLUDE CompilationParameters.inc}

interface

uses
  StructuresDonnees,
  Common,
  UnitStructuresDonneesEntites,
  UnitSilhouetteGalerie,
  Classes, SysUtils;

type

{ TDXFOutput }

 TDXFOutput = class
  private
    FpFDXF: TextFile;
    FCoinBasGauche: TPoint3Df;
    FCoinHautDroit: TPoint3Df;




    procedure DXFExtMax();
    procedure DXFExtMin();
    procedure DXFLimMax();
    procedure DXFLimMin();


    procedure wrtLn(const S: string);
  public
    function  Initialiser(const F: string): boolean;
    procedure SetLimitesDessin(const C1, C2: TPoint3Df);
    procedure Finaliser();
    // header et footer
    procedure BeginSection(const NomSection: string; const Tag: integer);
    procedure EndSection(const NomSection: string);
    procedure writeHeader();
    procedure writeFooter();
    // dessins
    procedure DXFDrawCircle(const QLayer: string; const qX, qY, qZ, qR: double);
    procedure DXFDrawLine(const QLayer: string; const qX1, qY1, qZ1, qX2, qY2, qZ2: double);
    procedure DXFDrawTexte(const QLayer: string; const qX1, qY1, qZ1: double; const QTextH: double; const QText: string);
    procedure DXFDrawStation(const QStation: TBaseStation);
    procedure DXFDrawEntrance(const QEntrance: TEntrance);
    procedure DXFDrawSilhouettes(const QSilhouette: TSilhouetteGalerie);
    procedure DXFDrawCenterlineSilhouettes(const QSilhouette: TSilhouetteGalerie);

end;

implementation
const
  LAYER_CENTERLINES     = 'CENTERLINES';
  //LAYER_CENTERLINES_POLY= 'POLYCENTERLINES';
  LAYER_CROSS_SECTIONS  = 'CROSSSECTIONS';
  LAYER_STATIONS        = 'STATIONS';
  LAYER_LBL_STATIONS    = 'LABELSTATIONS';
  LAYER_RADIANT_SHOTS   = 'RADIANTSHOTS';
  LAYER_ARTIFICIEL      = 'TUNNELS';
  LAYER_SURFACE         = 'TOPOSURFACE';
  LAYER_WALLS           = 'WALLS';
  LAYER_ENTRANCES       = 'ENTRANCES';
  LAYER_SILHOUETTES     = 'SILHOUETTES';

const FMT1 = '%.8f';

// dessin d'entités DXF




{ TDXFOutput }

procedure TDXFOutput.wrtLn(const S: string);
begin
  Write(FpFDXF, S + #13+#10);
end;

function TDXFOutput.Initialiser(const F: string): boolean;
begin
  Result := False;
  try
    AssignFile(FpFDXF, F);
    ReWrite(FpFDXF);
    Result := True;
  except

  end;
end;

procedure TDXFOutput.SetLimitesDessin(const C1, C2: TPoint3Df);
begin
  FCoinBasGauche := C1;
  FCoinHautDroit := C2;
end;



procedure TDXFOutput.Finaliser();
begin
  try

  finally
    CloseFile(FpFDXF);
  end;
end;

procedure TDXFOutput.BeginSection(const NomSection: string; const Tag: integer);
begin
  wrtLn('   0');
  wrtLn('SECTION');
  wrtLn(Format('  %d', [Tag]));
  wrtLn(UpperCase(NomSection));
end;


procedure TDXFOutput.EndSection(const NomSection: string);
begin
  wrtLn('  0');
  wrtLn('ENDSEC');
end;
procedure TDXFOutput.DXFExtMin();
begin

  wrtLn('9');
  wrtLn('$EXTMIN');
  wrtLn('10');
  wrtLn(Format(FMT1, [FCoinBasGauche.X]));
  wrtLn('20');
  wrtLn(Format(FMT1, [FCoinBasGauche.Y]));
  wrtLn('30');
  wrtLn(Format(FMT1, [FCoinBasGauche.Z]));
  //wrtLn('  0');
end;

procedure TDXFOutput.DXFExtMax();
begin

  wrtLn('9');
  wrtLn('$EXTMAX');
  wrtLn('10');
  wrtLn(Format(FMT1, [FCoinHautDroit.X]));
  wrtLn('20');
  wrtLn(Format(FMT1, [FCoinHautDroit.Y]));
  wrtLn('30');
  wrtLn(Format(FMT1, [FCoinHautDroit.Z]));
  //wrtLn('  0');
end;
procedure TDXFOutput.DXFLimMin();
begin

  wrtLn('9');
  wrtLn('$LIMMIN');
  wrtLn('10');
  wrtLn(Format(FMT1, [FCoinBasGauche.X]));
  wrtLn('20');
  wrtLn(Format(FMT1, [FCoinBasGauche.Y]));
  wrtLn('30');
  wrtLn(Format(FMT1, [FCoinBasGauche.Z]));
  //wrtLn('  0');
end;

procedure TDXFOutput.DXFLimMax();
begin

  wrtLn('9');
  wrtLn('$LIMMAX');
  wrtLn('10');
  wrtLn(Format(FMT1, [FCoinHautDroit.X]));
  wrtLn('20');
  wrtLn(Format(FMT1, [FCoinHautDroit.Y]));
  wrtLn('30');
  wrtLn(Format(FMT1, [FCoinHautDroit.Z]));
  //wrtLn('  0');
end;

procedure TDXFOutput.writeHeader;
  procedure WrtCouche(const QN: string; const AcadColor: integer; const LineWidth: integer);
  begin
    wrtLn('LAYER');                     // LAYER
    wrtLn('  2');                       //   2
    wrtLn(QN);
    wrtLn(' 70');
    wrtLn(Format('    %d', [AcadColor]));
    wrtLn(' 62 ');
    wrtLn(Format('    %d', [LineWidth]));
    wrtLn('  6');
    wrtLn('CONTINUOUS');
    wrtLn('  0');
  end;

begin
  wrtLn('  0');                       // 0
  wrtLn('SECTION');                   // SECTION
  wrtLn('  2');                       //   2
  wrtLn('HEADER');                    // HEADER
  wrtLn('  9');                       //   9
  wrtLn('$ACADVER');                  // $ACADVER
  wrtLn('  1');                       //   1
  wrtLn('AC1006');                    // AC1006
  DXFExtMin();
  DXFExtMax();
  DXFLimMin();
  DXFLimMax();
  wrtLn('  0');                       //   0
  wrtLn('ENDSEC');                    // ENDSEC
  wrtLn('  0');                       //   0
  wrtLn('SECTION');                   // SECTION
  wrtLn('  2');                       //   2
  wrtLn('TABLES');                    // TABLES
  wrtLn('  0');                       //   0
  wrtLn('TABLE');                     // TABLE
  wrtLn('  2');                       //   2
  wrtLn('LAYER');                     // LAYER
  wrtLn('  0');                       //   0
  WrtCouche(LAYER_CENTERLINES, 80, 5);
  WrtCouche(LAYER_WALLS, 78, 5);
  WrtCouche(LAYER_SILHOUETTES, 84, 1);
  WrtCouche(LAYER_ARTIFICIEL, 64, 1);
  WrtCouche(LAYER_SURFACE, 64, 1);
  WrtCouche(LAYER_RADIANT_SHOTS, 64, 1);
  WrtCouche(LAYER_CROSS_SECTIONS, 64, 1);
  WrtCouche(LAYER_STATIONS, 64, 1);
  WrtCouche(LAYER_LBL_STATIONS, 83, 1);
  WrtCouche(LAYER_ENTRANCES, 64, 1);

  wrtLn('ENDTAB');
  wrtLn('  0');                       //
  wrtLn('ENDSEC');
  wrtLn('  0');                       //
  wrtLn('SECTION');
  wrtLn('  2');                       //
  wrtLn('ENTITIES');
  wrtLn('  0');                       //

end;

procedure TDXFOutput.writeFooter;
begin
  wrtLn('ENDSEC');
  wrtLn('  0');
  wrtLn('EOF');
end;

// dessin des objets de base
procedure TDXFOutput.DXFDrawCircle(const QLayer: string; const qX, qY, qZ, qR: double);
begin

  wrtLn('CIRCLE');
  wrtLn('8');
  wrtLn(QLayer);
  wrtLn(' 10');
  wrtLn(Format(FMT1, [qX]));
  wrtLn(' 20');
  wrtLn(Format(FMT1, [qY]));
  wrtLn(' 30');
  wrtLn(Format(FMT1, [qZ]));
  wrtLn(' 40');
  wrtLn(Format(FMT1, [qR]));
  wrtLn('  0');
end;

procedure TDXFOutput.DXFDrawEntrance(const QEntrance: TEntrance);
begin
  DXFDrawCircle(LAYER_ENTRANCES, QEntrance.eXEntree, QEntrance.eYEntree, QEntrance.eZEntree, 0.50);
  DXFDrawCircle(LAYER_ENTRANCES, QEntrance.eXEntree, QEntrance.eYEntree, QEntrance.eZEntree, 0.30);
  DXFDrawTexte(LAYER_ENTRANCES, QEntrance.eXEntree + 0.40,
                                QEntrance.eYEntree + 0.40,
                                QEntrance.eZEntree,
                                2.00,
                                QEntrance.eNomEntree);

end;
procedure TDXFOutput.DXFDrawTexte(const QLayer: string; const qX1, qY1, qZ1: double; const QTextH: double; const QText: string);
begin
  wrtLn('TEXT');
  wrtLn('8');
  wrtLn(QLayer);
  wrtLn(' 10');
  wrtLn(Format(FMT1, [qX1]));
  wrtLn(' 20');
  wrtLn(Format(FMT1, [qY1]));
  wrtLn(' 30');
  wrtLn(Format(FMT1, [qZ1]));
  wrtLn(' 40');
  wrtLn(Format(FMT1, [QTextH]));
  wrtLn(' 1');
  wrtLn(QText);
  wrtLn(' 0');
end;

procedure TDXFOutput.DXFDrawLine(const QLayer: string; const qX1, qY1, qZ1, qX2, qY2, qZ2: double);
begin

  wrtLn('LINE');
  wrtLn('8');
  wrtLn(QLayer);
  wrtLn(' 10');
  wrtLn(Format(FMT1, [qX1]));
  wrtLn(' 20');
  wrtLn(Format(FMT1, [qY1]));
  wrtLn(' 30');
  wrtLn(Format(FMT1, [qZ1]));
  wrtLn(' 11');
  wrtLn(Format(FMT1, [qX2]));
  wrtLn(' 21');
  wrtLn(Format(FMT1, [qY2]));
  wrtLn(' 31');
  wrtLn(Format(FMT1, [qZ2]));
  wrtLn('  0');
end;

procedure TDXFOutput.DXFDrawStation(const QStation: TBaseStation);
var
  QCouche: String;
  WU: Boolean;
begin
  WU := QStation.Type_Entite in [tgDEFAULT, tgFOSSILE, tgVADOSE, tgENNOYABLE, tgSIPHON, tgSURFACE, tgTUNNEL, tgMINE];
  if (WU) then
  begin
    DXFDrawLine(LAYER_CROSS_SECTIONS,
                QStation.PosOPD.X, QStation.PosOPD.X, QStation.PosExtr0.Z,
                QStation.PosPD.X, QStation.PosPD.Y, QStation.PosStation.Z);
    DXFDrawCircle(LAYER_STATIONS, QStation.PosStation.X, QStation.PosStation.Y, QStation.PosStation.Z, 0.25);
    DXFDrawTexte(LAYER_LBL_STATIONS,
                 QStation.PosStation.X + 0.28, QStation.PosStation.Y + 0.28, QStation.PosStation.Z,
                 1.20, QStation.IDTerrain);
  end;
  if (QStation.Type_Entite = tgVISEE_RADIANTE) then
  begin
    DXFDrawLine(LAYER_RADIANT_SHOTS,
                QStation.PosExtr0.Y, QStation.PosExtr0.Y, QStation.PosExtr0.Z,
                QStation.PosStation.X, QStation.PosStation.Y, QStation.PosStation.Z);

  end;
end;

procedure TDXFOutput.DXFDrawSilhouettes(const QSilhouette: TSilhouetteGalerie);
var
  Nb, i: Integer;
  PP0, PP1: TPoint2Df;
begin
  if (not QSilhouette.BuildPolygoneParois) then Exit;
  Nb := QSilhouette.GetNbPointsPolygoneParois;
  AfficherMessageErreur(Format(' --  %s - %d; %d', [QSilhouette.GetNomSilhouette(), QSilhouette.GetNbPtsParois(), QSilhouette.GetNbPointsPolygoneParois()]));
  wrtLn('POLYLINE');
  wrtLn('8');
  wrtLn(LAYER_SILHOUETTES);
  wrtLn(' 10');
  wrtLn(Format(FMT1, [0.00]));
  wrtLn(' 20');
  wrtLn(Format(FMT1, [0.00]));
  wrtLn(' 30');
  wrtLn(Format(FMT1, [0.00]));
  wrtLn(' 39');
  wrtLn(Format(FMT1, [0.25]));   // largeur
  wrtLn(' 70');
  wrtLn(Format('  %d', [1]));   // Polyline flag (bit-coded); default is 0:
                                // 1 = This is a closed polyline (or a polygon mesh closed in the M direction).
                                // 2 = Curve-fit vertices have been added.
                                // 4 = Spline-fit vertices have been added.
                                // 8 = This is a 3D polyline.
                                // 16 = This is a 3D polygon mesh.
                                // 32 = The polygon mesh is closed in the N direction.
                                // 64 = The polyline is a polyface mesh.
                                // 128 = The linetype pattern is generated continuously around the vertices of this polyline.

  for i := 0 to Nb - 1 do
  begin
    PP0 := QSilhouette.GetPointPolygoneParois(i);
    wrtLn('  0');
    wrtLn('VERTEX');
    wrtLn('8');
    wrtLn(LAYER_SILHOUETTES);
    wrtLn(' 10');
    wrtLn(Format(FMT1, [PP0.X]));
    wrtLn(' 20');
    wrtLn(Format(FMT1, [PP0.Y]));
    wrtLn(' 30');
    wrtLn(Format(FMT1, [0.00]));
    //PP1 := QSilhouette.GetPointPolygoneParois(i);
    //DXFDrawLine(LAYER_SILHOUETTES, PP0.X, PP0.Y, 0.00, PP1.X, PP1.Y, 0.00);
  end;
  wrtLn('  0');
  wrtLn('SEQEND');
  wrtLn('8');
  wrtLn(LAYER_SILHOUETTES);
  wrtLn('  0');

  (*
  // hachures
  wrtLn('HATCH');
  wrtLn('8');
  wrtLn(LAYER_SILHOUETTES);
  wrtLn(' 10');
  wrtLn(Format(FMT1, [0.00]));
  wrtLn(' 20');
  wrtLn(Format(FMT1, [0.00]));
  wrtLn(' 30');
  wrtLn(Format(FMT1, [0.00]));
  wrtLn(' 70');   // Solid fill flag (solid fill = 1; pattern fill = 0)
  wrtLn(Format('  %d', [1])) ;
  wrtLn(' 71');
  wrtLn(Format('  %d', [0])) ;
  wrtLn(' 75');   // Hatch style: 0 = Hatch "odd parity" area (Normal style); 1 = Hatch outermost area only (Outer style); 2 = Hatch through entire area (Ignore style)
  wrtLn(Format('  %d', [0])) ;
  wrtLn(' 98');
  wrtLn(Format('  %d', [Nb])) ;
  (*
  for i := 0 to Nb -1 do
  begin
    PP0 := QSilhouette.GetPointPolygoneParois(i);
    wrtLn(' 10');
    wrtLn(Format(FMT1, [PP0.X]));
  end;

  for i := 0 to Nb -1 do
  begin
    PP0 := QSilhouette.GetPointPolygoneParois(i);
    wrtLn(' 20');
    wrtLn(Format(FMT1, [PP0.Y]));
  end;
  wrtLn('  0');
  //*)
  (*
  for i := 0 to Nb - 1 do
  begin
    PP0 := QSilhouette.GetPointPolygoneParois(i);
    wrtLn('  0');
    wrtLn('VERTEX');
    wrtLn('8');
    wrtLn(LAYER_SILHOUETTES);
    wrtLn(' 10');
    wrtLn(Format(FMT1, [PP0.X]));
    wrtLn(' 20');
    wrtLn(Format(FMT1, [PP0.Y]));
    wrtLn(' 30');
    wrtLn(Format(FMT1, [0.00]));
    //PP1 := QSilhouette.GetPointPolygoneParois(i);
    //DXFDrawLine(LAYER_SILHOUETTES, PP0.X, PP0.Y, 0.00, PP1.X, PP1.Y, 0.00);
  end;
  wrtLn('  0');
  wrtLn('SEQEND');
  wrtLn('8');
  wrtLn(LAYER_SILHOUETTES);
  wrtLn('  0');

  //*)
end;
procedure TDXFOutput.DXFDrawCenterlineSilhouettes(const QSilhouette: TSilhouetteGalerie);
var
  Nb: Integer;
  procedure EWE(const WU: byte; const QLayer: string);
  var
    i: integer;
    PP0: TPtsSectionTransversale;
    QAT: TPoint3Df;
  begin
    AfficherMessageErreur(Format(' --  %s - %d; %d', [QSilhouette.GetNomSilhouette(), QSilhouette.GetNbPtsParois(), QSilhouette.GetNbPointsPolygoneParois()]));
    wrtLn('POLYLINE');
    wrtLn('8');
    wrtLn(QLayer);
    wrtLn(' 10');
    wrtLn(Format(FMT1, [0.00]));
    wrtLn(' 20');
    wrtLn(Format(FMT1, [0.00]));
    wrtLn(' 30');
    wrtLn(Format(FMT1, [0.00]));
    wrtLn(' 39');
    wrtLn(Format(FMT1, [0.0]));   // largeur
    wrtLn(' 70');
    wrtLn(Format('  %d', [0]));   // Polyline flag (bit-coded); default is 0:
                                  // 1 = This is a closed polyline (or a polygon mesh closed in the M direction).
                                  // 2 = Curve-fit vertices have been added.
                                  // 4 = Spline-fit vertices have been added.
                                  // 8 = This is a 3D polyline.
                                  // 16 = This is a 3D polygon mesh.
                                  // 32 = The polygon mesh is closed in the N direction.
                                  // 64 = The polyline is a polyface mesh.
                                  // 128 = The linetype pattern is generated continuously around the vertices of this polyline.

    for i := 0 to Nb - 1 do
    begin
      PP0 := QSilhouette.GetPtsParois(i);
      case WU of
        0: QAT := PP0.PosStation;//MakeTPoint3Df(PP0.Station.X, PP0.Station.Y, PP0.Station.Z);
        1: QAT := MakeTPoint3Df(PP0.ParoiGaucheX, PP0.ParoiGaucheY, PP0.PosStation.Z);
        2: QAT := MakeTPoint3Df(PP0.ParoiDroiteX, PP0.ParoiDroiteY, PP0.PosStation.Z);
      end;

      wrtLn('  0');
      wrtLn('VERTEX');
      wrtLn('8');
      wrtLn(QLayer);
      wrtLn(' 10');
      wrtLn(Format(FMT1, [QAT.X]));
      wrtLn(' 20');
      wrtLn(Format(FMT1, [QAT.Y]));
      wrtLn(' 30');
      wrtLn(Format(FMT1, [QAT.Z]));
    end;
    wrtLn('  0');
    wrtLn('SEQEND');
    wrtLn('8');
    wrtLn(QLayer);
    wrtLn('  0');
  end;
begin
  if (not QSilhouette.BuildPolygoneParois) then Exit;
  Nb := QSilhouette.GetNbPtsParois();
  EWE(0, LAYER_CENTERLINES);
  EWE(1, LAYER_WALLS);
  EWE(2, LAYER_WALLS);
end;

end.

