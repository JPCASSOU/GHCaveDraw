unit unitGenererFichePointTopo;

{$INCLUDE CompilationParameters.inc}

interface

uses
  Classes
  , SysUtils
  , Graphics
  , GHCD_Types
  , GeneralFunctions
  , UnitDocDessin
  , UnitListesSimplesWithGeneriques
  , UnitTGHCaveDrawDrawingContext
  , BGRABitmap
  , BGRABitmapTypes
  , BGRACanvas
  , BGRAPhongTypes
  , BGRAGradients
  , Printers
  //, OSPrinters, // OSPrinters est INDISPENSABLE ici !!
  , PrintersDlgs
  , Printer4Lazarus
  , OSPrinters
  ;


type

{ TFichePointTopo }

 { TFichesPointTopo }

 TFichesPointTopo = class
  private
    FMyDocDessin   : TDocumentDessin;
    FListeStations : TListeSimple<TBaseStation>;
    FRedessinInProcess: boolean;

    FTextures       : TTexturesPolygonObject;

    FImgWidth, FImgHeight: integer;

    FParamsVue2D     : TParamsVue2D;
    procedure CreerUneFiche(const BP: TBaseStation);

    function QExporterTopoEnImage(const LimitesDessin: TRect2Df;
                                  out QGHCaveDrawDrawingContext: TGHCaveDrawDrawingContext): boolean;
    function SelectionnerImprimante(var MyPrinter: TPrinter): boolean;

  public
    function  Initialiser(const FD: TDocumentDessin;
                          const QTextures: TTexturesPolygonObject;
                          const FP: TParamsVue2D): boolean;


    function  getBaseSt(const Idx: integer): TBaseStation; inline;
    function  getNbBaseSts(): integer; inline;
    procedure AddStation(const S: TBaseStation);
    procedure CreerLesFiches();
    procedure ListerStations();
    procedure CreerUnePage(const L, C: integer; const QEchelle: double);
    procedure Finaliser();

end;


//******************************************************************************
implementation
const
  PAGE_HEIGHT       = 750;
  PAGE_WIDTH        = PAGE_HEIGHT / sqrt(2);
  COTE_CARRE_QRCODE = PAGE_WIDTH - 50;
  //POS_Y_RESEAU      = PAGE_HEIGHT - 20;
  POS_Y_LBL_STATION = PAGE_HEIGHT - 25;
  POS_Y_QRCODE      = PAGE_HEIGHT - 85 - COTE_CARRE_QRCODE;
  POS_FER_GAUCHE    = 25;

  POS_Y_COORDONNEES = POS_Y_QRCODE + COTE_CARRE_QRCODE + 30;
  POS_Y_DATE        = POS_Y_COORDONNEES + 40;

{ TFichePointTopo }
// -----------------------------------------------------------------------------
const MILLIMETERS_IN_ONE_INCH = 25.40;

function Millimetres2PixelsXf(const PRN: TPrinter; const Millims: double): integer;
begin
  Result := trunc(100 * PRN.XDPI * Millims / MILLIMETERS_IN_ONE_INCH) div 100;
end;
function Millimetres2PixelsYf(const PRN: TPrinter; const Millims: double): integer;
begin
  Result := trunc(100 * PRN.YDPI * Millims / MILLIMETERS_IN_ONE_INCH) div 100;
end;
function Pixels2MillimetresX(const PRN: TPrinter; const Pixls: integer): Double;
begin
  Result := MILLIMETERS_IN_ONE_INCH * Pixls / PRN.XDPI;
end;
function Pixels2MillimetresY(const PRN: TPrinter; const Pixls: integer): Double;
begin
  Result := MILLIMETERS_IN_ONE_INCH * Pixls / PRN.YDPI;
end;

procedure QPrintTexte(const QX, QY: double; const QTexte: string);
begin
  Printer.Canvas.TextOut(Millimetres2PixelsXf(Printer, QX),
                              Millimetres2PixelsYf(Printer, QY),
                              QTexte);
end;
procedure QDrawCadreImage(const QX, QY, QL, QH: double; const QLineWidth: double);
var
  QR: TRect;
begin
  Printer.Canvas.Pen.Color := clBlack;
  Printer.Canvas.Pen.Width := Millimetres2PixelsYf(Printer, QLineWidth);
  QR.Left   := Millimetres2PixelsXf(Printer, QX);
  QR.Top    := Millimetres2PixelsYf(Printer, QY);
  QR.Right  := Millimetres2PixelsXf(Printer, QX + QL);
  QR.Bottom := Millimetres2PixelsXf(Printer, QY + QH);
  Printer.Canvas.Rectangle(QR);
end;
procedure QDrawLine(const QX1, QY1, QX2, QY2: double; const QLineWidth: double);
begin
  Printer.Canvas.Pen.Color := clBlack;
  Printer.Canvas.Pen.Width := Millimetres2PixelsYf(Printer, QLineWidth);
  Printer.Canvas.Line(Millimetres2PixelsXf(Printer, QX1),
                           Millimetres2PixelsYf(Printer, QY1),
                           Millimetres2PixelsXf(Printer, QX2),
                           Millimetres2PixelsYf(Printer, QY2));

end;
function TFichesPointTopo.QExporterTopoEnImage(const LimitesDessin: TRect2Df;
                                               out   QGHCaveDrawDrawingContext: TGHCaveDrawDrawingContext): boolean;
var
  dx, dy: Extended;
  QImgHeight: Int64;
  NbGroupes: Integer;
  NbScraps: Integer;
  NbCourbes: Integer;
  NbPolyLines: Integer;
  NbPolygones: Integer;
  NbSimpleLignes: Integer;
  NbSymboles: Integer;
  NbTextes: Integer;
  NumGroupe: Integer;
  MyGroupe: TGroupeEntites;
  ii: Integer;
  QQC1: TPoint3Df;
  QQC2: TPoint3Df;
  P1, P2, PosCopyright: TPoint2Df;
begin
  dx := LimitesDessin.X2 - LimitesDessin.X1;
  dy := LimitesDessin.Y2 - LimitesDessin.Y1;
  //QImgHeight := round(ImgWidth * (dy/dx));
  AfficherMessage(Format('%s.QExporterTopoEnImage(%.2f, %.2f)->(%.2f, %.2f) - w=%d, h=%d',
                        [self.ClassName,
                         LimitesDessin.X1, LimitesDessin.Y1,
                         LimitesDessin.X2, LimitesDessin.Y2,
                         FImgWidth, FImgHeight]));
  try
    // paramétrage
    QGHCaveDrawDrawingContext.SetDocuTopo(FMyDocDessin);
    QGHCaveDrawDrawingContext.SetParamDessin(FParamsVue2D);
    QGHCaveDrawDrawingContext.SetTextures(FTextures);
    QGHCaveDrawDrawingContext.SetBounds(LimitesDessin.X1, LimitesDessin.Y1, LimitesDessin.X2, LimitesDessin.Y2);
    // début conventionnel
    QGHCaveDrawDrawingContext.BeginDrawing();
    //AfficherMessage(ClassName, 'Redessiner: ' + IIF(FDoDraw, '', '** Pas de document chargé **'));

    //************************
    // le dessin de la cavité
    NbGroupes      := FMyDocDessin.GetNbGroupes();
    NbScraps       := FMyDocDessin.GetNbScraps();
    NbCourbes      := FMyDocDessin.GetNbCourbes();
    NbPolyLines    := FMyDocDessin.GetNbPolylignes();
    NbPolygones    := FMyDocDessin.GetNbPolygones();
    NbSimpleLignes := FMyDocDessin.GetNbSimpleLignes();
    NbSymboles     := FMyDocDessin.GetNbSymboles();
    NbTextes       := FMyDocDessin.GetNbTextes();

    for NumGroupe := 0 to NbGroupes - 1 do
    begin
      MyGroupe := FMyDocDessin.GetGroupe(NumGroupe);
      //AfficherMessage(Format('Groupe %d: %d - %s', [NumGroupe, MyGroupe.IDGroupeEntites, MyGroupe.NomGroupe]));
      // tracer le BBX du groupe
      // on teste si le groupe est partiellement visible dans la fenêtre
      // avant de le tracer (forte accélération du dessin)
      //if (GroupeIsInView(MyGroupe) and (MyGroupe.Displayed)) then
      begin
        if (NbScraps > 0) then
        begin
          for ii:=0 to NbScraps - 1 do if (MyGroupe.Visible) then QGHCaveDrawDrawingContext.DessinerScrap(FMyDocDessin.GetScrap(ii), ii, False, MyGroupe.IDGroupeEntites);
        end;
        if (NbPolygones > 0) then
        begin
          for ii:=0 to NbPolygones - 1 do if (MyGroupe.Visible) then QGHCaveDrawDrawingContext.DessinerPolygone(FMyDocDessin.GetPolygone(ii), ii, False, MyGroupe.IDGroupeEntites);
        end;
        if (NbCourbes > 0) then
        begin
          for ii:=0 to NbCourbes - 1 do if (MyGroupe.Visible) then QGHCaveDrawDrawingContext.DessinerCourbe(FMyDocDessin.GetCourbe(ii), ii, MyGroupe.IDGroupeEntites);
        end;
        if (NbPolyLines > 0) then
        begin
          for ii:=0 to NbPolyLines - 1 do if (MyGroupe.Visible) then QGHCaveDrawDrawingContext.DessinerPolyLigne(FMyDocDessin.GetPolyligne(ii), ii, MyGroupe.IDGroupeEntites);
        end;
        if (NbSimpleLignes > 0) then
        begin
          for ii := 0 to NbSimpleLignes - 1 do if (MyGroupe.Visible) then QGHCaveDrawDrawingContext.DessinerSimpleLigne(FMyDocDessin.GetSimpleLigne(ii), false, MyGroupe.IDGroupeEntites);
        end;
        if (NbSymboles > 0) then   // Symboles OK
        begin
          for ii := 0 to NbSymboles - 1 do if (MyGroupe.Visible) then QGHCaveDrawDrawingContext.DessinerSymbole(FMyDocDessin.GetSymbole(ii), MyGroupe.IDGroupeEntites);
        end;
        if ((tedTEXTES in FParamsVue2D.ElementsDrawn) and (NbTextes > 0)) then
        begin
          for ii := 0 to NbTextes - 1 do if (MyGroupe.Visible) then QGHCaveDrawDrawingContext.DessinerTexte(FMyDocDessin.GetTexte(ii), False, MyGroupe.IDGroupeEntites);
        end;
      end;
    end; // for NoGroupe ...
    //************************
    //AfficherMessage('-- Quadrillage');
    if (tedQUADRILLES in FParamsVue2D.ElementsDrawn) then
    begin
      QGHCaveDrawDrawingContext.DrawQuadrillage();
    end;
    //AfficherMessage('-- centerlines');
    QGHCaveDrawDrawingContext.DefineBrosse(bsClear, FParamsVue2D.BackGroundColor, 128);
    if (tedCENTERLINES in FParamsVue2D.ElementsDrawn) then
    begin
      for ii:= 0 to FMyDocDessin.GetNbBaseStations - 1 do
        QGHCaveDrawDrawingContext.DessinerBaseStation(FMyDocDessin.GetBaseStation(ii), Millimetres2PixelsYf(Printer, HAUTEUR_LBL_BASESTATION_IN_MM));
    end;
    QGHCaveDrawDrawingContext.RestoreBrosse();
    // et enfin l'échelle
    if (tedECHELLE_NORD in FParamsVue2D.ElementsDrawn) then QGHCaveDrawDrawingContext.DrawEchelleNord(FImgWidth, FImgHeight);
    // cadre englobant (limites du dessin)
    QGHCaveDrawDrawingContext.DefineCrayon(psSolid,
                                           Millimetres2PixelsXf(Printer, 0.7),
                                           clBlue,
                                           255);
    QQC1 := FMyDocDessin.GetCoordsMini();
    QQC2 := FMyDocDessin.GetCoordsMaxi();
    P1.setFrom(QQC1.X, QQC1.Y);
    P2.setFrom(QQC2.X, QQC2.Y);
    QGHCaveDrawDrawingContext.DrawRectangle(P1, P2);
    QGHCaveDrawDrawingContext.RestoreCrayon();
    // fin conventionnelle
    QGHCaveDrawDrawingContext.EndDrawing();
  except
  end;
end;

procedure TFichesPointTopo.ListerStations();
var
  n, i: Integer;
  BP: TBaseStation;
begin
  n := getNbBaseSts();
  if (n = 0) then exit;
  for i := 0 to n - 1 do
  begin
    BP := getBaseSt(i);
    {$IFDEF TIDBASEPOINT_AS_TEXT}
    AfficherMessage(BP.IDStation);
    {$ELSE}
    AfficherMessage(BP.GetToporobotIDStationAsString(true));
    {$ENDIF TIDBASEPOINT_AS_TEXT}
  end;
end;

procedure TFichesPointTopo.AddStation(const S: TBaseStation);
begin
  FListeStations.AddElement(S);
end;

function TFichesPointTopo.SelectionnerImprimante(var MyPrinter: TPrinter): boolean;
var
  PC: TPrinterSetupDialog;
begin
  result := false;
  PC := TPrinterSetupDialog.Create(nil);
  try
    if (PC.Execute) then
    begin
      exit(True);
    end;
  finally
    PC.Free;
  end;
end;

procedure TFichesPointTopo.CreerLesFiches;
var
  n, i: Integer;
  BP: TBaseStation;
  Pr: String;
  VFile: TFileStream;
begin
  n := getNbBaseSts();
  AfficherMessage(format('%s.CreerLesFiches: %d fiches à créer', [ClassName, n]));
  if (n = 0) then exit;


  //Printer := Printer;
  // lister les imprimantes et attrapper l'imprimante PDF
  n := Printer.Printers.Count;
  Printer.PrinterIndex := 0;
  for i := 0 to n - 1 do
  begin
    Pr := Printer.Printers[i];
    AfficherMessage(Pr);
    if (Pos(UpperCase('PDFCreator'), UpperCase(Pr)) > 0) then
    begin
      AfficherMessage('*** Imprimante PDF trouvée');
      Printer.PrinterIndex := i;
      Break;
    end;
  end;
  Printer.Orientation := poPortrait;
  if (Printer.CanPrint) then AfficherMessage('** Prêt à imprimer');
  // affiche systématiquement le dialogue de config
  if (not SelectionnerImprimante(Printer)) then Exit();

  AfficherMessage(Format('Imprimante utilisée %d - %s', [Printer.PrinterIndex, Printer.PrinterName]));



  //Exit; //************ provisoire

  Printer.BeginDoc;

  n := getNbBaseSts();
  for i := 0 to n - 1 do
  begin
    AfficherMessage(Format('Impression page %d / %d', [i+1 ,  n]));
    BP := getBaseSt(i);
    CreerUneFiche(BP);
    if (i < n - 1) then Printer.NewPage;
  end;

  // sauvegarde finale
  //Printer.;
  AfficherMessage('*** Impression terminée');
  Printer.EndDoc;
end;

procedure TFichesPointTopo.CreerUnePage(const L, C: integer; const QEchelle: double);
const
  MARGE_PERIMETRIQUE = 100;
var
  TmpBuffer: TGHCaveDrawDrawingContext;
  QOldStyleCourbe, QSC: TStyleCourbe;
  BBX: TRect2Df;
  QFX, QFY: Double;
  CoinBasGauche: TPoint3Df;
begin
  if (FRedessinInProcess) then
  begin
    AfficherMessageErreur(Format('[FRedessinInProcess = %s] Redessin en cours', [BoolToStr(FRedessinInProcess)]));
    exit;
  end;
  FRedessinInProcess := True;   // Armement du sémaphore de dessin

  // préparation et génération de l'extrait de plan
  FImgWidth  := Printer.PageWidth - 2 * MARGE_PERIMETRIQUE;
  FImgHeight := Printer.PageHeight - 2 * MARGE_PERIMETRIQUE;
  AfficherMessage(format('%d x %d', [FImgWidth, FImgHeight]));

  TmpBuffer  := TGHCaveDrawDrawingContext.Create(FImgWidth, FImgHeight,
                                                 BGRA(Red(FParamsVue2D.BackGroundColor), Green(FParamsVue2D.BackGroundColor), Blue(FParamsVue2D.BackGroundColor), 255));
  try
    CoinBasGauche := FMyDocDessin.GetCoordsMini();
    QFX := Pixels2MillimetresX(Printer, FImgWidth);
    QFY := Pixels2MillimetresY(Printer, FImgHeight);
    BBX.setFrom(CoinBasGauche.X + QFX * C,
                CoinBasGauche.Y + QFY * L,
                CoinBasGauche.X + QFX * (C + 1),
                CoinBasGauche.Y + QFY * (L + 1));

    QExporterTopoEnImage(BBX, TmpBuffer);
    //QDrawCadreImage(QPosImgX, QPosImgY, LargeurExtraitPlan, HauteurExtraitPlan, 1.8);
    TmpBuffer.Draw(Printer.Canvas,
                   MARGE_PERIMETRIQUE, MARGE_PERIMETRIQUE,
                   True);
    AfficherMessage('-- OK');
  finally
    // et restauration des styles
    //FMyDocDessin.PutStyleCourbe(IDX_STYLE_COURBE_PAROIS, QOldStyleCourbe);
    TmpBuffer.Free;
    FRedessinInProcess := false;
  end;
end;

procedure TFichesPointTopo.CreerUneFiche(const BP: TBaseStation);
const
  MARGE_PERIMETRIQUE = 3.0;
var
  WU: String;
  TmpBuffer: TGHCaveDrawDrawingContext;
  BBX: TRect2Df;
  HalfWidth : double;
  LargeurFiche, HauteurFiche: double;
  LargeurExtraitPlan, HauteurExtraitPlan, QLargeurEchelle: double;
  QOldStyleCourbe, QSC: TStyleCourbe;
  QPosImgX, QPosImgY, QReticulePosX, QReticulePosY,
  QPosEchelleX,  QPosEchelleY: Extended;
begin
  if (FRedessinInProcess) then
  begin
    AfficherMessageErreur(Format('[FRedessinInProcess = %s] Redessin en cours', [BoolToStr(FRedessinInProcess)]));
    exit;
  end;
  FRedessinInProcess := True;   // Armement du sémaphore de dessin

  // définition de la taille de la fiche
  LargeurFiche := Pixels2MillimetresX(Printer, Printer.PageWidth);
  HauteurFiche := Pixels2MillimetresY(Printer, Printer.PageHeight);

  LargeurExtraitPlan := LargeurFiche - 10.00;
  QPosImgX := (LargeurFiche - LargeurExtraitPlan) * 0.5;
  QPosImgY := MARGE_PERIMETRIQUE + 40.00;
  HauteurExtraitPlan := LargeurExtraitPlan;




  HalfWidth := 50.00;
  // modification de styles d'objets
  QOldStyleCourbe := FMyDocDessin.GetStyleCourbe(IDX_STYLE_COURBE_PAROIS);
  QSC := QOldStyleCourbe;
  QSC.LineWidth := Millimetres2PixelsXf(Printer, 0.50);
  QSC.LineColor := clBlue;
  FMyDocDessin.PutStyleCourbe(IDX_STYLE_COURBE_PAROIS, QSC);
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  WU := BP.IDStation;
  {$ELSE}
  WU := BP.GetToporobotIDStationAsString(true);
  {$ENDIF TIDBASEPOINT_AS_TEXT}
  AfficherMessage(format(' -- CreerUneFiche: %s', [WU]));
  // le cadre périmétrique de la fiche
  QDrawCadreImage(MARGE_PERIMETRIQUE, MARGE_PERIMETRIQUE,
                  LargeurFiche - 2 * MARGE_PERIMETRIQUE,
                  HauteurFiche - 2 * MARGE_PERIMETRIQUE,
                  0.35);
  // préparation et génération de l'extrait de plan
  FImgWidth  := Millimetres2PixelsXf(Printer, LargeurExtraitPlan);
  FImgHeight := Millimetres2PixelsYf(Printer, HauteurExtraitPlan);
  AfficherMessage(format('%d x %d', [FImgWidth, FImgHeight]));
  TmpBuffer  := TGHCaveDrawDrawingContext.Create(FImgWidth, FImgHeight,
                                                 BGRA(Red(FParamsVue2D.BackGroundColor), Green(FParamsVue2D.BackGroundColor), Blue(FParamsVue2D.BackGroundColor), 255));
  try
    BBX.setFrom(BP.PosStation.X - HalfWidth, BP.PosStation.Y - HalfWidth,
                BP.PosStation.X + HalfWidth, BP.PosStation.X + HalfWidth);
    QExporterTopoEnImage(BBX, TmpBuffer);
    QDrawCadreImage(QPosImgX, QPosImgY, LargeurExtraitPlan, HauteurExtraitPlan, 1.8);
    TmpBuffer.Draw(Printer.Canvas,
                   Millimetres2PixelsXf(Printer, QPosImgX),
                   Millimetres2PixelsXf(Printer, QPosImgY),
                   True);
    AfficherMessage('-- OK');
  finally
    // et restauration des styles
    FMyDocDessin.PutStyleCourbe(IDX_STYLE_COURBE_PAROIS, QOldStyleCourbe);
    TmpBuffer.Free;
    FRedessinInProcess := false;
  end;
  // on met en place un réticule pour souligner la station
  QReticulePosX := QPosImgX + 0.50 * LargeurExtraitPlan;
  QReticulePosY := QPosImgY + 0.50 * HauteurExtraitPlan;
  QDrawLine(QPosImgX, QReticulePosY, QPosImgX + LargeurExtraitPlan, QReticulePosY, 0.25);
  QDrawLine(QReticulePosX, QPosImgY, QReticulePosX, QPosImgY + HauteurExtraitPlan, 0.25);
  // titres et autres
  Printer.Canvas.Font.Height := Millimetres2PixelsYf(Printer, 8.00);
  Printer.Canvas.Font.Style  := [fsBold];
  QPrintTexte(10, MARGE_PERIMETRIQUE + 5.0, WU);
  Printer.Canvas.Font.Height := Millimetres2PixelsYf(Printer, 5.00);
  Printer.Canvas.Font.Style  := [];
  WU := format('X = %.0f, Y = %.0f, Z = %.0f', [BP.PosStation.X, BP.PosStation.Y, BP.PosStation.Z]);
  QPosEchelleX := QPosImgX;
  QPosEchelleY := MARGE_PERIMETRIQUE + 25.0;
  QPrintTexte(QPosEchelleX, MARGE_PERIMETRIQUE + 15.0, WU);
  // échelle
  QLargeurEchelle := LargeurExtraitPlan / 2;
  WU := format('%.0f', [0.00]);
  QPrintTexte(QPosEchelleX, QPosEchelleY, WU);
  WU := format('%.0f m', [HalfWidth]);
  QPrintTexte(QPosEchelleX + QLargeurEchelle, QPosEchelleY, WU);
  QPosEchelleY += 5.0;
  QDrawLine(QPosEchelleX, QPosEchelleY,
            QPosEchelleX + QLargeurEchelle, QPosEchelleY,
            0.35);
end;


function TFichesPointTopo.getBaseSt(const Idx: integer): TBaseStation;
begin
  result := FListeStations.GetElement(Idx);
end;

function TFichesPointTopo.getNbBaseSts: integer;
begin
  result := FListeStations.GetNbElements();
end;

function TFichesPointTopo.Initialiser(const FD: TDocumentDessin;
                                      const QTextures: TTexturesPolygonObject;
                                      const FP: TParamsVue2D): boolean;
begin
  result := false;
  AfficherMessage(format('%s.Initialiser()', [ClassName]));
  FMyDocDessin   := FD;
  FRedessinInProcess := false;
  FImgWidth          := 600;
  FImgHeight         := FImgWidth;  // image carrée
  FParamsVue2D       := FP;

  //FFichierPDF    := QFileName;
  FListeStations := TListeSimple<TBaseStation>.Create;
  try

    FListeStations.ClearListe();
    result := true;

  except
  end;
end;
procedure TFichesPointTopo.Finaliser;
begin
  AfficherMessage(format('%s.Finaliser()', [ClassName]));
  try
    FListeStations.ClearListe();
  finally
    FListeStations.Free;
    //FMyDocPDF.Free;
  end;
end;

end.
