unit CentreImpressionExt;
// Centre d'impression nouvelle version
// Date: 08/11/2013
// Statut: Opérationnel. Des détails à compléter et à revoir
// Interfaçage par pointeur sur TBDDEntites: OK
// Prévisualisation: OK
// Impression: OK - des détails à revoir.
// 04/08/2014: Nouveau dessin des entrées
// 09/08/2017: Réactivation du module à la demande de S. Clément
// 09/08/2017: Suppression du type TPageProperties
//             et du membre FPrinter: Printer est déjà une variable déclarée dans Printers.pas


{$INCLUDE CompilationParameters.inc}
interface
uses
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GHCD_Types,
  GeneralFunctions,
  UnitDocDessin,
  unitGenererFichePointTopo,
  CallDialogsGHCaveDraw,

  //unitUtilsComposants,
  //CadreFiltres,

  math,
  types,
  Printers,
  OSPrinters, // OSPrinters est INDISPENSABLE ici !!
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons;
// types de données locaux
type

  { TfrmPrintingCenterExt }

  TfrmPrintingCenterExt = class(TForm)
    BitBtn1: TBitBtn;
    btnApply: TButton;
    btnApplyParams2D: TButton;
    btnHelpFiltres: TButton;
    btnSelectPrinter: TButton;
    btnStartImpression: TButton;
    btnParametrerVue2D: TButton;
    chkRegle: TCheckBox;
    editEchelle: TCurrencyEdit;
    editFiltres: TEdit;
    editTailleEchelle: TCurrencyEdit;
    grbPrinterNames: TGroupBox;
    grbQuadrillage: TGroupBox;
    GroupBox3: TGroupBox;
    Label3: TLabel;
    lbTitreQdrType: TLabel;
    lbTitreQdrSpacing: TLabel;
    lbTitreQdrColor: TLabel;
    lbSecQuadrillesColor: TStaticText;
    lbMainQuadrillesSpacing: TStaticText;
    lbMainQuadrillesColor: TStaticText;
    lbEchelle: TLabel;
    lbSecQuadrillesSpacing: TStaticText;
    lbSecQuadrillesType: TStaticText;
    lbMouseCoordinates: TStaticText;
    lbNbPages: TLabel;
    lbOrientation: TLabel;
    lbPageFormat: TLabel;
    lbPrintCurrent: TLabel;
    lbPrinterName: TLabel;
    PaintBoxVue: TPaintBox;
    Panel1: TPanel;
    pnlPrintSettings: TPanel;
    pnlCadre: TPanel;
    pnlProgressPrinting: TPanel;
    progbarPrinting: TProgressBar;
    lbQdrMain: TStaticText;
    lbQdrSec: TStaticText;
    lbMainQuadrillesType: TStaticText;
    procedure btnPreviewClick(Sender: TObject);
    procedure btnStartImpressionClick(Sender: TObject);
    procedure btnSelectPrinterClick(Sender: TObject);
    procedure btnApplyParams2DClick(Sender: TObject);
    procedure btnParametrerVue2DClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxVueDblClick(Sender: TObject);
    procedure PaintBoxVueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxVuePaint(Sender: TObject);
  private
    { private declarations }
    // Doc topo
    FDocTopo              : TDocumentDessin;
    FTextures             : TTexturesPolygonObject;
    FParamsVue2D          : TParamsVue2D;
    FEchelle              : double;

    // nombre de pages sur X et Y
    // tableau des pages dessinées
    FNbPagesX         : integer;
    FNbPagesY         : integer;
    FTableauPagesDrawn: array of array of boolean;
    FDoDrawPages      : boolean;

    // variables internes
    FRappScrReal : double;
    FInvRappScrReal: double;
    // coordonnées internes de la souris dans la vue
    FPP: TPoint;
    // limites du dessin
    FRXMini      : double;
    FRXMaxi      : double;
    FRYMini      : double;
    FRYMaxi      : double;
    // paramétrage imprimante
    procedure CalcIndexPageIncludingPt(const Pt: TPoint2Df; var IdxX, IdxY: integer);
    procedure ImprimerLaTopo();
    procedure PrintAPage(const QEchelle: double; const L, C: integer);
    procedure SetControlsWithParamsVue2D(const FPP: TParamsVue2D);
    procedure SetMyCurrentPrinter();
    // conversion de coordonnées
    function GetCoordsMonde(const PP: TPoint): TPoint2Df;
    function GetCoordsPlan(const PM: TPoint2Df): TPoint;

    // recadrage du dessin
    procedure Recadrer();
    procedure CalcNbPagesAndEmpty();
    procedure DrawApercu();
  public
    { public declarations }
    function Initialiser(const QDocDessin: TDocumentDessin;
                         const TP: TTexturesPolygonObject;
                         const FP: TParamsVue2D;
                         const QFiltres: string): boolean;
    // libération du contexte
    procedure Finaliser();
  end;

var
  frmPrintingCenterExt: TfrmPrintingCenterExt;

implementation

{$R *.lfm}
const ONE_INCH_IN_MM: double = 25.40;
// fonctions locales hors objet
function Millimetres2PixelsXf(const Millims: double): integer;
begin
  Result:=trunc(100 * Printer.XDPI * Millims / ONE_INCH_IN_MM) div 100;
end;
function Millimetres2PixelsYf(const Millims: double): integer;
begin
  Result:=trunc(100 * Printer.YDPI * Millims / ONE_INCH_IN_MM) div 100;
end;
function Pixels2MillimetresX(const Pixls: integer): Double;
begin
  Result := ONE_INCH_IN_MM * Pixls / Printer.XDPI;
end;
function Pixels2MillimetresY(const Pixls: integer): Double;
begin
  Result := ONE_INCH_IN_MM * Pixls / Printer.YDPI;
end;


{ TfrmPrintingCenterExt }
// définir imprimante courante: paramètres, etc ...
procedure TfrmPrintingCenterExt.SetMyCurrentPrinter();
begin
  lbPrinterName.Caption := Printer.PrinterName;
  lbOrientation.Caption := ChooseString(Ord(Printer.Orientation),
                                          ['PORTRAIT', 'LANDSCAPE', 'REVERSE LANDSCAPE', 'REVERSEPORTRAIT']);
  lbPageFormat.Caption  := Format('%f mm x %f mm', [Pixels2MillimetresX(Printer.PageWidth),
                                                    Pixels2MillimetresY(Printer.PageHeight)]);
  Recadrer();            // reconstruire le plan
  CalcNbPagesAndEmpty(); // calculer le nombre de pages
  DrawApercu();          // redessiner
end;




// conversion de coordonnées
function TfrmPrintingCenterExt.GetCoordsPlan (const PM: TPoint2Df): TPoint;
begin
  // rotation et mise à l'échelle
  (* Formule: (F = FRappScrReal)
  | Xp |   | +F    0    -F * FRXMini |   | PM.X |
  |    |   |                         |   |      |
  | Yp | = |  0   -F    +F * FRYMaxi | * | PM.Y |
  |    |   |                         |   |      |
  |  1 |   |  0    0          1      |   |   1  |
  //*)
  Result.X:=Round((PM.X - FRXMini) * FRappScrReal);
  Result.Y:=Round((FRYMaxi-PM.Y) * FRappScrReal);
end;
function TfrmPrintingCenterExt.GetCoordsMonde(const PP: TPoint): TPoint2Df;
begin
  (* Formule: (F = FRappScrReal)
  | Xm |   | +1/F    0     FRXMini   |   | PP.X |
  |    |   |                         |   |      |
  | Ym | = |  0   -1/F     FRYMaxi   | * | PP.Y |
  |    |   |                         |   |      |
  |  1 |   |  0    0          1      |   |   1  |
  //*)
  Result.X:=  FInvRappScrReal * PP.X + FRXMini;
  Result.Y:= -FInvRappScrReal * PP.Y + FRYMaxi;
end;

// cadrage du dessin
// pas besoin de fonction de zoom
// (sinon, on aurait utilisé un TCdrVisualisateur2D)
procedure TfrmPrintingCenterExt.Recadrer();
var
  Marge: double;
  qdx, qdy: Double;
  C1, C2: TPoint3Df;

begin
  AfficherMessage(Format('%s.Recadrer',[ClassName]));

  C1 := FDocTopo.GetCoordsMini();
  C2 := FDocTopo.GetCoordsMaxi();
  Marge:=0.02 * Hypot(C2.X - C1.X,
                      C2.Y - C1.Y);

  FRXMini:= C1.X - Marge;
  FRXMaxi:= C2.X + Marge;
  FRYMini:= C1.Y - Marge;
  FRYMaxi:= C2.Y + Marge;


  qdx := FRXMaxi - FRXMini;
  qdy := FRYMaxi - FRYMini;
  //pnlCadre.top:=0;
  //pnlCadre.left:=0;
  FRappScrReal := IIF((qdx > qdy), pnlCadre.Width / qdx, pnlCadre.Height / qdy);
  FInvRappScrReal:=1/FRappScrReal;
  AfficherMessage(Format('%s.Recadrer OK',[ClassName]));
end;

// récupération des paramètres depuis les contrôles
(*
function TfrmPrintingCenterExt.GetVue2DParamsFromDialog(): TVue2DParams;
begin
  Result.ongElementsDrawn := [];
  if (chkEntrances.Checked)   then Result.ongElementsDrawn := Result.ongElementsDrawn + [edENTRANCES]
                              else Result.ongElementsDrawn := Result.ongElementsDrawn - [edENTRANCES];
  if (chkStations.Checked)    then Result.ongElementsDrawn := Result.ongElementsDrawn + [edStations]
                              else Result.ongElementsDrawn := Result.ongElementsDrawn - [edStations];
  if (chkAltitudes.Checked)   then Result.ongElementsDrawn := Result.ongElementsDrawn + [edAltitudes]
                              else Result.ongElementsDrawn := Result.ongElementsDrawn - [edAltitudes];
  if (chkCotes.Checked)       then Result.ongElementsDrawn := Result.ongElementsDrawn + [edCotes]
                              else Result.ongElementsDrawn := Result.ongElementsDrawn - [edCotes];
  if (chkParois.Checked)      then Result.ongElementsDrawn := Result.ongElementsDrawn + [edWalls]
                              else Result.ongElementsDrawn := Result.ongElementsDrawn - [edWalls];
  if (chkIDStations.Checked)  then Result.ongElementsDrawn := Result.ongElementsDrawn + [edIDStations]
                              else Result.ongElementsDrawn := Result.ongElementsDrawn - [edIDStations];
  if (chkSections.Checked)    then Result.ongElementsDrawn := Result.ongElementsDrawn + [edCrossSections]
                              else Result.ongElementsDrawn := Result.ongElementsDrawn - [edCrossSections];
  if (chkRemplissage.Checked) then Result.ongElementsDrawn := Result.ongElementsDrawn + [edFillGalerie]
                              else Result.ongElementsDrawn := Result.ongElementsDrawn - [edFillGalerie];
  if (chkPolygonales.Checked) then Result.ongElementsDrawn := Result.ongElementsDrawn + [edPolygonals]
                              else Result.ongElementsDrawn := Result.ongElementsDrawn - [edPolygonals];
  if (chkAntennes.Checked)    then Result.ongElementsDrawn := Result.ongElementsDrawn + [edANTENNES]
                              else Result.ongElementsDrawn := Result.ongElementsDrawn - [edANTENNES];
  if (chkAnnotations.Checked) then Result.ongElementsDrawn := Result.ongElementsDrawn + [edANNOTATIONS]
                              else Result.ongElementsDrawn := Result.ongElementsDrawn - [edANNOTATIONS];
  if (chkNoeuds.Checked)      then Result.ongElementsDrawn := Result.ongElementsDrawn + [edJONCTIONS]
                              else Result.ongElementsDrawn := Result.ongElementsDrawn - [edJONCTIONS];
  // récupérer les valeurs de quadrillage depuis les box
  Result.ongQdrSpc        := editQdrSpacing.Value;
  Result.ongQdrColor      := btnColorMainQuadrilles.ButtonColor;
  Result.ongQdrType       := TQdrType(cmbTypeQuadrillage.ItemIndex);


  FQdrCrossSize  := Result.ongQdrSpc / 5.00 ; // provisoire

  FRegleSize     := editTailleEchelle.Value;
  Result.ongDegradeStart := btnDegradeStart.ButtonColor;
  Result.ongDegradeStop  := btnDegradeStop.ButtonColor;
  // largeur de trait
  Result.ongViseesLargeur := Millimetres2PixelsYf(editLargeurTraitPolygonale.Value);
  // mode de representation
  Result.ongModeRepresentation := TModeRepresentationGaleries(cmbRepresentation.ItemIndex);
end;
//*)

// options de dessin
(*
procedure TfrmPrintingCenterExt.SetVue2DParamsToDialogs(const FE: TVue2DParams);
begin
  FVue2DParams := FE;
  chkPolygonales.Checked := (edPolygonals     in FE.ongElementsDrawn);
  chkStations.Checked    := (edStations       in FE.ongElementsDrawn);
  chkAltitudes.Checked   := (edAltitudes      in FE.ongElementsDrawn);
  chkCotes.Checked       := (edCotes          in FE.ongElementsDrawn);
  chkIDStations.Checked  := (edIDStations     in FE.ongElementsDrawn);
  chkParois.Checked      := (edWalls          in FE.ongElementsDrawn);
  chkRemplissage.Checked := (edFillGalerie    in FE.ongElementsDrawn);
  chkSections.Checked    := (edCrossSections  in FE.ongElementsDrawn);
  chkEntrances.Checked   := (edENTRANCES      in FE.ongElementsDrawn);
  chkAntennes.Checked    := (edANTENNES       in FE.ongElementsDrawn);
  chkAnnotations.Checked := (edANNOTATIONS    in FE.ongElementsDrawn);
  chkNoeuds.Checked      := (edJONCTIONS      in FE.ongElementsDrawn);

  btnColorMainQuadrilles.ButtonColor := FE.ongQdrColor;
  cmbTypeQuadrillage.ItemIndex   := Ord(FE.ongQdrType);
  editQdrSpacing.Value           := FE.ongQdrSpc;

  cmbRepresentation.ItemIndex := ord(FE.ongModeRepresentation);

end;
//*)


procedure TfrmPrintingCenterExt.SetControlsWithParamsVue2D(const FPP: TParamsVue2D);
begin
  lbMainQuadrillesType.Caption    := FPP.MainGrid.DescTypeQuadrillage();
  lbMainQuadrillesSpacing.Caption := Format('%.0f m', [FPP.MainGrid.Spacing]);
  lbMainQuadrillesColor.Color     := FPP.MainGrid.Color;

  lbSecQuadrillesType.Caption     := FPP.SecGrid.DescTypeQuadrillage();
  lbSecQuadrillesSpacing.Caption  := Format('%.0f m', [FPP.SecGrid.Spacing]);
  lbSecQuadrillesColor.Color      := FPP.SecGrid.Color;
  editTailleEchelle.Value         := FParamsVue2D.TailleEchelle;
end;

function TfrmPrintingCenterExt.Initialiser(const QDocDessin: TDocumentDessin;
                                           const TP: TTexturesPolygonObject;
                                           const FP: TParamsVue2D;
                                           const QFiltres: string): boolean;
var
  i  : Integer;
  WU : TElementsDrawn;
begin
  AfficherMessage(Format('%s.InitialiseByPointer', [ClassName]));
  Result := False;
  FTextures := TP;
  // init de quelques variables
  (*
  SetVue2DParamsToDialogs(QVue2DParams);
  FBddDemarreeDepuisFichier   := false;
  //*)
  FParamsVue2D := FP;
  SetControlsWithParamsVue2D(FParamsVue2D);

  // affecter pointeur sur la BDD
  FDocTopo := QDocDessin;
  try
    QDocDessin.MetaFiltre(QFiltres);
    // Initialisation de l'imprimante: Nécessite le paquet Printers4Lazarus,
    // qui est à ajouter à la liste des paquets requis par GHTopo
    // Ouvrir l'inspecteur de projet
    // Cliquer sur [ + ] en tete de la fenetre
    // Une fenetre s'ouvre
    // Onglet __/ Nouvelle Condition \__
    // Dérouler la première combobox
    // Sélectionner 'Printer4Lazarus'
    // Les unités Printers et OSPrinters sont INDISPENSABLES !!
    // Printer est une variable globale initialisée par le paquet.
    // Ne pas utiliser  TPrinter.Create;
    AfficherMessage('Fixation variables locales');
    // fixation de variables locales
    editEchelle.Value := 1000.00;
    FEchelle       := 1 / editEchelle.Value;


    AfficherMessage('Setting printer');
    // définition de l'imprimante
    SetMyCurrentPrinter();
    // redessin
    AfficherMessage('Redessin preview');
    Recadrer();
    DrawApercu();
    //------------------------------
    AfficherMessage('init OK');
    Result := True;
  except
  end;
end;



procedure TfrmPrintingCenterExt.Finaliser();
begin
  pass;
end;
// calcul des pages vides
// calculer le nombre de pages et celles qui sont vides
procedure TfrmPrintingCenterExt.CalcNbPagesAndEmpty();
var
  i,j, v: integer;
  E1    : TBaseStation;
  L1, H1: double;
  QPageWidthInMM, QPageHeightInMM, QInvEchelle: Double;

  R666  : TRect2Df;
  RVS   : TRect2Df;
  Grayed: boolean;
begin
  QPageWidthInMM  := Pixels2MillimetresX(Printer.PageWidth);
  QPageHeightInMM := Pixels2MillimetresY(Printer.PageHeight);

  AfficherMessage('Calcul du nombre de pages');
  // calculer le nombre de pages
  FNbPagesX := 1 + Trunc(1000 * (FRXMaxi - FRXMini) * FEchelle / QPageWidthInMM);
  FNbPagesY := 1 + Trunc(1000 * (FRYMaxi - FRYMini) * FEchelle / QPageHeightInMM);
  lbNbPages.Caption :=Format('%d x %d pages',[FNbPagesX, FNbPagesY]);
  SetLength(FTableauPagesDrawn, 0, 0);
  SetLength(FTableauPagesDrawn, FNbPagesX, FNbPagesY);
  QInvEchelle := 1 / (1000 * FEchelle);
  L1 := QPageWidthInMM  * QInvEchelle ;
  H1 := QPageHeightInMM * QInvEchelle;
  for i:=0 to FNbPagesX-1 do
    for j:=0 to FNbPagesY-1 do
    begin
      R666.X1 := FRXMini + i * L1;
      R666.Y1 := FRYMini + j * H1;

      R666.X2 := FRXMini + (1+i) * L1;
      R666.Y2 := FRYMini + (1+j) * H1;
      // supprimer les pages vides
      Grayed:=false;
      for v := 1 to FDocTopo.GetNbBaseStations() - 1 do
      begin
        E1 := FDocTopo.GetBaseStation(v);
        //E1.TypeStation := tge;
        //if (E1.Type_Entite = tgEntrance) then Continue;
        RVS.X1 := Min(E1.PosExtr0.X, E1.PosStation.X);
        RVS.Y1 := Min(E1.PosExtr0.Y, E1.PosStation.Y);

        RVS.X2 := Max(E1.PosExtr0.X, E1.PosStation.X);
        RVS.Y2 := Max(E1.PosExtr0.Y, E1.PosStation.Y);
        Grayed := Grayed or IntersectRectangles(R666, RVS);
      end;
      FTableauPagesDrawn[i,j]:=Grayed;
      AfficherMessage(Format('--> Page %d.%d: %s',[i, j, IIF((FTableauPagesDrawn[i,j]), 'Affichée', 'Masquée')]));
    end;
    FDoDrawPages:=True;
end;



//******************************************************************************
// dessin de l'aperçu
procedure TfrmPrintingCenterExt.DrawApercu();
var
  TmpBuffer      : TBitmap;
  R              : TRect;
  procedure QResetPenBrush(const C: TColor);
  begin
    with TmpBuffer.Canvas do begin
      Pen.Color   := C;
      Brush.Color := C;
    end;
  end;

  // affichage des pages (avec mention de pages vides)
  procedure DrawPages();
  var
    i,j   : integer;
    P1, P2: TPoint;
    PM    : TPoint2Df;
    L1, H1, QInvEchelle, QPageWidthInMM, QPageHeightInMM: Double;
    R666  : TRect2Df;
    //E1    : TEntite;
    //Grayed: boolean;
  begin
    if not FDoDrawPages then Exit;
    with TmpBuffer.Canvas do
    begin
      // todo
      Pen.Width:=0;
      Pen.Color:=clBlue;
      Brush.Style:=bsClear;

      Font.Height := 4;
      Font.Color:=clRed;
      QInvEchelle := 1 / (1000*FEchelle);
      QPageWidthInMM  := Pixels2MillimetresX(Printer.PageWidth);
      QPageHeightInMM := Pixels2MillimetresY(Printer.PageHeight);

      L1 := QPageWidthInMM   * QInvEchelle;
      H1 := QPageHeightInMM  * QInvEchelle;
      for i:=0 to FNbPagesX-1 do
        for j:=0 to FNbPagesY-1 do
        begin
          PM.setFrom(FRXMini + i * L1, FRYMini + j * H1);
          R666.X1 := PM.X;
          R666.Y1 := PM.Y;

          P1   := GetCoordsPlan(PM);
          PM.setFrom(FRXMini + (1+i) * L1, FRYMini + (1+j) * H1);
          R666.X2 := PM.X;
          R666.Y2 := PM.Y;
          P2   := GetCoordsPlan(PM);
          Brush.Style := bsSolid;      // toujours spécifier styles de brosses
          Brush.Color := IIF(FTableauPagesDrawn[i,j], clWhite, clGray);

          Rectangle(P1.X, P1.Y, P2.X, P2.Y);
          //TextOut(p1.X + 2, p1.Y-(Font.Height shl 2), format('%d.%d',[i,j]));
          TextOut(p1.X + 2, p1.Y-(Font.Height shl 2), format('%d',[1 + FNbPagesY*i+j]));
        end;
    end;
  end;

  procedure DrawQuadrilles(const QdrSpc: double; const QGridColor: TColor);
  var
    P1, P2: TPoint;
    C1, C2: TPoint2Df;
    Coin1, Coin2: TPoint3Df;
    t: integer;
    A,B, B0: double;
  begin
    TmpBuffer.Canvas.Pen.Color := QGridColor;
    TmpBuffer.Canvas.Pen.Width := 0;
    Coin1 := FDocTopo.GetCoordsMini();
    Coin2 := FDocTopo.GetCoordsMaxi();
    AfficherMessage(Format('Quadrillage: Spc: %.0f - X = (%.0f, %.0f) - Y = (%.0f, %.0f',
                   [QdrSpc, Coin1.X, Coin2.X, Coin1.Y, Coin2.Y]));
    t := trunc(Coin1.X / QdrSpc);
    A := QdrSpc * t;
    while (A < Coin2.X) do
    begin
      // AfficherMessage(Format('%.2f  %.2f',[A, QdrEspc]));
      C1.setFrom(A, Coin1.Y);
      C2.setFrom(A, Coin2.Y);
      P1 := GetCoordsPlan(C1);
      P2 := GetCoordsPlan(C2);

      TmpBuffer.Canvas.MoveTo(P1.X, P1.Y);
      TmpBuffer.Canvas.LineTo(P2.X, P2.Y);
      A := A + QdrSpc;
    end;
    t := trunc(Coin1.Y / QdrSpc);
    A := QdrSpc * t;
    while (A < Coin2.Y) do
    begin
      C1.setFrom(Coin1.X, A);
      C2.setFrom(Coin2.X, A);
      P1 := GetCoordsPlan(C1);
      P2 := GetCoordsPlan(C2);
      TmpBuffer.Canvas.MoveTo(P1.X, P1.Y);
      TmpBuffer.Canvas.LineTo(P2.X, P2.Y);
      A := A + QdrSpc;
    end;
  end;
  //procedure DrawGaleries(const Filled: boolean);
  procedure DrawPolygonals();
  var
    i: integer;
    E: TBaseStation;
    P1, P2: TPoint;
    PM    : TPoint2Df;
  begin
    // propriétés de pinceau et de brosses par défaut
    TmpBuffer.Canvas.Brush.Style := bsSolid;
    TmpBuffer.Canvas.Pen.Style   := psSolid;
    TmpBuffer.Canvas.Brush.Color := clSilver;
    TmpBuffer.Canvas.Pen.Color   := clBlack;
    TmpBuffer.Canvas.Pen.Width   := 1;

    for i:=0 to FDocTopo.GetNbBaseStations() - 1 do
    begin
      E := FDocTopo.GetBaseStation(i);
      //if (E.Type_Entite = tgENTRANCE) then Continue;
      if (Not E.Enabled) then Continue;  // prise en compte du MétaFiltre
      PM.setFrom(E.PosExtr0.X, E.PosExtr0.Y);
      P1 := GetCoordsPlan(PM);
      PM.setFrom(E.PosStation.X, E.PosStation.Y);
      P2 := GetCoordsPlan(PM);
      TmpBuffer.Canvas.Pen.Color   := E.Couleur;
      TmpBuffer.Canvas.Line(P1, P2);
    end;
  end;

begin
  AfficherMessage(Format('%s.DrawApercu',[ClassName]));
  try
    TmpBuffer := TBitmap.Create;
    with TmpBuffer do begin
      Height:= PaintBoxVue.Height;
      Width := PaintBoxVue.Width;
      //affichermessage(format('%d %d',[PaintBoxVue.Height,PaintBoxVue.Width]));
      R.Left:=PaintBoxVue.Left;
      R.Top :=PaintBoxVue.Top;
      R.Bottom:=PaintBoxVue.Top  + PaintBoxVue.Height;
      R.Right :=PaintBoxVue.Left + PaintBoxVue.Width;
      Canvas.Brush.Color:=clWhite;
      Canvas.FillRect(R);
      // dessin des limites
      //DrawBoundingBox();
      // dessin des pages
      DrawPages();
      // dessin des éléments
      DrawQuadrilles(FParamsVue2D.SecGrid.Spacing , FParamsVue2D.SecGrid.Color);
      DrawQuadrilles(FParamsVue2D.MainGrid.Spacing, FParamsVue2D.MainGrid.Color);

      DrawPolygonals();
    end;
    PaintBoxVue.Canvas.CopyRect(R, TmpBuffer.Canvas, R);
  finally
    FreeAndNil(TmpBuffer);//TmpBuffer.Free;
  end;
end;

// retourne les index de pages en fonction des coordonnées réelles
procedure TfrmPrintingCenterExt.CalcIndexPageIncludingPt(const Pt: TPoint2Df; var IdxX, IdxY: integer);
var
  i,j   : integer;
  L1, H1, QInvEchelle, QPageWidthInMM, QPageHeightInMM: Double;
  R666  : TRect2Df;
begin
  IdxX:=-1;
  IdxY:=-1;
  AfficherMessage(Format('%s.CalcIndexPageIncludingPt(%.2f, %.2f)',
                         [ClassName, Pt.X, Pt.Y]));
  QInvEchelle := 1 / (1000*FEchelle);
  QPageWidthInMM  := Pixels2MillimetresX(Printer.PageWidth);
  QPageHeightInMM := Pixels2MillimetresY(Printer.PageHeight);

  L1 := QPageWidthInMM  * QInvEchelle;
  H1 := QPageHeightInMM * QInvEchelle;
  for i:=0 to FNbPagesX-1 do
  begin
    for j:=0 to FNbPagesY-1 do
    begin
      R666.X1 := FRXMini + i * L1;
      R666.Y1 := FRYMini + j * H1;
      R666.X2 := FRXMini + (1+i) * L1;
      R666.Y2 := FRYMini + (1+j) * H1;
      if (IsInRange(Pt.X, R666.X1, R666.X2) and
          IsInRange(Pt.Y, R666.Y1, R666.Y2)) then
      begin
        IdxX:=i; IdxY:=j;
        AfficherMessage(Format('Page %d, %d sélectionnée',[i,j]));
        Exit;
      end;
    end;  // for i,j
  end;
end;

//******************************************************************************
// imprimer la topo
// Statut: OK
procedure TfrmPrintingCenterExt.ImprimerLaTopo();
var
  L,C,N,Q: integer;
begin
  pnlPrintSettings.Visible     := False;
  pnlProgressPrinting.Visible  := True;
  lbPrintCurrent.Caption := GetResourceString(rsPRN_START_PRINTING);
  //FVue2DParams := GetVue2DParamsFromDialog();

  Printer.BeginDoc;
  AfficherMessage('>> 002');
  N:=0;
  progbarPrinting.Min := 0;
  Q := FNbPagesX * FNbPagesY;
  progbarPrinting.Max := Q;
  try
    for C:=0 to FNbPagesX-1 do
    begin
      for L:=0 to FNbPagesY-1 do
      begin
        Inc(N);
        progbarPrinting.Position := N;
        if (Not FTableauPagesDrawn[C,L]) then Continue;
        try
          AfficherMessage(Format('>> %d-%d - 003',[c,l]));
          PrintAPage(FEchelle, L, C);
          Printer.NewPage;
          Application.ProcessMessages;
          lbPrintCurrent.Caption:= Format('Printing %d/%d', [N,Q]);
        except
          AfficherMessage(Format('>> Printing error in page %d %d',[C,L]));
        end;
      end;
    end;
    Printer.EndDoc;
    AfficherMessage(GetResourceString(rsPRN_PRINTING_DONE));
  except
  end;
  //*)
  pnlProgressPrinting.Visible  := False;
  pnlPrintSettings.Visible     := True;
end;
//******************************************************************************
// imprimer une page

procedure TfrmPrintingCenterExt.PrintAPage(const QEchelle: double; const L,C: integer);
var
  WU: String;
  MyPageTopo: TFichesPointTopo;
  MyEchelle: Double;
begin
  MyPageTopo := TFichesPointTopo.Create;
  try
    AfficherMessage(Format('Impression carré %d.%d (page %d)',[L,C, Printer.PageNumber]));
    WU := ChooseString(Ord(Printer.Orientation), ['PORTRAIT', 'LANDSCAPE', 'REVERSE LANDSCAPE', 'REVERSEPORTRAIT']);
    AfficherMessage('-- Orientation: ' + WU);
    MyEchelle := QEchelle;
    //if (Printer.Orientation = poLandscape) then MyEchelle := QEchelle / sqrt(2);

    MyPageTopo.Initialiser(FDocTopo, FTextures, FParamsVue2D);
    MyPageTopo.CreerUnePage(L, C, MyEchelle);
  finally
  end;
end;


//******************************************************************************
procedure TfrmPrintingCenterExt.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Finaliser();
end;

procedure TfrmPrintingCenterExt.btnPreviewClick(Sender: TObject);
begin
  //FVue2DParams := GetVue2DParamsFromDialog();
  //CalcNbPagesAndEmpty;
  Recadrer();
  DrawApercu();
end;

procedure TfrmPrintingCenterExt.btnStartImpressionClick(Sender: TObject);
begin
  if (QuestionOuiNon('Lancer l''impression ?')) then ImprimerLaTopo();
end;

procedure TfrmPrintingCenterExt.btnApplyParams2DClick(Sender: TObject);
begin
  FEchelle := 1 / editEchelle.value;
  CalcNbPagesAndEmpty();
  Recadrer();
  DrawApercu();
end;

procedure TfrmPrintingCenterExt.btnParametrerVue2DClick(Sender: TObject);
begin
  if (ParametrerVue2D(FParamsVue2D)) then
  begin
    SetControlsWithParamsVue2D(FParamsVue2D);
    Recadrer();
    DrawApercu();
  end;
end;

procedure TfrmPrintingCenterExt.btnSelectPrinterClick(Sender: TObject);
begin
  SelectionnerImprimante(Printer);
  SetMyCurrentPrinter();
end;






procedure TfrmPrintingCenterExt.FormCreate(Sender: TObject);
begin
  chkRegle.Caption             := GetResourceString(rsREGLE);

  grbPrinterNames.Caption      := GetResourceString(rsPRN_TBPRINTER);
  grbQuadrillage.Caption       := GetResourceString(rsGRBX_QUADRILLAGE);
  lbQdrMain.Caption            := GetResourceString(rsQDR_MAIN);
  lbQdrSec.Caption             := GetResourceString(rsQDR_SECONDARY);

  lbTitreQdrType.Caption       := GetResourceString(rsQDR_TYPE);
  lbTitreQdrSpacing.Caption    := GetResourceString(rsQDR_SPACING);
  lbTitreQdrColor.Caption      := GetResourceString(rsQDR_COLOR);

  lbEchelle.Caption            := GetResourceString(rsECHELLE);
  btnStartImpression.Caption   := GetResourceString(rsSTARTPRINTING);

  btnParametrerVue2D.Caption   := GetResourceString(rsPRN_PARAMS_VUE);
end;

procedure TfrmPrintingCenterExt.PaintBoxVueDblClick(Sender: TObject);
var
  PM: TPoint2Df;
  IdxX, IdxY: integer;
begin
  PM:=GetCoordsMonde(FPP);
  IdxX := 0; IdxY := 0;
  CalcIndexPageIncludingPt(PM, IdxX, IdxY);
  if (IdxX > -1) and (IdxY>-1) then begin
    affichermessage(format('%d %d',[IdxX, IdxY]));
    FTableauPagesDrawn[IdxX, IdxY]:=Not(FTableauPagesDrawn[IdxX, IdxY]);
    DrawApercu;
  end;
end;

procedure TfrmPrintingCenterExt.PaintBoxVueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  PM: TPoint2Df;
begin
  try
    FPP.X:=X;
    FPP.Y:=Y;
    PM := GetCoordsMonde(FPP);
    lbMouseCoordinates.Caption:=Format('%.2f, %.2f',[PM.X, PM.Y]);
  except
  end;
end;

procedure TfrmPrintingCenterExt.PaintBoxVuePaint(Sender: TObject);
begin
  DrawApercu;
end;

end.
(*
BBX := MakeTRect2Df(BP.PosStation.X - HalfWidth, BP.PosStation.Y - HalfWidth,
                        BP.PosStation.X + HalfWidth, BP.PosStation.X + HalfWidth);
    QExporterTopoEnImage(BBX, TmpBuffer);
//*)



procedure TfrmPrintingCenterExt.CreerUneFiche(const BP: TBaseStation);
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

begin
  if (FRedessinInProcess) then
  begin
    AfficherMessageErreur(Format('[FRedessinInProcess = %s] Redessin en cours', [BoolToStr(FRedessinInProcess)]));
    exit;
  end;
  // définition de la taille de la fiche
  LargeurFiche := Pixels2MillimetresX(Printer, Printer.PageWidth);
  HauteurFiche := Pixels2MillimetresY(Printer, Printer.PageHeight);

  LargeurExtraitPlan := LargeurFiche - 10.00;
  QPosImgX := (LargeurFiche - LargeurExtraitPlan) * 0.5;
  QPosImgY := MARGE_PERIMETRIQUE + 40.00;

  HauteurExtraitPlan := LargeurExtraitPlan;



  FRedessinInProcess := True;   // Armement du sémaphore de dessin
  HalfWidth := 50.00;
  // modification de styles d'objets
  QOldStyleCourbe := FMyDocDessin.GetStyleCourbe(IDX_STYLE_COURBE_PAROIS);
  QSC := QOldStyleCourbe;
  QSC.LineWidth := Millimetres2PixelsXf(Printer, 0.50);
  QSC.LineColor := clBlue;
  FMyDocDessin.PutStyleCourbe(IDX_STYLE_COURBE_PAROIS, QSC);
  WU := GetToporobotIDStationAsString(BP, true);
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
                                                 BGRA(Red(FBackGroundColor), Green(FBackGroundColor), Blue(FBackGroundColor), 255));
  try
    BBX := MakeTRect2Df(BP.PosStation.X - HalfWidth, BP.PosStation.Y - HalfWidth,
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
    FreeAndNil(TmpBuffer);//TmpBuffer.Free;
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

end.

