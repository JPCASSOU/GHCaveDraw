// 07/04/2014: Révision du code
// 08/04/2014:
// -- Ajout de DrawPolygonPlein(): Dessin d'un polygone sans utilisation de CSS
// -- Inversion des paramètres de DrawBezierCurve()
// -- Fonction Initialise(): Nom de l'application automatique
// Dessin de polygones: Utilise 'path'
// 09/04/2014:
// -- ViewBox OK
// -- Export SVG et XHTML
// -- Adaptation dans GHCaveDraw
// 06/05/2015: Modification de l'initialisateur
// Positionnement des symboles et textes:
// - Point de base: toujours x="0,00" y="0.00"
// - utilisation de transform pour mise en position définitive
// transform="translate=(<X>, <Y>) [scale=(<fx, fy>)] [rotate=<angle_in_degrees>]"

// 16/10/2019: Cette unité devient indépendante de GHTopo
// 18/11/2019: Nouvelle version à monter sur TDGCContext2D
{$MODE delphi}{$H+}
{$WARNING 18/11/2019: Nouvelle version à monter sur TDGCContext2D}
unit SVGCanvasUnit;
interface
uses
  SysUtils,
  Classes,
  Math,
  Graphics,
  Forms, Dialogs;
//******************************************************************************
type TSVGPoint2Df = record
  X: double;
  Y: double;
end;
type TSVGBezierArc = record
  PT1 : TSVGPoint2Df;
  PC1 : TSVGPoint2Df;
  PC2 : TSVGPoint2Df;
  PT2 : TSVGPoint2Df;
end;

type TArraySVGPoints2Df = array of TSVGPoint2Df;
type TSVGProcedure      = procedure of object;
// liste provisoire de vertex
type TSVGTableVertex = class(TList)
private

public
  function GetNbElements(): integer;
  procedure ClearListe();
  function AddElement(const V: TSVGPoint2Df): boolean;
  function GetElement(const Idx: integer): TSVGPoint2Df;
  function RemoveElement(const Idx: integer): boolean;
end;

//*****************************************************************************
type TSVGTableArcs = class(TList)
private

public
  function GetNbElements(): integer;
  procedure ClearListe();
  function AddElement(const V: TSVGBezierArc): boolean; overload;
  function GetElement(const Idx: integer): TSVGBezierArc;
  function RemoveElement(const Idx: integer): boolean;
end;

//*****************************************************************************


// Class TCanvas ***************************************************************
type TSVGCanvas = class
    constructor Create;
    destructor  Destroy; override;
  private
    //FSVGFile: string;
    FProcSendError  : TSVGProcedure;
    FListeVertex    : TSVGTableVertex;
    FListeBezierArcs: TSVGTableArcs;
    FLastError      : string;
    FDoGenerateXHTML: boolean;
    FCommentaire    : string;
    FNbWrittenLines : integer;
    FScale: double;

    FXMin,
    FXMax,
    FYMin,
    FYMax : Double;

    // pour SVG uniquement: définition de la chaine de caractères de description
    // du crayon et de la brosse
    FPenStr  : string;
    FBrushStr: string;
    // limites du dessin
    procedure SetDrawingBounds(const X1, Y1, X2, Y2: Double);
     // conversion de coordonnées repère GHCD vers repère SVG
    function  CoordsGCSToSVG(const MyPt: TSVGPoint2Df): TSVGPoint2Df; overload;
    function  CoordsGCSToSVG(const QX, QY: double): TSVGPoint2Df; overload;

  public

    function  InitializeDocument(const QFileName: string; const DoXHTML: boolean; const QTitre, QDescro: string;
                                 const X1, Y1, X2, Y2: Double;
                                 const ProcCallbackError: TSVGProcedure): boolean;
    procedure FinalizeDocument();
    procedure SendError(const Msg: string);
    function GetLastError(): string;
    // groupes
    procedure BeginGroupe(const GroupName: string; const Description: string = ''; const dx: double = 0.00; const dy: double = 0.00);
    procedure BeginSubGroupe(const MainGroupName, SubGroupName, Description, SVGStyle: string);
    procedure EndSubGroupe(const MainGroupName, SubGroupName: string);
    procedure EndGroupe(const GroupName: string);
    // définition des patterns
    procedure BeginPatternsSection();
    procedure EndPatternsSection();
    // définition des feuilles de styles
    procedure BeginStylesSection();
    procedure EndStylesSection();
    procedure WriteStyleLinePolygoneTexte(const QNomstyle: string;
                                          const QLineColor: TColor;
                                          const QLineOpacity: byte;
                                          const QLineWidthInMM: double;
                                          const QLineStyle: TPenStyle;
                                          const QFillColor: TColor;
                                          const QFillOpacity: byte;
                                          const QFillStyle: TBrushStyle;
                                          const QFontName: string;
                                          const QFontSizeInMM: double;
                                          const QFontColor: TColor;
                                          const QFontOpacity: byte;
                                          const QFontAttr: TFontStyles;
                                          const QDescroStyle: string);
    // section dessin (contenu)
    procedure BeginDrawingSection();
    procedure EndDrawingSection();
    //****************************
    // définition de commentaires
    procedure WriteCommentaire(const s: string);
    procedure WriteCommand(const s: string);
    procedure WriteVerbatimLine(const s: string);

    // définition de couches
    procedure BeginLayer(const LayerName: string);
    procedure EndLayer(const LayerName: string);
    // routines de dessin
    procedure DrawPoint(const CSSClass: string; const X,Y: Double);
    procedure DrawLine(const CSSClass: string; const X1, Y1, X2, Y2: Double);
    procedure DrawCircle(const CSSClass: string; const XC, YC, R1, R2: double);
    procedure DrawArc(const CSSClass: string; const XC, YC, R, Ang0, Ang1: double; const DoPie: boolean);

    // Pour les entités à liste de sommets
    procedure BeginListeVertex();
    procedure AddVertex(const QX, QY: double);
    procedure EndListeVertex();
    procedure BeginListeBezierArcs();
    procedure AddBezierArc(const A: TSVGBezierArc); overload;
    procedure AddBezierArc(const X0, Y0, X1, Y1, X2, Y2, X3, Y3: double); overload;
    procedure EndListeBezierArcs();
    procedure DrawBezierCurve(const CSSClass: string; const Closed: boolean; const PreserveListeArcs: boolean = false);
    procedure DrawPolygonOrPolyline(const CSSClass: string; const Closed: boolean; const PreserveListeVertex: boolean = false);
    procedure DrawPolylign(const CSSClass: string; const PreserveListeVertex: boolean = false);
    procedure DrawPolygon(const CSSClass: string; const PreserveListeVertex: boolean = false);
    procedure DrawPolygoneInscrit(const CSSClass: string; const N: integer; const X0, Y0, R: double);

    procedure DrawPolygonWithoutStyle(const CouleurLigne, CouleurRemplissage: TColor;
                                      const LigneWidth: double;
                                      const Closed: boolean;
                                      const PreserveListeVertex: boolean);





    function  CalcSVGTextOrientation(const A: double): double;
    procedure DrawTexte(const CSSClass: string;  const Alignment: byte; const X,Y, Orientation: Double; const Text: string);
    procedure DrawImage(const FichierImage: string; const X, Y: double; const W, H: double);

    function  SVGColor(const C:TColor):string;
    procedure DrawTriangle(const CSSClass: string; const X1, Y1, X2, Y2, X3, Y3: Double);
    // dessin d'un  rectangle
    function  DrawRectangle(const CSSClass: string; const X1, Y1, X2, Y2: double): boolean;
    // dessin d'un symbole
    function  DrawSymbole(const IDSymbole: string; const X, Y, L, H, Rotate: double): boolean;
    //*******************
    property  Commentaire: string read FCommentaire write FCommentaire;
    property  Scale: double read FScale write FScale;
end;
//*)
implementation
uses
  DGCDummyUnit; // pour éviter le bug de 'Fin de code source non trouvée
var
  pSVGFILE: TextFile;

//******************************************************************************
// fonctions utilitaires pour le mode indépendant de GHTopo
procedure QPass();
begin
  ;; // ne fait rien
end;

function QFormatterNombreReel(const X: double): string;
var
  EWE: String;
begin
  EWE := Format('%.3f', [X]);
  Result := StringReplace(EWE, DefaultFormatSettings.DecimalSeparator, '.', [rfIgnoreCase, rfReplaceAll]);
end;

function QColorToHTMLColor(const Couleur: TColor): string;
begin
  Result := Format('#%.2X%.2X%.2X', [Red(Couleur), Green(Couleur), Blue(Couleur)]);
end;

procedure DisplayMsg(const Str: string);
begin
  ShowMessage(Str);
end;
procedure WriteLine(const S: string);
begin
  WriteLn(pSVGFILE, s);
end;

constructor TSVGCanvas.Create;
begin
 inherited Create;
 DisplayMsg(Format('%s.Create',[ClassName]));
 try

 except

 end;
end;
destructor TSVGCanvas.Destroy;
begin
 DisplayMsg(Format('%s.Free',[ClassName]));
 try

 finally

 end;
 inherited Destroy;
end;


procedure TSVGCanvas.DrawArc(const CSSClass: string; const XC, YC, R, Ang0, Ang1: double; const DoPie: boolean);
var
  AD, AA   : double;
  WU, QCmd: String;
  largeArcFlag: byte;
  PointDepart, PointArrivee: TSVGPoint2Df;
begin
  largeArcFlag := ifthen((AD - AA) <= 180.0, 0, 1);
  AD := degtorad(Ang0);
  AA := degtorad(Ang1);
  PointDepart  := CoordsGCSToSVG(XC + R * cos(AD), YC + R * sin(AD));
  PointArrivee := CoordsGCSToSVG(XC + R * cos(AA), YC + R * sin(AA));
  WU := 'M';
  if (DoPie) then WU += Format(' %.3f, %.3f ', [XC, YC]);
  WU += Format('%.3f, %.3f A %.3f, %.3f %d %d %d %.3f, %.3f',
               [PointDepart.X, PointDepart.Y,
                R, R,
                0, largeArcFlag, 0,
                PointArrivee.X, PointArrivee.Y]);
  if (DoPie) then WU += 'Z';
  QCmd := Format('  <path class="%s" d="%s" />', [CSSClass, WU ]);
  WriteLine(QCmd);
end;

procedure TSVGCanvas.BeginListeVertex();
begin
  FListeVertex.ClearListe();
end;

procedure TSVGCanvas.AddVertex(const QX, QY: double);
var
  V: TSVGPoint2Df;
begin
  V.X := QX;
  V.Y := QY;
  FListeVertex.AddElement(V);
end;

procedure TSVGCanvas.EndListeVertex();
begin
  QPass; // Ne fait rien. EndListeVertex() est juste pour des questions de formalisme
end;
//******************************************************************************
procedure TSVGCanvas.BeginListeBezierArcs();
begin
  FListeBezierArcs.ClearListe();
end;

procedure TSVGCanvas.AddBezierArc(const A: TSVGBezierArc);
begin
  FListeBezierArcs.AddElement(A);
end;

procedure TSVGCanvas.AddBezierArc(const X0, Y0, X1, Y1, X2, Y2, X3, Y3: double);
var
  A: TSVGBezierArc;
begin
  A.PT1.X := X0;
  A.PT1.Y := Y0;
  A.PC1.X := X1;
  A.PC1.Y := Y1;
  A.PC2.X := X2;
  A.PC2.Y := Y2;
  A.PT2.X := X3;
  A.PT2.Y := Y3;
  FListeBezierArcs.AddElement(A);
end;

procedure TSVGCanvas.EndListeBezierArcs();
begin
  QPass; // Ne fait rien. EndListeBezierArc() est juste pour des questions de formalisme
end;



// Conversion des coordonnées
function TSVGCanvas.CoordsGCSToSVG(const MyPt: TSVGPoint2Df): TSVGPoint2Df;
begin
  Result.X := MyPt.X - FXMin;
  Result.Y := (FYMax - MyPt.Y);
end;

function TSVGCanvas.CoordsGCSToSVG(const QX, QY: double): TSVGPoint2Df;
begin
  Result.X := QX - FXMin;
  Result.Y := (FYMax - QY);
end;

//**** Définition des pinceaux et couleurs
function  TSVGCanvas.SVGColor(const C:TColor):string;
var
  r,g,b: byte;
begin
  R := Red(C); G := Green(C); B := Blue(C);
  Result:=Format('#%.2X%.2X%.2X',[R,G,B]);
end;



//*******************
procedure TSVGCanvas.SetDrawingBounds(const X1, Y1, X2, Y2: Double);
begin
  FXMin := Min(X1, X2); // insensible à l'ordre de X1 et X2
  FYMin := Min(Y1, Y2);
  FXMax := Max(X1, X2);
  FYMax := Max(Y1, Y2);
end;

procedure TSVGCanvas.BeginPatternsSection();
begin
  WriteCommentaire('*** Patterns ***');
  WriteCommand('<defs>');
end;

procedure TSVGCanvas.EndPatternsSection();
begin
  WriteCommand('</defs>');
  WriteCommentaire('*** End Patterns ***');
end;

procedure TSVGCanvas.BeginStylesSection();
begin
  WriteCommentaire('*** Style sheets ***');
  WriteCommand('<defs>');
  WriteCommand('  <style type="text/css">');
  WriteVerbatimLine('    <![CDATA[');
end;

procedure TSVGCanvas.EndStylesSection();
begin
  WriteVerbatimLine('    ]]>');
  WriteCommand('  </style>');
  WriteCommand('</defs>');
  WriteCommentaire('*** End Style sheets ***');
end;


procedure TSVGCanvas.WriteStyleLinePolygoneTexte(const QNomstyle: string;
                                                 const QLineColor: TColor;
                                                 const QLineOpacity: byte;
                                                 const QLineWidthInMM: double;
                                                 const QLineStyle: TPenStyle;
                                                 const QFillColor: TColor;
                                                 const QFillOpacity: byte;
                                                 const QFillStyle: TBrushStyle;
                                                 const QFontName: string;
                                                 const QFontSizeInMM: double;
                                                 const QFontColor: TColor;
                                                 const QFontOpacity: byte;
                                                 const QFontAttr: TFontStyles;
                                                 const QDescroStyle: string);
var
  EWE, WU, QCSSFontStyle, QCSSFontDecoration, QCSSFontWeight, QSVGStyleLine: string;
  // Pointillés:
  // Paramètre: Tableau P1 d'entiers
  // Elément d'indice pair: longueur d'un trait (en unités)
  // Elément d'indice impair: longueur d'un espace
  // Le nombre d'éléments doit être pair.
  // Exemple: MakePointilles([5, 3, 1, 3, 1, 3]) donne
  //   -----   -   -   -----   -   -
  //     5   3 1 3 1 3   5   3 1 3 1 ...
  function MakePointilles(const P1: array of integer): string;
  var
    QAT: string;
    i: Integer;
  begin
    Result := 'stroke-dasharray:';
    for i := 0 to High(P1) do
    begin
      if (i = High(P1)) then QAT := ';' else QAT := ', ';
      Result += Format('%d%s', [P1[i], QAT]);
    end;
  end;
begin
  if ((-1 = QFillColor) or (QFillStyle = bsClear)) then WU := 'none' else WU := QColorToHTMLColor(QFillColor);
  QCSSFontStyle      := 'normal';
  QCSSFontWeight     := 'normal';
  QCSSFontDecoration := 'none';
  if (fsItalic     in QFontAttr) then QCSSFontStyle  := 'italic';
  if (fsBold       in QFontAttr) then QCSSFontWeight := 'bold';
  if (fsUnderline  in QFontAttr) then QCSSFontWeight := 'underline';
  QSVGStyleLine := '';
  case QLineStyle of
    psSolid: ;
    psDash       : QSVGStyleLine := MakePointilles([5, 5]);                 //-----     -----
    psDot        : QSVGStyleLine := MakePointilles([1, 2]);                 //-  -  -  -
    psDashDot    : QSVGStyleLine := MakePointilles([5, 3, 1, 3]);           //-----   -   -----   -   -----   -
    psDashDotDot : QSVGStyleLine := MakePointilles([5, 3, 1, 3, 1, 3]);     //-----   -   -   -----  -  -  -----
    psinsideFrame: ;                                                        //  5   3 1 3 1 3
    psPattern    : ;
    psClear      : ;
  end;
  // /!\ Pour les textes, SVG ne fait que tracer les contours des caractères
  // avec les paramètres stroke, fill et fill-opacity
  EWE := '      .%s {' +
         'stroke:%s; ' +
         'stroke-linecap:%s;' +
         'stroke-width:%smm; ' +
         QSVGStyleLine +
         'fill:%s; ' +
         'fill-opacity:%s; '  +
         'font-family:%s;' +
         'font-size:%smm; ' +
         'font-style:%s;'  + // CSS Font-style = {normal | italic}
         'font-weight:%s; ' +// CSS font-weight: normal | bold
         'color:%s;' +
         'text-decoration:%s; ' + // none | underline | overline | line-through | blink : rien | souligné | surligné |
         '} ' +
         '/* %s */';
  WriteVerbatimLine(Format(EWE, [
                           QNomstyle,
                           QColorToHTMLColor(QLineColor),
                           'round',
                           QFormatterNombreReel(QLineWidthInMM), // QLineStyle en attente
                           WU,   // couleur
                           QFormatterNombreReel(QFillOpacity / 256.0),
                           QFontName + ', sans-serif',
                           QFormatterNombreReel(QFontSizeInMM),
                           QCSSFontStyle,
                           QCSSFontWeight,
                           QColorToHTMLColor(QFontColor),
                           QCSSFontDecoration,
                           QDescroStyle
                           ]));
end;

procedure TSVGCanvas.BeginDrawingSection();
begin
  WriteCommentaire('BeginDrawingSection');
end;

procedure TSVGCanvas.EndDrawingSection();
begin
  WriteCommentaire('EndDrawingSection()');
end;


procedure TSVGCanvas.WriteCommentaire(const s: string);
begin
  WriteLine(Format('<!-- %s -->',[s]));
end;
procedure TSVGCanvas.WriteCommand(const s: string);
begin
  WriteLine(Trim(S));
end;
// pour écriture de texte indenté notamment
procedure TSVGCanvas.WriteVerbatimLine(const s: string);
begin
  WriteLine(s);
end;


function  TSVGCanvas.InitializeDocument(const QFileName: string; const DoXHTML: boolean; const QTitre, QDescro: string;
                                        const X1, Y1, X2, Y2: Double;
                                        const ProcCallbackError: TSVGProcedure): boolean;
const
  CHAR_ENCODING = 'UTF-8';
  MARGE_PERIMETRIQUE = 20.00;
var
  QC1, QC2: TSVGPoint2Df;
begin
  Result:=False;
  FProcSendError := ProcCallbackError;
  FLastError := '';
  self.SetDrawingBounds(X1, Y1, X2, Y2);
  FDoGenerateXHTML := DoXHTML;
  FNbWrittenLines:=0;
  // liste provisoire des vertex
  try
    FListeVertex      := TSVGTableVertex.Create;
    FListeBezierArcs  := TSVGTableArcs.Create;
    FListeVertex.ClearListe();
    FListeBezierArcs.ClearListe();
  except
    FLastError := 'Erreur en création de la liste provisoire de vertex';
  end;
  assignFile(pSVGFILE, QFileName);
  try
    ReWrite(pSVGFILE);
    // conversion
    QC1 := CoordsGCSToSVG(FXMin - MARGE_PERIMETRIQUE, FYMin - MARGE_PERIMETRIQUE);
    QC2 := CoordsGCSToSVG(FXMax + MARGE_PERIMETRIQUE, FYMax + MARGE_PERIMETRIQUE);
    //-------------------------------
    // écriture de l'en tête ici
    // ------------------------------
    WriteLine(Format('<?xml version ="1.0" encoding="%s"?>',[CHAR_ENCODING])); //"UTF-8"?>')
    if (FDoGenerateXHTML) then
    begin
      WriteLine('<!DOCTYPE html  PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">');
      WriteLine('<html xmlns="http://www.w3.org/1999/xhtml">');
      WriteLine('<head>');
      WriteLine(Format('<title>%s</title>', [QTitre]));
      WriteLine('</head>');
      WriteLine('<body>');
      WriteLine(Format('<h1>%s</h1>', [QDescro]));
    end
    else
    begin
      WriteLine('<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">');
    end;
    WriteLine('<!-- SVG File generated by ' + ApplicationName);
    WriteLine(Format(' File         : %s',[QFileName]));
    WriteLine(Format(' Date         : %s',[DateTimeToStr(Now)]));
    WriteLine(Format(' Drawing area : (%s, %s) to (%s, %s)',
                    [QFormatterNombreReel(FXMin), QFormatterNombreReel(FYMin),
                     QFormatterNombreReel(FXMax), QFormatterNombreReel(FYMax)]));
    WriteLine(Format(' Comments     : %s',[FCommentaire]));
    WriteLine('-->');
    WriteLine(Format('<svg xmlns="http://www.w3.org/2000/svg" ' +
                     'xml:space="preserve" '+
                     'width="%s%%" height="%s%%" ' +
                     'style="shape-rendering:geometricPrecision; '+
                     'text-rendering:geometricPrecision; image-rendering:optimizeQuality; fill-rule:evenodd; clip-rule:evenodd" ' +
                     'viewBox="%s %s %s %s" ' +
                     'preserveAspectRatio="%s" ' +
                     'xmlns:xlink="http://www.w3.org/1999/xlink">' ,
                     [QFormatterNombreReel(100.0), QFormatterNombreReel(100.0),               // 100% de l'espace
                      QFormatterNombreReel(QC1.X), QFormatterNombreReel(QC2.Y),
                      QFormatterNombreReel(QC2.X), QFormatterNombreReel(QC1.Y),
                      // ordre différent des coordonnées usuelles (dû à la conversion de coordonnées)
                      'xMidYMid meet'             // centré sur la zone
                     ]));
    Result:=True;
  except
    DisplayMsg('Error initializing text file');
    CloseFile(pSVGFILE);
  end;
end;
procedure TSVGCanvas.FinalizeDocument();
begin
  try
    WriteLine('</svg>');
    if (FDoGenerateXHTML) then
    begin
      WriteLine('</body>');
      WriteLine('</html>');
    end;
  finally
    CloseFile(pSVGFILE);
  end;
  try
    FListeVertex.ClearListe();
    FListeBezierArcs.ClearListe();
  finally
    FreeAndNil(FListeVertex);
    FreeAndNil(FListeBezierArcs);
  end;
end;

procedure TSVGCanvas.SendError(const Msg: string);
begin
  FLastError := Msg;
  if (assigned(FProcSendError)) then FProcSendError();
end;

function TSVGCanvas.GetLastError(): string;
begin
  Result := FLastError
end;

//********* Définition de couches
procedure TSVGCanvas.BeginLayer(const LayerName: string);
begin
  WriteLine(Format('<!-- Begin Layer: %s -->',[LayerName]));
  WriteLine(Format('<g id="%s">',[LayerName]));
end;
procedure TSVGCanvas.EndLayer(const LayerName: string);
begin
  WriteLine('</g>');
  WriteLine(Format('<!-- End Layer: %s -->',[LayerName]));
end;
//********* Définition de groupes
procedure TSVGCanvas.BeginGroupe(const GroupName: string; const Description: string = ''; const dx: double = 0.00; const dy: double = 0.00);
begin
  WriteLine(Format('  <!-- Begin Groupe: [%s] = %s -->',[GroupName, Description]));
  WriteLine(Format('  <g id="%s" transform="translate(%s, %s)">',[GroupName, QFormatterNombreReel(dx), QFormatterNombreReel(-dy)]));
  WriteLine(Format('  <desc>%s</desc>',[Description]));

end;
procedure TSVGCanvas.BeginSubGroupe(const MainGroupName, SubGroupName, Description, SVGStyle: string);
begin
  WriteLine(Format('    <!---- Begin SubGroupe: [%s_%s] = %s -->',[MainGroupName, SubGroupName, Description]));
  WriteLine(Format('    <g id="%s_%s" style="%s"><desc>%s</desc>',[MainGroupName, SVGStyle, Description]));
end;
procedure TSVGCanvas.EndSubGroupe(const MainGroupName, SubGroupName: string);
begin
  WriteLine('    </g>');
  WriteLine(Format('    <!-- End SubGroupe: %s_%s -->',[MainGroupName, SubGroupName]));
end;
procedure TSVGCanvas.EndGroupe(const GroupName: string);
begin
  WriteLine('  </g>');
  WriteLine(Format('  <!-- End Groupe: %s -->',[GroupName]));
end;

//********* routines de dessin

// tracé de courbes de Bézier à arcs multiples
(*
function TSVGCanvas.DrawBezierCurve(const QClassStyle: string; const BC : TArraySVGPoints2Df; const Closed: boolean): boolean;
var
  QCmd: string;
  i, Nb: integer;
  Pt: TSVGPoint2Df;
begin
  Result := True;
  Nb := High(BC);
  if (Nb < 1) then Exit;
  // tracé du premier point
  Pt := CoordsGCSToSVG(BC[0]);
  QCmd := Format('  <path class="%s" d="M %s %s C ', [ QClassStyle, QFormatterNombreReel(Pt.X), QFormatterNombreReel(Pt.Y)]);
  for i := 1 to Nb - 1 do    // tracé des points intermédiaires
  begin
    Pt   := CoordsGCSToSVG(BC[i]);
    QCmd += Format('%s %s, ', [QFormatterNombreReel(Pt.X), QFormatterNombreReel(Pt.Y)]);
  end;
  // dernier point et clôture de la commande SVG
  Pt := CoordsGCSToSVG(BC[Nb]);
  QCmd += Format('%s %s', [QFormatterNombreReel(Pt.X), QFormatterNombreReel(Pt.Y)]);
  if (Closed) then QCmd += 'Z';
  QCmd += '" />';
  // flush de la commande path
  WriteLine(QCmd);
end;
//*)
// dessin d'un symbole
function TSVGCanvas.DrawSymbole(const IDSymbole: string; const X, Y, L, H, Rotate: double): boolean;
var
  PM: TSVGPoint2Df;
begin
  PM := CoordsGCSToSVG(X, Y);
  WriteLine(Format('<use  x="%s" y="%s" width="%s" height="%s" transform="translate(%s, %s) rotate(%s)" xlink:href="#%s"/>',
                     [QFormatterNombreReel(0.00), QFormatterNombreReel(0.00),
                     QFormatterNombreReel(L), QFormatterNombreReel(H),
                     QFormatterNombreReel(PM.X), QFormatterNombreReel(PM.Y),
                     QFormatterNombreReel(Rotate), IDSymbole]));
end;
procedure TSVGCanvas.DrawPoint(const CSSClass: string; const X,Y: Double);
begin
  ; //pass;
end;
procedure TSVGCanvas.DrawCircle(const CSSClass: string; const XC, YC, R1, R2: double);
var
  PT1: TSVGPoint2Df;
begin
  PT1 := CoordsGCSToSVG(XC, YC);
  WriteLine(Format('<ellipse class="%s" cx="%s" cy="%s" rx="%s" ry="%s" transform="translate(%s, %s)" />',
            [CSSClass,
             QFormatterNombreReel(0.00), QFormatterNombreReel(0.00),
             QFormatterNombreReel(R1), QFormatterNombreReel(R2),
             QFormatterNombreReel(PT1.X), QFormatterNombreReel(PT1.Y)]));
end;


procedure TSVGCanvas.DrawLine(const CSSClass: string; const X1, Y1, X2, Y2: Double);
var
  PT1, PT2: TSVGPoint2Df;
begin
  PT1 := CoordsGCSToSVG(X1, Y1);
  PT2 := CoordsGCSToSVG(X2, Y2);
  WriteLine(Format('   <line class="%s" x1="%s" y1="%s" x2="%s" y2="%s"/>',
                   [CSSClass,
                    QFormatterNombreReel(PT1.X), QFormatterNombreReel(PT1.Y),
                    QFormatterNombreReel(PT2.X), QFormatterNombreReel(PT2.Y)
                   ]));
end;

// rectangle
function TSVGCanvas.DrawRectangle(const CSSClass: string; const X1, Y1, X2, Y2: double): boolean;
var
  dx, dy: double;
  P1, P2: TSVGPoint2Df;
begin
  P1 := CoordsGCSToSVG(X1, Y1);
  P2 := CoordsGCSToSVG(X2, Y2);
  dx := Abs(P2.X - P1.X);
  dy := P2.Y - P1.Y;
  if (dy < 0) then begin dy := -dy; P1.Y := P2.Y; end;
  WriteCommand(Format('      <rect class="%s" x="%s" y="%s" width="%s" height="%s" />',
                      [CSSClass,
                       QFormatterNombreReel(P1.X), QFormatterNombreReel(P1.Y),
                       QFormatterNombreReel(dx), QFormatterNombreReel(dy)
                      ]));
  Result := True;
end;

procedure TSVGCanvas.DrawTriangle(const CSSClass: string; const X1, Y1, X2, Y2, X3, Y3: Double);
var
  PT1, PT2, PT3: TSVGPoint2Df;
begin
  PT1 := CoordsGCSToSVG(X1, Y1);
  PT2 := CoordsGCSToSVG(X2, Y2);
  PT3 := CoordsGCSToSVG(X3, Y3);

   WriteCommand(Format('<path class="%s" d="M%s, %s L %s, %s L %s, %s Z"/>',
                       [CSSClass,
                        QFormatterNombreReel(PT1.X), QFormatterNombreReel(PT1.Y),
                        QFormatterNombreReel(PT2.X), QFormatterNombreReel(PT2.Y),
                        QFormatterNombreReel(PT3.X), QFormatterNombreReel(PT3.Y)
                        ]));

end;

// dessin d'une polyligne
procedure TSVGCanvas.DrawPolylign(const CSSClass: string; const PreserveListeVertex: boolean = false);
begin
  DrawPolygonOrPolyline(CSSClass, false, PreserveListeVertex); // polylign
end;
procedure TSVGCanvas.DrawPolygon(const CSSClass: string; const PreserveListeVertex: boolean = false);
begin
  DrawPolygonOrPolyline(CSSClass, True, PreserveListeVertex);  // polygon
end;
// polygone régulier inscrit dans le cercle de rayon R
procedure TSVGCanvas.DrawPolygoneInscrit(const CSSClass: string; const N: integer; const X0, Y0, R: double);
const PI_SUR_2 = PI/2.0;
var
  Points: TArraySVGPoints2Df;
  Ang: Extended;
  i: Integer;
  WU: Extended;
begin
  SetLength(Points, n);
  Ang := 2*PI/n;
  BeginListeVertex();
    for i := 0 to n-1 do
    begin
      WU := i * Ang + PI_SUR_2;
      self.AddVertex(X0 + R * cos(WU), Y0 + R * sin(WU));
    end;
  EndListeVertex();
  DrawPolygonOrPolyline(CSSClass, true, false);
end;

// dessin sans utilisation de styles (indispensable pour vue 3D en dégradé)
procedure TSVGCanvas.DrawPolygonWithoutStyle(const CouleurLigne, CouleurRemplissage: TColor;
                                             const LigneWidth: double;
                                             const Closed: boolean;
                                             const PreserveListeVertex: boolean);
var
  i, Nb: integer;
  Cd: string;
  PX: TSVGPoint2Df;
begin
  Nb := FListeVertex.GetNbElements();
  if (0 = Nb) then exit;
  PX := CoordsGCSToSVG(FListeVertex.GetElement(0));
  Cd := Format('   <path fill="%s" stroke="%s" stroke-width="%s" d="',
              [QColorToHTMLColor(CouleurRemplissage), QColorToHTMLColor(CouleurLigne), QFormatterNombreReel(LigneWidth)]);
  Cd += Format('M %s, %s ', [QFormatterNombreReel(PX.X), QFormatterNombreReel(PX.Y)]);
  for i := 1 to Nb - 1 do
  begin
    PX := CoordsGCSToSVG(FListeVertex.GetElement(i));
    Cd := Cd + Format(' L %s, %s',[QFormatterNombreReel(PX.X), QFormatterNombreReel(PX.Y)])
  end;
  if (Closed) then Cd += 'Z';
  Cd += '"/>';
  WriteLine(Cd);
  if (not PreserveListeVertex) then FListeVertex.ClearListe();
end;



// dessin d'un polygone ou d'une polyligne
procedure TSVGCanvas.DrawPolygonOrPolyline(const CSSClass: string;
                                           const Closed: boolean;
                                           const PreserveListeVertex: boolean);
var
  i, Nb: integer;
  Cd: string;
  PX: TSVGPoint2Df;
begin
  if (0 = FListeVertex.GetNbElements()) then Exit;
  PX := CoordsGCSToSVG(FListeVertex.GetElement(0));
  Cd := Format('<path class="%s" ', [CSSClass]);
  Cd += Format('d="M%s, %s ', [QFormatterNombreReel(PX.X), QFormatterNombreReel(PX.Y)]);
  Nb := FListeVertex.GetNbElements();
  if (0 = Nb) then SendError(Format('%s.DrawPolygonOrPolyline: Without vertex (%d)', [ClassName, Nb]));
  for i := 1 to Nb - 1 do
  begin
    PX := CoordsGCSToSVG(FListeVertex.GetElement(i));
    Cd += Format('L %s, %s ',[QFormatterNombreReel(PX.X), QFormatterNombreReel(PX.Y)]);
  end;
  if (Closed) then Cd += 'Z';
  Cd += '"/>';
  WriteLine(Cd);
  // et on vide la liste des vertex
  if (not PreserveListeVertex) then FListeVertex.ClearListe();
end;

// dessin d'ue courbe de Bézier (lissage auto)
procedure TSVGCanvas.DrawBezierCurve(const CSSClass: string;
                                     const Closed: boolean;
                                     const PreserveListeArcs: boolean = false);
var
  i, Nb: integer;
  Cd: string;
  A: TSVGPoint2Df;
  MyArc: TSVGBezierArc;
  function MiouMiou(const QX, QY: double): string;
  var
    PP: TSVGPoint2Df;
  begin
    PP := CoordsGCSToSVG(QX, QY);
    Result := Format('%s %s ', [QFormatterNombreReel(PP.X), QFormatterNombreReel(PP.Y)]);
  end;
begin
  // version 'arcs'
  Nb := FListeBezierArcs.GetNbElements();
  if (0 = Nb) then Exit;
  Cd := Format('<path class="%s" ', [CSSClass]);
  Cd += 'd="';
  for i := 0 to Nb - 1 do
  begin
    MyArc := FListeBezierArcs.GetElement(i);
    Cd += Format('M %s C %s %s %s ',
          [MiouMiou(MyArc.PT1.X, MyArc.PT1.Y),
           MiouMiou(MyArc.PC1.X, MyArc.PC1.Y),
           MiouMiou(MyArc.PC2.X, MyArc.PC2.Y),
           MiouMiou(MyArc.PT2.X, MyArc.PT2.Y)
          ]);
  end;
  if (Closed) then Cd += 'Z';
  Cd += '"/>';
  //***************************
  WriteLine(Cd);
  // et on vide la liste des vertex
  if (not PreserveListeArcs) then FListeVertex.ClearListe();
end;


function TSVGCanvas.CalcSVGTextOrientation(const A: double): double;
begin
  Result := -(Trunc(100 * A) MOD 36000) / 100.0;
end;
// Alignment: idem GHCaveDraw
// Rotation: En degrés et dans le sens trigo, zéro à droite
procedure TSVGCanvas.DrawTexte(const CSSClass: string; const Alignment: byte; const X, Y, Orientation: Double; const Text: string);
var
  QA: string;
  P: TSVGPoint2Df;
  QRotation: Extended;
begin
  QRotation := CalcSVGTextOrientation(Orientation);
  P := CoordsGCSToSVG(X, Y);
  case Alignment of
    0, 1, 4, 7: QA := 'start';
       2, 5, 8: QA := 'middle';
       3, 6, 9: QA := 'end';
  end;
  WriteLine(Format('  <text class="%s" x="%s" y="%s" text-anchor="%s" transform="translate(%s, %s) rotate(%s)"><tspan>%s</tspan></text>',
            [CSSClass,
             QFormatterNombreReel(0.00), QFormatterNombreReel(0.00),
             QA,
             QFormatterNombreReel(P.X), QFormatterNombreReel(P.Y),
             QFormatterNombreReel(QRotation),
             WideString(Text)]));
end;
// implantation d'une image
procedure TSVGCanvas.DrawImage(const FichierImage: string; const X, Y: double; const W, H: double);
var
  P: TSVGPoint2Df;
begin
  P := CoordsGCSToSVG(X, Y);
  WriteVerbatimLine(Format('<image xlink:href="%s" x="%s" y="%s" width="%s" height="%s" />',
                           [FichierImage,
                            QFormatterNombreReel(P.X), QFormatterNombreReel(P.Y),
                            QFormatterNombreReel(W), QFormatterNombreReel(H)]));
end;



//******************************************************************************
// Classes utilitaires: Liste des vertex
function TSVGTableVertex.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TSVGTableVertex.ClearListe();
var
  i, Nb: Integer;
begin
  Nb := self.Count;
  if (0 = Nb) then Exit;
  for i := 0 to Nb - 1 do
  begin
    try
      Dispose(self.Items[i]);
    except
    end;
  end;
  self.Clear();
end;



function TSVGTableVertex.AddElement(const V: TSVGPoint2Df): boolean;
var
  pS : ^TSVGPoint2Df;
begin
  result := false;
  try
    New(pS);
    pS^ := V;
    self.Add(pS);
    Result := True;
  except
  end;
end;

function TSVGTableVertex.GetElement(const Idx: integer): TSVGPoint2Df;
var
  pS : ^TSVGPoint2Df;
begin
  pS := self.Items[Idx];
  Result := pS^;
end;


function TSVGTableVertex.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
  finally
    self.Delete(Idx);
    Result := True;
  end;
end;

{ TSVGTableArcs }

function TSVGTableArcs.GetNbElements(): integer;
begin
  result := self.Count;
end;

procedure TSVGTableArcs.ClearListe();
var
  i, Nb: Integer;
begin
  Nb := self.Count;
  if (0 = Nb) then Exit;
  for i := 0 to Nb - 1 do
  begin
    try
      Dispose(self.Items[i]);
    except
    end;
  end;
  self.Clear();
end;

function TSVGTableArcs.AddElement(const V: TSVGBezierArc): boolean;
var
  pS : ^TSVGBezierArc;
begin
  result := false;
  try
    New(pS);
    pS^ := V;
    self.Add(pS);
    Result := True;
  except
  end;
end;


function TSVGTableArcs.GetElement(const Idx: integer): TSVGBezierArc;
var
  pS : ^TSVGBezierArc;
begin
  pS := self.Items[Idx];
  Result := pS^;
end;

function TSVGTableArcs.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
  finally
    self.Delete(Idx);
    Result := True;
  end;
end;


end.

//******************************************************************************
float smootherstep(float edge0, float edge1, float x) {
  // Scale, and clamp x to 0..1 range
  x = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
  // Evaluate polynomial
  return x * x * x * (x * (x * 6 - 15) + 10);
}

float clamp(float x, float lowerlimit, float upperlimit) {
  if (x < lowerlimit)
    x = lowerlimit;
  if (x > upperlimit)
    x = upperlimit;
  return x;
}
