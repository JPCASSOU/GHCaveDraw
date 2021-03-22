unit DXFCanvasUnit;
{$DEFINE USE_GHTOPO_FPC}
{.$UNDEF USE_GHTOPO_FPC}
{$DEFINE USE_GHCAVEDRAW}
{$UNDEF USE_GHCAVEDRAW}

{$INCLUDE CompilationParameters.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF USE_GHTOPO_FPC}
    StructuresDonnees,
    Common,
  {$ENDIF}
  {$IFDEF USE_GHCAVEDRAW}
    GHCD_Types,
    GeneralFunctions,
  {$ENDIF}
  Math,
  Graphics;

type

{ TSVGCanvas }

 { TDXFCanvas }

 TDXFCanvas = class
    constructor Create;
    destructor  Destroy; override;
  private
    //FSVGFile: string;
    FDoGenerateDXF: boolean;
    FCommentaire  : string;
    FNbWrittenLines: integer;
    FScale: double;
    FFont : TFontPSProperties;
    FPen  : TPenPSProperties;
    FBrush: TBrushPSProperties;
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

  public
    function  InitializeDocument(const QFileName: string; const DoXHTML: boolean; const QTitre, QDescro: string;
                                 const X1, Y1, X2, Y2: Double): boolean;
    procedure FinalizeDocument();
    // groupes
    procedure BeginGroupe(const GroupName, Description: string; const dx, dy: double);
    procedure BeginSubGroupe(const MainGroupName, SubGroupName, Description, SVGStyle: string);
    procedure EndSubGroupe(const MainGroupName, SubGroupName: string);
    procedure EndGroupe(const GroupName: string);
    // définition des patterns
    procedure BeginPatternsSection();
    procedure EndPatternsSection();
    // définition des feuilles de styles
    procedure BeginStylesSection();
    procedure EndStylesSection();
    procedure WriteStyleSolidLine(const Nomstyle: string; const CouleurLigne: TColor; const LargeurLigne: double; const DescroStyle: string);
    procedure WriteStyleSolidPolygone(const Nomstyle: string; const CouleurLigne, CouleurPolygone: TColor; const FillOpacity: byte; const LargeurLigne: double; const DescroStyle: string);
    procedure WriteStyleText(const Nomstyle: string; const QFontName: string; const QFontAttr: TFontStyles; const QFontColor: TColor; const QFontSize: double; const DescroStyle: string);
    // section dessin (contenu)
    procedure BeginDrawingSection();
    procedure EndDrawingSection();

    // définition des couleurs et fontes
    procedure SetPen(const Value: TPenPSProperties);
    function  GetPen: TPenPSProperties;
    procedure SetBrush(const Value: TBrushPSProperties);
    function  GetBrush(): TBrushPSProperties;
    procedure SetFont(const Value: TFontPSProperties);
    procedure SetDefaultPen(const Value: TPenPSProperties);
    procedure SetDefaultBrush(const Value: TBrushPSProperties);

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
    procedure DrawCircle(const CSSClass: string; const XC, YC, R: double);
    procedure DrawArc(const CSSClass: string; const XC, YC, R, Ang0, Ang1: double; const DoPie: boolean);
    procedure DrawPolylign(const CSSClass: string; const Points: TArrayPoints2Df);
    procedure DrawPolygon(const CSSClass: string; const Points: TArrayPoints2Df);
    procedure DrawPolygoneInscrit(const CSSClass: string; const N: integer; const X0, Y0, R: double);

    procedure DrawPolygonSansStyle(const CouleurLigne, CouleurRemplissage: TColor; const LigneWidth: double; const Points: TArrayPoints2Df; const Closed: boolean);
    procedure DrawPolygonOrPolyline(const CSSClass: string; const Points: TArrayPoints2Df; const Closed: boolean);


    procedure DrawText(const CSSClass: string;  const Alignment: byte; const X,Y, Rotation: Double; const Text: string);
    procedure DrawImage(const FichierImage: string; const X, Y: double; const W, H: double);

    function  SVGColor(const C:TColor):string;
    // conversion de coordonnées repère GHCD vers repère SVG
    function  CoordsGCSToSVG(const MyPt: TPoint2Df): TPoint2Df;
    // dessin d'un  rectangle
    function  DrawRectangle(const QClassStyle: string; const X1, Y1, X2, Y2: double): boolean;
    // dessin d'un symbole
    function  DrawSymbole(const IDSymbole: string; const X, Y, L, H, Rotate: double): boolean;
    // tracé de courbes de Bézier à arcs multiples
    function  DrawBezierCurve(const QClassStyle: string; const BC : TArrayPoints2Df; const Closed: boolean): boolean;
    //*******************
    property Commentaire: string read FCommentaire write FCommentaire;
    property Scale: double read FScale write FScale;
end;

implementation

{ TDXFCanvas }

constructor TDXFCanvas.Create;
begin

end;

destructor TDXFCanvas.Destroy;
begin
  inherited Destroy;
end;

procedure TDXFCanvas.SetDrawingBounds(const X1, Y1, X2, Y2: Double);
begin

end;

function TDXFCanvas.InitializeDocument(const QFileName: string; const DoXHTML: boolean; const QTitre, QDescro: string; const X1, Y1, X2, Y2: Double): boolean;
begin

end;

procedure TDXFCanvas.FinalizeDocument();
begin

end;

procedure TDXFCanvas.BeginGroupe(const GroupName, Description: string; const dx, dy: double);
begin

end;

procedure TDXFCanvas.BeginSubGroupe(const MainGroupName, SubGroupName, Description, SVGStyle: string);
begin

end;

procedure TDXFCanvas.EndSubGroupe(const MainGroupName, SubGroupName: string);
begin

end;

procedure TDXFCanvas.EndGroupe(const GroupName: string);
begin

end;

procedure TDXFCanvas.BeginPatternsSection();
begin

end;

procedure TDXFCanvas.EndPatternsSection();
begin

end;

procedure TDXFCanvas.BeginStylesSection();
begin

end;

procedure TDXFCanvas.EndStylesSection();
begin

end;

procedure TDXFCanvas.WriteStyleSolidLine(const Nomstyle: string; const CouleurLigne: TColor; const LargeurLigne: double; const DescroStyle: string);
begin

end;

procedure TDXFCanvas.WriteStyleSolidPolygone(const Nomstyle: string; const CouleurLigne, CouleurPolygone: TColor; const FillOpacity: byte; const LargeurLigne: double; const DescroStyle: string);
begin

end;

procedure TDXFCanvas.WriteStyleText(const Nomstyle: string; const QFontName: string; const QFontAttr: TFontStyles; const QFontColor: TColor; const QFontSize: double; const DescroStyle: string);
begin

end;

procedure TDXFCanvas.BeginDrawingSection();
begin

end;

procedure TDXFCanvas.EndDrawingSection();
begin

end;

procedure TDXFCanvas.SetPen(const Value: TPenPSProperties);
begin

end;

function TDXFCanvas.GetPen: TPenPSProperties;
begin

end;

procedure TDXFCanvas.SetBrush(const Value: TBrushPSProperties);
begin

end;

function TDXFCanvas.GetBrush(): TBrushPSProperties;
begin

end;

procedure TDXFCanvas.SetFont(const Value: TFontPSProperties);
begin

end;

procedure TDXFCanvas.SetDefaultPen(const Value: TPenPSProperties);
begin

end;

procedure TDXFCanvas.SetDefaultBrush(const Value: TBrushPSProperties);
begin

end;

procedure TDXFCanvas.WriteCommentaire(const s: string);
begin

end;

procedure TDXFCanvas.WriteCommand(const s: string);
begin

end;

procedure TDXFCanvas.WriteVerbatimLine(const s: string);
begin

end;

procedure TDXFCanvas.BeginLayer(const LayerName: string);
begin

end;

procedure TDXFCanvas.EndLayer(const LayerName: string);
begin

end;

procedure TDXFCanvas.DrawPoint(const CSSClass: string; const X, Y: Double);
begin

end;

procedure TDXFCanvas.DrawLine(const CSSClass: string; const X1, Y1, X2, Y2: Double);
begin

end;

procedure TDXFCanvas.DrawCircle(const CSSClass: string; const XC, YC, R: double);
begin

end;

procedure TDXFCanvas.DrawArc(const CSSClass: string; const XC, YC, R, Ang0, Ang1: double; const DoPie: boolean);
begin

end;

procedure TDXFCanvas.DrawPolylign(const CSSClass: string; const Points: TArrayPoints2Df);
begin

end;

procedure TDXFCanvas.DrawPolygon(const CSSClass: string; const Points: TArrayPoints2Df);
begin

end;

procedure TDXFCanvas.DrawPolygoneInscrit(const CSSClass: string; const N: integer; const X0, Y0, R: double);
begin

end;

procedure TDXFCanvas.DrawPolygonSansStyle(const CouleurLigne, CouleurRemplissage: TColor; const LigneWidth: double; const Points: TArrayPoints2Df; const Closed: boolean);
begin

end;

procedure TDXFCanvas.DrawPolygonOrPolyline(const CSSClass: string; const Points: TArrayPoints2Df; const Closed: boolean);
begin

end;

procedure TDXFCanvas.DrawText(const CSSClass: string; const Alignment: byte; const X, Y, Rotation: Double; const Text: string);
begin

end;

procedure TDXFCanvas.DrawImage(const FichierImage: string; const X, Y: double; const W, H: double);
begin

end;

function TDXFCanvas.SVGColor(const C: TColor): string;
begin

end;

function TDXFCanvas.CoordsGCSToSVG(const MyPt: TPoint2Df): TPoint2Df;
begin

end;

function TDXFCanvas.DrawRectangle(const QClassStyle: string; const X1, Y1, X2, Y2: double): boolean;
begin

end;

function TDXFCanvas.DrawSymbole(const IDSymbole: string; const X, Y, L, H, Rotate: double): boolean;
begin

end;

function TDXFCanvas.DrawBezierCurve(const QClassStyle: string; const BC: TArrayPoints2Df; const Closed: boolean): boolean;
begin

end;

end.

