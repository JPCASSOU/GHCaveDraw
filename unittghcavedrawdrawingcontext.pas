unit UnitTGHCaveDrawDrawingContext;
// 02/10/2015: Support de la Mention de copyright
// 03/01/2017: Support du type de courbe/polyligne nocMUR_MACONNE pour les murs maçonnés
// 30/08/2020: Pointage temporel
{$INCLUDE CompilationParameters.inc}
interface

uses
  GHCD_Types
  , types
  , UnitDocDessin
  , Classes
  , SysUtils
  , math
  , GeneralFunctions
  , Graphics
  , BGRABitmap
  , BGRABitmapTypes
  , BGRAGradients
  ;
type

{ TGHCaveDrawDrawingContext }

 TGHCaveDrawDrawingContext = class(TBGRABitmap)
  private
    FDocumentDessin       : TDocumentDessin;         // pointeur sur le doc dessin
    FParamsVue2D          : TParamsVue2D;            // éléments dessinés
    FTexturesPolygones    : TTexturesPolygonObject;  // textures
    // limites du dessin
    FRXMini,
    FRXMaxi,
    FRYMini,
    FRYMaxi: double;
    // paramètres internes pour le calcul des coordonnées
    FRappScrReal    : double;
    FInvRappScrReal : double;
    // sauvegarde des pinceaux et crayons
    FOldPenStyle    : TPenStyle;
    FOldPenWidth    : byte;
    FOldPenColor    : TColor;
    FOldPenOpacity  : byte;

    FOldFontName    : string;
    FOldFontColor   : TColor;
    FOldFontStyle   : TFontStyles;
    FOldFontHeight  : integer;

    FOldBrushStyle  : TBrushStyle;
    FOldBrushColor  : TColor;
    FOldBrushOpacity: byte;

    function BasePointIsVisible(const BP: TBaseStation): boolean;
    function CalcFontHeightFromHauteurEnMetres(const Hauteur: double): integer;

    procedure DefineCurveStyle(const ILS: TNatureObjetCourbe; out LS: TStyleCourbe);

    procedure DefineLinesStyle(const ILS: TNatureObjetLigne; out LS: TStyleLigne);
    procedure DefinePolygonStyle(const IPS: TNatureObjetPolygone; out PS: TStylePolygone);
    procedure DefineTextStyle(const ITS: TNatureObjetTexte; out TS: TStyleTexte);
    procedure DrawAsterisk(const Centre: TPoint2Df; const Rayon, DemiEpaisseur: double; const FillColor: TColor);
    procedure DrawBezierArc(const B: TBezierArc; const Barbule: TBarbule; const DoDrawPtsCtrls: boolean);


    procedure DrawCourbe(const MyCourbe: TCourbe; const IdxCourbe: Int64; const DoDrawPtsCtrls: boolean; const DoDrawBasePoints: boolean; const DoUseTStyleCourbe: boolean);
    procedure DrawPolyligne(const MyPolyLine: TPolyLigne; const IdxPolyligne: Int64; const DoDrawSommets: boolean; const QIdxGroupe: integer; const DoUseTStyleCourbe: boolean);

  public
    procedure DefineBrosse(const qStyle: TBrushStyle; const qColor: TColor; const qOpacity: byte);
    procedure DefineCrayon(const qStyle: TPenStyle; const qWidth: integer; const qColor: TColor; const qOpacity: byte);
    procedure DefineBrosseEtCrayon(const qBrushStyle: TBrushStyle; const qBrushColor: TColor; const qBrushOpacity: byte;
                                   const qPenStyle: TPenStyle; const qPenWidth: integer; const qPenColor: TColor; const qPenOpacity: byte);


    procedure DefineFonte(const QFontName: string; const QFontColor: TColor; const QFontStyle: TFontStyles; const QFontHeight: integer);

    procedure RestoreBrosse();
    procedure RestoreCrayon();
    procedure RestoreBrosseEtCrayon();

    procedure RestoreFonte();
    procedure SetDocuTopo(const FD: TDocumentDessin);
    procedure SetParamDessin(const P: TParamsVue2D);
    procedure SetTextures(const QTextures: TTexturesPolygonObject);
    function  GetTextures(): TTexturesPolygonObject;
    procedure SetBounds(const QX1, QY1, QX2, QY2: double);
    function  QGetCoordsPlan(const PM: TPoint2Df): TPoint; overload;
    function  QGetCoordsPlan(const QX, QY: double): TPoint; overload;

    function  QGetCoordsMonde(const PP: TPoint): TPoint2Df;
    procedure DrawQuadrillage();
    procedure BeginDrawing();
    procedure EndDrawing();
    procedure DrawCenteredRectangle(const Centre: TPoint2Df; const HalfWidth, HalfHeight: double; const Angle: double);
    procedure DrawRectangle(const C1, C2: TPoint2Df);
    procedure DrawRectangleRempli(const C1, C2: TPoint2Df; const Linecolor, FillColor: TColor; const LineOpacity, FillOpacity: byte; const WL: integer);
    procedure DrawTriangle(const P1, P2, P3: TPoint2Df; const Linecolor, FillColor: TColor; const LineOpacity, FillOpacity: byte; const WL: integer);
    procedure DrawPointeDeFleche(const BP: TPoint2Df; const AngleRot: double; const LONG_FLECHE: double);
    // dessin d'un polygone centré
    procedure DrawCenteredPolylinePolygone(const Center: TPoint2Df; const ArrPoints: TArrayPoints2Df; const Linecolor, FillColor: TColor; const LineOpacity, FillOpacity: byte; const WL: integer; const PolyClosed: boolean);

    procedure TraceTexte(const XX, YY: Double; const T: string);
    procedure TracerRotatedTexte(const X, Y, Angle: integer; const Texte: string);
    procedure TraceCercle(const XX, YY: Double; const R: integer);
    procedure TraceVers(const XX, YY: Double; const Drawn: boolean); overload;
    procedure TraceVers(const PT: TPoint2Df; const Drawn: boolean); overload;

    procedure DrawShape(const x, y: Double; const TypeSymbole: byte; const L, H: integer; const BC, PC: TColor);
    //----------------
    procedure DessinerBaseStation(const BP: TBaseStation; const QFontHeight: integer);
    procedure DessinerPolyLigne(const MyPolyligne: TPolyLigne; const IdxPolyligne: Int64; const QIdxGroupe: integer);
    procedure DessinerCourbe(const MyCourbe: TCourbe; const IdxCourbe: Int64; const QIdxGroupe: integer);
    procedure DessinerScrap(const MyScrap: TScrap; const IdxScrap: Int64; const DoDrawSommets: boolean; const QIdxGroupe: integer);
    procedure DessinerPolygone(const MyPolygon: TPolygone; const IdxPolygone: Int64; const DoDrawSommets: boolean; const QIdxGroupe: integer);


    procedure DessinerTexte(const T: TTextObject; const DoDrawTextExt: boolean; const QIdxGroupe: Integer);
    procedure DessinerSimpleLigne(const SL: TSimpleLigne;
                              const DoDrawHands: boolean;
                              const QIdxGroupe: integer);
    procedure DessinerSymbole(const EP: TSymbole; const QIdxGroupe: integer);
    procedure DrawImage(const IMG: TImageObject);
    procedure DrawCopyright(const QPosition: TPoint2Df; const StrCopyright: string);
    //----------------
    procedure DrawEchelleNord(const QImgWidth, QImgHeight: integer);

end;

implementation



{ TGHCaveDrawDrawingContext }

function TGHCaveDrawDrawingContext.QGetCoordsPlan (const PM: TPoint2Df): TPoint;
begin
  Result.X:=Round((PM.X - FRXMini) * FRappScrReal);
  Result.Y:=Round((FRYMaxi-PM.Y) * FRappScrReal);
end;
function TGHCaveDrawDrawingContext.QGetCoordsPlan(const QX, QY: double): TPoint;
var
  P1: TPoint2Df;
begin
  P1.setFrom(QX, QY);
  Result := QGetCoordsPlan(P1);
end;

function TGHCaveDrawDrawingContext.QGetCoordsMonde(const PP: TPoint): TPoint2Df;
begin
  result.setFrom( FInvRappScrReal * PP.X + FRXMini,
                 -FInvRappScrReal * PP.Y + FRYMaxi);
end;


procedure TGHCaveDrawDrawingContext.SetBounds(const QX1, QY1, QX2, QY2: double);
var
  RappHLVue: Extended;
  dx: Extended;
begin
  FRXMini := QX1;
  FRXMaxi := QX2;
  FRYMini := QY1;
  FRYMaxi := QY2;
  dx := FRXMaxi - FRXMini;
  // calcul du rapport Hauteur/largeur de vue
  RappHLVue       := Self.Height / self.Width;
  // calcul du rapport Ecran/Réel
  FRappScrReal    := self.Width / dx;
  FInvRappScrReal := 1 / FRappScrReal;
  // comme le repère est orthonormé, on doit recalculer le YMaxi
  FRYMaxi := FRYMini + dx * RappHLVue;
end;

procedure TGHCaveDrawDrawingContext.SetDocuTopo(const FD: TDocumentDessin);
begin

  FParamsVue2D.PenteLimiteDisp := 0.00;
  FDocumentDessin  := FD;
end;

procedure TGHCaveDrawDrawingContext.SetParamDessin(const P: TParamsVue2D);
begin
  FParamsVue2D := P;
end;

procedure TGHCaveDrawDrawingContext.SetTextures(const QTextures: TTexturesPolygonObject);
begin
  FTexturesPolygones := QTextures;
end;

function TGHCaveDrawDrawingContext.GetTextures(): TTexturesPolygonObject;
begin
  Result := FTexturesPolygones;
end;

procedure TGHCaveDrawDrawingContext.TraceCercle(const XX, YY: Double; const R: integer);
var
  Q: Integer;
  PM: TPoint2Df;
  PP: TPoint;
begin
  Q := R shr 1;
  PM.setFrom(XX, YY);
  PP := QGetCoordsPlan(PM);
  Self.CanvasBGRA.EllipseC(PP.X, PP.Y, Q, Q);
end;

procedure TGHCaveDrawDrawingContext.BeginDrawing();
begin
  // crayon, brosse et fontes par défaut
  self.CanvasBGRA.Brush.Texture := nil;
  self.CanvasBGRA.Brush.Style := bsSolid;
  self.CanvasBGRA.Brush.Color := clWhite;
  self.CanvasBGRA.Pen.Style   := psSolid;
  self.CanvasBGRA.Pen.Color   := clBlack;
  self.CanvasBGRA.Pen.Width   := 0;
  self.CanvasBGRA.Font.Color  := clBlack;
  self.CanvasBGRA.Font.Style  := [];
  self.CanvasBGRA.Font.Height := 8;
end;



procedure TGHCaveDrawDrawingContext.EndDrawing();
begin
  pass;
end;

procedure TGHCaveDrawDrawingContext.DrawCenteredRectangle(const Centre: TPoint2Df; const HalfWidth, HalfHeight: double; const Angle: double);
var
  AD, sa, ca: extended;
  PPP: array[0..3] of TPoint;
  function TransformCoin(const dx, dy: double): TPoint;
  var
    EWE: TPoint2Df;
  begin
    EWE.setFrom(Centre.X + (ca * dX - sa * dY),
                Centre.Y + (sa * dX + ca * dY));
    Result := QGetCoordsPlan(EWE);
  end;
begin
  AD := degtorad(Angle);
  SinCos(AD, sa, ca);
  PPP[0] := TransformCoin(-HalfWidth, -HalfHeight);
  PPP[1] := TransformCoin( HalfWidth, -HalfHeight);
  PPP[2] := TransformCoin( HalfWidth,  HalfHeight);
  PPP[3] := TransformCoin(-HalfWidth,  HalfHeight);
  self.CanvasBGRA.Polygon(PPP);
end;

procedure TGHCaveDrawDrawingContext.DrawAsterisk(const Centre: TPoint2Df; const Rayon, DemiEpaisseur: double; const FillColor: TColor);
begin
  DefineBrosseEtCrayon(bsSolid, FillColor, 255, psSolid, 0, FillColor, 255);
    self.CanvasBGRA.Brush.Texture:= nil;
    DrawCenteredRectangle(Centre, Rayon, DemiEpaisseur, 0);
    DrawCenteredRectangle(Centre, Rayon, DemiEpaisseur, 60);
    DrawCenteredRectangle(Centre, Rayon, DemiEpaisseur, 120);
  RestoreBrosseEtCrayon();
end;

procedure TGHCaveDrawDrawingContext.DrawRectangle(const C1, C2: TPoint2Df);
var
  PP1, PP2: TPoint;
begin
  PP1 := QGetCoordsPlan(C1);
  PP2 := QGetCoordsPlan(C2);
  self.CanvasBGRA.Rectangle(PP1.X, PP1.Y, PP2.X, PP2.Y, false);
end;
// dessin d'un rectangle
procedure TGHCaveDrawDrawingContext.DrawRectangleRempli(const C1, C2: TPoint2Df; const Linecolor, FillColor : TColor; const LineOpacity, FillOpacity: byte; const WL: integer);
var
  PP1, PP2: TPoint;
begin
  PP1 := QGetCoordsPlan(C1);
  PP2 := QGetCoordsPlan(C2);
  DefineBrosseEtCrayon(bsSolid, FillColor, FillOpacity, psSolid, WL, Linecolor, LineOpacity);
    self.CanvasBGRA.Rectangle(PP1.X, PP1.Y, PP2.X, PP2.Y, True);
  RestoreBrosseEtCrayon();
end;
// dessin d'un triangle
// très utilisé => procédure distincte
procedure TGHCaveDrawDrawingContext.DrawTriangle(const P1, P2, P3: TPoint2Df;
                                                 const Linecolor, FillColor: TColor;
                                                 const LineOpacity, FillOpacity: byte;
                                                 const WL: integer);
var
  Sommets: array[0..2] of TPoint;
begin
  Sommets[0] := QGetCoordsPlan(P1); // Ne pas toucher
  Sommets[1] := QGetCoordsPlan(P2); // Ne pas toucher
  Sommets[2] := QGetCoordsPlan(P3); // Ne pas toucher
  self.DefineBrosseEtCrayon(bsSolid, FillColor, FillOpacity, psSolid, WL, Linecolor, LineOpacity);
    self.CanvasBGRA.Brush.Texture:= nil;
    self.CanvasBGRA.Polygon(Sommets);
  self.RestoreBrosseEtCrayon();
end;
// dessin d'une pointe de flèche
procedure TGHCaveDrawDrawingContext.DrawPointeDeFleche(const BP: TPoint2Df;
                                                              const AngleRot: double;
                                                              const LONG_FLECHE: double);
const
  PHI_2       = 20 * PI/180;
  PPI         = 0;
var
  E1, E2, E3: TPoint2Df;
  AAA: Double;
begin
  E1 := BP;
  AAA := AngleRot - PHI_2;
  E2.X  := E1.X + LONG_FLECHE * cos(AAA);
  E2.Y  := E1.Y + LONG_FLECHE * sin(AAA);
  AAA := AngleRot + PHI_2;
  E3.X  := E1.X + LONG_FLECHE * cos(AAA);
  E3.Y  := E1.Y + LONG_FLECHE * sin(AAA);
  // flèche
  DrawTriangle(E1, E2, E3, clBlack, clGray, 255, 255, 0);
end;

procedure TGHCaveDrawDrawingContext.DrawCenteredPolylinePolygone(const Center: TPoint2Df;
                                                                 const ArrPoints: TArrayPoints2Df;
                                                                 const Linecolor, FillColor: TColor;
                                                                 const LineOpacity, FillOpacity: byte;
                                                                 const WL: integer;
                                                                 const PolyClosed: boolean);
var
  i, Nb: Integer;
  PPP  : array of TPoint;
begin
  Nb := length(ArrPoints);
  if (Nb < 3) then exit;
  SetLength(PPP, Nb);
  DefineBrosseEtCrayon(bsSolid, FillColor, FillOpacity, psSolid, WL, Linecolor, LineOpacity);
  self.CanvasBGRA.Brush.Texture := nil;
    for i := 0 to Nb - 1 do PPP[i] := self.QGetCoordsPlan(Center.X + ArrPoints[i].X, Center.Y + ArrPoints[i].Y);
    if (PolyClosed) then self.CanvasBGRA.Polygon(PPP) else self.CanvasBGRA.Polyline(PPP);
  RestoreBrosseEtCrayon();
end;


procedure TGHCaveDrawDrawingContext.TraceTexte(const XX, YY: Double; const T: string);
var
  PP: TPoint;
begin
  PP := QGetCoordsPlan(XX, YY);
  self.CanvasBGRA.TextOut(PP.X, PP.Y, T);
end;
procedure TGHCaveDrawDrawingContext.TraceVers(const PT: TPoint2Df; const Drawn: boolean);
var
  PP: TPoint;
begin
  PP := QGetCoordsPlan(PT);
  if (Drawn) then self.CanvasBGRA.LineTo(PP.X, PP.Y)
             else self.CanvasBGRA.MoveTo(PP.X, PP.Y);

end;

procedure TGHCaveDrawDrawingContext.TraceVers(const XX, YY: Double; const Drawn: boolean);
var
  PM: TPoint2Df;
begin
  PM.setFrom(XX, YY);
  self.TraceVers(PM, Drawn);
end;


//******************************************************************************
// Le TCanvas de Lazarus possède un paramètre 'Orientation' (absent du TCanvas de Delphi 5)
// qui n'est rien d'autre que l'angle de rotation en 1/10 de degré
// 3 instructions toutes simples LOL !
procedure TGHCaveDrawDrawingContext.TracerRotatedTexte(const X, Y, Angle: integer; const Texte: string);
begin
  self.CanvasBGRA.Font.Orientation := Angle * 10; // angle en 1/10e de degré
  self.CanvasBGRA.TextOut(X, Y, Texte);
  self.CanvasBGRA.Font.Orientation := 0; // penser à remettre à l'horizontale XD
end;

procedure TGHCaveDrawDrawingContext.DrawShape(const x, y: Double; const TypeSymbole: byte; const L, H: integer; const BC, PC: TColor);
var
  PM: TPoint2Df;
  PP: TPoint;
  dl: Integer;
  dh: Integer;
begin
  PM.setFrom(X, Y);
  PP := QGetCoordsPlan(PM);
  dl := L shr 1;
  dh := H shr 1;
  self.CanvasBGRA.Brush.Texture := nil;
  self.CanvasBGRA.Brush.Style := bsSolid;
  self.CanvasBGRA.Brush.Color := BC;
  self.CanvasBGRA.Pen.Color   := PC;
  case TypeSymbole of
    0: self.CanvasBGRA.Rectangle(PP.X-dl,PP.Y-dh,PP.X+dh+1,PP.Y+dh+1);
    1: self.CanvasBGRA.EllipseC(PP.X, PP.Y, dl+1, dh+1);
  end;
end;
//------------------------------------------------------------------------------
procedure TGHCaveDrawDrawingContext.DessinerScrap(const MyScrap: TScrap;
                                                  const IdxScrap: Int64;
                                                  const DoDrawSommets: boolean;
                                                  const QIdxGroupe: integer);
const
  C = 4;
var
  i, NB          : integer; // NB Points
  V              : TVertexPolygon;
  PM             : TPoint2Df;
  PTSV           : array of TPoint;
  CP             : TPoint;  // point central du polygone
  errCode        : integer;
  QStylePolygone : TStylePolygone;
  WU             : Boolean;
begin
  try
    if (not (tedSCRAPS in FParamsVue2D.ElementsDrawn)) then Exit;
    WU := (QIdxGroupe = -1) OR
          (QIdxGroupe = MyScrap.IDGroupe);
    if (not WU) then Exit;

    NB := High(MyScrap.Sommets);
    if (Nb < 1) then Exit;
    SetLength(PTSV, NB+1);
    self.CanvasBGRA.pen.Width := 0;
    if (FParamsVue2D.DoDrawScrapsMonochromes) then
    begin
      //AfficherMessageErreur('Tracé des scraps en monochrome');
      Self.CanvasBGRA.Pen.Color   := FParamsVue2D.ColorScrapMonochrome;
      Self.CanvasBGRA.Pen.Opacity := 128;
      Self.CanvasBGRA.Brush.Color   := Self.CanvasBGRA.Pen.Color;
      Self.CanvasBGRA.Brush.Opacity := Self.CanvasBGRA.Pen.Opacity;
    end
    else
    begin
      Self.CanvasBGRA.Pen.Color     := MyScrap.Couleur;
      Self.CanvasBGRA.Pen.Opacity   := MyScrap.Opacite;
      Self.CanvasBGRA.Brush.Color   := MyScrap.Couleur; //SCRAP_COLOR;
      Self.CanvasBGRA.Brush.Opacity := MyScrap.Opacite;
    end;

    Self.CanvasBGRA.Brush.Texture := nil;
    Self.CanvasBGRA.Brush.Style   := bsSolid;

    //Cnv.Brush.Style := bs;
    for i:=0 to NB do
    begin
      V := MyScrap.getVertex(i);
      FDocumentDessin.GetCoordsGCS(V.IDStation, MyScrap.IDGroupe, V.Offset, PM, errCode);
      if (ErrCode = -1) then Exit;
      PTSV[i] := QGetCoordsPlan(PM);
    end;
    if (DoDrawSommets) then // dessiner les sommets
    begin
      self.DefineBrosseEtCrayon(bsSolid, clAqua, 255, psSolid, 0, clblue, 255);
      for i:= 0 to High(PTSV) do
      begin
        self.CanvasBGRA.Rectangle(PTSV[i].X - C, PTSV[i].Y - C,
                                  PTSV[i].X + C, PTSV[i].Y + C);
      end;
      DefineBrosseEtCrayon(bsSolid, clAqua, 255, psSolid, 0, clGray, 255);
      self.DrawRectangle(MyScrap.BoundingBox.C1, MyScrap.BoundingBox.C2);
      self.TraceTexte(MyScrap.BoundingBox.C1.X, MyScrap.BoundingBox.C2.Y,
                     Format('[%d] %s', [IdxScrap, MyScrap.Nom]));
    end else
    begin
      self.CanvasBGRA.Polygon(PTSV);
    end;
    self.CanvasBGRA.Polyline(PTSV);
    SetLength(PTSV, 0);
    self.RestoreBrosseEtCrayon();
  except
    AfficherMessageErreur(Format('** Echec en dessin du scrap: %d ***', [IdxScrap]));
  end;
end;
//------------------------------------------------------------------------------
procedure TGHCaveDrawDrawingContext.DessinerCourbe(const MyCourbe: TCourbe;
                                                   const IdxCourbe: Int64;
                                                   const QIdxGroupe: integer);
begin
  if (QIdxGroupe = MyCourbe.IDGroupe) then
  begin
    if (MyCourbe.IDStyleCourbe = nocMUR_MACONNE) then  // les murs sont tracés en deux fois
    begin
      self.DefineCrayon(psSolid, 4, clMaroon, 255);
        DrawCourbe(MyCourbe, IdxCourbe, False, false, false);
      self.RestoreCrayon();
    end;
    if (MyCourbe.IDStyleCourbe = nocPAROI_FRACASSEE) then  // les parois fracassées sont tracées en deux fois
    begin
      self.DefineCrayon(psSolid, 5, clRed, 255);
        DrawCourbe(MyCourbe, IdxCourbe, False, false, false);
      self.RestoreCrayon();
    end;
    DrawCourbe(MyCourbe, IdxCourbe, False, false, True);
  end;
end;


//******************************************************************************
//------------------------------------------------------------------------------
procedure TGHCaveDrawDrawingContext.DefineCrayon(const qStyle: TPenStyle;
                                                 const qWidth: integer;
                                                 const qColor: TColor;
                                                 const qOpacity: byte);
begin

  FOldPenStyle    := self.CanvasBGRA.Pen.Style;
  FOldPenColor    := self.CanvasBGRA.Pen.Color;
  FOldPenOpacity  := self.CanvasBGRA.Pen.Opacity;
  FOldPenWidth    := self.CanvasBGRA.Pen.Width;

  self.CanvasBGRA.Pen.Style := qStyle;
  self.CanvasBGRA.Pen.Width := qWidth;
  self.CanvasBGRA.Pen.Color := qColor;
  self.CanvasBGRA.Pen.Opacity := qOpacity;
end;

procedure TGHCaveDrawDrawingContext.DefineBrosseEtCrayon(const qBrushStyle: TBrushStyle; const qBrushColor: TColor; const qBrushOpacity: byte;
                                                         const qPenStyle: TPenStyle; const qPenWidth: integer; const qPenColor: TColor; const qPenOpacity: byte);
begin
  DefineBrosse(qBrushStyle, qBrushColor, qBrushOpacity);
  DefineCrayon(qPenStyle, qPenWidth, qPenColor, qPenOpacity);
end;


procedure TGHCaveDrawDrawingContext.DefineFonte(const QFontName: string; const QFontColor: TColor; const QFontStyle: TFontStyles; const QFontHeight: integer);
begin
  // sauvegarde de l'ancienne fonte (utiliser RestoreFont() pour la rappeler)
  FOldFontName   := self.CanvasBGRA.Font.Name;
  FOldFontStyle  := self.CanvasBGRA.Font.Style;
  FOldFontColor  := self.CanvasBGRA.Font.Color;
  FOldFontHeight := self.CanvasBGRA.Font.Height;

  self.CanvasBGRA.Font.Name    := QFontName;
  self.CanvasBGRA.Font.Color   := QFontColor;
  self.CanvasBGRA.Font.Style   := QFontStyle;
  self.CanvasBGRA.Font.Height  := QFontHeight; // TS.FontHeight;

end;


procedure TGHCaveDrawDrawingContext.DefineBrosse(const qStyle: TBrushStyle;
                                                   const qColor: TColor;
                                                   const qOpacity: byte);
begin
  FOldBrushStyle    := self.CanvasBGRA.Brush.Style;
  FOldBrushColor    := self.CanvasBGRA.Brush.Color;
  FOldBrushOpacity  := self.CanvasBGRA.Brush.Opacity;
  self.CanvasBGRA.Brush.Style   := qStyle;
  self.CanvasBGRA.Brush.Color   := qColor;
  self.CanvasBGRA.Brush.Opacity := qOpacity;
end;


procedure TGHCaveDrawDrawingContext.RestoreBrosse();
begin
  self.CanvasBGRA.Brush.Style    := FOldBrushStyle;
  self.CanvasBGRA.Brush.Color    := FOldBrushColor;
  self.CanvasBGRA.Brush.Opacity  := FOldBrushOpacity;
  //self.CanvasBGRA.Brush.Texture  := FOldBrushTexture;
 end;

procedure TGHCaveDrawDrawingContext.RestoreFonte();
begin
  self.CanvasBGRA.Font.Name      := FOldFontName;
  self.CanvasBGRA.Font.Color     := FOldFontColor;
  self.CanvasBGRA.Font.Style     := FOldFontStyle;
  self.CanvasBGRA.Font.Height    := FOldFontHeight;
  self.CanvasBGRA.Font.Opacity   := 255;        // pour mémoire (non utilisé)
end;

procedure TGHCaveDrawDrawingContext.RestoreCrayon();
begin
  self.CanvasBGRA.Pen.Style      := FOldPenStyle;
  self.CanvasBGRA.Pen.Width      := FOldPenWidth;
  self.CanvasBGRA.Pen.Color      := FOldPenColor;
  self.CanvasBGRA.Pen.Opacity    := FOldPenOpacity;
end;

procedure TGHCaveDrawDrawingContext.RestoreBrosseEtCrayon();
begin
  RestoreBrosse();
  RestoreCrayon();
end;

//------------------------------------------------------------------------------
// une station est visible ?
function TGHCaveDrawDrawingContext.BasePointIsVisible(const BP: TBaseStation): boolean;
var
  R0: TRect2Df;
  P1, P2: TPoint2Df;
begin
  R0.setFrom(FRXMini, FRYMini, FRXMaxi, FRYMaxi);
  P1.setFrom(BP.PosExtr0.X, BP.PosExtr0.Y);
  P2.setFrom(BP.PosStation.X, BP.PosStation.Y);
  result := PointIsInRectangle(P1, R0) OR PointIsInRectangle(P1, R0);
end;
//------------------------------------------------------------------------------
procedure TGHCaveDrawDrawingContext.DessinerBaseStation(const BP: TBaseStation; const QFontHeight: integer);
  procedure DessinerVisee();
  const
    RD = 2;
  var
    BP_Shell: TColor;  // super bénéfice de deux sociétés françaises: La BP et la Shell
    EWE: TToporobotIDStation;
    ar: Extended;
    dp: double;
    dz: double;
    pl: double;
    DeltaZ: Extended;
    ar0: double;
    CL: TColor;
    P1: TPoint2Df;
  begin
    try
      {$IFDEF TIDBASEPOINT_AS_TEXT}
      BP_Shell := IIF(BP.IDStation[1] = '-', clSilver, BP.Couleur);
      {$ELSE}
      BP_Shell := IIF(BP.IDStation < 0 , clSilver, BP.Couleur);
      {$ENDIF TIDBASEPOINT_AS_TEXT}


      self.CanvasBGRA.Brush.Color := BP_Shell;
      DefineCrayon(psSolid, 0, BP_Shell, 255);
      self.TraceVers(BP.PosExtr0.X, BP.PosExtr0.Y, False);
      self.TraceVers(BP.PosStation.X, BP.PosStation.Y, True);
      DefineCrayon(psSolid, 0, clGray, 255);
      self.TraceVers(BP.PosPG.X, BP.PosPG.Y, False);
      self.TraceVers(BP.PosPD.X, BP.PosPD.Y, True);
      //if (FDoDisplayIDStations) then
      if (tedIDSTATIONS in FParamsVue2D.ElementsDrawn) then
      begin
        self.CanvasBGRA.Brush.Color   := clAqua;
        self.CanvasBGRA.Font.Color    := clRed;
        self.CanvasBGRA.Font.Height   := QFontHeight;

        {$IFDEF TIDBASEPOINT_AS_TEXT}
        if (BP.IDStation[1] <> '-') then Exit;
        TraceTexte(BP.PosStation.X + 0.25, BP.PosStation.Y + 0.25,BP.IDStation);
        {$ELSE}
        if (BP.IDStation < 0) then Exit; // zap des visées en antenne
        EWE := BP.GetToporobotIDStation();
        if (EWE.aStation >= 0) then TraceTexte(BP.PosStation.X + 0.25, BP.PosStation.Y + 0.25, BP.getToporobotIDStationWithIDTerrainPriority());
        {$ENDIF TIDBASEPOINT_AS_TEXT}

      end;

      dp := Hypot(BP.PosStation.X - BP.PosExtr0.X,
                  BP.PosStation.Y - BP.PosExtr0.Y);
      dz := BP.PosStation.Z - BP.PosExtr0.Z;
      pl := abs(radtodeg(arctan2(dz, dp)));

      if ((tedDISP_PENTES in FParamsVue2D.ElementsDrawn) and
          (pl > FParamsVue2D.PenteLimiteDisp) and
          {$IFDEF TIDBASEPOINT_AS_TEXT}
          (BP.IDStation <> '')) then
          {$ELSE}
          (BP.IDStation > 0)) then
          {$ENDIF TIDBASEPOINT_AS_TEXT}


      begin
        CL := GetColorByScale(pl, 0, 90, BP_Shell,
                              [BP_Shell,
                               clLime,
                               clOlive,
                               clGreen,
                               clBlue,
                               clFuchsia,
                               clPurple,
                               clRed,
                               clMaroon,
                               clBlack
                              ]);

        DefineCrayon(psSolid, 3, CL, 255);
        self.CanvasBGRA.Brush.Color   := clAqua;
        self.TraceVers(BP.PosExtr0.X, BP.PosExtr0.Y, False);
        self.TraceVers(BP.PosStation.X, BP.PosStation.Y, True);

        // flèche indiquant le point bas de la visée
        DeltaZ := BP.PosStation.Z - BP.PosExtr0.Z;
        ar     := arctan2(BP.PosStation.Y - BP.PosExtr0.Y,
                          BP.PosStation.X - BP.PosExtr0.X);
        if (DeltaZ < 0) then
        begin
          ar := pi + ar;
          P1.setFrom(BP.PosStation.X, BP.PosStation.Y);
        end
        else
        begin
          P1.setFrom(BP.PosExtr0.X, BP.PosExtr0.Y);
        end;
        DrawPointeDeFleche(P1, ar, 0.60);
      end;
    except
      ;
    end;
  end; //procedure DessinerVisee;
  procedure DessinerEntree;
  const
    RD = 10;
  begin
    DrawShape(BP.PosStation.X, BP.PosStation.Y, 1, RD, RD, BP.Couleur, clBlack);
  end; // procedure DessinerEntree;
begin
  if (Not BP.Enabled) then Exit;
  if (not BasePointIsVisible(BP)) then Exit;
  if (BP.TypeStation = 1) then DessinerEntree else DessinerVisee;
end;
//------------------------------------------------------------------------------
// définition de styles de courbe
procedure TGHCaveDrawDrawingContext.DefineCurveStyle(const ILS: TNatureObjetCourbe; out LS: TStyleCourbe);
begin
  LS := FDocumentDessin.GetStyleCourbe(ord(ILS));
  self.CanvasBGRA.Pen.Style  := LS.LineStyle; //8psSolid;
  self.CanvasBGRA.Pen.Color  := LS.LineColor;
  self.CanvasBGRA.Pen.Width  := LS.LineWidth;
  self.CanvasBGRA.Pen.Opacity := 255;
end;
// définition de styles de polygones
procedure TGHCaveDrawDrawingContext.DefinePolygonStyle(const IPS: TNatureObjetPolygone; out PS: TStylePolygone);
  procedure SetSolidRemplissage(const QBackColor: TColor);
  begin
    self.CanvasBGRA.Brush.Texture := nil;  // Vide l'éventuel bitmap (indispensable en Lazarus)
    self.CanvasBGRA.Brush.Style  := bsSolid;
    self.CanvasBGRA.Brush.Color  := QBackColor;
    self.CanvasBGRA.Brush.Opacity := 255;
  end;
  procedure SetRemplissage(const QBMP: TBGRABitmap; const QBackColor: TColor);
  begin
    self.CanvasBGRA.Pen.Style    := psClear;    // psclear = pas de ligne
    self.CanvasBGRA.Brush.Style  := bsClear; // en Lazarus
    self.CanvasBGRA.Brush.Color  := QBackColor;
    self.CanvasBGRA.Brush.Texture := QBMP;
  end;
begin
  PS := FDocumentDessin.GetStylePolygone(ord(IPS));
  self.CanvasBGRA.Pen.Width    := 0;
  self.CanvasBGRA.Pen.Color    := PS.LineColor;
  self.CanvasBGRA.Pen.Style    := psSolid;
  self.CanvasBGRA.Pen.Opacity  := 255;
  case IPS of
    nopARGILE           : SetRemplissage(FTexturesPolygones.bmpClay, clWhite);
    nopSABLE            : SetRemplissage(FTexturesPolygones.bmpSand, clWhite);
    nopEBOULIS          : SetRemplissage(FTexturesPolygones.bmpTextureEboulis, clWhite); //SetRemplissage(FbmpEboulis, clSilver);
    nopGALETS           : SetRemplissage(FTexturesPolygones.bmpGalets, clSilver);
    nopNEIGE            : SetRemplissage(FTexturesPolygones.bmpSnow, clAqua);
    nopARGILES_GALETS   : SetRemplissage(FTexturesPolygones.bmpClayGalets, clWhite);
    nopLAC              : SetRemplissage(FTexturesPolygones.bmpTextureFlotte, clAqua);
    //nopCHEMINS  : SetSolidRemplissage(PS.FillColor); // parcours aménagés
  else
    SetSolidRemplissage(PS.FillColor);
  end;
end;
// définition de styles de texte
procedure TGHCaveDrawDrawingContext.DefineTextStyle(const ITS: TNatureObjetTexte; out TS: TStyleTexte);
var
  HH: Integer;
begin
  TS := FDocumentDessin.GetStyleTexte(Ord(ITS));
  HH := CalcFontHeightFromHauteurEnMetres(TS.FontPrnHeight);
  self.CanvasBGRA.Brush.Texture:= nil;
  self.CanvasBGRA.Brush.Color  := clWhite;
  self.CanvasBGRA.Font.Name    := TS.FontName;
  self.CanvasBGRA.Font.Color   := TS.FontColor;
  self.CanvasBGRA.Font.Style   := TS.FontStyle;
  self.CanvasBGRA.Font.Height    := HH; // TS.FontHeight;
end;
procedure TGHCaveDrawDrawingContext.DefineLinesStyle(const ILS: TNatureObjetLigne; out LS: TStyleLigne);
begin
  //AfficherMessage(Format('Dans DefineLinesStyle: ILS = %d', [ord(ILS)]));
  LS := FDocumentDessin.GetStyleLigne(Ord(ILS));
  self.CanvasBGRA.Pen.Style  := psSolid;
  self.CanvasBGRA.Pen.Color  := LS.LineColor;
  self.CanvasBGRA.Pen.Width  := LS.LineWidth;
  self.CanvasBGRA.Pen.Opacity := 255;
end;

//------------------------------------------------------------------------------
// dessin d'une courbe
procedure TGHCaveDrawDrawingContext.DrawCourbe(const MyCourbe: TCourbe;
                                               const IdxCourbe: Int64;
                                               const DoDrawPtsCtrls: boolean;
                                               const DoDrawBasePoints: boolean;
                                               const DoUseTStyleCourbe: boolean);
var
  QErrCode: integer;
  i, NB: integer; // NB Points
  AC    : TArcCourbe;
  AB    : TBezierArc;
  QStyleCourbe: TStyleCourbe;
  procedure GetBezierArc(const A: TArcCourbe; out QResult: TBezierArc; out errCode: integer);
  begin
    errCode := 0;
    try
      FDocumentDessin.GetCoordsGCS(A.IDStationP1, MyCourbe.IDGroupe, A.OffsetP1, QResult.PT1, errCode);
      if (ErrCode = -1) then Exit;
      QResult.Tgt1.setFrom(A.TangP1.X, A.TangP1.Y);
      FDocumentDessin.GetCoordsGCS(A.IDStationP2, MyCourbe.IDGroupe, A.OffsetP2, QResult.PT2, errCode);
      if (ErrCode = -1) then Exit;
      QResult.Tgt2.setFrom(A.TangP2.X, A.TangP2.Y);
      QResult.Pas    :=  QStyleCourbe.LongBarbules; // longueur d'un segment de courbe = longueur des barbules

    except
      errCode := -1;
    end;
  end;
begin
  try
    Nb := MyCourbe.getNbArcs(); //NB := High(MyCourbe.Arcs) + 1;
    if (Nb < 1) then Exit;
    if (DoUseTStyleCourbe) then DefineCurveStyle(MyCourbe.IDStyleCourbe, QStyleCourbe);
    for i:= 0 to NB - 1 do
    begin
      AC := MyCourbe.getArc(i);  //Arcs[i];
      //SL := FDocumentDessin.GetStyleCourbe(Ord(MyCourbe.IDStyleCourbe));
      GetBezierArc(AC, AB, QErrCode);
      if (QErrCode = 0) then DrawBezierArc(AB, QStyleCourbe.Barbules, DoDrawPtsCtrls)
                        else AfficherMessageErreur(Format('** Erreur en dessin de la courbe #%d, arc: %d/%d **', [IdxCourbe, i, Nb]));
    end;
    if (DoDrawPtsCtrls) then
    begin
      Self.CanvasBGRA.Brush.Color := clYellow;
      Self.DrawRectangle(MyCourbe.BoundingBox.C1, MyCourbe.BoundingBox.C2);
      Self.TraceTexte(MyCourbe.BoundingBox.C1.X, MyCourbe.BoundingBox.C2.Y, IntToStr(IdxCourbe));
    end;
  except
    AfficherMessageErreur(Format('** Echec en dessin de la courbe: %d ***', [IdxCourbe]));
  end;
end;
//TGHCaveDrawDrawingContext
function TGHCaveDrawDrawingContext.CalcFontHeightFromHauteurEnMetres(const Hauteur: double): integer;
var
  c1 : TPoint3Df;
  PP1: TPOINT;
  PP2: TPOINT;
begin
  try
    c1 := FDocumentDessin.GetCoordsMini;
    PP1 := QGetCoordsPlan(c1.X, c1.Y);
    PP2 := QGetCoordsPlan(c1.X, c1.Y - Hauteur);
    Result := PP2.Y - PP1.Y;
  except
    Result := 6;
  end;
end;
//------------------------------------------------------------------------------
procedure TGHCaveDrawDrawingContext.DrawBezierArc(const B: TBezierArc;
                                                  const Barbule: TBarbule;
                                                  const DoDrawPtsCtrls: boolean);
var
  Nb, i : integer;
  PC1, PC2: TPoint2Df; // points de contrôle
  LC: double; // longueur des segments P1>PC1>PC2>P2
  Bezier: TArrayPoints2Df;
  QArcBezier: array[0..3] of TPoint;
  Ang, DeltaX, DeltaY, dx, dy: double;
  L_Barbule: double;
  toto: float;
begin
  PC1.setFrom(B.PT1.X + B.Tgt1.X, B.PT1.Y + B.Tgt1.Y);
  PC2.setFrom(B.PT2.X + B.Tgt2.X, B.PT2.Y + B.Tgt2.Y);
  // si pas de barbules, on trace une vraie courbe de Bezier native
  // et non une polyligne passant par les points calculés avec CalcBezierCurve
  // les calculs suivants étant inutiles
  if (Barbule = tbNONE) then
  begin
    QArcBezier[0] := QGetCoordsPlan(B.PT1);
    QArcBezier[1] := QGetCoordsPlan(PC1);
    QArcBezier[2] := QGetCoordsPlan(PC2);
    QArcBezier[3] := QGetCoordsPlan(B.PT2);
    self.CanvasBGRA.PolyBezier(QArcBezier);
  end
  else
  begin  // Barbule != tbNone
    // calcul du nb de subdivisions, basé sur la longueur PT1-PC1-PC2-PT2
    LC := Hypot(PC1.X - B.PT1.X, PC1.Y - B.PT1.Y) +
          Hypot(PC2.X - PC1.X, PC2.Y - PC1.Y) +
          Hypot(B.PT2.X - PC2.X, B.PT2.Y - PC2.Y);
    Nb := round(LC / B.Pas);
    if (Nb = 0) then Nb := 1;
    Bezier := CalcBezierCurve(B.PT1, PC1, PC2, B.PT2, Nb);
    // tracé de la courbe-support (sauf pour chenal de voute, )
    if (Barbule <> tbCHENAL_VOUTE) then
    begin
      TraceVers(Bezier[0].X, Bezier[0].Y, False);
      for i := 0 to High(Bezier) do TraceVers(Bezier[i].X, Bezier[i].Y, True);
    end;
    // tracé des barbules
    L_Barbule := B.Pas;
    self.CanvasBGRA.Pen.Width := 0;
    for i := 1 to High(Bezier) do
    begin
      DeltaY := Bezier[i].Y - Bezier[i-1].Y;
      DeltaX := Bezier[i].X - Bezier[i-1].X;
      Ang := ArcTan2(DeltaY, DeltaX) + PI_2;
      dx := L_Barbule * cos(Ang);
      dy := L_Barbule * sin(Ang);
      DeltaY := 0.5 * DeltaY + Bezier[i-1].Y;
      DeltaX := 0.5 * DeltaX + Bezier[i-1].X;
      case Barbule of
        tbRESSAUT, tbMINI_RESSAUT:  // _|_|_|_|
          begin
            TraceVers(DeltaX, DeltaY, False);
            TraceVers(DeltaX+dx, DeltaY+dy, True);
          end;
        tbSURPLOMB: // /\/\/\
          begin
            TraceVers(Bezier[i].X, Bezier[i].Y, False);
            TraceVers(DeltaX+dx, DeltaY+dy, True);
            TraceVers(Bezier[i-1].X, Bezier[i-1].Y, True);
          end;
        tbCHENAL_VOUTE: // _|_  _|_
          begin
            if ((i MOD 2) = 0) then
            begin
              TraceVers(Bezier[i-1].X, Bezier[i-1].Y, False);
              self.TraceVers(Bezier[i].X, Bezier[i].Y, True);
              self.TraceVers(DeltaX, DeltaY, False);
              self.TraceVers(DeltaX+dx, DeltaY+dy, True);
            end;
          end;
       end;
    end;
  end; // if Barbult := tbNone
  // Points de contrôle
  if (DoDrawPtsCtrls) then
  begin
    self.CanvasBGRA.Pen.Width := 0;
    self.CanvasBGRA.Pen.Color := clGray;
    self.TraceVers(B.PT1.X, B.PT1.Y, False);
    self.TraceVers(PC1.X, PC1.Y, True);
    self.TraceVers(PC2.X, PC2.Y, True);
    self.TraceVers(B.PT2.X, B.PT2.Y, True);
    self.CanvasBGRA.Brush.Color := clBlue;
    self.CanvasBGRA.Brush.Style := bsSolid;
    self.TraceCercle(B.PT1.X, B.PT1.Y, 8);
    self.TraceCercle(B.PT2.X, B.PT2.Y, 8);
    self.TraceCercle(PC1.X, PC1.Y, 4);
    self.TraceCercle(PC2.X, PC2.Y, 4);
  end;
end;
// dessin d'un texte de copyright en filigrane
procedure TGHCaveDrawDrawingContext.DrawCopyright(const QPosition: TPoint2Df; const StrCopyright: string);
var
  PP: TPoint;
  WU: Extended;
begin
  WU := 7.00; //FTailleEchelle / 20.00;
  self.CanvasBGRA.Brush.Color   := FParamsVue2D.BackGroundColor;
  self.CanvasBGRA.Brush.Opacity := 128;
  self.CanvasBGRA.Font.Height   := CalcFontHeightFromHauteurEnMetres(WU);
  self.CanvasBGRA.Font.Color    := clBlack;
  self.CanvasBGRA.Font.Opacity  := self.CanvasBGRA.Brush.Opacity;
  // pour amener le point de base en bas gauche du texte
  PP := QGetCoordsPlan(QPosition.X, QPosition.Y + WU);
  self.CanvasBGRA.TextOut(PP.X, PP.Y, StrCopyright);
end;

// dessin d'un polygone
procedure TGHCaveDrawDrawingContext.DessinerPolygone(const MyPolygon: TPolygone;
                                                     const IdxPolygone: Int64;
                                                     const DoDrawSommets: boolean;
                                                     const QIdxGroupe: integer);
const
  C = 4;
var
  i, NB: integer; // NB Points
  V    : TVertexPolygon;
  PM   : TPoint2Df;
  PTSV : array of TPoint;
  CP   : TPoint;  // point central du polygone
  errCode       : integer;
  QStylePolygone: TStylePolygone;
  WU: Boolean;
begin
  try
    WU := (QIdxGroupe = -1) OR (QIdxGroupe = MyPolygon.IDGroupe);
    if (not WU) then Exit;

    NB := High(MyPolygon.Sommets);
    if (Nb < 1) then Exit;
    SetLength(PTSV, NB+1);
    DefinePolygonStyle(MyPolygon.IDStylePolygone, QStylePolygone);
    for i:=0 to NB do
    begin
      V := MyPolygon.getVertex(i);
      FDocumentDessin.GetCoordsGCS(V.IDStation, MyPolygon.IDGroupe, V.Offset, PM, errCode);
      if (ErrCode = -1) then Exit;
      PTSV[i] := QGetCoordsPlan(PM);
    end;
    if (DoDrawSommets) then // dessiner les sommets
    begin
      self.CanvasBGRA.Pen.Style   := psSolid;
      self.CanvasBGRA.Brush.Color := clAqua;
      self.CanvasBGRA.Pen.Color := clBlue;
      for i:= 0 to High(PTSV) do
      begin
        self.CanvasBGRA.Rectangle(PTSV[i].X - C, PTSV[i].Y - C, PTSV[i].X + C, PTSV[i].Y + C);
      end;
      self.CanvasBGRA.Brush.Color := clAqua;
      self.CanvasBGRA.Pen.Color   := clGray;
      self.CanvasBGRA.Pen.Style   := psSolid;
      self.DrawRectangle(MyPolygon.BoundingBox.C1, MyPolygon.BoundingBox.C2);
      self.TraceTexte(MyPolygon.BoundingBox.C1.X, MyPolygon.BoundingBox.C2.Y, IntToStr(IdxPolygone));
    end else
    begin
      self.CanvasBGRA.Polygon(PTSV);
      // si le polygone est un parcours, dessiner le contour
      if (MyPolygon.IDStylePolygone = nopCHEMINS) then self.CanvasBGRA.Pen.Style := psSolid;
    end;
    self.CanvasBGRA.Polyline(PTSV);
    // dessin des gros blocs
    if (MyPolygon.IDStylePolygone = nopGROS_BLOC) then
    begin
      // calcul du point central
      CP := MakeTPoint(0, 0);
      Nb := High(PTSV);
      for i := 0 to NB do CP := MakeTPoint(PTSV[i].X + CP.X, PTSV[i].Y + CP.Y);
      // pas de MakeTPoint2Df(): modif de la même variable;
      CP.X := CP.X div (Nb+1);
      CP.Y := CP.Y div (Nb+1);
      self.CanvasBGRA.Pen.Style := psSolid;
      self.CanvasBGRA.Pen.Color := clBlack;
      for i := 0 to NB do
      begin
        self.CanvasBGRA.MoveTo(CP.X, CP.Y);
        self.CanvasBGRA.LineTo(PTSV[i].X , PTSV[i].Y);
      end;
      self.CanvasBGRA.MoveTo(PTSV[NB].X, PTSV[NB].Y);
      self.CanvasBGRA.LineTo(PTSV[0].X, PTSV[0].Y);
    end;
    SetLength(PTSV, 0);
  except
    AfficherMessageErreur(Format('** Echec en dessin du polygone: %d ***', [IdxPolygone]));
  end;
end;

procedure TGHCaveDrawDrawingContext.DrawPolyligne(const MyPolyLine: TPolyLigne; const IdxPolyligne: Int64;
                                                  const DoDrawSommets: boolean;
                                                  const QIdxGroupe: integer;
                                                  const DoUseTStyleCourbe: boolean);
const C = 4;
var
  i, NB: integer; // NB Points
  V: TVertexPolygon;
  P0, P1, PM: TPoint2Df;
  PTSV: array of TPoint;
  errCode: integer;
  Ang: Extended;
  QStyleCourbe: TStyleCourbe;
  Ldd: Extended;
  R  : double;
  ca, sa: double;
  Qx, Qy: Extended;
  qdx,qdy: Extended;
  V0: TVertexPolygon;
  V1: TVertexPolygon;
  DeltaY: Extended;
  DeltaX: Extended;
  hb: Extended;
  WU: Boolean;
begin
  try
    WU := (QIdxGroupe = -1) OR (QIdxGroupe = MyPolyLine.IDGroupe);
    if (not WU) then Exit;
    NB := High(MyPolyLine.Sommets);
    if (Nb < 1) then Exit;
    SetLength(PTSV, NB+1);
    for i := 0 to NB do
    begin
      V := MyPolyLine.getVertex(i);
      FDocumentDessin.GetCoordsGCS(V.IDStation, MyPolyLine.IDGroupe, V.Offset, PM, errCode);
      if (ErrCode = -1) then Exit;
      PTSV[i] := QGetCoordsPlan(PM);
    end;
    if (DoDrawSommets) then // dessiner les sommets
    begin
      DefineBrosseEtCrayon(bsSolid, clAqua, 255, psSolid, 0, clAqua, 255);
      for i:= 0 to High(PTSV) do
      begin
        self.CanvasBGRA.Rectangle(PTSV[i].X - C, PTSV[i].Y - C, PTSV[i].X + C, PTSV[i].Y + C);
      end;
      RestoreBrosseEtCrayon();
      DefineBrosseEtCrayon(bsSolid, clAqua, 255, psSolid, 0, clGray, 255);
        self.DrawRectangle(MyPolyLine.BoundingBox.C1, MyPolyLine.BoundingBox.C2);
        self.TraceTexte(MyPolyLine.BoundingBox.C1.X, MyPolyLine.BoundingBox.C2.Y,
                       IntToStr(IdxPolyligne));
      RestoreBrosseEtCrayon();
    end;
    if (DoUseTStyleCourbe) then DefineCurveStyle(MyPolyLine.IDStylePolyLine, QStyleCourbe);
    self.CanvasBGRA.Polyline(PTSV);
    if (MyPolyLine.Closed) then
    begin
      self.CanvasBGRA.Brush.Texture := nil;
      DefineBrosse(bsSolid, clSilver, 192);
        self.CanvasBGRA.Polygon(PTSV);
      RestoreBrosse();
    end;
    // barbules
    if (QStyleCourbe.Barbules <> tbNONE) then
    begin
      for i := 1 to NB do
      begin
        V0 := MyPolyLine.getVertex(i-1);
        V1 := MyPolyLine.getVertex(i);
        FDocumentDessin.GetCoordsGCS(V0.IDStation, MyPolyLine.IDGroupe, V0.Offset, P0, errCode);
        FDocumentDessin.GetCoordsGCS(V1.IDStation, MyPolyLine.IDGroupe, V1.Offset, P1, errCode);
        DeltaY := P1.Y - P0.Y;
        DeltaX := P1.X - P0.X;
        // longueur développée du segment, à laquelle on retire la valeur d'un pas
        // pour éviter toute barbule en dehors du segment
        R   := hypot(DeltaX, DeltaY) - QStyleCourbe.LongBarbules;
        Ang := ArcTan2(DeltaY, DeltaX);
        SinCos(Ang, sa, ca);
        hb  := QStyleCourbe.LongBarbules * 0.5;
        // utilisation des identités trigo: cos(a+90°) = -sin(a) et sin(a+90) = cos(a)
        qdx := QStyleCourbe.LongBarbules * (-sa);
        qdy := QStyleCourbe.LongBarbules *  ca;
        // dessin des barbules
        Ldd := 0.00;
        self.CanvasBGRA.Pen.Width := QStyleCourbe.LineWidth;
        self.CanvasBGRA.Pen.Color := QStyleCourbe.LineColor;
        case QStyleCourbe.Barbules of
          tbRESSAUT, tbMINI_RESSAUT:  // _|_|_|_|_;
            begin
              while (Ldd < R) do
              begin
                Ldd += QStyleCourbe.LongBarbules;
                Qx := P0.X + Ldd * ca;
                Qy := P0.Y + Ldd * sa;
                self.TraceVers(Qx, Qy, False);
                self.TraceVers(Qx + qdx, Qy + qdy, True);
              end;
            end;
          tbSURPLOMB:
            begin
              while (Ldd < R) do
              begin
                Ldd += QStyleCourbe.LongBarbules;
                Qx := P0.X + (Ldd - hb) * ca;
                Qy := P0.Y + (Ldd - hb) * sa;
                self.TraceVers(Qx, Qy, False);
                Qx := P0.X + (Ldd) * ca;
                Qy := P0.Y + (Ldd) * sa;
                self.TraceVers(Qx + qdx, Qy + qdy, True);
                Qx := P0.X + (Ldd + hb) * ca;
                Qy := P0.Y + (Ldd + hb) * sa;
                self.TraceVers(Qx, Qy, True);
              end;
            end;
          tbCHENAL_VOUTE: pass; // un chenal de voûte n'est jamais rectiligne --> non supporté
        end; //  case QStyleCourbe.Barbules of
      end;   //  for i := 1 to NB do
    end;     // if (QStyleCourbe.Barbules = tbNONE) then
    //----------------------------------------------------------------------------
    SetLength(PTSV, 0);
  except
    AfficherMessageErreur(Format('** Echec en dessin de la polyligne: %d ***', [IdxPolyligne]));
  end;
end;

procedure TGHCaveDrawDrawingContext.DessinerPolyLigne(const MyPolyligne: TPolyLigne; const IdxPolyligne: Int64; const QIdxGroupe: integer);
begin
  if (QIdxGroupe = MyPolyligne.IDGroupe) then
  begin
    if (MyPolyligne.IDStylePolyLine = nocMUR_MACONNE) then  // les murs sont tracés en deux fois
    begin
      self.DefineCrayon(psSolid, 4, clMaroon, 255);
        DrawPolyligne(MyPolyligne, IdxPolyligne, False, MyPolyligne.IDGroupe, false);
      self.RestoreCrayon();
    end;
    if (MyPolyligne.IDStylePolyLine = nocPAROI_FRACASSEE) then  // les parois fracassées sont tracées en deux fois
    begin
      self.DefineCrayon(psSolid, 6, clRed, 255);
        DrawPolyligne(MyPolyligne, IdxPolyligne, False, MyPolyligne.IDGroupe, false);
      self.RestoreCrayon();
    end;
    DrawPolyligne(MyPolyligne, IdxPolyligne, False, MyPolyligne.IDGroupe, True);
  end;
end;

//------------------------------------------------------------------------------
procedure TGHCaveDrawDrawingContext.DessinerTexte(const T: TTextObject;
                                                  const DoDrawTextExt: boolean;
                                                  const QIdxGroupe: Integer);
var
  BP: TBaseStation;
  GP: TGroupeEntites;
  PM, PM0: TPoint2Df;
  PP, PP0: TPoint;
  PosTXT: TPoint;
  ExTxt: TSize;
  S: string;
  QStyleTexte: TStyleTexte;
  WU: Boolean;
begin
    // textes de débogage
  if (not (tedDEBUG_TEXTS in FParamsVue2D.ElementsDrawn)) then
  begin
    if (T.IDStyleTexte = notDEBUG) then Exit;
  end;
  try
    DefineTextStyle(T.IDStyleTexte, QStyleTexte);
    if (Not FDocumentDessin.GetBasePointByIndex(T.IDBaseStation, BP)) then Exit;
    WU := (QIdxGroupe = -1) OR (QIdxGroupe = T.IDGroupe);
    if (not WU) then Exit;
    GP   := FDocumentDessin.GetGroupeByIDGroupe(T.IDGroupe);
    PM   := MakeTPoint2DfWithGroupeAndBaseStation(BP, GP, T.Offset.X,  T.Offset.Y);
    PP   := QGetCoordsPlan(PM);
    S    := InterpreterTexteAnnotation(T.Text, T.MaxLength, BP);
    S    := Utf8ToAnsi(S); // TODO: A revoir
    ExTxt :=  self.CanvasBGRA.TextExtent(S);
    if (T.IDStyleTexte = notLIEU_DIT) then
    begin
      // dessin de la ligne d'attache
      DefineCrayon(psSolid, 0, clBlue, 255);
      PM0 := MakeTPoint2DfWithGroupeAndBaseStation(BP, GP, 0.00,  0.00);
      PP0 := QGetCoordsPlan(PM0);
      self.CanvasBGRA.MoveTo(PP0);
      self.CanvasBGRA.LineTo(PP);
      PosTXT := MakeTPoint(PP.X, PP.Y - ExTxt.cy);
      DefineBrosse(bsSolid, clYellow, 255);
      self.CanvasBGRA.Pen.Color   := clAqua; // modification d'un seul paramètre de brosse ou crayon
      self.CanvasBGRA.Rectangle(PosTXT.X - 1, PosTXT.y - 1,
                                PosTXT.X + 1 + self.CanvasBGRA.TextExtent(s).cX,
                                PosTXT.Y + 1 + self.CanvasBGRA.TextExtent(s).cY,
                                True);
      self.CanvasBGRA.TextOut(PosTXT.X, PosTXT.Y, S);
    end
    else  // les autres types de textes
    begin
      // 7 8 9
      // 4 5 6
      // 1 2 3
      //   0
      case T.Alignment of
        0,1: PosTXT := MakeTPoint(PP.X, PP.Y - ExTxt.cy);
        2  : PosTXT := MakeTPoint(PP.X - ExTxt.cx div 2, PP.Y - ExTxt.cy);
        3  : PosTXT := MakeTPoint(PP.X - ExTxt.cx, PP.Y - ExTxt.cy);
        4  : PosTXT := MakeTPoint(PP.X, PP.Y - ExTxt.cy div 2);
        5  : PosTXT := MakeTPoint(PP.X - ExTxt.cx div 2, PP.Y - ExTxt.cy div 2);
        6  : PosTXT := MakeTPoint(PP.X - ExTxt.cx, PP.Y - ExTxt.cy div 2);
        7  : PosTXT := MakeTPoint(PP.X, PP.Y);
        8  : PosTXT := MakeTPoint(PP.X - ExTxt.cx div 2, PP.Y);
        9  : PosTXT := MakeTPoint(PP.X - ExTxt.cx, PP.Y);
      else
        PosTXT := MakeTPoint(PP.X, PP.Y - ExTxt.cy);
      end;
      if (DoDrawTextExt) then
      begin
        self.CanvasBGRA.Brush.Color := clAqua;     // modification d'un seul paramètre de brosse ou crayon
        self.CanvasBGRA.Pen.Color   := clBlue;     // modification d'un seul paramètre de brosse ou crayon
        self.CanvasBGRA.Rectangle(PosTXT.X, PosTXT.y,
                                  PosTXT.X + self.CanvasBGRA.TextExtent(s).cX,
                                  PosTXT.Y + self.CanvasBGRA.TextExtent(s).cY);
        self.CanvasBGRA.Rectangle(PP.X, PP.Y, PP.X + 2, PP.Y + 2); // débogage: Tracé du point de base
        self.CanvasBGRA.TextOut(PosTXT.X, PosTXT.Y, S);
      end else begin
        self.CanvasBGRA.TextOut(PosTXT.X, PosTXT.Y, S);
      end;
    end;

  except
     AfficherMessageErreur(Format('** Echec en dessin du texte: %s ***', [T.Text]));
  end;
end;
// dessin d'une entité ligne
procedure TGHCaveDrawDrawingContext.DessinerSimpleLigne(const SL: TSimpleLigne;
                                                    const DoDrawHands: boolean;
                                                    const QIdxGroupe: integer);
const
  RR          = 3;
var
  E1, E2, E3, E4: TPoint2Df;
  PP: TPoint;
  errCode: integer;
  QLineStyle: TStyleLigne;
  QAngle: Int64;
  P0: TPoint2Df;
  A1: Int64;
  dy, dx, R, A2, Ang: Extended;
  PTD: TPoint2Df;
  WU: Boolean;
begin
  try
    WU := (QIdxGroupe = -1) OR (QIdxGroupe = SL.IDGroupe);
    if (not WU) then Exit;
    DefineLinesStyle(SL.IDStyleLigne, QLineStyle);
    FDocumentDessin.GetCoordsGCS(SL.IDBaseStExt1, SL.IDGroupe, SL.OffsetExtr1, E1, errCode);
    if (ErrCode = -1) then Exit;
    FDocumentDessin.GetCoordsGCS(SL.IDBaseStExt2, SL.IDGroupe, SL.OffsetExtr2, E2, errCode);
    if (ErrCode = -1) then Exit;
    self.TraceVers(E1.X, E1.Y, False);
    self.TraceVers(E2.X, E2.Y, True);
    // si le style est une ligne de pente, dessiner une flèche simple
    // à l'extrémité 2
    case SL.IDStyleLigne of
      nolPENTE:
      begin
        Ang := ArcTan2(E2.Y - E1.Y, E2.X - E1.X) + PI;
        self.DrawPointeDeFleche(E2, Ang, 0.25);
      end;
      nolFRACTURE:
      begin
        // si la ligne est une fracture, écrire son orientation
        self.CanvasBGRA.Brush.Texture := nil;
        self.CanvasBGRA.Brush.Color := clWhite;
        self.CanvasBGRA.Font.Color  := clBlue;
        self.CanvasBGRA.Pen.Color   := clBlack;
        self.CanvasBGRA.Pen.Width   := 2;
        dy := E2.Y - E1.Y; dx := E2.X - E1.X;
        if (dx < 0) then begin dx := -dx; dy := -dy; PTD := E2; end
                    else begin PTD := E1;                       end;
        Ang := ArcTan2(dy, dx);
        A1  := Trunc(Ang * INV_PI_180);
        QAngle := Trunc(90 - A1);
        if (QAngle < 0) then QAngle += 360;
        A2 := Ang - PI_2;
        R := 0.350;
        PP := QGetCoordsPlan(PTD.X + R * cos(A2), PTD.Y + R * sin(A2));
        self.CanvasBGRA.Font.Color  := clBlack;
        self.CanvasBGRA.Font.Height := CalcFontHeightFromHauteurEnMetres(2.0);
        self.TracerRotatedTexte(PP.X, PP.Y, A1, Format(FMT_DIRECTION_FAILLE,[QAngle]));
      end;
    else
      pass;
    end;
    if (DoDrawHands) then   // dessin des poignées
    begin
      self.CanvasBGRA.Brush.Color := clRed;     // modification d'un seul paramètre de brosse ou crayon
      self.CanvasBGRA.Pen.Color := clRed;       // modification d'un seul paramètre de brosse ou crayon
      PP := QGetCoordsPlan(E1);
      self.CanvasBGRA.EllipseC(PP.X, PP.Y, RR, RR);
      PP := QGetCoordsPlan(E2);
      self.CanvasBGRA.EllipseC(PP.X, PP.Y, RR, RR);
    end;
  except
    pass;
  end;
end;
//------------------------------------------------------------------------------
// Dessin d'un symbole
// dessin éléments ponctuels
procedure TGHCaveDrawDrawingContext.DessinerSymbole(const EP: TSymbole; const QIdxGroupe: integer);
var
  GP: TGroupeEntites;
  SD: TStyleSymboles;
  procedure DrawPhoto();
  var
    L, H: double;
    s          : string;
    S0         : TPoint2Df;
    Q0         : TPoint;
    RR         : TRect;
    C1, C2, P0 : TPoint2Df; // coins de la photo en coordonnées réelles
    Ratio      : double;
    BS         : TBaseStation;
    GP         : TGroupeEntites;
    errCode: integer;
  begin
    if (not (tedCADRES_PHOTO in FParamsVue2D.ElementsDrawn)) then Exit;
    DefineCrayon(psSolid, 0, clBlue, 255);                         // crayon
    P0 := GetOffsetCentrage(EP.UnTag, EP.ScaleX, EP.ScaleY);       // calcul de l'offset de centrage
    FDocumentDessin.GetCoordsGCSPonctObj(EP, S0, errCode);         // point de base
    if (errCode = -1) then Exit;
    Q0 := QGetCoordsPlan(S0);
    DefineBrosse(bsSolid, clSilver, 255);
    self.CanvasBGRA.Brush.Texture := nil; // textures sont modifiées directement
    self.CanvasBGRA.Rectangle(Q0.X - 4, Q0.Y - 4,
                                   Q0.X + 4, Q0.Y + 4);

    // coordonnées des coins de la photo
    C1.setFrom(P0.X + S0.X, P0.Y + S0.Y + EP.ScaleY);
    C2.setFrom(P0.X + S0.X + EP.ScaleX, P0.Y + S0.Y);
    // coordonnées écran de la photo
    RR.TopLeft     := self.QGetCoordsPlan(C1);
    RR.BottomRight := self.QGetCoordsPlan(C2);
    // station d'attache de la photo
    if (not FDocumentDessin.GetBasePointByIndex(EP.IDBaseStation, BS)) then Exit;
    GP := FDocumentDessin.GetGroupeByIDGroupe(EP.IDGroupe);
    DefineCrayon(psSolid, 0, clGray, 255);
    self.TraceVers(S0.X, S0.Y, False);
    self.TraceVers(BS.PosStation.X + GP.Decalage.X, BS.PosStation.Y + GP.Decalage.Y, True);
    // dessin de la zone photo
    self.CanvasBGRA.Rectangle(RR);
    if (EP.PhotoDisplayed) then
    begin
      RR.TopLeft.X := RR.TopLeft.X + 4;
      RR.TopLeft.Y := RR.TopLeft.Y + 4;
      RR.BottomRight.X := RR.BottomRight.X - 4;
      RR.BottomRight.Y := RR.BottomRight.Y - 4;
      self.CanvasBGRA.Pen.Color := clRed;
      // nom de la photo
      self.CanvasBGRA.Brush.Color := clWhite;   // modification d'un seul paramètre de brosse ou crayon
      self.CanvasBGRA.Font.Color  := clBlue;    // modification d'un seul paramètre de brosse ou crayon
      self.CanvasBGRA.Font.Height   := 12;      // modification d'un seul paramètre de brosse ou crayon
      self.CanvasBGRA.TextOut(RR.TopLeft.X + 1, RR.TopLeft.Y + 1, EP.TagTexte);
      self.CanvasBGRA.Rectangle(RR);
    end;
  end;
  procedure DrawEntree();
  const PHI_2 = 15.0;
  var
    P1, P2, P3: TPoint2Df;
    A1, AAA   : double;
    errCode: integer;
  begin
    FDocumentDessin.GetCoordsGCSPonctObj(EP, P1, errCode);
    if (errCode = -1) then Exit;
    A1  := EP.AngleRot + PHI_2;
    AAA := A1 * PI_180;
    P2.setFrom(P1.X + EP.ScaleX * cos(AAA), P1.Y + EP.ScaleX * sin(AAA));
    A1  := EP.AngleRot - PHI_2;
    AAA := A1 * PI_180;
    P3.setFrom(P1.X + EP.ScaleX * cos(AAA), P1.Y + EP.ScaleX * sin(AAA));
    self.DrawTriangle(P1, P2, P3, clBlack, clLime, 255, 255, 0);
  end;
  procedure DrawPointTopo();
  var
    P0, P1, P2, P3: TPoint2Df;
    A1, A2    : double;
    BP: TBaseStation;
  begin
    A1   := EP.ScaleX * 0.866;
    A2   := EP.ScaleX * 0.500;
    if (FDocumentDessin.GetBasePointByIndex(EP.IDBaseStation, BP)) then
    begin
      P0.setFrom(BP.PosStation.X + GP.Decalage.X,  BP.PosStation.Y + GP.Decalage.Y);
      P1.setFrom(P0.X, P0.Y + EP.ScaleX);
      P2.setFrom(P0.X + A1, P0.Y - A2);
      P3.setFrom(P0.X - A1, P2.Y);
      self.DrawTriangle(P1, P2, P3, clBlack, clRed, 255, 192, 0);
    end;
  end;
  procedure DrawPointFixe();
  var
    P0, P1, P2: TPoint2Df;
    R: double;
    PT1, PT2: TPoint;
    BP: TBaseStation;
  begin
    if (FDocumentDessin.GetBasePointByIndex(EP.IDBaseStation, BP)) then
    begin
      P0.setFrom(BP.PosStation.X + GP.Decalage.X,  BP.PosStation.Y + GP.Decalage.Y);
      R    := EP.ScaleX * 0.50;
      P1.setFrom(P0.X - R, P0.Y - R);
      P2.setFrom(P0.X + R, P0.Y + R);
      self.CanvasBGRA.Pen.Color := clBlack;
      self.CanvasBGRA.Brush.Color := clBlue;
      self.DrawRectangle(P1, P2);
    end;
  end;
  // ligne de correspondance entre le dessin décalé d'un groupe et sa position initiale
  procedure DrawLigneCorrespondance();
  var
    P1, P2: TPoint2Df;
    ErrCode: integer;
  begin
    FDocumentDessin.GetCoordsGCSPonctObj(EP, P1, ErrCode);
    if (errCode = -1) then Exit;
    if (Not GP.DecalageActif) then Exit;
    P2.setFrom(P1.X - GP.Decalage.X, P1.Y - GP.Decalage.Y);
    DefineCrayon(psSolid, 0, clGray, 255);
    self.CanvasBGRA.brush.Color:= clBlue;
    self.TraceVers(P1.X, P1.Y, False);
    self.TraceVers(P2.X, P2.Y, True);
    self.TraceCercle(P1.X, P1.Y, 5);
    self.TraceCercle(P2.X, P2.Y, 5);
  end;
  procedure DrawFistuleuses();
  const
    nn = 10;
  var
    i: integer;
    P1, P2: TPoint2Df;
    Pas, x: double;
    Moy   : double;
    q     : double;
    errCode: integer;
  begin

    FDocumentDessin.GetCoordsGCSPonctObj(EP, P1, errCode);
    if (errCode = -1) then Exit;
    P1.X := P1.X - EP.ScaleX / 2;
    P1.Y := P1.Y - EP.ScaleY / 2;
    P2.setFrom(P1.X + EP.ScaleX, P1.Y + EP.ScaleY);
    Pas := (P2.X - P1.X) / nn;
    Moy := (P2.Y - P1.Y) /2;
    // ligne supérieure
    DefineCrayon(psSolid, 2, clBlack, 255);
    self.TraceVers(P1.X, P2.Y, False);
    self.TraceVers(P2.X, P2.Y, True);
    // fistuleuses
    DefineCrayon(psSolid, 1, clBlack, 255);
    for i := 1 to nn - 1 do begin
      X := P1.X + Pas * i;
      q := P2.Y - RandG(Moy, Moy * 0.3);
      self.TraceVers(X, P2.Y, False);
      self.TraceVers(X, Q, True);
    end;
  end;
  procedure DrawConcretionsZigZag(); //'Concrétions /\/\';
  var
    i: integer;
    P1, P2: TPoint2Df;
    ZZ, ZZZ : TArrayPoints2Df;
    L  : double;
    errCode: integer;
  begin
    DefineCrayon(psSolid, 0, clBlack, 255);
    //AfficherMessage('Dessin zig-zags');
    FDocumentDessin.GetCoordsGCSPonctObj(EP, P1, errCode);
    if (errCode = -1) then Exit;
    P1.X := P1.X - EP.ScaleX / 2;
    P1.Y := P1.Y - EP.ScaleY / 2;
    P2.setFrom(P1.X + EP.ScaleX, P1.Y + EP.ScaleY);
    SetLength(ZZ, 7);
    L := (P2.X - P1.X) / 6;
    for i:= 0 to High(ZZ) do
    begin
      // pas de MakeTPoint2Df(): lisibilité;
      ZZ[i].X := P1.X + L * i;
      ZZ[i].Y := IIF(((i mod 2)=0), P1.Y, P2.Y);
    end;
    ZZ := RotateSetPoints(EP.AngleRot, ZZ, P1);  // rotation
    self.TraceVers(ZZ[0].X, ZZ[0].Y, False);     // tracé du zig-zag
    for i:=1 to High(ZZ) do self.TraceVers(ZZ[i].X, ZZ[i].Y, True);
    // purge
    SetLength(ZZ, 0);
  end;
  // excentriques
  procedure DrawExcentriques();
  var
    P1, P2: TPoint2Df;
    q, h, m, l: double;
    errCode: integer;
  begin
    FDocumentDessin.GetCoordsGCSPonctObj(EP, P1, errCode);
    if (errCode = -1) then Exit;
    P1.X := P1.X - EP.ScaleX / 2;
    P1.Y := P1.Y - EP.ScaleY / 2;
    P2.setFrom(P1.X + EP.ScaleX, P1.Y + EP.ScaleY);
    // ligne de plafond
    DefineCrayon(psSolid, 2, clBlack, 255);
    Self.TraceVers(P1.X, P2.Y, False);
    self.TraceVers(P2.X, P2.Y, True);
    q := 0.5 * (P1.X + P2.X);
    h := 0.5 * (P1.Y + P2.Y);
    m := (P2.X - P1.X) / 3;
    l := (P2.Y - P1.Y) / 8;
    DefineCrayon(psSolid, 1, clBlack, 255);
    // hampe
    self.TraceVers(q, P2.Y, False);
    self.TraceVers(q, P1.Y, true);
    // excentrique
    self.TraceVers(P1.X + m, P2.Y - l, False);
    self.TraceVers(P1.X + m, h, True);
    self.TraceVers(P2.X - m, h, True);
    self.TraceVers(P2.X - m, P1.Y + l, True);
  end;
  // cristallisations
  procedure DrawCristallisation();
  var
    i     : integer;
    R, T  : double;
    P1, P2: TPoint2Df;
    errCode: integer;
  begin
    FDocumentDessin.GetCoordsGCSPonctObj(EP, P1, errCode);
    if (errCode = -1) then Exit;
    R := Hypot(EP.ScaleX, EP.ScaleY);
    DefineCrayon(psSolid, 1, clBlack, 255);
    for i := 0 to 7 do begin
      T := i * PI_4;
      P2.setFrom(P1.X + R * cos(T), P1.Y + R * sin(T));
      self.TraceVers(P1.X, P1.Y, False);
      self.TraceVers(P2.X, P2.Y, True);
    end;
    RestoreCrayon();
  end;
  // stalactites
  procedure DrawStalactite();
  var
    X1, S1: double;
    P0, P1, P2, P3: TPoint2Df;
    errCode: integer;
  begin
    DefineCrayon(psSolid, 1, clBlack, 255);
    FDocumentDessin.GetCoordsGCSPonctObj(EP, P0, errCode);
    if (errCode = -1) then Exit;
    X1   := P0.X; // réutilisé plus loin
    S1   := EP.ScaleX / 2;
    P1.setFrom(X1 - S1, P0.Y - EP.ScaleY / 2);
    P2.setFrom(P1.X + EP.ScaleX, P1.Y + EP.ScaleY);
    P3.setFrom(X1, P2.Y - S1);
    self.TraceVers(P1.X, P2.Y, False);
    self.TraceVers(P3.X, P3.Y, True);
    self.TraceVers(P2.X, P2.Y, True);
    self.TraceVers(P3.X, P3.Y, False);
    self.TraceVers(P3.X, P1.Y, True);
    RestoreCrayon();
  end;
  // colonnes
  procedure DrawColonne();
  var
    X1, S1: double;
    P0, P1, P2, P3, P4: TPoint2Df;
    errCode: integer;
  begin
    DefineCrayon(psSolid, 1, clBlack, 255);
    FDocumentDessin.GetCoordsGCSPonctObj(EP, P0, errCode);
    if (errCode = -1) then Exit;
    X1   := P0.X; // réutilisé plus loin
    S1   := EP.ScaleX / 2;
    P1.setFrom(X1 - S1, P0.Y - EP.ScaleY / 2);
    P2.setFrom(P1.X + EP.ScaleX, P1.Y + EP.ScaleY);
    P3.setFrom(X1, P2.Y - S1);
    P4.setFrom(X1, P1.Y + S1);
    self.TraceVers(P1.X, P2.Y, False);
    self.TraceVers(P3.X, P3.Y, True);
    self.TraceVers(P2.X, P2.Y, True);
    self.TraceVers(P1.X, P1.Y, False);
    self.TraceVers(P4.X, P4.Y, True);
    self.TraceVers(P2.X, P1.Y, True);
    self.TraceVers(P3.X, P3.Y, False);
    self.TraceVers(P4.X, P4.Y, True);
    RestoreCrayon();
  end;
  // stalacmites
  procedure DrawStalagmite();
  var
    X1, S1: double;
    P0, P1, P2, P3: TPoint2Df;
    errCode: integer;
  begin
    DefineCrayon(psSolid, 1, clBlack, 255);
    FDocumentDessin.GetCoordsGCSPonctObj(EP, P0, errCode);
    if (errCode = -1) then Exit;
    X1   := P0.X; // réutilisé plus loin
    S1   := EP.ScaleX / 2;
    P1.setFrom(X1 - S1, P0.Y - EP.ScaleY / 2);
    P2.setFrom(P1.X + EP.ScaleX, P1.Y + EP.ScaleY);
    P3.setFrom(X1, P1.Y + S1);
    self.TraceVers(P1.X, P1.Y, False);
    self.TraceVers(P3.X, P3.Y, True);
    self.TraceVers(P2.X, P1.Y, True);
    self.TraceVers(P3.X, P3.Y, False);
    self.TraceVers(P3.X, P2.Y, True);
    RestoreCrayon();
  end;
  // failles et fractures
  procedure DrawFracture();
  var
    P0, P1, PR: TPoint2Df;
    A: double;
    Angle, AngleTopo : integer;
    R, dx, dy: double;
    PP: TPoint;
    errCode: integer;
  begin
    DefineCrayon(psSolid, 2, clBlack, 255);
    FDocumentDessin.GetCoordsGCSPonctObj(EP, P0, errCode);
    if (errCode = -1) then Exit;
    P1.setFrom(P0.X + EP.ScaleX, P0.Y + EP.ScaleY);
    A := ArcTan2(EP.ScaleY, EP.ScaleX);
    Angle := Trunc(A * INV_PI_180);
    AngleTopo := 90 - Angle;
    if (AngleTopo<0) then AngleTopo := AngleTopo + 360;
    // ligne
    self.TraceVers(P0.X, P0.Y, False);
    self.TraceVers(P1.X, P1.Y, True);
    // texte
    self.CanvasBGRA.Brush.Color := clWhite;
    self.CanvasBGRA.Font.Color  := clBlack;
    self.CanvasBGRA.Font.Style  := [];
    self.CanvasBGRA.Font.Height   := CalcFontHeightFromHauteurEnMetres(2.0);
    A := A - PI_2;
    R := 0.50;
    P0.setFrom(P0.X + R * cos(A), P0.Y + R * sin(A));
    PP := self.QGetCoordsPlan(P0);
    self.TracerRotatedTexte(PP.X, PP.Y, Angle, Format(FMT_DIRECTION_FAILLE, [AngleTopo]));
    // reset angle de rotation des textes
    self.TracerRotatedTexte(0,0,0, '');
    RestoreCrayon();
  end;
  // cupules
  procedure DrawCupule();
  const
    N = 4;
  var
    PS   : TArrayPoints2Df;
    PTSV : array of TPoint;
    PR   : TPoint2Df;
    i: Integer;
    errCode: integer;
  begin
    DefineCrayon(psSolid, 1, clBlack, 255);
    SetLength(PS, N);
    SetLength(PTSV, N);
    FDocumentDessin.GetCoordsGCSPonctObj(EP, PR, errCode);
    if (errCode = -1) then Exit;
    PS[0].setFrom(PR.X + EP.ScaleX, PR.Y + 0.50 * EP.ScaleY);
    PS[1].setFrom(PR.X, PS[0].Y);
    PS[2].setFrom(PS[1].X, PR.Y - 0.50 * EP.ScaleY);
    PS[3].setFrom(PS[0].X, PS[2].Y);
    PS := RotateSetPoints(EP.AngleRot, PS, PR);
    for i := 0 to N - 1 do PTSV[i] := self.QGetCoordsPlan(PS[i]);
    Self.CanvasBGRA.PolyBezier(PTSV);
    SetLength(PS, 2);
    PS[0].setFrom(PR.X - 0.2 * EP.ScaleX, PR.Y);
    PS[1].setFrom(PR.X + 1.2 * EP.ScaleX, PR.Y);
    PS := RotateSetPoints(EP.AngleRot, PS, PR);
    Self.TraceVers(PS[0].X, PS[0].Y, False);
    Self.TraceVers(PS[1].X, PS[1].Y, True);
    RestoreCrayon();
  end;
  procedure DrawZeff();
  const
    LONG_FLECHE = 0.25;
  var
    E1, E2, E3: TPoint2Df;
    R: double;
    dx, dy: double;
    Ang: float;
    WU: ValReal;
    errCode: integer;
  begin
    FDocumentDessin.GetCoordsGCSPonctObj(EP, E1, errCode);
    if (errCode = -1) then Exit;
    DefineCrayon(psSolid, 1, clBlack, 255);
    try
      Ang   := ArcTan2(EP.ScaleX, EP.ScaleY); //
      R := Hypot(EP.ScaleX, EP.ScaleY);
      dx := R * cos(Ang);
      dy := R * sin(Ang);
    except
      Ang := 0.00;
      dx  := 1.00; dy := 0.00;
    end;
    self.DrawPointeDeFleche(E1, Ang, 0.25);
    // hampe
    self.TraceVers(E1.X, E1.Y, false);
    self.TraceVers(E1.X + dx, E1.Y + dy, True);
    // talon
    WU := Ang + PI / 4;
    self.TraceVers(E1.X + dx + LONG_FLECHE * cos(WU),
                        E1.Y + dy + LONG_FLECHE * sin(WU),
                        True);
    RestoreCrayon();
  end;
  procedure DrawSinusoide(const QBAsePoint: TPoint2Df; const QL, QH, QR: double; const AvecFleche: boolean; const BP: boolean);
  const
    F = 2;
    N = 8 * (F + 1);
  var
    PS   : TArrayPoints2Df;
    PTSV : array of TPoint;
    L14, L24, L34: Extended;
    H02, H12: Extended;
    i: Integer;
    No: Integer;
    WU1, WU2: Extended;
    E1, E2, E3: TPoint2Df;
    miou: float;
    QBP: TPoint2Df;
    PT666: TPoint;
  begin
    SetLength(PS, N);
    SetLength(PTSV, N);
    L14 :=  0.25 * QL;
    L24 :=  0.50 * QL;
    L34 :=  0.75 * QL;
    H02 := -0.50 * QH;
    H12 := -H02;
    // BP = True => point de base en queue sinon en pointe de flèche
    if (BP) then QBP.X := QBAsePoint.X
            else QBP.X := QBAsePoint.X - QL * (F + 1);
    QBP.Y := QBAsePoint.Y - QH * 0.50;
    for i := 0 to F do
    begin
      No := i * 8;
      if (i = F) then
      begin
        WU1 := QH/2;
        WU2 := WU1;
      end
      else
      begin
        WU1 := QH;
        WU2 := 0.00;
      end;
      PS[No].setFrom(QBP.X + i * QL, QBP.Y);
      PS[No+1].setFrom(PS[No].X + L14, PS[No].Y + WU2);
      PS[No+2].setFrom(PS[No].X + L14, PS[No].Y + WU1);
      PS[No+3].setFrom(PS[No].X + L24, PS[No].Y + WU1);
      PS[No+4].setFrom(PS[No+3].X, PS[No+3].Y);
      PS[No+5].setFrom(PS[No].X + L34, PS[No].Y + WU1);
      PS[No+6].setFrom(PS[No].X + L34, PS[No].Y + WU2);
      PS[No+7].setFrom(PS[No].X + QL, PS[No].Y + WU2);
    end;
    PS := RotateSetPoints(QR, PS, QBAsePoint);
    for i := 0 to N-1 do  PTSV[i] := QGetCoordsPlan(PS[i]);
    DefineCrayon(psSolid, 1, clBlack, 255);
      self.CanvasBGRA.PolyBezier(PTSV);
    RestoreCrayon();
    // pointe de la flèche
    if (AvecFleche) then self.DrawPointeDeFleche(PS[n-1], degtorad(QR)+ PI, 0.25);
  end;

  procedure DrawArriveeEau();
  var
    EWE: TPoint2Df;
    errCode: integer;
  begin
    FDocumentDessin.GetCoordsGCSPonctObj(EP, EWE, errCode);
    if (errCode = -1) then Exit;
    DrawSinusoide(EWE, EP.ScaleX, EP.ScaleY, EP.AngleRot, True, False);
  end;
  procedure DrawPerte();
  var
    EWE: TPoint2Df;
    errCode: integer;
  begin
    FDocumentDessin.GetCoordsGCSPonctObj(EP, EWE, errCode);
    if (errCode = -1) then Exit;
    DrawSinusoide(EWE, EP.ScaleX, EP.ScaleY, EP.AngleRot, True, True);
  end;
  procedure DrawDesob(); // desob = on dessine une massette
  const
    NB_VTX = 8;
    LM     = 2;
    HM     = 8;
    F1     = 3;
    D1     = LM + F1 shl 1;
    D2     = 4;
  var
    Ux, Uy : double;
    QP   : array [0 .. NB_VTX - 1] of TPoint2Df;
    PTSV : array [0 .. NB_VTX - 1] of TPoint;
    i: Integer;
    errCode: integer;
  begin
    // la massette est dessinée à 135 degrés
    // le point de base est la base du manche de la massette
    FDocumentDessin.GetCoordsGCSPonctObj(EP, QP[0], errCode);
    if (errCode = -1) then Exit;
    Ux := 0.10 * EP.ScaleX;
    Uy := 0.10 * EP.ScaleY;
    QP[1].setFrom(QP[0].X + Ux * LM, QP[0].Y + Uy * LM);
    QP[2].setFrom(QP[1].X - Ux * HM, QP[1].Y + Uy * HM);
    QP[3].setFrom(QP[2].X + Ux * F1, QP[2].Y + Uy * F1);
    QP[4].setFrom(QP[3].X - Ux * D2, QP[3].Y + Uy * D2);
    QP[5].setFrom(QP[4].X - Ux * D1, QP[4].Y - Uy * D1);
    QP[6].setFrom(QP[5].X + Ux * D2, QP[5].Y - Uy * D2);
    QP[7].setFrom(QP[6].X + Ux * F1, QP[6].Y + Uy * F1);

    for i:=0 to NB_VTX - 1 do PTSV[i] := QGetCoordsPlan(QP[i]);
    DefineBrosseEtCrayon(bsSolid, clGray, 255, psSolid, 1, clBlack, 255);
      self.CanvasBGRA.Polygon(PTSV);
    RestoreBrosseEtCrayon();
  end;
  procedure DrawDanger();
  var
    P0, P1, P2, P3: TPoint2Df;
    A1, A2    : double;
    BP: TBaseStation;
  begin
    A1   := EP.ScaleX * 0.866;
    A2   := EP.ScaleX * 0.500;
    if (FDocumentDessin.GetBasePointByIndex(EP.IDBaseStation, BP)) then
    begin
      P0.setFrom(BP.PosStation.X + GP.Decalage.X + EP.Offset.X,
                 BP.PosStation.Y + GP.Decalage.Y + EP.Offset.Y);
      P1.setFrom(P0.X, P0.Y + EP.ScaleX);
      P2.setFrom(P0.X + A1, P0.Y - A2);
      P3.setFrom(P0.X - A1, P2.Y);
      self.DrawTriangle(P1, P2, P3, clRed, clYellow, 255, 255, 3);
    end;
  end;
  procedure DrawPointRemarquable();
  var
    C0: TPoint2Df;
    BP: TBaseStation;
  begin
    if (FDocumentDessin.GetBasePointByIndex(EP.IDBaseStation, BP)) then
    begin
      C0.setFrom(BP.PosStation.X + GP.Decalage.X + EP.Offset.X,
                 BP.PosStation.Y + GP.Decalage.Y + EP.Offset.Y);
      DrawAsterisk(C0, 2.00, 0.4, clRed);
    end;
  end;
  procedure DrawGrotteEnSurface();
  const
    HALF_WIDTH  = 2.500;
    HALF_HEIGHT = 1.25 * HALF_WIDTH;

  var
    C0: TPoint2Df;
    BP: TBaseStation;
    ArrPoints: TArrayPoints2Df;
  begin
    if (FDocumentDessin.GetBasePointByIndex(EP.IDBaseStation, BP)) then
    begin
      C0.setFrom(BP.PosStation.X, BP.PosStation.Y);
      SetLength(ArrPoints, 6);
      ArrPoints[0].setFrom( HALF_WIDTH, -HALF_HEIGHT);
      ArrPoints[1].setFrom( HALF_WIDTH,  HALF_HEIGHT / 3);
      ArrPoints[2].setFrom( HALF_WIDTH / 3,  HALF_HEIGHT);
      ArrPoints[3].setFrom(-HALF_WIDTH / 3,  HALF_HEIGHT);
      ArrPoints[4].setFrom(-HALF_WIDTH,  HALF_HEIGHT / 3);
      ArrPoints[5].setFrom(-HALF_WIDTH, -HALF_HEIGHT);
      DrawCenteredPolylinePolygone(C0, ArrPoints, clBlack, clGray, 255, 192, 2, True);
    end;
  end;
  procedure DrawGouffreEnSurface();
  const
    HALF_WIDTH  = 2.50;
    TIERS_HEIGHT = HALF_WIDTH * 0.667;
  var
    C0, P1, P2, P3: TPoint2Df;
    BP: TBaseStation;
  begin
    if (FDocumentDessin.GetBasePointByIndex(EP.IDBaseStation, BP)) then
    begin
      C0.setFrom(BP.PosStation.X, BP.PosStation.Y);
      P1.setFrom(C0.X - HALF_WIDTH,  C0.Y + TIERS_HEIGHT);
      P2.setFrom(C0.X,  C0.Y - 2 * TIERS_HEIGHT);
      P3.setFrom(C0.X + HALF_WIDTH,  C0.Y + TIERS_HEIGHT);
      DrawTriangle(P1, P2, P3, clBlack, clBlack, 255, 192, 0);
    end;
  end;

begin
  if (QIdxGroupe = EP.IDGroupe) then
  begin
    //try
      //SD := FDocumentDessin.GetStyleSymbole(Ord(EP.TypeObject));
      //AfficherMessageErreur(Format('DrawSymbole: %d - Type: %d',[EP.IDBaseStation, Ord(EP.TypeObject)]));
      //GP := FDocumentDessin.GetGroupeByIDGroupe(EP.IDGroupe);
      //if (not GP.Displayed) then Exit;
      case EP.TypeObject of
        nosPHOTO                 : DrawPhoto(); // Photo
        nosENTREE                : DrawEntree(); // Entrée
        nosPOINT_TOPO            : DrawPointTopo(); // Point topo
        nosPOINT_FIXE            : DrawPointFixe(); // Point fixe
        nosCORRESPONDANCE        : DrawLigneCorrespondance();
        nosFISTULEUSE            : DrawFistuleuses();
        nosCONCRETION_PAROI      : DrawConcretionsZigZag(); //'Concrétions /\/\';
        nosEXCENTRIQUES          : DrawExcentriques();  // excentriques
        nosSTALACTITES           : DrawStalactite(); // Stalactite
        nosCOLONNES              : DrawColonne(); // Colonne
        nosSTALAGMITES           : DrawStalagmite(); // Stalagmite
        nosCRISTAUX              : DrawCristallisation();
        nosFRACTURES             : DrawFracture();
        nosCUPULES               : DrawCupule();
        nosZEFF                  : DrawZeff();
        nosARRIVEE_EAU           : DrawArriveeEau();
        nosPERTE                 : DrawPerte();
        nosDESOB                 : DrawDesob();
        nosDANGER                : DrawDanger();
        nosGROTTE_SURFACE        : DrawGrotteEnSurface();
        nosGOUFFRE_SURFACE       : DrawGouffreEnSurface();
        nosPOINT_REMARQUABLE     : DrawPointRemarquable();
      else
        pass;
      end;
    //except
    //  pass;
    //end;
  end;
end;
// dessin de l'échelle et du nord
// Signification de PC1, PC2, PPC1 à PPC5:
//  +----------------------------------------------------------- PC2 +
//  |                                                                |
//  |                                                                |
//  |                                                                |
//  |                                                                |
//  |                                                                |
//  |                                                                |
//  |                                                                |
//  |  PPC3                  (zone de dessin)                        |
//  |   |\                                                           |
//  |   | \                                                          |
//  |   |  \                                                         |
//  |   |   \                                                        |
//  |  PPC5---PPC4                                                   |
//  |   |                                                            |
//  |   |                                                            |
//  |   [====    ====    ]PPC2                                       |
//  |   [PPC1====    ====]                                           |
//  |                                                                |
//  +PC1-------------------------------------------------------------+
procedure TGHCaveDrawDrawingContext.DrawEchelleNord(const QImgWidth, QImgHeight: integer);
const
  QMARGE = 20;
var
  L, H: double;
  P1, P2: TPoint;
  PC1, PC2: TPoint2Df;
  PPC1, PPC2: TPoint2Df;
  PPC3, PPC4, PPC5: TPoint2Df;
  QH1, QH2, QL1: Extended;
  EWE1, EWE2: TPoint2Df;
  iii, QQ: Integer;
  HH: Integer;
  PPP: TPOINT;
  RC: types.TSize;
  WU: String;
begin
  AfficherMessage(Format('%s.DessinerEchelleNord(%.2f)', [self.ClassName, FParamsVue2D.TailleEchelle]));
  DefineCrayon(psSolid, 0, clBlack, 255);
  self.CanvasBGRA.Brush.Style := bsSolid;
  P1   := MakeTPoint(0, QImgHeight); //MakeTPoint(0, Vue.Height);
  P2   := MakeTPoint(QImgWidth, 0); //MakeTPoint(Vue.Width, 0);
  PC1  := QGetCoordsMonde(P1);
  PC2  := QGetCoordsMonde(P2);
  L    := PC2.X - PC1.X;
  H    := PC2.Y - PC1.Y;
  QH1  := H / 4;      // position du sommet de la flèche Nord
  QH2  := 0.70 * QH1; // hauteur flèche
  QL1  := L / 50;     // largeur flèche
  PPC1 := QGetCoordsMonde(MakeTPoint(0 + QMarge, QImgHeight - QMarge));
  PPC2.setFrom(PPC1.X + FParamsVue2D.TailleEchelle,
               PPC1.Y + FParamsVue2D.TailleEchelle / 10);
  self.DrawRectangleRempli(PPC1, PPC2, clBlack, clWhite, 192, 128, 0);
  for iii := 0 to 3 do
  begin
    QQ   := IIF(iii mod 2 = 0, 1, 0);
    EWE1.setFrom(PPC1.X + iii * FParamsVue2D.TailleEchelle / 4,
                 PPC1.Y + QQ  * FParamsVue2D.TailleEchelle / 20);
    EWE2.setFrom(PPC1.X + (iii + 1) * FParamsVue2D.TailleEchelle / 4,
                 PPC1.Y + (QQ + 1)  * FParamsVue2D.TailleEchelle / 20);
    self.DrawRectangleRempli(EWE1, EWE2, clBlack, clGray, 192, 128, 0);
  end;
  // flèche du Nord
  PPC3.setFrom(PPC1.X, PPC1.Y + QH1);
  PPC4.setFrom(PPC3.X + QL1, PPC3.Y - QH2);
  PPC5.setFrom(PPC1.X, PPC3.Y - QH2);
  self.TraceVers(PPC1.X, PPC1.Y, False);
  self.TraceVers(PPC3.X, PPC3.Y, True);
  self.DrawTriangle(PPC5, PPC4, PPC3, clBlack, clGray, 255, 192, 0);
  // texte de l'échelle
  HH := CalcFontHeightFromHauteurEnMetres(FParamsVue2D.TailleEchelle / 12);
  self.CanvasBGRA.Font.Height := HH;
  self.CanvasBGRA.Font.Style  := [];
  self.CanvasBGRA.Font.Color  := clBlack;
  self.CanvasBGRA.Brush.Color := clWhite;
  WU := Format('%.0f m', [FParamsVue2D.TailleEchelle]);
  RC := self.CanvasBGRA.TextExtent(WU);
  PPP := self.QGetCoordsPlan(PPC2);
  self.CanvasBGRA.TextOut(PPP.X - RC.cx, PPP.Y - RC.cy - 1, WU);

  RestoreCrayon();
end;

procedure TGHCaveDrawDrawingContext.DrawImage(const IMG: TImageObject);
var
  P1, P2: TPOINT;
  R: TRECT;
  R0: TRECT;
  FQTmpBuff: TBGRABitmap;
  WU: Boolean;
begin
  AfficherMessageErreur('DrawImage: ' + IMG.Description);
  WU := (IMG.IsReady) and (IMG.Displayed);
  if (not WU) then Exit;
  try
    P1  := QGetCoordsPlan(IMG.PositionCoinsImage.X1, IMG.PositionCoinsImage.Y1);
    P2  := QGetCoordsPlan(IMG.PositionCoinsImage.X2, IMG.PositionCoinsImage.Y2);
    R   := MakeTRect(P1.X, P1.Y, P2.X, P2.Y);
    R0  := MakeTRect(0, IMG.ConteneurImg.Height, IMG.ConteneurImg.Width, 0);
    self.CanvasBGRA.CopyRect(R, IMG.ConteneurImg, R0);
  except
    AfficherMessageErreur(Format('** Echec en dessin de l''image: %s ***', [IMG.SrcFilename]));
  end;
end;



procedure TGHCaveDrawDrawingContext.DrawQuadrillage();
  procedure DrawQuadrillageGrille(const QdrSpc: double; const QdrColor: TColor; const QDoDrawCoords: boolean);
  var
    P1, P2: TPoint;
    q,t, qy: integer;
    A,B, B0: double;
    S: string;
  begin
    DefineCrayon(psSolid, 0, QdrColor, 255);
    t := trunc(FRXMini / QdrSpc);
    A := QdrSpc * t;
    qy := self.CanvasBGRA.Height - 1 - self.CanvasBGRA.TextHeight('8');
    while (A < FRXMaxi) do
    begin
      P1 := self.QGetCoordsPlan(A, FRYMini);
      P2 := self.QGetCoordsPlan(A, FRYMaxi);
      self.CanvasBGRA.MoveTo(P1.X, P1.Y);
      self.CanvasBGRA.LineTo(P2.X, P2.Y);
      //-------- Coordonnées en rive
      if (QDoDrawCoords) then
      begin
        S:=Format('%.0f',[A]);
        q:=P1.X - self.CanvasBGRA.TextWidth(S) div 2;
        //self.CanvasBGRA.TextOut(q, 1, S);    // chiffres en haut
        self.CanvasBGRA.TextOut(q, qy, S);     // chiffres en bas
      end;
      A += QdrSpc;
    end;
    t := trunc(FRYMini / QdrSpc);
    A := QdrSpc * t;
    while (A < FRYMaxi) do
    begin
      P1 := self.QGetCoordsPlan(FRXMini, A);
      P2 := self.QGetCoordsPlan(FRXMaxi, A);
      self.CanvasBGRA.MoveTo(P1.X, P1.Y);
      self.CanvasBGRA.LineTo(P2.X, P2.Y);
      //-------- Coordonnées en rive
      if (QDoDrawCoords) then
      begin
        S:=Format('%.0f',[A]);
        q:=P1.Y - self.CanvasBGRA.TextHeight(S) div 2;
        self.CanvasBGRA.TextOut(2, q, S);
      end;
      //----------------------------
      A += QdrSpc;
    end;
  end;
  procedure DrawQuadrillageCroix(const CrossSpc, CrossSize: double; const CrossColor: TColor; const QDoDrawCoords: boolean);
  var
    P1, P2, P3, P4: TPoint;
    C1, C2, C3, C4: TPoint2Df;
    tx, ty: Int64;
    Ax, Ay: Extended;
    S: String;
    q: Integer;
  begin
    DefineCrayon(psSolid, 0, CrossColor, 255);
    tx := trunc(FRXMini / CrossSpc);
    ty := trunc(FRYMini / CrossSpc);
    Ax := CrossSpc * tx;
    Ay := CrossSpc * ty;
    //-------- Coordonnées en rive
    if (QDoDrawCoords) then
    begin
      self.CanvasBGRA.Font.Style  := [];
      //self.CanvasBGRA.Font.Height := CalcFontHeightFromHauteurEnMetres();
      while (Ax < FRXMaxi) do
      begin
        P1 := self.QGetCoordsPlan(Ax, FRYMini);
        P2 := self.QGetCoordsPlan(Ax, FRYMaxi);
        // amorces
        self.CanvasBGRA.MoveTo(P2.X, P2.Y);
        self.CanvasBGRA.LineTo(P2.X, P2.Y + self.CanvasBGRA.Font.Height + 20);
        // texte
        S  := Format('%.0f',[Ax]);
        q  := P1.X - self.CanvasBGRA.TextWidth(S) div 2;
        self.CanvasBGRA.TextOut(q, 1, S);
        Ax += CrossSpc;
      end;
      while (Ay < FRYMaxi) do
      begin
        P1 := self.QGetCoordsPlan(FRXMini, Ay);
        P2 := self.QGetCoordsPlan(FRXMaxi, Ay);
        S  := Format('%.0f',[Ay]);
        q  := P1.Y - self.CanvasBGRA.TextHeight(S) div 2;
        // amorces
        self.CanvasBGRA.MoveTo(P1.X, P1.y);
        self.CanvasBGRA.LineTo(P1.X + self.CanvasBGRA.TextWidth(S) + 20, P1.y);
        // texte
        self.CanvasBGRA.TextOut(2, q, S);
        Ay += CrossSpc;
      end;
    end;


    //        C4
    //        |
    //        |
    // C1-----|-----C2
    //        |
    //        |
    //        C3
    //FParametresGrilles.GrdCrossSize := 10;
    Ay := CrossSpc * ty;
    while (Ay < FRYMaxi) do
    begin
      Ax := CrossSpc * tx;
      while (Ax < FRXMaxi) do
      begin
        C1.setFrom(Ax - CrossSize, Ay);
        C2.setFrom(Ax + CrossSize, Ay);
        C3.setFrom(Ax, Ay - CrossSize);
        C4.setFrom(Ax, Ay + CrossSize);
        self.TraceVers(C1, false);
        self.TraceVers(C2, true);
        self.TraceVers(C3, false);
        self.TraceVers(C4, true);
        Ax += CrossSpc;
      end;
      Ay += CrossSpc;
    end;
  end;

begin
  self.CanvasBGRA.Brush.Color := FParamsVue2D.BackGroundColor;
  self.CanvasBGRA.Font.Name   := DEFAULT_FONT_NAME;
  self.CanvasBGRA.Font.Color  := clBlack;
  self.CanvasBGRA.Font.Height := 14;
  // vue courante
  case FParamsVue2D.MainGrid.TypeQuadrillage of
    tqGRID : DrawQuadrillageGrille(FParamsVue2D.SecGrid.Spacing, FParamsVue2D.SecGrid.Color, false);
    tqCROSS: DrawQuadrillageCroix (FParamsVue2D.SecGrid.Spacing, FParamsVue2D.SecGrid.CrossSize, FParamsVue2D.SecGrid.Color, false);
  end;
  case FParamsVue2D.MainGrid.TypeQuadrillage of
    tqGRID : DrawQuadrillageGrille(FParamsVue2D.MainGrid.Spacing, FParamsVue2D.MainGrid.Color, false);
    tqCROSS: DrawQuadrillageCroix (FParamsVue2D.MainGrid.Spacing, FParamsVue2D.MainGrid.CrossSize, FParamsVue2D.MainGrid.Color, false);
  end;
end;

end.
// TGHCaveDrawDrawingContext

