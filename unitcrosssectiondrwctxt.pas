unit unitCrossSectionDrwCtxt;

{$INCLUDE CompilationParameters.inc}

interface

uses
  GHCD_Types,
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GeneralFunctions,
  math, Types,
  Graphics,
  unitSectionsTransversales,
  BGRABitmap,
  Classes, SysUtils;

type
 { TCrossSectionDrawingContext }

 TCrossSectionDrawingContext = class(TBGRABitmap)
  private
    // pointeur sur la section transversale
    FMyCrossSection: TCrossSection;
    // limites du dessin
    FRXMini,
    FRXMaxi,
    FRYMini,
    FRYMaxi: double;
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
    //FOldBrushTexture: IBGRAScanner;


    // paramètres internes pour le calcul des coordonnées
    FRappScrReal    : double;
    FInvRappScrReal : double;

    FTexturesPolygones: TTexturesPolygonObject;
    FTexturesAreOK    : boolean;



    procedure DessinerUnePointeDeFleche(const BP: TPoint2Df; const AngleRotInRad: double; const LONG_FLECHE: double);
    procedure DrawTriangle(const P1, P2, P3: TPoint2Df; const Linecolor, FillColor: TColor);
    procedure DrawATexte(const P: TPoint2Df; const Alignment: byte; const OrientationInDegres: double; const Texte: string);
    function QGetCoordsPlan(const PM: TPoint2Df): TPoint; overload;
    function QGetCoordsPlan(const QX, QY: double): TPoint; overload;
    function QGetCoordsMonde(const PP: TPoint): TPoint2Df;


    function CalcFontHeightFromHauteurEnMetres(const H: double): integer;
    //procedure TracerRotatedTexte(const X, Y, Angle: integer; const Texte: string);

  public
    procedure SetCrossSection(const S: TCrossSection);

    procedure SetTexturesPolygones(const T: TTexturesPolygonObject; const TexturesOK: boolean);
    procedure SetBounds(const QX1, QY1, QX2, QY2: double);
    procedure BeginDrawing();
    procedure EndDrawing();

    procedure DefineCrayon(const qStyle: TPenStyle; const qWidth: integer; const qColor: TColor; const qOpacity: byte);
    procedure DefineBrosse(const qStyle: TBrushStyle; const qColor: TColor; const qOpacity: byte);
    procedure DefineFonte(const QFontName: string; const QFontColor: TColor; const QFontStyle: TFontStyles; const QFontHeight: integer);


    procedure TraceVers(const XX, YY: Double; const Drawn: boolean); overload;
    procedure TraceVers(const PT: TPoint2Df; const Drawn: boolean); overload;
    procedure TraceCercle(const XX, YY: Double; const R: integer);
    procedure TraceRectangle(const C1, C2: TPoint2Df);
    procedure TracePolygone(const MyPolygone: TCrossSectionPolygone; const DoDrawSommets: boolean; const DoUseStyles: boolean = true);
    procedure TraceTexte(const MyTexte: TCrossSectionTexte);
    procedure TraceCourbeBezier(const MyCourbe: TCrossSectionCourbe; const DoUseTStyleCourbe: boolean);
    procedure TraceSimpleLigne(const MySimpleLigne: TCrossSectionSimpleLigne);

    procedure RestoreCrayon();
    procedure RestoreFonte();
    procedure RestoreBrosse();

    procedure DefineCurveStyle(const ILS: TNatureObjetCourbe; out LS: TStyleCourbe);
    procedure DefinePolygonStyle(const IPS: TNatureObjetPolygone; out PS: TStylePolygone);
    procedure DefineLinesStyle(const ILS: TNatureObjetLigne; out LS: TStyleLigne);
    procedure DefineTextStyle(const ITS: TNatureObjetTexte; out TS: TStyleTexte);

end;

implementation

{ TCrossSectionDrawingContext }

function TCrossSectionDrawingContext.QGetCoordsPlan (const PM: TPoint2Df): TPoint;
begin
  Result.X:=Round((PM.X - FRXMini) * FRappScrReal);
  Result.Y:=Round((FRYMaxi-PM.Y) * FRappScrReal);
end;

procedure TCrossSectionDrawingContext.RestoreBrosse();
begin
  self.CanvasBGRA.Brush.Style    := FOldBrushStyle;
  self.CanvasBGRA.Brush.Color    := FOldBrushColor;
  self.CanvasBGRA.Brush.Opacity  := FOldBrushOpacity;
  //self.CanvasBGRA.Brush.Texture  := FOldBrushTexture;
 end;

procedure TCrossSectionDrawingContext.RestoreFonte();
begin
  self.CanvasBGRA.Font.Name      := FOldFontName;
  self.CanvasBGRA.Font.Color     := FOldFontColor;
  self.CanvasBGRA.Font.Style     := FOldFontStyle;
  self.CanvasBGRA.Font.Height    := FOldFontHeight;
  self.CanvasBGRA.Font.Opacity   := 255;        // pour mémoire (non utilisé)
end;

procedure TCrossSectionDrawingContext.RestoreCrayon();
begin
  self.CanvasBGRA.Pen.Style      := FOldPenStyle;
  self.CanvasBGRA.Pen.Width      := FOldPenWidth;
  self.CanvasBGRA.Pen.Color      := FOldPenColor;
  self.CanvasBGRA.Pen.Opacity    := FOldPenOpacity;
end;

function TCrossSectionDrawingContext.QGetCoordsPlan(const QX, QY: double): TPoint;
var
  P1: TPoint2Df;
begin
  P1.setFrom(QX, QY);
  Result := QGetCoordsPlan(P1);
end;

function TCrossSectionDrawingContext.QGetCoordsMonde(const PP: TPoint): TPoint2Df;
begin
  Result.setFrom( FInvRappScrReal * PP.X + FRXMini,
                 -FInvRappScrReal * PP.Y + FRYMaxi);
end;


procedure TCrossSectionDrawingContext.SetBounds(const QX1, QY1, QX2, QY2: double);
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

procedure TCrossSectionDrawingContext.SetCrossSection(const S: TCrossSection);
begin
  FMyCrossSection := S;
end;

procedure TCrossSectionDrawingContext.SetTexturesPolygones(const T: TTexturesPolygonObject; const TexturesOK: boolean);
begin
  FTexturesAreOK := false;
  try
    FTexturesPolygones := T;
    FTexturesAreOK     := TexturesOK;
  except
  end;
end;

//******************************************************************************
procedure TCrossSectionDrawingContext.DefineCrayon(const qStyle: TPenStyle;
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

procedure TCrossSectionDrawingContext.DefineFonte(const QFontName: string; const QFontColor: TColor; const QFontStyle: TFontStyles; const QFontHeight: integer);
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


procedure TCrossSectionDrawingContext.DefineBrosse(const qStyle: TBrushStyle;
                                                   const qColor: TColor;
                                                   const qOpacity: byte);
begin
  FOldBrushStyle    := self.CanvasBGRA.Brush.Style;
  FOldBrushColor    := self.CanvasBGRA.Brush.Color;
  FOldBrushOpacity  := self.CanvasBGRA.Brush.Opacity;
  //FOldBrushTexture  := self.CanvasBGRA.Brush.Texture;

  self.CanvasBGRA.Brush.Style   := qStyle;
  self.CanvasBGRA.Brush.Color   := qColor;
  self.CanvasBGRA.Brush.Opacity := qOpacity;
end;
(*
procedure TCrossSectionDrawingContext.TracerRotatedTexte(const X, Y, Angle: integer; const Texte: string);
begin
  self.CanvasBGRA.Font.Orientation := Angle * 10; // angle en 1/10e de degré
  self.CanvasBGRA.TextOut(X, Y, Texte);
  self.CanvasBGRA.Font.Orientation := 0; // penser à remettre à l'horizontale XD
end;
//*)
// dessin d'une pointe de flèche - AngleRot est en radian
procedure TCrossSectionDrawingContext.DessinerUnePointeDeFleche(const BP: TPoint2Df;
                                                               const AngleRotInRad: double;
                                                               const LONG_FLECHE: double);
const
  PHI_2       = 20 * PI/180;
  PPI         = 0;
var
  E1, E2, E3: TPoint2Df;
begin
  E1 := BP;
  E2.X  := E1.X + LONG_FLECHE * cos(AngleRotInRad - PHI_2);
  E2.Y  := E1.Y + LONG_FLECHE * sin(AngleRotInRad - PHI_2);
  E3.X  := E1.X + LONG_FLECHE * cos(AngleRotInRad + PHI_2);
  E3.Y  := E1.Y + LONG_FLECHE * sin(AngleRotInRad + PHI_2);
  // flèche
  DrawTriangle(E1, E2, E3, clBlack, clGray);
end;
procedure TCrossSectionDrawingContext.DrawTriangle(const P1, P2, P3: TPoint2Df; const Linecolor, FillColor: TColor);
var
  Sommets: array[0..2] of TPoint;
begin
  Sommets[0] := QGetCoordsPlan(P1);
  Sommets[1] := QGetCoordsPlan(P2);
  Sommets[2] := QGetCoordsPlan(P3);
  self.CanvasBGRA.Pen.Color    := Linecolor;
  self.CanvasBGRA.Brush.Texture:= nil;
  self.CanvasBGRA.Brush.Style  := bsSolid;
  self.CanvasBGRA.Brush.Color  := FillColor;
  self.CanvasBGRA.Polygon(Sommets);
  RestoreBrosse();
  RestoreCrayon();
end;



procedure TCrossSectionDrawingContext.TraceVers(const PT: TPoint2Df; const Drawn: boolean);
var
  PP: TPoint;
begin
  PP := QGetCoordsPlan(PT);
  if (Drawn) then self.CanvasBGRA.LineTo(PP.X, PP.Y)
             else self.CanvasBGRA.MoveTo(PP.X, PP.Y);

end;

procedure TCrossSectionDrawingContext.TraceVers(const XX, YY: Double; const Drawn: boolean);
begin
  self.TraceVers(XX, YY, Drawn);
end;
procedure TCrossSectionDrawingContext.TraceCercle(const XX, YY: Double; const R: integer);
var
  Q: Integer;
  PP: TPoint;
begin
  Q := R shr 1;
  PP := QGetCoordsPlan(XX, YY);
  Self.CanvasBGRA.EllipseC(PP.X, PP.Y, Q, Q);
end;

procedure TCrossSectionDrawingContext.TraceSimpleLigne(const MySimpleLigne: TCrossSectionSimpleLigne);
var
  QStyleLigne: TStyleLigne;
  Ang, A2: float;
  E1, E2, PC: TPoint2Df;
  dy, dx: Double;
  QAngle, A1: Int64;
  PP: TPoint;
  R: Extended;
begin
  self.DefineLinesStyle(MySimpleLigne.IDStyleLigne, QStyleLigne);
  E1 := MySimpleLigne.Extr1;
  E2 := MySimpleLigne.Extr2;
  PC.setFrom(0.50 * (E1.X + E2.X), 0.50 * (E1.Y + E2.Y));
  self.TraceVers(E1, false);
  self.TraceVers(E2, true);
  case MySimpleLigne.IDStyleLigne of
    nolPENTE:
      begin
        Ang := ArcTan2(E2.Y - E1.Y, E2.X - E1.X) + PI;
        self.DessinerUnePointeDeFleche(E2, Ang, 0.25);
      end;
    nolFRACTURE, nolPENDAGE:
      begin
        Ang := radtodeg(ArcTan2(E2.Y - E1.Y, E2.X - E1.X));
        self.CanvasBGRA.Brush.Texture := nil;
        self.CanvasBGRA.Font.Color  := clBlue;
        self.DrawATexte(PC, 5, Ang, Format('%.0f', [Ang]));
      end;
  else
    ;
  end;
  AfficherMessageErreur(Format('TraceSimpleLigne %d', [ord(MySimpleLigne.IDStyleLigne)]));
end;

procedure TCrossSectionDrawingContext.TraceCourbeBezier(const MyCourbe: TCrossSectionCourbe; const DoUseTStyleCourbe: boolean);
var
  n: integer;
  i: Integer;
  QStyleCourbe: TStyleCourbe;
  procedure QDrawBezierArc(const B: TCrossSectionArcCourbe);
  var
    QArcBezier: array[0..3] of TPoint;
    PC1: TPoint2Df;
    PC2: TPoint2Df;
  begin
    PC1.setFrom(B.CoordsP1.X + B.TangP1.X, B.CoordsP1.Y + B.TangP1.Y);
    PC2.setFrom(B.CoordsP2.X + B.TangP2.X, B.CoordsP2.Y + B.TangP2.Y);
    QArcBezier[0] := QGetCoordsPlan(B.CoordsP1);
    QArcBezier[1] := QGetCoordsPlan(PC1);
    QArcBezier[2] := QGetCoordsPlan(PC2);
    QArcBezier[3] := QGetCoordsPlan(B.CoordsP2);
    Self.CanvasBGRA.PolyBezier(QArcBezier);
  end;

begin
  if (DoUseTStyleCourbe) then DefineCurveStyle(MyCourbe.IDStyleCourbe, QStyleCourbe);
  n := 1 + High(MyCourbe.Arcs);
  if (n = 0) then Exit;
  for i := 0 to n - 1 do QDrawBezierArc(MyCourbe.Arcs[i]);
end;

procedure TCrossSectionDrawingContext.TraceRectangle(const C1, C2: TPoint2Df);
var
  PP1, PP2: TPoint;
begin
  PP1 := QGetCoordsPlan(C1);
  PP2 := QGetCoordsPlan(C2);
  self.CanvasBGRA.Rectangle(PP1.X, PP1.Y, PP2.X, PP2.Y);
end;

procedure TCrossSectionDrawingContext.TraceTexte(const MyTexte: TCrossSectionTexte);
var
  PM: TPoint2Df;
  PP: TPoint;
  QStyleTexte: TStyleTexte;
  ExTxt: TSize;
  PosTXT: TPoint;
begin
  DefineTextStyle(MyTexte.IDStyleTexte, QStyleTexte);
  PM.setFrom(MyTexte.PosX, MyTexte.PosY);
  DrawATexte(PM, MyTexte.Alignment, 0.00, MyTexte.Text);
end;

procedure TCrossSectionDrawingContext.DrawATexte(const P: TPoint2Df; const Alignment: byte; const OrientationInDegres: double; const Texte: string);
var
  PP, PosTXT: TPoint;
  ExTxt: TSize;
  QOldTextOrientation: Integer;
begin
  PP := QGetCoordsPlan(P);
  ExTxt :=  self.CanvasBGRA.TextExtent(Texte);
  case Alignment of
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
  QOldTextOrientation := self.CanvasBGRA.Font.Orientation;
  self.CanvasBGRA.Font.Orientation := round(10 * OrientationInDegres);
  self.CanvasBGRA.TextOut(PosTXT.X, PosTXT.Y, Texte);
  self.CanvasBGRA.Font.Orientation := QOldTextOrientation;
end;

procedure TCrossSectionDrawingContext.TracePolygone(const MyPolygone: TCrossSectionPolygone;
                                                    const DoDrawSommets: boolean;
                                                    const DoUseStyles: boolean = true);
const
  C = 4;
var
  i, NB: integer; // NB Points
  V: TPoint2Df;
  PM: TPoint2Df;
  PTSV: array of TPoint;
  CP  : TPoint;  // point central du polygone
  errCode: integer;
  WU: Boolean;
  QStylePolygone: TStylePolygone;
begin
  AfficherMessage('Trace polygone 001');
    if (DoUseStyles) then DefinePolygonStyle(MyPolygone.IDStylePolygone, QStylePolygone);
    NB := High(MyPolygone.Vertexes);
    if (Nb < 1) then Exit;
    SetLength(PTSV, NB+1);
    for i:=0 to NB do
    begin
      V := MyPolygone.Vertexes[i];
      PTSV[i] := QGetCoordsPlan(V);
    end;
    self.CanvasBGRA.Polygon(PTSV);
    self.CanvasBGRA.Polyline(PTSV);
    SetLength(PTSV, 0);
end;


//******************************************************************************
// définition de styles de polygones
procedure TCrossSectionDrawingContext.DefinePolygonStyle(const IPS: TNatureObjetPolygone; out PS: TStylePolygone);
  procedure SetSolidRemplissage(const QBackColor: TColor);
  begin
    self.CanvasBGRA.Brush.Texture := nil;  // Vide l'éventuel bitmap (indispensable en Lazarus)
    self.CanvasBGRA.Brush.Style  := bsSolid;
    self.CanvasBGRA.Brush.Color  := QBackColor;
    self.CanvasBGRA.Brush.Opacity := 255;
  end;
  procedure SetRemplissage(const QBMP: TBGRABitmap; const QBackColor: TColor);
  begin
    AfficherMessage('SetRemplissage');
    self.CanvasBGRA.Pen.Style    := psClear;    // psclear = pas de ligne
    self.CanvasBGRA.Brush.Style  := bsClear; // en Lazarus
    self.CanvasBGRA.Brush.Color  := QBackColor;
    self.CanvasBGRA.Brush.Texture := QBMP;
  end;
begin
  AfficherMessageErreur(Format('%s.DefinePolygonStyle: %d', [ClassName, ord(IPS)]));
  PS := FMyCrossSection.GetStylePolygone(ord(IPS));
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

// styles de courbes
procedure TCrossSectionDrawingContext.DefineCurveStyle(const ILS: TNatureObjetCourbe; out LS: TStyleCourbe);
begin
  LS := FMyCrossSection.GetStyleCourbe(ord(ILS));
  self.CanvasBGRA.Pen.Style  := LS.LineStyle; //8psSolid;
  self.CanvasBGRA.Pen.Color  := LS.LineColor;
  self.CanvasBGRA.Pen.Width  := LS.LineWidth;
  self.CanvasBGRA.Pen.Opacity := 255;
end;
// définition de styles de texte
procedure TCrossSectionDrawingContext.DefineTextStyle(const ITS: TNatureObjetTexte; out TS: TStyleTexte);
var
  HH: Integer;
begin
  TS := FMyCrossSection.GetStyleTexte(Ord(ITS));
  HH := CalcFontHeightFromHauteurEnMetres(TS.FontPrnHeight);
  self.CanvasBGRA.Brush.Texture:= nil;
  self.CanvasBGRA.Brush.Color  := clWhite;
  self.CanvasBGRA.Font.Name    := TS.FontName;
  self.CanvasBGRA.Font.Color   := TS.FontColor;
  self.CanvasBGRA.Font.Style   := TS.FontStyle;
  self.CanvasBGRA.Font.Height    := HH; // TS.FontHeight;
end;
procedure TCrossSectionDrawingContext.DefineLinesStyle(const ILS: TNatureObjetLigne; out LS: TStyleLigne);
begin
  //AfficherMessage(Format('Dans DefineLinesStyle: ILS = %d', [ord(ILS)]));
  LS := FMyCrossSection.GetStyleLigne(Ord(ILS));
  self.CanvasBGRA.Pen.Style  := psSolid;
  self.CanvasBGRA.Pen.Color  := LS.LineColor;
  self.CanvasBGRA.Pen.Width  := LS.LineWidth;
  self.CanvasBGRA.Pen.Opacity := 255;
end;



//******************************************************************************
procedure TCrossSectionDrawingContext.BeginDrawing();
begin
  DefineCrayon(psSolid, 0, clBlack, 255);
  DefineBrosse(bsSolid, clWhite, 255);
  DefineFonte('Arial', clBlack, [], 10);
end;

function TCrossSectionDrawingContext.CalcFontHeightFromHauteurEnMetres(const H: double): integer;
begin
  Result := 1 + Trunc((FRYMaxi - FRYMini) * 0.5);   // formule de calcul provisoire
end;

procedure TCrossSectionDrawingContext.EndDrawing();
begin
  ;
end;
//******************************************************************************


end.

