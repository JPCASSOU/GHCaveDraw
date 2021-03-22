unit RoutinesDeDessin;

{$mode delphi}

interface

uses
  Classes, SysUtils
  , BGRABitmap
  , BGRABitmapTypes
  , BGRACanvas
  , BGRAPhongTypes
  , BGRAGradients
  ;

Procedure DrawRectangle(const Cnv: TBGRACanvas;
                        const C1, C2: TPoint2Df);
Procedure DrawVolatileRectangle(const Cnv: TCanvas;
                                const C1, C2: TPoint2Df);

procedure DrawRectangleRempli(const Cnv: TBGRACanvas;
                              const C1, C2: TPoint2Df;
                              const Linecolor, FillColor : TColor);
procedure DrawTriangle(const Cnv: TBGRACanvas;
                       const P1, P2, P3: TPoint2Df;
                       const Linecolor, FillColor : TColor);
procedure DrawVolatileTriangle(const Cnv: TCanvas;
                               const P1, P2, P3: TPoint2Df);
procedure DrawSimpleLigne(const Cnv: TBGRACanvas;
                          const SL: TSimpleLigne;
                          const Index: integer;
                          const DoDrawHands: boolean);
Procedure DrawShape(const Cnv: TBGRACanvas;
                      const x, y: Double;
                      const TypeSymbole: byte; // 0: rectangle, 1: cercle, sinon, rectangle
                      const L, H: integer;
                      const BC, PC: TColor);

procedure TraceTexte(const Cnv: TBGRACanvas; const XX, YY: Double; const T: string);
procedure TraceVolatileTexte(const Cnv: TCanvas; const XX, YY: Double; const T: string);

procedure TraceCercle(const Cnv: TBGRACanvas; const XX, YY: Double; const R: integer);
procedure TraceMarqueurDirectionnel(const Cnv: TCanvas; const XX, YY: double; const Angle: double);
    //*)

implementation

end.

