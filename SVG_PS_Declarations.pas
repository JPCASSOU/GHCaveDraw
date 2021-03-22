unit SVG_PS_Declarations;
{$INCLUDE CompilationParameters.inc}

interface
uses
  Graphics;
// **********************************************
//  Types de données pour les outils graphiques
// **********************************************
// coordonnées

type TsvgpsPoint2Df = record
  X : double;
  Y : double;
end;
type TsvgpsArrayPts2Df = array of TsvgpsPoint2Df;

// Fontes pour PostScript
type TFontPSProperties = record
  Name  : string;
  Size  : integer;
  Height: integer;
  Color : TColor;
  Style : TFontStyles;
end;
type TPenPSProperties = record
  Name    : string;
  Color   : TColor;
  fWidth  : double;
  nWidth  : integer;
end;
type TBrushPSProperties = record
  Color   : TColor;
  Alpha   : integer;
end;

implementation

end.

