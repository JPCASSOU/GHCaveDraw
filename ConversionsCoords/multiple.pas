unit multiple;

interface

uses
  Classes,
  ancetre, type_quadri;

const

// liste des différents type de projections multiples avec le nom correspondant
  tmMercatorMultiple='Transverse Mercator Multiple';

type

// projection générique à zones multiples, toute projection multiple doit
// dériver de TMultiple

  TMultiple = class(TAncetre)
  protected
  public
    procedure VersGrille(Lat, Long : real; var X, Y : real; var fuseau : integer); virtual; abstract;
    procedure VersLatLong(X, Y : real; fuseau : integer; var Lat, Long : real); virtual; abstract;
    function NbZoneQuadrillage(LatTopLeft, LongTopLeft, LatTopRight, LongTopRight,
                               LatBottomLeft, LongBottomLeft, LatBottomRight, LongBottomRight : real
                               ) : integer; virtual; abstract; // indique le nombre de zones couvertes par la demande
    procedure CalculQuadrillage(var LesZones : array of TZoneQuadri;
                                LatTopLeft, LongTopLeft, LatTopRight, LongTopRight,
                                LatBottomLeft, LongBottomLeft, LatBottomRight, LongBottomRight,
                                Pas : real); virtual; abstract;
    function NbLimiteZoneQuadrillage(LatTopLeft, LongTopLeft, LatTopRight, LongTopRight,
                                     LatBottomLeft, LongBottomLeft, LatBottomRight, LongBottomRight : real
                                     ) : integer; virtual; abstract; // indique le nombre de limites entre zones
    // calcul le quadrillage suivant la projection multiple actuelle en utilisant
    // les limites fournies en WGS84
    // à implanter pour chaque type de projection multiple 
    procedure CalculLimiteZone(var LesLimites : array of TPolySegment;
                                LatTopLeft, LongTopLeft, LatTopRight, LongTopRight,
                                LatBottomLeft, LongBottomLeft, LatBottomRight, LongBottomRight,
                                Pas : real); virtual; abstract;
  end;

implementation

uses
  arrondir;


{ TMultiple }

end.
