unit type_quadri;

interface

type

  TPolySegment = record
    Lat, Long : array of real;
    Nom : string;
  end;

  TZoneQuadri = record
    LesMeridien, LesParallele : array of TPolySegment;
  end;

implementation

end.
 