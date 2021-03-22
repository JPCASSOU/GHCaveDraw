unit projection;

interface

uses
  Classes, sysutils,
  ancetre, type_quadri, relevement, arrondir;

const

// liste des différents type de projection avec le nom correspondant
  tpLambertConique='Lambert Conique';
  tpLambertConiqueSecant='Lambert Conique Secant';
  tpLambertConiqueTangent='Lambert Conique Tangent';
  tpLambertConiqueOblique='Lambert Conique Oblique';

  tpTransverseMercatorBrut='Transverse Mercator Brut'; // Gauss-Krüger
  tpTransverseMercator='Transverse Mercator'; // cas Gauss-Krüger
  tpUTM='UTM Fuseau';
  tpMTM='MTM Fuseau';
  tpGaussLaborde='Gauss Laborde';
  tpGaussLabordeCourbure='Gauss Laborde Courbure';
  tpGaussLabordeEquatoriale='Gauss Laborde Equatoriale';
  tpGaussLabordeBitangente='Gauss Laborde Bitangente';

  tpGaussLabordeOblique='Gauss Laborde Oblique';
  tpGaussRosenmund='Gauss Rosenmund'; // Suisse
  tpHotineCentre='Hotine centre'; // la version de Hotine
  tpHotine='Hotine'; // la version de l'USGS, souvent appelee Mertacor Oblique
  tpHotine2='Hotine deux points'; // la version de l'USGS, souvent appelee Mertacor Oblique
  tpCylindreOblique='Cylindrique oblique';


  tpGaussColle='Gauss Cole'; // Italie?
  tpGaussSchreiber='Gauss Schreiber';
  tpStereographic='Stereographique';
  tpStereographicCourbure='Stereographique Courbure';
  tpStereographicEquatoriale='Stereographique Equatoriale';
  tpStereographicBitangente='Stereographique Bitangente';
  tpStereographicPolaire='Stereographique Polaire';
  tpNewZealandMapGrid='New Zealand Map Grid'; // Nouvelle Zelande
  tpBonne='Bonne';

  tpMercatorSphere='Mercator sur sphere';
  tpMercatorEllipse='Mercator sur ellipsoïde';
  tpProjLatLong='Projection Latitude/Longitude';

type

// projection simple générique, toute projection simple doit dériver de
// TProjection

  TProjection = class(TAncetre)
  protected
  public
    procedure VersGrille(Lat, Long : real; var X, Y : real); virtual; abstract;
    procedure VersLatLong(X, Y : real; var Lat, Long : real); virtual; abstract;
    // calcul le quadrillage suivant la projection actuelle en utilisant les limites
    // fournies en WGS84
    procedure CalculQuadrillage(var LesZones : array of TZoneQuadri;
                                LatTopLeft, LongTopLeft, LatTopRight, LongTopRight,
                                LatBottomLeft, LongBottomLeft, LatBottomRight, LongBottomRight,
                                Pas : real);
  end;



implementation

//uses  SysUtils,   relevement, arrondir;

{ TProjection }

procedure TProjection.CalculQuadrillage(var LesZones: array of TZoneQuadri;
  LatTopLeft, LongTopLeft, LatTopRight, LongTopRight, LatBottomLeft,
  LongBottomLeft, LatBottomRight, LongBottomRight, Pas: real);
var
  J, K : integer;
  HMin, HMax, VMin, VMax : integer;
  XMin, XMax, YMin, YMax : real;
  LatMin, LatMax, LongMin, LongMax : real;
  Lat, Long : real;
begin
  try
    Pas:=Arrondi125(Pas);
    LimiteCercleCirconscrit(LatTopLeft, LongTopLeft,
                   LatTopRight, LongTopRight, LatBottomLeft, LongBottomLeft,
                   LatBottomRight, LongBottomRight, Datum,
                   LatMin, LatMax, LongMin, LongMax);
    VersGrille(LatMin,LongMin,XMin,YMin);
    VersGrille(LatMax,LongMax,XMax,YMax);
    HMin:=MyTrunc(XMin/Pas);
    HMax:=MyTrunc(XMax/Pas)+1;
    VMin:=MyTrunc(YMin/Pas);
    VMax:=MyTrunc(YMax/Pas)+1;
    //méridiens
    SetLength(LesZones[0].LesMeridien,HMax-HMin+1);
    SetLength(LesZones[0].LesParallele,VMax-VMin+1);
    for K:=0 to HMax-HMin do
    begin
      SetLength(LesZones[0].LesMeridien[K].Lat,VMax-VMin+1);
      SetLength(LesZones[0].LesMeridien[K].Long,VMax-VMin+1);
      LesZones[0].LesMeridien[K].Nom:=Format('%g',[(HMin+K)*Pas/1000]);
      for J:=0 to VMax-VMin do
      begin
        VersLatLong((HMin+K)*Pas,(VMin+J)*Pas,Lat, Long);
        Datum.VersWGS84(Lat,Long,Lat,Long);
        LesZones[0].LesMeridien[K].Lat[J]:=Lat;
        LesZones[0].LesMeridien[K].Long[J]:=Long;
      end;
    end;
    // parallèles
    for J:=0 to VMax-VMin do
    begin
      SetLength(LesZones[0].Lesparallele[J].Lat,HMax-HMin+1);
      SetLength(LesZones[0].Lesparallele[J].Long,HMax-HMin+1);
      LesZones[0].Lesparallele[J].Nom:=Format('%g',[(VMin+J)*Pas/1000]);
      for K:=0 to HMax-HMin do
      begin
        VersLatLong((HMin+K)*Pas,(VMin+J)*Pas,Lat, Long);
        Datum.VersWGS84(Lat,Long,Lat,Long);
        LesZones[0].Lesparallele[J].Lat[K]:=Lat;
        LesZones[0].Lesparallele[J].Long[K]:=Long;
      end;
    end;
  except;  
  end;
end;

end.
