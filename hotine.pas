unit hotine;

// cas de la projection Oblique Mercator - Hotine

// documentation de référence
//
// NOAA Manual NOS NGS 5
// State Plane Coordinate System of 1983
//
// http://www.ngs.noaa.gov/PUBS_LIB/ManualNOSNGS5.pdf

// note : les longitudes sont de signe opposé (positif vers l'ouest) dans le
// document de référence par rapport au reste du logiciel (positif vers l'est)
// d'où des changements de signe dans les équations

// note 2 : il s'agit de la version de l'USGS (par Snyder?) qui a l'origine à
// l'intersection de l'oblique avec l'équateur et non au centre de la projection
// comme dans la définition d'Hotine. En plus, il y a sans doute de légères
// différences dans les formules, Hotine utilisant des hyperboliques alors que
// l'USGS recourt à des exponentielles

interface

uses
  projection, cylindre_oblic;

type

  THotine = class(TCylindreOblic)
  private
    procedure Preparation(var F0, F2, F4, F6, B, C, D, Landa0, F, G, I : real);
  protected
    function GetNature : string; override;
  public
    procedure VersGrille(Lat, Long : real; var X, Y : real); override;
    procedure VersLatLong(X, Y : real; var Lat, Long : real); override;
  end;

implementation

uses
  SysUtils, Math,
  outils, ign;


function THotine.GetNature : string;
begin
  result:=tpHotine;
end;

procedure THotine.Preparation(var F0, F2, F4, F6, B, C, D, Landa0, F, G, I : real);
var
  e2, ep2 : real;
  c2, c4, c6, c8 : real;
  Wc, A, Qc, sinPhiC : real;
begin
  e2:=sqr(Datum.Ellipsoide.e);
  ep2:=e2/(1-e2);

  c2:=e2/2+5/24*sqr(e2)+Intpower(e2,3)/12+13/360*IntPower(e2,4);
  c4:=7/48*sqr(e2)+29/240*IntPower(e2,3)+811/11520*IntPower(e2,4);
  c6:=7/120*IntPower(e2,3)+81/1120*IntPower(e2,4);
  c8:=4279/161280*IntPower(e2,4);
  F0:=2*(c2-2*c4+3*c6-4*c8);
  F2:=8*(c4-4*c6+10*c8);
  F4:=32*(c6-6*c8);
  F6:=128*c8;

  B:=sqrt(1+ep2*IntPower(cos(fPhi0Real),4));
  Wc:=sqrt(1-e2*sqr(sin(fPhi0Real)));
  A:=Datum.Ellipsoide.a*B*sqrt(1-e2)/sqr(Wc);
  sinPhiC:=sin(fPhi0Real);
  Qc:=0.5*(ln((1+sinPhiC)/(1-sinPhiC))-
      Datum.Ellipsoide.e*ln((1+Datum.Ellipsoide.e*sinPhiC)/(1-Datum.Ellipsoide.e*sinPhiC)));
  C:=ArcCosh(B*sqrt(1-e2)/(Wc*cos(fPhi0Real)))-B*Qc;
  D:=fk0*A/B;
  F:=Datum.Ellipsoide.a*sin(fTheta0Real)*cos(fPhi0Real)/(A*Wc); // sin(Alpha0)
  G:=sqrt(1-sqr(F)); // cos(Alpha0) adaptation personnelle
  Landa0:=-fLanda0Real + arcsin(F*sinh(B*Qc+C)/G)/B;
  I:=fk0*A/Datum.Ellipsoide.a;
end;


procedure THotine.VersGrille(Lat, Long : real; var X, Y : real);
var
  F0, F2, F4, F6, B, C, D, Landa0, F, G, I : real;
  L, Q, J, K, u, v : real;
  sinPhi : real;
begin
  Preparation(F0, F2, F4, F6, B, C, D, Landa0, F, G, I);
  L:=(-Long-Landa0)*B;
  sinPhi:=sin(Lat);
  Q:=0.5*(ln((1+sinPhi)/(1-sinPhi))-
     Datum.Ellipsoide.e*ln((1+Datum.Ellipsoide.e*sinPhi)/(1-Datum.Ellipsoide.e*sinPhi)));
  J:=sinh(B*Q+C);
  K:=cosh(B*Q+C);

  u:=D*arctan((J*G-F*sin(L))/cos(L));
  v:=D/2*ln((K-F*J-G*sin(L))/(K+F*J+G*sin(L)));
  Y:=u*cos(fTheta0Real)-v*sin(fTheta0Real)+fY0;
  X:=u*sin(fTheta0Real)+v*cos(fTheta0Real)+fX0;
end;  

procedure THotine.VersLatLong(X, Y : real; var Lat, Long : real);
var
  F0, F2, F4, F6, B, C, D, Landa0, F, G, I : real;
  Q, R, S, T, Khi, u, v : real;
  cosKhi : real;
begin
  Preparation(F0, F2, F4, F6, B, C, D, Landa0, F, G, I);
  u:=(X-fX0)*sin(fTheta0Real)+(Y-fY0)*cos(fTheta0Real);
  v:=(X-fX0)*cos(fTheta0Real)-(Y-fY0)*sin(fTheta0Real);
  R:=sinh(v/D);
  S:=cosh(v/D);
  T:=sin(u/D);
  Q:=(0.5*ln((S-R*F+G*T)/(S+R*F-G*T))-C)/B;
  Khi:=2*arctan((exp(Q)-1)/(exp(Q)+1));
  cosKhi:=cos(Khi);
  Lat:=Khi+sin(Khi)*cosKhi*(F0+F2*sqr(cosKhi)+F4*IntPower(cosKhi,4)+F6*IntPower(cosKhi,6));
  Long:=-(Landa0-arctan((R*G+T*F)/cos(u/D))/B);
end;


end.
