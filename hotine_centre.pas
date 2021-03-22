unit hotine_centre;

// cas de la projection Oblique Mercator - Hotine

// documentation de référence
//
// http://remotesensing.org/geotiff/proj_list/guid7.html

// note : il s'agit de la version d'origine qui a l'origine au centre de la
// projection comme dans la définition d'Hotine.
// Des fois aussi appelée Rectfied Skew Orthomorphic

interface

uses
  projection, cylindre_oblic;

type

  { THotineCentre }

  THotineCentre = class(TCylindreOblic)
  private
    // les champs suivants sont utilisés en interne pour les calculs
    F0, F2, F4, F6,
    A, B, D, F, G, H, uc, t0,
    Landa0, cosGamma0, sinGamma0 : real;
    old_e, old_a : real; // anciens paramètres de l'ellipsoïde, pour voir si il y a eu changement
    fReady : boolean; // indique si les champs précédents ont été calculés
    procedure Prepare;
  protected
    function GetNature : string; override;
  public
    constructor Create; override;
    procedure VersGrille(Lat, Long : real; var X, Y : real); override;
    procedure VersLatLong(X, Y : real; var Lat, Long : real); override;
  end;

implementation

uses
  SysUtils, Math,
  outils, ign;


constructor THotineCentre.Create;
begin
  fReady:=false;
  old_e:=1;
  old_a:=1;
  inherited Create;
end;

function THotineCentre.GetNature : string;
begin
  result:=tpHotineCentre;
end;

procedure THotineCentre.Prepare;
var
  e2, ep2, Gamma0 : real;
  sinPhiC, c2, c4, c6, c8 : real;
begin
  if not fReady or (old_e<>fDatum.Ellipsoide.e) or (old_a<>fDatum.Ellipsoide.a) then
  begin
    e2:=sqr(Datum.Ellipsoide.e);
    ep2:=e2/(1-e2);
    sinPhiC:=sin(fPhi0Real);

    c2:=e2/2+5/24*sqr(e2)+Intpower(e2,3)/12+13/360*IntPower(e2,4);
    c4:=7/48*sqr(e2)+29/240*IntPower(e2,3)+811/11520*IntPower(e2,4);
    c6:=7/120*IntPower(e2,3)+81/1120*IntPower(e2,4);
    c8:=4279/161280*IntPower(e2,4);
    F0:=2*(c2-2*c4+3*c6-4*c8);
    F2:=8*(c4-4*c6+10*c8);
    F4:=32*(c6-6*c8);
    F6:=128*c8;

    B:=sqrt(1+ep2*IntPower(cos(fPhi0Real),4));
    A:=Datum.Ellipsoide.a*B*fK0*sqrt(1-e2)/(1-e2*sqr(sinPhiC));
    t0:=tan(Pi/4-fPhi0Real/2)/Power((1-Datum.Ellipsoide.e*sinPhiC)/(1+Datum.Ellipsoide.e*sinPhiC),Datum.Ellipsoide.e/2);
    D:=B*sqrt(1-e2)/(cos(fPhi0Real)*sqrt(1-e2*sqr(sinPhiC)));
    F:=D+sqrt(sqr(D)-1)*sign(fPhi0Real);
    H:=F*Power(t0,B);
    G:=(F-1/F)*0.5;
    Gamma0:=arcsin(sin(fTheta0Real)/D);
    cosGamma0:=cos(Gamma0); sinGamma0:=sin(Gamma0);
    Landa0:=fLanda0Real-arcsin(G*tan(Gamma0))/B;
    uc:=A/B*arctan(sqrt(sqr(D)-1)/cos(fTheta0Real))*sign(fPhi0Real);

    fReady:=true;
    old_e:=Datum.Ellipsoide.e;
    old_a:=Datum.Ellipsoide.a;
  end;
end;


procedure THotineCentre.VersGrille(Lat, Long : real; var X, Y : real);
var
  u, v, sinPhi : real;
  t, Q, S, GrandT, GrandU, GrandV : real;
begin
  Prepare;

  sinPhi:=sin(Lat);

  t:=tan(Pi/4-Lat/2)/Power((1-Datum.Ellipsoide.e*sinPhi)/(1+Datum.Ellipsoide.e*sinPhi),Datum.Ellipsoide.e/2);
  Q:=H/power(t,B);
  S:=(Q-1/Q)/2;
  GrandT:=(Q+1/Q)/2;
  GrandV:=sin(B*(Long-Landa0));
  GrandU:=(-GrandV*cosGamma0+S*sinGamma0)/GrandT;
  v:=A*ln((1-GrandU)/(1+GrandU))/B*0.5;
  u:=A*arctan((S*cosGamma0+GrandV*sinGamma0)/cos(B*(Long-Landa0)))/B-uc;
//  u:=A*arctan((S*cosGamma0+GrandV*sinGamma0)/cos(B*(Long-Landa0)))/B;
  X:=v*cosGamma0+u*sinGamma0+fX0;
  Y:=u*cosGamma0-v*sinGamma0+fY0;

end;

procedure THotineCentre.VersLatLong(X, Y : real; var Lat, Long : real);
var
  Q, S, T, Khi, u, v : real;
  GrandU, GrandV, GrandT : real;
  cosKhi : real;
begin
  Prepare;
  v:=(X-fX0)*cosGamma0-(Y-fY0)*sinGamma0;
  u:=(Y-fY0)*cosGamma0+(X-fX0)*sinGamma0+uc;
  Q:=exp(-B*v/A);
  S:=(Q-1/Q)/2;
  GrandT:=(Q+1/Q)/2;
  GrandV:=sin(B*u/A);
  GrandU:=(GrandV*cosGamma0+S*sinGamma0)/GrandT;
  t:=power(H/sqrt((1+GrandU)/(1-GrandU)),1/B);
  Khi:=Pi/2-2*arctan(t);
  cosKhi:=cos(Khi);
  Lat:=Khi+sin(Khi)*cosKhi*(F0+F2*sqr(cosKhi)+F4*IntPower(cosKhi,4)+F6*IntPower(cosKhi,6));
  Long:=Landa0-arctan((S*cosGamma0-GrandV*sinGamma0)/cos(B*u/A))/B;
end;


end.
