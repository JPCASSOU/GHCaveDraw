unit laborde_oblic;

// cas de la projection Gauss-Laborde Oblique

// documentation de référence
// D'après le code C de D. Steinwand/EROS 4/94 lui-même dérivé du code GWBASIC
// de John P. Snyder

// Résultats vérifiés avec
//
// Traité des projections des cartes géographiques
// à l'usage  des cartographes et des géodésiens
// L. Driencourt et J. Laborde
// quatrième fascicule
// Théorie de la représentation conforme
// Emploi des projections rigoureusement conformes en géodésie
//
// Notice d'emploi de la projection Mercator Oblique
// Foiben-Taosarintanin' I Madagasikara (Institut National de Géodésie et de Cartographie)
// Solonavalona Andriamihaja

interface

uses
  projection, cylindre_oblic;

type

  TLabordeOblic = class(TCylindreOblic)
  private

    // les champs suivants sont utilisés en interne pour les calculs
    r : real; // rayon de la sphère de courbure
    phisl, alpha, fchi0, acon, bcon : real;
    old_e, old_a : real; // anciens paramètres de l'ellipsoïde, pour voir si il y a eu changement
    fReady : boolean; // indique si les champs précédents ont été calculés

    procedure Prepare;
  protected
    // real
    procedure SetParamReal(index : integer; Value : real); override;
    // Angle
    procedure SetParamAngle(index : integer; Value : string); override;
    function GetNature : string; override;
  public
    constructor Create; override;
    procedure VersGrille(Lat, Long : real; var X, Y : real); override;
    procedure VersLatLong(X, Y : real; var Lat, Long : real); override;
  end;

implementation

uses
  SysUtils,
  outils, ign, math;

constructor TLabordeOblic.Create;
begin
  fReady:=false;
  old_e:=1;
  old_a:=1;
  inherited Create;
end;

function TLabordeOblic.GetNature : string;
begin
  result:=tpGaussLabordeOblique;
end;

procedure TLabordeOblic.SetParamReal(index : integer; Value : real);
begin
  inherited;
  fReady:=false;
end;

procedure TLabordeOblic.SetParamAngle(index : integer; Value : string);
begin
  try
    inherited;
    fReady:=false;
  except
  end;
end;

procedure TLabordeOblic.Prepare;
var
  e2 : real;
  sph0 : real;
begin
  if not fReady or (old_e<>fDatum.Ellipsoide.e) or (old_a<>fDatum.Ellipsoide.a) then
  begin
    e2:=sqr(fDatum.Ellipsoide.e);
    sph0:=sin(fPhi0Real);

    r:=fDatum.Ellipsoide.a*sqrt(1-e2)/(1-e2*sqr(sph0))*fk0;
    phisl := arctan(tan(fphi0Real)*sqrt((1.0-e2)/(1.0-e2*sph0*sph0)));
    alpha:=sqrt(1.0+e2/(1.-e2)*power(cos(fphi0Real),4.0));
    fchi0 := sqrt((1.0+sph0)/(1.0-sph0)*power((1.0-fDatum.Ellipsoide.e*sph0)/(1.0+fDatum.Ellipsoide.e*sph0),fDatum.Ellipsoide.e));
    acon := (1.0 - cos(2.0*fTheta0Real))/12.0;
    bcon := sin(2.0*fTheta0Real)/12.0;

    old_e:=fDatum.Ellipsoide.e;
    old_a:=fDatum.Ellipsoide.a;
    fReady:=true;
  end;
end;

procedure TLabordeOblic.VersGrille(Lat, Long : real; var X, Y : real);
var
  sph, fchi, fac, phil, dl, bfac, x_ptt, cl, y_ptt, v3, v4 : real;
begin
  Prepare;

  sph := sin(Lat);
  fchi := sqrt((1.0+sph)/(1.0-sph)*power((1.0-fDatum.Ellipsoide.e*sph)/
                         (1.0+fDatum.Ellipsoide.e*sph),fDatum.Ellipsoide.e));
  fac := power((fchi/fchi0),alpha)*tan(pi/4.0+phisl/2.0);
  phil := 2.0*arctan(fac)-pi/2.0;
  dl := alpha*(Long-fLanda0Real);
  bfac := cos(phil)*sin(dl);

  // This is the Gauss-Schreiber Transverse Mercator for unit sphere x,y := X,Y

  x_ptt:=0.5*ln((1.0+bfac)/(1.0-bfac));
  cl := cos(dl);
  y_ptt := arctan(tan(phil)/cl) - phisl;
  if (cl < 0.0) then y := y+ pi;

// Third order complex rotation with false eastings and false northings

  v3 := x_ptt*(3.0*y_ptt*y_ptt-x_ptt*x_ptt);
  v4 := y_ptt*(y_ptt*y_ptt-3.0*x_ptt*x_ptt);
  x := fX0+r*(x_ptt+acon*v3+bcon*v4);
  y := fY0+r*(y_ptt-bcon*v3+acon*v4);

end;

procedure TLabordeOblic.VersLatLong(X, Y : real; var Lat, Long : real);
var
  xpr,ypr,v5,v6,v7,v8,v9,v10,v11 : real;
  dx,dy : real;
  coshx,sinhx,fac1,dphi : real;
  x_ptt,y_ptt,v3,v4,fac : real;
  phil,fchi,sph : real;
begin
  Prepare;

  xpr := (X-fX0)/r;
  ypr := (Y-fY0)/r;
  x_ptt := xpr;
  y_ptt := ypr;
  repeat
     v3 := x_ptt*(3.0*y_ptt*y_ptt-x_ptt*x_ptt);
     v4 := y_ptt*(y_ptt*y_ptt-3.0*x_ptt*x_ptt);
     v5 := 3.0*(y_ptt*y_ptt-x_ptt*x_ptt);
     v6 := 6.0*x_ptt*y_ptt;
     v7 := y_ptt-bcon*v3+acon*v4-ypr;
     v8 := x_ptt+acon*v3+bcon*v4-xpr;
     v9 := 1.0+acon*v5-bcon*v6;
     v10 := bcon*v5+acon*v6;
     v11 := v9*v9+v10*v10;
     dx := -(v8*v9-v7*v10)/v11;
     dy := -(v7*v9+v8*v10)/v11;
     x_ptt := x_ptt + dx;
     y_ptt := y_ptt + dy;
  until abs(dx)+abs(dy)<0.00001/r;

//   Here, x,y are Gauss-Schreiber Transverse Mercator coordinates
  fac := exp(x_ptt);
  coshx := (fac+1.0/fac)/2.0;
  sinhx := (fac-1.0/fac)/2.0;
  fac1 := sin(y_ptt+phisl)/coshx;
  phil := arctan(fac1/sqrt(abs(1.0-fac1*fac1)));
  Long := fLanda0Real+arctan(sinhx/cos(y_ptt+phisl))/alpha;
  fchi := fchi0*power((tan(pi/4.0+phil/2.0)/tan(pi/4.0+phisl/2.0)),(1.0/alpha));
  Lat := phil;
  repeat
     sph := sin(Lat);
     dphi := 2.0 * arctan(fchi*power((1.0+fDatum.Ellipsoide.e*sph)/
         (1.0-fDatum.Ellipsoide.e*sph),(fDatum.Ellipsoide.e/2.0)))-pi/2.0-Lat;
     Lat := Lat+ dphi;
  until abs(dphi) < 1.0e-15;
end;

end.

