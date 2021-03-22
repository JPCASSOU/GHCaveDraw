unit relevement;

interface

uses
  math,
  ign,
  donnee,
  datum;

// calcule le relèvement pour aller du Depart à l'Arrivee en suivant une orthodromie (plus court chemin)
// e sa première excentricité
// toutes les unités d'angle sont en radian (entrée et sortie)
function GetRelevement(LatDepart, LongDepart, LatArrivee, LongArrivee, e : real) : real;

// calcul la distance entre deux points à la surface de la sphère de rayon r
function GetDistanceSphere(LatDepart, LongDepart, LatArrivee, LongArrivee, r : real) : real;

// calcule les limites en latitude et longitudes du cercle de Centre LatCentre, LongCentre
// et de rayon rCercle sur la sphère de rayon rSphere
procedure GetLimiteCercle(LatCentre, LongCentre, rCercle, rSphere : real; var LatMin, LatMax, LongMin, LongMax : real);

// calcule les limites en latitude et longitudes dans le systeme Datum
// du cercle circonscrit aux quatre coins (en wgs84)
procedure LimiteCercleCirconscrit(LatTopLeft, LongTopLeft,
  LatTopRight, LongTopRight, LatBottomLeft, LongBottomLeft, LatBottomRight,
  LongBottomRight: real; Datum : TDatum;
  var LatMin, LatMax, LongMin, LongMax : real);


implementation

//uses Math, ign, donnee;

type
  TVecteur = record
    X, Y, Z : real;
  end;

procedure Normalise(var V : TVecteur);
var
  N : real;
begin
  with V do
  begin
    N:=1/sqrt(sqr(X)+Sqr(Y)+sqr(Z));
    X:=N*X;
    Y:=N*Y;
    Z:=N*Z;
  end;
end;

function GetRelevement(LatDepart, LongDepart, LatArrivee, LongArrivee, e : real) : real;
var
  OD, OA, TanDA, Nord, Est : TVecteur;
  Beta, CNord, CEst : real;
begin
  alg0009(LongDepart, LatDepart, 0, 1, e, OD.X, OD.Y, OD.Z);
  alg0009(LongArrivee, LatArrivee, 0, 1, e, OA.X, OA.Y, OA.Z);
  Normalise(OD);
  Normalise(OA);
  Beta:=1/(OD.X*OA.X+OD.Y*OA.Y+OD.Z*OA.Z);
  with TanDA do
  begin
    X:=OA.X*Beta-OD.X;
    Y:=OA.Y*Beta-OD.Y;
    Z:=OA.Z*Beta-OD.Z;
  end;
  Normalise(TanDA);
  with Nord do
  begin
    X:=-cos(LongDepart)*sin(LatDepart);
    Y:=-sin(LongDepart)*sin(LatDepart);
    Z:=cos(LatDepart);
  end;
  with Est do
  begin
    X:=-sin(LongDepart);
    Y:=cos(LongDepart);
    Z:=0;
  end;
  with TanDA do
  begin
    CNord:=X*Nord.X+Y*Nord.Y+Z*Nord.Z;
    CEst:=X*Est.X+Y*Est.Y+Z*Est.Z;
  end;
  result:=Arctan2(CEst,CNord);
end;

function GetDistanceSphere(LatDepart, LongDepart, LatArrivee, LongArrivee, r : real) : real;
var
  XDepart, YDepart, ZDepart,
  XArrivee, YArrivee, ZArrivee,
  angle : real;
begin
  XDepart:=cos(LatDepart)*cos(LongDepart);
  YDepart:=cos(LatDepart)*sin(LongDepart);
  ZDepart:=sin(LatDepart);
  XArrivee:=cos(LatArrivee)*cos(LongArrivee);
  YArrivee:=cos(LatArrivee)*sin(LongArrivee);
  ZArrivee:=sin(LatArrivee);
  Angle:=arccos(XDepart*XArrivee+YDepart*YArrivee+ZDepart*ZArrivee);
  result:=Angle*r;
end;

procedure GetLimiteCercle(LatCentre, LongCentre, rCercle, rSphere : real; var LatMin, LatMax, LongMin, LongMax : real);
var
  DeltaLat, DeltaLong, XCentre, a, rapport : real;
begin
  DeltaLat:=rCercle/rSphere;
  LatMin:=LatCentre-DeltaLat;
  LatMax:=LatCentre+DeltaLat;
  if (LatMin<=-Pi/2) or (LatMax>=Pi/2) then
  begin // le cercle englobe un pôle (non testé)
    LongMin:=-Pi;
    LongMax:=Pi;
    if LatMin<=-Pi/2 then
      LatMin:=-Pi/2;
    if LatMax>=Pi/2 then
      LatMax:=Pi/2;
  end
  else begin  // le cercle n'englobe pas de pôle
    // on travaille sur une sphère normalisée et sur le méridien d'origine
    // on projète le cercle sur sur le plan de l'équateur
    // ça devient une ellipse
    XCentre:=cos(DeltaLat)*cos(LatCentre);
    a:=sin(DeltaLat);
    rapport:=1/sin(LatCentre);

    DeltaLong:=arcsin(a/(XCentre*Rapport));
    DeltaLong:=arctan(Rapport*tan(DeltaLong));

{    b:=a*sin(LatCentre);
    DeltaLong:=arcsin(a/(XCentre/b*a));
    DeltaLong:=arctan(a/b*tan(DeltaLong)); }
    LongMin:=LongCentre-DeltaLong;
    LongMax:=LongCentre+DeltaLong;
  end;
end;

procedure LimiteCercleCirconscrit(LatTopLeft, LongTopLeft,
  LatTopRight, LongTopRight, LatBottomLeft, LongBottomLeft, LatBottomRight,
  LongBottomRight: real; Datum : TDatum;
  var LatMin, LatMax, LongMin, LongMax : real);
var
  XTopLeft, YTopLeft, ZTopLeft,
  XTopRight, YTopRight, ZTopRight,
  XBottomLeft, YBottomLeft, ZBottomLeft,
  XBottomRight, YBottomRight, ZBottomRight : real;
  XCentre, YCentre, ZCentre,
  LongCentre, LatCentre, rayonMax, rayon, h : real;
begin
  // calcul du centre de la zone à quadriller
  alg0009(LongTopLeft, LatTopLeft, 0, aWGS84, eWGS84, XTopLeft, YTopLeft, ZTopLeft);
  alg0009(LongTopRight, LatTopRight, 0, aWGS84, eWGS84, XTopRight, YTopRight, ZTopRight);
  alg0009(LongBottomLeft, LatBottomLeft, 0, aWGS84, eWGS84, XBottomLeft, YBottomLeft, ZBottomLeft);
  alg0009(LongBottomRight, LatBottomRight, 0, aWGS84, eWGS84, XBottomRight, YBottomRight, ZBottomRight);
  XCentre:=(XTopLeft+XTopRight+XBottomLeft+XBottomRight)/4;
  YCentre:=(YTopLeft+YTopRight+YBottomLeft+YBottomRight)/4;
  ZCentre:=(ZTopLeft+ZTopRight+ZBottomLeft+ZBottomRight)/4;
  alg0012(XCentre, YCentre, ZCentre, aWGS84, eWGS84, LongCentre, LatCentre, h);

  // calcul de la taille de la zone à quadriller
  rayonMax:=GetDistanceSphere(LatCentre,LongCentre,LatTopLeft,LongTopLeft,(aWGS84+bWGS84)/2);
  rayon:=GetDistanceSphere(LatCentre,LongCentre,LatTopRight,LongTopRight,(aWGS84+bWGS84)/2);
  if rayonMax<rayon then
    rayonMax:=rayon;
  rayon:=GetDistanceSphere(LatCentre,LongCentre,LatBottomLeft,LongTopLeft,(aWGS84+bWGS84)/2);
  if rayonMax<rayon then
    rayonMax:=rayon;
  rayon:=GetDistanceSphere(LatCentre,LongCentre,LatTopRight,LongTopRight,(aWGS84+bWGS84)/2);
  if rayonMax<rayon then
    rayonMax:=rayon;

  // conversion de WGS84 vers le datum courant
  Datum.DepuisWGS84(LatCentre, LongCentre,LatCentre, LongCentre);

  GetLimiteCercle(LatCentre,LongCentre,rayonMax,(Datum.Ellipsoide.a+Datum.Ellipsoide.b)/2,
                  LatMin, LatMax, LongMin, LongMax);
end;

end.
