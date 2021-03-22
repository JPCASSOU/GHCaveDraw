unit ign_laborde;

interface

procedure alg0034_laborde(Landa, Phi, e, LandaC, PhiC, Theta0, n1, n2, Xs, Ys, c : real;
                          var X, Y : real);

implementation

uses
  ign, math;

procedure changement_de_repere(
    lambda0, phi0, theta0, lambda, phi : real;
    var new_lambda, new_phi : real );
var
  cosT,sinT,cosP,sinP,cosL,sinL : real;
  x1,y1,z1 : real;
  x,y,z : real;
begin
  cosT := cos(theta0);
  sinT := sin(-theta0);
  cosP := cos(phi0);
  sinP := sin(-phi0);
  cosL := cos(lambda0);
  sinL := sin(-lambda0);
  x1:=cos(lambda)*cos(phi);
  y1:=sin(lambda)*cos(phi);
  z1:=sin(phi);
  x := cosL*cosP*x1 - sinL*cosP*y1 - sinP*z1;
  y := (cosL*sinP*sinT+sinL*cosT)*x1 + (-sinL*sinP*sinT+cosL*cosT)*y1 + cosP*sinT*z1;
  z := (cosL*sinP*cosT-sinL*sinT)*x1 + (-sinL*sinP*cosT-cosL*sinT)*y1 + cosP*cosT*z1;
  new_phi := arcsin(z);
  new_lambda := arctan2(y,x);
end;

procedure changement_retour(
  lambda0, phi0, theta0, lambda, phi : real;
  var new_lambda, new_phi : real );
var
  cosT,sinT,cosP,sinP,cosL,sinL : real;
  x1,y1,z1 : real;
  x,y,z : real;
begin
  cosT := cos(theta0);
  sinT := sin(-theta0);
  cosP := cos(phi0);
  sinP := sin(-phi0);
  cosL := cos(lambda0);
  sinL := sin(-lambda0);
  x1:=cos(lambda)*cos(phi);
  y1:=sin(lambda)*cos(phi);
  z1:=sin(phi);

  x := cosL*cosP*x1 + (cosL*sinP*sinT+sinL*cosT)*y1 + (cosL*sinP*cosT-sinL*sinT)*z1;
  y := -sinL*cosP*x1 + (-sinL*sinP*sinT+cosL*cosT)*y1 + (-sinL*sinP*cosT-cosL*sinT)*z1;
  z := -sinP*x1 + cosP*sinT*y1 + cosP*cosT*z1;
  new_phi := arcsin(z);
  new_lambda := arctan2(y,x);
end;

procedure alg0034_laborde(Landa, Phi, e, LandaC, PhiC, Theta0, n1, n2, Xs, Ys, c : real;
                          var X, Y : real);
var
  LandaMaj, L_IsoS : real;
  Xi, Yi : extended;
  A, B : extended;
  X1, X2, X3, X4, Y1, Y2, Y3, Y4 : extended;
begin
// projection ellipsoïde -> sphère
  LandaMaj:=n1*(Landa-LandaC);
  L_IsoS:=c+n1*alg0001(Phi,e);
// changement de coordonnées sur la sphère
  changement_de_repere(0,PhiC,Theta0,
                       LandaMaj, L_IsoS,
                       LandaMaj, L_IsoS);
{  changement_retour(0,PhiC,0,
                    LandaMaj, L_IsoS,
                    LandaMaj, L_IsoS); }
// projection sphère cylindre
  Yi:=n2*alg0001(arcsin(sin(LandaMaj)/cosh(L_IsoS)),0);
  Xi:=n2*arctan(sinh(L_IsoS)/cos(LandaMaj));
// rotation dans le plan
  X1:=Xi*cos(Theta0)-Yi*sin(Theta0);
  Y1:=Xi*sin(Theta0)+Yi*cos(Theta0);
  Xi:=X1; Yi:=Y1;  
// "rotation" sur le plan
  A:=(1-n1*cos(2*Theta0))/(12*sqr(n2));
  B:=n1*sin(2*Theta0)/(12*sqr(n2));
  X1:=A*IntPower(Xi,3); X2:=-3*B*sqr(Xi)*Yi; X3:=-3*A*Xi*sqr(Yi); X4:=B*IntPower(Yi,3);
  Y1:=B/A*X1; Y2:=-A/B*X2; Y3:=B/A*X3; Y4:=-A/B*X4;
  Y:=Ys+Xi+X1+X2+X3+X4;
  X:=Xs+Yi+Y1+Y2+Y3+Y4;
{  Y:=Ys+Xi+A*intpower(Xi,3) - 3*B*sqr(Xi)*Yi - 3*A*Xi*sqr(Yi) + B*intPower(Yi,3);
  X:=Xs+Yi+B*intpower(Xi,3) + 3*A*sqr(Xi)*Yi - 3*B*Xi*sqr(Yi) - A*intPower(Yi,3); }
end;


end.
