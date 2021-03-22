unit AlgorithmesIGN;

// cette unité contient les différents algorithmes de l'ign pour les
// changement de systèmes de coordonnées
//
// les différents documents sont
//
// Projection cartographique conique conforme de Lambert
// Notes techniques NT/G 71
// http://professionnels.ign.fr/DISPLAY/000/526/701/5267019/NTG_71.pdf
//
// Changement de système géodésique
// Notes techniques NT/G 80
// http://professionnels.ign.fr/DISPLAY/000/526/702/5267024/NTG_80.pdf
//
// Projection cartographique Gauss-Laborde
// Notes techniques NT/G 73
// http://professionnels.ign.fr/DISPLAY/000/526/702/5267020/NTG_73.pdf
//
// Projection cartographique Mercator Transverse
// Notes techniques NT/G 76
// http://professionnels.ign.fr/DISPLAY/000/526/702/5267021/NTG_76.pdf
//
// Projection cartographique stéréographique oblique
// Notes techniques NT/G 78

interface

const
  Eps =1e-11; // critère de convergence

type

  TTableau5 = array[1..5] of double;

function ALG0001(const Phi, e : double) : double;

function ALG0002(const L, e : double) : double;

procedure ALG0003(const Landa, Phi, n, c, e, LandaC, Xs, Ys : double; var X, Y : double);

procedure ALG0004(const X, Y, n, c, e, LandaC, Xs, Ys : double; var Landa, Phi : double);

procedure ALG0009(const Landa, Phi, h, a, e : double; var X, Y, Z : double);

procedure ALG0012(const X, Y, Z, a, e : double; var Landa, Phi, h : double);

procedure ALG0013(const Tx, Ty, Tz, D, Rx, Ry, Rz, Ux, Uy, Uz : double; var Vx, Vy, Vz : double);

procedure ALG0013bis(const Tx, Ty, Tz, D, Rx, Ry, Rz, Ux, Uy, Uz : double; var Vx, Vy, Vz : double);

procedure ALG0019(const a, e, Phi0, k0, Y0 : double; var n, c, Ys : double);

function ALG0021(const Phi, a, e : double) : double;

function ALG0028(const ee : double) : TTableau5;

procedure ALG0030(const LandaC, n, Xs, Ys, e, Landa, Phi : double; var X, Y : double);

procedure ALG0031(const LandaC, n, Xs, Ys, e, X, Y : double; var Landa, Phi : double);

procedure ALG0034(const Landa, Phi, e, LandaC, n1, n2, Xs, Ys, c : double; var X, Y : double);

procedure ALG0035(const X, Y, e, LandaC, n1, n2, Xs, Ys, c : double; var Landa, Phi : double);

procedure ALG0038(const LandaC, PhiC, c, n1, n2, Xs, Ys, e, Landa, Phi : double;
                  var X, Y : double);

procedure ALG0039(const LandaC, PhiC, c, n1, n2, Xs, Ys, e, X, Y : double;
                  var Landa, Phi : double);

procedure ALG0043_courbure(const a, e, Phi0, k0, Y0 : double; var n1, n2, c : double);

procedure ALG0043_equatoriale(const a, e, Phi0, k0 : double; var n2, PhiC : double);

procedure ALG0043_bitan(const a, e, Phi0, k0 : double; var n2, c : double);

procedure ALG0046_courbure(const a, e, Phi0, k0, Y0 : double; var n1, c, n2, Ys : double);

procedure ALG0046_equatoriale(const a, e, Phi0, k0 : double; var n2 : double);

procedure ALG0046_bitan(const a, e, Phi0, k0 : double; var n2, c : double);

procedure ALG0052(const a, e, k0, Phi0, Y0 : double; var n, Ys : double);

procedure ALG0054(const a, e, Phi0, Phi1, Phi2, Y0 : double; var n, c, Ys : double);

procedure ALG0063(const Tx, Ty, Tz, D, Rx, Ry, Rz, VVx, VVy, VVz : double; var Ux, Uy, Uz : double);

// SuissTopo
function swiss01(const Phi : double) : double;
function swiss02(const e, alpha, Phi : double) : double;
function swiss03(const a, e, Phi : double) : double;
function swiss04(const e, Phi : double) : double;


implementation

uses
  Math, NombresComplexes;



function ALG0001(const Phi, e : double) : double;
begin
  result:=ln(tan(Pi/4+Phi/2)*power((1-e*sin(Phi))/(1+e*sin(Phi)),e/2));
end;

function ALG0002(const L, e : double) : double;
var
  PhiN, PhiN1 : double;
begin
  PhiN1:=2*arctan(exp(L))-Pi/2;
  repeat
    PhiN:=PhiN1;
    PhiN1:=2*arctan(power((1+e*sin(PhiN))/(1-e*sin(PhiN)),e/2)*exp(L))-Pi/2;
  until abs(PhiN-PhiN1)<Eps;
  result:=PhiN1;
end;

procedure ALG0003(const Landa, Phi, n, c, e, LandaC, Xs, Ys : double; var X, Y : double);
var
  L : double;
begin
  L:=ALG0001(Phi, e);
  X:=Xs+c*exp(-n*L)*sin(n*(Landa-LandaC));
  Y:=Ys-c*exp(-n*L)*cos(n*(Landa-LandaC));
end;

procedure ALG0004(const X, Y, n, c, e, LandaC, Xs, Ys : double; var Landa, Phi : double);
var
  R, Gamma, L : double;
begin
  R:=sqrt(sqr(X-Xs)+sqr(Y-Ys));
  Gamma:=arctan((X-Xs)/(Ys-Y));
  Landa:=LandaC+Gamma/n;
//  L:=-1/n*ln(R/c);  formule originale; problème de signe négatif dans l'hémisphère sud
  L:=-1/n*ln(abs(R/c));
  Phi:=ALG0002(L,e);
end;

procedure ALG0009(const Landa, Phi, h, a, e : double; var X, Y, Z : double);
var
  N : double;
begin
  N:=ALG0021(Phi,a,e);
  X:=(N+h)*cos(Phi)*cos(Landa);
  Y:=(N+h)*cos(Phi)*sin(Landa);
  Z:=(N*(1-sqr(e))+h)*sin(Phi);
end;

procedure ALG0012(const X, Y, Z, a, e : double; var Landa, Phi, h : double);
var
  PhiN, PhiN1 : extended;
  C1, C2, cosP, sinP : extended;
begin
  C1 := 1.0 / sqrt(sqr(X) + sqr(Y));
  C2 := a * sqr(e) * C1;
  C1 := C1 * Z;

  Landa:=arctan2(Y,X);
  PhiN1:=arctan(C1/(1.0-a*sqr(e)/sqrt(sqr(X)+sqr(Y)+sqr(Z))));
//  PhiN1:=arctan(Z/sqrt(sqr(X)+sqr(Y))/(1-a*sqr(e)/sqrt(sqr(X)+sqr(Y)+sqr(Z))));
  repeat
    PhiN:=PhiN1;
{    PhiN1:=arctan(Z/sqrt(sqr(X)+sqr(Y))/(1-a*sqr(e)*cos(PhiN)/sqrt(sqr(X)+sqr(Y))/
                 sqrt(1-sqr(e*sin(PhiN))))); }
    SinCos(PhiN, SinP, CosP);
    PhiN1:=arctan(C1/(1.0-C2*cosP/sqrt(1.0-sqr(e*sinP))));
  until abs(PhiN-PhiN1)<Eps;
  Phi:=PhiN1;
  h:=sqrt(sqr(X)+sqr(Y))/cos(Phi)-a/sqrt(1.0-sqr(e*sin(Phi)));
end;

procedure ALG0013(const Tx, Ty, Tz, D, Rx, Ry, Rz, Ux, Uy, Uz : double; var Vx, Vy, Vz : double);
// Rx, Ry et Rz doivent être en radian
// D est le facteur d'échelle, doit être proche de 0
begin
  Vx:=Tx+Ux*(1+D)+Uz*Ry-Uy*Rz;
  Vy:=Ty+Uy*(1+D)+Ux*Rz-Uz*Rx;
  Vz:=Tz+Uz*(1+D)+Uy*Rx-Ux*Ry;
end;

procedure ALG0013bis(const Tx, Ty, Tz, D, Rx, Ry, Rz, Ux, Uy, Uz : double; var Vx, Vy, Vz : double);
// Rx, Ry et Rz doivent être en radian
// D est le facteur d'échelle, doit être proche de 0
begin
  Vx:=(Tx-Ux)*(D-1)+(Tz-Uz)*Ry-(Ty-Uy)*Rz;
  Vy:=(Ty-Uy)*(D-1)+(Tx-Ux)*Rz-(Tz-Uz)*Rx;
  Vz:=(Tz-Uz)*(D-1)+(Ty-Uy)*Rx-(Tx-Ux)*Ry;
end;

procedure ALG0014y(const Vx, Vy, Vz, Alpha : double; var Ux, Uy, Uz : double);
begin
  Ux:=Vx*cos(Alpha)-Vz*sin(Alpha);
  Uy:=Vy;
  Uz:=Vz*cos(Alpha)+Vx*sin(Alpha);
end;

procedure ALG0015(const Landa, Phi : double; var Ux, Uy, Uz : double);
begin
  Ux:=cos(Phi)*Cos(Landa);
  Uy:=cos(Phi)*sin(Landa);
  Uz:=sin(Phi);
end;

procedure ALG0016(const Ux, Uy, Uz : double; var Landa, Phi : double);
var
  p : double;
begin
  p:=sqrt(sqr(Ux)+sqr(Uy));
  if p<Eps then
  begin
    Landa:=0;
    Phi:=sign(Uz)*Pi/2;
  end
  else begin
    Landa:=2*arctan(Uy/(Ux+p));
    Phi:=arctan(Uz/P);
  end;
end;

procedure ALG0019(const a, e, Phi0, k0, Y0 : double; var n, c, Ys : double);
begin
  n:=sin(Phi0);
  c:=k0*ALG0021(Phi0,a,e)*cotan(Phi0)*exp(n*ALG0001(Phi0,e));
  Ys:=Y0+k0*ALG0021(Phi0,a,e)*cotan(Phi0);
end;

function ALG0021(const Phi, a, e : double) : double;
begin
  result:=a/sqrt(1-sqr(e*sin(Phi)));
end;

function ALG0025(const ee : double) : TTableau5;
var
  f, e : double;
begin
  e:=sqr(ee); // e^2
  result[1]:=1-e/4;
  result[2]:=-3*e/8;

  f:=sqr(e); // e^4
  result[1]:=result[1]-3*f/64;
  result[2]:=result[2]-3*f/32;
  result[3]:=15*f/256;

  f:=f*e; // e^6
  result[1]:=result[1]-5*f/256;
  result[2]:=result[2]-45*f/1024;
  result[3]:=result[3]+45*f/1024;
  result[4]:=-35*f/3072;

  f:=f*e; // e^8
  result[1]:=result[1]-175*f/16384;
  result[2]:=result[2]-105*f/4096;
  result[3]:=result[3]+525*f/16384;
  result[4]:=result[4]-175*f/12288;
  result[5]:=315*f/131072;
end;

function ALG0026(const Phi, e : double) : double;
var
  C : TTableau5;
  k : integer;
begin
  C:=ALG0025(e);
  result:=C[1]*Phi;
  for K:=1 to 4 do
    result:=result+C[k+1]*sin(2*k*Phi);
end;

function ALG0028(const ee : double) : TTableau5;
var
  f, e : double;
begin
  e:=sqr(ee); // e^2
  result[1]:=1-e/4;
  result[2]:=e/8;

  f:=sqr(e); // e^4
  result[1]:=result[1]-3*f/64;
  result[2]:=result[2]-f/96;
  result[3]:=13*f/768;

  f:=f*e; // e^6
  result[1]:=result[1]-5*f/256;
  result[2]:=result[2]-9*f/1024;
  result[3]:=result[3]+17*f/5120;
  result[4]:=61*f/15360;

  f:=f*e; // e^8
  result[1]:=result[1]-175*f/16384;
  result[2]:=result[2]-901*f/184320;
  result[3]:=result[3]-311*f/737280;
  result[4]:=result[4]+899*f/430080;
  result[5]:=49561*f/41287680;
end;

function ALG0029(e : double) : TTableau5;
var
  f : double;
begin
  e:=sqr(e); // e^2
  result[1]:=1-e/4;
  result[2]:=e/8;

  f:=sqr(e); // e^4
  result[1]:=result[1]-3*f/64;
  result[2]:=result[2]+f/48;
  result[3]:=f/768;

  f:=f*e; // e^6
  result[1]:=result[1]-5*f/256;
  result[2]:=result[2]+7*f/2048;
  result[3]:=result[3]+3*f/1280;
  result[4]:=17*f/30720;

  f:=f*e; // e^8
  result[1]:=result[1]-175*f/16384;
  result[2]:=result[2]+f/61440;
  result[3]:=result[3]+559*f/368640;
  result[4]:=result[4]+283*f/430080;
  result[5]:=4397*f/41287680;
end;

procedure ALG0030(const LandaC, n, Xs, Ys, e, Landa, Phi : double; var X, Y : double);
var
  C : TTableau5;
  L_Iso, L_IsoS, Psi, LandaMaj : double;
  PttZ, GrandZ : TComplex;
  i : integer;
begin
  C:=ALG0028(e);
  L_Iso:=ALG0001(Phi,e);
  Psi:=arcsin(sin(Landa-LandaC)/cosh(L_Iso));
  L_IsoS:=ALG0001(Psi,0);
  LandaMaj:=arctan(sinh(L_Iso)/cos(Landa-LandaC));

  PttZ.x:=LandaMaj; PttZ.y:=L_IsoS;
  GrandZ:=CplxMultiplicationScalaire(PttZ, n*C[1]);

  for I:=1 to 4 do
    GrandZ:=CplxAddition(GrandZ, CplxMultiplicationScalaire(CplxSinus(CplxMultiplicationScalaire(PttZ,2*I))    , n*C[i+1]));

  X:=Xs+GrandZ.y;
  Y:=Ys+GrandZ.x;
end;

procedure ALG0031(const LandaC, n, Xs, Ys, e, X, Y : double; var Landa, Phi : double);
var
  C : TTableau5;
  zp, z : TComplex;
  k : integer;
  L_Iso, L_IsoS, Psi : double;
begin
  C:=ALG0029(e);
  Zp.x:=(Y-Ys)/(n*C[1]);
  Zp.y:=(X-Xs)/(n*C[1]);
  Z:=Zp;

  for k:=1 to 4 do
    Z:=CplxAddition(Z, CplxMultiplicationScalaire(CplxSinus(CplxMultiplicationScalaire(Zp,2*k))    , -C[k+1]));
  L_Iso:=Z.x;
  L_Isos:=Z.y;

  Landa:=LandaC+arctan(sinh(L_IsoS)/cos(L_Iso));
  Psi:=arcsin(sin(L_Iso)/cosh(L_IsoS));
  Phi:=ALG0002(ALG0001(Psi,0),e);
end;

procedure ALG0034(const Landa, Phi, e, LandaC, n1, n2, Xs, Ys, c : double; var X, Y : double);
var
  LandaMaj, L_IsoS : double;
begin
  LandaMaj:=n1*(Landa-LandaC);
  L_IsoS:=c+n1*ALG0001(Phi,e);
  X:=Xs+n2*ALG0001(arcsin(sin(LandaMaj)/cosh(L_IsoS)),0);
  Y:=Ys+n2*arctan(sinh(L_IsoS)/cos(LandaMaj));
end;

procedure ALG0035(const X, Y, e, LandaC, n1, n2, Xs, Ys, c : double; var Landa, Phi : double);
var
  LandaMaj, L_Iso : double;
begin
  LandaMaj:=arctan(sinh((X-Xs)/n2)/cos((Y-Ys)/n2));
  L_Iso:=ALG0001(arcsin(sin((Y-Ys)/n2)/cosh((X-Xs)/n2)),0);
  Landa:=LandaC+LandaMaj/n1;
  Phi:=ALG0002((L_Iso-c)/n1,e);
end;

procedure ALG0038(const LandaC, PhiC, c, n1, n2, Xs, Ys, e, Landa, Phi : double;
                  var X, Y : double);
var
  LandaMaj, L_Iso, PhiMaj, Ux, Uy, Uz : double;
begin
  LandaMaj:=n1*(Landa-LandaC);
  L_Iso:=c + n1*ALG0001(Phi,e);
  PhiMaj:=ALG0002(L_Iso,0);
  ALG0015(LandaMaj, PhiMaj, Ux, Uy, Uz);
  ALG0014y(Ux, Uy, Uz, Pi/2-PhiC, Ux, Uy, Uz);
  X:=Xs+n2*Uy/(1+Uz)*2;
  Y:=Ys-n2*Ux/(1+Uz)*2;
end;

procedure ALG0039(const LandaC, PhiC, c, n1, n2, Xs, Ys, e, X, Y : double;
                  var Landa, Phi : double);
var
  LandaMaj, PhiMaj, Ux, Uy, Uz, r : double;
begin
  Ux:=X-Xs;
  Uy:=-(Y-Ys);
  r:=sqrt(sqr(Ux)+sqr(Uy));
  if r<Eps then
    LandaMaj:=0
  else
    LandaMaj:=2*arctan(Ux/(Uy+r));
  PhiMaj:=Pi/2-2*arctan(r/n2/2);
  ALG0015(LandaMaj, PhiMaj, Ux, Uy, Uz);
  ALG0014y(Ux, Uy, Uz, PhiC-Pi/2, Ux, Uy, Uz);
  ALG0016(Ux, Uy, Uz, LandaMaj, PhiMaj);
  Landa:=Landac+LandaMaj/n1;
  Phi:=ALG0002((ALG0001(PhiMaj,0)-c)/n1,e);
end;

procedure ALG0043_courbure(const a, e, Phi0, k0, Y0 : double; var n1, n2, c : double);
var
  PhiC : double;
begin
  n1:=swiss04(e,Phi0);
  PhiC:=arcsin(sin(Phi0)/n1);
  c:=ALG0001(PhiC,0)-n1*ALG0001(Phi0,e);
  n2:=k0*a*sqrt(1-sqr(e))/(1-sqr(e*sin(Phi0)));
end;

procedure ALG0043_equatoriale(const a, e, Phi0, k0 : double; var n2, PhiC : double);
begin
  PhiC:=ALG0002(ALG0001(Phi0,e),0);
  n2:=k0*a*cos(Phi0)/sqrt(1-sqr(e*sin(Phi0)))/cos(PhiC);
end;

procedure ALG0043_bitan(const a, e, Phi0, k0 : double; var n2, c : double);
begin
  c:=ALG0001(Phi0,0)-ALG0001(Phi0,e);
  n2:=k0*a/sqrt(1-sqr(e*sin(Phi0)));
end;

procedure ALG0046_courbure(const a, e, Phi0, k0, Y0 : double; var n1, c, n2, Ys : double);
var
  PhiC : double;
begin
  n1:=swiss04(e,Phi0);
  PhiC:=arcsin(sin(Phi0)/n1);
  c:=ALG0001(PhiC,0)-n1*ALG0001(Phi0,e);
  n2:=k0*a*sqrt(1-sqr(e))/(1-sqr(e*sin(Phi0)));
  Ys:=Y0-n2*PhiC;
end;

procedure ALG0046_equatoriale(const a, e, Phi0, k0 : double; var n2 : double);
var
  PhiC : double;
begin
  PhiC:=ALG0002(ALG0001(Phi0,e),0);
  n2:=k0*a/sqrt(1-sqr(e*sin(Phi0)))*cos(Phi0)/cos(PhiC);
end;

procedure ALG0046_bitan(const a, e, Phi0, k0 : double; var n2, c : double);
begin
  c:=ALG0001(Phi0,0)-ALG0001(Phi0,e);
  n2:=k0*a/sqrt(1-sqr(e*sin(Phi0)));
end;

procedure ALG0052(const a, e, k0, Phi0, Y0 : double; var n, Ys : double);
begin
  n:=k0*a;
  Ys:=Y0-n*ALG0026(Phi0,e);
end;

procedure ALG0054(const a, e, Phi0, Phi1, Phi2, Y0 : double; var n, c, Ys : double);
begin
  n:=ln(ALG0021(Phi2,a,e)*cos(Phi2)/ALG0021(Phi1,a,e)/cos(Phi1))/
                (ALG0001(Phi1,e)-ALG0001(Phi2,e));
  C:=ALG0021(Phi1,a,e)*cos(Phi1)/n*exp(n*ALG0001(Phi1,e));
  if abs(Phi0-Pi/2)<Eps then
    Ys:=Y0
  else
    Ys:=Y0+c*exp(-n*ALG0001(Phi0,e));
end;

procedure ALG0063(const Tx, Ty, Tz, D, Rx, Ry, Rz, VVx, VVy, VVz : double; var Ux, Uy, Uz : double);
// Rx, Ry et Rz doivent être en radian
// D est le facteur d'échelle, doit être proche de 0
var
  e, det : double;
  Vx: Extended;
  Vy: Extended;
  Vz: Extended;
begin
  Vx:=VVx-Tx;
  Vy:=VVy-Ty;
  Vz:=VVz-Tz;
  e:=1+D;
  det:= e * (sqr(e)+sqr(Rx)+sqr(Ry)+sqr(Rz));
  Ux:=((sqr(e)+sqr(Rx))*Vx + (e*Rz+Rx*Ry)*Vy + (Rx*Rz-e*Ry)*Vz)/det;
  Uy:=((-e*Rz+Rx*Ry)*Vx + (sqr(e)+sqr(Ry))*Vy + (e*Rx+Ry*Rz)*Vz)/det;
  Uz:=((e*Ry+Rx*Rz)*Vx + (-e*Rx+Ry*Rz)*Vy + (sqr(e)+sqr(Rz))*Vz)/det;
end;

// SuissTopo
function swiss01(const Phi : double) : double;
begin
  result:=ln(tan(Pi/4+Phi/2));
end;

function swiss02(const e, alpha, Phi : double) : double;
begin
  result:=Alpha*e/2*ln((1+e*sin(Phi))/(1-e*sin(Phi)));
end;

function swiss03(const a, e, Phi : double) : double;
begin
  result:=a*sqrt(1-sqr(e))/(1-sqr(e*sin(Phi)));
end;

function swiss04(const e, Phi : double) : double;
begin
  result:=sqrt(1+sqr(e)/(1-sqr(e))*power(cos(Phi),4));
end;

end.

