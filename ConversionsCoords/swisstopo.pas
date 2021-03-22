unit swisstopo;

interface

function swiss01(Phi : real) : real;
function swiss02(e, alpha, Phi : real) : real;
function swiss03(a, e, Phi : real) : real;
function swiss04(e, Phi : real) : real;

implementation

uses
  Math;

function swiss01(Phi : real) : real;
begin
  result:=ln(tan(Pi/4+Phi/2));
end;

function swiss02(e, alpha, Phi : real) : real;
begin
  result:=Alpha*e/2*ln((1+e*sin(Phi))/(1-e*sin(Phi)));
end;

function swiss03(a, e, Phi : real) : real;
begin
  result:=a*sqrt(1-sqr(e))/(1-sqr(e*sin(Phi)));
end;

function swiss04(e, Phi : real) : real;
begin
  result:=sqrt(1+sqr(e)/(1-sqr(e))*power(cos(Phi),4));
end;  

end.
