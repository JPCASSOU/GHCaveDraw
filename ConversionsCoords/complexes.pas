Unit complexes;
{$mode delphi}{$H+}
interface

type

  TComplex = record
    x, y : real;
  end;

function CplxZero : TComplex;

function CplxAddition (A, B : TComplex) : TComplex;

function CplxMultiplication(A, B : TComplex) : TComplex;

function CplxMultiplicationScalaire(A : TComplex; R : real) : TComplex;

function CplxDivision(A, B : TComplex) : TComplex;

function CplxPowerInt(Z : TComplex; N : integer) : TComplex;

function CplxSinus(A : TComplex) : TComplex;

procedure CplxPolar(Z : TComplex; var Module, Argument: real);


implementation

uses
  Math;

function CplxZero : TComplex;
begin
  result.x := 0;
  result.y := 0;
end;

function CplxAddition (A, B : TComplex) : TComplex;
begin
  result.x := A.x + B.x;
  result.y := A.y + B.y;
end;

function CplxMultiplication(A, B : TComplex) : TComplex;
begin
  result.x := A.x * B.x - A.y * B.y;
  result.y := A.x * B.y + A.y * B.x;
end;

function CplxMultiplicationScalaire(A : TComplex; R : real) : TComplex;
begin
  result.x := A.x * R;
  result.y := A.y * R;
end;

function CplxDivision(A, B : TComplex) : TComplex;
var
  Delta : real;
begin
  Delta:=1/(sqr(B.x)+sqr(B.y));
  result.x:=(A.x*B.x+A.y*B.y)*Delta;
  result.y:=(A.y*B.x-A.x*B.y)*Delta;
end;

function CplxPowerInt(Z : TComplex; N : integer) : TComplex;
var
  I : integer;
begin
  assert(N>=0,'La puissance doit être positive ou nulle dans CplxPowerInt');
  result.x := 1;
  result.y := 0;
  for I := 1 to N do
    result := CplxMultiplication(result, Z);
end;

function CplxSinus(A : TComplex) : TComplex;
begin
  result.X := Sin(A.x)*Cosh(A.y);
  result.Y := Cos(A.x)*Sinh(A.y);
end;

procedure CplxPolar(Z : TComplex; var Module, Argument : real);
begin
  Module := sqrt(sqr(Z.x) + sqr(Z.y));
  Argument := Arctan2(Z.x, Z.y);
end;

end.
