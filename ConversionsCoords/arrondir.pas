unit arrondir;

interface

// renvoie la valeur entière strictement inférieure
function MyInt(X : real) : real;

// renvoie l'entier strictement inférieur
function MyTrunc(X : real) : integer;

// partie fractionnaire, renvoie la différence avec l'entier strictement inférieur
function MyFrac(X : real) : real;

// arrondi de sorte à avoir 1, 2 ou 5 comme premier chiffre puis que des zéros.
function Arrondi125(Pas : real) : real;

// arrondi un réel compris entre 1 et 60 à la valeur entière sous multiple
// de 60 la plus proche (à l'exception de 12 et 6).
function Arrondi60(Pas : real) : real;


implementation

uses
  Math;


function MyInt(X : real) : real;
begin
  if X>=0 then
    result:=Int(X)
  else
    result:=Int(X)-1;
end;

function MyTrunc(X : real) : integer;
begin
  if X>=0 then
    result:=trunc(X)
  else
    result:=trunc(X)-1;
end;

function MyFrac(X : real) : real;
begin
  result:=X-MyInt(X);
end;

function Arrondi125(Pas : real) : real;
var
  FractionPas : real;
begin
  FractionPas:=MyFrac(log10(Pas));
  if FractionPas<0.15 then
    result:=Power(10,MyTrunc(Log10(Pas)))
  else
  if FractionPas<0.5 then
    result:=2*Power(10,MyTrunc(Log10(Pas)))
  else
  if FractionPas<0.85 then
    result:=5*Power(10,MyTrunc(Log10(Pas)))
  else
    result:=10*Power(10,MyTrunc(Log10(Pas)));
end;

function Arrondi60(Pas : real) : real;
begin
  assert((Pas>=1) and (Pas<=60),'Pas sexadecimal hors limites');
  if Pas>42.4 then
    result:=60
  else if Pas>24.5 then
    result:=30
  else if Pas>17.3 then
    result:=20
  else if Pas>12.25 then
    result:=15
  else if Pas>7.07 then
    result:=10
  else if Pas>3.87 then
    result:=5
  else if Pas>2.45 then
    result:=3
  else if Pas>1.414 then
    result:=2
  else result:=1;
end;

end.
