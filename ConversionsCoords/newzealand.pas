unit newzealand;

// cas de la projection Néo-Zélandaise (New Zealand Map Grid)
// utilisée uniquement en Nouvelle-Zélande

// documentation de référence
//
// Land Information New Zealand (LINZ)
// Conversion between Latitude and Longitude and New Zealand Map Grid
//
// http://www.linz.govt.nz/docs/miscellaneous/nz-map-definition.pdf

interface

uses
  projection;

type

  TNewZealand = class(TProjection)
  private
    fLanda0 : string; // longitude de l'origine par rapport au méridien d'origine du Datum
    fLanda0Real : real; // valeur en radian de la longitude précédente
    fPhi0 : string; // latitude de l'origine par rapport au méridien d'origine du Datum
    fPhi0Real : real; // valeur en radian de la latitude précédente
    fXs, fYs : real; // coordonnées en projection du pôle (false Est et false Nord)
  protected
    // real
    function GetNbReal : integer; override;
    function GetParamReal(index : integer) : real; override;
    procedure SetParamReal(index : integer; Value : real); override;
    function GetNom_ParamReal(index : integer) : string; override;
    // integer
    function GetNbInteger : integer; override;
    function GetParamInteger(index : integer) : Integer; override;
    procedure SetParamInteger(index : integer; Value : Integer); override;
    function GetNom_ParamInteger(index : integer) : string; override;
    // Angle
    function GetNbAngle : integer; override;
    function GetParamAngle(index : integer) : string; override;
    procedure SetParamAngle(index : integer; Value : string); override;
    function GetNom_ParamAngle(index : integer) : string; override;
    // Boolean
    function GetNbBoolean : integer; override;
    function GetParamBoolean(index : integer) : Boolean; override;
    procedure SetParamBoolean(index : integer; Value : Boolean); override;
    function GetNom_ParamBoolean(index : integer) : string; override;

    function GetNature : string; override;
  public
    procedure VersGrille(Lat, Long : real; var X, Y : real); override;
    procedure VersLatLong(X, Y : real; var Lat, Long : real); override;
  end;

implementation

uses
  SysUtils, Math,
  complexes, outils;


function TNewZealand.GetNature : string;
begin
  result:=tpNewZealandMapGrid;
end;

function TNewZealand.GetNbReal : integer;
begin
  result:=2;
end;

function TNewZealand.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fXs;
    1 : result:=fYs;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TNewZealand.SetParamReal(index : integer; Value : real);
begin
  case Index of
    0 : fXs:=Value;
    1 : fYs:=Value;
  else
    begin
      fValid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;
end;

function TNewZealand.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='X0';
    1 : result:='Y0';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TNewZealand.GetNbInteger : integer;
begin
  result:=0;
end;

function TNewZealand.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TNewZealand.SetParamInteger(index : integer; Value : Integer);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TNewZealand.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TNewZealand.GetNbAngle : integer;
begin
  result:=2;
end;

function TNewZealand.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fLanda0;
    1 : result:=fPhi0;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;  
end;

procedure TNewZealand.SetParamAngle(index : integer; Value : string);
begin
  case index of
    0 : begin
      try
        fLanda0:=Value;
        fLanda0real:=StrToAngle(Value);
      except
        fValid:=false;
        Raise;
      end;
    end;
    1 : begin
      try
        fPhi0:=Value;
        fPhi0real:=StrToAngle(Value);
      except
        fValid:=false;
        Raise;
      end;
    end;
  else
    begin
      fValid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;  
end;

function TNewZealand.GetNom_ParamAngle(index : integer) : string;
begin
  case index of
    0 : result:='Landa0';
    1 : result:='Phi0';
  else
    begin
      result:='';
      raise ERangeError.Create('Demande de paramètre hors-limites');
    end;
  end;  
end;

function TNewZealand.GetNbBoolean : integer;
begin
  result:=0;
end;

function TNewZealand.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TNewZealand.SetParamBoolean(index : integer; Value : Boolean);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TNewZealand.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;


const

  A : array[1..10] of real =
   (0.6399175073,
   -0.1358797613,
   0.063294409,
   -0.02526853,
   0.0117879,
   -0.0055161,
   0.0026906,
   -0.001333,
   0.00067,
   -0.00034);

  Breal : array[1..6] of real =
    (0.7557853228,
    0.249204646,
    -0.001541739,
    -0.10162907,
    -0.26623489,
    -0.6870983);

  Bimaginary : array[1..6] of real =
    (0,
    0.003371507,
    0.04105856,
    0.01727609,
    -0.36249218,
    -1.1651967);

  B : array[1..6] of TComplex =
    ((x:0.7557853228;y:0),
    (x:0.249204646;y:0.003371507),
    (x:-0.001541739;y:0.04105856),
    (x:-0.10162907;y:0.01727609),
    (x:-0.26623489;y:-0.36249218),
    (x:-0.6870983;y:-1.1651967));

  Creal : array[1..6] of real =
    (1.3231270439,
    -0.577245789,
    0.508307513,
    -0.15094762,
    1.01418179,
    1.9660549);

  Cimaginary : array[1..6] of real =
    (0,
    -0.007809598,
    -0.112208952,
    0.18200602,
    1.64497696,
    2.5127645);

  D : array[1..9] of real =
    (1.5627014243,
    0.5185406398,
    -0.03333098,
    -0.1052906,
    -0.0368594,
    0.007317,
    0.01220,
    0.00394,
    -0.0013);

procedure TNewZealand.VersGrille(Lat, Long : real; var X, Y : real);
var
  DeltaPhi, DeltaPsi, DeltaLanda : real;
  n : integer;
  Tetha, z : TComplex;
begin
  DeltaPhi:=(Lat-fPhi0real)/Pi*180*3600*1e-5;
  DeltaPsi:=0;
  for n:=1 to 10 do
    DeltaPsi:=DeltaPsi+A[n]*IntPower(DeltaPhi,n);
  DeltaLanda:=Long-fLanda0real;

  Tetha.x:=DeltaPsi;
  Tetha.y:=DeltaLanda;

  Z:=CplxZero;

  for n:=1 to 6 do
    Z:=CplxAddition(Z, CplxMultiplication(B[n],CplxPowerInt(Tetha,n)));

  X:=Z.y*Datum.Ellipsoide.a+fXs;
  Y:=Z.x*Datum.Ellipsoide.a+fYs;
end;

procedure TNewZealand.VersLatLong(X, Y : real; var Lat, Long : real);
var
  DeltaPhi, DeltaPsi, DeltaLanda : real;
  n, cycle : integer;
  Tetha, TethaPlus, z, Haut, Bas : TComplex;
begin
  Z.x:=(Y-fYs)/Datum.Ellipsoide.a;
  Z.y:=(X-fXs)/Datum.Ellipsoide.a;

  TethaPlus:=CplxZero;

  for n:=1 to 6 do
    TethaPlus:=CplxAddition(TethaPlus, CplxPowerInt(Z, n));

  cycle:=2;
  repeat
    Tetha:=TethaPlus;
    Haut:=z;
    for n:=2 to 6 do
      Haut:=CplxAddition(Haut,CplxMultiplication(CplxMultiplicationScalaire(B[n],(n-1)),CplxPowerInt(Tetha,n)));
    Bas:=CplxZero;
    for n:=1 to 6 do
      Bas:=CplxAddition(Bas,CplxMultiplication(CplxMultiplicationScalaire(B[n],n),CplxPowerInt(Tetha,n-1)));
    TethaPlus:=CplxDivision(Haut,Bas);
    dec(cycle);
  until cycle<=0;

  DeltaPsi:=TethaPlus.x;
  DeltaLanda:=TethaPlus.y;

  DeltaPhi:=0;
  for n:=1 to 9 do
    DeltaPhi:=DeltaPhi+D[n]*IntPower(DeltaPsi,n);

  Long:=fLanda0Real+DeltaLanda;
  Lat:=fPhi0Real+DeltaPhi*1e5/3600/180*Pi;
end;

end.
