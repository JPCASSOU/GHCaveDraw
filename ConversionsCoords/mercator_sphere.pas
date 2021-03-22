unit mercator_sphere;

// cas de la projection Mercator appliquée à la sphère
// utilisée pour la carte générale de la terre, Google Map, OpenStreetMap
// et sans doute beaucoup d'autres

// documentation de référence
//
// Map Projections: A Working Manual, Snyder, John P., 1987, Geological Survey (U.S.)
// http://pubs.er.usgs.gov/usgspubs/pp/pp1395
//
// http://pubs.er.usgs.gov/djvu/PP/PP_1395.pdf


interface

uses
  projection;

type

  TMercator_sphere = class(TProjection)
  private
    fLanda0 : string; // longitude de l'origine par rapport au méridien d'origine du Datum
    fLanda0Real : real; // valeur en radian de la longitude précédente
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
  outils;


function TMercator_sphere.GetNature : string;
begin
  result:=tpMercatorSphere;
end;

function TMercator_sphere.GetNbReal : integer;
begin
  result:=2;
end;

function TMercator_sphere.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fXs;
    1 : result:=fYs;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TMercator_sphere.SetParamReal(index : integer; Value : real);
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

function TMercator_sphere.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='X0';
    1 : result:='Y0';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TMercator_sphere.GetNbInteger : integer;
begin
  result:=0;
end;

function TMercator_sphere.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TMercator_sphere.SetParamInteger(index : integer; Value : Integer);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TMercator_sphere.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TMercator_sphere.GetNbAngle : integer;
begin
  result:=1;
end;

function TMercator_sphere.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fLanda0;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TMercator_sphere.SetParamAngle(index : integer; Value : string);
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
  else
    begin
      fValid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;  
end;

function TMercator_sphere.GetNom_ParamAngle(index : integer) : string;
begin
  case index of
    0 : result:='Landa0';
  else
    begin
      result:='';
      raise ERangeError.Create('Demande de paramètre hors-limites');
    end;
  end;  
end;

function TMercator_sphere.GetNbBoolean : integer;
begin
  result:=0;
end;

function TMercator_sphere.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TMercator_sphere.SetParamBoolean(index : integer; Value : Boolean);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TMercator_sphere.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure InsidePiPi(var X : real);
  // on remet la longitude entre -Pi et +Pi
var
  N : integer;
begin
  N:=trunc(X/Pi);
  if abs(N)>0 then
  begin
    if N>0 then
      X:=X-((N+1) div 2)*2*Pi
    else
      X:=X-((N-1) div 2)*2*Pi
  end;
end;

procedure TMercator_sphere.VersGrille(Lat, Long : real; var X, Y : real);
var
  DeltaLanda : real;
begin
  DeltaLanda:=Long-fLanda0real;
  InsidePiPi(DeltaLanda);

  X:= Datum.Ellipsoide.a * DeltaLanda + fXs;
  Y:= Datum.Ellipsoide.a * ln (tan (Pi/4+Lat/2)) + fYs;
end;

procedure TMercator_sphere.VersLatLong(X, Y : real; var Lat, Long : real);
begin
  Long:=(X-fXs)/Datum.Ellipsoide.a + fLanda0real;
  InsidePiPi(Long);
  Lat:=Pi/2 - 2 *arctan(exp(-(Y-fYs)/Datum.Ellipsoide.a));
end;


end.
