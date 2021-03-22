unit utm;

// cas de la projection UTM

// documentation de référence
//
// IGN
// Projection cartographique Mercator transverse
// Notes techniques NT/G 76
//
// http://professionnels.ign.fr/DISPLAY/000/526/702/5267021/NTG_76.pdf

// Note : il serait judicieux d'avoir un ancètre commun à TUtm et TMtm

interface

uses
  projection, transmercator_brut;

type

  TUTM = class(TTransverseMercatorBrut)
  private
    ff : integer; // numéro du fuseau
    fSud : boolean; // au sud
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
    procedure ConvertirParam; override; // convertit les paramètres vers la projection TLambert (générique)
  public
  end;

implementation

uses
  SysUtils,
  outils, ign;


function TUTM.GetNature : string;
begin
  result:=tpUTM;
end;

function TUTM.GetNbReal : integer;
begin
  result:=0;
end;

function TUTM.GetParamReal(index : integer) : real;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TUTM.SetParamReal(index : integer; Value : real);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TUTM.GetNom_ParamReal(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TUTM.GetNbInteger : integer;
begin
  result:=1;
end;

function TUTM.GetParamInteger(index : integer) : Integer;
begin
  if index=0 then
    result:=ff
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TUTM.SetParamInteger(index : integer; Value : Integer);
begin
  if index=0 then
    ff:=Value
  else
  begin
    fValid:=false;
    raise ERangeError.Create('Ecriture de paramètre hors-limites');
  end;
  ConvertirParam;
end;

function TUTM.GetNom_ParamInteger(index : integer) : string;
begin
  if index=0 then
    result:='Fuseau'
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TUTM.GetNbAngle : integer;
begin
  result:=0;
end;

function TUTM.GetParamAngle(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TUTM.SetParamAngle(index : integer; Value : string);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TUTM.GetNom_ParamAngle(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TUTM.GetNbBoolean : integer;
begin
  result:=1;
end;

function TUTM.GetParamBoolean(index : integer) : Boolean;
begin
  if index=0 then
    result:=fSud
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TUTM.SetParamBoolean(index : integer; Value : Boolean);
begin
  if index=0 then
    fSud:=Value
  else
  begin
    fValid:=false;
    raise ERangeError.Create('Ecriture de paramètre hors-limites');
  end;
  ConvertirParam;
end;

function TUTM.GetNom_ParamBoolean(index : integer) : string;
begin
  if index=0 then
    result:='Sud'
  else
  begin
    result:='';
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TUTM.ConvertirParam;
begin
  if fValid then
  try
    inherited SetParamReal(0, 0.9996*Datum.Ellipsoide.a);
    inherited SetParamReal(1, 500000);
    if fSud then
      inherited SetParamReal(2, 10000000)
    else
      inherited SetParamReal(2, 0);
    inherited SetParamAngle(0,Format('%dd',[6*ff-183]));
  except
  end;
end;


end.
