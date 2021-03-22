unit mtm;

// cas de la projection MTM (Modified Transverse Mercator)

// cette projection ne semble utilisée qu'au Québec et peut-être
// dans les provinces maritimes canadiennes (Terre-Neuve, Nouvelle-Ecosse
// Nouveau-Brunswick).

// elle utilise une numérotation similaire à l'UTM avec des fuseaux de 3°
// au lieu de 6°.
// bien que la projection ne semble pas utilisée dans l'hémisphère sud
// j'ai utilisé le même principe que pour l'UTM pour les aspects Nord/Sud

// Note : il n'y a pas eu de tests extensifs sur cette projection

// Note(2) : il serait judicieux d'avoir un ancètre commun à TUtm et TMtm



interface

uses
  projection, transmercator_brut;

type

  Tmtm = class(TTransverseMercatorBrut)
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


function Tmtm.GetNature : string;
begin
  result:=tpMTM;
end;

function Tmtm.GetNbReal : integer;
begin
  result:=0;
end;

function Tmtm.GetParamReal(index : integer) : real;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure Tmtm.SetParamReal(index : integer; Value : real);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function Tmtm.GetNom_ParamReal(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function Tmtm.GetNbInteger : integer;
begin
  result:=1;
end;

function Tmtm.GetParamInteger(index : integer) : Integer;
begin
  if index=0 then
    result:=ff
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure Tmtm.SetParamInteger(index : integer; Value : Integer);
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

function Tmtm.GetNom_ParamInteger(index : integer) : string;
begin
  if index=0 then
    result:='Fuseau'
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function Tmtm.GetNbAngle : integer;
begin
  result:=0;
end;

function Tmtm.GetParamAngle(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure Tmtm.SetParamAngle(index : integer; Value : string);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function Tmtm.GetNom_ParamAngle(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function Tmtm.GetNbBoolean : integer;
begin
  result:=1;
end;

function Tmtm.GetParamBoolean(index : integer) : Boolean;
begin
  if index=0 then
    result:=fSud
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure Tmtm.SetParamBoolean(index : integer; Value : Boolean);
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

function Tmtm.GetNom_ParamBoolean(index : integer) : string;
begin
  if index=0 then
    result:='Sud'
  else
  begin
    result:='';
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure Tmtm.ConvertirParam;
begin
  if fValid then
  try
    inherited SetParamReal(0, 0.9999*Datum.Ellipsoide.a);
    inherited SetParamReal(1, 304800);
    if fSud then
      inherited SetParamReal(2, 10000000)
    else
      inherited SetParamReal(2, 0);
    //inherited SetParamAngle(0,Format('-%d°30''',[3*ff+49]));
    inherited SetParamAngle(0,Format('-%.8fd',[(3*ff+49) + 0.5]));
  except
  end;
end;

end.
