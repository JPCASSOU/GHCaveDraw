unit projlatlong;

// cas de la projection utilisant directement les latitudes et longitudes
// présente de fortes déformations dès qu'on s'éloigne de l'équateurs
// mais utilisée par certains logiciels

// documentation de référence
//
// aucune
//
// X := longitude * rayon_terrestre
// Y := latitude * rayon_terrestre


interface

uses
  projection;

type

  TProj_LatLonj = class(TProjection)
  private
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


function TProj_LatLonj.GetNature : string;
begin
  result:=tpProjLatLong;
end;

function TProj_LatLonj.GetNbReal : integer;
begin
  result:=0;
end;

function TProj_LatLonj.GetParamReal(index : integer) : real;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TProj_LatLonj.SetParamReal(index : integer; Value : real);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TProj_LatLonj.GetNom_ParamReal(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TProj_LatLonj.GetNbInteger : integer;
begin
  result:=0;
end;

function TProj_LatLonj.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TProj_LatLonj.SetParamInteger(index : integer; Value : Integer);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TProj_LatLonj.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TProj_LatLonj.GetNbAngle : integer;
begin
  result:=0;
end;

function TProj_LatLonj.GetParamAngle(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TProj_LatLonj.SetParamAngle(index : integer; Value : string);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TProj_LatLonj.GetNom_ParamAngle(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TProj_LatLonj.GetNbBoolean : integer;
begin
  result:=0;
end;

function TProj_LatLonj.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TProj_LatLonj.SetParamBoolean(index : integer; Value : Boolean);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TProj_LatLonj.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TProj_LatLonj.VersGrille(Lat, Long : real; var X, Y : real);
begin
  X:= Datum.Ellipsoide.a * Long;
  Y:= Datum.Ellipsoide.a * Lat;
end;

procedure TProj_LatLonj.VersLatLong(X, Y : real; var Lat, Long : real);
begin
  Long:=X/Datum.Ellipsoide.a;
  Lat:=Y/Datum.Ellipsoide.a;
end;


end.
