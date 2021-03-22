unit lambert;
{$mode delphi}{$H+}

// cas générale de la projection Lambert

// documentation de référence
//
// IGN
// Projection cartographique conique conforme de Lambert
// Notes techniques NT/G 71
//
// http://professionnels.ign.fr/DISPLAY/000/526/701/5267019/NTG_71.pdf

interface

uses
  projection;

type

  TLambert = class(TProjection)
  private
    fn : real; // exposant de la projection
    fc : real; // constante de la projection
    fLandaC : string; // longitude de l'origine par rapport au méridien d'origine du Datum
    fLandaCReal : real; // valeur en radian de la longitude précédente
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
  SysUtils,
  outils, ign;


function TLambert.GetNature : string;
begin
  result:=tpLambertConique;
end;  

function TLambert.GetNbReal : integer;
begin
  result:=4;
end;

function TLambert.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fn;
    1 : result:=fc;
    2 : result:=fXs;
    3 : result:=fYs;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TLambert.SetParamReal(index : integer; Value : real);
begin
  case Index of
    0 : fn:=Value;
    1 : fc:=Value;
    2 : fXs:=Value;
    3 : fYs:=Value;
  else
    begin
      Valid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;
end;

function TLambert.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='n';
    1 : result:='c';
    2 : result:='X0';
    3 : result:='Y0';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TLambert.GetNbInteger : integer;
begin
  result:=0;
end;

function TLambert.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLambert.SetParamInteger(index : integer; Value : Integer);
begin
  Valid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TLambert.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TLambert.GetNbAngle : integer;
begin
  result:=1;
end;

function TLambert.GetParamAngle(index : integer) : string;
begin
  if index=0 then
    result:=fLandaC
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLambert.SetParamAngle(index : integer; Value : string);
begin
  if index=0 then
  begin
    try
      fLandaC:=Value;
      fLandaCreal:=StrToAngle(Value);
    except
      Valid:=false;
      Raise;
    end;
  end
  else begin
    Valid:=false;
    raise ERangeError.Create('Ecriture de paramètre hors-limites');
  end;
end;

function TLambert.GetNom_ParamAngle(index : integer) : string;
begin
  if index=0 then
    result:='LandaC'
  else begin
    result:='';
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TLambert.GetNbBoolean : integer;
begin
  result:=0;
end;

function TLambert.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLambert.SetParamBoolean(index : integer; Value : Boolean);
begin
  Valid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TLambert.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;




procedure TLambert.VersGrille(Lat, Long : real; var X, Y : real);
begin
  alg0003(Long,Lat,fn,fc,Datum.Ellipsoide.e,fLandaCReal,fXs, fYs,X,Y);
end;

procedure TLambert.VersLatLong(X, Y : real; var Lat, Long : real);
begin
  alg0004(X, Y, fn, fc, Datum.Ellipsoide.e,fLandaCReal,fXs, fYs, Long, Lat);
end;

end.
