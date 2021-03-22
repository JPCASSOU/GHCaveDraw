unit transmercator_brut;

// cas générale de la projection Transverse Mercator

// documentation de référence
//
// IGN
// Projection cartographique Mercator transverse
// Notes techniques NT/G 76
//
// http://professionnels.ign.fr/DISPLAY/000/526/702/5267021/NTG_76.pdf

interface

uses
  projection;

type

  TTransverseMercatorBrut = class(TProjection)
  private
    fn : real; // rayon de la sphère intermédiaire
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

    procedure SetValid(const Value: boolean); override;
    function GetNature : string; override;

    procedure ConvertirParam;  virtual; // convertit les paramètres vers la projection TTransmercator_brut (générique)
  public
    procedure VersGrille(Lat, Long : real; var X, Y : real); override;
    procedure VersLatLong(X, Y : real; var Lat, Long : real); override;
  end;

implementation

uses
  SysUtils,
  outils, ign;


function TTransverseMercatorBrut.GetNature : string;
begin
  result:=tpTransverseMercatorBrut;
end;

function TTransverseMercatorBrut.GetNbReal : integer;
begin
  result:=3;
end;

function TTransverseMercatorBrut.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fn;
    1 : result:=fXs;
    2 : result:=fYs;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TTransverseMercatorBrut.SetParamReal(index : integer; Value : real);
begin
  case Index of
    0 : fn:=Value;
    1 : fXs:=Value;
    2 : fYs:=Value;
  else
    begin
      fValid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;
end;

function TTransverseMercatorBrut.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='n';
    1 : result:='X0';
    2 : result:='Y0';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TTransverseMercatorBrut.GetNbInteger : integer;
begin
  result:=0;
end;

function TTransverseMercatorBrut.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TTransverseMercatorBrut.SetParamInteger(index : integer; Value : Integer);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TTransverseMercatorBrut.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TTransverseMercatorBrut.GetNbAngle : integer;
begin
  result:=1;
end;

function TTransverseMercatorBrut.GetParamAngle(index : integer) : string;
begin
  if index=0 then
    result:=fLandaC
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TTransverseMercatorBrut.SetParamAngle(index : integer; Value : string);
begin
  if index=0 then
  begin
    try
      fLandaC:=Value;
      fLandaCreal:=StrToAngle(Value);
    except
      fValid:=false;
      Raise;
    end;
  end
  else begin
    fValid:=false;
    raise ERangeError.Create('Ecriture de paramètre hors-limites');
  end;
end;

function TTransverseMercatorBrut.GetNom_ParamAngle(index : integer) : string;
begin
  if index=0 then
    result:='LandaC'
  else begin
    result:='';
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TTransverseMercatorBrut.GetNbBoolean : integer;
begin
  result:=0;
end;

function TTransverseMercatorBrut.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TTransverseMercatorBrut.SetParamBoolean(index : integer; Value : Boolean);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TTransverseMercatorBrut.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;




procedure TTransverseMercatorBrut.VersGrille(Lat, Long : real; var X, Y : real);
begin
  alg0030(fLandaCReal, fn, fXs, fYs, Datum.Ellipsoide.e, Long, Lat, X, Y);
end;

procedure TTransverseMercatorBrut.VersLatLong(X, Y : real; var Lat, Long : real);
begin
  alg0031(fLandaCReal,fn, fXs, fYs, Datum.Ellipsoide.e, X, Y, Long, Lat);
end;

procedure TTransverseMercatorBrut.ConvertirParam;
begin
// rien, c'est juste pour l'héritage
end;

procedure TTransverseMercatorBrut.SetValid(const Value: boolean);
begin
  inherited;
  ConvertirParam;
end;


end.
