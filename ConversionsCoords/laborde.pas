unit laborde;

// cas générale de la projection Gauss-Laborde

// documentation de référence
//
// IGN
// Projection cartographique Gauss-Laborde
// Notes techniques NT/G 73
//
// http://www.ign.fr/telechargement/MPro/geodesie/CIRCE/NTG_73.pdf

interface

uses
  projection;

type

  TLaborde = class(TProjection)
  protected
    fn1 : real; // exposant de la projection ellipsoïde-sphère
    fn2 : real; // rayon de la sphère intermédiaire
    fc : real; // constante de la projection
    fLandaC : string; // longitude de l'origine par rapport au méridien d'origine du Datum
    fLandaCReal : real; // valeur en radian de la longitude précédente
    fXs, fYs : real; // coordonnées en projection du pôle (false Est et false Nord)
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
    procedure Remonte(n1, n2, c, Xs, Ys : real; Landa0 : string); // remonte les paramètres calculés par les descandants dans l'ancètre TLaborde
  public
    procedure VersGrille(Lat, Long : real; var X, Y : real); override;
    procedure VersLatLong(X, Y : real; var Lat, Long : real); override;
  end;

implementation

uses
  SysUtils,
  outils, ign;


function TLaborde.GetNature : string;
begin
  result:=tpGaussLaborde;
end;

function TLaborde.GetNbReal : integer;
begin
  result:=5;
end;

function TLaborde.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fn1;
    1 : result:=fn2;
    2 : result:=fc;
    3 : result:=fXs;
    4 : result:=fYs;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TLaborde.SetParamReal(index : integer; Value : real);
begin
  case Index of
    0 : fn1:=Value;
    1 : fn2:=Value;
    2 : fc:=Value;
    3 : fXs:=Value;
    4 : fYs:=Value;
  else
    begin
      fValid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;
end;

function TLaborde.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='n1';
    1 : result:='n2';
    2 : result:='c';
    3 : result:='X0';
    4 : result:='Y0';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TLaborde.GetNbInteger : integer;
begin
  result:=0;
end;

function TLaborde.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLaborde.SetParamInteger(index : integer; Value : Integer);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TLaborde.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TLaborde.GetNbAngle : integer;
begin
  result:=1;
end;

function TLaborde.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fLandaC;
//    1 : result:=fPhiC;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TLaborde.SetParamAngle(index : integer; Value : string);
begin
  case index of
    0 : begin
      try
        fLandaC:=Value;
        fLandaCreal:=StrToAngle(Value);
      except
        fValid:=false;
        Raise;
      end;
    end;
{    1 : begin
      try
        fPhiC:=Value;
        fPhiCreal:=StrToAngle(Value);
      except
        fValid:=false;
        Raise;
      end;
    end; }
  else
    begin
      fValid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;
end;

function TLaborde.GetNom_ParamAngle(index : integer) : string;
begin
  case index of
    0 : result:='LandaC';
    1 : result:='PhiC';
  else
    begin
      result:='';
      raise ERangeError.Create('Demande de paramètre hors-limites');
    end;
  end;
end;

function TLaborde.GetNbBoolean : integer;
begin
  result:=0;
end;

function TLaborde.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLaborde.SetParamBoolean(index : integer; Value : Boolean);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TLaborde.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;




procedure TLaborde.VersGrille(Lat, Long : real; var X, Y : real);
begin
  alg0034(Long,Lat,Datum.Ellipsoide.e,fLandaCReal, fn1, fn2, fXs, fYs, fc, X, Y);
end;

procedure TLaborde.VersLatLong(X, Y : real; var Lat, Long : real);
begin
  alg0035(X, Y, Datum.Ellipsoide.e, fLandaCReal, fn1, fn2, fXs, fYs, fc, Long, Lat);
end;

procedure TLaborde.Remonte(n1, n2, c, Xs, Ys : real; Landa0 : string);
// remonte les paramètres calculés par les descandants dans l'ancètre TLaborde
begin
  fn1:=n1;
  fn2:=n2;
  fc:=c;
  fXs:=Xs;
  fYs:=Ys;
  fLandaC:=Landa0;
  fLandaCReal:=StrToAngle(fLandaC);
end;

end.
