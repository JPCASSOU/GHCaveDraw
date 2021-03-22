unit bonne;

// cas de la projection de Bonne
// utilisée en particulier par les cartes Michelin

// documentation de référence
//
//
// http://www.ac-strasbourg.fr/microsites/hist_geo01/localisation/Projections/formulaire.htm
//
// Note 1 : bien que non spécifié, j'ai rajouté un false easting et un false northing
//
// Note 2 : non testée faute d'exemple

interface

uses
  projection;

type

  TBonne = class(TProjection)
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
  outils;


function TBonne.GetNature : string;
begin
  result:=tpBonne;
end;

function TBonne.GetNbReal : integer;
begin
  result:=2;
end;

function TBonne.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fXs;
    1 : result:=fYs;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TBonne.SetParamReal(index : integer; Value : real);
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

function TBonne.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='X0';
    1 : result:='Y0';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TBonne.GetNbInteger : integer;
begin
  result:=0;
end;

function TBonne.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TBonne.SetParamInteger(index : integer; Value : Integer);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TBonne.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TBonne.GetNbAngle : integer;
begin
  result:=2;
end;

function TBonne.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fLanda0;
    1 : result:=fPhi0;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;  
end;

procedure TBonne.SetParamAngle(index : integer; Value : string);
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

function TBonne.GetNom_ParamAngle(index : integer) : string;
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

function TBonne.GetNbBoolean : integer;
begin
  result:=0;
end;

function TBonne.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TBonne.SetParamBoolean(index : integer; Value : Boolean);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TBonne.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TBonne.VersGrille(Lat, Long : real; var X, Y : real);
var
  ro, tetha : real;
begin
  ro:=cot(fPhi0real)+fPhi0real-Lat;
  tetha:=cos(Lat)*(Long-fLanda0real)/ro;
  X:=fXs+ro*sin(Tetha)*Datum.Ellipsoide.a;
  Y:=fYs-ro*cos(Tetha)*Datum.Ellipsoide.a;
end;

procedure TBonne.VersLatLong(X, Y : real; var Lat, Long : real);
var
  ro, tetha : real;
begin
  X:=(X-fXs)/Datum.Ellipsoide.a;
  Y:=-(Y-fYs)/Datum.Ellipsoide.a;
  ro:=sqrt(sqr(X)+sqr(Y));
  Tetha:=arctan2(X,Y);
  Lat:=cot(fPhi0real)+fPhi0real-ro;
  Long:=fLanda0real+Tetha*ro/cos(Lat);
end;

end.
