unit rosenmund;

// cas générale de la projection de Gauss-Rosenmund
// principalement utilisée en Suisse

// documentation de référence
//
// Office fédérale de topographie
// Formules et constantes pour le calcul pour la projection cylindrique
// à axe transformation entre des systèmes de référence
//
// http://www.swisstopo.admin.ch/internet/swisstopo/fr/home/topics/survey/sys/refsys/switzerland.parsysrelated1.31216.downloadList.63873.DownloadFile.tmp/swissprojectionfr.pdf

interface

uses
  projection;

type

  TRosenmund = class(TProjection)
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
  outils, swisstopo, ign;


function TRosenmund.GetNature : string;
begin
  result:=tpGaussRosenmund;
end;

function TRosenmund.GetNbReal : integer;
begin
  result:=2;
end;

function TRosenmund.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fXs;
    1 : result:=fYs;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TRosenmund.SetParamReal(index : integer; Value : real);
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

function TRosenmund.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='X0';
    1 : result:='Y0';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TRosenmund.GetNbInteger : integer;
begin
  result:=0;
end;

function TRosenmund.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TRosenmund.SetParamInteger(index : integer; Value : Integer);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TRosenmund.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TRosenmund.GetNbAngle : integer;
begin
  result:=2;
end;

function TRosenmund.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fLanda0;
    1 : result:=fPhi0;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;  
end;

procedure TRosenmund.SetParamAngle(index : integer; Value : string);
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

function TRosenmund.GetNom_ParamAngle(index : integer) : string;
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

function TRosenmund.GetNbBoolean : integer;
begin
  result:=0;
end;

function TRosenmund.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TRosenmund.SetParamBoolean(index : integer; Value : Boolean);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TRosenmund.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;




procedure TRosenmund.VersGrille(Lat, Long : real; var X, Y : real);
var
  R, alpha, b0, K : real;
  S, b, l : real;
  lbar, bbar : real;
begin
  R:=swiss03(Datum.Ellipsoide.a, Datum.Ellipsoide.e, fPhi0Real);
  Alpha:=swiss04(Datum.Ellipsoide.e, fPhi0Real);
  b0:=arcsin(sin(fPhi0Real)/Alpha);
  K:=swiss01(b0)-Alpha*swiss01(fPhi0Real)+swiss02(Datum.Ellipsoide.e, Alpha, fPhi0Real);

  S:=-Alpha*swiss01(-Lat)-swiss02(Datum.Ellipsoide.e, Alpha, Lat) + K;
  b:=2*(arctan(exp(S))-Pi/4);
  l:=Alpha*(Long-fLanda0Real);

  lbar:=arctan(sin(l)/(sin(b0)*tan(b)+cos(b0)*cos(l)));
  bbar:=arcsin(cos(b0)*sin(b)-sin(b0)*cos(b)*cos(l));

  X:=R*lbar + fXs;
  Y:=R/2*ln((1+sin(bbar))/(1-sin(bbar))) + fYs;

end;

procedure TRosenmund.VersLatLong(X, Y : real; var Lat, Long : real);
var
  R, alpha, b0, K : real;
  S, b, l : real;
  lbar, bbar : real;
  PhiN, PhiN1 : real;
begin
  R:=swiss03(Datum.Ellipsoide.a, Datum.Ellipsoide.e, fPhi0Real);
  Alpha:=swiss04(Datum.Ellipsoide.e, fPhi0Real);
  b0:=arcsin(sin(fPhi0Real)/Alpha);
  K:=swiss01(b0)-Alpha*swiss01(fPhi0Real)+swiss02(Datum.Ellipsoide.e, Alpha, fPhi0Real);

  X:=X-fXs;
  Y:=Y-fYs;
  lbar:=X/R;
  bbar:=2*(arctan(exp(Y/R))-Pi/4);

  b:=arcsin(cos(b0)*sin(bbar)+sin(b0)*cos(bbar)*cos(lbar));
  l:=arctan(sin(lbar)/(cos(b0)*cos(lbar)-sin(b0)*tan(bbar)));

  Long:=fLanda0Real+l/Alpha;
  PhiN1:=b;
  repeat
    PhiN:=PhiN1;
    S:=1/Alpha*(swiss01(b)-K)+Datum.Ellipsoide.e*swiss01(arcsin(Datum.Ellipsoide.e*sin(PhiN)));
    PhiN1:=2*arctan(exp(S))-Pi/2;
  until abs(PhiN-PhiN1)<Eps;
  Lat:=PhiN1;
end;

end.
