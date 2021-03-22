unit lambert_oblic;

// cas de la projection Lambert Conique Oblique
// Tchécoslovaquie

// documentation de référence
//
//
// http://sas2.elte.hu/tg/krovakpaper_en.htm
// http://remotesensing.org/lists/proj_archive/msg00306.html
// http://www.posc.org/Epicentre.2_2/DataModel/ExamplesofUsage/eu_cs34e2.html
// http://homel.vsb.cz/~hom50/GEOINFRM/KROVAK/KROVAK.HTM
// http://krovak.webpark.cz/e_version/index_e_index.htm

interface

uses
  projection;

type

  TLambert_oblic = class(TProjection)
  private
    fLandaC : string; // longitude du centre de la projection
    fLandaCReal : real; // valeur en radian de la longitude précédente
    fPhiC : string; // latitude du centre de la projection
    fPhiCReal : real; // valeur en radian de la latitude précédente
    fPhi1 : string; // latitude du parallèle pseudo-standard
    fPhi1Real : real; // valeur en radian de la latitude précédente
    fAlphaC : string; // azimuth de la ligne centrale passant par le centre de la projection
    fAlphaCReal : real; // valeur en radian de l'azimuth précédent
    fk0 : real; // facteur d'échelle à l'origine
    fX0, fY0 : real; // coordonnées en projection du point d'origine
//    procedure ConvertirParam;  // convertit les paramètres vers la projection TLambert (générique)
    procedure Prepare(var B, A, g0, t0, n, r0 : real);
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
//    procedure SetValid(const Value: boolean);  override;
  public
    procedure VersGrille(Lat, Long : real; var X, Y : real); override;
    procedure VersLatLong(X, Y : real; var Lat, Long : real); override;
  end;

implementation

uses
  SysUtils,
  outils, math;


function TLambert_oblic.GetNature : string;
begin
  result:=tpLambertConiqueOblique;
end;

function TLambert_oblic.GetNbReal : integer;
begin
  result:=3;
end;

function TLambert_oblic.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fk0;
    1 : result:=fX0;
    2 : result:=fY0;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TLambert_oblic.SetParamReal(index : integer; Value : real);
begin
  case Index of
    0 : fk0:=Value;
    1 : fX0:=Value;
    2 : fY0:=Value;
  else
    begin
      Valid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;
end;

function TLambert_oblic.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='k0';
    1 : result:='X0';
    2 : result:='Y0';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TLambert_oblic.GetNbInteger : integer;
begin
  result:=0;
end;

function TLambert_oblic.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLambert_oblic.SetParamInteger(index : integer; Value : Integer);
begin
  Valid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TLambert_oblic.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TLambert_oblic.GetNbAngle : integer;
begin
  result:=4;
end;

function TLambert_oblic.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fLandaC;
    1 : result:=fPhiC;
    2 : result:=fPhi1;
    3 : result:=fAlphaC;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TLambert_oblic.SetParamAngle(index : integer; Value : string);
begin
  case index of
    0 : begin
      try
        fLandaC:=Value;
        fLandaCreal:=StrToAngle(Value);
      except
        Valid:=false;
        Raise;
      end;
    end;
    1 : begin
      try
        fPhiC:=Value;
        fPhiCreal:=StrToAngle(Value);
      except
        Valid:=false;
        Raise;
      end;
    end;
    2 : begin
      try
        fPhi1:=Value;
        fPhi1real:=StrToAngle(Value);
      except
        Valid:=false;
        Raise;
      end;
    end;
    3 : begin
      try
        fAlphaC:=Value;
        fAlphaCreal:=StrToAngle(Value);
      except
        Valid:=false;
        Raise;
      end;
    end;
  else
    begin
      Valid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;
end;

function TLambert_oblic.GetNom_ParamAngle(index : integer) : string;
begin
  case index of
    0 : result:='LandaC';
    1 : result:='PhiC';
    2 : result:='Phi1';
    3 : result:='AlphaC';
  else
    begin
      result:='';
      raise ERangeError.Create('Demande de paramètre hors-limites');
    end;
  end;
end;

function TLambert_oblic.GetNbBoolean : integer;
begin
  result:=0;
end;

function TLambert_oblic.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLambert_oblic.SetParamBoolean(index : integer; Value : Boolean);
begin
  Valid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TLambert_oblic.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;
{
procedure TLambert_oblic.ConvertirParam;
var
  n, c, Ys : real;
begin
  if fValid then
  try
    alg0019(Datum.Ellipsoide.a, Datum.Ellipsoide.e, fPhi0real, fk0, fY0,
            n, c, Ys);
    inherited SetParamReal(0, n);
    inherited SetParamReal(1, c);
    inherited SetParamReal(2, fX0);
    inherited SetParamReal(3, Ys);
    inherited SetParamAngle(0,fLanda0);
  except
  end;
end;

procedure TLambert_oblic.SetValid(const Value: boolean);
begin
  inherited;
  ConvertirParam;
end;
 }

procedure TLambert_oblic.Prepare(var B, A, g0, t0, n, r0: real);
begin
  B:=sqrt(1+sqr(Datum.Ellipsoide.e)*IntPower(cos(fPhiCReal),4)/(1-sqr(Datum.Ellipsoide.e)));
  A:=Datum.Ellipsoide.a*sqrt(1-sqr(Datum.Ellipsoide.e))/(1-sqr(sin(fPhiCReal)*Datum.Ellipsoide.e));
  g0:=arcsin(sin(fPhiCReal)/B);
  t0:=tan(Pi/4+fLandaCReal/2)*
     power((1+Datum.Ellipsoide.e*sin(fPhiCReal))/(1-Datum.Ellipsoide.e*sin(fPhiCReal)),Datum.Ellipsoide.e*B/2)/
     power(tan(Pi/4+fPhiCReal/2),B);
{  t0:=tan(Pi/4+fLandaCReal/2)*
     power((1+sqr(Datum.Ellipsoide.e)*sin(fPhiCReal))/(1-sqr(Datum.Ellipsoide.e)*sin(fPhiCReal)),Datum.Ellipsoide.e*B/2)/
     power(tan(Pi/4+fPhiCReal/2),B); }
  n:=sin(fPhi1Real);
  r0:=fK0*A/tan(fPhi1Real);
end;

procedure TLambert_oblic.VersGrille(Lat, Long : real; var X, Y : real);
var
  A, B, g0, t0, n, r0 : real;
  U, V, S, D, theta, r : real;
begin
  Prepare(B, A, g0, t0, n, r0);
  U:=2*(arctan(fK0*power(tan(Pi/4+Lat/2),B)*
     power((1+Datum.Ellipsoide.e*sin(Lat))/(1-Datum.Ellipsoide.e*sin(Lat)),Datum.Ellipsoide.e*B/2))
     -Pi/4);
  V:=B*(fLandaCReal-Long);
  S:=arcsin(cos(fAlphaCReal)*sin(U)+sin(fAlphaCReal)*cos(U)*cos(V));
  D:=arcsin(cos(U)*sin(V)/cos(S));
  theta:=n*D;
  r:=r0*tan(Pi/4+g0/2)/power(tan(S/2+Pi/4),n);

  X:=fX0+r*cos(theta);
  Y:=fY0+r*sin(theta);

end;

procedure TLambert_oblic.VersLatLong(X, Y : real; var Lat, Long : real);
begin
//  alg0004(X, Y, fn, fc, Datum.Ellipsoide.e,fLandaCReal,fXs, fYs, Long, Lat);
end;

end.
