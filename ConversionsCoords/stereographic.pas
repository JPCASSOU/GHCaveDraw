unit stereographic;

// cas générale de la projection Stéréographic

// documentation de référence
//
// IGN
// Projection cartographique stéréographique oblique
// Notes techniques NT/G 78
//

interface

uses
  projection;

type

  TStereographic = class(TProjection)
  private
    fn1 : real; // exposant de la projection
    fn2 : real; // exposant de la projection
    fc : real; // constante de la projection
    fLandaC : string; // longitude de l'origine par rapport au méridien d'origine du Datum
    fLandaCReal : real; // valeur en radian de la longitude précédente
    fPhiC : string; // latitude du point d'origine (sphère)
    fPhiCReal : real; // valeur en radian de la latitude précédente
    fXs, fYs : real; // constantes sur X et Y
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
    procedure Remonte(n1, n2, c, Xs, Ys : real; Landa0, Phi0 : string);
  end;

implementation

uses
  SysUtils,
  outils, ign;


function TStereographic.GetNature : string;
begin
  result:=tpStereographic;
end;

function TStereographic.GetNbReal : integer;
begin
  result:=5;
end;

function TStereographic.GetParamReal(index : integer) : real;
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

procedure TStereographic.SetParamReal(index : integer; Value : real);
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

function TStereographic.GetNom_ParamReal(index : integer) : string;
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

function TStereographic.GetNbInteger : integer;
begin
  result:=0;
end;

function TStereographic.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TStereographic.SetParamInteger(index : integer; Value : Integer);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TStereographic.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TStereographic.GetNbAngle : integer;
begin
  result:=2;
end;

function TStereographic.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fLandaC;
    1 : result:=fPhiC;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TStereographic.SetParamAngle(index : integer; Value : string);
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
    1 : begin
      try
        fPhiC:=Value;
        fPhiCreal:=StrToAngle(Value);
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

function TStereographic.GetNom_ParamAngle(index : integer) : string;
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

function TStereographic.GetNbBoolean : integer;
begin
  result:=0;
end;

function TStereographic.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TStereographic.SetParamBoolean(index : integer; Value : Boolean);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TStereographic.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;




procedure TStereographic.VersGrille(Lat, Long : real; var X, Y : real);
begin
  alg0038(fLandaCReal, fPhiCReal, fc, fn1, fn2, fXs, fYs, Datum.Ellipsoide.e,
          Long, Lat, X, Y);
end;

procedure TStereographic.VersLatLong(X, Y : real; var Lat, Long : real);
begin
  alg0039(fLandaCReal, fPhiCReal, fc, fn1, fn2, fXs, fYs, Datum.Ellipsoide.e,
          X, Y, Long, Lat);
end;

procedure TStereographic.Remonte(n1, n2, c, Xs, Ys : real; Landa0, Phi0 : string);
// remonte les paramètres calculés par les descandants dans l'ancètre TLaborde
begin
  fn1:=n1;
  fn2:=n2;
  fc:=c;
  fXs:=Xs;
  fYs:=Ys;
  fLandaC:=Landa0;
  fLandaCReal:=StrToAngle(fLandaC);
  fPhiC:=Phi0;
  fPhiCReal:=StrToAngle(fPhiC);
end;

end.
