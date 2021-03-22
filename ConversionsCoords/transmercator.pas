unit transmercator;

// cas de la projection Transverse Mercator avec paramètres classiques

// documentation de référence
//
// IGN
// Projection cartographique Mercator transverse
// Notes techniques NT/G 76
//
// http://professionnels.ign.fr/DISPLAY/000/526/702/5267021/NTG_76.pdf

interface

uses
  projection, transmercator_brut;

type

  TTransverseMercator = class(TTransverseMercatorBrut)
  private
    fLanda0 : string; // longitude de l'origine par rapport au méridien d'origine du Datum
    fLanda0Real : real; // valeur en radian de la longitude précédente
    fPhi0 : string; // latitude de l'origine
    fPhi0Real : real; // valeur en radian de la latitude précédente
    fk0 : real; // facteur d'échelle au point d'origine
    fX0, fY0 : real; // coordonnées en projection du point d'origine
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


function TTransverseMercator.GetNature : string;
begin
  result:=tpTransverseMercator;
end;

function TTransverseMercator.GetNbReal : integer;
begin
  result:=3;
end;

function TTransverseMercator.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fk0;
    1 : result:=fX0;
    2 : result:=fY0;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TTransverseMercator.SetParamReal(index : integer; Value : real);
begin
  case Index of
    0 : fk0:=Value;
    1 : fX0:=Value;
    2 : fY0:=Value;
  else
    begin
      fValid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;
  ConvertirParam;
end;

function TTransverseMercator.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='k0';
    1 : result:='X0';
    2 : result:='Y0';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TTransverseMercator.GetNbInteger : integer;
begin
  result:=0;
end;

function TTransverseMercator.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TTransverseMercator.SetParamInteger(index : integer; Value : Integer);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TTransverseMercator.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TTransverseMercator.GetNbAngle : integer;
begin
  result:=2;
end;

function TTransverseMercator.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fLanda0;
    1 : result:=fPhi0;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;  
end;

procedure TTransverseMercator.SetParamAngle(index : integer; Value : string);
begin
  case index of
    0 : begin
      try
        fLanda0:=Value;
        fLanda0real:=StrToAngle(Value);
        ConvertirParam;
      except
        fValid:=false;
        Raise;
      end;
    end;
    1 : begin
      try
        fPhi0:=Value;
        fPhi0real:=StrToAngle(Value);
        ConvertirParam;
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

function TTransverseMercator.GetNom_ParamAngle(index : integer) : string;
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

function TTransverseMercator.GetNbBoolean : integer;
begin
  result:=0;
end;

function TTransverseMercator.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TTransverseMercator.SetParamBoolean(index : integer; Value : Boolean);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TTransverseMercator.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TTransverseMercator.ConvertirParam;
var
  n, Ys : real;
begin
  if fValid then
  try
    alg0052(Datum.Ellipsoide.a, Datum.Ellipsoide.e, fk0, fPhi0real, fY0,
            n, Ys);
    inherited SetParamReal(0, n);
    inherited SetParamReal(1, fX0);
    inherited SetParamReal(2, Ys);
    inherited SetParamAngle(0,fLanda0);
  except
  end;
end;

end.
