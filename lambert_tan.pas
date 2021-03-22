unit lambert_tan;

// cas de la projection Lambert tangente
// définie par un parallèle tangent et un facteur d'échelle

// documentation de référence
//
// IGN
// Projection cartographique conique conforme de Lambert
// Notes techniques NT/G 71
//
// http://professionnels.ign.fr/DISPLAY/000/526/701/5267019/NTG_71.pdf

interface

uses
  projection, lambert;

type

  TLambert_tan = class(TLambert)
  private
    fLanda0 : string; // longitude de l'origine par rapport au méridien d'origine du Datum
    fLanda0Real : real; // valeur en radian de la longitude précédente
    fPhi0 : string; // latitude de l'origine 
    fPhi0Real : real; // valeur en radian de la latitude précédente
    fk0 : real; // facteur d'échelle à l'origine
    fX0, fY0 : real; // coordonnées en projection du point d'origine
    procedure ConvertirParam;  // convertit les paramètres vers la projection TLambert (générique)
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
    procedure SetValid(const Value: boolean);  override;
  public
  end;

implementation

uses
  SysUtils,
  outils, ign;


function TLambert_tan.GetNature : string;
begin
  result:=tpLambertConiqueTangent;
end;

function TLambert_tan.GetNbReal : integer;
begin
  result:=3;
end;

function TLambert_tan.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fk0;
    1 : result:=fX0;
    2 : result:=fY0;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TLambert_tan.SetParamReal(index : integer; Value : real);
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
  ConvertirParam;
end;

function TLambert_tan.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='k0';
    1 : result:='X0';
    2 : result:='Y0';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TLambert_tan.GetNbInteger : integer;
begin
  result:=0;
end;

function TLambert_tan.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLambert_tan.SetParamInteger(index : integer; Value : Integer);
begin
  Valid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TLambert_tan.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TLambert_tan.GetNbAngle : integer;
begin
  result:=2;
end;

function TLambert_tan.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fLanda0;
    1 : result:=fPhi0;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;  
end;

procedure TLambert_tan.SetParamAngle(index : integer; Value : string);
begin
  case index of
    0 : begin
      try
        fLanda0:=Value;
        fLanda0real:=StrToAngle(Value);
        ConvertirParam;
      except
        Valid:=false;
        Raise;
      end;
    end;
    1 : begin
      try
        fPhi0:=Value;
        fPhi0real:=StrToAngle(Value);
        ConvertirParam;
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

function TLambert_tan.GetNom_ParamAngle(index : integer) : string;
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

function TLambert_tan.GetNbBoolean : integer;
begin
  result:=0;
end;

function TLambert_tan.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLambert_tan.SetParamBoolean(index : integer; Value : Boolean);
begin
  Valid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TLambert_tan.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLambert_tan.ConvertirParam;
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

procedure TLambert_tan.SetValid(const Value: boolean);
begin
  inherited;
  ConvertirParam;
end;


end.
