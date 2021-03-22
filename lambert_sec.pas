unit lambert_sec;

// cas de la projection Lambert sécante
// définie par un parallèle d'origine et deux parallèles automécoïdes

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

  TLambert_sec = class(TLambert)
  private
    fLanda0 : string; // longitude de l'origine par rapport au méridien d'origine du Datum
    fLanda0Real : real; // valeur en radian de la longitude précédente
    fPhi0 : string; // latitude de l'origine
    fPhi0Real : real; // valeur en radian de la latitude précédente
    fPhi1, fPhi2 : string; // latitude des parallèles automécoïdes
    fPhi1Real, fPhi2Real : real; // valeur en radian des latitudes
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
    procedure SetValid(const Value: boolean); override;
  public
  end;

implementation

uses
  SysUtils,
  outils, ign;


function TLambert_sec.GetNature : string;
begin
  result:=tpLambertConiqueSecant;
end;

function TLambert_sec.GetNbReal : integer;
begin
  result:=2;
end;

function TLambert_sec.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fX0;
    1 : result:=fY0;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TLambert_sec.SetParamReal(index : integer; Value : real);
begin
  case Index of
    0 : fX0:=Value;
    1 : fY0:=Value;
  else
    begin
      Valid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;
  ConvertirParam;
end;

function TLambert_sec.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='X0';
    1 : result:='Y0';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TLambert_sec.GetNbInteger : integer;
begin
  result:=0;
end;

function TLambert_sec.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLambert_sec.SetParamInteger(index : integer; Value : Integer);
begin
  Valid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TLambert_sec.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TLambert_sec.GetNbAngle : integer;
begin
  result:=4;
end;

function TLambert_sec.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fLanda0;
    1 : result:=fPhi0;
    2 : result:=fPhi1;
    3 : result:=fPhi2;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TLambert_sec.SetParamAngle(index : integer; Value : string);
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
    2 : begin
      try
        fPhi1:=Value;
        fPhi1real:=StrToAngle(Value);
        ConvertirParam;
      except
        Valid:=false;
        Raise;
      end;
    end;
    3 : begin
      try
        fPhi2:=Value;
        fPhi2real:=StrToAngle(Value);
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

function TLambert_sec.GetNom_ParamAngle(index : integer) : string;
begin
  case index of
    0 : result:='Landa0';
    1 : result:='Phi0';
    2 : result:='Phi1';
    3 : result:='Phi2';
  else
    begin
      result:='';
      raise ERangeError.Create('Demande de paramètre hors-limites');
    end;
  end;  
end;

function TLambert_sec.GetNbBoolean : integer;
begin
  result:=0;
end;

function TLambert_sec.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLambert_sec.SetParamBoolean(index : integer; Value : Boolean);
begin
  Valid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TLambert_sec.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TLambert_sec.ConvertirParam;
var
  n, c, Ys : real;
begin
  if fValid then
  try
    alg0054(Datum.Ellipsoide.a, Datum.Ellipsoide.e,
            fPhi0Real, fPhi1Real, fPhi2Real, fY0,
            n, c, Ys);
    inherited SetParamReal(0, n);
    inherited SetParamReal(1, c);
    inherited SetParamReal(2, fX0);
    inherited SetParamReal(3, Ys);
    inherited SetParamAngle(0,fLanda0);
  except
  end;
end;

procedure TLambert_sec.SetValid(const Value: boolean);
begin
  inherited;
  ConvertirParam;
end;

end.
