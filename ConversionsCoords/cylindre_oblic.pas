unit cylindre_oblic;

// cas des projection cylindrique oblique d�finies par un point et une direction
// d�finition d'un anc�tre commun aux diff�rentes projections
// Laborde, Hotine et Mercator Oblique

interface

uses
  projection;

type

  TCylindreOblic = class(TProjection)
  private
  protected
    fLanda0 : string; // longitude de l'origine par rapport au m�ridien d'origine du Datum
    fLanda0Real : real; // valeur en radian de la longitude pr�c�dente
    fPhi0 : string; // latitude de l'origine
    fPhi0Real : real; // valeur en radian de la latitude pr�c�dente
    fTheta0 : string; // obliquit� de l'indicatrice
    fTheta0Real : real; // valeur en radian de l'obliquit� pr�c�dente
    fk0 : real; // facteur d'�chelle � l'origine
    fX0, fY0 : real; // coordonn�es en projection du point d'origine
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
    constructor Create; override;
  end;


implementation

uses
  SysUtils,
  outils;

constructor TCylindreOblic.Create;
begin
  inherited Create;
end;

function TCylindreOblic.GetNature : string;
begin
  result:=tpCylindreOblique;
end;

function TCylindreOblic.GetNbReal : integer;
begin
  result:=3;
end;

function TCylindreOblic.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fk0;
    1 : result:=fX0;
    2 : result:=fY0;
  else
    raise ERangeError.Create('Demande de param�tre hors-limites');
  end;
end;

procedure TCylindreOblic.SetParamReal(index : integer; Value : real);
begin
  case Index of
    0 : fk0:=Value;
    1 : fX0:=Value;
    2 : fY0:=Value;
  else
    begin
      fValid:=false;
      raise ERangeError.Create('Ecriture de param�tre hors-limites');
    end;
  end;
end;

function TCylindreOblic.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='k0';
    1 : result:='X0';
    2 : result:='Y0';
  else
    raise ERangeError.Create('Demande de param�tre hors-limites');
  end;
end;

function TCylindreOblic.GetNbInteger : integer;
begin
  result:=0;
end;

function TCylindreOblic.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de param�tre hors-limites');
end;

procedure TCylindreOblic.SetParamInteger(index : integer; Value : Integer);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de param�tre hors-limites');
end;

function TCylindreOblic.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de param�tre hors-limites');
end;

function TCylindreOblic.GetNbAngle : integer;
begin
  result:=3;
end;

function TCylindreOblic.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fLanda0;
    1 : result:=fPhi0;
    2 : result:=fTheta0;
  else
    raise ERangeError.Create('Demande de param�tre hors-limites');
  end;  
end;

procedure TCylindreOblic.SetParamAngle(index : integer; Value : string);
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
    2 : begin
      try
        fTheta0:=Value;
        fTheta0real:=StrToAngle(Value);
      except
        fValid:=false;
        Raise;
      end;
    end;
  else
    begin
      fValid:=false;
      raise ERangeError.Create('Ecriture de param�tre hors-limites');
    end;
  end;
end;

function TCylindreOblic.GetNom_ParamAngle(index : integer) : string;
begin
  case index of
    0 : result:='Landa0';
    1 : result:='Phi0';
    2 : result:='Theta0';
  else
    begin
      result:='';
      raise ERangeError.Create('Demande de param�tre hors-limites');
    end;
  end;
end;

function TCylindreOblic.GetNbBoolean : integer;
begin
  result:=0;
end;

function TCylindreOblic.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de param�tre hors-limites');
end;

procedure TCylindreOblic.SetParamBoolean(index : integer; Value : Boolean);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de param�tre hors-limites');
end;

function TCylindreOblic.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de param�tre hors-limites');
end;

end.
