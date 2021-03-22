unit stereo_polaire;

// cas de la projection Stereographique polaire

// documentation de référence
//
// IGN
// Projection cartographique Stereographique
// Notes techniques NT/G 78
//

interface

uses
  projection, stereographic;

type

  TStereo_polaire = class(TStereographic)
  private
    fLanda0 : string; // longitude de l'origine par rapport au méridien d'origine du Datum
    fLanda0Real : real; // valeur en radian de la longitude précédente
    fk0 : real; // facteur d'échelle au point d'origine
    fX0, fY0 : real; // coordonnées en projection du point d'origine
    fSud : boolean; // Pôle Sud (ou Nord)
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

    procedure ConvertirParam; virtual; // convertit les paramètres vers la projection TStereographic (générique)
  public
  end;

implementation

uses
  SysUtils, Math,
  outils, ign;


function TStereo_Polaire.GetNature : string;
begin
  result:=tpStereographicPolaire;
end;

function TStereo_polaire.GetNbReal : integer;
begin
  result:=3;
end;

function TStereo_polaire.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fk0;
    1 : result:=fX0;
    2 : result:=fY0;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TStereo_polaire.SetParamReal(index : integer; Value : real);
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

function TStereo_polaire.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='k0';
    1 : result:='X0';
    2 : result:='Y0';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TStereo_polaire.GetNbInteger : integer;
begin
  result:=0;
end;

function TStereo_polaire.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TStereo_polaire.SetParamInteger(index : integer; Value : Integer);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TStereo_polaire.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TStereo_polaire.GetNbAngle : integer;
begin
  result:=1;
end;

function TStereo_polaire.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fLanda0;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TStereo_polaire.SetParamAngle(index : integer; Value : string);
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
  else
    begin
      fValid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;
end;

function TStereo_polaire.GetNom_ParamAngle(index : integer) : string;
begin
  case index of
    0 : result:='Landa0';
  else
    begin
      result:='';
      raise ERangeError.Create('Demande de paramètre hors-limites');
    end;
  end;
end;

function TStereo_polaire.GetNbBoolean : integer;
begin
  result:=1;
end;

function TStereo_polaire.GetParamBoolean(index : integer) : Boolean;
begin
  if index=0 then
    result:=fSud
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TStereo_polaire.SetParamBoolean(index : integer; Value : Boolean);
begin
  if index=0 then
    fSud:=Value
  else
  begin
    fValid:=false;
    raise ERangeError.Create('Ecriture de paramètre hors-limites');
  end;
  ConvertirParam;
end;

function TStereo_polaire.GetNom_ParamBoolean(index : integer) : string;
begin
  if index=0 then
    result:='Sud'
  else
  begin
    result:='';
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TStereo_polaire.ConvertirParam;
begin
  if fValid then
  try
    with Datum.Ellipsoide do
      if fSud then
        Remonte(1, a*fk0/sqrt(1-sqr(e))*power((1-e)/(1+e),e/2),0, fX0, fY0, fLanda0,'-90°')
      else
        Remonte(1, a*fk0/sqrt(1-sqr(e))*power((1-e)/(1+e),e/2),0, fX0, fY0, fLanda0,'90°');
  except
  end;
end;

procedure TStereo_polaire.SetValid(const Value: boolean);
begin
  inherited;
  ConvertirParam;
end;

end.
