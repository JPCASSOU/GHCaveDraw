unit ancetre;

interface

uses
  Classes, datum;


type

// ancètre commun des projections simples (TProjection) et multiples (TMultiples)
// ne doit pas être utilisée ni même dérivée en dehors des deux types précédents
// explications supplémentaires à la fin de l'interface

  TAncetre = class
  protected
    fDatum : TDatum; // datum de la projection
    fNom : string; // nom (abrégé) de la projection
    fCodeEPSG: integer; // code EPSG
    fDescription : string; // description de la projection
    fValid : boolean; // indique si les paramètres de la projection sont bien définis
    // real
    function GetNbReal : integer; virtual; abstract;
    function GetParamReal(index : integer) : real; virtual; abstract;
    procedure SetParamReal(index : integer; Value : real); virtual; abstract;
    function GetNom_ParamReal(index : integer) : string; virtual; abstract;
    // integer
    function GetNbInteger : integer; virtual; abstract;
    function GetParamInteger(index : integer) : Integer; virtual; abstract;
    procedure SetParamInteger(index : integer; Value : Integer); virtual; abstract;
    function GetNom_ParamInteger(index : integer) : string; virtual; abstract;
    // Angle
    function GetNbAngle : integer; virtual; abstract;
    function GetParamAngle(index : integer) : string; virtual; abstract;
    procedure SetParamAngle(index : integer; Value : string); virtual; abstract;
    function GetNom_ParamAngle(index : integer) : string; virtual; abstract;
    // Boolean
    function GetNbBoolean : integer; virtual; abstract;
    function GetParamBoolean(index : integer) : Boolean; virtual; abstract;
    procedure SetParamBoolean(index : integer; Value : Boolean); virtual; abstract;
    function GetNom_ParamBoolean(index : integer) : string; virtual; abstract;

    function GetNature : string; virtual; abstract;
    procedure SetValid(const Value: boolean); virtual;
  public
    property Datum : TDatum read fDatum write fDatum;
    property Nom : string read fNom write fNom;
    property Description : string read fDescription write fDescription;
    property CodeEPSG: integer read fCodeEPSG write fCodeEPSG;
    property Nature : string read GetNature;
    property Valid : boolean read fValid write SetValid;
    // real
    property Nb_ParamReal : integer read GetNbReal;
    property ParamReal[index : integer] : real read GetParamReal write SetParamReal;
    property Nom_ParamReal[index : integer] : string read GetNom_ParamReal;
    // Integer
    property Nb_ParamInteger : integer read GetNbInteger;
    property ParamInteger[index : integer] : Integer read GetParamInteger write SetParamInteger;
    property Nom_ParamInteger[index : integer] : string read GetNom_ParamInteger;
    // Angle
    property Nb_ParamAngle : integer read GetNbAngle;
    property ParamAngle[index : integer] : string read GetParamAngle write SetParamAngle;
    property Nom_ParamAngle[index : integer] : string read GetNom_ParamAngle;
    // Boolean
    property Nb_ParamBoolean : integer read GetNbBoolean;
    property ParamBoolean[index : integer] : Boolean read GetParamBoolean write SetParamBoolean;
    property Nom_ParamBoolean[index : integer] : string read GetNom_ParamBoolean;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream (Stream : TStream);
    procedure SaveToStream (Stream : TStream);
  end;

// une projection peut disposer de quatre types de données :
// Real, Integer, Boolean et Angle.
// les angles sont stockées sous forme texte comme expliqué dans
// l'unité outils
// chaque projection peut avoir un nombre variable de paramètres de chaque
// type, y compris zéro et sans limite maximale
// TAncetre se charge de la lecture et de l'enregistrement des données
// les descendants ne doivent pas s'en occuper


implementation

uses
  SysUtils,
  outils, donnee, multiple;

constructor TAncetre.Create;
begin
  inherited Create;
  fValid:=false;
  fCodeEPSG:= 0;
end;

destructor TAncetre.Destroy;
begin
  inherited Destroy;
end;

procedure TAncetre.LoadFromStream (Stream : TStream);
var
  Parametres : TstringList;
  I, Compteur : integer;
begin
  Parametres:=TStringList.Create;
  Valid:=false;
  try
    DetailParam(FinLigne(Stream),Parametres);
    if Parametres.Count<>Nb_ParamReal+Nb_ParamInteger+Nb_ParamAngle+
                         Nb_ParamBoolean+3 then
      Raise ERangeError.Create('Nombre incorrecte de paramètre lors de la lecture d''une projection');
    Nom:=Parametres[0];
    Description:=Parametres[1];
    // recherche du datum associé
    I:=0;
    while (I<Donnees.LesDatum.Count) and
      ((Donnees.LesDatum[I] as TDatum).Nom<>Parametres[2]) do
      inc(I);
    if I>=Donnees.LesDatum.Count then
      Raise ERangeError.CreateFmt('Datum %s inconnu dans la projection %s',
      [Parametres[2],Parametres[0]]);
    Datum:=Donnees.LesDatum[I] as TDatum;

    Compteur:=3;
    for I:=0 to Nb_ParamReal-1 do
      ParamReal[I]:=StrToFloat(Parametres[I+Compteur]);
    inc(Compteur,Nb_ParamReal);
    for I:=0 to Nb_ParamInteger-1 do
      ParamInteger[I]:=StrToInt(Parametres[I+Compteur]);
    inc(Compteur,Nb_ParamInteger);
    for I:=0 to Nb_ParamAngle-1 do
      ParamAngle[I]:=Parametres[I+Compteur];
    inc(Compteur,Nb_ParamAngle);
    for I:=0 to Nb_ParamBoolean-1 do
      ParamBoolean[I]:=(Parametres[I+Compteur]='1');

    Valid:=true;
  finally
    Parametres.Free;
  end;
end;

procedure TAncetre.SaveToStream (Stream : TStream);
var
  Preambule, S : string;
  I : integer;
begin
  if (self is TMultiple) then
    Preambule:='Multiple'
  else
    Preambule:='Projection';
  S:=Format('%s "%s"="%s" "%s" "%s"',[Preambule, Nature, Nom, Description, Datum.Nom]);
  for I:=0 to Nb_ParamReal-1 do
    S:=S+Format(' %.12g',[ParamReal[I]]);
  for I:=0 to Nb_ParamInteger-1 do
    S:=S+Format(' %d',[ParamInteger[I]]);
  for I:=0 to Nb_ParamAngle-1 do
    S:=S+' '+ParamAngle[I];
  for I:=0 to Nb_ParamBoolean-1 do
    S:=S+' '+TextBoolean[ParamBoolean[I]];
  S:=S+#13#10;
  Stream.Write(S[1],length(S));
end;


procedure TAncetre.SetValid(const Value: boolean);
begin
  fValid := Value;
end;

end.
