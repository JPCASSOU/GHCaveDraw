unit ellipsoide;

interface

uses
  Classes
  ;//StructuresDonnees;

type

  { TEllipsoide }

  TEllipsoide = class
  private
    fa : real; // demi grand axe en mètre
    fe : real; // excentricité
    fNom : string; // nom (abrégé) de l'ellipsoïde
    fDescription : string; // description d'ellipsoïde
    fCodeEPSG    : integer;
    fValid : boolean; // indique si les paramètres de l'ellipsoïde sont bien définis
    function GetB : real;

    procedure SetB (Value : real);

    //function GetInvF : real;
    //procedure SetInvF (Value : real);
  public
    property a : real read fa write fa;
    property e : real read fe write fe;
    property b : real read GetB write SetB;
    //property Invf : real read GetInvF write SetInvF;
    property Nom : string read fNom write fNom;
    property CodeEPSG: integer read fCodeEPSG write fCodeEPSG;
    property Description : string read fDescription write fDescription;
    property Valid : boolean read fValid;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream (Stream : TStream);
    procedure SaveToStream (Stream : TStream);
    procedure SetParameters(const qCodeEPSG: integer; const QNom, QDescription: string; const qA, qE: double);
    procedure Assign(Autre : TEllipsoide);
  end;

implementation

uses
  SysUtils,
  outils;

constructor TEllipsoide.Create;
begin
  inherited Create;
  fValid:=false;
end;

destructor TEllipsoide.Destroy;
begin
  inherited Destroy;
end;

function TEllipsoide.GetB : real;
begin
  result:=a*sqrt(1-sqr(e));
end;
procedure TEllipsoide.SetB (Value : real);
begin
  e:=sqrt(1-sqr(Value/a));
end;
//*)
(*
function TEllipsoide.GetInvF : real;
begin
  result:=a/(a-b);
end;
//*)
(*
procedure TEllipsoide.SetInvF (Value : real);
begin
//  b:=a*(1-1/Value);
end;
//*)

procedure TEllipsoide.LoadFromStream (Stream : TStream);
var
  Parametres : TstringList;
begin
  fValid:=false;
  Parametres:=TStringList.Create;
  try
    DetailParam(FinLigne(Stream),Parametres);
    if Parametres.Count<>4 then
      Raise ERangeError.Create('Nombre incorrecte de paramètre lors de la lecture d''un ellipsoïde');
    Nom:=Parametres[0];
    Description:=Parametres[1];
    a:=StrToFloat(Parametres[2]);
    e:=StrToFloat(Parametres[3]);
    fValid:=true;
  finally
    Parametres.Free;
  end;
end;

procedure TEllipsoide.SaveToStream (Stream : TStream);
var
  S : string;
begin
  S:=Format('Ellipsoide="%s" "%s" %.12g %.12g',[Nom, Description, a, e]);
  S:=S+#13#10;
  Stream.Write(S[1],length(S));
end;

procedure TEllipsoide.Assign(Autre : TEllipsoide);
begin
  Nom:=Autre.Nom;
  Description:=Autre.Description;
  a:=Autre.a;
  e:=Autre.e;
end;

procedure TEllipsoide.SetParameters(const qCodeEPSG: integer;
                                    const QNom, QDescription: string;
                                    const qA, qE: double);
begin
  Nom         := QNom;
  CodeEPSG    := qCodeEPSG;
  Description := QDescription;
  a           := qA;
  e           := qE;
  fValid:=true;

end;


end.
