unit JPC_Ellipsoide;
interface

uses
  sysutils,
  outils,
  Classes,
  StructuresDonnees;

type

  { TJPCEllipsoide }

  TJPCEllipsoide = class
  private
    fa : real; // demi grand axe en mètre
    fe : real; // excentricité
    fNom : string; // nom (abrégé) de l'ellipsoïde
    fDescription : string; // description d'ellipsoïde
    fValid : boolean; // indique si les paramètres de l'ellipsoïde sont bien définis

    function GetB : real;

    procedure SetB (Value : real);
    //function GetInvF : real;
    //procedure SetInvF (Value : real);
  public
    property Nom : string read fNom write fNom;
    property Description : string read fDescription write fDescription;
    property a : real read fa write fa;
    property e : real read fe write fe;
    property b : real read GetB write SetB;
    //property Invf : real read GetInvF write SetInvF;

    property Valid : boolean read fValid;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Autre: TJPCEllipsoide);
  end;

implementation


constructor TJPCEllipsoide.Create;
begin
  inherited Create;
  fValid:=false;
end;

destructor TJPCEllipsoide.Destroy;
begin
  inherited Destroy;
end;

function TJPCEllipsoide.GetB : real;
begin
  result:=a*sqrt(1-sqr(e));
end;
procedure TJPCEllipsoide.SetB (Value : real);
begin
  e:=sqrt(1-sqr(Value/a));
end;
//*)
(*
function TJPCEllipsoide.GetInvF : real;
begin
  result:=a/(a-b);
end;
//*)
(*
procedure TJPCEllipsoide.SetInvF (Value : real);
begin
//  b:=a*(1-1/Value);
end;
//*)


procedure TJPCEllipsoide.Assign(Autre : TJPCEllipsoide);
begin
  Nom:=Autre.Nom;
  Description:=Autre.Description;
  a:=Autre.a;
  e:=Autre.e;
end;


end.


end.
