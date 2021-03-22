unit stereo_bitan;

// cas de la projection Stereographique d�finie avec une sph�re bitangente

// documentation de r�f�rence
//
// IGN
// Projection cartographique Stereographique
// Notes techniques NT/G 78
//

interface

uses
  projection, stereo_reel;

type

  TStereo_bitan = class(TStereo_reel)
  private
  protected
    function GetNature : string; override;
    procedure ConvertirParam; override; // convertit les param�tres vers la projection TStereographic (g�n�rique)
  public
  end;

implementation

uses
  ign, outils;

function TStereo_bitan.GetNature : string;
begin
   result:=tpStereographicBitangente;
end;


procedure TStereo_bitan.ConvertirParam;
// convertit les param�tres vers la projection TStereographic (g�n�rique)
var
  c, n2, Phi0 : real;
begin
  if fValid then
  try
    Phi0:=StrToAngle(ParamAngle[1]);
    alg0043_bitan(Datum.Ellipsoide.a, Datum.Ellipsoide.e, Phi0, ParamReal[0],
                  n2, c);
    Remonte(1, n2, c, ParamReal[1], ParamReal[2], ParamAngle[0], ParamAngle[1]);
  except
  end;
end;

end.

