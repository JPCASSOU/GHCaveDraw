unit stereo_courbure;

// cas de la projection Stereographique définie avec une sphère de courbure

// documentation de référence
//
// IGN
// Projection cartographique Stereographique
// Notes techniques NT/G 78
//

interface

uses
  projection, stereo_reel;

type

  TStereo_courbure = class(TStereo_reel)
  private
  protected
    function GetNature : string; override;
    procedure ConvertirParam; override; // convertit les paramètres vers la projection TStereographic (générique)
  public
  end;

implementation

uses
  ign, outils;

function TStereo_courbure.GetNature : string;
begin
   result:=tpStereographicCourbure;
end;


procedure TStereo_courbure.ConvertirParam;
// convertit les paramètres vers la projection TStereographic (générique)
var
  n1, c, n2, Phi0 : real;
begin
  if fValid then
  try
    Phi0:=StrToAngle(ParamAngle[1]);
    alg0043_courbure(Datum.Ellipsoide.a, Datum.Ellipsoide.e, Phi0, ParamReal[0],
                     ParamReal[2],
                     n1, n2, c);
    Remonte(n1, n2, c, ParamReal[1], ParamReal[2], ParamAngle[0], ParamAngle[1]);
  except
  end;
end;

end.
