unit stereo_equatoriale;

// cas de la projection Stereographique d�finie avec une sph�re �quatoriale

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

  TStereo_equatoriale = class(TStereo_reel)
  private
  protected
    function GetNature : string; override;
    procedure ConvertirParam; override; // convertit les param�tres vers la projection TStereographic (g�n�rique)
  public
  end;

implementation

uses
  ign, outils;

function TStereo_equatoriale.GetNature : string;
begin
   result:=tpStereographicEquatoriale;
end;


procedure TStereo_equatoriale.ConvertirParam;
// convertit les param�tres vers la projection TStereographic (g�n�rique)
var
  n2, Phi0, PhiC : real;
begin
  if fValid then
  try
    Phi0:=StrToAngle(ParamAngle[1]);
    alg0043_equatoriale(Datum.Ellipsoide.a, Datum.Ellipsoide.e, Phi0, ParamReal[0],
                        n2, PhiC);
    Remonte(1, n2, 0, ParamReal[1], ParamReal[2], ParamAngle[0],
            AngleToStrGrade(PhiC));
  except
  end;
end;

end.

