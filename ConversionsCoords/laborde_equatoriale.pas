unit laborde_equatoriale;

// cas de la projection Gauss-Laborde définie avec une sphère équatoriale

// documentation de référence
//
// IGN
// Projection cartographique Gauss-Laborde
// Notes techniques NT/G 73
//
// http://professionnels.ign.fr/DISPLAY/000/526/702/5267020/NTG_73.pdf

interface

uses
  projection, laborde_reel, laborde;

type

  TLaborde_equatoriale = class(TLaborde_reel)
  private
  protected
    function GetNature : string; override;
    procedure ConvertirParam; override; // convertit les paramètres vers la projection TLambert (générique)
  public
  end;

implementation

uses
  ign, outils;

function TLaborde_equatoriale.GetNature : string;
begin
   result:=tpGaussLabordeequatoriale;
end;


procedure TLaborde_equatoriale.ConvertirParam;
// convertit les paramètres vers la projection TLaborde (générique)
var
  n2, Phi0 : real;
begin
  try
    Phi0:=StrToAngle(ParamAngle[1]);
    alg0046_equatoriale(Datum.Ellipsoide.a, Datum.Ellipsoide.e, Phi0, ParamReal[0],
                     n2);
    Remonte(1, n2, 0, ParamReal[1], ParamReal[2], ParamAngle[0]);
  except
  end;
end;

end.
