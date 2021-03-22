unit laborde_courbure;

// cas de la projection Gauss-Laborde d�finie avec une sph�re de courbure

// documentation de r�f�rence
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

  TLaborde_courbure = class(TLaborde_reel)
  private
  protected
    function GetNature : string; override;
    procedure ConvertirParam; override; // convertit les param�tres vers la projection TLaborde (g�n�rique)
  public
  end;

implementation

uses
  ign, outils;

function TLaborde_courbure.GetNature : string;
begin
   result:=tpGaussLabordeCourbure;
end;


procedure TLaborde_courbure.ConvertirParam;
// convertit les param�tres vers la projection TLaborde (g�n�rique)
var
  n1, c, n2, Ys, Phi0 : real;
begin
  if fValid then
  try
    Phi0:=StrToAngle(ParamAngle[1]);
    alg0046_courbure(Datum.Ellipsoide.a, Datum.Ellipsoide.e, Phi0, ParamReal[0],
                     ParamReal[2],
                     n1, c, n2, Ys);
    Remonte(n1, n2, c, ParamReal[1], Ys, ParamAngle[0]);
  except
  end;
end;

end.
