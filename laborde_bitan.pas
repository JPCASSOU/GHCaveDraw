unit laborde_bitan;

// cas de la projection Gauss-Laborde d�finie avec une sph�re bitangente

// documentation de r�f�rence
//
// IGN
// Projection cartographique Gauss-Laborde
// Notes techniques NT/G 73
//
// http://professionnels.ign.fr/DISPLAY/000/526/702/5267020/NTG_73.pdf

// Note : faute d'exemple num�rique, cette partie n'a pas �t� test�e

interface

uses
  projection, laborde_reel, laborde;

type

  TLaborde_bitan = class(TLaborde_reel)
  private
  protected
    function GetNature : string; override;
    procedure ConvertirParam; override; // convertit les param�tres vers la projection TLambert (g�n�rique)
  public
  end;

implementation

uses
  ign, outils;

function TLaborde_bitan.GetNature : string;
begin
   result:=tpGaussLabordeBitangente;
end;


procedure TLaborde_bitan.ConvertirParam;
// convertit les param�tres vers la projection TLaborde (g�n�rique)
var
  n2, c, Phi0 : real;
begin
  try
    Phi0:=StrToAngle(ParamAngle[1]);
    alg0046_bitan(Datum.Ellipsoide.a, Datum.Ellipsoide.e, Phi0, ParamReal[0],
                  n2, c);
    Remonte(1, n2, c, ParamReal[1], ParamReal[2], ParamAngle[0]);
  except
  end;
end;

end.

