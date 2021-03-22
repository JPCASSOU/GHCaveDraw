unit laborde_bitan;

// cas de la projection Gauss-Laborde définie avec une sphère bitangente

// documentation de référence
//
// IGN
// Projection cartographique Gauss-Laborde
// Notes techniques NT/G 73
//
// http://professionnels.ign.fr/DISPLAY/000/526/702/5267020/NTG_73.pdf

// Note : faute d'exemple numérique, cette partie n'a pas été testée

interface

uses
  projection, laborde_reel, laborde;

type

  TLaborde_bitan = class(TLaborde_reel)
  private
  protected
    function GetNature : string; override;
    procedure ConvertirParam; override; // convertit les paramètres vers la projection TLambert (générique)
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
// convertit les paramètres vers la projection TLaborde (générique)
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

