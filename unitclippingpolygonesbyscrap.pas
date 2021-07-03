unit unitClippingPolygonesByScrap;
// 30/08/2020: Pointage temporel
{$mode delphi}
interface

uses
  Classes, SysUtils,
  GHCD_Types,
  UnitDocDessin,
  GeneralFunctions,
  GeometryPolygonesUtils,
  clipper;

// Délimite un polygone par un scrap (utile pour remplir une galerie)
function DelimiterUnPolygoneParUnScrap(const FD: TDocumentDessin;
                                       const QScrap: TScrap; var QPolygone: TPolygone): integer;



implementation

// Délimite un polygone par un scrap (utile pour remplir une galerie)
function DelimiterUnPolygoneParUnScrap(const FD: TDocumentDessin;
                                       const QScrap: TScrap;
                                       var   QPolygone: TPolygone): integer;
var
  OP: TClipType;
  G1, G2: TGroupeEntites;
  OpsCSG: TOperationsCSGClipping;
  PP1, PP2: TArrayVertexPolygon;
begin
  Result := errMERGE_SCRAP_AND_POLYGON_ANY_ERROR;
  AfficherMessage('DelimiterUnPolygoneParUnScrap()');

  OP := ctIntersection;
  // il est interdit de fusionner deux polygones appartenant à deux groupes différents
  G1 := FD.GetGroupeByIDGroupe(QScrap.IDGroupe);
  G2 := FD.GetGroupeByIDGroupe(QPolygone.IDGroupe);
  AfficherMessageErreur(Format('DelimiterUnPolygoneParUnScrap: Scrap: %d: %d ; Poly: %d: %d',
                               [QScrap.IDGroupe, G1.IDGroupeEntites,
                                QPolygone.IDGroupe, G2.IDGroupeEntites]));
  if (G2.IDGroupeEntites <> G1.IDGroupeEntites) then Exit(errMERGE_POLYGONES_GROUPES_MISMATCH);

  PP1 := QScrap.Sommets;
  PP2 := QPolygone.Sommets;

  OpsCSG := TOperationsCSGClipping.Create;
  try
    OpsCSG.Initialise(FD, PP1, PP2);
    if (OpsCSG.IntersectionVertexPolygonArray()) then
    begin
      AfficherMessage('-- polygones 1 et 2 se coupent');
      if (OpsCSG.ExecuteCSG(OP)) then
      begin
        QPolygone.Sommets := OpsCSG.GetVertexPolygonArrayResult();
        Result := errMERGE_POLYGONES_OK;
      end;
    end
    else
    begin
      Result := errMERGE_POLYGONES_NO_INTERSECT;
      AfficherMessage('-- polygones 1 et 2 sont disjoints');
    end;
    OpsCSG.Finalise();
  finally
    FreeAndNil(OpsCSG);
  end;
end;




end.

