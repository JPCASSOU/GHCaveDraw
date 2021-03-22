unit UnitSilhouetteGalerie;

{$INCLUDE CompilationParameters.inc}

interface

uses
  StructuresDonnees,
  //UnitStructuresDonneesEntites,
  Common,
  {$IFDEF USE_GENERIC_TYPES}
  UnitListesSimplesWithGeneriques,
  {$ELSE}
  UnitListesSimplesWithoutGeneriques,
  {$ENDIF}

  Classes, SysUtils, Graphics;
// silhouette d'une série
type
{ TSilhouetteGalerie }
  TSilhouetteGalerie = class(TListeSimple<TPtsSectionTransversale>)
  private
    FFillColor: TColor;
    FPenColor: TColor;
    FArrayPolygone: TArrayPoints2Df;
    FNomSilhouette: string;
    FIdxEntrance: TNumeroEntrance;
    FIdxReseau: TNumeroReseau;
    constructor Create;
    destructor  Destroy;

  public
    procedure SetNomSilhouette(const N: string);
    procedure SetIdxEntranceReseau(const E: TNumeroEntrance; const R: TNumeroReseau);
    function  GetNomSilhouette(): string;

    function  GetNumeroEntrance(): TNumeroEntrance;
    function  GetNumeroReseau(): TNumeroReseau;
    function  GetFillColor: TColorRGBA;
    procedure ClearSilhouette(const PenColor, FillColor: TColor);
    procedure AddPtsParois(const E: TPtsSectionTransversale);
    function  BuildPolygoneParois(): Boolean;
    function  GetNbPtsParois(): integer;
    function  GetPointPolygoneParois(const Idx: integer): TPoint2Df;
    function  GetNbPointsPolygoneParois(): integer;
    function  GetPtsParois(const Idx: integer): TPtsSectionTransversale;
end;

implementation
{ TSilhouetteGalerie }



procedure TSilhouetteGalerie.ClearSilhouette(const PenColor, FillColor: TColor);
var
  i: integer;
begin
  FPenColor := PenColor;
  FFillColor:= FillColor;
  SetLength(FArrayPolygone, 0);
  self.ClearListe();
end;


constructor TSilhouetteGalerie.Create;
begin
  inherited;
  FNomSilhouette := '';
end;

destructor TSilhouetteGalerie.Destroy;
begin
  inherited;
end;

procedure TSilhouetteGalerie.SetNomSilhouette(const N: string);
begin
  FNomSilhouette := N;
end;

procedure TSilhouetteGalerie.SetIdxEntranceReseau(const E: TNumeroEntrance; const R: TNumeroReseau);
begin
  FIdxEntrance := E;
  FIdxReseau   := R;
end;

function TSilhouetteGalerie.GetNomSilhouette(): string;
begin
  Result := FNomSilhouette;
end;

function TSilhouetteGalerie.GetNumeroEntrance(): TNumeroEntrance;
begin
  Result := FIdxEntrance;
end;

function TSilhouetteGalerie.GetNumeroReseau(): TNumeroReseau;
begin
  result := FIdxReseau;
end;

procedure TSilhouetteGalerie.AddPtsParois(const E: TPtsSectionTransversale);
begin
  self.AddElement(E);
end;

function TSilhouetteGalerie.GetPtsParois(const Idx: integer): TPtsSectionTransversale;
begin
  Result := self.GetElement(Idx);
end;

function TSilhouetteGalerie.GetFillColor: TColorRGBA;
begin
  Result.R := Red(FFillColor);
  Result.G := Green(FFillColor);
  Result.B := Blue(FFillColor);
  Result.A := $FF;
end;

function TSilhouetteGalerie.GetNbPtsParois(): integer;
begin
  Result := self.GetNbElements();
end;

function TSilhouetteGalerie.GetPointPolygoneParois(const Idx: integer
  ): TPoint2Df;
begin
  Result := FArrayPolygone[Idx];
end;

function TSilhouetteGalerie.GetNbPointsPolygoneParois(): integer;
begin
  Result := High(FArrayPolygone) + 1;
end;
function TSilhouetteGalerie.BuildPolygoneParois(): Boolean;
var
  i, Nb: integer;
  WU: TPtsSectionTransversale;
  EWE : TGHStringArray;
  TempStrings: TStringList;
begin
  Result := False;
  SetLength(FArrayPolygone, 0);
  // la classique astuce du TStringList ...
  TempStrings := TStringList.Create;
  try
    TempStrings.Clear;
    // paroi droite
    for i:= 0 to self.GetNbPtsParois - 1 do
    begin
     WU := self.GetPtsParois(i);
     TempStrings.Add(Format('%.2f|%.2f', [WU.ParoiDroiteX, WU.ParoiDroiteY]));
    end;
    // paroi gauche = parcours en arrière
    for i:= self.GetNbPtsParois - 1 downto 0 do
    begin
      WU := self.GetPtsParois(i);
      TempStrings.Add(Format('%.2f|%.2f', [WU.ParoiGaucheX, WU.ParoiGaucheY]));
    end;
    // construire le tableau ici
    if (TempStrings.Count > 0) then
    begin
      SetLength(FArrayPolygone, TempStrings.Count);
      for i:= 0 to TempStrings.Count - 1 do
      begin
        EWE := split(TempStrings.Strings[i], '|');
        FArrayPolygone[i].X := ConvertirEnNombreReel(EWE[0], 0.00);
        FArrayPolygone[i].Y := ConvertirEnNombreReel(EWE[1], 0.00);
      end;
      // résultat OK si le polygone contient au moins quatre points
      Result := (High(FArrayPolygone) > 3);
    end;
  finally
    FreeAndNil(TempStrings);//       TempStrings.Free;
  end;
end;

end.

