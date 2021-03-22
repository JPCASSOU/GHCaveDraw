// Dessin direct dans OpenOffice.org Draw
unit UnitDessinOOo;

{$mode delphi}

interface

uses
  GHCD_Types,
  UnitDocDessin,
  GeneralFunctions,
  variants, varutils,
  OOoConstants, OOoMessages, OOoTools,
  Graphics,
  Classes, SysUtils, FileUtil;


const COM_SUN_STAR_DRAWING ='com.sun.star.drawing.';
// facteur d'échelle interne, en 1/100 de mm
const FACTEUR_ECHELLE_OOO = 1000.00;

type TPointOOo = record
  X: double;
  Y: double;
end;

type TTypeOOoForme = (
  toofLINE,
  toofPOLYLINE,
  toofRECTANGLE,
  toofELLIPSE,
  toofPOLYGON,
  toofOPEN_BEZIER,
  toofCLOSE_BEZIER,
  toofTEXTE,
  toofIMAGE
);

type TDocOOoDraw = Variant;
type TCahier     = Variant;
type TCouches    = Variant;
type TUnePage    = Variant;
type TCouche     = Variant;

type TOOoForme   = Variant;


type

{ TOOoDrawContext }

 TOOoDrawContext = class
  private
    FMyDocDessin    : TDocumentDessin;
    FDocumentDraw   : TDocOOoDraw;
    FLesCouches     : TCouches;
    FMesFeuilles    : TCahier;
    FPageCourante   : TUnePage;
    FCoucheCourante : TCouche;

    FLaFormeCourante: TOOoForme;
    procedure AjouterCouche(const NomCouche: string);
    function CreateForme(const Lay: TCouche; const TF: TTypeOOoForme): TOOoForme;
    procedure DessinerDansPage(const MyPage: TUnePage);
    procedure DrawLigne(var TF: TOOoForme; const P1, P2: TPoint3Df);
    procedure DrawRectangle(var TF: TOOoForme; const P1, P2: TPoint3Df);
    function  GCSCoordsToOOoCoords(const PT: TPoint3Df): TPointOOo;
    function  LancerOOoDraw: boolean;
    procedure PreparerPage(const UnePage: TUnePage);
    procedure SetCoucheByIndex(const Idx: integer);
    procedure SetCoucheByName(const S: string);
    procedure SetPen(const LaForme: TOOoForme; const LineColor: TColor; const LineWidthMM: double);
  public
    function  Initialise(const DC: TDocumentDessin): boolean;
    procedure Finalise;

end;

implementation

{ TOOoDrawContext }

function TOOoDrawContext.LancerOOoDraw(): boolean;
var
  i  : integer;
  nbP: integer;
  NbC: integer;
  EWE: WideString;
  LaCouche: TCouche;
begin
  Result := False;
  {$IFDEF MSWINDOWS}
  ConnectOpenoffice;
  if (IsOpenOfficeConnected) then
  begin
    FDocumentDraw := StarDesktop.LoadComponentFromURL('private:factory/sdraw', '_blank', 0, dummyArray);
    if (FDocumentDraw.SupportsService(COM_SUN_STAR_DRAWING + 'DrawingDocument')) then
    begin
      // Laz 1.8: ExtractFileNameWithoutExt remplace ExtractFileNameOnly
      FDocumentDraw.Title := WideString(ExtractFileNameWithoutExt(FMyDocDessin.GetDocumentName));
      FMesFeuilles := FDocumentDraw.DrawPages;

      NbP := FMesFeuilles.Count;                     // OK
      AfficherMessage(Format('002: %d pages', [NbP]));   // OK
      FPageCourante   := FMesFeuilles.GetByIndex(0); // OK
      FPageCourante.Name := 'Plan_principal';        // OK - Visible sur le doc OOo Draw
      AfficherMessage('003');
      EWE := FPageCourante.Name;
      AfficherMessage('004: ' + EWE);
      //----------------------------------------
      // couches

      FLesCouches := FDocumentDraw.LayerManager;        // OK
      // sandbox: gestion des couches
      // ajout d'une couche
      AjouterCouche('Parois');
      AjouterCouche('Details');
      NbC         := FLesCouches.Count;                 // OK
      AfficherMessage(Format('Layers: %d couches',[NbC]));
      for i := 0 to NbC - 1 do
      begin
        LaCouche := FLesCouches.GetByIndex(i);
        AfficherMessage(Format('-- Couche: %d - %s', [i, LaCouche.Name]));
      end;
      SetCoucheByIndex(1);          // OK
      SetCoucheByName('Parois');    // OK
      // couches erronées = la couche courante ne doit pas changer
      SetCoucheByName('blaireau');  // OK
      SetCoucheByIndex(666);
      // la couche courante doit ici changer
      SetCoucheByName('Details');    // OK
      // préparation de la page
      PreparerPage(FPageCourante);
      // dessin
      DessinerDansPage(FPageCourante);
      Result := true;
    end;
  end
  else
  begin
    AfficherMessage('--- Impossible de se connecter à OpenOffice');

  end;
  {$ENDIF}
end;




function TOOoDrawContext.Initialise(const DC: TDocumentDessin): boolean;
begin
  Result := False;
  FMyDocDessin := DC;
  AfficherMessage(ClassName, Format('Initialise(%s)', [FMyDocDessin.GetDocumentName]));
  Result := LancerOOoDraw();
end;

procedure TOOoDrawContext.Finalise;
begin
  AfficherMessage(ClassName, 'Finalise');
end;
// gestion des couches
procedure TOOoDrawContext.AjouterCouche(const NomCouche: string);
var
  WU: integer;
  LaCouche : Variant;
begin
  WU := FLesCouches.Count;
  LaCouche := FLesCouches.insertNewByIndex(WU);
  LaCouche.Name := WideString(NomCouche);
end;
procedure TOOoDrawContext.SetCoucheByIndex(const Idx: integer);
var
  EWE: String;
begin
  try
    FCoucheCourante := FLesCouches.GetByIndex(Idx);
    EWE := 'Couche changee';
  except
    EWE := 'Ref introuvable - Couche inchangee';
  end;
  AfficherMessage(Format('-- SetCoucheByIndex: (%d): %s - %s', [Idx, FCoucheCourante.Name, EWE]));
end;
procedure TOOoDrawContext.SetCoucheByName(const S: string);
var
  EWE: String;
begin
  try
    FCoucheCourante := FLesCouches.GetByName(WideString(S));
    EWE := 'Couche changee';
    AfficherMessage(Format('-- SetCoucheByName: (%s): %s - %s', [S, FCoucheCourante.Name, EWE]));
  except
    EWE := 'Ref introuvable - Couche inchangee';
    AfficherMessage(Format('-- %s.SetCoucheByName: (%s): %s - %s', [ClassName,S , FCoucheCourante.Name, EWE]));
  end;

end;

// préparer la page
procedure TOOoDrawContext.PreparerPage(const UnePage: TUnePage);
var
  C1: TPoint3Df;
  C2: TPoint3Df;
  dx: Extended;
  dy: Extended;
begin
  AfficherMessage(ClassName, Format('Preparation de la page: %s', [UnePage.Name]));
  // Largeur et hauteur de la page = celles de notre topo
  C1 := FMyDocDessin.GetCoordsMini;
  C2 := FMyDocDessin.GetCoordsMaxi;
  dx := (C2.X - C1.X) * FACTEUR_ECHELLE_OOO;
  dy := (C2.Y - C1.Y) * FACTEUR_ECHELLE_OOO;
  UnePage.Width  := dx;
  UnePage.Height := dy;
end;

// lancer le dessin
procedure TOOoDrawContext.DessinerDansPage(const MyPage: Variant);
begin
  AfficherMessage(ClassName, Format('Dessin de la page: %s', [MyPage.Name]));
  AfficherMessage('--------------------------');
  // couche 'Parois'
  SetCoucheByName('Parois');
  // style de ligne

    FLaFormeCourante := CreateForme(FCoucheCourante, toofPOLYLINE);
      SetPen(PasColor2OOoColor(FLaFormeCourante), clRed, 0.35);
  AfficherMessage('--------------------------');

end;
// conversion de coordonnées
function TOOoDrawContext.GCSCoordsToOOoCoords(const PT: TPoint3Df): TPointOOo;
var
  C1: TPoint3Df;
begin
  C1 := FMyDocDessin.GetCoordsMini;
  Result.X := (PT.X - C1.X) * FACTEUR_ECHELLE_OOO;
  Result.Y := (PT.Y - C1.Y) * FACTEUR_ECHELLE_OOO;
end;
// définition des couleurs et styles d'objets
procedure TOOoDrawContext.SetPen(const LaForme: TOOoForme; const LineColor: TColor; const LineWidthMM: double);
begin
  AfficherMessage(ClassName, Format('SetPen: %X, %.2f', [LineColor, LineWidthMM]));
  LaForme.LineWidth := LineWidthMM * 100;
  LaForme.Style     := COM_SUN_STAR_DRAWING + 'LineStyle.SOLID';
  LaForme.LineColor := PasColor2OOoColor(LineColor);

end;
// création d'une forme
function TOOoDrawContext.CreateForme(const Lay: TCouche; const TF: TTypeOOoForme): TOOoForme;
const
  SHP = 'Shape';
var
  EWE: String;
begin
  case TF of
    toofLINE         : EWE := COM_SUN_STAR_DRAWING + 'Line' + SHP;
    toofPOLYLINE     : EWE := COM_SUN_STAR_DRAWING + 'PolyLine' + SHP;
    toofRECTANGLE    : EWE := COM_SUN_STAR_DRAWING + 'Rectangle' + SHP;
    toofELLIPSE      : EWE := COM_SUN_STAR_DRAWING + 'Ellipse' + SHP;
    toofPOLYGON      : EWE := COM_SUN_STAR_DRAWING + 'PolyPolygon' + SHP;
    toofOPEN_BEZIER  : EWE := COM_SUN_STAR_DRAWING + 'OpenBezier' + SHP;
    toofCLOSE_BEZIER : EWE := COM_SUN_STAR_DRAWING + 'CloseBezier' + SHP;
    toofTEXTE        : EWE := COM_SUN_STAR_DRAWING + 'Text' + SHP;
    toofIMAGE        : EWE := COM_SUN_STAR_DRAWING + 'GraphicObject' + SHP;
  else
    ;
  end;
  AfficherMessage(ClassName, Format('CreateForme: %d - %s', [ord(TF), EWE]));
  Result := FDocumentDraw.createInstance(EWE);
  AfficherMessage(ClassName, Format('001', [ord(TF), EWE]));

  Result.LayerName := Lay.Name;
end;

// dessin des formes
procedure TOOoDrawContext.DrawLigne(var TF: TOOoForme; const P1, P2: TPoint3Df);
var
  WU0, WU1: TPointOOo;
begin
  WU0 := GCSCoordsToOOoCoords(P1);
  WU1 := GCSCoordsToOOoCoords(P2);

end;
procedure TOOoDrawContext.DrawRectangle(var TF: TOOoForme; const P1, P2: TPoint3Df);
begin

end;

// transformer

end.
