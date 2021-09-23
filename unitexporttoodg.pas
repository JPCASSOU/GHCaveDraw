unit UnitExportToODG;
// 17/02/2014: Export OK : Courbes, lignes, polygones, polylignes
//             En cours  : Textes
//             A faire   : symboles, photos
//             DONE      : Conversion de coordonnées
//             A fixer   : Textes - appels OK
//             A fixer   : Le document créé nécessite une réparation (qui réussit toutefois)
//             Factorisation de QGetCoordsGCS
// 18/02/2014: Export par groupes
// 11/03/2014: Adaptation à Lazarus 1.2
//             Résolution d'un conflit de types
// 10/04/2014: Ajout des cartouches, contenant une échelle
//             Les objets polygones sont dessinés en premier
// 01/07/2015: Objet Polylignes, gestion du décalage de groupes
// 19/01/2016: Sécurisation des exports ODG, KML et OSM
{$INCLUDE CompilationParameters.inc}

interface
uses
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr
  {$ENDIF}
  , SysUtils
  , Classes
  , Graphics
  , UnitDocDessin
  , GeneralFunctions
  , GHCD_Types
  , oodrawwriter;
type

{ TExportOdgUtils }

 TExportOdgUtils = class
  private
    FMyDocDessin: TDocumentDessin;
    FODGFileName: string;
    FODGContexte: TOdg;
    // variables internes
    FLeGroupeCourant: string;
    // limites de dessin
    FCoordCoin1: TPoint3Df;
    FCoordCoin2: TPoint3Df;
    FHauteur   : double;
    FRapportX  : Extended;
    FEchelle   : double;
    // scraps uniquement
    FDoExportScrapsOnly: boolean;

    // wrappers pour les groupes
    procedure ConversionCoordonnees(const QX, QY: double; out CX, CY: double);
    procedure DebutDeGroupe(const NomGroupe: string);
    procedure DessinCartouche;
    procedure DessinCenterLines;
    procedure DessinEntrees;
    procedure DessinerGroupe(const MyGroupe: TGroupeEntites);
    procedure DessinerLesScraps(const LeGroupe: TGroupeEntites);
    procedure DessinerLesCourbes(const LeGroupe: TGroupeEntites);
    procedure DessinerLesPolylignes(const LeGroupe: TGroupeEntites);
    procedure DessinerLesLignes(const LeGroupe: TGroupeEntites);
    procedure DessinerLesPolygones(const LeGroupe: TGroupeEntites);
    procedure DessinerLesSymboles(const LeGroupe: TGroupeEntites);
    procedure DessinerLesTextes(const LeGroupe: TGroupeEntites);
    procedure FinDeGroupe(const NomGroupe: string);
    function  QGetCoordsGCS(const IDBS: TIDBaseStation; const IDGroupe: TIDGroupeEntites; const Offset: TPoint3Df): TODGPoint2Df;
    procedure TracerCourbe(const C: TCourbe);
    procedure TracerScrap(const S: TScrap);

    procedure TracerPolygone(const P: TPolygone);
    procedure TracerPolyline(const P: TPolyLigne);
    procedure TracerSimpleLigne(const L: TSimpleLigne);
    procedure TracerTexte(const T: TTextObject);
  public
    function  Initialiser(const FP: TDocumentDessin; const FileName: string; const DoExportScrapsOnly: boolean): boolean;
    procedure Finaliser;
    procedure LancerExportODG(const Echelle: double);

end;

implementation
uses DGCDummyUnit;

{ TExportOdgUtils }

function TExportOdgUtils.QGetCoordsGCS(const IDBS: TIDBaseStation; const IDGroupe: TIDGroupeEntites; const Offset: TPoint3Df): TODGPoint2Df;
var
 BS : TBaseStation;
 GP : TGroupeEntites;
 QDecalage: TPoint3Df;
begin
  Result.X := 0.00; Result.Y := 0.00;
  if (FMyDocDessin.GetBasePointByIndex(IDBS, BS)) then;
  begin
   GP := FMyDocDessin.GetGroupeByIDGroupe(IDGroupe);
   if (GP.DecalageActif) then QDecalage := GP.Decalage else QDecalage.Empty();

   Result.X := BS.PosStation.X + QDecalage.X + Offset.X;
   Result.Y := BS.PosStation.Y + QDecalage.Y + Offset.Y;
  end;
end;


function TExportOdgUtils.Initialiser(const FP: TDocumentDessin; const FileName: string; const DoExportScrapsOnly: boolean): boolean;
begin
  Result := False;
  FDoExportScrapsOnly := DoExportScrapsOnly;
  FMyDocDessin := FP;
  FODGFileName := FileName;
  //AfficherMessage(Format('%s.Initialiser: %s - %s', [ClassName, FileName, FMyDocDessin.ClassName]));
  FODGContexte := Todg.Create;
  try
    // limites du dessin
    FCoordCoin1 := FMyDocDessin.GetCoordsMini;
    FCoordCoin2 := FMyDocDessin.GetCoordsMaxi;
    FHauteur    := FCoordCoin2.Y - FCoordCoin1.Y;
    FRapportX   := 1.00;
    Result := True;
  except
  end;
  // ...
end;


procedure TExportOdgUtils.Finaliser;
begin
  //AfficherMessage(Format('%s.Finaliser', [ClassName]));
  try

  finally
    FreeAndNil(FODGContexte);//     FODGContexte.Free;
  end;
end;

// tracé des polygones
procedure TExportOdgUtils.TracerPolygone(const P: TPolygone);
var
  i, Nb: Integer;
  PolySommets: array of TODGPoint2Df;
  PST: TVertexPolygon;
  PP, ST: TODGPoint2Df;
  EWE: String;
begin
  AfficherMessage('TracerPolygone');
  Nb := P.getNbVertex();
  SetLength(PolySommets, Nb);
  for i := 0 to Nb - 1 do
  begin
    PST := P.getVertex(i);
    PP  := QGetCoordsGCS(PST.IDStation, P.IDGroupe, PST.Offset);
    ConversionCoordonnees(PP.X, PP.Y, ST.X, ST.Y);
    PolySommets[i] := ST; // Ne pas toucher
  end;
  EWE := SelectSVGStylePolygone(P.IDStylePolygone);
  FODGContexte.DessinPolygoneSVG(EWE, PolySommets, True, True);
end;

procedure TExportOdgUtils.TracerPolyline(const P: TPolyLigne);
var
  i, Nb: Integer;
  PolySommets: array of TODGPoint2Df;
  PST: TVertexPolygon;
  PP, ST: TODGPoint2Df;
  EWE: String;

begin
  AfficherMessage('TracerPolyline');
  Nb := P.getNbVertex();
  SetLength(PolySommets, Nb);
  for i := 0 to Nb - 1 do
  begin
    PST := P.getVertex(i);
    PP  := QGetCoordsGCS(PST.IDStation, P.IDGroupe, PST.Offset);
    ConversionCoordonnees(PP.X, PP.Y, ST.X, ST.Y);
    PolySommets[i] := ST; // Ne pas toucher
  end;
  // NOTA: Une polyligne est fonctionnellement une courbe
  EWE := SelectSVGStyleCourbe(P.IDStylePolyLine);
  FODGContexte.DessinPolygoneSVG(EWE, PolySommets, False, False);

end;

procedure TExportOdgUtils.TracerSimpleLigne(const L: TSimpleLigne);
var
  EWE: String;
  PP1, PP2: TODGPoint2Df;
  X1, Y1, X2, Y2: TScalaire;
begin
  PP1 := QGetCoordsGCS(L.IDBaseStExt1, L.IDGroupe, L.OffsetExtr1);
  PP2 := QGetCoordsGCS(L.IDBaseStExt2, L.IDGroupe, L.OffsetExtr2);
  ConversionCoordonnees(PP1.X, PP1.Y, X1, Y1);
  ConversionCoordonnees(PP2.X, PP2.Y, X2, Y2);
  EWE := SelectSVGStyleLigne(L.IDStyleLigne);
  FODGContexte.DessinLigne(EWE, X1, Y1, X2, Y2);
end;

// tracé des textes
procedure TExportOdgUtils.TracerTexte(const T : TTextObject);
var
  PP1: TODGPoint2Df;
  EWE: String;
  X1, Y1: double;
  BP: TBaseStation;
  WU: String;
begin
  // 7 8 9
  // 4 5 6
  // 1 2 3
  if (FMyDocDessin.GetBasePointByIndex(T.IDBaseStation, BP)) then
  begin
    PP1 := QGetCoordsGCS(T.IDBaseStation, T.IDGroupe, T.Offset);
    ConversionCoordonnees(PP1.X, PP1.Y, X1, Y1);
    EWE := SelectSVGStyleTexte(T.IDStyleTexte);
    WU  := InterpreterTexteAnnotation(T.Text, T.MaxLength, BP);
    FODGContexte.DessinTexte(XML_real_escape_string(EWE), X1, Y1, T.Alignment, WU);
  end;
end;

// tracé des courbes
procedure TExportOdgUtils.TracerCourbe(const C : TCourbe);
var
  i, n: integer;
  QBezierArcs: array of TODGBezierArc;
  AC: TArcCourbe;
  QA : TODGBezierArc;
  QX, QY: double;
  P1, PC1, PC2, P2: TPoint2Df;
  EWE: String;
  function QGetBezierArc(const A: TArcCourbe): TODGBezierArc;
  var
    MonArc: TODGBezierArc;
  begin
    MonArc.P1 := QGetCoordsGCS(A.IDStationP1, C.IDGroupe, A.OffsetP1);
    MonArc.PC1.X  := MonArc.P1.X + A.TangP1.X;
    MonArc.PC1.Y  := MonArc.P1.Y + A.TangP1.Y;
    MonArc.P2 := QGetCoordsGCS(A.IDStationP2, C.IDGroupe, A.OffsetP2);
    MonArc.PC2.X  := MonArc.P2.X + A.TangP2.X;
    MonArc.PC2.Y  := MonArc.P2.Y + A.TangP2.Y;
    // conversion des coordonnées
    ConversionCoordonnees(MonArc.P1.X, MonArc.P1.Y, Result.P1.X, Result.P1.Y);
    ConversionCoordonnees(MonArc.PC1.X, MonArc.PC1.Y, Result.PC1.X, Result.PC1.Y);
    ConversionCoordonnees(MonArc.PC2.X, MonArc.PC2.Y, Result.PC2.X, Result.PC2.Y);
    ConversionCoordonnees(MonArc.P2.X, MonArc.P2.Y, Result.P2.X, Result.P2.Y);
  end;
begin
  n := C.getNbArcs();// 1+ high(C.Arcs);
  SetLength(QBezierArcs, n);
  for i := 0 to High(QBezierArcs) do
  begin
    AC := c.getArc(i);//Arcs[i];
    QA := QGetBezierArc(AC);
    QBezierArcs[i] := QA;
  end;
  EWE := SelectSVGStyleCourbe(C.IDStyleCourbe);
  FODGContexte.DessinPolyBezier(EWE, QBezierArcs, False, False);
end;

procedure TExportOdgUtils.TracerScrap(const S: TScrap);
var
  i, Nb: Integer;
  PolySommets: array of TODGPoint2Df;
  PST: TVertexPolygon;
  PP, ST: TODGPoint2Df;
  EWE: String;
begin
  Nb := S.getNbVertex();
  SetLength(PolySommets, Nb);
  for i := 0 to Nb - 1 do
  begin
    PST := S.getVertex(i);
    PP  := QGetCoordsGCS(PST.IDStation, S.IDGroupe, PST.Offset);
    ConversionCoordonnees(PP.X, PP.Y, ST.X, ST.Y);
    PolySommets[i] := ST; // Ne pas toucher
  end;
  FODGContexte.DessinPolygoneSVG(SVG_STYLE_SCRAP, PolySommets, True, True);
end;


// lancer l'export
procedure TExportOdgUtils.LancerExportODG(const Echelle: double);
var
  Gr: Integer;
  QGroupe: TGroupeEntites;
begin
  AfficherMessage(Format('%s.LancerExport: %f', [ClassName, Echelle]));
  FEchelle := Echelle;
  if (FODGContexte.InitialiserFichierContent) then
  begin
    // définition des styles
    FODGContexte.AddUsuallyStyleGraphic(STYLE_STANDARD                  , 'solid', 0.00, clBlack     , 100, 'solid', clWhite  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLES_CENTERLINES_VISEES   , 'solid', 0.00, clMaroon    , 100, 'solid', clWhite  , 100);  // style Polygone controle
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLES_CENTERLINES_ANTENNES , 'solid', 0.00, clGray      , 100, 'solid', clWhite  , 100);  // style Polygone controle
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLES_CENTERLINES_SECTIONS , 'solid', 0.00, clSilver    , 100, 'solid', clWhite  , 100);  // style Polygone controle
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLES_CENTERLINES_ENTREES  , 'solid', 0.00, clBlack     , 100, 'solid', clGreen  , 100);  // style Polygone controle
    // scrap
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_SCRAP                 , 'solid', 0.00, clSilver    , 100, 'solid', clSilver , 100);
    // courbes
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_COURBE_PAROIS         , 'solid', 0.045, clMaroon   , 100, 'solid', clWhite  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_COURBE_PAROIS_CACHEES , 'solid', 0.045, clGray     , 100, 'solid', clWhite  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_COURBE_ECOULEMENTS    , 'solid', 0.070, clBlue     , 100, 'solid', clWhite  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_COURBE_RESSAUTS       , 'solid', 0.01 , clPurple   , 100, 'solid', clWhite  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_COURBE_MINI_RESSAUTS  , 'solid', 0.01 , clOlive    , 100, 'solid', clWhite  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_COURBE_PENTES         , 'solid', 0.00 , clGray     , 100, 'solid', clWhite  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_COURBE_SURPLOMB       , 'solid', 0.01 , clGreen    , 100, 'solid', clWhite  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_COURBE_CHENAL         , 'solid', 0.01 , clNavy     , 100, 'solid', clWhite  , 100);
    // polygones
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_DEFAUT       , 'solid', 0.00, clBlack     , 100, 'solid', clWhite  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_LAC          , 'solid', 0.00, clBlue      , 100, 'solid', clBlue   , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_SILHOUETTE   , 'solid', 0.00, clGray      , 100, 'solid', clGray   , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_ARGILE       , 'none' , 0.00, clBlack     , 100, 'solid', clMaroon , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_SABLE        , 'none' , 0.00, clBlack     , 100, 'solid', clOlive  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_EBOULIS      , 'none' , 0.00, clBlack     , 100, 'solid', clSilver , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_GALETS       , 'none' , 0.00, clBlack     , 100, 'solid', clPurple , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_NEIGE        , 'solid', 0.00, clBlue      , 100, 'solid', clAqua   , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_GROS_BLOC    , 'solid', 0.00, clGray      , 100, 'solid', clTeal   , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_GOUR         , 'solid', 0.00, clMaroon    , 100, 'solid', clAqua   , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_SIPHON       , 'none' , 0.00, clNavy      , 100, 'solid', clNavy   , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_VARVES       , 'none' , 0.00, clMaroon    , 100, 'solid', clCream  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_POLYGONE_CHEMIN       , 'solid', 0.00, clRed       , 100, 'solid', clYellow , 100);
    // lignes
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_LIGNE_DEFAULT         , 'solid', 0.00, clBlack     , 100, 'solid', clWhite  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_LIGNE_FLECHE          , 'solid', 0.00, clBlack     , 100, 'solid', clBlack  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_LIGNE_SUITE_RESEAU    , 'solid', 0.00, clNavy      , 100, 'solid', clWhite  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_LIGNE_FRACTURE        , 'solid', 0.00, clRed       , 100, 'solid', clWhite  , 100);
    FODGContexte.AddUsuallyStyleGraphic(SVG_STYLE_LIGNE_PENTE           , 'solid', 0.00, clBlack     , 100, 'solid', clBlack  , 100);

    // textes
    FODGContexte.AddUsuallyStyleText(SVG_STYLE_TEXTE_TITRES      , clWhite ,'Arial' , clBlack  , [fsBold, fsUnderline], 12, 0);
    FODGContexte.AddUsuallyStyleText(SVG_STYLE_TEXTE_SOUS_TITRES , clWhite ,'Arial' , clGray   , [fsBold], 10.5, 0);
    FODGContexte.AddUsuallyStyleText(SVG_STYLE_TEXTE_COTATION    , clWhite ,'Arial' , clBlue   , [fsBold], 8, 0);
    FODGContexte.AddUsuallyStyleText(SVG_STYLE_TEXTE_ORDINAIRE_1 , clWhite ,'Arial' , clMaroon , [], 7, 0);
    FODGContexte.AddUsuallyStyleText(SVG_STYLE_TEXTE_ORDINAIRE_1 , clWhite ,'Arial' , clMaroon , [], 6, 0);
    FODGContexte.AddUsuallyStyleText(SVG_STYLE_TEXTE_DEBUG       , clWhite ,'Arial' , clRed    , [], 6, 0);
    //--------------
    FODGContexte.AddUsuallyStyleGraphic('00_PolygoneControle', 'solid', 0.00, clAqua, 100, 'solid', clWhite, 100);  // style Polygone controle
    FODGContexte.AddUsuallyStyleGraphic('00_Rectangle', 'solid', 0.00, clRed, 100, 'solid', clBlue, 58);            // style Rectangle
    FODGContexte.AddUsuallyStyleGraphic('00_Epures', 'solid', 0.00, clGray, 100, 'solid', clWhite, 100);            // style 00_Epures

    FODGContexte.AddUsuallyStyleGraphic('00_CadresPhotos', 'solid', 0.08, clBlue, 100, 'solid', clgray, 50);      // styles 00_Ecoulements

    // styles de texte
    FODGContexte.AddUsuallyStyleText('00_Titres', clAqua,'Arial', clRed, [fsBold, fsUnderline], 14.5, 0);
    FODGContexte.AddUsuallyStyleText('00_TexteBleu', clYellow,'Arial', clBlue, [fsBold, fsUnderline], 8.4, 0);
    // styles spécifiques aux cartouches
    FODGContexte.AddUsuallyStyleGraphic('00_CartoucheRegle', 'solid', 0.01, clBlack, 100, 'solid', clGray, 100);
    FODGContexte.AddUsuallyStyleText('00_CartoucheTexte', clWhite,'Arial', clBlack, [fsBold, fsItalic], 5.0, 0);
    //--------------------------------------------------------------------------
    // on ouvre une nouvelle page
    FODGContexte.OuvrirNouvellePage('Page001', 'StylePagePrincipale', 'Standard');
    // LES DESSINS ICI
    //=======================================================
    DessinCartouche;
    DessinCenterLines;
    DessinEntrees;
    for Gr := 0 to FMyDocDessin.GetNbGroupes() - 1 do
    begin
      QGroupe := FMyDocDessin.GetGroupe(Gr);
      DessinerGroupe(QGroupe);
    end;
    //=======================================================
    // FIN SECTION DESSINS
    //--------------------------------------------------------------------------
    // on ferme cette page
    FODGContexte.FermerPage('Page001');
    //--------------------------------------------------------------------------
    // Fermer le fichier CONTENT.XML
    FODGContexte.CloturerFichierContent();
    // construction du fichier ODG
    FODGContexte.GenererOdg(FODGFileName);
  end // if (testodg.InitialiserFichierContent) then
  else
  begin

  end;

end;
// dessin des entitées

procedure TExportOdgUtils.DessinCartouche;
  procedure DessinEchelle(const QX, QY, L, H: double);
  const
    SD = 10;
  var
    HL, HH: double;
    FX1, FY1, FX2, FY2: Double;
    i, EWE: Integer;
  begin
    AfficherMessage('-------- DessinEchelle');
    HH := H / 2;
    HL := L / SD;
    DebutDeGroupe(GROUPE_ECHELLE);
      AfficherMessage(Format('-----> %.0f %.0f - %.0f %.0f', [QX, QY, QX + L, QY + H]));
      ConversionCoordonnees(QX, QY, FX1, FY1);
      ConversionCoordonnees(QX + L, QY + H, FX2, FY2);
      FODGContexte.DessinRectangle('00_CartoucheRegle', FX1, FY1, FX2, FY2, False);
      for i := 0 to SD -1 do
      begin
        EWE := IIF(Odd(i), 0, 1);
        ConversionCoordonnees(QX + i     * HL, QY +       EWE * HH, FX1, FY1);
        ConversionCoordonnees(QX + (i+1) * HL, QY + (EWE + 1) * HH, FX2, FY2);
        FODGContexte.DessinRectangle('00_CartoucheRegle', FX1, FY1, FX2, FY2, True);
      end;
    FinDeGroupe(GROUPE_ECHELLE);
  end;
begin
  AfficherMessage('---- DessinCartouche');
  DebutDeGroupe(GROUPE_CARTOUCHE);
    DessinEchelle(FCoordCoin1.X + 0.50, FCoordCoin1.Y + 0.50, 50.00, 02.00);
  FinDeGroupe(GROUPE_CARTOUCHE);
end;

procedure TExportOdgUtils.DessinCenterLines;
var
  i: Integer;
  BP: TBaseStation;
  QX1, QY1, QX2, QY2: double;
  QS: String;
begin
  AfficherMessage('---- DessinCenterLines');
  DebutDeGroupe(GROUPE_CENTERLINES);
  for i := 1 to FMyDocDessin.GetNbBaseStations - 1 do
  begin
    BP := FMyDocDessin.GetBaseStation(i);
    if (BP.TypeStation <> 1) then
    begin
      ConversionCoordonnees(BP.PosExtr0.X, BP.PosExtr0.Y, QX1, QY1);
      ConversionCoordonnees(BP.PosStation.X, BP.PosStation.Y, QX2, QY2);
      {$IFDEF TIDBASEPOINT_AS_TEXT}
      QS := IIF(BP.IDStation <> '', STYLES_CENTERLINES_VISEES, SVG_STYLES_CENTERLINES_ANTENNES);
      {$ELSE}
      QS := IIF(BP.IDStation > 0, SVG_STYLES_CENTERLINES_VISEES, SVG_STYLES_CENTERLINES_ANTENNES);
      {$ENDIF TIDBASEPOINT_AS_TEXT}
      FODGContexte.DessinLigne(QS, QX1, QY1, QX2, QY2);
      ConversionCoordonnees(BP.PosPD.X, BP.PosPD.Y, QX1, QY1);
      ConversionCoordonnees(BP.PosPG.X, BP.PosPG.Y, QX2, QY2);
      FODGContexte.DessinLigne(SVG_STYLES_CENTERLINES_SECTIONS, QX1, QY1, QX2, QY2);
    end;
  end;
  FinDeGroupe(GROUPE_CENTERLINES);
end;
procedure TExportOdgUtils.DessinEntrees;
var
  i: Integer;
  BP: TBaseStation;
  QX1: double;
  QY1: double;
begin
  AfficherMessage('---- DessinEntrees');
  DebutDeGroupe(GROUPE_CENTERLINES_ENTREES);
  for i := 1 to FMyDocDessin.GetNbBaseStations - 1 do
  begin
    BP := FMyDocDessin.GetBaseStation(i);
    if (BP.TypeStation = 1) then
    begin
      ConversionCoordonnees(BP.PosStation.X, BP.PosStation.Y, QX1, QY1);
      FODGContexte.DessinEllipseC(SVG_STYLES_CENTERLINES_ENTREES, QX1, QY1, 100, 100);
    end;
  end;
  FinDeGroupe(GROUPE_CENTERLINES);
end;
procedure TExportOdgUtils.DessinerLesCourbes(const LeGroupe: TGroupeEntites);
var
  C: TCourbe;
  i, NbObj: Integer;
begin
  AfficherMessage('---- DessinCourbes');
  NbObj := FMyDocDessin.GetNbCourbes();
  if (NbObj = 0) then Exit;
  for i := 0 to NbObj - 1 do
  begin
    C := FMyDocDessin.GetCourbe(i);
    if (C.IDGroupe = LeGroupe.IDGroupeEntites) then TracerCourbe(C);
  end;
end;
procedure TExportOdgUtils.DessinerLesPolygones(const LeGroupe: TGroupeEntites);
var
  i, NbObj: Integer;
  PP: TPolygone;
begin
  AfficherMessage('---- DessinPolygones');
  NbObj := FMyDocDessin.GetNbPolygones();
  if (NbObj = 0) then Exit;
  for i := 0 to NbObj - 1 do
  begin
    PP := FMyDocDessin.GetPolygone(i);
    if (PP.IDGroupe = LeGroupe.IDGroupeEntites) then TracerPolygone(PP);
  end;
end;
procedure TExportOdgUtils.DessinerLesScraps(const LeGroupe: TGroupeEntites);
var
  i, NbObj: Integer;
  PP: TScrap;
begin
  AfficherMessage('---- DessinScraps');
  NbObj := FMyDocDessin.GetNbScraps();
  if (NbObj = 0) then Exit;
  for i := 0 to NbObj - 1 do
  begin
    PP := FMyDocDessin.GetScrap(i);
    if (PP.IDGroupe = LeGroupe.IDGroupeEntites) then TracerScrap(PP);
  end;

end;

procedure TExportOdgUtils.DessinerLesPolyLignes(const LeGroupe: TGroupeEntites);
var
  i, NbObj: Integer;
  PP: TPolyLigne;
begin
  AfficherMessage('---- DessinPolylignes');
  NbObj := FMyDocDessin.GetNbPolylignes();
  if (NbObj = 0) then Exit;
  for i := 0 to NbObj - 1 do
  begin
    PP := FMyDocDessin.GetPolyligne(i);
    if (PP.IDGroupe = LeGroupe.IDGroupeEntites) then TracerPolyline(PP);
  end;

end;

procedure TExportOdgUtils.DessinerLesSymboles(const LeGroupe: TGroupeEntites);
var
  i, NbObj: Integer;
begin
  AfficherMessage('---- DessinSymboles');
  NbObj := FMyDocDessin.GetNbSymboles();

end;

procedure TExportOdgUtils.DessinerLesTextes(const LeGroupe: TGroupeEntites);
var
  i, NbObj: Integer;
  TT: TTextObject;
begin
  AfficherMessage('---- DessinTextes');
  NbObj := FMyDocDessin.GetNbTextes();
  if (NbObj = 0) then Exit;
  for i := 0 to NbObj - 1 do
  begin
    TT := FMyDocDessin.GetTexte(i);
    if (TT.IDStyleTexte = notDEBUG) then Continue; // zap des textes d'aide-mémoire
    if (TT.IDGroupe = LeGroupe.IDGroupeEntites) then TracerTexte(TT);
  end;
end;

procedure TExportOdgUtils.DessinerLesLignes(const LeGroupe: TGroupeEntites);
var
  LL: TSimpleLigne;
  i, Nb: Integer;
begin
  AfficherMessage('---- DessinLignes');
  Nb := FMyDocDessin.GetNbSimpleLignes();
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    LL := FMyDocDessin.GetSimpleLigne(i);
    if (LL.IDGroupe = LeGroupe.IDGroupeEntites) then TracerSimpleLigne(LL);
  end;
end;
// dessin des groupes
procedure TExportOdgUtils.DessinerGroupe(const MyGroupe: TGroupeEntites);
begin
  AfficherMessage(Format('%s.DessinerGroupe = %d: %s',[ClassName, MyGroupe.IDGroupeEntites, MyGroupe.NomGroupe]));
  DebutDeGroupe(MyGroupe.NomGroupe);
    DessinerLesScraps(MyGroupe); // Rend le doc illisible avec LibreOffice 4, OK avec OpenOffice
    if (not FDoExportScrapsOnly) then
    begin
      DessinerLesPolygones(MyGroupe);
      DessinerLesPolylignes(MyGroupe);
      DessinerLesCourbes(MyGroupe);
      DessinerLesLignes(MyGroupe);
      DessinerLesTextes(MyGroupe);
      DessinerLesSymboles(MyGroupe);
    end;
  FinDeGroupe(MyGroupe.NomGroupe);
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// conversion de coordonnées
// TODO : Enrichir cette fonction
procedure TExportOdgUtils.ConversionCoordonnees(const QX, QY: double; out CX, CY: double);
var
  FX, FY: Extended;
begin
  FX := QX - FCoordCoin1.X;
  FY := QY - FCoordCoin1.Y;
  CX := FX * FEchelle;
  CY := (FHauteur - FY) * FEchelle;
end;

procedure TExportOdgUtils.DebutDeGroupe(const NomGroupe: string);
begin
  FLeGroupeCourant := NomGroupe;
  FODGContexte.DebutGroupe(NomGroupe);
end;

procedure TExportOdgUtils.FinDeGroupe(const NomGroupe: string);
begin
  if (FLeGroupeCourant <> NomGroupe) then
    AfficherMessageErreur(Format('Mismatch groupe: %s, must be %s', [NomGroupe, FLeGroupeCourant]));
  FODGContexte.FinGroupe(FLeGroupeCourant);
end;


end.

