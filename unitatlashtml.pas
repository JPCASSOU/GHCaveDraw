unit UnitAtlasHTML;

// 15/09/2015: Première version OK: Génère un atlas déjà utilisable
// 01/10/2015: Utilisation des CSS
// 02/10/2015: Pages d'atlas de forme rectangulaire OK
// 15/02/2016: Personnalisation des notes de copyright
{$mode delphi}

interface

uses
  GHCD_Types,
  GeneralFunctions,
  UnitDocDessin,
  Graphics,
  UnitTGHCaveDrawDrawingContext,
  Classes, SysUtils
  , math
  , FileUtil
  , BGRABitmap
  , BGRABitmapTypes
  , BGRACanvas
  , BGRAPhongTypes
  , BGRAGradients
  , Forms
  , Dialogs
  ;

type

{ TGenerateurAtlasHTML }

 TGenerateurAtlasHTML = class
  private
    FDocumentDessin: TDocumentDessin;
    FDossierAtlas  : string;
    FRedessinInProcess : boolean;
    FTextures      : TTexturesPolygonObject;
    FParamsVue2D   : TParamsVue2D;
    FDoListerPOI   : boolean;

    FpFileDocHTML      : TextFile;
    // nombre de carrés en X et Y
    FNbCarresSurX      : integer;
    FNbCarresSurY      : integer;
    // procédure d'indication d'avancement
    FProcAfficherProgression: TProcAfficherProgression;
    // étapes de la progression
    FProgressPosition: integer;
    FProgressTotal   : integer;
    // la fonction CalcNbCarres a été appelée ?
    FCalcNbCarresDone: boolean;
    // taille du carré de la page d'atlas
    FTailleCarreEnPixelsX  : integer;
    FTailleCarreEnPixelsY  : integer;
    FEtendueCarreEnMetresX: double;
    FEtendueCarreEnMetresY: double;
    // doit-on insérer le copyright
    FDoDrawCopyright: boolean;
    FStrCopyright: string;
    // fonctions de rappel
    FProcDoAbortQuestionON : TProcAbortQuestionON;


    function  QExporterTopoEnImage(const QFileName: string; const LimitesDessin: TRect2Df; const ImgWidth, ImgHeight: integer): boolean;
    procedure GenererWaitingPage(const QFileName: string; const L, C: integer);
    procedure GenererUnePageAtlas(const L, C: integer; const LargeurReelle, HauteurReelle: double);
    function InsererLienVersFichier(const URL: string; const Desc: string; const IsLienInCell: boolean): string;
    function InsertImageAvecLienInHTML(const URL: string; const NomImage: string; const w, h: integer): string;
    function  InsertImageInHTML(const NomImage: string; const w, h: integer): string;
    procedure OpenHTMLDoc(const QFileName: string; const QTitre: string; const BorderWidth: integer);
    procedure CloseHTMLMainPage(const QFileName: string);
    procedure CloseHTMLPageAtlas(const QFileName: string);

    procedure BeginCSSEntityStyleClass(const S: string);
    procedure EndCSSEntityStyleClass(const S: string);


    procedure WriteCSSCleValeurCommentaire(const K, V, Comment: string);
    procedure WriteCSSCleValeurEnPixels(const K: string; const V: integer);
    procedure WriteCSSCleValeurCouleur(const K: string; const C: TColor);

    procedure WriteLineHTML(const S: string);
    function  RemoveHashTag(const S: string): string;
  public
    function Initialise(const FD : TDocumentDessin;
                        const QDossierDestAtlas: string;
                        const DoDrawCopyright: boolean;
                        const StrCopyright: string;
                        const FP: TParamsVue2D): boolean;
    procedure SetProcDoAbort(const PA: TProcAbortQuestionON);
    procedure SetDimensionsCarres(const DimEnPixelsX, DimEnPixelsY: integer; const EtendueTopoX: double); overload;
    procedure SetTexturesPolygones(const T: TTexturesPolygonObject);
    procedure SetParamsVue2D(const FP: TParamsVue2D);
    function GetParamsVue2D(): TParamsVue2D;

    procedure SetProcAfficherProgression(const P: TProcAfficherProgression);
    procedure GenererFeuilleCSSExterne(const FichierCSS: string);
    procedure GenererMainPage();
    procedure GenererLesPagesAtlas();
    procedure GenererTopoTuileeHTML(const PlanHTML: string; const TailleTuile: integer);
    function  GetProgressionDone(): integer;
    function  GetProgressionTotal(): integer;

    procedure CalcNbCarres; // a appeler avant GenererMainPage() ou GenererLesPagesAtlas();

end;


implementation
uses DGCDummyUnit;

{ TGenerateurAtlasHTML }
const
  NAMESTYLE_MAINPAGE_HEADER     = '#header-main-page';
  NAMESTYLE_MAINPAGE_FOOTER     = '#footer-main-page';
  NAMESTYLE_PAGE_ATLAS_HEADER   = '#header-page-atlas';
  NAMESTYLE_PAGE_ATLAS_FOOTER   = '#footer-page-atlas';

  NAMESTYLE_CONTAINER           = '#container';
  NAMESTYLE_LISTE_POI           = '#ListeDesPOI';



  DOSSIER_IMAGES       = 'Images';
  NOM_IMAGE_MAIN_PAGE  = 'ImgEnsemble.png';
  TAILLE_DES_TUILES_X  = 256;
  NOM_PLAN_ENSEMBLE    = '0_PlanEnsemble.htm';
  NOM_FEUILLE_CSS      = 'Styles.css';
  CSS_LISTE_POI        = 'ListePOI';

const
  COUNTRY_CODE = 'fr';
  XMLNS        = 'http://www.w3.org/1999/xhtml';
  DTD_VERSION  = '-//W3C//DTD XHTML 1.0 Strict//EN';
  DTD_W3C      = 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd';
  BACKGROUND_MAIN_PAGE_HEADER = $F3E2A9;
  BACKGROUND_MAIN_PAGE_FOOTER = $81F7D8;
  BACKGROUND_PAGE_ATLAS_HEADER  = clAqua;
  BACKGROUND_PAGE_ATLAS_FOOTER  = clLime;
  BACKGROUND_CONTAINER_COLOR = $5555ff;
  BACKGROUND_LISTE_POI_COLOR = clCream;

  MARGES_COLOR               = $DDDDDD;
  // positionnement de l'image et de ses pads
  MAP_TOP      = 40;
  MAP_LEFT     = 0;
  PAD_WIDTH    = 25; // largeur des pads
  WIDTH_LISTE_POI = 400;

function TGenerateurAtlasHTML.Initialise(const FD: TDocumentDessin;
                                         const QDossierDestAtlas: string;
                                         const DoDrawCopyright: boolean;
                                         const StrCopyright: string;
                                         const FP: TParamsVue2D): boolean;
begin
  Result := false;
  AfficherMessage(Format('%s.Initialise()', [ClassName]));
  try
    FDocumentDessin := FD;
    FDoDrawCopyright   := DoDrawCopyright;
    FStrCopyright      := StrCopyright;

    FRedessinInProcess := false;
    FDoListerPOI       := true;
    FParamsVue2D       := FP;
    FEtendueCarreEnMetresX := 0.00;
    FEtendueCarreEnMetresY := 0.00;
    FTailleCarreEnPixelsX  := 0;
    FTailleCarreEnPixelsY  := 0;

    FDossierAtlas   := QDossierDestAtlas;
    AfficherMessage('-- Dossier destination de l''atlas: ' + QDossierDestAtlas);
    AfficherMessage('-- Dossier contenant le dessin en cours: ' + FD.GetDossierContenantDoc());
    //************************
    Result := True;
  except
  end;
end;
// pour les atlas avec images carrées
procedure TGenerateurAtlasHTML.SetDimensionsCarres(const DimEnPixelsX, DimEnPixelsY: integer;
                                                   const EtendueTopoX: double); overload;
begin
  FTailleCarreEnPixelsX  := DimEnPixelsX;
  FEtendueCarreEnMetresX := EtendueTopoX;
  FTailleCarreEnPixelsY  := DimEnPixelsY;
  FEtendueCarreEnMetresY := EtendueTopoX * (DimEnPixelsY / DimEnPixelsX);
end;

procedure TGenerateurAtlasHTML.SetTexturesPolygones(const T: TTexturesPolygonObject);
begin
  FTextures := T;
end;

procedure TGenerateurAtlasHTML.SetParamsVue2D(const FP: TParamsVue2D);
begin
  FParamsVue2D := FP;
end;
function TGenerateurAtlasHTML.GetParamsVue2D(): TParamsVue2D;
begin
  Result := FParamsVue2D;
end;
procedure TGenerateurAtlasHTML.SetProcAfficherProgression(const P: TProcAfficherProgression);
begin
  FProcAfficherProgression := P;
end;

procedure TGenerateurAtlasHTML.SetProcDoAbort(const PA: TProcAbortQuestionON);
begin
  FProcDoAbortQuestionON := PA;
end;


procedure TGenerateurAtlasHTML.GenererMainPage();
var
  QC1: TPoint3Df;
  QC2: TPoint3Df;
  EWE: TRect2Df;
  DossierImages: String;
  FichierIndexHTML: String;
begin
  // préparer les dossiers
  DossierImages := FDossierAtlas + PathDelim + DOSSIER_IMAGES;
  ForceDirectories(DossierImages);
  // générer l'image principale
  AfficherMessage(Format('%s.GenererMainPage: ', [ClassName]));
  AfficherMessage('-- Main image');
  FDocumentDessin.SetMiniEtMaxi;
  QC1 := FDocumentDessin.GetCoordsMini();
  QC2 := FDocumentDessin.GetCoordsMaxi();
  EWE.setFrom(QC1, QC2);
  self.QExporterTopoEnImage(DossierImages + PathDelim + NOM_IMAGE_MAIN_PAGE,
                           EWE,
                           FTailleCarreEnPixelsX,
                           FTailleCarreEnPixelsY);
  Application.ProcessMessages;
  AfficherMessage('-- Done');
end;


procedure TGenerateurAtlasHTML.CalcNbCarres;
var
  QC1, QC2: TPoint3Df;
  dx, dy  : Extended;
  QEtendueCarreEnMetresX: Double;
  QEtendueCarreEnMetresY: Double;
begin
  try
    FDocumentDessin.SetMiniEtMaxi();
    QC1 := FDocumentDessin.GetCoordsMini();
    QC2 := FDocumentDessin.GetCoordsMaxi();
    dx  := QC2.X - QC1.X;
    dy  := QC2.Y - QC1.Y;
    QEtendueCarreEnMetresX := FEtendueCarreEnMetresX;
    QEtendueCarreEnMetresY := FEtendueCarreEnMetresY;

    FNbCarresSurX := 1 + trunc(dx / QEtendueCarreEnMetresX);
    FNbCarresSurY := 1 + trunc(dy / QEtendueCarreEnMetresY);
    FProgressTotal := FNbCarresSurX * FNbCarresSurY;
    FCalcNbCarresDone := True;
  except
  end;
end;

procedure TGenerateurAtlasHTML.GenererLesPagesAtlas();
var
  QC1, QC2   : TPoint3Df;
  dx, dy     : Extended;
  L, C       : integer;
  FProgressMin: Integer;
  QEtendueCarreEnMetresX: Double;
  QEtendueCarreEnMetresY: Double;
  EWE: Boolean;
begin
  AfficherMessage(Format('%s.GenererLesPagesAtlas: ', [ClassName]));
  if (not FCalcNbCarresDone) then
  begin
    AfficherMessageErreur(' -- La fonction CalcNbCarres() doit être appelée avant cette opération');
    exit;
  end;
  if (IsZero(FEtendueCarreEnMetresX * FEtendueCarreEnMetresY) OR
     ((FTailleCarreEnPixelsX * FTailleCarreEnPixelsY ) = 0)) then
  begin
    AfficherMessageErreur(' -- La fonction SetDimensionsCarres() doit être appelée avant cette opération');
    exit;
  end;
  // générer la feuille de style externe
  GenererFeuilleCSSExterne(NOM_FEUILLE_CSS);

  QEtendueCarreEnMetresX := FEtendueCarreEnMetresX;
  QEtendueCarreEnMetresY := FEtendueCarreEnMetresY;
  // générer la page d'essai
  for L := 0 to FNbCarresSurY - 1 do
  begin
    for C := 0 to FNbCarresSurX - 1 do
    begin
      FProgressPosition := L * FNbCarresSurX + C;
      GenererWaitingPage(NOM_PLAN_ENSEMBLE, L, C);
      GenererUnePageAtlas(L, C, QEtendueCarreEnMetresX, QEtendueCarreEnMetresY);
      if (Assigned(FProcAfficherProgression)) then FProcAfficherProgression('Style sheet', FProgressMin, FProgressTotal, FProgressPosition);
      EWE := ((L = 0) and (C = 0));
      EWE := EWE and (Assigned(FProcDoAbortQuestionON));
      if (EWE) then
      begin
        if (not FProcDoAbortQuestionON) then Exit;
      end;
      Application.ProcessMessages;
    end;
  end;

  // générer la topo d'ensemble
  GenererTopoTuileeHTML(NOM_PLAN_ENSEMBLE, TAILLE_DES_TUILES_X);
  Application.ProcessMessages;
end;
// génère une page unique HTML contenant les tuiles de la topo
procedure TGenerateurAtlasHTML.GenererTopoTuileeHTML(const PlanHTML: string; const TailleTuile: integer);
var

  QFichierHTML: String;
  MonImage: String;
  MonCarreHTML: String;
  L, C: Integer;
  r: Extended;
begin
  r := FTailleCarreEnPixelsY / FTailleCarreEnPixelsX;
  AfficherMessage(Format('%s.GenererTopoTuileeHTML: %s: ', [ClassName, PlanHTML]));
  if (not FCalcNbCarresDone) then
  begin
    AfficherMessageErreur(' -- La fonction CalcNbCarres() doit être appelée avant cette opération');
    exit;
  end;
  QFichierHTML := FDossierAtlas + PathDelim + PlanHTML;
  OpenHTMLDoc(QFichierHTML, 'Plan d''ensemble', 0);

    // header
  WriteLineHTML(Format('    <div id="%s">', [RemoveHashTag(NAMESTYLE_MAINPAGE_HEADER)]));
    WriteLineHTML('<H1>Plan d''ensemble</H1>');
    WriteLineHTML('<P>Cliquer sur le plan pour zoomer sur un carré</P>');
  WriteLineHTML('    </div >');
  WriteLineHTML('<HR>');
  WriteLineHTML('  <table>');
  for L := 0 to FNbCarresSurY - 1 do
  begin
    WriteLineHTML('  <tr>');
    for C := 0 to FNbCarresSurX -1 do
    begin
      MonImage := Format('Carre%dx%d.png', [FNbCarresSurY - L - 1, C]);
      MonCarreHTML := Format('./Carre%dx%d.htm', [FNbCarresSurY - L - 1, C]);
      WriteLineHTML(Format('     <td>%s</td>',
                          [InsertImageAvecLienInHTML(MonCarreHTML, MonImage, TailleTuile, round(r * TailleTuile))]));

    end;
    WriteLineHTML('  </tr>');
  end;
  WriteLineHTML('  </table>');
  Application.ProcessMessages;
  CloseHTMLMainPage(QFichierHTML);
end;
// insérer un lien
function TGenerateurAtlasHTML.InsererLienVersFichier(const URL: string; const Desc: string; const IsLienInCell: boolean): string;
begin
  if (IsLienInCell) then result := Format('<a href="%s" style="display:block;width:%d%%;height:%d%%;">%s</a>', [URL, 100, 100, Desc])
                    else result := Format('<a href="%s">%s</a>', [URL, Desc]);

end;
// page d'atlas avec des tableaux
procedure TGenerateurAtlasHTML.GenererUnePageAtlas(const L, C: integer;
                                                   const LargeurReelle, HauteurReelle: double);
var
  QC1, QC2: TPoint3Df;
  DossierImages: String;
  FichierHTML: String;
  MonImage: String;
  RectVue : TRect2Df;
  NomDuCarre: String;
  QOldElementDrawn: TELementsDrawn;
  LS: TStringList;
  i, Nb: Integer;
  function MakeNomDuCarre(const QL, QC: integer): string;
  begin
    Result := Format('Carre%dx%d.htm', [QL, QC]);
  end;
  function MakeLienVersPageAdjacente(const QL, QC: integer): string;
  var
    WU: Boolean;
    MyURL: String;
  begin
    Result := '';
    WU := IsInRange(QL, 0, FNbCarresSurY - 1) and
          IsInRange(QC, 0, FNbCarresSurX - 1);
    if (WU) then
    begin
      MyURL := './' + MakeNomDuCarre(QL, QC);
      Result := InsererLienVersFichier(MyURL, '', true);
    end;
  end;
  procedure MakeCellules(const QID: string; const QUrl: string);
  var
    EWE: String;
  begin
    EWE := '      <div id="%s">%s</div>';
    WriteLineHTML(Format(EWE, [QID, QURL]));
  end;
begin
  // sauvegarder l'état de quelques propriétés
  QOldElementDrawn := FParamsVue2D.ElementsDrawn;
  FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedDEBUG_TEXTS,
                                                              tedDISP_PENTES,
                                                              tedIDSTATIONS,
                                                              tedIMAGES];
  //-----------
  DossierImages := FDossierAtlas + PathDelim + DOSSIER_IMAGES;
  NomDuCarre    := MakeNomDuCarre(L, C);
  FichierHTML := FDossierAtlas + PathDelim + NomDuCarre;
  AfficherMessage(Format('%s.GenererUnePageAtlas: L%dC%d - %s', [ClassName, L, C, FichierHTML]));

  QC1 := FDocumentDessin.GetCoordsMini();
  QC2 := FDocumentDessin.GetCoordsMaxi();
  // générer l'image
  MonImage := Format('Carre%dx%d.png', [L, C]);
  QC1 := FDocumentDessin.GetCoordsMini();
  QC2 := FDocumentDessin.GetCoordsMaxi();
  RectVue.setFrom(QC1.X + LargeurReelle * C, QC1.Y + HauteurReelle * L,
                  QC1.X + LargeurReelle * (1+C), QC1.Y + HauteurReelle * (1+L));

  self.QExporterTopoEnImage(DossierImages + PathDelim + MonImage, RectVue, FTailleCarreEnPixelsX, FTailleCarreEnPixelsY);
  // créer la page HTML
  OpenHTMLDoc(FichierHTML, 'Atlas', 1);
  // header
  WriteLineHTML(Format('    <div id="%s">', [RemoveHashTag(NAMESTYLE_PAGE_ATLAS_HEADER)]));

    WriteLineHTML(Format('Carré: %dx%d - (%.2f, %.2f) -> (%.2f, %.2f) - %s',
                        [L, C, RectVue.X1, RectVue.Y1, RectVue.X2, RectVue.Y2,
                         InsererLienVersFichier('./' + NOM_PLAN_ENSEMBLE, 'Plan d''ensemble', false)
                        ]));
  WriteLineHTML('    </div >');
  // contenu
  WriteLineHTML(Format('    <div id="%s">', [RemoveHashTag(NAMESTYLE_CONTAINER)]));
    MakeCellules('cell11', MakeLienVersPageAdjacente(L+1, C-1));
    MakeCellules('cell12', MakeLienVersPageAdjacente(L+1, C));
    MakeCellules('cell13', MakeLienVersPageAdjacente(L+1, C+1));

    MakeCellules('cell21', MakeLienVersPageAdjacente(L, C-1));
    WriteLineHTML(Format('      <div id="%s">%s</div>', ['cell22', InsertImageInHTML(MonImage, FTailleCarreEnPixelsX, FTailleCarreEnPixelsY)]));
    MakeCellules('cell23', MakeLienVersPageAdjacente(L, C+1));

    MakeCellules('cell31', MakeLienVersPageAdjacente(L-1, C-1));
    MakeCellules('cell32', MakeLienVersPageAdjacente(L-1, C));
    MakeCellules('cell33', MakeLienVersPageAdjacente(L-1, C+1));

    if (FDoListerPOI) then
    begin
      WriteLineHTML(Format('    <div id="%s">', [RemoveHashTag(NAMESTYLE_LISTE_POI)]));
      // on liste les groupes visibles
      LS := TStringList.Create;
      try
        if (FDocumentDessin.ExtractNomsGroupes(RectVue, LS)) then
        begin
          Nb := LS.Count;
          WriteLineHTML('      <ul>');
          for i := 0 to Nb -1 do WriteLineHTML(Format('        <li>%s</li>', [AnsiToUtf8(LS.Strings[i])]));
          WriteLineHTML('      </ul>');
        end;
      finally
        FreeAndNil(LS);//LS.Free;
      end;
      WriteLineHTML('     </div >');
    end;
  WriteLineHTML('    </div >');
  CloseHTMLPageAtlas(FichierHTML);
  // et on restaure les propriétés modifiées
  FParamsVue2D.ElementsDrawn := QOldElementDrawn;
end;

function TGenerateurAtlasHTML.GetProgressionDone(): integer;
begin
  Result := FProgressPosition;
end;

function TGenerateurAtlasHTML.GetProgressionTotal(): integer;
begin
  Result := GetProgressionTotal();
end;

// la factorisation de ce code avec la fonction TCadreDessinBGRA.RedessinEcran()
// demande le passage de nombreux paramètres supplémentaires.
function TGenerateurAtlasHTML.QExporterTopoEnImage(const QFileName: string;
                                                  const LimitesDessin: TRect2Df;
                                                  const ImgWidth, ImgHeight: integer): boolean;
var
  dx, dy: Extended;
  QImgHeight: Int64;
  TmpBuffer: TGHCaveDrawDrawingContext;
  NbGroupes: Integer;
  NbScraps: Integer;
  NbCourbes: Integer;
  NbPolyLines: Integer;
  NbPolygones: Integer;
  NbSimpleLignes: Integer;
  NbSymboles: Integer;
  NbTextes: Integer;
  NumGroupe: Integer;
  MyGroupe: TGroupeEntites;
  ii: Integer;
  QQC1: TPoint3Df;
  QQC2: TPoint3Df;
  P1, P2, PosCopyright: TPoint2Df;
begin
  dx := LimitesDessin.X2 - LimitesDessin.X1;
  dy := LimitesDessin.Y2 - LimitesDessin.Y1;
  AfficherMessage(Format('%s.("%s", (%.2f, %.2f)->(%.2f, %.2f) - w=%d, h=%d',
                        [self.ClassName,
                         QFileName,
                         LimitesDessin.X1, LimitesDessin.Y1,
                         LimitesDessin.X2, LimitesDessin.Y2,
                         ImgWidth, ImgHeight]));


  //***************************
  if (FRedessinInProcess) then
  begin
    AfficherMessageErreur(Format('[FRedessinInProcess = %s] Redessin en cours', [BoolToStr(FRedessinInProcess)]));
    exit;
  end;
  FRedessinInProcess := True;   // Armement du sémaphore
  try
    TmpBuffer:= TGHCaveDrawDrawingContext.Create(ImgWidth, ImgHeight, BGRA(Red(FParamsVue2D.BackGroundColor),
                                                 Green(FParamsVue2D.BackGroundColor),
                                                 Blue(FParamsVue2D.BackGroundColor),
                                                 255));
    try
      // paramétrage
      TmpBuffer.SetDocuTopo(FDocumentDessin);
      TmpBuffer.SetParamDessin(FParamsVue2D);
      TmpBuffer.SetTextures(FTextures);
      TmpBuffer.SetBounds(LimitesDessin.X1, LimitesDessin.Y1, LimitesDessin.X2, LimitesDessin.Y2);

      // début conventionnel
      TmpBuffer.BeginDrawing();

      //AfficherMessage(ClassName, 'Redessiner: ' + IIF(FDoDraw, '', '** Pas de document chargé **'));

      //************************
      // le dessin de la cavité
      NbGroupes      := FDocumentDessin.GetNbGroupes();
      NbScraps       := FDocumentDessin.GetNbScraps();
      NbCourbes      := FDocumentDessin.GetNbCourbes();
      NbPolyLines    := FDocumentDessin.GetNbPolylignes();
      NbPolygones    := FDocumentDessin.GetNbPolygones();
      NbSimpleLignes := FDocumentDessin.GetNbSimpleLignes();
      NbSymboles     := FDocumentDessin.GetNbSymboles();
      NbTextes       := FDocumentDessin.GetNbTextes();

      for NumGroupe := 0 to NbGroupes - 1 do
      begin
        MyGroupe := FDocumentDessin.GetGroupe(NumGroupe);
        //AfficherMessage(Format('Groupe %d: %d - %s', [NumGroupe, MyGroupe.IDGroupeEntites, MyGroupe.NomGroupe]));
        // tracer le BBX du groupe
        // on teste si le groupe est partiellement visible dans la fenêtre
        // avant de le tracer (forte accélération du dessin)
        //if (GroupeIsInView(MyGroupe) and (MyGroupe.Displayed)) then
        begin
          if (NbScraps > 0) then
          begin
            for ii:=0 to NbScraps - 1 do if (MyGroupe.Visible) then TmpBuffer.DessinerScrap(FDocumentDessin.GetScrap(ii), ii, False, MyGroupe.IDGroupeEntites);
          end;
          if (NbPolygones > 0) then
          begin
            for ii:=0 to NbPolygones - 1 do if (MyGroupe.Visible) then TmpBuffer.DessinerPolygone(FDocumentDessin.GetPolygone(ii), ii, False, MyGroupe.IDGroupeEntites);
          end;
          if (NbCourbes > 0) then
          begin
            for ii:=0 to NbCourbes - 1 do if (MyGroupe.Visible) then TmpBuffer.DessinerCourbe(FDocumentDessin.GetCourbe(ii), ii, MyGroupe.IDGroupeEntites);
          end;
          if (NbPolyLines > 0) then
          begin
            for ii:=0 to NbPolyLines - 1 do if (MyGroupe.Visible) then TmpBuffer.DessinerPolyLigne(FDocumentDessin.GetPolyligne(ii), ii, MyGroupe.IDGroupeEntites);
          end;
          if (NbSimpleLignes > 0) then
          begin
            for ii := 0 to NbSimpleLignes - 1 do if (MyGroupe.Visible) then TmpBuffer.DessinerSimpleLigne(FDocumentDessin.GetSimpleLigne(ii), false, MyGroupe.IDGroupeEntites);
          end;
          if (NbSymboles > 0) then   // Symboles OK
          begin
            for ii := 0 to NbSymboles - 1 do if (MyGroupe.Visible) then TmpBuffer.DessinerSymbole(FDocumentDessin.GetSymbole(ii), MyGroupe.IDGroupeEntites);
          end;
          if ((tedTEXTES in FParamsVue2D.ElementsDrawn) and (NbTextes > 0)) then
          begin
            for ii := 0 to NbTextes - 1 do if (MyGroupe.Visible) then TmpBuffer.DessinerTexte(FDocumentDessin.GetTexte(ii), False, MyGroupe.IDGroupeEntites);
          end;
        end;
      end; // for NoGroupe ...
      //************************
      //AfficherMessage('-- Quadrillage');
      if (tedQUADRILLES in FParamsVue2D.ElementsDrawn) then
      begin
        TmpBuffer.DrawQuadrillage();
      end;
      //AfficherMessage('-- centerlines');
      if (tedCENTERLINES in FParamsVue2D.ElementsDrawn) then
      begin
        for ii:= 0 to FDocumentDessin.GetNbBaseStations - 1 do TmpBuffer.DessinerBaseStation(FDocumentDessin.GetBaseStation(ii), 12);
      end;
      // et enfin l'échelle
      if (tedECHELLE_NORD in FParamsVue2D.ElementsDrawn) then TmpBuffer.DrawEchelleNord(ImgWidth, ImgHeight);
      // cadre englobant (limites du dessin)
      TmpBuffer.CanvasBGRA.Pen.Width := 4;
      TmpBuffer.CanvasBGRA.Pen.color := clBlue;
      QQC1 := FDocumentDessin.GetCoordsMini();
      QQC2 := FDocumentDessin.GetCoordsMaxi();
      P1.setFrom(QQC1.X, QQC1.Y);
      P2.setFrom(QQC2.X, QQC2.Y);
      TmpBuffer.DrawRectangle(P1, P2);
      // copyright éventuel
      PosCopyright.setFrom(LimitesDessin.X1 + 5, LimitesDessin.Y1 + 5);
      if (FDoDrawCopyright) then TmpBuffer.DrawCopyright(PosCopyright, FStrCopyright);
      // fin conventionnelle
      TmpBuffer.EndDrawing();
    except
    end;
    // ne pas utiliser le SaveToFile de TmpBuffer
    TmpBuffer.SaveToFileUTF8(QFileName);      // OK (Charentais: 1.2 Mo)
    //TmpBuffer.Bitmap.SaveToFile(QFileName); // OK mais ne compresse pas l'image (Charentais: 44.1 Mo :-O )
  finally
    FreeAndNil(TmpBuffer);//TmpBuffer.Free;
    FRedessinInProcess := False; // libération du sémaphore
    //lbMessages.Caption := IIF(FRedessinInProcess, 'REDESS', 'DONE');
    // ne pas mettre cette ligne avant la libération du sémaphore
    //Application.ProcessMessages;
  end;
end;

procedure TGenerateurAtlasHTML.GenererWaitingPage(const QFileName: string; const L, C: integer);
var
  QFichierHTML: String;
  QQ, QN: Integer;
begin
  QQ := FNbCarresSurX * L + C;
  QN := FNbCarresSurX * FNbCarresSurY;
  AfficherMessageErreur(Format('%s.GenererWaitingPage: L%dC%d', [ClassName, L, C]));
  QFichierHTML := FDossierAtlas + PathDelim + QFileName;
  OpenHTMLDoc(QFichierHTML, 'Atlas en cours de construction', 0);
    WriteLineHTML('<H1>GHCaveDraw reconstruit actuellement l''atlas</H1>');
    WriteLineHTML(Format('<P>Génération en cours du carré L%dC%d</P>', [L, C]));
    WriteLineHTML(Format('<P>%d carrés générés sur %d</P>', [QQ + 1, QN]));
  WriteLineHTML('<HR>');

  CloseHTMLMainPage(QFichierHTML);
  Application.ProcessMessages;
end;

procedure TGenerateurAtlasHTML.WriteLineHTML(const S: string);
begin
  WriteLn(FpFileDocHTML, S);
end;

function TGenerateurAtlasHTML.RemoveHashTag(const S: string): string;
var
  EWE: String;
begin
  EWE := S;
  System.Delete(EWE, 1, 1);
  result := EWE;
end;

procedure TGenerateurAtlasHTML.BeginCSSEntityStyleClass(const S: string);
begin
  WriteLn(FpFileDocHTML, Format('%s {', [S]));
end;
procedure TGenerateurAtlasHTML.EndCSSEntityStyleClass(const S: string);
begin
  WriteLn(FpFileDocHTML, Format('} /* %s */', [S]));
end;



procedure TGenerateurAtlasHTML.WriteCSSCleValeurCommentaire(const K, V, Comment: string);
begin
  WriteLn(FpFileDocHTML, Format('   %s: %s; /* %s */', [K, V, Comment]));
end;

procedure TGenerateurAtlasHTML.WriteCSSCleValeurCouleur(const K: string; const C: TColor);
begin
  WriteLn(FpFileDocHTML, Format('   %s: %s;', [K, ColorToHTMLColor(C)]));
end;

procedure TGenerateurAtlasHTML.WriteCSSCleValeurEnPixels(const K: string; const V: integer);
begin
  WriteLn(FpFileDocHTML, Format('   %s: %dpx;', [K, V]));
end;

function TGenerateurAtlasHTML.InsertImageInHTML(const NomImage: string; const w, h: integer): string;
begin
  Result := Format('<img src="./%s/%s" width="%d" height="%d">', [DOSSIER_IMAGES, NomImage, w, h]);
end;
function TGenerateurAtlasHTML.InsertImageAvecLienInHTML(const URL: string; const NomImage: string;  const w, h: integer): string;
begin
  Result := Format('<a href="%s"><img src="./%s/%s" width="%d" height="%d"></a>', [URL, DOSSIER_IMAGES, NomImage, w, h]);
end;

// TODO: Pour le plan d'ensemble, faire une version simplifiée.
// TODO: Feuille de style externe à faire
procedure TGenerateurAtlasHTML.OpenHTMLDoc(const QFileName: string; const QTitre: string; const BorderWidth: integer);
var
  BW: String;
  WU: string;
  QQ: Integer;
  QQX1, QQX2, QQX3: Integer;
  QQY1, QQY2, QQY3: Integer;
  procedure DefineStyleLateralPads(const QName: string; const QX, QY, QL, QH: integer; const QBgColor: TColor);
  begin
    BeginCSSEntityStyleClass(QName);
      WriteCSSCleValeurCommentaire('display', 'block', '');
      WriteCSSCleValeurCommentaire('position', 'absolute', '');
      WriteCSSCleValeurEnPixels('top', QY);
      WriteCSSCleValeurEnPixels('left', QX);
      WriteCSSCleValeurEnPixels('width', QL);
      WriteCSSCleValeurEnPixels('height', QH);
      WriteCSSCleValeurCouleur('background-color', QBgColor);
      WriteCSSCleValeurCommentaire('text-align', 'center', '');
      WriteCSSCleValeurCommentaire('border', Format('solid %dpx %s', [1, ColorToHTMLColor(clSilver)]), '');
    EndCSSEntityStyleClass(QName);
  end;
begin
  AssignFile(FpFileDocHTML, QFileName);
  ReWrite(FpFileDocHTML);
  WriteLineHTML(Format('<!DOCTYPE HTML PUBLIC "%s" "%s">', [DTD_VERSION, DTD_W3C]));

  WriteLineHTML(Format('<HTML xmlns="%s" lang="%s" xml:lang="%s">', [XMLNS, COUNTRY_CODE, COUNTRY_CODE]));

  WriteLineHTML('<HEAD>');
  WriteLineHTML(Format('  <TITLE>%s</TITLE>', [QTitre]));
  WriteLineHTML(Format('  <meta http-equiv="%s" content="%s" />', ['Content-Type', 'text/html; charset=UTF-8']));
  WriteLineHTML(Format('  <LINK REL=stylesheet HREF="./%s" TYPE="%s">', [NOM_FEUILLE_CSS, 'text/css']));
  WriteLineHTML('</HEAD>');
  WriteLineHTML('<BODY>');
end;


procedure TGenerateurAtlasHTML.CloseHTMLMainPage(const QFileName: string);
var
  NoteDeLicence: String;
begin
  if (FDoDrawCopyright) then NoteDeLicence := FStrCopyright
                        else NoteDeLicence := MENTION_CC_BY_SA;
  try
    WriteLineHTML(Format('<div id="%s">', [RemoveHashTag(NAMESTYLE_MAINPAGE_FOOTER)]));
    WriteLineHTML(Format('<P>Generated by %s version %s - %s - Date: %s</P>' , [
                                                                     GetGHCaveDrawApplicationName(),
                                                                     GetGHCaveDrawVersion(),
                                                                     NoteDeLicence,
                                                                     DateTimeToStr(Now())
                                                                    ]));
    WriteLineHTML('</div>');
    WriteLineHTML('</BODY>');
    WriteLineHTML('</HTML>');
  finally
    CloseFile(FpFileDocHTML);
  end;
end;
procedure TGenerateurAtlasHTML.CloseHTMLPageAtlas(const QFileName: string);
var
  NoteDeLicence: string;
begin
  if (FDoDrawCopyright) then NoteDeLicence := FStrCopyright
                        else NoteDeLicence := MENTION_CC_BY_SA;
  try
    WriteLineHTML(Format('<div id="%s">', [RemoveHashTag(NAMESTYLE_PAGE_ATLAS_FOOTER)]));
    WriteLineHTML(Format('<P>Generated by %s version %s - %s - Date: %s</P>' , [
                                                                     GetGHCaveDrawApplicationName(),
                                                                     GetGHCaveDrawVersion(),
                                                                     NoteDeLicence,
                                                                     DateTimeToStr(Now())
                                                                    ]));
    WriteLineHTML('</div>');
    WriteLineHTML('</BODY>');
    WriteLineHTML('</HTML>');
  finally
    CloseFile(FpFileDocHTML);
  end;
end;

procedure TGenerateurAtlasHTML.GenererFeuilleCSSExterne(const FichierCSS: string);
var
  BW: String;
  WU: string;
  QQ: Integer;
  QQX1, QQX2, QQX3: Integer;
  QQY1, QQY2, QQY3: Integer;
  QFichierCSS: String;
  HauteurListeDesPOI: Integer;
  procedure DefineStyleLateralPads(const QName: string; const QX, QY, QL, QH: integer; const QBgColor: TColor);
    begin
      BeginCSSEntityStyleClass(QName);
        WriteCSSCleValeurCommentaire('display', 'block', '');
        WriteCSSCleValeurCommentaire('position', 'absolute', '');
        WriteCSSCleValeurEnPixels('top', QY);
        WriteCSSCleValeurEnPixels('left', QX);
        WriteCSSCleValeurEnPixels('width', QL);
        WriteCSSCleValeurEnPixels('height', QH);
        WriteCSSCleValeurCouleur('background-color', QBgColor);
        WriteCSSCleValeurCommentaire('text-align', 'center', '');
        WriteCSSCleValeurCommentaire('border', Format('solid %dpx %s', [1, ColorToHTMLColor(clSilver)]), '');
      EndCSSEntityStyleClass(QName);
    end;
begin
  AfficherMessageErreur(FichierCSS);
  (*
  ///* Feuille de style de base */
  html{
	height: 100%;
	width : 100%;
  }
  ....*)
  QFichierCSS := FDossierAtlas + PathDelim + FichierCSS;
  assignfile(FpFileDocHTML, QFichierCSS);
  try
    ReWrite(FpFileDocHTML);
    WriteLineHTML('/* Feuille de style générale */');
    // pas de marges ni de padding par défaut
    WU := '*';
    BeginCSSEntityStyleClass(WU);
      WriteLineHTML('  margin:0;');
      WriteLineHTML('  padding:0;');
    EndCSSEntityStyleClass(WU);
    WriteLineHTML('');
    WU := 'BODY';
    BeginCSSEntityStyleClass(WU);
      WriteCSSCleValeurCommentaire('background-color', '#FFFFD3', '');
      WriteCSSCleValeurCommentaire('font-family', 'arial, sans-serif', '');
      WriteCSSCleValeurCommentaire('font-size', '100%', '');
    EndCSSEntityStyleClass(WU);
    // tableaux
    WU := 'table';
    BeginCSSEntityStyleClass(WU);
      WriteCSSCleValeurCommentaire('border-collapse', 'collapse', 'Les bordures du tableau seront collées (plus joli)');
      WriteCSSCleValeurCommentaire('border', 'none', '');
      //WriteCSSCleValeurCommentaire('border', '', '');
      WriteCSSCleValeurEnPixels('spacing', 0);
      WriteCSSCleValeurEnPixels('padding', 0);
      //WriteCSSCleValeurEnPixels('',);
    EndCSSEntityStyleClass(WU);
   
    WU := 'tr';
    BeginCSSEntityStyleClass(WU);
      WriteCSSCleValeurCommentaire('border', 'none', '');
      WriteCSSCleValeurCommentaire('border-collapse', 'collapse', '');
      WriteCSSCleValeurEnPixels('spacing', 0);
    EndCSSEntityStyleClass(WU);

    WU := 'td';
    BeginCSSEntityStyleClass(WU);
      WriteCSSCleValeurCommentaire('border', 'none', '');
      WriteCSSCleValeurCommentaire('border-collapse', 'collapse', '');
    EndCSSEntityStyleClass(WU);

    // ajustage de l'image dans une case
    WU := 'td img';
    BeginCSSEntityStyleClass(WU);
      WriteCSSCleValeurCommentaire('display', 'block', 'Indispensable pour supprimer le jour entre les lignes de images');
    EndCSSEntityStyleClass(WU);
 

    // styles des div
    WU := NAMESTYLE_CONTAINER;
    BeginCSSEntityStyleClass(WU);
      WriteCSSCleValeurCommentaire('margin', '0px', '');
      WriteCSSCleValeurCommentaire('font-family', 'arial, sans-serif', '');
      WriteCSSCleValeurCommentaire(' font-size', '100%', '');
      WriteCSSCleValeurCouleur('background-color', BACKGROUND_CONTAINER_COLOR);
      //WriteCSSCleValeurCommentaire('', '', '');
    EndCSSEntityStyleClass(WU);
    WU := NAMESTYLE_LISTE_POI;
    HauteurListeDesPOI := FTailleCarreEnPixelsY + 2 * PAD_WIDTH;
    BeginCSSEntityStyleClass(WU);
      WriteCSSCleValeurCommentaire('position', 'absolute', '');
      WriteCSSCleValeurEnPixels('top', 40);
      WriteCSSCleValeurEnPixels('left', FTailleCarreEnPixelsX + 2 * PAD_WIDTH);
      WriteCSSCleValeurEnPixels('width', WIDTH_LISTE_POI);
      WriteCSSCleValeurEnPixels('height', HauteurListeDesPOI);
      WriteCSSCleValeurCouleur('background-color', BACKGROUND_LISTE_POI_COLOR);
      WriteCSSCleValeurCommentaire('border', '1px solid #000px', '');
      WriteCSSCleValeurCommentaire('overflow', 'auto', '');
      WriteCSSCleValeurCommentaire('border', 'solid 1px black', '');
    EndCSSEntityStyleClass(WU);
    WU := NAMESTYLE_MAINPAGE_HEADER;
    BeginCSSEntityStyleClass(WU);
      WriteCSSCleValeurCouleur('background-color', BACKGROUND_MAIN_PAGE_HEADER);
      WriteCSSCleValeurEnPixels('height', 80);
    EndCSSEntityStyleClass(WU);

    WU := NAMESTYLE_MAINPAGE_FOOTER;
    BeginCSSEntityStyleClass(WU);
      WriteCSSCleValeurCouleur('background-color', BACKGROUND_MAIN_PAGE_FOOTER);
      WriteCSSCleValeurCommentaire('clear', 'both', '');
    EndCSSEntityStyleClass(WU);

    WU := NAMESTYLE_PAGE_ATLAS_HEADER;
    BeginCSSEntityStyleClass(WU);
      WriteCSSCleValeurCouleur('background-color', BACKGROUND_PAGE_ATLAS_HEADER);
      WriteCSSCleValeurEnPixels('height', 40);
    EndCSSEntityStyleClass(WU);

    WU := NAMESTYLE_PAGE_ATLAS_FOOTER;
    BeginCSSEntityStyleClass(WU);
      WriteCSSCleValeurCommentaire('position', 'absolute', '');
      WriteCSSCleValeurEnPixels('top', 40 + FTailleCarreEnPixelsY + 2 * PAD_WIDTH + 2);
      WriteCSSCleValeurEnPixels('left', 0);
      WriteCSSCleValeurEnPixels('width', FTailleCarreEnPixelsX + 2 * PAD_WIDTH + WIDTH_LISTE_POI);
      WriteCSSCleValeurEnPixels('height', 30);
      WriteCSSCleValeurCouleur('background-color', BACKGROUND_PAGE_ATLAS_FOOTER);
      WriteCSSCleValeurCommentaire('clear', 'both', '');
    EndCSSEntityStyleClass(WU);


    QQX1 := MAP_LEFT;
    QQX2 := MAP_LEFT + PAD_WIDTH;
    QQX3 := MAP_LEFT + PAD_WIDTH + FTailleCarreEnPixelsX;

    QQY1 := MAP_TOP;
    QQY2 := MAP_TOP + PAD_WIDTH;
    QQY3 := MAP_TOP + PAD_WIDTH + FTailleCarreEnPixelsY;

    DefineStyleLateralPads('#cell11', QQX1, QQY1,
                                      PAD_WIDTH, PAD_WIDTH,
                                      MARGES_COLOR);
    DefineStyleLateralPads('#cell12', QQX2, QQY1,
                                      FTailleCarreEnPixelsX, PAD_WIDTH,
                                      MARGES_COLOR);
    DefineStyleLateralPads('#cell13', QQX3, QQY1,
                                      PAD_WIDTH, PAD_WIDTH,
                                      MARGES_COLOR);
    DefineStyleLateralPads('#cell21', QQX1, QQY2,
                                      PAD_WIDTH, FTailleCarreEnPixelsY,
                                      MARGES_COLOR);
    DefineStyleLateralPads('#cell22', QQX2, QQY2,
                                      FTailleCarreEnPixelsX, FTailleCarreEnPixelsY,
                                      MARGES_COLOR);
    DefineStyleLateralPads('#cell23', QQX3, QQY2,
                                      PAD_WIDTH, FTailleCarreEnPixelsY,
                                      MARGES_COLOR);
    DefineStyleLateralPads('#cell31', QQX1, QQY3,
                                      PAD_WIDTH, PAD_WIDTH,
                                      MARGES_COLOR);
    DefineStyleLateralPads('#cell32', QQX2, QQY3,
                                      FTailleCarreEnPixelsX, PAD_WIDTH,
                                      MARGES_COLOR);
    DefineStyleLateralPads('#cell33', QQX3, QQY3,
                                      PAD_WIDTH, PAD_WIDTH,
                                      MARGES_COLOR);

  finally
    CloseFile(FpFileDocHTML);
  end;
end;



end.

