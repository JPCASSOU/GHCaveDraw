unit mainfrmViewer;

{$INCLUDE CompilationParameters.inc}
{$WARNING: Mode Viewer}

// 19/12/2014: Suppression de la gestion des onglets
//             (boguée et inutilisée)
// 25/02/2015: SetWindowVue est reportée dans TCadreDessin
// 17/06/2015: Ajout d'une fonction de sauvegarde rapide
// 29/06/2015: Le contexte de dessin utilise cdrdessincanvasBGRA
// 22/07/2015: Portage sous MacOSX
//             -- Compilation OK
//             -- TBGRABitMap OK
//             -- Exécution   KO (mauvais fonctionnement de la LCL sous Carbon)
// 23/07/2015: Fusion de deux scraps
// 24/07/2005: Mise en place des modes étendus de courbes, polylignes, scraps et polygones
//             (fusion, fission, édition, suppression, ...)
// 27/07/2015: La liste des groupes part dans CdrListeGroupes et devient un TFrame
// 03/08/2015: Possibilité de générer des scraps via une zone de texte et une commande 'Générer un scrap depuis un groupe'
// 02/10/2015: Génération d'atlas via un dialogue
// 17/10/2015: Refonte partielle de l'UI - Barres d'outils refaite
// 28/10/2015: Fusion de deux polygones - Concaténation de plusieurs courbes
// 17/11/2015: Génération de scrap depuis polylignes
// 14/04/2016: Procédure de fusion de courbes complètement réécrite
// 29/06/2016: Les functions GenererScrapDepuisArrayCourbes() et GenererScrapDepuisArrayPolylignes() retournent un booléen (OK, KO)
// 29/06/2016: Construction de scraps et polygones d'après courbes de délimitation
// 03/02/2017: Corrections diverses


interface

uses
  SysUtils
  , Classes
  {$IFDEF LANGUAGE_FRENCH}
  , UnitMessages_fr
  {$ENDIF}
  , GHCD_Types
  , GeneralFunctions
  , UnitDocDessin
  , GeometryPolygonesUtils
  , CallDialogsGHCaveDraw
  , cdrdessincanvasBGRA
  , CdrListeGroupes
  , FileUtil, curredit
  , Forms, Controls, Graphics,
  Dialogs, StdCtrls, ActnList, Menus, ComCtrls, PairSplitter, CheckLst,
  ExtCtrls, Buttons;


type

{ TMainWindow }

 TMainWindow = class(TForm)
    acQuit: TAction;
    acOpen: TAction;
    acSaveAs: TAction;
    acExportSVG: TAction;
    acRefresh: TAction;
    acStylesObjets: TAction;
    acPan2Points: TAction;
    acRechercher: TAction;
    acApropos: TAction;
    acGenererDessinOOoDraw: TAction;
    acExportXHTML: TAction;
    acPanLeft: TAction;
    acGenererCarteLeaflet: TAction;
    acExportSVGScrapsOnly: TAction;
    acExporterCenterLines: TAction;
    acCloseDocument: TAction;
    acPrint: TAction;
    acParamsVue2D: TAction;
    acDisplayPOI: TAction;
    acListerObjetsCurrGroupe: TAction;
    acGestionSupergroupes: TAction;
    acPanRight: TAction;
    acPanUp: TAction;
    acPanDown: TAction;
    acExportImgVueCourante: TAction;
    acGenererPDF: TAction;
    acExportImgAllReseau: TAction;
    acCreerUnAtlasHTML: TAction;
    acMnuEditionObjets: TAction;
    acMnuExportImages: TAction;
    acExportForSIG: TAction;
    acMesureDistance: TAction;
    acZoomPlus: TAction;
    acZoomMoins: TAction;
    acZoom2Points: TAction;
    acZoomWindow: TAction;
    acZoomAll: TAction;
    actlstMenus: TActionList;
    btnDrwObjMode29: TSpeedButton;
    btnDrwObjMode30: TSpeedButton;
    btnDrwObjMode31: TSpeedButton;
    btnDrwObjMode32: TSpeedButton;
    btnDrwObjMode33: TSpeedButton;
    btnDrwObjMode34: TSpeedButton;
    btnDrwObjMode35: TSpeedButton;
    btnDrwObjMode36: TSpeedButton;
    btnDrwObjMode37: TSpeedButton;
    btnDrwObjMode39: TSpeedButton;
    btnDrwObjMode40: TSpeedButton;
    btnDrwObjMode46: TSpeedButton;
    btnDrwObjMode47: TSpeedButton;
    btnDrwObjMode48: TSpeedButton;
    btnDrwObjMode49: TSpeedButton;
    btnDrwObjMode50: TSpeedButton;
    btnDrwObjMode51: TSpeedButton;
    btnDrwObjMode52: TSpeedButton;
    btnDrwObjMode53: TSpeedButton;
    btnDrwObjMode56: TSpeedButton;
    btnDrwObjMode59: TSpeedButton;
    btnDrwObjMode60: TSpeedButton;
    btnDrwObjMode61: TSpeedButton;
    btnDrwObjMode62: TSpeedButton;
    btnDrwObjMode63: TSpeedButton;
    CadreDessinBGRA1: TCadreDessinBGRA;

    lbSupergroupe: TLabel;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem85: TMenuItem;
    pnlSuperGroupe: TPanel;
    ListBox1: TListBox;

    imglstActionsMenus: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    mnuOutils: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem7: TMenuItem;
    mnuExportAsImage: TMenuItem;
    mnuAide: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem23: TMenuItem;
    mnuEDITION: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuAFFICHAGE: TMenuItem;
    mnuFichier: TMenuItem;
    pnlCadreDessin: TPanel;
    pnlExport: TPanel;
    pnlFichier: TPanel;
    Panel4: TPanel;
    pnlBtnControleVue: TPanel;
    lbSuperGroupeName: TStaticText;
    SpeedButton3: TSpeedButton;
    procedure acAproposExecute(Sender: TObject);
    procedure acCalcAllBoundingBoxesExecute(Sender: TObject);
    procedure acCloseDocumentExecute(Sender: TObject);
    procedure acCreerUnAtlasHTMLExecute(Sender: TObject);
    procedure acDisplayPOIExecute(Sender: TObject);
    procedure acExporterCenterLinesExecute(Sender: TObject);
    procedure acExportForSIGExecute(Sender: TObject);
    procedure acExportImgAllReseauExecute(Sender: TObject);
    procedure acExportSVGExecute(Sender: TObject);
    procedure acExportSVGScrapsOnlyExecute(Sender: TObject);

    procedure acExportVersSIGExecute(Sender: TObject);
    procedure acExportXHTMLExecute(Sender: TObject);
    procedure acGenererDessinOOoDrawExecute(Sender: TObject);
    procedure acGenererGCDFromCurrGroupeExecute(Sender: TObject);
    procedure acGestionSupergroupesExecute(Sender: TObject);
    procedure acListerObjetsCurrGroupeExecute(Sender: TObject);
    procedure acMesureDistanceExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPan2PointsExecute(Sender: TObject);
    procedure acPanDownExecute(Sender: TObject);
    procedure acPanLeftExecute(Sender: TObject);
    procedure acPanUpExecute(Sender: TObject);
    procedure acParamsVue2DExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acQuitExecute(Sender: TObject);
    procedure acReattribBasePointsExecute(Sender: TObject);
    procedure acRechercherExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acExportImgVueCouranteExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acSetModeTravailNoneExecute(Sender: TObject);
    procedure acStylesObjetsExecute(Sender: TObject);
    procedure acSymboleArriveeEauExecute(Sender: TObject);
    procedure acSymboleChouxFleursExecute(Sender: TObject);
    procedure acSymboleColonnesExecute(Sender: TObject);
    procedure acSymboleCristauxExecute(Sender: TObject);
    procedure acSymboleCupuleExecute(Sender: TObject);
    procedure acSymboleDangerExecute(Sender: TObject);
    procedure acSymboleDesobExecute(Sender: TObject);
    procedure acSymboleEntreeExecute(Sender: TObject);
    procedure acSymboleExcentriquesExecute(Sender: TObject);
    procedure acSymboleFaillesExecute(Sender: TObject);
    procedure acSymboleFistuleusesExecute(Sender: TObject);
    procedure acSymboleGouffreDeSurfaceExecute(Sender: TObject);
    procedure acSymboleGrotteDeSurfaceExecute(Sender: TObject);
    procedure acSymboleLiaisonsGroupesExecute(Sender: TObject);
    procedure acSymboleNoneExecute(Sender: TObject);
    procedure acSymbolePerteExecute(Sender: TObject);
    procedure acSymbolePhotoExecute(Sender: TObject);
    procedure acSymbolePointFixeExecute(Sender: TObject);
    procedure acSymbolePointRemarquableExecute(Sender: TObject);
    procedure acSymbolePointTopoExecute(Sender: TObject);
    procedure acSymboleStalactitesExecute(Sender: TObject);
    procedure acSymboleStalagmitesExecute(Sender: TObject);
    procedure acSymboleZeffExecute(Sender: TObject);
    procedure acTexteCotationExecute(Sender: TObject);
    procedure acTexteCotationExterieureExecute(Sender: TObject);
    procedure acTexteDefaultExecute(Sender: TObject);
    procedure acTexteLieuDitExecute(Sender: TObject);
    procedure acTexteOrdinaire1Execute(Sender: TObject);
    procedure acTexteOrdinaire2Execute(Sender: TObject);
    procedure acTexteSousTitreExecute(Sender: TObject);
    procedure acTexteTitreExecute(Sender: TObject);
    procedure acPanRightExecute(Sender: TObject);
    procedure Action1Execute(Sender: TObject);

    procedure acUndoDeleteCourbeExecute(Sender: TObject);
    procedure acUndoDeleteImageExecute(Sender: TObject);
    procedure acUndoDeletePolygoneExecute(Sender: TObject);
    procedure acUndoDeletePolyligneExecute(Sender: TObject);
    procedure acUndoDeleteScrapExecute(Sender: TObject);
    procedure acUndoDeleteSimpleLigneExecute(Sender: TObject);
    procedure acUndoDeleteSymboleExecute(Sender: TObject);
    procedure acUndoDeleteTexteExecute(Sender: TObject);
    procedure acZoom2PointsExecute(Sender: TObject);
    procedure acZoomAllExecute(Sender: TObject);
    procedure acZoomMoinsExecute(Sender: TObject);
    procedure acZoomPlusExecute(Sender: TObject);
    procedure acZoomWindowExecute(Sender: TObject);


    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure InitCaptions();

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pnlCadreDessinClick(Sender: TObject);

  private
    // nom du fichier actuel
    FNomFichierActuel: string;
    // supergroupe courant
    FCurrentIdxSuperGroupe: TIDSuperGroupe;
    //procedure AfficherEtapeTravail(const Etape: string; const Min, Max, Position: integer);
    procedure DisplayCurrentSuperGroupe(const SG: TSuperGroupe);
    procedure GenererUneEbauche(const QFileName: TStringDirectoryFileName);
    procedure LocaliserGroupe(const Grp: TGroupeEntites);



    function  ChargerLeDessin(const MonDessin: TStringDirectoryFileName): Boolean;
    procedure Redessiner(); inline;
    // remplacement d'une polygonale
    procedure DispInfoBasePoint(const B: TBaseStation);
    procedure DispInfosScrap(const S: TScrap; const IdxScrap: Int64);
    procedure DispInfosCourbe(const C: TCourbe; const IdxCourbe: Int64);
    procedure DispInfosPolyligne(const P: TPolyLigne; const IdxPolygone: Int64);

    procedure DispInfosPolygone(const P: TPolygone; const IdxPolygone: Int64);
    procedure DispInfosPonctObject(const O: TSymbole; const IdxObj: Int64);
    procedure DispInfosSimpleLine(const L: TSimpleLigne; const IdxLine: Int64);
    procedure DispInfosTextObject(const T: TTextObject; const IdxTxtObj: Int64);
    procedure InitialiserGHC(const QP: TParamsVue2D);
    procedure SetNomFichierActuel(const QFilename: TStringDirectoryFileName; const Editable: boolean);
    procedure ShowMenus(const R90: boolean);
    { private declarations }
    procedure S666(const WU: TSpeedButton; const QAction: TAction); inline;
    // fusion d'objets

  public
    { public declarations }
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}
uses
  DGCDummyUnit
  , frmJournal
  , frmParametrageAtlas
  {$IFDEF LINUX}
  {$ENDIF}

  ;
const
  INI_CURRENT_GCS                = 'CurrentCoordinatesSystem';
  INI_CODE_EPSG                  = 'CodeEPSG';
  INI_SETTINGS_WINDOWS_POSITIONS = 'WindowsPositions';
  INI_SECTION_RECENT_FILES       = 'RecentFiles';
  INI_SECTION_LAST_FILE_OPENED   = 'LastFile';
  INI_KEY_LAST_FILE_OPENED       = 'LastOpened1';

CONST DEFAULT_MODE_OPERATIONS_BOOLEENNES = 1;

{ TMainWindow }
procedure TMainWindow.S666(const WU: TSpeedButton; const QAction: TAction); inline;
begin
  WU.Glyph  := nil;
  WU.Action := QAction;
  AfficherMessageErreur(QAction.Caption);
end;

// initialisation
procedure TMainWindow.ShowMenus(const R90: boolean);
begin
  (*
  acGridSettings.Visible         := R90;
  acSaveAs.Visible                 := R90;
  acStylesObjets.Visible         := R90;
  // dessin
  acModeSelect.Visible           := R90;
  acLine.Visible                 := R90;
  acCurve.Visible                := R90;
  acPolygone.Visible             := R90;
  acPolygoneCentre.Visible       := R90;
  acSymbole.Visible              := R90;
  acTexte.Visible                := R90;
  acRecalcBBGrps.Visible         := R90;
  // impression
  acPrint.Visible                := R90;
  // vues et zoom
  acZoomWindow.Visible           := R90;
  acZoomTout.Visible             := R90;
  acPanVue.Visible               := R90;
  acRedess.Visible               := R90;
  acSelectZoom.Visible           := R90;
  // sélection
  //lbSelectionnables.Visible      := R90;
  //cmbModeSelectEntites.Visible   := R90;
  // édition
  acDeleteLast.Visible           := R90;
  // éditeur de section
  acEditerSection.Visible        := R90;
  //*)
end;





// chargement du dessin
procedure TMainWindow.SetNomFichierActuel(const QFilename: TStringDirectoryFileName; const Editable: boolean);
var
  WU: String;
begin
  // nom actuel du dessin
  FNomFichierActuel := QFilename;
  {$IFDEF TIDBASEPOINT_AS_TEXT}
    WU := 'Litteral notation (VisualTopo, Compass, Survex)';
  {$ELSE}
    WU := 'TOPOROBOT';
  {$ENDIF TIDBASEPOINT_AS_TEXT}
  self.Caption := Format(FMT_APPLICATION_TITLE,[ExtractFileName(FNomFichierActuel), IIF(Editable, 'Editor', 'Viewer'), WU]);
end;

function TMainWindow.ChargerLeDessin(const MonDessin: TStringDirectoryFileName): Boolean;
var
  FD: TDocumentDessin;
begin
  FCurrentIdxSuperGroupe := 0;
  AfficherMessage('Dessin à charger: ' + MonDessin);
  Result := False;
  CadreDessinBGRA1.DoDraw := False;
  FD := CadreDessinBGRA1.GetDocDessin();
  FD.SetProcAfficherProgression(nil); //****
  Result := CadreDessinBGRA1.ChargerInitialiserDessin(MonDessin, False);
  if Not(Result) then
  begin
    CadreDessinBGRA1.SetMsgInLbMessage(errFATAL  , 'Echec en ouverture du dessin');
    dlgProcessing.SetFocus;
    Exit;
  end
  else begin
    ShowMenus(Result);
    CadreDessinBGRA1.SetCurrentGroupeByIdx(0);
    CadreDessinBGRA1.SetCurrentNatureCourbe(nocPAROI);
    CadreDessinBGRA1.SetCurrentNatureLigne(nolPENTE);
    CadreDessinBGRA1.SetCurrentNaturePolygone(nopARGILE);
    CadreDessinBGRA1.SetCurrentNatureSymbole(nosPHOTO);
    CadreDessinBGRA1.SetCurrentNatureTexte(notDEBUG);
    CadreDessinBGRA1.RefreshVue();
    SetNomFichierActuel(MonDessin, CadreDessinBGRA1.DrawingEditable);   // nom actuel du dessin
    CadreDessinBGRA1.SetMsgInLbMessage(errNONE  , GetResourceString(rsMSG_READY));
    Result := True;
    DisplayCurrentSuperGroupe(FD.GetSuperGroupe(FCurrentIdxSuperGroupe));
  end;
  FD.SetProcAfficherProgression(nil);
end;

procedure TMainWindow.LocaliserGroupe(const Grp: TGroupeEntites);
begin
  ShowMessageFmt('Groupe: %d - %s', [Grp.IDGroupeEntites, Grp.NomGroupe]);
end;


procedure TMainWindow.InitialiserGHC(const QP: TParamsVue2D);
var
  i  : integer;
  PMG: TParamsVue2D;
  QFileName: String;
begin
  AfficherMessage(Format('%s.InitialiserGHC()', [ClassName]));
  SetNomFichierActuel('SansNom.gcd', false);
  // armement des menus
  ShowMenus(False);
  CadreDessinBGRA1.MyProcGetInfoBasePoint := DispInfoBasePoint;
  CadreDessinBGRA1.MyProcGetScrap         := DispInfosScrap;
  CadreDessinBGRA1.MyProcGetCourbe        := DispInfosCourbe;
  CadreDessinBGRA1.MyProcGetPolyligne     := DispInfosPolyligne;
  CadreDessinBGRA1.MyProcGetPolygon       := DispInfosPolygone;
  CadreDessinBGRA1.MyProcGetSimpleline    := DispInfosSimpleLine;
  CadreDessinBGRA1.MyProcGetTextObject    := DispInfosTextObject;
  CadreDessinBGRA1.MyProcGetPonctObject   := DispInfosPonctObject;
  if (CadreDessinBGRA1.CdrDrwInitialize()) then
  begin
    // paramètres de vue
    CadreDessinBGRA1.SetParametresVue2D(QP);
    CadreDessinBGRA1.SetCurrentGroupeByIdx(0);
    // index de styles courants
    CadreDessinBGRA1.SetCurrentNatureLigne(nolPENTE);
    CadreDessinBGRA1.SetCurrentNatureCourbe(nocPAROI);
    CadreDessinBGRA1.SetCurrentNaturePolygone(nopARGILE);
    CadreDessinBGRA1.SetCurrentNatureSymbole(nosPHOTO);
    CadreDessinBGRA1.SetCurrentNatureTexte(notDEBUG);
;
    // charger le dessin si spécifié dans ParamStr
    if (ParamCount = 1) then
    begin
      if (Not ChargerLeDessin(Trim(ParamStr(1)))) then NotifyError('Echec en chargement du dessin - Démarrage avec un document vierge');
    end
    else
    begin
      QFileName := GetGHCaveDrawDirectory() + 'Ebauche001.gcd';
      GenererUneEbauche(QFileName);
      if (ChargerLeDessin(QFileName)) then AfficherMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS))
                                      else NotifyError(GetResourceString(rsERR_MSG_LOAD_GCD_FAIL));
    end;
  end else
  begin
    ShowMessage('Echec du démarrage de l''éditeur graphique');
    Application.Terminate;
  end;
end;
procedure TMainWindow.GenererUneEbauche(const QFileName: TStringDirectoryFileName);
var
  LS: TStringList;
  BP: TBaseStation;
  function MakeTBaseStation(const QIDStation      : TIDBaseStation;
                            const QIDTerrain      : string;
                            const QTypeStation    : TTypeStation;
                            const QCouleur        : TColor;
                            const QPosExtr0       : TPoint3Df; // départ visée
                            const QPosStation     : TPoint3Df; // coordonnées de référence
                            const QPosPG          : TPoint3Df; //
                            const QPosPD          : TPoint3Df;
                            const QEnabled        : boolean): TBaseStation;
  begin
    Result.IDStation      := QIDStation;
    Result.IDTerrain      := QIDTerrain;
    Result.TypeStation    := QTypeStation;
    Result.Couleur        := QCouleur;
    Result.PosExtr0       := QPosExtr0;
    Result.PosStation     := QPosStation;
    Result.PosPG          := QPosPG;
    Result.PosPD          := QPosPD;
    Result.Enabled        := QEnabled;
  end;
begin
  LS := TStringList.Create;
  try
    LS.Clear;
    LS.Add('# Stub file');
    LS.Add('# Base points ');
    //CodeEPSG	27573
    LS.Add('CodeEPSG' +#9 + IntToStr(DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG));
    LS.Add(BASEPOINTMARKER);
    {$IFDEF TIDBASEPOINT_AS_TEXT}
    BP := MakeTBaseStation('1.0', '1.00B', 8, TColor(26367),
    {$ELSE}
    BP := MakeTBaseStation(100010, '1.00B', 8, TColor(26367),
    {$ENDIF TIDBASEPOINT_AS_TEXT}
                           MakeTPoint3Df(392351.45, 3286107.27, 0.00),
                           MakeTPoint3Df(392349.81, 3286106.12, 0.00),
                           MakeTPoint3Df(392350.96, 3286104.48, 0.00),
                           MakeTPoint3Df(392348.66, 3286107.76, 0.00),
                           True);
    LS.Add(BP.toLineGCP());
    {$IFDEF TIDBASEPOINT_AS_TEXT}
    BP := MakeTBaseStation('1.1', '1.00B', 8, TColor(26367),
    {$ELSE}
    BP := MakeTBaseStation(100020, '1.00C', 8, TColor(26367),
    {$ENDIF TIDBASEPOINT_AS_TEXT}
                           MakeTPoint3Df(392349.81, 3286106.12, 0.00),
                           MakeTPoint3Df(392349.65, 3286106.01, 0.00),
                           MakeTPoint3Df(392350.79, 3286104.37, 0.00),
                           MakeTPoint3Df(392348.50, 3286107.64, 0.00),
                           True);
    LS.Add(BP.toLineGCP());
    LS.Add(ENDBASEPOINTMARKER);
    LS.SaveToFile(QFileName);
  finally
    FreeAndNil(LS);//LS.Free;
  end;
end;

// fonction retournant le basepoint le plus proche
procedure TMainWindow.DispInfoBasePoint(const B: TBaseStation);
begin
  //lb;
  //FDlgStationCourante.SetInfosStation(B);
  //if not (FDlgStationCourante.Visible) then FDlgStationCourante.Show;
end;
procedure TMainWindow.DispInfosCourbe(const C: TCourbe; const IdxCourbe: Int64);
begin
  ; //if not FDlgToolEntites.Visible then FDlgToolEntites.Show;
  //FDlgToolEntites.DispInfosCourbe(C, IdxCourbe, CadreDessin1.GetCurrentGroupeIdx);
end;

procedure TMainWindow.DispInfosPolyligne(const P: TPolyLigne; const IdxPolygone: Int64);
begin

end;

procedure TMainWindow.DispInfosPolygone(const P: TPolygone; const IdxPolygone: Int64);
begin
  ; //if not FDlgToolEntites.Visible then FDlgToolEntites.Show;
  //FDlgToolEntites.DispInfosPolygone(P, IdxPolygone, CadreDessin1.GetCurrentGroupeIdx);
end;

procedure TMainWindow.DispInfosTextObject(const T: TTextObject; const IdxTxtObj: Int64);
begin
  ; //if not FDlgToolEntites.Visible then FDlgToolEntites.Show;
  //FDlgToolEntites.DispInfosTextObject(T, IdxTxtObj, CadreDessin1.GetCurrentGroupeIdx);
end;

procedure TMainWindow.DispInfosPonctObject(const O: TSymbole; const IdxObj: Int64);
begin
  pass; //if not FDlgToolEntites.Visible then FDlgToolEntites.Show;
  //ShowMessagefmt('Objet ponctuel %d',[IdxObj]);
  //;FDlgToolEntites.DispInfosPonctObject(O, IdxObj, CadreDessin1.GetCurrentGroupeIdx);
end;

procedure TMainWindow.DispInfosScrap(const S: TScrap; const IdxScrap: Int64);
begin
  pass;
end;


procedure TMainWindow.DispInfosSimpleLine(const L: TSimpleLigne; const IdxLine: Int64);
begin
  ; //if not FDlgToolEntites.Visible then FDlgToolEntites.Show;
  //FDlgToolEntites.DispInfosSimpleLine(L, IdxLine, CadreDessin1.GetCurrentGroupeIdx);
end;
//------------------------------------------------------------------------------
procedure TMainWindow.FormCreate(Sender: TObject);
begin
  //inherited;
  DefaultFormatSettings.DecimalSeparator := '.';
  {$IFDEF MSWINDOWS}
  dlgProcessing := TdlgProcessing.Create(Application);
  {$ENDIF}
  {$IFDEF LINUX}
  pass;
  {$ENDIF}
end;

procedure TMainWindow.InitCaptions();
  procedure S777(const WU: TAction; const CaptionHint: string);
  begin
    WU.Caption  := GetResourceString(CaptionHint);
    WU.Hint     := GetResourceString(CaptionHint);
  end;
begin
   mnuFichier.Caption    := GetResourceString(rsMNU_FILE);
    S777(acOpen           , rsMNU_OPEN);
    S777(acSaveAs         , rsMNU_SAVE_AS);
    S777(acCloseDocument  , rsMNU_CLOSE_DOC);
    //S777(acPrint          , rsMNU_PRINT);
    S777(acExportSVG      , rsMNU_SVG);
    S777(acExportSVGScrapsOnly, rsMNU_SVG_SCRAPS_ONLY);
    S777(acGenererDessinOOoDraw, rsMNU_ODG);
    S777(acMnuExportImages, rsMNU_EXPORT_IMAGE);
      S777(acExportImgVueCourante , rsMNU_EXPORT_IMG_CURR_VUE);
      S777(acExportImgAllReseau   , rsMNU_EXPORT_IMG_ALL);
    S777(acGenererPDF            , rsMNU_PDF);
    S777(acExportImgVueCourante  , rsMNU_VUE_IMAGE);
    S777(acExporterCenterLines   , rsMNU_EXPORT_GCP);
    s777(acCreerUnAtlasHTML      , rsMNU_GENERER_ATLAS);
    S777(acQuit                  , rsMNU_QUIT);

  //mnuDESSIN.Caption   := rsMNU_DESSIN;

  mnuEDITION.Caption  := GetResourceString(rsMNU_EDITION);
    S777(acMnuEditionObjets, rsMNU_EDIT_OBJETS);


    S777(acStylesObjets   , rsMNU_STYLES_OBJETS);
    s777(acRechercher     , rsMNU_RECHERCHER);
    S777(acGestionSupergroupes    , rsMNU_GESTION_SUPERGROUPES);

  mnuAFFICHAGE.Caption  := GetResourceString(rsMNU_AFFICHAGE);
    s777(acRefresh        , rsMNU_REDESS);
    S777(acZoomAll        , rsMNU_ZOOMALL);
    S777(acZoomWindow     , rsMNU_ZOOMWND);
    S777(acPan2Points     , rsMNU_PANVUE);
    S777(acZoomPlus       , rsMNU_ZOOM_PLUS);
    S777(acZoomMoins      , rsMNU_ZOOM_MOINS);
    S777(acPanLeft        , rsMNU_PAN_LEFT);
    S777(acPanRight       , rsMNU_PAN_RIGHT);
    S777(acPanUp          , rsMNU_PAN_UP);
    S777(acPanDown        , rsMNU_PAN_DOWN);
    S777(acParamsVue2D    , rsMNU_PARAM_VUE_2D);
  mnuAide.Caption       := GetResourceString(rsMNU_AIDE);
    S777(acApropos        , rsMNU_APROPOS);
  S777(acMesureDistance   , rsBTN_MESURE_DISTANCE);
end;

procedure TMainWindow.Redessiner();
begin
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.acQuitExecute(Sender: TObject);
begin
  self.Close;
end;

// réattribuer les points de base de tous les objets du groupe courant
// vers les basepoints métafiltrés
// Utilisation type: On a créé des objets sans avoir filtré les centerlines
//                   et on veut les accrocher aux bons basepoints
procedure TMainWindow.acReattribBasePointsExecute(Sender: TObject);
var
  FD       : TDocumentDessin;
  QIdx     : TIDGroupeEntites;
  Grp      : TGroupeEntites;
  QFiltres : String;
begin
  FD := CadreDessinBGRA1.GetDocDessin;
  QIdx := CadreDessinBGRA1.GetCurrentGroupeIdx();
  if (QIdx < 0) then Exit;
  Grp := FD.GetGroupeByIDGroupe(QIdx);
  QFiltres := CadreDessinBGRA1.GetMetaFiltre();
  if (Not QuestionOuiNon(Format('Attribuer les basepoints du groupe %d: %s' + #13#10 + 'Filtres: %s', [QIdx, Grp.NomGroupe, QFiltres]))) then Exit;
  FD.ReattribBasepointsAllObjetsGroupe(Grp, QFiltres);
  ShowMessage('Attribution OK');
  CadreDessinBGRA1.RefreshVue(); //CadreDessinBGRA1.RedessinEcran(false);
end;

// Recalcule les points de base de l'objet courant
// à partir de la certerline métafiltrée
procedure TMainWindow.acRechercherExecute(Sender: TObject);
var
  WU: String;
begin
  WU := '';
  if (InputQuery(rsMISC_DLG_FIND_STATION_TITRE, rsMISC_DLG_FIND_STATION_PROMPT, WU)) then
  begin
    CadreDessinBGRA1.RechercherStation(WU);
  end;
end;

procedure TMainWindow.acRefreshExecute(Sender: TObject);
begin
  CadreDessinBGRA1.RefreshVue();
end;



procedure TMainWindow.acExportImgVueCouranteExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
  QC1, QC2: TPoint2Df;
  EWE: TRect2Df;
  WU: String;
  QAT: double;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  QFileName := 'Vue001.png';
  QAT := 6000;
  if (InputNumValue('Génération d''une image', 'Taille de l''image', 500, 32000, QAT)) then
  begin
    if (DoDialogSaveFilename('Image PNG|*.png|Image JPG|*.jpg', '.png', QFileName)) then
    begin
      QC1 := CadreDessinBGRA1.GetDrwCoinBasGauche();
      QC2 := CadreDessinBGRA1.GetDrwCoinHautDroit();
      EWE := MakeTRect2Df(QC1.X, QC1.Y, QC2.X, QC2.Y);
      CadreDessinBGRA1.ExporterTopoEnImage(QFileName, EWE, Trunc(QAT));
      ShowMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
    end;
  end;
end;

procedure TMainWindow.acSaveAsExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  FD.SaveEncryptedFile(GetGHCaveDrawDirectory() + '_topo_encrypted.gcb', '');
end;

procedure TMainWindow.acSetModeTravailNoneExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtNONE);
end;

procedure TMainWindow.acStylesObjetsExecute(Sender: TObject);
begin
   if (CadreDessinBGRA1.DoDraw) then
  begin
    EditerStylesObjets(CadreDessinBGRA1.MyDocumentDessin, Redessiner);
    CadreDessinBGRA1.RefreshVue(); //CadreDessin1.RedessinEcran(false);
  end;
end;





procedure TMainWindow.acSymboleArriveeEauExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosARRIVEE_EAU);
end;

procedure TMainWindow.acSymboleChouxFleursExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosCONCRETION_PAROI);
end;

procedure TMainWindow.acSymboleColonnesExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosCOLONNES);
end;

procedure TMainWindow.acSymboleCristauxExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosCRISTAUX);
end;

procedure TMainWindow.acSymboleCupuleExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosCUPULES);
end;

procedure TMainWindow.acSymboleDangerExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosDANGER);
end;

procedure TMainWindow.acSymboleDesobExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosDESOB);
end;

procedure TMainWindow.acSymboleEntreeExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosENTREE);
end;

procedure TMainWindow.acSymboleExcentriquesExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosEXCENTRIQUES);
end;

procedure TMainWindow.acSymboleFaillesExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosFRACTURES);
end;

procedure TMainWindow.acSymboleFistuleusesExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosFISTULEUSE);
end;

procedure TMainWindow.acSymboleGouffreDeSurfaceExecute(Sender: TObject);
begin
   CadreDessinBGRA1.SetCurrentNatureSymbole(nosGOUFFRE_SURFACE);
end;

procedure TMainWindow.acSymboleGrotteDeSurfaceExecute(Sender: TObject);
begin
   CadreDessinBGRA1.SetCurrentNatureSymbole(nosGROTTE_SURFACE);
end;

procedure TMainWindow.acSymboleLiaisonsGroupesExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosCORRESPONDANCE);
end;

procedure TMainWindow.acSymboleNoneExecute(Sender: TObject);
begin
  ;
end;

procedure TMainWindow.acSymbolePerteExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosPERTE);
end;

procedure TMainWindow.acSymbolePhotoExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosPHOTO);
end;

procedure TMainWindow.acSymbolePointFixeExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosPOINT_FIXE);
end;

procedure TMainWindow.acSymbolePointRemarquableExecute(Sender: TObject);
begin
   CadreDessinBGRA1.SetCurrentNatureSymbole(nosPOINT_REMARQUABLE);
end;

procedure TMainWindow.acSymbolePointTopoExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosPOINT_TOPO);
end;


procedure TMainWindow.acSymboleStalactitesExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosSTALACTITES);
end;

procedure TMainWindow.acSymboleStalagmitesExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosSTALAGMITES);
end;

procedure TMainWindow.acSymboleZeffExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureSymbole(nosZEFF);
end;

procedure TMainWindow.acTexteCotationExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureTexte(notCOTATION);
end;

procedure TMainWindow.acTexteCotationExterieureExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureTexte(notCOTATION_EXTERIEURE);
end;

procedure TMainWindow.acTexteDefaultExecute(Sender: TObject);
begin

end;

procedure TMainWindow.acTexteLieuDitExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureTexte(notLIEU_DIT);
end;

procedure TMainWindow.acTexteOrdinaire1Execute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureTexte(notTEXTE1);
end;

procedure TMainWindow.acTexteOrdinaire2Execute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureTexte(notTEXTE2);
end;

procedure TMainWindow.acTexteSousTitreExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureTexte(notSOUS_TITRES);
end;

procedure TMainWindow.acTexteTitreExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureTexte(notTITRES);
end;

procedure TMainWindow.Action1Execute(Sender: TObject);
begin

end;


procedure TMainWindow.acUndoDeleteCourbeExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  FD.RestoreLastCourbeDeleted();
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.acUndoDeleteImageExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  FD.RestoreLastImageDeleted();
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.acUndoDeletePolygoneExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  FD.RestoreLastPolygoneDeleted();
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.acUndoDeletePolyligneExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  FD.RestoreLastPolylineDeleted();
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.acUndoDeleteScrapExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  FD.RestoreLastScrapDeleted();
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.acUndoDeleteSimpleLigneExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  FD.RestoreLastSimpleLineDeleted();
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.acUndoDeleteSymboleExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  FD.RestoreLastSymboleDeleted();
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.acUndoDeleteTexteExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  FD.RestoreLastTexteDeleted();
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.acPanRightExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetWindowView(1.0, -0.05, 0.0);
end;
procedure TMainWindow.acPan2PointsExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtPAN_PREMIER_POINT);
end;

procedure TMainWindow.acZoom2PointsExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtZOOM_PREMIER_COIN);
end;

procedure TMainWindow.acZoomAllExecute(Sender: TObject);
begin
  //SetModeSelectionEntites(mseNONE);
  CadreDessinBGRA1.SetModeTravail(mtNONE);
  CadreDessinBGRA1.ResetViewLimits;
end;

procedure TMainWindow.acZoomMoinsExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetWindowView(0.95, 0.0, 0.0);
end;

procedure TMainWindow.acZoomPlusExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetWindowView(1.05, 0.0, 0.0);
end;

procedure TMainWindow.acZoomWindowExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtZOOM_PREMIER_COIN);
end;

procedure TMainWindow.acOpenExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
begin
  QFileName :=  GetGHCaveDrawDirectory() + 'Dessin001.gcd';
  if (DoDialogOpenFilename('Fichiers GCD|*.gcd|Polygonales GCP|*.gcp|Tous|*.*', QFileName)) then
  begin
    if (ChargerLeDessin(QFileName)) then
    begin
      AfficherMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
    end
    else
    begin
      NotifyError(GetResourceString(rsERR_MSG_LOAD_GCD_FAIL));
    end;
  end;
end;



procedure TMainWindow.acPanDownExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetWindowView(1.0, 0.00, 0.05);
end;

procedure TMainWindow.acPanLeftExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetWindowView(1.0, 0.05, 0.00);
end;

procedure TMainWindow.acPanUpExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetWindowView(1.0, 0.00, -0.05);
end;

procedure TMainWindow.acParamsVue2DExecute(Sender: TObject);
var
  EWE: TParamsVue2D;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  EWE := CadreDessinBGRA1.GetParametresVue2D();
  if (ParametrerVue2D(EWE)) then CadreDessinBGRA1.SetParametresVue2D(EWE);
end;


procedure TMainWindow.acPrintExecute(Sender: TObject);
begin
  DisplayPrintingCenter(CadreDessinBGRA1.GetDocDessin(),
                        CadreDessinBGRA1.GetTexturesPolygones(),
                        CadreDessinBGRA1.GetParametresVue2D());
end;




procedure TMainWindow.acExportSVGExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  QFileName  := GetGHCaveDrawDirectory() + 'Svg001.svg';
  if (DoDialogSaveFilename('Fichiers SVG (*.svg)|*.svg|Tous|*.*', '.svg', QFileName)) then
  begin
    if (Not CadreDessinBGRA1.MyDocumentDessin.ExportToSvg(QFileName, False, False, '', '')) then NotifyError(rsMSG_ERR_CTXT_SVG);
    AfficherMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
  end;
end;

procedure TMainWindow.acExportSVGScrapsOnlyExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  QFileName  := GetGHCaveDrawDirectory() + 'Svg001.svg';
  if (DoDialogSaveFilename('Fichiers SVG (*.svg)|*.svg|Tous|*.*', '.svg', QFileName)) then
  begin
    if (Not CadreDessinBGRA1.MyDocumentDessin.ExportToSvg(QFileName, True, False, '', '')) then NotifyError(rsMSG_ERR_CTXT_SVG);
    AfficherMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
  end;
end;


procedure TMainWindow.acExportVersSIGExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  FD := CadreDessinBGRA1.GetDocDessin();
  DisplayDlgExportGIS(FD);
end;

procedure TMainWindow.acExportXHTMLExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  QFileName  := GetGHCaveDrawDirectory() + 'Svg001.xhtml';
  if (DoDialogSaveFilename('Fichiers SVG (XHTML) (*.xhtml)|*.xhtml|Tous|*.*', '.xhtml', QFileName)) then
  begin
    if (Not CadreDessinBGRA1.MyDocumentDessin.ExportToSvg(QFileName, False, True,
                                                      'Dessin',
                                                      'Generated by GHCaveDraw')) then NotifyError(rsMSG_ERR_CTXT_SVG);
  end;
end;


procedure TMainWindow.acGenererDessinOOoDrawExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  QFileName   := 'PlanTopo001.odg';
  if (DoDialogSaveFilename('OpenOffice / LibreOffice Draw (*.odg)|*.odg', '.odg', QFileName)) then
  begin
    CadreDessinBGRA1.MyDocumentDessin.ExportToODG(QFileName, false);
  end;
end;

procedure TMainWindow.acGenererGCDFromCurrGroupeExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
  EWE: TIDGroupeEntites;
  FD : TDocumentDessin;
begin
  EWE := CadreDessinBGRA1.GetCurrentGroupeIdx();
  FD  := CadreDessinBGRA1.GetDocDessin();
  QFileName := Format('Groupe_%d', [EWE]);
  if (DoDialogSaveFilename('Fichier GCD|*.gcd|Fichier Include (*.inc)', '.gcd', QFileName)) then
  begin
    FD.ExporterUnGroupeEnGCD(EWE, QFileName);
  end;
end;



procedure TMainWindow.acGestionSupergroupesExecute(Sender: TObject);
var
  QIdx: TIDSuperGroupe;
  QTodo: byte;
begin
  QIdx := FCurrentIdxSuperGroupe;
  DisplaySuperGroupes(CadreDessinBGRA1.GetDocDessin(), QIdx, QTodo);
  CadreDessinBGRA1.ActualiserCmbListeDesGroupes();
  CadreDessinBGRA1.Vue.Invalidate;
end;



procedure TMainWindow.acListerObjetsCurrGroupeExecute(Sender: TObject);
var
  FD  : TDocumentDessin;
  QIdx: TIDGroupeEntites;

begin
  FD := CadreDessinBGRA1.GetDocDessin;
  QIdx := CadreDessinBGRA1.GetCurrentGroupeIdx();
  if (QIdx < 0) then Exit;
  DisplayObjetsOfThisGroupe(FD, QIdx);
end;


procedure TMainWindow.acMesureDistanceExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtMESURE_DISTANCE_SURFACE);
end;



procedure TMainWindow.acExporterCenterLinesExecute(Sender: TObject);
var
  FD: TDocumentDessin;
  QFileName: TStringDirectoryFileName;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  FD := CadreDessinBGRA1.GetDocDessin();
  if (DoDialogSaveFilename('Fichiers GCP|*.gcp', '.gcp', QFileName)) then
  begin
    FD.ExportBasePoints(QFileName);
    AfficherMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
  end;
end;

procedure TMainWindow.acExportForSIGExecute(Sender: TObject);
begin

end;

procedure TMainWindow.acExportImgAllReseauExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
  QC1, QC2: TPoint3Df;
  EWE: TRect2Df;
  FD: TDocumentDessin;
  QAT: double;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  QAT := 4000;
  if (InputNumValue('Génération d''une image', 'Taille de l''image', 500, 32000, QAT)) then
  begin
    QFileName := 'Vue001.png';
    if (DoDialogSaveFilename('Image PNG|*.png|Image JPG|*.jpg', '.png', QFileName)) then
    begin
      FD := CadreDessinBGRA1.GetDocDessin();
      //CadreDessinBGRA1.SaveVueAsImage(QFileName);
      FD.SetMiniEtMaxi;
      QC1 := FD.GetCoordsMini();
      QC2 := FD.GetCoordsMaxi();
      EWE := MakeTRect2Df(QC1.X, QC1.Y, QC2.X, QC2.Y);
      CadreDessinBGRA1.ExporterTopoEnImage(QFileName, EWE, Trunc(QAT));
      ShowMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
    end;
  end;
end;


procedure TMainWindow.acCreerUnAtlasHTMLExecute(Sender: TObject);
var
  PD: TdlgParametrageAtlas;
begin
  PD := TdlgParametrageAtlas.Create(application);
  try
    PD.Initialiser(CadreDessinBGRA1.GetDocDessin(),
                   CadreDessinBGRA1.GetTexturesPolygones(),
                   CadreDessinBGRA1.GetParametresVue2D());
    PD.ShowModal;
  finally
    PD.Release;
  end;
end;



procedure TMainWindow.acDisplayPOIExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  FD := CadreDessinBGRA1.GetDocDessin();
  DisplayPOI(FD);
end;



procedure TMainWindow.acCloseDocumentExecute(Sender: TObject);
var
  EWE: TParamsVue2D;
begin
  EWE := CadreDessinBGRA1.GetParametresVue2D();
  if (QuestionOuiNon('Fermer le document courant')) then InitialiserGHC(EWE);
end;

procedure TMainWindow.acAproposExecute(Sender: TObject);
begin
  DisplayHelpSystem(CadreDessinBGRA1.GetDocDessin, '', True);
end;

procedure TMainWindow.acCalcAllBoundingBoxesExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  FD := CadreDessinBGRA1.GetDocDessin;
  FD.CalcBoundingBoxAllGroupes;
  CadreDessinBGRA1.RefreshVue();
end;



procedure TMainWindow.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Canclose := QuestionOuiNon(rsMSG_QUIT_VIEWER);
end;



//******************************************************************************

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  dlgProcessing.Release;
  {$ENDIF}
  {$IFDEF MAC_OS_X_VERSION_MIN_REQUIRED}
  dlgProcessing.Release;
  {$ENDIF}
  {$IFDEF LINUX}
  ;
  {$ENDIF}
end;




procedure TMainWindow.FormShow(Sender: TObject);
var
  QParamsVue2D: TParamsVue2D;
begin
  InitCaptions;
  {$IFDEF MSWINDOWS}
  dlgProcessing.Show;
  EnvoyerFenetreVersMoniteur(dlgProcessing, -1, 100, 100, 640, 480);
  EnvoyerFenetreVersMoniteur(self, 0, 10, 10, Screen.Width -20, Screen.Height - 100);
  {$ENDIF}
  {$IFDEF MAC_OS_X_VERSION_MIN_REQUIRED}
  dlgProcessing.Show;
  EnvoyerFenetreVersMoniteur(dlgProcessing, -1, 100, 100, 640, 480);
  EnvoyerFenetreVersMoniteur(self, 0, 10, 10, Screen.Width -20, Screen.Height - 100);
  {$ENDIF}
  {$IFDEF LINUX}
  ;
  {$ENDIF}

  AfficherMessage('Démarrage de GHCaveDraw');
  self.Width    := Screen.Width - 60;
  self.Height   := Screen.Height - 100;
  QParamsVue2D.ElementsDrawn       := [tedCENTERLINES                    // dessin des centerlines
                                  //  ,tedECHELLE_NORD                   // dessin de l'échelle et du nord
                                  //  ,tedIDSTATIONS                     // dessiner les stations
                                      ,tedQUADRILLES                     // dessiner les quadrillages
                                      ,tedDEBUG_TEXTS                    // afficher les textes de débug.
                                      ,tedTEXTES                         // afficher les textes du dessin
                                      //,tedCADRES_PHOTO                 // afficher les cadres des photos
                                      ,tedSCRAPS                         // afficher les scraps
                                      ,tedDETAILS                        // détails
                                      //,tedBBXGroupes                   // bounding box groupes
                                  //  ,tedDISP_PENTES                    // pentes supérieures à une limite donnée
                                  //  ,tedIMAGES                         // images
                                      ];
  QParamsVue2D.GrdTypeQuadrillage := tqGRID;
  QParamsVue2D.BackGroundColor    := clWhite;
  QParamsVue2D.GrdMainGridColor   := RGBToColor(255, 128,  64);//clGray;
  QParamsVue2D.GrdSecGridColor    := RGBToColor(242, 203, 159); // (//clSilver;
  QParamsVue2D.GrdSpcMainGrid     := 100.00;
  QParamsVue2D.GrdSpcSecGrid      := QParamsVue2D.GrdSpcMainGrid / 10;
  QParamsVue2D.GrdDispMainGrid    := True;
  QParamsVue2D.GrdDispSecGrid     := True;
  QParamsVue2D.GroupeBackColor    := RGBToColor(205, 254, 239);
  QParamsVue2D.GrdCrossSize       := QParamsVue2D.GrdSpcMainGrid / 20;
  QParamsVue2D.DoDrawScrapsMonochromes     := false;
  QParamsVue2D.ColorScrapMonochrome        := clGray;
  QParamsVue2D.PenteLimiteDisp             := 5.00;
  QParamsVue2D.RegleDisplayed              := true;
  QParamsVue2D.TailleEchelle               := 50.00;

  InitialiserGHC(QParamsVue2D);


end;

procedure TMainWindow.pnlCadreDessinClick(Sender: TObject);
begin

end;

procedure TMainWindow.DisplayCurrentSuperGroupe(const SG: TSuperGroupe);
begin
  lbSuperGroupeName.Caption := SG.NomSuperGroupe;
end;

end.

