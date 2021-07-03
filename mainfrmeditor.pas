unit MainFrmEditor;

{$INCLUDE CompilationParameters.inc}

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
  //, process
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
    aclstBoutons: TActionList;
    acSetModeTravailNone: TAction;
    acNouvelObjet: TAction;
    acEditObjet: TAction;
    acNewCourbe: TAction;
    acNewLigne: TAction;
    acNewPolygone: TAction;
    acNewSymbole: TAction;
    acNewTexte: TAction;
    acNewNone: TAction;
    acEditCourbe: TAction;
    acEditLigne: TAction;
    acEditPolygone: TAction;
    acEditSymbole: TAction;
    acEditTexte: TAction;
    acRefresh: TAction;
    acMoveObject: TAction;
    acCourbeNone: TAction;
    acCourbeParois: TAction;
    acCourbeParoisCachees: TAction;
    acCourbeEcoulements: TAction;
    acCourbeLignesPentes: TAction;
    acCourbeRessauts: TAction;
    acCourbeChenalDeVoute: TAction;
    acPolygoneNone: TAction;
    acPolygoneLac: TAction;
    acCourbeSurplombs: TAction;
    acCourbeMiniRessaut: TAction;
    acPolygoneArgile: TAction;
    acPolygoneArgileGalets: TAction;
    acPolygoneChemins: TAction;
    acSymboleNone: TAction;
    acPolygoneSable: TAction;
    acPolygoneBlocs: TAction;
    acPolygoneGalets: TAction;
    acPolygoneNeige: TAction;
    acPolygoneSilhouettes: TAction;
    acPolygoneGrosBloc: TAction;
    acPolygoneGour: TAction;
    acPolygoneSiphon: TAction;
    acSymbolePhoto: TAction;
    acSymboleColonnes: TAction;
    acSymboleStalagmites: TAction;
    acSymboleCristaux: TAction;
    acSymboleFailles: TAction;
    acSymboleEntree: TAction;
    acSymbolePointTopo: TAction;
    acSymbolePointFixe: TAction;
    acSymboleLiaisonsGroupes: TAction;
    acSymboleFistuleuses: TAction;
    acSymboleChouxFleurs: TAction;
    acSymboleExcentriques: TAction;
    acSymboleStalactites: TAction;
    acLigneNone: TAction;
    acLigneFleche: TAction;
    acLigneFracture: TAction;
    acLigneSuiteReseau: TAction;
    acLignePentes: TAction;
    acDeleteObject: TAction;
    acStylesObjets: TAction;
    acPan2Points: TAction;
    acChargerCanevasTopo: TAction;
    acTexteTitre: TAction;
    acTexteSousTitre: TAction;
    acTexteCotation: TAction;
    acTexteOrdinaire1: TAction;
    acTexteOrdinaire2: TAction;
    acTexteDefault: TAction;
    acRechercher: TAction;
    acApropos: TAction;
    acSymboleCupule: TAction;
    acSymboleZeff: TAction;
    acSymboleArriveeEau: TAction;
    acSymbolePerte: TAction;
    acSymboleDesob: TAction;
    acGenererDessinOOoDraw: TAction;
    acNewFreeHandCurve: TAction;
    acExportXHTML: TAction;
    Action1: TAction;
    acPanLeft: TAction;
    acNewPolylign: TAction;
    acEditPolylign: TAction;
    acNettoyerObjetsProblematiques: TAction;
    acMnuRestoreLastObject: TAction;
    acGenererCarteLeaflet: TAction;
    acExportSVGScrapsOnly: TAction;
    acSaveWOCenterlines: TAction;
    acExporterCenterLines: TAction;
    acOpenCaveMap: TAction;
    acExportToTherion: TAction;
    acCourbeMurMaconne: TAction;
    acExportVersSIG: TAction;
    acCourbeParoiFracassee: TAction;
    acCloseDocument: TAction;
    acGenererGCDFromCurrGroupe: TAction;
    acPrint: TAction;
    acSaveAnCopy: TAction;
    acParamsVue2D: TAction;
    acMakePolygoneFrom4Curves: TAction;
    acSymboleGouffreDeSurface: TAction;
    acSymbolePointRemarquable: TAction;
    acSymboleGrotteDeSurface: TAction;
    acSymboleDanger: TAction;
    acMesureDistance: TAction;
    acTexteCotationExterieure: TAction;
    acDisplayPOI: TAction;
    acListerObjetsCurrGroupe: TAction;
    acGestionSupergroupes: TAction;
    acAddCurrCourbeAtListForScraps: TAction;
    acAddCurrPolylineAtListForScraps: TAction;
    acGenererScrapFromCourbes: TAction;
    acGenererScrapFromPolylines: TAction;
    acDummy: TAction;
    acUndoDeleteImage: TAction;
    acUndoDeleteTexte: TAction;
    Action2: TAction;
    Action3: TAction;
    acPanRight: TAction;
    acPanUp: TAction;
    acPanDown: TAction;
    acExportImgVueCourante: TAction;
    acQSave: TAction;
    acNewScrap: TAction;
    acEditScrap: TAction;
    acNettoyerGroupes: TAction;
    acGenererPDF: TAction;
    acCalcAllBoundingBoxes: TAction;
    acCourbeParoiIncertaine: TAction;
    acMergeScraps: TAction;
    acMergeLastScraps: TAction;
    acDeleteLastScrap: TAction;
    acDeleteLastCourbe: TAction;
    acDeleteLastPolyline: TAction;
    acDeleteLastPolygon: TAction;
    acDeleteLastSymbole: TAction;
    acDeleteLastTexte: TAction;
    acDeleteLastSimpleLine: TAction;
    acMergeCourbes: TAction;
    acMergeLastCourbes: TAction;
    acMergePolylines: TAction;
    acMergeLastPolylines: TAction;
    acMergePolygones: TAction;
    acMergeLastPolygones: TAction;
    acGenererScrapDepuisGroupe: TAction;
    acExportImgAllReseau: TAction;
    Action4: TAction;
    acCreerUnAtlasHTML: TAction;
    acDialogimage: TAction;
    acNewImage: TAction;
    acNouveauDessin: TAction;
    acTexteLieuDit: TAction;
    acEditerLesScraps: TAction;
    acMnuEditionObjets: TAction;
    acSubMenuEditScrap: TAction;
    acSubMenuEditCourbe: TAction;
    acSubMenuEditPolyline: TAction;
    acSubMenuEditSimpleLine: TAction;
    acSubMenuEditSymbole: TAction;
    acSubMenuEditTexte: TAction;
    acSubMenuEditPolygone: TAction;
    acMnuDeleteObjets: TAction;
    acMnuExportImages: TAction;
    acReattribBasePoints: TAction;
    acDeleteallObjGroupe: TAction;
    acUndoDeleteCourbe: TAction;
    acUndoDeletePolyligne: TAction;
    acUndoDeletePolygone: TAction;
    acUndoDeleteSimpleLigne: TAction;
    acUndoDeleteSymbole: TAction;
    acUndoDeleteScrap: TAction;
    acZoomPlus: TAction;
    acZoomMoins: TAction;
    acZoom2Points: TAction;
    acZoomWindow: TAction;
    acZoomAll: TAction;
    actlstMenus: TActionList;
    btnDrwObjMode1: TSpeedButton;
    btnDrwObjMode10: TSpeedButton;
    btnDrwObjMode11: TSpeedButton;
    btnDrwObjMode12: TSpeedButton;
    btnDrwObjMode13: TSpeedButton;
    btnDrwObjMode14: TSpeedButton;
    btnDrwObjMode15: TSpeedButton;
    btnDrwObjMode16: TSpeedButton;
    btnDrwObjMode17: TSpeedButton;
    btnDrwObjMode18: TSpeedButton;
    btnDrwObjMode19: TSpeedButton;
    btnDrwObjMode2: TSpeedButton;
    btnDrwObjMode20: TSpeedButton;
    btnDrwObjMode21: TSpeedButton;
    btnDrwObjMode22: TSpeedButton;
    btnDrwObjMode23: TSpeedButton;
    btnDrwObjMode24: TSpeedButton;
    btnDrwObjMode25: TSpeedButton;
    btnDrwObjMode26: TSpeedButton;
    btnDrwObjMode27: TSpeedButton;
    btnDrwObjMode28: TSpeedButton;
    btnDrwObjMode29: TSpeedButton;
    btnDrwObjMode3: TSpeedButton;
    btnDrwObjMode30: TSpeedButton;
    btnDrwObjMode31: TSpeedButton;
    btnDrwObjMode32: TSpeedButton;
    btnDrwObjMode33: TSpeedButton;
    btnDrwObjMode34: TSpeedButton;
    btnDrwObjMode35: TSpeedButton;
    btnDrwObjMode36: TSpeedButton;
    btnDrwObjMode37: TSpeedButton;
    btnDrwObjMode38: TSpeedButton;
    btnDrwObjMode39: TSpeedButton;
    btnDrwObjMode4: TSpeedButton;
    btnDrwObjMode40: TSpeedButton;
    btnDrwObjMode42: TSpeedButton;
    btnDrwObjMode44: TSpeedButton;
    btnDrwObjMode46: TSpeedButton;
    btnDrwObjMode47: TSpeedButton;
    btnDrwObjMode48: TSpeedButton;
    btnDrwObjMode49: TSpeedButton;
    btnDrwObjMode5: TSpeedButton;
    btnDrwObjMode50: TSpeedButton;
    btnDrwObjMode51: TSpeedButton;
    btnDrwObjMode52: TSpeedButton;
    btnDrwObjMode53: TSpeedButton;
    btnDrwObjMode55: TSpeedButton;
    btnDrwObjMode56: TSpeedButton;
    btnDrwObjMode57: TSpeedButton;
    btnDrwObjMode58: TSpeedButton;
    btnDrwObjMode59: TSpeedButton;
    btnDrwObjMode6: TSpeedButton;
    btnDrwObjMode60: TSpeedButton;
    btnDrwObjMode61: TSpeedButton;
    btnDrwObjMode62: TSpeedButton;
    btnDrwObjMode63: TSpeedButton;
    btnDrwObjMode7: TSpeedButton;
    btnDrwObjMode8: TSpeedButton;
    btnDrwObjMode9: TSpeedButton;
    btnDrwTypeObjet1: TSpeedButton;
    btnDrwTypeObjet2: TSpeedButton;
    btnDrwTypeObjet3: TSpeedButton;
    btnDrwTypeObjet4: TSpeedButton;
    btnDrwTypeObjet5: TSpeedButton;
    btnDrwTypeObjet6: TSpeedButton;
    btnDrwTypeObjet7: TSpeedButton;
    btnDrwTypeObjet8: TSpeedButton;
    btnDrwTypeObjet9: TSpeedButton;
    btnMergeObjects: TButton;
    btnPickScrap2: TButton;
    btnDeleteImage: TButton;
    btnEditImage: TButton;
    btnActionsCourbe: TButton;
    btnConcatCourbes: TButton;
    btnPickScrap1: TButton;
    btnHelpEditFamillesCourbes: TButton;
    Button1: TButton;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    btnAddIdxCurrObjectAtListForScraps: TButton;
    btnGenererScrapDepuisPolytope: TButton;
    CadreDessinBGRA1: TCadreDessinBGRA;
    CadreListeGroupesForMainWindow1: TCadreListeGroupesForMainWindow;

    lbSupergroupe: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MenuItem33: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    pnlSuperGroupe: TPanel;
    Polylignes: TButton;
    chkCenterlinesInExternalFile: TCheckBox;
    cmbOperationsBooleennes: TComboBox;
    editNoObjet1: TCurrencyEdit;
    editNoObjet2: TCurrencyEdit;
    imglstButtonsDessin: TImageList;
    Label2: TLabel;
    lbNatureObj: TLabel;
    lbNbImages: TLabel;
    ListBox1: TListBox;

    imglstActionsMenus: TImageList;
    lsbImages: TCheckListBox;
    MainMenu1: TMainMenu;
    editNumsCourbesPoly: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    mnuOutils: TMenuItem;
    MenuItem70: TMenuItem;
    mnuCurrentGroupe: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem7: TMenuItem;
    mnuExportAsImage: TMenuItem;
    mnuDELETE_LAST_OBJECTS: TMenuItem;
    mnuAide: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
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
    pagectrlScrapsImages: TPageControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    pnlCdrListeGroupes: TPanel;
    pnlCadreDessin: TPanel;
    pnlEditDessin: TPanel;
    pnlExport: TPanel;
    pnlFichier: TPanel;
    Panel4: TPanel;
    pnlBtnControleVue: TPanel;
    pnlBoutons: TPanel;
    pnlPopUpActionsCourbes: TPanel;
    ScrollBox1: TScrollBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton25: TSpeedButton;
    SpeedButton26: TSpeedButton;
    lbSuperGroupeName: TStaticText;
    SpeedButton3: TSpeedButton;
    StaticText2: TStaticText;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure acAddCurrCourbeAtListForScrapsExecute(Sender: TObject);
    procedure acAddCurrPolylineAtListForScrapsExecute(Sender: TObject);
    procedure acAproposExecute(Sender: TObject);
    procedure acCalcAllBoundingBoxesExecute(Sender: TObject);
    procedure acChargerCanevasTopoExecute(Sender: TObject);
    procedure acCloseDocumentExecute(Sender: TObject);
    procedure acCourbeChenalDeVouteExecute(Sender: TObject);
    procedure acCourbeEcoulementsExecute(Sender: TObject);
    procedure acCourbeLignesPentesExecute(Sender: TObject);
    procedure acCourbeMiniRessautExecute(Sender: TObject);
    procedure acCourbeMurMaconneExecute(Sender: TObject);
    procedure acCourbeParoiFracasseeExecute(Sender: TObject);
    procedure acCourbeParoiIncertaineExecute(Sender: TObject);
    procedure acCourbeParoisCacheesExecute(Sender: TObject);
    procedure acCourbeParoisExecute(Sender: TObject);
    procedure acCourbeRessautsExecute(Sender: TObject);
    procedure acCourbeSurplombsExecute(Sender: TObject);
    procedure acCreerUnAtlasHTMLExecute(Sender: TObject);
    procedure acDeleteallObjGroupeExecute(Sender: TObject);
    procedure acDeleteLastCourbeExecute(Sender: TObject);
    procedure acDeleteLastPolygonExecute(Sender: TObject);
    procedure acDeleteLastPolylineExecute(Sender: TObject);
    procedure acDeleteLastScrapExecute(Sender: TObject);
    procedure acDeleteLastSimpleLineExecute(Sender: TObject);
    procedure acDeleteLastSymboleExecute(Sender: TObject);
    procedure acDeleteLastTexteExecute(Sender: TObject);
    procedure acDisplayPOIExecute(Sender: TObject);
    procedure acDummyExecute(Sender: TObject);
    procedure acEditCourbeExecute(Sender: TObject);
    procedure acEditLigneExecute(Sender: TObject);
    procedure acEditObjetExecute(Sender: TObject);
    procedure acEditPolygoneExecute(Sender: TObject);
    procedure acEditPolylignExecute(Sender: TObject);
    procedure acEditScrapExecute(Sender: TObject);
    procedure acEditSymboleExecute(Sender: TObject);
    procedure acEditTexteExecute(Sender: TObject);
    procedure acExporterCenterLinesExecute(Sender: TObject);
    procedure acExportImgAllReseauExecute(Sender: TObject);
    procedure acExportSVGExecute(Sender: TObject);
    procedure acExportSVGScrapsOnlyExecute(Sender: TObject);
    procedure acExportToTherionExecute(Sender: TObject);
    procedure acExportVersSIGExecute(Sender: TObject);
    procedure acExportXHTMLExecute(Sender: TObject);
    procedure acGenererDessinOOoDrawExecute(Sender: TObject);
    procedure acGenererGCDFromCurrGroupeExecute(Sender: TObject);
    procedure acGenererScrapDepuisGroupeExecute(Sender: TObject);
    procedure acGenererScrapFromCourbesExecute(Sender: TObject);
    procedure acGenererScrapFromPolylinesExecute(Sender: TObject);
    procedure acGestionSupergroupesExecute(Sender: TObject);
    procedure acLigneFlecheExecute(Sender: TObject);
    procedure acLigneFractureExecute(Sender: TObject);
    procedure acLignePentesExecute(Sender: TObject);
    procedure acLigneSuiteReseauExecute(Sender: TObject);
    procedure acListerObjetsCurrGroupeExecute(Sender: TObject);
    procedure acMergeLastScrapsExecute(Sender: TObject);
    procedure acMergePolygonesExecute(Sender: TObject);
    procedure acMergeScrapsExecute(Sender: TObject);
    procedure acMesureDistanceExecute(Sender: TObject);
    procedure acMnuDeleteObjetsExecute(Sender: TObject);
    procedure acMnuEditionObjetsExecute(Sender: TObject);
    procedure acMnuExportImagesExecute(Sender: TObject);
    procedure acMnuRestoreLastObjectExecute(Sender: TObject);
    procedure acMoveObjectExecute(Sender: TObject);
    procedure acNettoyerGroupesExecute(Sender: TObject);
    procedure acNettoyerObjetsProblematiquesExecute(Sender: TObject);
    procedure acNewCourbeExecute(Sender: TObject);
    procedure acNewFreeHandCurveExecute(Sender: TObject);
    procedure acNewImageExecute(Sender: TObject);
    procedure acNewLigneExecute(Sender: TObject);
    procedure acNewNoneExecute(Sender: TObject);
    procedure acNewPolygoneExecute(Sender: TObject);
    procedure acNewPolylignExecute(Sender: TObject);
    procedure acNewScrapExecute(Sender: TObject);
    procedure acNewSymboleExecute(Sender: TObject);
    procedure acNewTexteExecute(Sender: TObject);
    procedure acNouveauDessinExecute(Sender: TObject);
    procedure acNouvelObjetExecute(Sender: TObject);
    procedure acOpenCaveMapExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPan2PointsExecute(Sender: TObject);
    procedure acPanDownExecute(Sender: TObject);
    procedure acPanLeftExecute(Sender: TObject);
    procedure acPanUpExecute(Sender: TObject);
    procedure acParamsVue2DExecute(Sender: TObject);
    procedure acPolygoneArgileExecute(Sender: TObject);
    procedure acPolygoneArgileGaletsExecute(Sender: TObject);
    procedure acPolygoneBlocsExecute(Sender: TObject);
    procedure acPolygoneCheminsExecute(Sender: TObject);
    procedure acPolygoneGaletsExecute(Sender: TObject);
    procedure acPolygoneGourExecute(Sender: TObject);
    procedure acPolygoneGrosBlocExecute(Sender: TObject);
    procedure acPolygoneLacExecute(Sender: TObject);
    procedure acPolygoneNeigeExecute(Sender: TObject);
    procedure acPolygoneNoneExecute(Sender: TObject);
    procedure acPolygoneSableExecute(Sender: TObject);
    procedure acPolygoneSilhouettesExecute(Sender: TObject);
    procedure acPolygoneSiphonExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acQSaveExecute(Sender: TObject);
    procedure acQuitExecute(Sender: TObject);
    procedure acReattribBasePointsExecute(Sender: TObject);
    procedure acRechercherExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acSaveAnCopyExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acExportImgVueCouranteExecute(Sender: TObject);
    procedure acSetModeTravailNoneExecute(Sender: TObject);
    procedure acStylesObjetsExecute(Sender: TObject);
    procedure acSubMenuEditCourbeExecute(Sender: TObject);
    procedure acSubMenuEditPolygoneExecute(Sender: TObject);
    procedure acSubMenuEditPolylineExecute(Sender: TObject);
    procedure acSubMenuEditScrapExecute(Sender: TObject);
    procedure acSubMenuEditSimpleLineExecute(Sender: TObject);
    procedure acSubMenuEditSymboleExecute(Sender: TObject);
    procedure acSubMenuEditTexteExecute(Sender: TObject);
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
    procedure acMakePolygoneFrom4CurvesExecute(Sender: TObject);
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
    procedure btnActionsCourbeClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure CadreDessinBGRA1Click(Sender: TObject);
    procedure CoolBar1Change(Sender: TObject);
    procedure editNumsCourbesPolyChange(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure lsbGroupesClick(Sender: TObject);
    procedure MenuItem84Click(Sender: TObject);
    procedure PolylignesClick(Sender: TObject);
    procedure btnDeleteImageClick(Sender: TObject);


    procedure btnGenPolygoneFromScrapClick(Sender: TObject);
    procedure btnEditImageClick(Sender: TObject);
    procedure btnGenererScrapdepuisTexteClick(Sender: TObject);
    procedure btnHelpEditFamillesCourbesClick(Sender: TObject);

    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnPickScrap1Click(Sender: TObject);
    procedure btnPickScrap2Click(Sender: TObject);
    procedure btnConcatCourbesClick(Sender: TObject);
    procedure btnScrapFromPolyligneClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);

    procedure btnPolygoneFrom2ListesCourbesClick(Sender: TObject);
    procedure btnPolyFrom4LastCourbesClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormResize(Sender: TObject);
    procedure InitCaptions();

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lsbImagesClickCheck(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure pagectrlScrapsImagesChange(Sender: TObject);
    procedure VueClick(Sender: TObject);

  private
    // nom du fichier actuel
    FNomFichierActuel: string;
    // supergroupe courant
    FCurrentIdxSuperGroupe: TIDSuperGroupe;
    //procedure AfficherEtapeTravail(const Etape: string; const Min, Max, Position: integer);
    procedure DisplayCurrentSuperGroupe(const SG: TSuperGroupe);
    procedure GenererPolyligneDepuisLstPolylignes(const strListeIdxPolylines: string; const IdxGrp: TIDGroupeEntites; out PolylineOut: TPolyLigne);
    procedure GenererUneEbauche(const QFileName: TStringDirectoryFileName);
    function  GenererUnScrapDepuisLesParoisDunGroupe(const FD: TDocumentDessin;
                                                     const QIdxGroupe: TIDGroupeEntites;
                                                     out ScrapOut: TScrap): boolean;
    procedure ListerLesImages(const Idx: integer);
    procedure LocaliserGroupe(const Grp: TGroupeEntites);
    function  MakeListeCourbesPolylignForGenScrapOrPolygons(const strListeIdxCourbes: string;
                                                            const QMode: TCourbesOrPolylignes;
                                                            out   ArrIdxObj: TArrayIdxObjets): boolean;
    procedure MakePolygonFromFourCurves(const IdxC0, IdxC1, IdxC2, IdxC3: integer; const DoRemoveCurves: boolean);
    procedure MakePolygonFromTwoCurvesFamily(const Expr: string);
    procedure MakeScrapFromTwoCurvesFamily(const Expr: string);
    procedure NotifyTherionErrors(const E: string);
    procedure QSave(const DoUpdateCurrentFolder: boolean);
    procedure QuickSave();


    function  ChargerLeDessin(const MonDessin: TStringDirectoryFileName): Boolean;
    procedure Redessiner(); inline;
    // remplacement d'une polygonale
    function  RemplacerCanevasTopo(const FichierGCD: string): boolean;
    procedure DispInfoBasePoint(const B: TBaseStation);
    procedure DispInfosScrap(const S: TScrap; const IdxScrap: Int64);
    procedure DispInfosCourbe(const C: TCourbe; const IdxCourbe: Int64);
    procedure DispInfosPolyligne(const P: TPolyLigne; const IdxPolygone: Int64);

    procedure DispInfosPolygone(const P: TPolygone; const IdxPolygone: Int64);
    procedure DispInfosPonctObject(const O: TSymbole; const IdxObj: Int64);
    procedure DispInfosSimpleLine(const L: TSimpleLigne; const IdxLine: Int64);
    procedure DispInfosTextObject(const T: TTextObject; const IdxTxtObj: Int64);
    procedure InitialiserGHC(const QP: TParamsVue2D);
    procedure SetNomFichierActuel(const QFilename: TStringDirectoryFileName);
    procedure SetPnlCombinerObjets(const M: TModeTravailCanvas; const qAcMerge, qAcMergeLast: TAction; const Descro: string);
    procedure ShowMenus(const R90: boolean);
    procedure PurgerBoutonsDrw();
    procedure PreparerSubBoutons(const QMode: TModeTravailCanvas);
    { private declarations }
    procedure S666(const WU: TSpeedButton; const QAction: TAction); inline;
    // fusion d'objets
    procedure FusionnerDeuxScraps(const Idx1, Idx2: integer; const BoolOpsMode: byte);
    procedure FusionnerDeuxPolygones(const Idx1, Idx2: integer; const BoolOpsMode: byte);
    procedure FusionnerDeuxPolylignes(const Idx1, Idx2: integer; const BoolOpsMode: byte);
    function  GenererScrapDepuisArrayCourbes(const ArrIdxCourbes: TArrayIdxObjets;
                                             const IdxGrp: TIDGroupeEntites;
                                             out ScrapOut: TScrap): boolean;
    function  GenererScrapDepuisArrayPolylignes(const ArrIdxPolylignes: TArrayIdxObjets;
                                                const IdxGrp: TIDGroupeEntites;
                                                out   ScrapOut: TScrap): boolean;

    function  GenererPolygoneDepuisArrayCourbes(const ArrIdxCourbes: TArrayIdxObjets;
                                                const IdxGrp: TIDGroupeEntites;
                                                out   PolygoneOut: TPolygone): boolean;
    procedure GenererScrapDepuisStrListeIdxCourbes(const strListeIdxCourbes: string;
                                                   const QIdxGroupe: TIDGroupeEntites;
                                                   out ScrapOut: TScrap);
    procedure GenererPolygoneDepuisStrLstIdxInterCourbes(const strListeIdxCourbes: string;
                                                         const QIdxGroupe: TIDGroupeEntites;
                                                         out PolygonOut: TPolygone);
    procedure GenererScrapDepuisStrListeIdxPolylignes(const strListeIdxPolylignes: string;
                                                      const QIdxGroupe: TIDGroupeEntites;
                                                      out ScrapOut: TScrap);
    procedure GenererPolyDepuisArrayIntersecCourbes(const ArrIdxCourbes: TArrayIdxObjets; const IdxGrp: TIDGroupeEntites; out PolygonOut: TPolygone);
    procedure GenererCourbeDepuisLstCourbes(const strListeIdxCourbes: string;
                                            const IdxGrp: TIDGroupeEntites;
                                            out   CourbeOut: TCourbe);

  public
    { public declarations }
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}
uses
  DGCDummyUnit,
  frmJournal
  , frmParametrageAtlas
  {$IFDEF OPENCAVEMAP}
    {$IFDEF TIDBASEPOINT_AS_TEXT}
       {$WARNING: La BDD doit être réorganisée dans ce mode}
    {$ELSE}
      , frmOpenCaveMap
    {$ENDIF TIDBASEPOINT_AS_TEXT}


  {$ENDIF}
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

procedure TMainWindow.PurgerBoutonsDrw;
begin
  S666(btnDrwTypeObjet1, acNewNone);
  S666(btnDrwTypeObjet2, acNewNone);
  S666(btnDrwTypeObjet3, acNewNone);
  S666(btnDrwTypeObjet4, acNewNone);
  S666(btnDrwTypeObjet5, acNewNone);
  S666(btnDrwTypeObjet6, acNewNone);
  S666(btnDrwTypeObjet7, acNewNone);
  S666(btnDrwTypeObjet8, acNewNone);
  S666(btnDrwTypeObjet9, acNewNone);
  PreparerSubBoutons(mtNONE);
end;
// préparation des boutons en fonction du mode de dessin
procedure TMainWindow.PreparerSubBoutons(const QMode: TModeTravailCanvas);
begin
  S666(btnDrwObjMode1, acNewNone);
  S666(btnDrwObjMode2, acNewNone);
  S666(btnDrwObjMode3, acNewNone);
  S666(btnDrwObjMode4, acNewNone);

  S666(btnDrwObjMode5, acNewNone);
  S666(btnDrwObjMode6, acNewNone);
  S666(btnDrwObjMode7, acNewNone);
  S666(btnDrwObjMode8, acNewNone);

  S666(btnDrwObjMode9, acNewNone);
  S666(btnDrwObjMode10, acNewNone);
  S666(btnDrwObjMode11, acNewNone);
  S666(btnDrwObjMode12, acNewNone);

  S666(btnDrwObjMode13, acNewNone);
  S666(btnDrwObjMode14, acNewNone);
  S666(btnDrwObjMode15, acNewNone);
  S666(btnDrwObjMode16, acNewNone);

  S666(btnDrwObjMode17, acNewNone);
  S666(btnDrwObjMode18, acNewNone);
  S666(btnDrwObjMode19, acNewNone);
  S666(btnDrwObjMode20, acNewNone);

  S666(btnDrwObjMode21, acNewNone);
  S666(btnDrwObjMode22, acNewNone);
  S666(btnDrwObjMode23, acNewNone);
  S666(btnDrwObjMode24, acNewNone);

  S666(btnDrwObjMode25, acNewNone);
  S666(btnDrwObjMode26, acNewNone);
  S666(btnDrwObjMode27, acNewNone);
  S666(btnDrwObjMode28, acNewNone);
  case QMode of
    mtNONE: ; // ne rien faire
    mtDRAW_COURBE, mtDRAW_FREEHAND_COURBE, mtDRAW_POLYLINE:
      begin
        S666(btnDrwObjMode1 , acCourbeParois);
        S666(btnDrwObjMode2 , acCourbeParoisCachees);
        S666(btnDrwObjMode3 , acCourbeEcoulements);
        S666(btnDrwObjMode4 , acCourbeLignesPentes);
        S666(btnDrwObjMode5 , acCourbeRessauts);
        S666(btnDrwObjMode6 , acCourbeSurplombs);
        S666(btnDrwObjMode7 , acCourbeChenalDeVoute);
        S666(btnDrwObjMode8 , acCourbeMiniRessaut);
        S666(btnDrwObjMode9 , acCourbeParoiIncertaine);
        S666(btnDrwObjMode10, acCourbeMurMaconne);
        S666(btnDrwObjMode11, acCourbeParoiFracassee);
      end;
    mtLIGNE_PREMIER_POINT:
      begin
        S666(btnDrwObjMode1, acLigneFleche);
        S666(btnDrwObjMode2, acLigneFracture);
        S666(btnDrwObjMode3, acLigneSuiteReseau);
        S666(btnDrwObjMode4, acLignePentes);
      end;
    mtDRAW_POLYGONE:
      begin
        S666(btnDrwObjMode1, acPolygoneLac);
        S666(btnDrwObjMode2, acPolygoneArgile);
        S666(btnDrwObjMode3, acPolygoneSable);
        S666(btnDrwObjMode4, acPolygoneBlocs);
        S666(btnDrwObjMode5, acPolygoneGalets);
        S666(btnDrwObjMode6, acPolygoneNeige);
        S666(btnDrwObjMode7, acPolygoneSilhouettes);
        S666(btnDrwObjMode8, acPolygoneGrosBloc);
        S666(btnDrwObjMode9, acPolygoneGour);
        S666(btnDrwObjMode10, acPolygoneSiphon);
        S666(btnDrwObjMode11, acPolygoneArgileGalets);
        S666(btnDrwObjMode12, acPolygoneChemins);
      end;
    mtDRAW_SYMBOLE:
      begin               // -- symboles
        S666(btnDrwObjMode1, acSymbolePhoto);
        S666(btnDrwObjMode2, acSymboleEntree);
        S666(btnDrwObjMode3, acSymbolePointTopo);
        S666(btnDrwObjMode4, acSymbolePointFixe);
        S666(btnDrwObjMode5, acSymboleLiaisonsGroupes);
        S666(btnDrwObjMode6, acSymboleFistuleuses);
        S666(btnDrwObjMode7, acSymboleChouxFleurs);
        S666(btnDrwObjMode8, acSymboleExcentriques);
        S666(btnDrwObjMode9, acSymboleStalactites);
        S666(btnDrwObjMode10, acSymboleColonnes);
        S666(btnDrwObjMode11, acSymboleStalagmites);
        S666(btnDrwObjMode12, acSymboleCristaux);
        S666(btnDrwObjMode13, acSymboleFailles);
        S666(btnDrwObjMode14, acSymboleCupule);
        S666(btnDrwObjMode15, acSymboleZeff);
        S666(btnDrwObjMode16, acSymboleArriveeEau);
        S666(btnDrwObjMode17, acSymbolePerte);
        S666(btnDrwObjMode18, acSymboleDesob);
        S666(btnDrwObjMode19, acSymboleDanger);
        S666(btnDrwObjMode20, acSymboleGrotteDeSurface);
        S666(btnDrwObjMode21, acSymboleGouffreDeSurface);
        S666(btnDrwObjMode22, acSymbolePointRemarquable);
      end;
    mtDRAW_TEXTE:
      begin
        S666(btnDrwObjMode1, acTexteDefault);
        S666(btnDrwObjMode2, acTexteTitre);
        S666(btnDrwObjMode3, acTexteSousTitre);
        S666(btnDrwObjMode4, acTexteCotation);
        S666(btnDrwObjMode5, acTexteOrdinaire1);
        S666(btnDrwObjMode6, acTexteOrdinaire2);
        S666(btnDrwObjMode7, acTexteLieuDit);
        S666(btnDrwObjMode8, acTexteCotationExterieure);
      end;
  end;
end;

// chargement du dessin
procedure TMainWindow.SetNomFichierActuel(const QFilename: TStringDirectoryFileName);
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
  self.Caption := Format(FMT_APPLICATION_TITLE,[ExtractFileName(FNomFichierActuel), 'Editor ', WU]);
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
  Result := CadreDessinBGRA1.ChargerInitialiserDessin(MonDessin, True);
  chkCenterlinesInExternalFile.Checked := FD.CenterlinesIsIncludeFileGCP();
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
    CadreListeGroupesForMainWindow1.Initialiser(CadreDessinBGRA1, True, LocaliserGroupe);
    ListerLesImages(0);

    SetNomFichierActuel(MonDessin);   // nom actuel du dessin
    CadreDessinBGRA1.SetMsgInLbMessage(errNONE  , GetResourceString(rsMSG_READY));
    Result := True;
    DisplayCurrentSuperGroupe(FD.GetSuperGroupe(FCurrentIdxSuperGroupe));
  end;
  FD.SetProcAfficherProgression(nil);
  cmbOperationsBooleennes.ItemIndex := DEFAULT_MODE_OPERATIONS_BOOLEENNES;
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
  SetNomFichierActuel('SansNom.gcd');
  btnAddIdxCurrObjectAtListForScraps.Action := acDummy;
  btnGenererScrapDepuisPolytope.Action      := acDummy;
  lbNatureObj.Caption := '';
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
  CadreDessinBGRA1.MyProcRefreshImages    := ListerLesImages;
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
    // popup Courbes
    pnlPopUpActionsCourbes.Visible := false;     ;
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
  P1, P2, P3, P4: TPoint3Df;
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
    P1.setFrom(392351.45, 3286107.27, 0.00);
    P2.setFrom(392349.81, 3286106.12, 0.00);
    P3.setFrom(392350.96, 3286104.48, 0.00);
    P4.setFrom(392348.66, 3286107.76, 0.00);
    {$IFDEF TIDBASEPOINT_AS_TEXT}
    BP := MakeTBaseStation('1.0', '1.00B', 8, TColor(26367),
    {$ELSE}
    BP := MakeTBaseStation(100010, '1.00B', 8, TColor(26367),
    {$ENDIF TIDBASEPOINT_AS_TEXT}
    P1, P2, P3, P4, True);
    LS.Add(GenererLigneBasepoint(0, BP));
    P1.setFrom(392349.81, 3286106.12, 0.00);
    P2.setFrom(392349.65, 3286106.01, 0.00);
    P3.setFrom(392350.79, 3286104.37, 0.00);
    P4.setFrom(392348.50, 3286107.64, 0.00);
    {$IFDEF TIDBASEPOINT_AS_TEXT}
    BP := MakeTBaseStation('1.1', '1.00B', 8, TColor(26367),
    {$ELSE}
    BP := MakeTBaseStation(100020, '1.00C', 8, TColor(26367),
    {$ENDIF TIDBASEPOINT_AS_TEXT}
    P1, P2, P3, P4, True);
    LS.Add(GenererLigneBasepoint(0, BP));
    LS.Add(ENDBASEPOINTMARKER);
    LS.SaveToFile(QFileName);
  finally
    FreeAndNil(LS);//LS.Free;
  end;
end;

function TMainWindow.RemplacerCanevasTopo(const FichierGCD: string): boolean;
var
  SauveGrdTXT: String;
  FMyDocDessin: TDocumentDessin;
begin
  Result := False;
  // sauvegarde du canevas topo
  SauveGrdTXT := ExtractFilePath(FNomFichierActuel) + 'Polygonale_' + ExtractFileNameWithoutExt(FNomFichierActuel) + '.txt';
  CadreDessinBGRA1.SetMsgInLbMessage(errBUSY, 'Backup in ' + ExtractFileName(SauveGrdTXT));
  FMyDocDessin := CadreDessinBGRA1.MyDocumentDessin;
  //FMyDocDessin.ExportBasePoints(SauveGrdTXT);
  CadreDessinBGRA1.SetMsgInLbMessage(errBUSY, 'Remplacement de la polygonale');
  if (FMyDocDessin.ImportBasePoints(FichierGCD)) then
  begin
    FMyDocDessin.CalcBoundingBoxAllGroupes;
    CadreDessinBGRA1.RefreshVue();
    CadreDessinBGRA1.SetMsgInLbMessage(errOK, 'Polygonale remplacée');
  end
  else
  begin
    NotifyError(GetResourceString(rsERR_MSG_IMPORT_CENTERLINE));
    CadreDessinBGRA1.SetMsgInLbMessage(errERROR, 'Erreur en remplacement de la polygonale');
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
  ; //if not FDlgToolEntites.Visible then FDlgToolEntites.Show;
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
  {$IFDEF MAC_OS_X_VERSION_MIN_REQUIRED}
  dlgProcessing := TdlgProcessing.Create(Application);
  {$ENDIF}
  {$IFDEF LINUX}
  ;
  {$ENDIF}
end;

procedure TMainWindow.InitCaptions();
  procedure S777(const WU: TAction; const CaptionHint: string);
  begin
    WU.Caption  := GetResourceString(CaptionHint);
    WU.Hint     := GetResourceString(CaptionHint);
  end;
begin
  acOpenCaveMap.Enabled := false;
  chkCenterlinesInExternalFile.caption := GetResourceString(rsMNU_CENTERLINES_IN_EXTERNAL_FILE);
  mnuFichier.Caption    := GetResourceString(rsMNU_FILE);
    S777(acNouveauDessin  , rsMNU_NEW_DRAWING);
    S777(acOpen           , rsMNU_OPEN);
    S777(acSaveAs         , rsMNU_SAVE_AS);
    S777(acSaveAnCopy     , rsMNU_SAVE_A_COPY);
    S777(acQSave          , rsMNU_QUICK_SAVE);
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
    S777(acChargerCanevasTopo    , rsMNU_POLYGO_GHTOPO);
    S777(acExporterCenterLines   , rsMNU_EXPORT_GCP);
    s777(acCreerUnAtlasHTML      , rsMNU_GENERER_ATLAS);
    S777(acExportVersSIG         , rsMNU_EXPORT_VERS_SIG);
    S777(acQuit                  , rsMNU_QUIT);

  //mnuDESSIN.Caption   := rsMNU_DESSIN;

  mnuEDITION.Caption  := GetResourceString(rsMNU_EDITION);
    S777(acMnuEditionObjets, rsMNU_EDIT_OBJETS);
      S777(acEditScrap       , rsMNU_EDIT_SCRAP)      ;  S777(acSubMenuEditScrap      , rsMNU_EDIT_SCRAP);
      S777(acEditCourbe      , rsMNU_EDIT_COURBE)     ;  S777(acSubMenuEditCourbe     , rsMNU_EDIT_COURBE);
      S777(acEditPolylign    , rsMNU_EDIT_POLYLIGNE)  ;  S777(acSubMenuEditPolyline   , rsMNU_EDIT_POLYLIGNE);
      S777(acEditPolygone    , rsMNU_EDIT_POLYGONE)   ;  S777(acSubMenuEditPolygone   , rsMNU_EDIT_POLYGONE);
      S777(acEditLigne       , rsMNU_EDIT_SIMPLE_LINE);  S777(acSubMenuEditSimpleLine , rsMNU_EDIT_SIMPLE_LINE);
      S777(acEditSymbole     , rsMNU_EDIT_SYMBOLE)    ;  S777(acSubMenuEditSymbole    , rsMNU_EDIT_SYMBOLE);
      S777(acEditTexte       , rsMNU_EDIT_TEXTE)      ;  S777(acSubMenuEditTexte      , rsMNU_EDIT_TEXTE);
      // groupe
        S777(acDeleteallObjGroupe       , rsMNU_DELETE_ALL_OBJ_CURR_GROUPE);
        S777(acGenererScrapDepuisGroupe , rsMNU_GENERER_SCRAP_DEPUIS_PAROIS_GROUPE);
        S777(acReattribBasePoints       , rsMNU_REATTRIB_BASEPOINTS_GROUPE);

    S777(acStylesObjets   , rsMNU_STYLES_OBJETS);
    s777(acRechercher     , rsMNU_RECHERCHER);
    s777(acCalcAllBoundingBoxes, rsMNU_CALC_ALL_BB);

    S777(acMnuDeleteObjets, rsMNU_DELETE_LAST_OBJECTS);
      s777(acDeleteLastScrap      , rsMNU_DELETE_SCRAP);
      s777(acDeleteLastCourbe     , rsMNU_DELETE_COURBE);
      s777(acDeleteLastPolyline   , rsMNU_DELETE_POLYLINE);
      s777(acDeleteLastSimpleLine , rsMNU_DELETE_SLINE);
      s777(acDeleteLastPolygon    , rsMNU_DELETE_POLYGON);
      s777(acDeleteLastSymbole    , rsMNU_DELETE_SYMBOLE);
      s777(acDeleteLastTexte      , rsMNU_DELETE_TEXTE);
    S777(acMnuRestoreLastObject   , rsMNU_RESTORE_LAST_OBJECTS);
      s777(acUndoDeleteScrap      , rsMNU_DELETE_SCRAP);
      s777(acUndoDeleteCourbe     , rsMNU_DELETE_COURBE);
      s777(acUndoDeletePolyligne  , rsMNU_DELETE_POLYLINE);
      s777(acUndoDeleteSimpleLigne, rsMNU_DELETE_SLINE);
      s777(acUndoDeletePolygone   , rsMNU_DELETE_POLYGON);
      s777(acUndoDeleteSymbole    , rsMNU_DELETE_SYMBOLE);
      s777(acUndoDeleteTexte      , rsMNU_DELETE_TEXTE);
      s777(acUndoDeleteImage      , rsMNU_DELETE_IMAGE);
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

  // actions de boutons
  S777(acSetModeTravailNone      , rsBTN_DRW_HINT_SETMODE_0);
  S777(acNouvelObjet             , rsBTN_DRW_HINT_NEW_OBJ);
  S777(acEditObjet               , rsBTN_DRW_HINT_EDIT_OBJ);
  S777(acDeleteObject            , rsBTN_DRW_HINT_DELETE_OBJ);
  // action de création d'objets
  S777(acNewScrap                , rsBTN_DRW_HINT_NEW_SCRAP);
  S777(acNewCourbe               , rsBTN_DRW_HINT_NEW_COURBE);
  S777(acNewPolylign             , rsBTN_DRW_HINT_NEW_POLYLIGNE);
  S777(acNewLigne                , rsBTN_DRW_HINT_NEW_LIGNE);
  S777(acNewPolygone             , rsBTN_DRW_HINT_NEW_POLYGON);
  S777(acNewSymbole              , rsBTN_DRW_HINT_NEW_SYMBOLE);
  S777(acNewTexte                , rsBTN_DRW_HINT_NEW_TEXTE);
  S777(acNewImage                , rsBTN_DRW_HINT_NEW_IMAGE_FOND);
  // actions pour types d'objets
  // -- courbes
  S777(acCourbeNone              , rsBTN_COURBE_NONE);
  S777(acCourbeParois            , rsBTN_COURBE_PAROI);
  S777(acCourbeParoisCachees     , rsBTN_COURBE_PAROI_CACHEE);
  S777(acCourbeEcoulements       , rsBTN_COURBE_ECOULEMENT);
  S777(acCourbeLignesPentes      , rsBTN_COURBE_LIGNES_PENTE);
  S777(acCourbeRessauts          , rsBTN_COURBE_RESSAUT);
  S777(acCourbeSurplombs         , rsBTN_COURBE_SURPLOMB);
  S777(acCourbeChenalDeVoute     , rsBTN_COURBE_CHENAL);
  S777(acCourbeMiniRessaut       , rsBTN_COURBE_MINI_RESSAUT);
  S777(acCourbeParoiIncertaine   , rsBTN_COURBE_PAROI_INCERTAINE);
  S777(acCourbeMurMaconne        , rsBTN_COURBE_MUR_MACONNE);
  S777(acCourbeParoiFracassee    , rsBTN_COURBE_PAROI_FRACASSEE);


  // -- lignes
  S777(acLigneNone               , rsBTN_LIGNE_NONE);
  S777(acLigneFleche             , rsBTN_LIGNE_FLECHE);
  S777(acLigneFracture           , rsBTN_LIGNE_FRACTURES);
  S777(acLigneSuiteReseau        , rsBTN_LIGNE_SUITE);
  S777(acLignePentes             , rsBTN_LIGNE_PENTE);

  // -- polygones
  S777(acPolygoneNone            , rsBTN_POLYGONE_NONE);
  S777(acPolygoneLac             , rsBTN_POLYGONE_LAC);
  S777(acPolygoneArgile          , rsBTN_POLYGONE_ARGILE);
  S777(acPolygoneSable           , rsBTN_POLYGONE_SABLE);
  S777(acPolygoneBlocs           , rsBTN_POLYGONE_BLOCS);
  S777(acPolygoneGalets          , rsBTN_POLYGONE_GALETS);
  S777(acPolygoneNeige           , rsBTN_POLYGONE_NEIGE);
  S777(acPolygoneSilhouettes     , rsBTN_POLYGONE_SILHOUETTES);
  S777(acPolygoneGrosBloc        , rsBTN_POLYGONE_GROS_BLOC);
  S777(acPolygoneGour            , rsBTN_POLYGONE_GOUR);
  S777(acPolygoneSiphon          , rsBTN_POLYGONE_SIPHON);
  S777(acPolygoneArgileGalets    , rsBTN_POLYGONE_VARVES);
  S777(acPolygoneChemins         , rsBTN_POLYGONE_CHEMINS);
  // -- symboles
  S777(acSymboleNone             , rsBTN_SYMBOLE_NONE);
  S777(acSymbolePhoto            , rsBTN_SYMBOLE_PHOTO);
  S777(acSymboleEntree           , rsBTN_SYMBOLE_ENTREE);
  S777(acSymbolePointTopo        , rsBTN_SYMBOLE_POINT_TOPO);
  S777(acSymbolePointFixe        , rsBTN_SYMBOLE_POINT_FIXE);
  S777(acSymboleLiaisonsGroupes  , rsBTN_SYMBOLE_CORRESPONDANCE);
  S777(acSymboleFistuleuses      , rsBTN_SYMBOLE_FISTULEUSES);
  S777(acSymboleChouxFleurs      , rsBTN_SYMBOLE_CONCRETIONS_PAROI);
  S777(acSymboleExcentriques     , rsBTN_SYMBOLE_EXCENTRIQUES);
  S777(acSymboleStalactites      , rsBTN_SYMBOLE_STALACTITES);
  S777(acSymboleColonnes         , rsBTN_SYMBOLE_COLONNES);
  S777(acSymboleStalagmites      , rsBTN_SYMBOLE_STALAGMITES);
  S777(acSymboleCristaux         , rsBTN_SYMBOLE_CRISTAUX);
  S777(acSymboleFailles          , rsBTN_SYMBOLE_FRACTURE);
  S777(acSymboleCupule           , rsBTN_SYMBOLE_CUPULE);
  S777(acSymboleZeff             , rsBTN_SYMBOLE_ZEFF);
  S777(acSymboleArriveeEau       , rsBTN_SYMBOLE_ARRIVEE_EAU);
  S777(acSymbolePerte            , rsBTN_SYMBOLE_PERTE);
  S777(acSymboleDesob            , rsBTN_SYMBOLE_DESOB);
  S777(acSymboleDanger           , rsBTN_SYMBOLE_DANGER);
  S777(acSymboleGrotteDeSurface  , rsBTN_SYMBOLE_SURFACE_GROTTE);
  S777(acSymboleGouffreDeSurface , rsBTN_SYMBOLE_SURFACE_GOUFFRE);
  S777(acSymbolePointRemarquable , rsBTN_SYMBOLE_PT_REMARQUABLE);
  // textes
  S777(acTexteTitre              , rsBTN_TEXTE_TITRES);
  S777(acTexteSousTitre          , rsBTN_TEXTE_SOUS_TITRES);
  S777(acTexteCotation           , rsBTN_TEXTE_COTATION);
  S777(acTexteOrdinaire1         , rsBTN_TEXTE_TEXTE_ORDINAIRE_1);
  S777(acTexteOrdinaire2         , rsBTN_TEXTE_TEXTE_ORDINAIRE_2);
  S777(acTexteDefault            , rsBTN_TEXTE_DEBUG);
  S777(acTexteLieuDit            , rsBTN_TEXTE_LIEU_DIT);
  S777(acTexteCotationExterieure , rsBTN_TEXTE_COTATION_EXT);

  // combinaison d'objets (fusion, ...)
  S777(acMergeScraps             , rsBTN_COMBINE_OBJ_MERGE_SCRAPS);
  S777(acMergeLastScraps         , rsBTN_COMBINE_OBJ_MERGE_LAST_SCRAPS);

  // panneau images
  btnEditImage.Caption            := GetResourceString(rsBTN_EDIT_IMAGE);
  btnMoveUp.Caption               := GetResourceString(rsBTN_IMG_MOVE_UP);
  btnMoveDown.Caption             := GetResourceString(rsBTN_IMG_MOVE_DOWN);
  btnDeleteImage.Caption          := GetResourceString(rsBTN_DELETE_IMAGE);
  // mesure de distance
  S777(acMesureDistance           , rsBTN_MESURE_DISTANCE);
  // pour l'ajout à la liste des contours de scraps
  S777(acDummy                          ,  rsAC_DUMMY);
  S777(acAddCurrCourbeAtListForScraps   ,  rsACN_ADD_CURR_CURVE_AT_SCRAPLISTE);
  S777(acAddCurrPolylineAtListForScraps ,  rsACN_ADD_CURR_POLYLIN_AT_SCRAPLISTE);

  S777(acGenererScrapFromCourbes        ,  rsACN_GENERER_SCRAP_FROM_COURBE);
  S777(acGenererScrapFromPolylines      ,  rsACN_GENERER_SCRAP_FROM_POLYLINE);

  // activer OpenCaveMap
  S777(acOpenCaveMap, 'OpenCaveMap');
  {$IFDEF OPENCAVEMAP}
  acOpenCaveMap.Enabled := true;
  {$ENDIF}

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

procedure TMainWindow.QSave(const DoUpdateCurrentFolder: boolean);
var
  QFileName: TStringDirectoryFileName;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  QFileName := FNomFichierActuel;
  if (DoDialogSaveFilename('Fichiers GCD|*.gcd', '.gcd', QFileName)) then
  begin
    CadreDessinBGRA1.SetMsgInLbMessage(errBUSY, 'Sauvegarde du dessin ...');
    CadreDessinBGRA1.MyDocumentDessin.SaveToFile(QFileName, -1, chkCenterlinesInExternalFile.Checked);
    if (DoUpdateCurrentFolder) then SetNomFichierActuel(QFileName);
    AfficherMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
    CadreDessinBGRA1.SetMsgInLbMessage(errOK, GetResourceString(rsMISC_DONE_WITH_SUCCESS));
  end;
end;

procedure TMainWindow.acSaveAnCopyExecute(Sender: TObject);
begin
  QSave(false);
end;

procedure TMainWindow.acSaveAsExecute(Sender: TObject);
begin
  QSave(true);
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
      EWE.setFrom(QC1, QC2);
      CadreDessinBGRA1.ExporterTopoEnImage(QFileName, EWE, Trunc(QAT));
      ShowMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
    end;
  end;
end;

procedure TMainWindow.acSetModeTravailNoneExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtNONE);
  PurgerBoutonsDrw();
  btnMergeObjects.Action           := acDummy;//nil;
  btnAddIdxCurrObjectAtListForScraps.Action := acDummy;
  btnGenererScrapDepuisPolytope.Action      := acDummy;
  lbNatureObj.Caption := '';
end;



procedure TMainWindow.acStylesObjetsExecute(Sender: TObject);
begin
  if (CadreDessinBGRA1.DoDraw) then
  begin
    EditerStylesObjets(CadreDessinBGRA1.MyDocumentDessin, Redessiner);
    CadreDessinBGRA1.RefreshVue(); //CadreDessin1.RedessinEcran(false);
  end;
end;

procedure TMainWindow.acSubMenuEditCourbeExecute(Sender: TObject);
begin
  acEditCourbeExecute(self);
end;

procedure TMainWindow.acSubMenuEditPolygoneExecute(Sender: TObject);
begin
  acEditPolygoneExecute(self);
end;

procedure TMainWindow.acSubMenuEditPolylineExecute(Sender: TObject);
begin
  acEditPolylignExecute(self);
end;

procedure TMainWindow.acSubMenuEditScrapExecute(Sender: TObject);
begin
  acEditScrapExecute(self);
end;

procedure TMainWindow.acSubMenuEditSimpleLineExecute(Sender: TObject);
begin
  acEditLigneExecute(self);
end;

procedure TMainWindow.acSubMenuEditSymboleExecute(Sender: TObject);
begin
  acEditSymboleExecute(self);
end;

procedure TMainWindow.acSubMenuEditTexteExecute(Sender: TObject);
begin
  acEditTexteExecute(self);
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

procedure TMainWindow.acMakePolygoneFrom4CurvesExecute(Sender: TObject);
var
  strListeIdxCourbes: string;
  ArrIdxCourbes: TArrayIdxObjets;
  n: Integer;
begin
  strListeIdxCourbes := Trim(editNumsCourbesPoly.Text);
  if (QuestionOuiNon('Ceci va construire un polygone à partir des quatre courbes sélectionnées - Continuer')) then
  begin
    if (MakeListeCourbesPolylignForGenScrapOrPolygons(strListeIdxCourbes, tcpCOURBE, ArrIdxCourbes)) then
    begin
      n := Length(ArrIdxCourbes);
      if (n < 4) then Exit;
      MakePolygonFromFourCurves(ArrIdxCourbes[n - 1],
                                ArrIdxCourbes[n - 2],
                                ArrIdxCourbes[n - 3],
                                ArrIdxCourbes[n - 4],
                                True);
    end;
  end;
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

procedure TMainWindow.btnActionsCourbeClick(Sender: TObject);
begin
  pnlPopUpActionsCourbes.Visible := not pnlPopUpActionsCourbes.Visible;
end;





procedure TMainWindow.Button2Click(Sender: TObject);
begin
  editNumsCourbesPoly.Text := editNumsCourbesPoly.Text + ';';
end;

procedure TMainWindow.Button6Click(Sender: TObject);
begin
  editNumsCourbesPoly.Text := editNumsCourbesPoly.Text + '-';
end;




procedure TMainWindow.btnDeleteImageClick(Sender: TObject);
var
  FD       : TDocumentDessin;
  WU       : Integer;
  DoDelete : Boolean;
begin
  FD := CadreDessinBGRA1.GetDocDessin;
  if (FD.GetNbImages() = 0) then Exit;
  WU := lsbImages.ItemIndex;
  DoDelete := QuestionOuiNon(Format(GetResourceString(rsDO_DELETE_IMAGE), [WU]));
  if ((WU >= 0) and (DoDelete)) then
  begin
    FD.DeleteImage(WU);
    ListerLesImages(0);
    CadreDessinBGRA1.RefreshVue();
  end;
end;




// transforme un scrap en polygone et le monte dans le groupe courant
// Peu utilisé. Utiliser un menu
procedure TMainWindow.btnGenPolygoneFromScrapClick(Sender: TObject);
var
  QNoScrap: Integer;
  QIdxGroupeCourant: TIDGroupeEntites;
  FD: TDocumentDessin;
  MyScrap: TScrap;
  MyPolygone: TPolygone;
begin
  FD := CadreDessinBGRA1.GetDocDessin;
  QNoScrap := CadreDessinBGRA1.GetCurrScrapIdx();
  QIdxGroupeCourant := CadreDessinBGRA1.GetCurrentGroupeIdx;
  MyScrap := FD.GetScrap(QNoScrap);
  MyScrap.Nom := Format('Duplicate of scrap %d', [QNoScrap]);
  MyScrap.Couleur := clRed;
  MyScrap.IDGroupe := QIdxGroupeCourant;
  // conversion en polygone
  MyPolygone.IDGroupe := MyScrap.IDGroupe;
  MyPolygone.IDStylePolygone := nopCHEMINS;
  MyPolygone.Sommets  := MyScrap.Sommets;
  MyPolygone.LastModified := MyScrap.LastModified;
  MyPolygone.MarkToDelete := false;
  FD.AddPolygone(MyPolygone, True);
  // destruction du scrap
  FD.DeleteScrap(QNoScrap);

  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.btnHelpEditFamillesCourbesClick(Sender: TObject);
begin
  DisplayHelpSystem(CadreDessinBGRA1.GetDocDessin(), '');
end;


procedure TMainWindow.btnEditImageClick(Sender: TObject);
var
  FD: TDocumentDessin;
  QAbsPos: TPoint3Df;
  MyImage: TImageObject;
  WU: Integer;
begin
  WU := lsbImages.ItemIndex;
  FD := CadreDessinBGRA1.GetDocDessin;
  if (WU > 0) then
  begin
    MyImage := FD.GetImage(WU);
    QAbsPos.setFrom(MyImage.PositionCoinsImage.X1, MyImage.PositionCoinsImage.Y2, 0.00);
    if (DoDialogTImageObject(FD, MyImage, FD.GetDossierContenantDoc(), QAbsPos, False)) then
    begin
      pass;
    end;
  end;
end;




procedure TMainWindow.btnGenererScrapdepuisTexteClick(Sender: TObject);
var
  ScrapOut: TScrap;
  IdxGrp: TIDGroupeEntites;
begin
  IdxGrp := CadreDessinBGRA1.GetCurrentGroupeIdx;
  GenererScrapDepuisStrListeIdxCourbes(editNumsCourbesPoly.Text, IdxGrp, ScrapOut);
end;



procedure TMainWindow.btnMoveDownClick(Sender: TObject);
var
  FD: TDocumentDessin;
  Idx1: Integer;
  Idx2: Integer;
begin
  FD := CadreDessinBGRA1.GetDocDessin;
  if (FD.GetNbImages() = 0) then Exit;
  Idx1 := lsbImages.ItemIndex;
  Idx2 := lsbImages.ItemIndex + 1;
  if (Idx2 > (lsbImages.Count - 1)) then Exit;

  FD.PermuterImages(Idx1, Idx2);
  ListerLesImages(Idx2);
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.btnMoveUpClick(Sender: TObject);
var
  FD: TDocumentDessin;
  Idx1: Integer;
  Idx2: Integer;
begin
  FD := CadreDessinBGRA1.GetDocDessin;
  if (FD.GetNbImages() = 0) then Exit;
  Idx1 := lsbImages.ItemIndex - 1;
  Idx2 := lsbImages.ItemIndex;
  if (Idx1 < 0) then Exit;

  FD.PermuterImages(Idx1, Idx2);
  ListerLesImages(Idx1);
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.btnPickScrap1Click(Sender: TObject);
var
  WU: TModeSelectionEntites;
begin
  WU := CadreDessinBGRA1.GetModeSelectionEntites();
  case WU of
    mseSCRAPS     : editNoObjet1.AsInteger := CadreDessinBGRA1.GetCurrScrapIdx();
    msePOLYGONES  : editNoObjet1.AsInteger := CadreDessinBGRA1.GetCurrPolygoneIdx();
  end;
end;

procedure TMainWindow.btnPickScrap2Click(Sender: TObject);
var
  WU: TModeSelectionEntites;
begin
  WU := CadreDessinBGRA1.GetModeSelectionEntites();
  case WU of
    mseSCRAPS     : editNoObjet2.AsInteger := CadreDessinBGRA1.GetCurrScrapIdx();
    msePOLYGONES  : editNoObjet2.AsInteger := CadreDessinBGRA1.GetCurrPolygoneIdx();
  end;
end;




procedure TMainWindow.btnScrapFromPolyligneClick(Sender: TObject);
var
  IdxGrp: TIDGroupeEntites;
  ScrapOut: TScrap;
begin
  IdxGrp := CadreDessinBGRA1.GetCurrentGroupeIdx;
  GenererScrapDepuisStrListeIdxPolylignes(editNumsCourbesPoly.Text, IdxGrp, ScrapOut);
end;


procedure TMainWindow.Button1Click(Sender: TObject);
begin
  editNumsCourbesPoly.Text := '';
end;

procedure TMainWindow.btnConcatCourbesClick(Sender: TObject);
var
  OutCourbe: TCourbe;
  IdxGrp: TIDGroupeEntites;
begin
  Exit;
  IdxGrp := CadreDessinBGRA1.GetCurrentGroupeIdx;
  GenererCourbeDepuisLstCourbes(editNumsCourbesPoly.Text, IdxGrp, OutCourbe);
end;

procedure TMainWindow.PolylignesClick(Sender: TObject);
var
  IdxGrp: TIDGroupeEntites;
  OutPolyligne: TPolyLigne;
begin
  Exit; // Fonction à déboguer
  IdxGrp := CadreDessinBGRA1.GetCurrentGroupeIdx;
  GenererPolyligneDepuisLstPolylignes(editNumsCourbesPoly.Text, IdxGrp, OutPolyligne);
end;


procedure TMainWindow.GenererCourbeDepuisLstCourbes(const strListeIdxCourbes: string; const IdxGrp: TIDGroupeEntites; out CourbeOut: TCourbe);
var
  FD: TDocumentDessin;
  S0, S1, SR: TCourbe;
  ArrIdxCourbes: TArrayIdxObjets;
  NbC: Integer;
  Idx1: Int64;
  i, Nb: Integer;
  MyArc: TArcCourbe;
begin
  AfficherMessage(format('%s.GenererCourbeDepuisLstCourbes', [ClassName]));
  AfficherMessage(format(' --- Groupe: %d - IDs: %s', [IdxGrp, strListeIdxCourbes]));
  FD := CadreDessinBGRA1.GetDocDessin;
  if (not MakeListeCourbesPolylignForGenScrapOrPolygons(strListeIdxCourbes, tcpCOURBE, ArrIdxCourbes)) then
  begin
    NotifyError('Erreur en construction de la liste d''objets');
    Exit;
  end;
  if (ConcatenerCourbes(FD, ArrIdxCourbes, SR)) then
  begin
    // marquer les courbes à effacer
    Nb := length(ArrIdxCourbes);
    for i := 0 to Nb -1 do
    begin
      FD.MarquerCourbesAEffacer(ArrIdxCourbes[i]);
      AfficherMessage(Format('---- Courbe à effacer: %d', [ArrIdxCourbes[i]]));
    end;
    FD.NettoyerCourbes;      // Nettoyer la base
    FD.AddCourbe(SR, True);  // Ajouter la courbe fusionnée
  end
  else
    NotifyError('Echec fusion des courbes');
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.GenererPolyligneDepuisLstPolylignes(const strListeIdxPolylines: string;
                                                          const IdxGrp: TIDGroupeEntites;
                                                          out   PolylineOut: TPolyLigne);
var
  FD: TDocumentDessin;
  i, Nb: Integer;
  ArrIdxPolylignes: TArrayIdxObjets;
  SR: TPolyligne;
begin
  AfficherMessage(format('%s.GenererPolyligneDepuisLstPolylignes', [ClassName]));
  AfficherMessage(format(' --- Groupe: %d - IDs: %s', [IdxGrp, strListeIdxPolylines]));
  FD := CadreDessinBGRA1.GetDocDessin;
  if (not MakeListeCourbesPolylignForGenScrapOrPolygons(strListeIdxPolylines, tcpPOLYLIGNE, ArrIdxPolylignes)) then
  begin
    NotifyError('Erreur en construction de la liste d''objets');
    Exit;
  end;
  if (ConcatenerPolylignes(FD, ArrIdxPolylignes, SR)) then
  begin
    // marquer les courbes à effacer
    Nb := length(ArrIdxPolylignes);
    for i := 0 to Nb -1 do
    begin
      FD.MarquerPolylignesAEffacer(ArrIdxPolylignes[i]);
      AfficherMessage(Format('---- Polyligne à effacer: %d', [ArrIdxPolylignes[i]]));
    end;
    FD.NettoyerPolylignes();      // Nettoyer la base
    FD.AddPolyLigne(SR, True);  // Ajouter la courbe fusionnée
  end
  else
    NotifyError('Echec fusion des polylignes');
  CadreDessinBGRA1.RefreshVue();
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

procedure TMainWindow.acPolygoneArgileExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNaturePolygone(nopARGILE);
end;

procedure TMainWindow.acPolygoneArgileGaletsExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNaturePolygone(nopARGILES_GALETS);
end;

procedure TMainWindow.acPolygoneBlocsExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNaturePolygone(nopEBOULIS);
end;

procedure TMainWindow.acPolygoneCheminsExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNaturePolygone(nopCHEMINS);
end;

procedure TMainWindow.acPolygoneGaletsExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNaturePolygone(nopGALETS);
end;

procedure TMainWindow.acPolygoneGourExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNaturePolygone(nopGOUR);
end;

procedure TMainWindow.acPolygoneGrosBlocExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNaturePolygone(nopGROS_BLOC);
end;

procedure TMainWindow.acPolygoneLacExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNaturePolygone(nopLAC);
end;

procedure TMainWindow.acPolygoneNeigeExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNaturePolygone(nopNEIGE);
end;

procedure TMainWindow.acPolygoneNoneExecute(Sender: TObject);
begin

end;

procedure TMainWindow.acPolygoneSableExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNaturePolygone(nopSABLE);
end;

procedure TMainWindow.acPolygoneSilhouettesExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNaturePolygone(nopSILHOUETTE);
end;

procedure TMainWindow.acPolygoneSiphonExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNaturePolygone(nopSIPHON);
end;

procedure TMainWindow.acPrintExecute(Sender: TObject);
begin
  DisplayPrintingCenter(CadreDessinBGRA1.GetDocDessin(),
                        CadreDessinBGRA1.GetTexturesPolygones(),
                        CadreDessinBGRA1.GetParametresVue2D());
end;



procedure TMainWindow.acQSaveExecute(Sender: TObject);
begin
  QuickSave();
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

procedure TMainWindow.NotifyTherionErrors(const E: string);
begin
  ShowMessage(E);
end;

procedure TMainWindow.acExportToTherionExecute(Sender: TObject);
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  pass;
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






procedure TMainWindow.acGenererScrapDepuisGroupeExecute(Sender: TObject);
var
  ScrapOut: TScrap;
  WU: TIDGroupeEntites;
  FD: TDocumentDessin;
begin
  if (not QuestionOuiNon(GetResourceString(rsADVICE_FOR_SCRAPS_FROM_COURBES))) then Exit;
  FD  := CadreDessinBGRA1.GetDocDessin;
  WU  := CadreDessinBGRA1.GetCurrentGroupeIdx;

  GenererUnScrapDepuisLesParoisDunGroupe(FD, WU, ScrapOut);
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.acGenererScrapFromCourbesExecute(Sender: TObject);
var
  ScrapOut: TScrap;
  IdxGrp: TIDGroupeEntites;
begin
  IdxGrp := CadreDessinBGRA1.GetCurrentGroupeIdx;
  GenererScrapDepuisStrListeIdxCourbes(editNumsCourbesPoly.Text, IdxGrp, ScrapOut);
end;

procedure TMainWindow.acGenererScrapFromPolylinesExecute(Sender: TObject);
var
  IdxGrp: TIDGroupeEntites;
  ScrapOut: TScrap;
begin
  IdxGrp := CadreDessinBGRA1.GetCurrentGroupeIdx;
  GenererScrapDepuisStrListeIdxPolylignes(editNumsCourbesPoly.Text, IdxGrp, ScrapOut);
end;

procedure TMainWindow.acGestionSupergroupesExecute(Sender: TObject);
var
  QIdx: TIDSuperGroupe;
  QTodo: byte;
begin
  QIdx := FCurrentIdxSuperGroupe;
  DisplaySuperGroupes(CadreDessinBGRA1.GetDocDessin(), QIdx, QTodo);
  CadreListeGroupesForMainWindow1.ListerLesGroupes(0);
  CadreDessinBGRA1.ActualiserCmbListeDesGroupes();
  CadreDessinBGRA1.Vue.Invalidate;
end;



procedure TMainWindow.acLigneFlecheExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureLigne(nolFLECHE);
end;

procedure TMainWindow.acLigneFractureExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureLigne(nolFRACTURE);
end;

procedure TMainWindow.acLignePentesExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureLigne(nolPENTE);
end;

procedure TMainWindow.acLigneSuiteReseauExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureLigne(nolSUITE_RESEAU);
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

procedure TMainWindow.acMergeLastScrapsExecute(Sender: TObject);
begin

end;

procedure TMainWindow.acMergePolygonesExecute(Sender: TObject);
begin
  FusionnerDeuxPolygones(editNoObjet1.AsInteger, editNoObjet2.AsInteger, cmbOperationsBooleennes.ItemIndex);
end;





procedure TMainWindow.acMergeScrapsExecute(Sender: TObject);
begin
  FusionnerDeuxScraps(editNoObjet1.AsInteger, editNoObjet2.AsInteger, cmbOperationsBooleennes.ItemIndex);
end;

procedure TMainWindow.acMesureDistanceExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtMESURE_DISTANCE_SURFACE);
end;

procedure TMainWindow.acMnuDeleteObjetsExecute(Sender: TObject);
begin
  ;
end;

procedure TMainWindow.acMnuEditionObjetsExecute(Sender: TObject);
begin
  ;
end;

procedure TMainWindow.acMnuExportImagesExecute(Sender: TObject);
begin
  ;
end;

procedure TMainWindow.acMnuRestoreLastObjectExecute(Sender: TObject);
begin
  ;
end;



procedure TMainWindow.acMoveObjectExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtMOVE_OBJET);
end;

procedure TMainWindow.acNettoyerGroupesExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin;
  FD.NettoyerGroupes;
  CadreListeGroupesForMainWindow1.ListerLesGroupes(0);
end;

procedure TMainWindow.acNettoyerObjetsProblematiquesExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  FD.NettoyerObjetsProblematiques();
  ShowMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
end;

procedure TMainWindow.acNewNoneExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtNONE);
  PreparerSubBoutons(mtNONE);
end;

procedure TMainWindow.acNewCourbeExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtDRAW_COURBE);
  PreparerSubBoutons(mtDRAW_COURBE);
end;

procedure TMainWindow.acNewFreeHandCurveExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtDRAW_FREEHAND_COURBE);
  PreparerSubBoutons(mtDRAW_FREEHAND_COURBE);
end;

procedure TMainWindow.acNewImageExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtDRAW_IMAGE);
  pagectrlScrapsImages.ActivePageIndex := 1;
end;


procedure TMainWindow.acNewLigneExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtLIGNE_PREMIER_POINT);
  PreparerSubBoutons(mtLIGNE_PREMIER_POINT);
end;
procedure TMainWindow.acNewPolygoneExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtDRAW_POLYGONE);
  PreparerSubBoutons(mtDRAW_POLYGONE);
end;

procedure TMainWindow.acNewPolylignExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtDRAW_POLYLINE);
  PreparerSubBoutons(mtDRAW_POLYLINE);
end;

procedure TMainWindow.acNewScrapExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtDRAW_SCRAP);
end;

procedure TMainWindow.acNewSymboleExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtDRAW_SYMBOLE);
  PreparerSubBoutons(mtDRAW_SYMBOLE);
end;

procedure TMainWindow.acNewTexteExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetModeTravail(mtDRAW_TEXTE);
  PreparerSubBoutons(mtDRAW_TEXTE);
end;
// cette entrée de menu est en fait un alias pour l'ouverture d'un dessin
// Raison: - Un fichier GCP est un sous-ensemble du format GCD
//         - On commence un dessin en important une polygonale
procedure TMainWindow.acNouveauDessinExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
begin
  if (CadreDessinBGRA1.DoDraw) then
  begin
    ShowMessage(GetResourceString(rsMISC_DRAWING_MUST_BE_CLOSED));
    Exit;
  end;
  QFileName :=  GetGHCaveDrawDirectory() + 'Polygonale.gcp';
  if (DoDialogOpenFilename('Polygonales GCP|*.gcp|Tous|*.*', QFileName)) then
  begin
    if (ChargerLeDessin(QFileName)) then AfficherMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS))
                                    else NotifyError(GetResourceString(rsERR_MSG_LOAD_GCD_FAIL));
  end;
end;



procedure TMainWindow.acEditObjetExecute(Sender: TObject);
begin
  acSetModeTravailNoneExecute(self);
  S666(btnDrwTypeObjet1, acEditScrap);
  S666(btnDrwTypeObjet2, acEditCourbe);
  S666(btnDrwTypeObjet3, acEditPolylign);
  S666(btnDrwTypeObjet4, acEditLigne);
  S666(btnDrwTypeObjet5, acEditPolygone);
  S666(btnDrwTypeObjet6, acEditSymbole);
  S666(btnDrwTypeObjet7, acEditTexte);
end;
//------------------------------------------------------------------------------
procedure TMainWindow.SetPnlCombinerObjets(const M: TModeTravailCanvas;
                                           const qAcMerge, qAcMergeLast: TAction;
                                           const Descro: string);
begin
  CadreDessinBGRA1.SetModeTravail(M);
  pagectrlScrapsImages.ActivePageIndex := 0;
  btnMergeObjects.Action        := qAcMerge;
  case M of
    mtSELECT_SCRAP     : lbNatureObj.Caption := 'Scrap';
    mtSELECT_POLYGONE  : lbNatureObj.Caption := 'Polygone';
    //mtSELECT_COURBE    : lbNatureObj.Caption := 'Courbe';
    //mtSELECT_POLYLIGNE : lbNatureObj.Caption := 'Polyligne';
  else
    lbNatureObj.Caption := '';
    //pass;//  pnlCombinerObjets.Enabled := false;
  end;
  // actions pour l'ajout d'objets pour la génération des scraps
  //TODO: Fusionner les bouttons [btnAddIdxCurrObjectAtList] et
  case M of
    mtSELECT_COURBE    :
      begin
        btnAddIdxCurrObjectAtListForScraps.Action := acAddCurrCourbeAtListForScraps;
        btnGenererScrapDepuisPolytope.Action      := acGenererScrapFromCourbes;

      end;
    mtSELECT_POLYLIGNE :
      begin
        btnAddIdxCurrObjectAtListForScraps.Action := acAddCurrPolylineAtListForScraps;
        btnGenererScrapDepuisPolytope.Action      := acGenererScrapFromPolylines;
      end
  else
    btnAddIdxCurrObjectAtListForScraps.Action := acDummy;
    btnGenererScrapDepuisPolytope.Action      := acDummy;
  end;
  //*)

end;

procedure TMainWindow.acEditScrapExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  AfficherMessageErreur(Format('Nb objets: %d %s', [FD.GetNbScraps(), 'Scraps']));
  if (0 = FD.GetNbScraps()) then CadreDessinBGRA1.SetModeTravail(mtNONE)
                            else SetPnlCombinerObjets(mtSELECT_SCRAP, acMergeScraps, acMergeLastScraps, 'Mode Fusion de scraps');
end;
procedure TMainWindow.acEditCourbeExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  AfficherMessageErreur(Format('Nb objets: %d %s', [FD.GetNbCourbes(), 'Courbes']));
  if (0 = FD.GetNbCourbes()) then CadreDessinBGRA1.SetModeTravail(mtNONE)
                             else SetPnlCombinerObjets(mtSELECT_COURBE, acMergeCourbes, acMergeLastCourbes, 'Mode Fusion de courbes');
end;
procedure TMainWindow.acEditPolylignExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  AfficherMessageErreur(Format('Nb objets: %d %s', [FD.GetNbPolylignes(), 'Polylignes']));
  if (0 = FD.GetNbPolylignes()) then CadreDessinBGRA1.SetModeTravail(mtNONE)
                                else SetPnlCombinerObjets(mtSELECT_POLYLIGNE, acMergePolylines, acMergeLastPolylines, 'Mode Fusion de polylignes');
end;
procedure TMainWindow.acEditPolygoneExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  AfficherMessageErreur(Format('Nb objets: %d %s', [FD.GetNbPolygones(), 'Polygones']));
  if (0 = FD.GetNbPolygones()) then CadreDessinBGRA1.SetModeTravail(mtNONE)
                               else SetPnlCombinerObjets(mtSELECT_POLYGONE, acMergePolygones, acMergeLastPolygones, 'Mode Fusion de polygones');
end;
//------------------------------------------------------------------------------
procedure TMainWindow.acEditSymboleExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  AfficherMessageErreur(Format('Nb objets: %d %s', [FD.GetNbSymboles(), 'symboles']));
  if (0 = FD.GetNbSymboles()) then CadreDessinBGRA1.SetModeTravail(mtNONE)
                              else CadreDessinBGRA1.SetModeTravail(mtSELECT_SYMBOLE);
end;

procedure TMainWindow.acEditTexteExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  AfficherMessageErreur(Format('Nb objets: %d %s', [FD.GetNbTextes(), 'Textes']));
  if (0 = FD.GetNbTextes()) then CadreDessinBGRA1.SetModeTravail(mtNONE)
                            else CadreDessinBGRA1.SetModeTravail(mtSELECT_TEXTE);
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
      EWE.setFrom(QC1, QC2);
      CadreDessinBGRA1.ExporterTopoEnImage(QFileName, EWE, Trunc(QAT));
      ShowMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
    end;
  end;
end;


procedure TMainWindow.acCourbeParoisExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureCourbe(nocPAROI);
end;

procedure TMainWindow.acCourbeRessautsExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureCourbe(nocRESSAUT);
end;

procedure TMainWindow.acCourbeSurplombsExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureCourbe(nocSURPLOMB);
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



procedure TMainWindow.acDeleteallObjGroupeExecute(Sender: TObject);
var
  QIdxGroupe: TIDGroupeEntites;
  FD: TDocumentDessin;
  QGrp: TGroupeEntites;
  EWE: String;
begin
  QIdxGroupe := CadreDessinBGRA1.GetCurrentGroupeIdx();
  if (QIdxGroupe < 0) then Exit;
  FD := CadreDessinBGRA1.GetDocDessin();
  QGrp := FD.GetGroupeByIDGroupe(QIdxGroupe);
  EWE := Format(rsMSG_CONFIRM_DELETE_ALL_OBJ_GROUPE, [QGrp.IDGroupeEntites, QGrp.NomGroupe]);
  if (QuestionOuiNon(EWE)) then
  begin
    FD.DeleteAllObjectsOfGroupe(QGrp);
    CadreDessinBGRA1.RefreshVue();
  end;
end;

procedure TMainWindow.acDeleteLastCourbeExecute(Sender: TObject);
begin
  CadreDessinBGRA1.DeleteLastObject(mseCOURBES);
end;

procedure TMainWindow.acDeleteLastPolygonExecute(Sender: TObject);
begin
  CadreDessinBGRA1.DeleteLastObject(msePOLYGONES);
end;

procedure TMainWindow.acDeleteLastPolylineExecute(Sender: TObject);
begin
  CadreDessinBGRA1.DeleteLastObject(msePOLYLIGNES);
end;

procedure TMainWindow.acDeleteLastScrapExecute(Sender: TObject);
begin
  CadreDessinBGRA1.DeleteLastObject(mseSCRAPS);
end;

procedure TMainWindow.acDeleteLastSimpleLineExecute(Sender: TObject);
begin
  CadreDessinBGRA1.DeleteLastObject(mseLIGNES);
end;

procedure TMainWindow.acDeleteLastSymboleExecute(Sender: TObject);
begin
  CadreDessinBGRA1.DeleteLastObject(mseSYMBOLES);
end;

procedure TMainWindow.acDeleteLastTexteExecute(Sender: TObject);
begin
  CadreDessinBGRA1.DeleteLastObject(mseTEXTES);
end;

procedure TMainWindow.acDisplayPOIExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;
  FD := CadreDessinBGRA1.GetDocDessin();
  DisplayPOI(FD);
end;

procedure TMainWindow.acDummyExecute(Sender: TObject);
begin
  pass;
end;

procedure TMainWindow.acCourbeParoisCacheesExecute(Sender: TObject);
begin
   CadreDessinBGRA1.SetCurrentNatureCourbe(nocPAROIS_CACHEE);
end;

procedure TMainWindow.acCourbeEcoulementsExecute(Sender: TObject);
begin
   CadreDessinBGRA1.SetCurrentNatureCourbe(nocECOULEMENT);
end;

procedure TMainWindow.acCourbeChenalDeVouteExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureCourbe(nocCHENAL_VOUTE);
end;

procedure TMainWindow.acChargerCanevasTopoExecute(Sender: TObject);
var
  QFileName: TStringDirectoryFileName;
begin
  if (Not QuestionOuiNon(rsMISC_SAUVEGARDE_CONSEILLEE)) then Exit;
  QFileName :=  GetGHCaveDrawDirectory() + 'Dessin001.gcd';
  if (DoDialogOpenFilename('Polygonales GHCaveDraw (*.gcp)|*.gcp|Tous|*.*', QFileName)) then
  begin
    if (RemplacerCanevasTopo(QFileName)) then
    begin
      ShowMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
    end;
  end;
end;

procedure TMainWindow.acCloseDocumentExecute(Sender: TObject);
var
  EWE: TParamsVue2D;
begin
  EWE := CadreDessinBGRA1.GetParametresVue2D();
  if (QuestionOuiNon(GetResourceString('Fermer le document courant'))) then InitialiserGHC(EWE);
end;

procedure TMainWindow.acAproposExecute(Sender: TObject);
begin
  DisplayHelpSystem(CadreDessinBGRA1.GetDocDessin, '');
end;

procedure TMainWindow.acAddCurrCourbeAtListForScrapsExecute(Sender: TObject);
var
  EWE: Integer;
begin
  EWE := CadreDessinBGRA1.GetCurrCourbeIdx();
  editNumsCourbesPoly.Text := editNumsCourbesPoly.Text + Format('%d', [EWE]);
end;

procedure TMainWindow.acAddCurrPolylineAtListForScrapsExecute(Sender: TObject);
var
  EWE: Integer;
begin
  EWE := CadreDessinBGRA1.GetCurrPolylineIdx();
  editNumsCourbesPoly.Text := editNumsCourbesPoly.Text + Format('%d', [EWE]);
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

procedure TMainWindow.acCourbeLignesPentesExecute(Sender: TObject);
begin
   CadreDessinBGRA1.SetCurrentNatureCourbe(nocLIGNES_PENTE);
end;

procedure TMainWindow.acCourbeMiniRessautExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureCourbe(nocMARCHE);
end;

procedure TMainWindow.acCourbeMurMaconneExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureCourbe(nocMUR_MACONNE);
end;

procedure TMainWindow.acCourbeParoiFracasseeExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureCourbe(nocPAROI_FRACASSEE);
end;

procedure TMainWindow.acCourbeParoiIncertaineExecute(Sender: TObject);
begin
  CadreDessinBGRA1.SetCurrentNatureCourbe(nocPAROI_INCERTAINE);
end;

procedure TMainWindow.acEditLigneExecute(Sender: TObject);
var
  FD: TDocumentDessin;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  AfficherMessageErreur(Format('Nb objets: %d %s', [FD.GetNbSimpleLignes(), 'lignes']));
  if (0 = FD.GetNbSimpleLignes()) then CadreDessinBGRA1.SetModeTravail(mtNONE)
                                  else CadreDessinBGRA1.SetModeTravail(mtSELECT_LIGNE);
end;

procedure TMainWindow.acNouvelObjetExecute(Sender: TObject);
begin
  acSetModeTravailNoneExecute(self);
  //PurgerBoutonsDrw;
  S666(btnDrwTypeObjet1, acNewScrap);
  S666(btnDrwTypeObjet2, acNewCourbe);
  S666(btnDrwTypeObjet3, acNewPolylign);
  S666(btnDrwTypeObjet4, acNewPolygone);
  S666(btnDrwTypeObjet5, acNewLigne);
  S666(btnDrwTypeObjet6, acNewFreeHandCurve);
  S666(btnDrwTypeObjet7, acNewSymbole);
  S666(btnDrwTypeObjet8, acNewTexte);
  S666(btnDrwTypeObjet9, acNewImage);

end;

///!\ Bugs inexplicables (crash)
//Dans la BDD, poser des index KEY dans la première colonne des tables ArcCourbes et VertexXXX
procedure TMainWindow.acOpenCaveMapExecute(Sender: TObject);
{$IFDEF TIDBASEPOINT_AS_TEXT}
begin
  pass;
end;

{$ELSE}
var
  dlgOCM: TdlgOpenCaveMap;
  FD: TDocumentDessin;
  QParamsVue2D: TParamsVue2D;
begin
  {$IFDEF OPENCAVEMAP}
  FD := CadreDessinBGRA1.GetDocDessin();
  QParamsVue2D := CadreDessinBGRA1.GetParametresVue2D();
  dlgOCM := TdlgOpenCaveMap.Create(Application);
  try
    if (dlgOCM.Initialiser(FD)) then
    begin
      dlgOCM.ShowModal;
      if (dlgOCM.ModalResult = mrOK) then
      begin

        FD := dlgOCM.GetDocDessinExtraitFromBDD();
        FD.SetMiniEtMaxi();

        showmessagefmt('%d scraps, %d groupes, %d courbes', [FD.GetNbScraps(), FD.GetNbGroupes(), FD.GetNbCourbes()]);
        CadreDessinBGRA1.MyDocumentDessin := FD;
        CadreDessinBGRA1.PreparerCtxt(QParamsVue2D);

        CadreDessinBGRA1.SetCurrentGroupeByIdx(0);

        CadreDessinBGRA1.SetCurrentNatureCourbe(nocPAROI);
        CadreDessinBGRA1.SetCurrentNatureLigne(nolPENTE);
        CadreDessinBGRA1.SetCurrentNaturePolygone(nopARGILE);
        CadreDessinBGRA1.SetCurrentNatureSymbole(nosPHOTO);
        CadreDessinBGRA1.SetCurrentNatureTexte(notDEBUG);
        // peupler les listes
        CadreListeGroupesForMainWindow1.Initialiser(CadreDessinBGRA1, True, LocaliserGroupe);
        //CadreListeGroupes1.Rafraichir(CadreDessinBGRA1);
        //ListerLesImages(0);
        // nom actuel du dessin
        FNomFichierActuel := 'From BDD';
        self.Caption := Format(FMT_APPLICATION_TITLE,[ExtractFileName(FNomFichierActuel), 'Editor']);
        //Result := True;
        cmbOperationsBooleennes.ItemIndex := DEFAULT_MODE_OPERATIONS_BOOLEENNES;
      end;
    end;
  finally
    FreeAndNil(dlgOCM);//dlgOCM.Free;
  end;
  {$ENDIF}
end;
{$ENDIF TIDBASEPOINT_AS_TEXT}

procedure TMainWindow.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Canclose := QuestionOuiNon(rsMSG_QUIT);
end;



// Peu utilisé. Utiliser un menu
procedure TMainWindow.Button4Click(Sender: TObject);
var
  IdxScrap, IdxPolygone: Integer;
  FD: TDocumentDessin;
  MyScrap: TScrap;
  MyPolygone: TPolygone;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  IdxScrap    := CadreDessinBGRA1.GetCurrScrapIdx();
  IdxPolygone := CadreDessinBGRA1.GetCurrPolygoneIdx();
  FD.TrimPolygonByScrap(IdxScrap, IdxPolygone);
  CadreDessinBGRA1.RefreshVue();
end;
// Délimiter tous les polygones par les scraps du groupe courant
procedure TMainWindow.Button5Click(Sender: TObject);
var
  FD: TDocumentDessin;
  IdxGroupe: TIDGroupeEntites;
begin
  CadreDessinBGRA1.SetMsgInLbMessage(errBUSY, 'Détourage du groupe ...');
  FD := CadreDessinBGRA1.GetDocDessin();
  IdxGroupe := CadreDessinBGRA1.GetCurrentGroupeIdx();
  FD.TrimPolygonsOfGroupe(IdxGroupe);
  CadreDessinBGRA1.RefreshVue();
  CadreDessinBGRA1.SetMsgInLbMessage(errOK, 'Détourage OK');
end;

procedure TMainWindow.CadreDessinBGRA1Click(Sender: TObject);
begin

end;

procedure TMainWindow.CoolBar1Change(Sender: TObject);
begin

end;

procedure TMainWindow.editNumsCourbesPolyChange(Sender: TObject);
begin

end;


// Peu utilisé. Utiliser un menu
procedure TMainWindow.btnPolygoneFrom2ListesCourbesClick(Sender: TObject);
begin
  MakePolygonFromTwoCurvesFamily(editNumsCourbesPoly.Text);
end;

procedure TMainWindow.btnPolyFrom4LastCourbesClick(Sender: TObject);
begin

end;

// Construit un polygone délimité par 4 courbes sélectionnées
// Process: (n est l'index de la dernière courbe)
// - 1: Un polygone est généré depuis les courbes n-3 et n-2
// - 2: Un autre polygone est généré depuis les courbes n-1 et n
// - 3: Les deux polygones résultants sont intersectés
// - 4: Le résultat de cette intersection est ajouté à la table des polygones
// - 5: Si DoRemoveCurves est armé, on retire les courbes de construction
procedure TMainWindow.MakePolygonFromFourCurves(const IdxC0, IdxC1, IdxC2, IdxC3: integer; const DoRemoveCurves: boolean);
var
  ArrIdxCourbes : TArrayIdxObjets;
  FD: TDocumentDessin;
  Nb, i: Integer;
  IdxGrp: TIDGroupeEntites;
  Polygone0, Polygone1, SR: TPolygone;
  WU0, WU1: Boolean;
begin
  FD := CadreDessinBGRA1.GetDocDessin();
  IdxGrp := CadreDessinBGRA1.GetCurrentGroupeIdx();
  SetLength(ArrIdxCourbes, 2);
  ArrIdxCourbes[0] := IdxC0;
  ArrIdxCourbes[1] := IdxC1;
  WU0 := (GenererPolygoneDepuisArrayCourbes(ArrIdxCourbes, IdxGrp, Polygone0));
  ArrIdxCourbes[0] := IdxC2;
  ArrIdxCourbes[1] := IdxC3;
  WU1 := (GenererPolygoneDepuisArrayCourbes(ArrIdxCourbes, IdxGrp, Polygone1));
  if (WU0 and WU1) then
  begin
    case MergePolygones(FD, Polygone0, Polygone1, 0, SR) of
      errMERGE_POLYGONES_OK:
        begin
          AfficherMessageErreur(GetResourceString(rsMSG_MERGE_POLY_ACTION_OK));
          // on ajoute le polygone fusionné
          FD.AddPolygone(SR, True);
          // un premier redessin
          CadreDessinBGRA1.RefreshVue();
          // et on supprime les courbes support après demande de confirmation
          if (DoRemoveCurves and QuestionOuiNon(GetResourceString(rsMSG_MERGE_CONFIRM_REMOVE_BUILD_COURBES))) then
          begin
            //for i := 1 to 4 do FD.RemoveLastObject(mseCOURBES);
            for i := 1 to 2 do FD.RemoveLastObject(mseCOURBES);
            // un nouveau redess pour acquitter la suppression des courbes
            CadreDessinBGRA1.RefreshVue();
          end;
          if (not QuestionOuiNon(GetResourceString(rsMSG_MERGE_CONFIRM_CORRECT_RESULT))) then
          begin
            FD.RemoveLastObject(msePOLYGONES);
          end;
          // et un redessin pour le résultat final
          CadreDessinBGRA1.RefreshVue();
        end;
      errMERGE_POLYGONES_GROUPES_MISMATCH: NotifyError(GetResourceString(rsMSG_MERGE_POLY_GROUPES_MISMATCH));
      errMERGE_POLYGONES_NO_INTERSECT    : NotifyError(GetResourceString(rsMSG_MERGE_POLY_OBJ_DISJOINTS));
      else
        NotifyError(GetResourceString(rsMSG_MERGE_POLY_ANY_ERROR));
    end;
  end;
end;
// Construit un polygone délimité par deux familles de courbes
// définies dans Expr. Les familles sont séparées par un '*' qui est un produit
// Process:
// - 1: L'expression est décomposée en deux parties
// - 2: Un polygone est construit à partir de la première famille de courbes
// - 3: Un polygone est construit à partir de la seconde  famille de courbes
// - 4: Les deux polygones sont fusionnés par l'opération booléenne d'intersection
// - 5: Le polygone est ajouté au dessin
procedure TMainWindow.MakePolygonFromTwoCurvesFamily(const Expr: string);
var
  IdxGrp: TIDGroupeEntites;
  EWE: TGHStringArray;
  ArrIdxCourbes: TArrayIdxObjets;
  Polygone0, Polygone1, SR: TPolygone;
  FD: TDocumentDessin;
  WU0, WU1: Boolean;
begin
  // l'expression doit impérativement comporter deux familles de courbes
  // séparées par le symbole *
  if (Pos('*', Expr) = 0) then
  begin
    ShowMessage('Les deux familles de courbes doivent être séparées par un *');
    Exit;
  end;
  EWE := split(trim(Expr), '*');
  FD := CadreDessinBGRA1.GetDocDessin();
  IdxGrp := CadreDessinBGRA1.GetCurrentGroupeIdx;
  MakeListeCourbesPolylignForGenScrapOrPolygons(EWE[0], tcpCOURBE, ArrIdxCourbes);
  WU0 := GenererPolygoneDepuisArrayCourbes(ArrIdxCourbes, IdxGrp, Polygone0);
  MakeListeCourbesPolylignForGenScrapOrPolygons(EWE[1], tcpCOURBE, ArrIdxCourbes);
  WU1 := GenererPolygoneDepuisArrayCourbes(ArrIdxCourbes, IdxGrp, Polygone1);
  // opération booléenne d'intersection
  // test: 193;194; * 195;196; (grotte du Clocher avec intersections)
  if (WU0 and WU1) then
  begin
    case MergePolygones(FD, Polygone0, Polygone1, 0, SR) of
      errMERGE_POLYGONES_OK:
        begin
          AfficherMessageErreur(GetResourceString(rsMSG_MERGE_POLY_ACTION_OK));
          FD.AddPolygone(SR, True);
          // un premier redessin pour contrôle
          CadreDessinBGRA1.RefreshVue();
          // si le résultat est incorrect, on supprime l'objet créé
          if (not QuestionOuiNon(GetResourceString(rsMSG_MERGE_CONFIRM_CORRECT_RESULT))) then
          begin
            FD.RemoveLastObject(msePOLYGONES);
            // et on redessine
            CadreDessinBGRA1.RefreshVue();
          end;
        end;
      errMERGE_POLYGONES_GROUPES_MISMATCH:   NotifyError(GetResourceString(rsMSG_MERGE_POLY_GROUPES_MISMATCH));
      errMERGE_POLYGONES_NO_INTERSECT    :   NotifyError(GetResourceString(rsMSG_MERGE_POLY_OBJ_DISJOINTS));
    else
      NotifyError(GetResourceString(rsMSG_MERGE_POLY_ANY_ERROR));
    end;
  end;
end;


// BoolOpsMode = Ord(ctIntersection, ctUnion, ctDifference, ctXor);
procedure TMainWindow.FusionnerDeuxScraps(const Idx1, Idx2: integer; const BoolOpsMode: byte);
var
  FD: TDocumentDessin;
  S1, S2, SR: TScrap;
begin
  FD := CadreDessinBGRA1.GetDocDessin;
  if (Idx1 = Idx2) then
  begin
    ShowMessage( GetResourceString(rsMSG_MERGE_POLY_SAME_OBJ));
    Exit;
  end;
  S1 := FD.GetScrap(Idx1);
  S2 := FD.GetScrap(Idx2);
  case MergeScraps(FD, S1, S2, BoolOpsMode, SR) of
    errMERGE_SCRAPS_OK:
      begin
        AfficherMessageErreur(GetResourceString(rsMSG_MERGE_POLY_ACTION_OK) + ': ' + SR.Nom);
        FD.MarquerScrapAEffacer(Idx1);
        FD.MarquerScrapAEffacer(Idx2);
        FD.AddScrap(SR, True);
        FD.NettoyerScraps;
        CadreDessinBGRA1.RefreshVue();
      end;
    errMERGE_SCRAPS_GROUPES_MISMATCH:
      begin
        NotifyError(GetResourceString(rsMSG_MERGE_POLY_GROUPES_MISMATCH));
      end;
    errMERGE_SCRAPS_NO_INTERSECT:
      begin
        NotifyError(GetResourceString(rsMSG_MERGE_POLY_OBJ_DISJOINTS));
      end;
    else
      NotifyError(GetResourceString(rsMSG_MERGE_POLY_ANY_ERROR));
  end;
end;

procedure TMainWindow.GenererPolyDepuisArrayIntersecCourbes(const ArrIdxCourbes: TArrayIdxObjets;
                                                            const IdxGrp: TIDGroupeEntites;
                                                            out PolygonOut: TPolygone);
var
  FD: TDocumentDessin;
  S1: TArrayOfTCourbe;
  i, n: Integer;
begin
  FD := CadreDessinBGRA1.GetDocDessin;
  n := 1 + High(ArrIdxCourbes);
  AfficherMessageErreur(Format('GenererPolyDepuisArrayIntersecCourbes: Grp: %d; %d', [n, IdxGrp]));
  SetLength(S1, n);
  for i := 0 to n - 1 do
  begin
    S1[i] := FD.GetCourbe(ArrIdxCourbes[i]);
    AfficherMessageErreur(Format('S1[%d][%d] = %d', [i, ArrIdxCourbes[i], S1[i].IDGroupe]));
  end;
  case GenererUnPolygoneDepuisIntersectedCourbes(FD, S1, IdxGrp, PolygonOut.Sommets) of
    errGEN_SCRAP_OR_POLYGONE_FROM_COURBES_ANY_ERROR: pass;
    //... TODO
  else
    pass;
  end;
end;
function TMainWindow.GenererScrapDepuisArrayCourbes(const ArrIdxCourbes: TArrayIdxObjets;
                                                    const IdxGrp: TIDGroupeEntites;
                                                    out   ScrapOut: TScrap): boolean;
var
  FD: TDocumentDessin;
  S1: TArrayOfTCourbe;
  i, n: Integer;
begin
  Result := false;
  FD := CadreDessinBGRA1.GetDocDessin;
  n := 1 + High(ArrIdxCourbes);
  AfficherMessage(Format('GenererScrapDepuisArrayCourbes: Grp: %d; %d', [n, IdxGrp]));
  SetLength(S1, n);
  for i := 0 to n - 1 do
  begin
    S1[i] := FD.GetCourbe(ArrIdxCourbes[i]);
    AfficherMessageErreur(Format('S1[%d] = %d', [i, S1[i].IDGroupe]));
  end;
  case GenererUnScrapDepuisCourbes(FD, S1, IdxGrp, ScrapOut) of
    errGEN_SCRAP_FROM_COURBES_OK:
      begin
        AfficherMessageErreur(Format('Génération du scrap OK: %d vertex', [1+High(ScrapOut.Sommets)]));
        ScrapOut.Couleur  := clSilver;
        ScrapOut.IDGroupe := IdxGrp;
        Result := True;
      end;
    errGEN_SCRAP_FROM_COURBES_GROUPES_MISMATCH:
      begin
        NotifyError(GetResourceString(rsERR_MSG_GROUPES_MISMATCH));
      end;
    else
      NotifyError(GetResourceString(rsERR_MSG_CREATION_SCRAP_FAILED));
  end;
end;
function TMainWindow.GenererPolygoneDepuisArrayCourbes(const ArrIdxCourbes: TArrayIdxObjets; const IdxGrp: TIDGroupeEntites; out PolygoneOut: TPolygone): boolean;
var
  FD: TDocumentDessin;
  S1: TArrayOfTCourbe;
  i, n: Integer;
  EWE: TVertexPolygon;
begin
  Result := false;
  FD := CadreDessinBGRA1.GetDocDessin;
  n := 1 + High(ArrIdxCourbes);
  AfficherMessage(Format('GenererScrapDepuisArrayCourbes: Grp: %d; %d', [n, IdxGrp]));
  SetLength(S1, n);
  for i := 0 to n - 1 do
  begin
    S1[i] := FD.GetCourbe(ArrIdxCourbes[i]);
    AfficherMessageErreur(Format('S1[%d] = %d', [i, S1[i].IDGroupe]));
  end;
  case GenererUnPolygoneDepuisCourbes(FD, S1, IdxGrp, PolygoneOut) of
    errGEN_POLYGONE_FROM_COURBES_OK:
      begin
        AfficherMessageErreur(Format('Génération du scrap OK: %d vertex', [1+High(PolygoneOut.Sommets)]));
        Result := True;
      end;
   errGEN_POLYGONE_FROM_COURBES_GROUPES_MISMATCH:
      begin
        NotifyError(GetResourceString(rsERR_MSG_GROUPES_MISMATCH));
      end;
   else
      NotifyError(GetResourceString(rsERR_MSG_CREATION_SCRAP_FAILED));
  end;
end;




function TMainWindow.GenererScrapDepuisArrayPolylignes(const ArrIdxPolylignes: TArrayIdxObjets; const IdxGrp: TIDGroupeEntites; out ScrapOut: TScrap): boolean;
var
  FD: TDocumentDessin;
  i, n: Integer;
  S1: TArrayOfTPolyligne;
begin
  Result := false;
  FD := CadreDessinBGRA1.GetDocDessin;
  n := 1 + High(ArrIdxPolylignes);
  AfficherMessage(Format('GenererScrapDepuisArrayPolylignes: Grp: %d; %d', [n, IdxGrp]));
  SetLength(S1, n);
  for i := 0 to n - 1 do
  begin
    S1[i] := FD.GetPolyligne(ArrIdxPolylignes[i]);
    AfficherMessageErreur(Format('S1[%d] = %d', [i, S1[i].IDGroupe]));
  end;
  case GenererUnScrapDepuisPolylignes(FD, S1, IdxGrp, ScrapOut) of
    errGEN_SCRAP_FROM_POLYLIGNES_OK:
      begin
        AfficherMessageErreur(Format('Génération du scrap OK: %d vertex', [1+High(ScrapOut.Sommets)]));
        ScrapOut.Couleur  := clSilver;
        ScrapOut.IDGroupe := IdxGrp;
        Result := True;
      end;
    errGEN_SCRAP_FROM_POLYLIGNES_GROUPES_MISMATCH:
      begin
        NotifyError(GetResourceString(rsERR_MSG_GROUPES_MISMATCH));
      end;
    else
      NotifyError(GetResourceString(rsERR_MSG_CREATION_SCRAP_FAILED));
  end;
end;

function  TMainWindow.MakeListeCourbesPolylignForGenScrapOrPolygons(const strListeIdxCourbes: string;
                                                                    const QMode: TCourbesOrPolylignes;
                                                                    out   ArrIdxObj: TArrayIdxObjets): boolean;
var
  FD : TDocumentDessin;
  EWE: TGHStringArray;
  LS : TStringList;
  i, n, X: Integer;
  WU: String;

  FCurrGrp: TIDGroupeEntites;
  P: SizeInt;
  j: integer;
  U1, U2: Int64;
  procedure totoCourbe(const Q: Int64);
  var
    C: TCourbe;
  begin
    if (Q >= 0) then
    begin
      C := FD.GetCourbe(Q);
      if (FCurrGrp = C.IDGroupe) then LS.Add(format('%d', [Q]));
    end;
  end;
  procedure totoPolyligne(const Q: Int64);
  var
    PL: TPolyLigne;
  begin
    if (Q >= 0) then
    begin
      PL := FD.GetPolyligne(Q);
      if (FCurrGrp = PL.IDGroupe) then LS.Add(format('%d', [Q]));
    end;
  end;
  procedure toto(const Q: Int64);
  begin
    case QMode of
      tcpCOURBE   : totoCourbe(Q);
      tcpPOLYLIGNE: totoPolyligne(Q);
    end;
  end;
begin
  Result := false;
  SetLength(ArrIdxObj, 0);
  FCurrGrp := CadreDessinBGRA1.GetCurrentGroupeIdx;
  AfficherMessage(Format('--- MakeListeCourbesForGenScrapOrPolygons: %d, "%s"', [FCurrGrp, strListeIdxCourbes]));
  FD := CadreDessinBGRA1.GetDocDessin;
  EWE := Split(Trim(strListeIdxCourbes), ';');
  LS := TStringList.Create;
  try
    LS.Sorted     := false;
    LS.Clear;
    for i := 0 to High(EWE) do
    begin
      WU := Trim(EWE[i]);
      // Recherche d'une expression de la forme 123-456 = intervalle
      P := Pos('-', WU);
      if (P > 0) then
      begin
        try
          U1 := StrToInt64Def(Copy(WU, 1, P-1), -1);
          U2 := StrToInt64Def(Copy(WU, P+1, Length(WU)), -1);
          if ((U1 <> U2) and (U2 > U1)) then for j := U1 to U2 do toto(j);
        except
          ;
        end;
      end else
      begin
        // - Est un nombre positif ou nul
        toto(StrToInt64Def(WU, -1));  // StrToInt64Def élimine tout ce qui n'est pas un entier
      end;
    end;
    n := LS.Count;
    AfficherMessageErreur(inttostr(n));
    SetLength(ArrIdxObj, n);
    for i := 0 to n - 1 do
    begin
      ArrIdxObj[i] := StrToInt64Def(LS.Strings[i], -1);
      AfficherMessage(Format('------- %d/%d: %d', [i, n, ArrIdxObj[i]]));
    end;
    Result := true;
  finally
    FreeAndNil(LS);//LS.Free;
  end;
end;

procedure TMainWindow.GenererScrapDepuisStrListeIdxCourbes(const strListeIdxCourbes: string;
                                                           const QIdxGroupe: TIDGroupeEntites;
                                                           out ScrapOut: TScrap);
var
  ArrIdxCourbes: TArrayIdxObjets;
  i, n: Integer;
  FD: TDocumentDessin;
begin
  CadreDessinBGRA1.SetMsgInLbMessage(errBUSY, 'Génération du scrap ...');
  MakeListeCourbesPolylignForGenScrapOrPolygons(strListeIdxCourbes, tcpCOURBE, ArrIdxCourbes);
  n := high(ArrIdxCourbes);
  if (GenererScrapDepuisArrayCourbes(ArrIdxCourbes, QIdxGroupe, ScrapOut)) then
  begin
    FD := CadreDessinBGRA1.GetDocDessin();
    FD.AddScrap(ScrapOut, True);
    CadreDessinBGRA1.RefreshVue();
    CadreDessinBGRA1.SetMsgInLbMessage(errOK, 'Scrap généré correctement');
  end;
end;
procedure TMainWindow.GenererPolygoneDepuisStrLstIdxInterCourbes(const strListeIdxCourbes: string; const QIdxGroupe: TIDGroupeEntites; out PolygonOut: TPolygone);
var
  ArrIdxCourbes: TArrayIdxObjets;
  i, n: Integer;
  FD: TDocumentDessin;
begin
  MakeListeCourbesPolylignForGenScrapOrPolygons(strListeIdxCourbes, tcpCOURBE, ArrIdxCourbes);
  n := high(ArrIdxCourbes);
  if (GenererPolygoneDepuisArrayCourbes(ArrIdxCourbes, QIdxGroupe, PolygonOut)) then
  begin
    FD := CadreDessinBGRA1.GetDocDessin();
    FD.AddPolygone(PolygonOut, True);
    CadreDessinBGRA1.RefreshVue();
  end;
end;


procedure TMainWindow.GenererScrapDepuisStrListeIdxPolylignes(const strListeIdxPolylignes: string;
                                                              const QIdxGroupe: TIDGroupeEntites;
                                                              out ScrapOut: TScrap);
var
  ArrIdxPolylines: TArrayIdxObjets;
  FD: TDocumentDessin;
begin
  CadreDessinBGRA1.SetMsgInLbMessage(errBUSY, 'Génération du scrap ...');
  MakeListeCourbesPolylignForGenScrapOrPolygons(strListeIdxPolylignes, tcpPOLYLIGNE, ArrIdxPolylines);
  if (GenererScrapDepuisArrayPolylignes(ArrIdxPolylines, QIdxGroupe, ScrapOut)) then
  begin
    FD := CadreDessinBGRA1.GetDocDessin();
    FD.AddScrap(ScrapOut, True);
    CadreDessinBGRA1.RefreshVue();
    CadreDessinBGRA1.SetMsgInLbMessage(errOK, 'Scrap généré correctement');
  end;

end;



procedure TMainWindow.FusionnerDeuxPolygones(const Idx1, Idx2: integer; const BoolOpsMode: byte);
var
  FD: TDocumentDessin;
  S1, S2, SR: TPolygone;
begin
  FD := CadreDessinBGRA1.GetDocDessin;
  if (Idx1 = Idx2) then
  begin
    ShowMessage(GetResourceString(rsMSG_MERGE_POLY_SAME_OBJ));
    Exit;
  end;
  S1 := FD.GetPolygone(Idx1);
  S2 := FD.GetPolygone(Idx2);
  // vérifier si les polygones sont de même nature
  if (S1.IDStylePolygone <> S2.IDStylePolygone) then
  begin
    if (not QuestionOuiNon(GetResourceString(rsMSG_MERGE_POLY_TYPES_MISMATCH_QUESTION))) then Exit;
  end;
  case MergePolygones(FD, S1, S2, BoolOpsMode, SR) of
    errMERGE_POLYGONES_OK:
      begin
        AfficherMessageErreur(GetResourceString(rsMSG_MERGE_POLY_ACTION_OK));
        FD.MarquerPolygoneAEffacer(Idx1);
        FD.MarquerPolygoneAEffacer(Idx2);
        FD.AddPolygone(SR, True);
        FD.NettoyerPolygones;
        CadreDessinBGRA1.RefreshVue();
      end;
    errMERGE_POLYGONES_GROUPES_MISMATCH:
      begin
        NotifyError(GetResourceString(rsMSG_MERGE_POLY_GROUPES_MISMATCH));
      end;
    errMERGE_POLYGONES_NO_INTERSECT:
      begin
        NotifyError(GetResourceString(rsMSG_MERGE_POLY_OBJ_DISJOINTS));
      end;
    else
      NotifyError(GetResourceString(rsMSG_MERGE_POLY_ANY_ERROR));
  end;
end;
// fusion avec opératins booléennes, différente de la concaténation
procedure TMainWindow.FusionnerDeuxPolylignes(const Idx1, Idx2: integer; const BoolOpsMode: byte);
var
  FD: TDocumentDessin;
  S1, S2, SR: TPolyLigne;
begin
  FD := CadreDessinBGRA1.GetDocDessin;
  if (Idx1 = Idx2) then
  begin
    ShowMessage(GetResourceString(rsERR_MSG_SAME_OBJECTS));
    Exit;
  end;
  S1 := FD.GetPolyligne(Idx1);
  S2 := FD.GetPolyligne(Idx2);
  case MergePolylignes(FD, S1, S2, BoolOpsMode, SR) of
    errMERGE_POLYLIGNES_OK:
      begin
        AfficherMessageErreur('Fusion des polylignes OK: ');
        FD.MarquerPolylignesAEffacer(Idx1);
        FD.MarquerPolylignesAEffacer(Idx2);
        FD.AddPolyLigne(SR, True);
        FD.NettoyerPolylignes;
        CadreDessinBGRA1.RefreshVue();
      end;
    errMERGE_POLYLIGNES_GROUPES_MISMATCH:
      begin
        NotifyError(GetResourceString(rsERR_MSG_GROUPES_MISMATCH));
      end;
    errMERGE_POLYLIGNES_NO_INTERSECT:
      begin
        NotifyError('Les polylignes sont disjointes');
      end;
    else
      NotifyError('Echec de la fusion des polylignes');
  end;
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

procedure TMainWindow.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  EWE: String;
begin
  EWE := FileNames[0];
  ShowMessage(EWE);
end;

procedure TMainWindow.lsbGroupesClick(Sender: TObject);
begin

end;

procedure TMainWindow.MenuItem84Click(Sender: TObject);
begin

end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
  PairSplitter1.Position := self.ClientWidth - 425;
end;

procedure TMainWindow.FormShow(Sender: TObject);
var
  QParamsVue2D: TParamsVue2D;
  QGrdSpacing: Extended;
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
  PairSplitter1.Position := self.ClientWidth - 425;
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
  QParamsVue2D.BackGroundColor    := clWhite;
  QParamsVue2D.GroupeBackColor    := RGBToColor(205, 254, 239);
  QGrdSpacing := 100.00;
  QParamsVue2D.MainGrid.setFrom(tqGRID, RGBToColor(255, 128,  64), QGrdSpacing     , QGrdSpacing / 20, True, True);
  QParamsVue2D.SecGrid.setFrom (tqGRID, RGBToColor(242, 203, 159), QGrdSpacing / 10,  1.00, True, True);


  QParamsVue2D.DoDrawScrapsMonochromes     := false;
  QParamsVue2D.ColorScrapMonochrome        := clGray;
  QParamsVue2D.PenteLimiteDisp             := 5.00;
  QParamsVue2D.RegleDisplayed              := true;
  QParamsVue2D.TailleEchelle               := 50.00;

  InitialiserGHC(QParamsVue2D);


end;

procedure TMainWindow.lsbImagesClickCheck(Sender: TObject);
var
  FD: TDocumentDessin;
  EWE: TImageObject;
  WU: Boolean;
  QIdx: Integer;
begin
  QIdx := lsbImages.ItemIndex;
  FD   :=  CadreDessinBGRA1.GetDocDessin;
  WU   :=  lsbImages.Checked[QIdx];
  EWE  :=  FD.GetImage(QIdx);
  EWE.Displayed := WU;
  FD.PutImage(QIdx, EWE);
  CadreDessinBGRA1.RefreshVue();
end;

procedure TMainWindow.MenuItem24Click(Sender: TObject);
var
  MyGroupeIdx: TIDGroupeEntites;
begin
  MyGroupeIdx := 0;
  if (SelectIndexGroupe(CadreDessinBGRA1.MyDocumentDessin,
                        CadreDessinBGRA1.GetCurrentGroupeIdx(),
                        MyGroupeIdx)) then
  begin
    ShowMessageFmt('Groupe %d selected', [MyGroupeIdx]);
  end;
end;



procedure TMainWindow.QuickSave();
var
  BackUpDir: String;
  QFileName: String;
  AAAA, MM, JJ, HH, MN, SS, MS: word;
  Maintenant: TDateTime;
begin
  if (Not CadreDessinBGRA1.DoDraw) then Exit;

  BackUpDir := GetGHCaveDrawDirectory() + 'Sauvegardes_rapides' + PathDelim;
  ForceDirectories(BackUpDir);
  Maintenant := Now();
  DecodeDate(Maintenant, AAAA, MM, JJ);
  DecodeTime(Maintenant, HH, MN, SS, MS);

  QFileName := BackUpDir +
               Format('QSave_%.4d%.2d%.2d_%.2dh%.2dm%.2ds%.3dms.gcd',
                      [AAAA, MM, JJ, HH, MN, SS, MS]);
  CadreDessinBGRA1.SetMsgInLbMessage(errBUSY, Format('Sauvegarde de %s', [ExtractFileName(QFileName)]));
    CadreDessinBGRA1.MyDocumentDessin.SaveToFile(QFileName, -1, false);
    AfficherMessage(GetResourceString(rsMISC_DONE_WITH_SUCCESS));
  CadreDessinBGRA1.SetMsgInLbMessage(errOK, 'Opération terminée');
end;



// générer un scrap d'après les parois d'un groupe
function TMainWindow.GenererUnScrapDepuisLesParoisDunGroupe(const FD: TDocumentDessin;
                                                            const QIdxGroupe: TIDGroupeEntites;
                                                            out   ScrapOut: TScrap): boolean;
var
  EWE: String;
  C: TCourbe;
  i, Nb: Integer;
  WU: Boolean;
  Groupe: TGroupeEntites;
begin
  result := false;
  Groupe    := FD.GetGroupeByIDGroupe(QIdxGroupe);
  EWE := '';
  AfficherMessage('GenererUnScrapDepuisLesParoisDunGroupe: ' + Groupe.NomGroupe);
  Nb := FD.GetNbCourbes();
  if (Nb = 0) then Exit;
  try
    for i := 0 to Nb-1 do
    begin
      C := FD.GetCourbe(i);
      WU := (Groupe.IDGroupeEntites = C.IDGroupe) and
            (C.IDStyleCourbe in [nocPAROI, nocPAROIS_CACHEE, nocPAROI_INCERTAINE, nocMUR_MACONNE,nocPAROI_FRACASSEE]);
      if (WU) then EWE := EWE + Format('%d;', [i]);
    end;
    // on laisse la possibilité de modifier la liste des courbes
    if (InputQuery('Génération de scrap', 'Liste des courbes', EWE)) then
    begin
      GenererScrapDepuisStrListeIdxCourbes(EWE, QIdxGroupe, ScrapOut);
      Result := True;
    end;
  except
  end;
end;
// lister les images d'un doc topo
procedure TMainWindow.ListerLesImages(const Idx: integer);
var
  FD: TDocumentDessin;
  i, Nb: Integer;
  EWE: TImageObject;
begin
  lsbImages.Clear;
  FD := CadreDessinBGRA1.GetDocDessin;
  Nb := FD.GetNbImages();
  lbNbImages.Caption := Format('%d images', [Nb]);
  if (Nb > 0) then
  begin
    for i := 0 to Nb - 1 do
    begin
      EWE := FD.GetImage(i);
      lsbImages.Items.Add(EWE.SrcFilename);
      lsbImages.Checked[i] := EWE.Displayed;
    end;
    lsbImages.ItemIndex := Idx;
  end;
end;
// Construit un polygone délimité par l'intersection de deux familles de courbes
// Les familles de courbes sont séparées par le symbole '*' (produit)
// Exemples d'expressions: "193;194; * 196;197" ; "193-195; * 198;199;"
procedure TMainWindow.MakeScrapFromTwoCurvesFamily(const Expr: string);
var
  EWE: TGHStringArray;
  IdxGrp: TIDGroupeEntites;
  strListeIdxCourbes: String;
  ArrIdxCourbes: TArrayIdxObjets;
  ScrapOut0, ScrapOut1, SR: TScrap;
  n: Integer;
  FD: TDocumentDessin;
begin
  IdxGrp := CadreDessinBGRA1.GetCurrentGroupeIdx;
  FD := CadreDessinBGRA1.GetDocDessin();
  EWE := split(trim(Expr), '*');
  strListeIdxCourbes := EWE[0];
  MakeListeCourbesPolylignForGenScrapOrPolygons(strListeIdxCourbes, tcpCOURBE, ArrIdxCourbes);
  if (GenererScrapDepuisArrayCourbes(ArrIdxCourbes, IdxGrp, ScrapOut0)) then ScrapOut0.Couleur := clRed;
  strListeIdxCourbes := EWE[1];
  MakeListeCourbesPolylignForGenScrapOrPolygons(strListeIdxCourbes, tcpCOURBE, ArrIdxCourbes);
  if (GenererScrapDepuisArrayCourbes(ArrIdxCourbes, IdxGrp, ScrapOut1)) then ScrapOut1.Couleur := clBlue;
  // opération booléenne d'intersection
  case MergeScraps(FD, ScrapOut0, ScrapOut1, 0, SR) of
    errMERGE_SCRAPS_OK:
      begin
        AfficherMessageErreur(GetResourceString(rsMSG_MERGE_POLY_ACTION_OK) + ': ' + SR.Nom);
        SR.Couleur := clBlue;
        FD.AddScrap(SR, True);
        CadreDessinBGRA1.RefreshVue();
      end;
    errMERGE_SCRAPS_GROUPES_MISMATCH:    NotifyError(GetResourceString(rsMSG_MERGE_POLY_GROUPES_MISMATCH));
    errMERGE_SCRAPS_NO_INTERSECT    :    NotifyError(GetResourceString(rsMSG_MERGE_POLY_OBJ_DISJOINTS));
  else
    NotifyError(GetResourceString(rsMSG_MERGE_POLY_ANY_ERROR));
  end;
end;

procedure TMainWindow.pagectrlScrapsImagesChange(Sender: TObject);
begin
  //pnlPopUpActionsCourbes.Visible := (1 = pagectrlScrapsImages.ActivePageIndex);
end;

procedure TMainWindow.VueClick(Sender: TObject);
begin

end;

procedure TMainWindow.DisplayCurrentSuperGroupe(const SG: TSuperGroupe);
begin
  lbSuperGroupeName.Caption := SG.NomSuperGroupe;
end;

end.

