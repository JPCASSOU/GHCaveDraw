unit cdrdessincanvasBGRA;
{$INCLUDE CompilationParameters.inc}

// DONE: Installer le nouveau package BGRABitmap 8.7

(* Contexte de dessin pour GHCaveDraw
17/08/2013: Support des désobs, CA, cupules et arrivées/pertes
04/02/2014: Les courbes sans barbules utilisent le dessin des courbes de Bezier natives
09/04/2014: Suppression des mtCHOIX_*, doublonnant les mtSELECT_*
            Modification de courbes et polygones sans réarmement manuel
03/12/2014: Visées en antennes sont représentées en gris clair.
            Quadrillage en orange type 'papier millimétré'
05/01/2015: Support des polylignes (utilisent les styles de courbes)
20/02/2015: Insertion d'objets multiples de même type
25/02/2015: Support de la molette souris pour le zoom et les pans vue
            (conventions Inkscape)
25/02/2015: Suppression du support du tableau de zooms (inutilisé et inutile)
25/02/2015: Persistance du dessin des objets temporaires lors des pan/zoom
            (typiquement: dessin des courbes provisoires)
27/02/2015: Dessin à main levée
24/03/2015: Désactivation du mode d'édition des vertex de courbes (bugs très difficiles à résoudre)
24/03/2015: Déplacement des sommets de polygones OK
24/03/2015: Suppression de sommets OK: polylignes, polygones
25/03/2015: Suppression de sommets OK: courbes
25/03/2015: Déplacement des sommets de courbes OK (avec auto-lissage)
11/04/2015: Portage vers LINUX: Réglage des directives de compilation OK
09/06/2015: Mise en place d'un sémaphore dans RedessinEcran() pour éviter un bug lié à des appels rapprochés de RedessinEcran
12/06/2015: Corrections dans le support des textes tournés
15/06/2015: Affichage/masquage centerlines, quadrillages, textes, Nord
            Affichage de l'échelle et du Nord
16/06/2015: Les fonctions de destruction de courbes, polylignes et polygones
            doivent vider le FCourbePolygoneProvisoire
18/06/2015: Bug fixé: Destruction d'une polyligne
22/06/2015: Sélection d'un groupe: Suppression de l'affichage du fond (était bogué et inutile)
26/06/2015: Support des scraps + le dessin se fait groupe après groupe
            Création, modif, suppression de scraps: OK
29/06/2015: Utilisation de TBGRACanvas: OK - /!\ Utiliser TBGRACanvas et non TCanvas
02/07/2015: Remplacement des drapeaux d'affichage par un 'set of'
04/07/2015: Les textes utilisent désormais les hauteurs réelles en mètres
11/07/2015: Accélération du tracé grâce au test de visibilité de la BBX d'un groupe
20/07/2015: Tracé des BoundingBox, corrections
30/07/2015: Tracé du sens des courbes
06/08/2015: Aides au dessin: Mise en évidence des visées pentues, avec le sens.
20/08/2015: La procédure TCadreDessinBGRA.RedessinEcran(const DoGenererImage: boolean),
            qui est très volumineuse, est déportée dans le fichier d'inclusion
            CadreDessinBGRARedessinEcran.inc
31/08/2015: Déport de routines vers TGHCaveDrawDrawingContext situé dans l'unité
            UnitTGHCaveDrawDrawingContext.pas
            (isolation de code afin de permettre la génération d'images)
01/09/2015: Export en image de toute taille OK
24/09/2015: Support des images de type 'fond' OK (images n'appartenant pas à un groupe)
            Les images sont positionnées en coordonnées réelles.
24/09/2015: Les polylignes temporaires sont tracées comme les courbes (couleurs + marqueurs)
28/10/2015: Possibilité de réassigner les basepoints pour les objets
28/10/2015: Pour les objets volatiles: Dessin des basepoints pour scrab, courbe, polygone, polyligne
17/11/2015: Fix de bugs dans SetCurrent*Idx()
18/01/2016: Centralisation de FIdxGroupes et de FIdxStyles*, ces arguments sont supprimés dans
            les fonctions de création et éditions, remplacées par SetIDXGroupe() et SetIdxStyles*()
22/01/2015: Support de l'affichage des détails (tedDETAILS in FElementsDessines)
            Getter et Setter pour FElementsDessines
15/03/2016: Les listes passent aux génériques
16/03/2016: Implantation de DrawVolatileTexte() et DrawVolatileSymbole();
08/04/2016: La gestion des courbes à main levée passe dans unitFreeHandCourbe
03/01/2017: Support du type de courbe/polyligne nocMUR_MACONNE pour les murs maçonnés
16/11/2017: Mise en place de seuils de visibilité pour accélérer le tracé du plan
30/01/2019: Fonctionnalité de mesure de distance

Cette unité possède les dépendances suivantes:
- CadreDessinAfficherDebugVariables.inc
- CadreDessinBGRARedessinEcran.inc

Fonctionnalités ( S/O = Sans objet)
                           Courbes   Polylignes  Lignes    Polygones   Symboles   Textes     Scraps    Images
Création                      X           X         X         X           X         X        X           X
Sélection                     X           X         X         X           X         X        X           .
Modif de sommets              X           X         X         X          S/O       S/O       X           .
Suppr. de sommets             X           X        S/O        X          S/O       S/O       X          S/O
Inversion de chemins          X           X      En attente   S/O         S/O       S/O     S/O         S/O
Edition par dialogue         S/O         S/0       S/O       S/O          X         X        .           .
Chgt rapide de groupes        X           X         X         X           X         X        .          S/O
Chgt rapide de styles         X           X         X         X           X         X       S/O         S/O

// Remplacement des méthodes de picking
// la touche MAJ ne sert qu'à déplacer les sommets des polygones et courbes
Zoom 2 points              : OK
Pan 2 points               : OK
Dessin de lignes           : OK
Choix d'une courbe         : OK - Réarmement manuel non nécessaire
Choix d'un polygone        : OK - Réarmement manuel non nécessaire

Recherche d'uns station    : OK (par ID littéral ou couple Serie.Station)

//**********************************************************************)
// NOTE IMPORTANTE:
// L'utilisation de TBGRABitmap impose l'utilisation de TBGRACanvas
// (TCanvas fonctionne mais est très lent)
//**********************************************************************)
interface
uses
 {$IFDEF LANGUAGE_FRENCH}
   UnitMessages_fr
 {$ENDIF}
  {$IFDEF LINUX}
  , Types
  {$ENDIF}
  , GHCD_Types
  , SysUtils
  , Classes
  , UnitListesSimplesWithGeneriques
  , Graphics
  {$IFDEF MSWINDOWS}
  , windows
  {$ENDIF}

  , BGRABitmap
  , BGRABitmapTypes
  , BGRACanvas
  , BGRAGradients
  , UnitTGHCaveDrawDrawingContext

  , UnitDocDessin
  , unitFreeHandCourbe
  , UnitGraphismesProvisoires
  , Math
  , GeneralFunctions
  , GeometryPolygonesUtils
  , CallDialogsGHCaveDraw
  , Controls
  , Forms
  , Dialogs
  , ExtCtrls
  , StdCtrls
  , Menus
  , ActnList
  , Buttons, PopupNotifier
  ;

type TModeEditPoint = (tepNONE, tepMOV_PT, tepTAN_LEFT, tepTAN_RIGHT);

type
  { TCadreDessinBGRA }
  TCadreDessinBGRA = class(TFrame)
    acCourbeChenalDeVoute: TAction;
    acCourbeEcoulements: TAction;
    acCourbeLignesPentes: TAction;
    acCourbeMiniRessaut: TAction;
    acCourbeMurMaconne: TAction;
    acCourbeNone: TAction;
    acCourbeParoiIncertaine: TAction;
    acCourbeParois: TAction;
    acCourbeParoisCachees: TAction;
    acCourbeRessauts: TAction;
    acCourbeSurplombs: TAction;
    acDeleteObject: TAction;
    acEditCourbe: TAction;
    acEditLigne: TAction;
    acEditObjet: TAction;
    acEditPolygone: TAction;
    acEditPolylign: TAction;
    acEditScrap1: TAction;
    acEditSymbole1: TAction;
    acEditTexte1: TAction;
    acLigneFleche: TAction;
    acLigneFracture: TAction;
    acLigneNone: TAction;
    acLignePentes: TAction;
    acLigneSuiteReseau: TAction;
    acMoveObject: TAction;
    acNewCourbe: TAction;
    acNewFreeHandCurve: TAction;
    acNewImage: TAction;
    acNewLigne: TAction;
    acNewNone: TAction;
    acNewPolygone: TAction;
    acNewPolylign: TAction;
    acNewScrap: TAction;
    acNewSymbole: TAction;
    acNewTexte: TAction;
    acNouvelObjet: TAction;
    acPolygoneArgile: TAction;
    acPolygoneArgileGalets: TAction;
    acPolygoneBlocs: TAction;
    acPolygoneChemins: TAction;
    acPolygoneGalets: TAction;
    acPolygoneGour: TAction;
    acPolygoneGrosBloc: TAction;
    acPolygoneLac: TAction;
    acPolygoneNeige: TAction;
    acPolygoneNone: TAction;
    acPolygoneSable: TAction;
    acPolygoneSilhouettes: TAction;
    acPolygoneSiphon: TAction;
    acSelectionZoom: TAction;
    acDisplayDebugTexts: TAction;
    acDeleteCourbe: TAction;
    acDeleteLigne: TAction;
    acDeletePolygone: TAction;
    acDeleteSymbole: TAction;
    acDeleteTexte: TAction;
    acCourbeAffecterGroupe: TAction;
    acLigneAffecterGroupe: TAction;
    acPolygoneAffecterGroupe: TAction;
    acSetModeTravailNone: TAction;
    acSymboleAffecterGroupe: TAction;
    acSymboleArriveeEau: TAction;
    acSymboleChouxFleurs: TAction;
    acSymboleColonnes: TAction;
    acSymboleCristaux: TAction;
    acSymboleCupule: TAction;
    acSymboleDesob: TAction;
    acSymboleEntree: TAction;
    acSymboleExcentriques: TAction;
    acSymboleFailles: TAction;
    acSymboleFistuleuses: TAction;
    acSymboleLiaisonsGroupes: TAction;
    acSymboleNone: TAction;
    acSymbolePerte: TAction;
    acSymbolePhoto: TAction;
    acSymbolePointFixe: TAction;
    acSymbolePointTopo: TAction;
    acSymboleStalactites: TAction;
    acSymboleStalagmites: TAction;
    acSymboleZeff: TAction;
    acTexteAffecterGroupe: TAction;
    acCourbeAffecterStyle: TAction;
    acLigneAffecterStyle: TAction;
    acPolygoneAffecterStyle: TAction;
    acEmptyAction: TAction;
    acEditSymbole: TAction;
    acEditTexte: TAction;
    acSymboleAffecterStyle: TAction;
    acTexteAffecterStyle: TAction;
    acReverseCourbe: TAction;
    acApplyMetaFiltre: TAction;
    acPolyLineAffecterGroupe: TAction;
    acPolylineAffecterStyle: TAction;
    acDeletePolyline: TAction;
    acReversePolyline: TAction;
    acDisplayIDStations: TAction;
    acClosePolylign: TAction;
    acScrapAffecterGroupe: TAction;
    acDeleteScrap: TAction;
    acEditScrap: TAction;
    acCourbeAffecterCurrGrp: TAction;
    acScrapAffecterCurrGrp: TAction;
    acPolylignAffecterCurrGrp: TAction;
    acPolygonAffecterCurrGrp: TAction;
    acLineAffecterCurrGrp: TAction;
    acSymboleAffecterCurrGrp: TAction;
    acTexteAffecterCurrGrp: TAction;
    acSectionTransversale: TAction;
    acLockCurrentStation: TAction;
    acDispCodeGHCaveDrawObjet: TAction;
    acPrintInfosStation: TAction;
    acSetCurrentGroupeFromThisScrap: TAction;
    acSetCurrentGroupeFromThisCourbe: TAction;
    acSetCurrentGroupeFromThisPolyline: TAction;
    acSetCurrentGroupeFromThisPolygone: TAction;
    acSetCurrentGroupeFromThisSimpleLine: TAction;
    acSetCurrentGroupeFromThisSymbole: TAction;
    acSetCurrentGroupeFromThisTextObject: TAction;
    acDoCreateFichePointTopo: TAction;
    acTexteCotation: TAction;
    acTexteDefault: TAction;
    acTexteLieuDit: TAction;
    acTexteOrdinaire1: TAction;
    acTexteOrdinaire2: TAction;
    acTexteSousTitre: TAction;
    acTexteTitre: TAction;
    acMasquerImages: TAction;
    Action2: TAction;
    acTransformerCourbeEnPolyligne: TAction;
    acVue: TAction;
    btnAffecterAuGroupeCourant: TSpeedButton;
    btnAffecterNatureByCombo1: TSpeedButton;
    btnDispCodeGHCaveDraw: TSpeedButton;
    btnRecupereFiltreDepuisGroupe: TButton;
    btnEditObjectSpecial: TSpeedButton;
    btnFermerCourbePolyligne: TSpeedButton;
    btnPanelDebug: TButton;
    btnMetaFiltre: TButton;
    btnAssignFiltreGrpCourant: TButton;
    Button1: TButton;
		btnEraseFiltre: TButton;
    CdrDrawActionList: TActionList;

    acCancelCourbe: TAction;
    acRedess: TAction;
    acPanVue: TAction;
    acDrawLine: TAction;
    acSetCurrGroupeByObject: TAction;
    acZoomTout: TAction;
    acZoomWindow: TAction;
    acValidateCourbe: TAction;
    chkMetafiltreActif: TCheckBox;
    chkBaseStationLocked: TCheckBox;
    cmbGroupeObjet: TComboBox;
    cmbNatureObjet: TComboBox;

    editFiltre: TEdit;
    CdrDrwImageList: TImageList;
    Label1: TLabel;
    lbMessages: TStaticText;
    lbInfosEntite: TLabel;
    lbCurrentObject: TLabel;

    lbIDBaseStation: TStaticText;
    lbMousePos: TStaticText;
    lbModeTravail: TStaticText;
    lbCurrentGroupe: TStaticText;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    mnuAffecterGroupeFromObject: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem8: TMenuItem;
    mnuAddColorAtFiltre: TMenuItem;
    MenuItem7: TMenuItem;
    mnuAddStationAtFiltre: TMenuItem;
    mnuFILTRES_STATION: TMenuItem;
    MenuItem9: TMenuItem;
    mnuFILTRES_COULEURS: TMenuItem;
    mnuFILTRES_SERIE: TMenuItem;
    mnuFiltres: TMenuItem;
    mnuAddSerieAtFiltre: TMenuItem;
    MenuItem6: TMenuItem;

    mnuCourbe: TPopupMenu;
    pnlFiltres: TPanel;
    pnlTypeObjet: TPanel;

    pnlVue: TPanel;
    PopUpGeneral: TPopupMenu;

    btnDeleteObject: TSpeedButton;
    btnAffecterGroupeByCombo: TSpeedButton;
    btnAffecterNatureByCombo: TSpeedButton;
    btnEditObject: TSpeedButton;
    PopupNotifier1: TPopupNotifier;
    SpeedButton1: TSpeedButton;
    lbColorGroupe: TStaticText;

    Vue: TPaintBox;

    procedure acApplyMetaFiltreExecute(Sender: TObject);
    procedure acClosePolylignExecute(Sender: TObject);
    procedure acDispCodeGHCaveDrawObjetExecute(Sender: TObject);
    procedure acDoCreateFichePointTopoExecute(Sender: TObject);
    procedure acLocaliserGroupeExecute(Sender: TObject);
    procedure acLockCurrentStationExecute(Sender: TObject);
    procedure acMasquerImagesExecute(Sender: TObject);
    procedure acPanVueExecute(Sender: TObject);
    procedure acPrintInfosStationExecute(Sender: TObject);

    procedure acScrapAffecterCurrGrpExecute(Sender: TObject);
    procedure acCourbeAffecterCurrGrpExecute(Sender: TObject);
    procedure acPolylignAffecterCurrGrpExecute(Sender: TObject);
    procedure acPolygonAffecterCurrGrpExecute(Sender: TObject);
    procedure acSectionTransversaleExecute(Sender: TObject);
    procedure acSetCurrentGroupeFromThisCourbeExecute(Sender: TObject);
    procedure acSetCurrentGroupeFromThisPolygoneExecute(Sender: TObject);
    procedure acSetCurrentGroupeFromThisPolylineExecute(Sender: TObject);
    procedure acSetCurrentGroupeFromThisScrapExecute(Sender: TObject);
    procedure acSetCurrentGroupeFromThisSimpleLineExecute(Sender: TObject);
    procedure acSetCurrentGroupeFromThisSymboleExecute(Sender: TObject);
    procedure acSetCurrentGroupeFromThisTextObjectExecute(Sender: TObject);
    procedure acSymboleAffecterCurrGrpExecute(Sender: TObject);
    procedure acLineAffecterCurrGrpExecute(Sender: TObject);
    procedure acTexteAffecterCurrGrpExecute(Sender: TObject);

    procedure acScrapAffecterGroupeExecute(Sender: TObject);
    procedure acCourbeAffecterGroupeExecute(Sender: TObject);
    procedure acPolygoneAffecterGroupeExecute(Sender: TObject);
    procedure acPolyLineAffecterGroupeExecute(Sender: TObject);
    procedure acLigneAffecterGroupeExecute(Sender: TObject);
    procedure acSymboleAffecterGroupeExecute(Sender: TObject);
    procedure acTexteAffecterGroupeExecute(Sender: TObject);


    procedure acDeleteScrapExecute(Sender: TObject);

    procedure acDisplayIDStationsExecute(Sender: TObject);
    procedure acEditScrapExecute(Sender: TObject);

    procedure acLigneAffecterStyleExecute(Sender: TObject);

    procedure acEmptyActionExecute(Sender: TObject);

    procedure acPolylineAffecterStyleExecute(Sender: TObject);
    procedure acReverseCourbeExecute(Sender: TObject);
    procedure acReversePolylineExecute(Sender: TObject);


    procedure acSetCurrGroupeByObjectExecute(Sender: TObject);


    procedure acSymboleAffecterStyleExecute(Sender: TObject);


    procedure acCourbeAffecterStyleExecute(Sender: TObject);
    procedure acPolygoneAffecterStyleExecute(Sender: TObject);

    procedure acDeleteCourbeExecute(Sender: TObject);
    procedure acDeleteLigneExecute(Sender: TObject);
    procedure acDeletePolygoneExecute(Sender: TObject);
    procedure acDeleteSymboleExecute(Sender: TObject);
    procedure acDeleteTexteExecute(Sender: TObject);
    procedure acDeletePolylineExecute(Sender: TObject);


    procedure acEditSymboleExecute(Sender: TObject);
    procedure acEditTexteExecute(Sender: TObject);

    procedure acTexteAffecterStyleExecute(Sender: TObject);
    procedure acTransformerCourbeEnPolyligneExecute(Sender: TObject);
    procedure acZoomWindowExecute(Sender: TObject);

    procedure AddZoomParameters(const Z: TZoomParameters);   // ajouter un zoom
    procedure btnAssignFiltreGrpCourantClick(Sender: TObject);
		procedure btnEraseFiltreClick(Sender: TObject);
    procedure btnRecupereFiltreDepuisGroupeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure chkBaseStationLockedChange(Sender: TObject);
    procedure chkMetafiltreActifChange(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure mnuAddSerieAtFiltreClick(Sender: TObject);
    procedure mnuAddColorAtFiltreClick(Sender: TObject);
    procedure mnuAddStationAtFiltreClick(Sender: TObject);
    procedure mnuCancelClick(Sender: TObject);
    procedure mnuFILTRES_COULEURSClick(Sender: TObject);
    procedure mnuFILTRES_SERIEClick(Sender: TObject);
    procedure mnuFILTRES_STATIONClick(Sender: TObject);
    procedure pnlTypeObjetClick(Sender: TObject);
    procedure PopUpGeneralPopup(Sender: TObject);
    procedure PopupNotifier1Close(Sender: TObject; var CloseAction: TCloseAction);
    procedure TraceMarqueurDirectionnel(const Cnv: TCanvas; const XX, YY: double; const Angle: double);
    //*********************************************

    // conversions de coordonnées
    function  GetCoordsMonde(const PP: TPoint): TPoint2Df;
    function  GetCoordsPlan (const PM: TPoint2Df): TPoint; overload;
    function  GetCoordsPlan (const QX, QY: double): TPoint; overload;

    // zooms et limites de vue
    procedure SetViewLimits(const X1, Y1, X2, Y2: double);
    procedure ResetViewLimits();       // zoom tout
    function  GetRYMaxi(): double;     // attraper la hauteur de vue
    procedure DeplacerVue(const DepX, DepY: integer);


    // ajout d'objets
    procedure AjouterUnScrap();            // ajout d'un scrap
    procedure AjouterUneCourbe();          // ajout d'une entité courbe
    procedure AjouterUnePolyligne();       // ajout d'une entité polyligne
    procedure AjouterUnPolygone();         // ajout d'une entité polygone
    // redessin écran
    procedure RedessinEcran(const DoGenererImage: boolean);
    // édition et dessin des courbes
    // dessin de la courbe provisoire
    procedure DessinerCourbeProvisoire(const Cnv: TCanvas);

    // définit le métafiltre
    procedure SetMetaFiltre(const MF: string);
    function  GetMetaFiltre(): string;
    // procs événementielles
    procedure VueClick(Sender: TObject);
    procedure VueDblClick(Sender: TObject);
    procedure VueMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure VuePaint(Sender: TObject);
    procedure VueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnEditPolyClick(Sender: TObject);
    procedure editFiltreKeyPress(Sender: TObject; var Key: Char);
    procedure acValidateCourbeExecute(Sender: TObject);
    procedure acCancelCourbeExecute(Sender: TObject);
    procedure acRedessExecute(Sender: TObject);
    procedure acZoomToutExecute(Sender: TObject);
    procedure acDeleteLastObjectExecute(Sender: TObject);
    procedure Couleurcourante1Click(Sender: TObject);
    procedure Sriecourante1Click(Sender: TObject);
    procedure acDisplayDebugTextsExecute(Sender: TObject);
    //**********************************************************
    // Objets volatiles
    procedure DrawVolatileScrap(const Cnv: TCanvas;
                                const MyScrap: TScrap;
                                const IdxScrap: Int64;
                                const DoDrawSommets: boolean;
                                const DoDrawBasePoints: boolean;
                                const QIdxGroupe: integer);
    procedure DrawVolatileCourbe(const Cnv: TCanvas;
                                 const MyCourbe: TCourbe;
                                 const IdxCourbe: Int64;
                                 const DoDrawPtsCtrls: boolean;
                                 const DoDrawBasePoints: boolean);
    procedure DrawVolatilePolygone(const Cnv: TCanvas;
                           const MyPolygon: TPolygone;
                           const IdxPolygone: Int64;
                           const DoDrawSommets: boolean;
                           const DoDrawBasePoints: boolean;
                           const QIdxGroupe: integer);
    procedure DrawVolatilePolyligne(const Cnv: TCanvas;
                            const MyPolyLine: TPolyLigne;
                            const IdxPolygone: Int64;
                            const DoDrawSommets: boolean;
                            const DoDrawBasePoints: boolean;
                            const QIdxGroupe: integer);
    procedure DrawVolatileSimpleLigne(const Cnv: TCanvas;
                              const SL: TSimpleLigne;
                              const Index: integer;
                              const DoDrawHands: boolean;
                              const DoDrawBasePoints: boolean);
    procedure DrawVolatileSymbole(const Cnv: TCanvas;
                                  const SL: TSymbole;
                                  const Index: integer;
                                  const DoDrawHands: boolean;
                                  const DoDrawBasePoints: boolean);
    procedure DrawVolatileTexte(const Cnv: TCanvas;
                                const TT: TTextObject;
                                const Index: integer;
                                const DoDrawHands: boolean;
                                const DoDrawBasePoints: boolean);

    Procedure VolatileDrawRectangle(const Cnv: TCanvas;
                                    const C1, C2: TPoint2Df);

    procedure VolatileDrawTriangle(const Cnv: TCanvas;
                                   const P1, P2, P3: TPoint2Df);


    procedure VolatileTraceTexte(const Cnv: TCanvas; const XX, YY: Double; const T: string);
    procedure VolatileTraceVers(const Cnv: TCanvas; const XX, YY: Double; const Drawn: boolean);
    procedure VolatileTraceCercle(const Cnv: TCanvas; const XX, YY: Double; const R: integer);
  private
    FEditableDrawing     : boolean;
    FParamsVue2D         : TParamsVue2D;
    // positions courantes de la souris
    FMyPos: TPoint2Df;
    FPP: TPoint;
    // modes de sélection d'entités
    FModeSelectionEntites: TModeSelectionEntites;
    // modes de travail de la zone de dessin
    FModeTravailCanvas: TModeTravailCanvas;
    // sémaphore pour le dessin
    FRedessinInProcess: boolean;
    // nom de fichier image pour le dessin
    FFichierImage : string;
    // document de dessin
    FDocumentDessin   : TDocumentDessin;


    // limites du dessin
    FRXMini      : double;
    FRXMaxi      : double;
    FRYMini      : double;
    FRYMaxi      : double;
    // étendue du dessin
    FEtendueX    : double;
    FEtendueY    : double;
    FEtendueXY   : double;

    // variables diverses pour zoom
    FZC1, FZC2     : TPoint2Df;
    FZP1, FZP2     : TPoint;
    FRappHLVue   : double;
    FRappScrReal : double;
    FInvRappScrReal: double;
    // point topo de base
    FCurrentBasePoint: TBaseStation;
    FOffsetPoint0: TPoint2Df;
    FOffsetPoint1: TPoint2Df;

    // dernier point topo trouvé
    FLastStationFound : TBaseStation;
    // variables pour l'élément en cours d'édition
    FIDGroupeEditing          : TIDGroupeEntites;
    FIDStyleCurveEditing      : TNatureObjetCourbe;
    FIDStylePolygoneEditing   : TNatureObjetPolygone;


    // drapeaux  généraux
    FDoDraw                   : boolean; // prêt pour l'édition (eg: dessin correctement chargé) ?
    FDoLockCurrentStation     : boolean; // verrouiller le point de référence courant ?
    // compteur d'avertissement: on verrouille un basepoint pour quelques vertex seulement
    FNbSommetsAjoutesAvecPtLocked: integer; // compteur
    FAttenduSecondPoint       : boolean; // zoom, pan et ligne
    FUnObjetEstSelectionne    : boolean; // un objet est sélectionné
    FVertexModified           : boolean; // Un vertex est modifié ?
    FDoAddVertexCourbePolygone: boolean; // Est-on en mode Ajout de sommet ?

    FDoMovePoint              : boolean; // peut-on déplacer un sommet ?
    FMovingPoint              : boolean; // mode déplacement sommet est actif ?

    FDoFreeHand               : boolean; // mode Main Levée activé



    // type d'objet trouvé
    FTypeObjetTrouve          : TRetourTypeObjet;
    // index de natures d'objets
    FCurrentNatureCourbe     : TNatureObjetCourbe;
    FCurrentNaturePolygone   : TNatureObjetPolygone;
    FCurrentNatureLigne      : TNatureObjetLigne;
    FCurrentNatureSymbole    : TNatureObjetSymbole;
    FCurrentNatureTexte      : TNatureObjetTexte;

    // index d'entités courants
    FCurrentScrapIdx      : integer;
    FCurrentCurveIdx      : integer;
    FCurrentPolylineIdx   : integer;
    FCurrentPolygonIdx    : integer;
    FCurrentSimpleLineIdx : integer;
    FCurrentSymboleIdx    : integer;
    FCurrentTextIdx       : integer;
    FCurrentImageIdx      : integer;
    // actualisation de l'onglet
    FProcActualiserOnglet: TProcActualiserOnglet;
    // fonctions de rappel
    FProcGetInfoBasePoint: TProcGetInfoBasePoint;
    FProcGetScrap        : TProcGetScrap;
    FProcGetCourbe       : TProcGetCourbe;
    FProcGetPolyLigne    : TProcGetPolyLigne;
    FProcGetPolygon      : TProcGetPolygon;
    FProcGetSimpleline   : TProcGetSimpleline;
    FProcGetTextObject   : TProcGetTextObject;
    FProcGetPonctObject  : TProcGetPonctObject;

    FProcRefreshImages   : TProcRefreshImages;
    // courbe à main levée
    FCourbeFreehand      : TFreeHandCourbe;
    // stockage des entités en cours d'édition
    FCourbePolygoneProvisoire : TCourbePolygoneProvisoire;
    FNoVertex         : integer; // pour points de contrôle
    FNoPtCtrl         : integer;
    FCurrVtxCurveProv : TVertexCourbe;
    FModeEditPoint    : TModeEditPoint;
    FPoignee          : byte;

    // liste des points obtenus par tracé à main levée
    FListePtsFreeHand : TListeSimple<TPoint2Df>;


    // Coordonnées d'extrémités de lignes en valeur absolue
    FAbsFirstPoint,
    FAbsSecondpoint: TPoint2Df;

    // points pour opérations de zoom, pan et tracé de ligne
    //FLastPos: TPoint;
    FFirstPoint,
    FSecondPoint: TPoint2Df;

    FCurrentGroupeIdx       : TIDGroupeEntites;
    FCurrentSimpleLine      : TSimpleLigne;     // entité ligne provisoire
    FCurrentPonctObject     : TSymbole;         // entité object ponctuel provisoire

    // Bitmaps de remplissage
    FTexturesPolygones      : TTexturesPolygonObject;





    procedure AffecterActionABouton(const btn: TSpeedButton; const Actn: TAction);
    function BasePointIsVisible(const BP: TBaseStation): boolean;
    function CalcFontHeightFromHauteurEnMetres(const Hauteur: double): integer;


    function QRechercheEtTraceLigne(out QIdx: integer): boolean;
    function QRechercheEtTraceCourbe(out QIdx: integer): boolean;
    function QRechercheEtTracePolyligne(out QIdx: integer): boolean;
    function QRechercheEtTracePolygone(out QIdx: integer): boolean;
    function QRechercheEtTraceScrap(out QIdx: integer): boolean;

    procedure RafraichirVue;
    // le groupe est dans la vue courante ?
    function GroupeIsInView(const QGroupe: TGroupeEntites): boolean;
    procedure SetLabelsGroupe(const GP: TGroupeEntites);

    procedure VolatileDrawBasePoint(const Cnv: TCanvas; const BP: TBaseStation);

  public
    { Déclarations publiques }
    procedure RefreshVue();
    function  PreparerCtxt(const FP: TParamsVue2D): boolean;
    // récupérer pointeur sur doc dessin
    function GetDocDessin(): TDocumentDessin;
    // affichage de messages
    procedure SetMsgInLbMessage(const ErrCode: TErrorCodeGeneric; const S: string);
    // paramètres vue 2D
    procedure SetParametresVue2D(const PG: TParamsVue2D);
    function  GetParametresVue2D(): TParamsVue2D;



    // initialisation  et finalisation
    function  CdrDrwInitialize(): boolean;
    function  CdrDrwFinalize()  : boolean;
    // attraper les limites du dessin
    function GetDrwCoinBasGauche(): TPoint2Df;
    function GetDrwCoinHautDroit(): TPoint2Df;

    // charger et initialiser fichier dessin
    function  ChargerInitialiserDessin(const FichierDessin: TStringDirectoryFileName; const Editable: boolean): boolean;
    // actualiser la comboliste des groupes
    procedure ActualiserCmbListeDesGroupes();
    // modes de travail
    procedure SetModeTravail(const MT: TModeTravailCanvas);
    function  GetModeTravail(): TModeTravailCanvas;

    function  GetCurrentIdxObjectForModeSelection(): integer;

    procedure SetCurrentBaseStation(const BS: TBaseStation; const STLock: boolean);
    // groupes
    procedure SetCurrentGroupeByIdx(const Idx: integer);
    procedure SetCurrentGroupeByIDGroupe(const IDGrp: TIDGroupeEntites);

    function  GetCurrentGroupeIdx(): TIDGroupeEntites;
    // textures
    function GetTexturesPolygones(): TTexturesPolygonObject;

    // le dessin est éditable ?
    property DrawingEditable: boolean read FEditableDrawing;

    // paramètres de vue
    procedure SetGrdSpcGrid(const QdrSpcMain, QdrSpcSec: double);
    property MyDocumentDessin : TDocumentDessin     read FDocumentDessin
                                                    write FDocumentDessin;
    // fonctions de rappel
    property MyProcActualiserOnglet: TProcActualiserOnglet read FProcActualiserOnglet write FProcActualiserOnglet;
    property MyProcGetInfoBasePoint: TProcGetInfoBasePoint read  FProcGetInfoBasePoint write FProcGetInfoBasePoint;
    property MyProcGetScrap        : TProcGetScrap read FProcGetScrap write FProcGetScrap;
    property MyProcGetCourbe       : TProcGetCourbe read FProcGetCourbe write FProcGetCourbe;
    property MyProcGetPolyligne    : TProcGetPolyLigne read FProcGetPolyLigne write FProcGetPolyLigne;
    property MyProcGetPolygon      : TProcGetPolygon read FProcGetPolygon write FProcGetPolygon;
    property MyProcGetSimpleline   : TProcGetSimpleline read FProcGetSimpleline write FProcGetSimpleline;
    property MyProcGetTextObject   : TProcGetTextObject read FProcGetTextObject write FProcGetTextObject;
    property MyProcGetPonctObject  : TProcGetPonctObject read FProcGetPonctObject write FProcGetPonctObject;

    property MyProcRefreshImages   : TProcRefreshImages read FProcRefreshImages write FProcRefreshImages;
    // dessin valide ?
    property DoDraw: boolean read FDoDraw write FDoDraw;
    // index de styles
    procedure SetCurrentNatureCourbe(const WU: TNatureObjetCourbe);
    procedure SetCurrentNaturePolyline(const WU: TNatureObjetCourbe);

    procedure SetCurrentNaturePolygone(const WU: TNatureObjetPolygone);
    procedure SetCurrentNatureLigne(const WU: TNatureObjetLigne);
    procedure SetCurrentNatureSymbole(const WU: TNatureObjetSymbole);
    procedure SetCurrentNatureTexte(const WU: TNatureObjetTexte);
    procedure SetCurrentCourbeIdx(const QIdx: integer);
    procedure SetCurrentPolylineIdx(const QIdx: integer);
    procedure SetCurrentLigneIdx(const QIdx: integer);
    procedure SetCurrentPolygoneIdx(const QIdx: integer);
    procedure SetCurrentSymboleIdx(const QIdx: integer);
    procedure SetCurrentTexteIdx(const QIdx: integer);
    procedure SetCurrentScrapIdx(const QIdx: integer);

    // recherche d'une station
    procedure RechercherStation(const Cle: string);
    // armement des drapeaux de dessin
    procedure SetFlagDrawEchelle(const Q: boolean; const TailleEchelle: double);
    procedure SetFlagDrawTexteDebug(const Q: boolean);
    procedure SetFlagDrawCenterLine(const Q: Boolean);
    procedure SetFlagDrawIDStations(const Q: Boolean);
    procedure SetFlagDrawQuadrilles(const Q: Boolean);
    procedure SetFlagDrawTextObjects(const Q: Boolean);
    procedure SetFlagDrawCadresPhotos(const Q: boolean);
    procedure SetFlagDrawScraps(const Q: boolean);
    procedure SetFlagDrawBoundingBoxGroupes(const Q: boolean);
    procedure SetFlagDrawPentes(const Q: boolean);
    procedure SetFlagDrawImages(const Q: boolean);
    procedure SetFlagScrapsMonochromes(const Q: boolean);

    procedure SetColorDefaultScrapsMonochromes(const C: TColor);
    procedure SetTypeQuadrillage(const Q: TTypeQuadrillage);

    procedure SetTailleEchelle(const T: double);
    procedure SetLimiteDispPente(const T: double);


    // zoom et pan
    procedure SetWindowView(const Fact: double; const OffFactorX, OffFactorY: double);

    // enregistrer le contenu de la vue comme image
    procedure SaveVueAsImage(const FichierImage: string);
    // effacer le dernier objet
    procedure DeleteLastObject(const M: TModeSelectionEntites);
    // exporter en image
    function  ExporterTopoEnImage(const QFileName: string;
                                  const LimitesDessin: TRect2Df;
                                  const ImgWidth: integer): boolean;
    // attraper les Idx d'objets
    function GetCurrScrapIdx(): integer;
    function GetCurrCourbeIdx(): integer;
    function GetCurrPolylineIdx(): integer;
    function GetCurrPolygoneIdx(): integer;
    function GetCurrSimpleLineIdx(): integer;
    function GetCurrSymboleIdx(): integer;
    function GetCurrTextIdx(): integer;
    function GetCurrImgIdx(): integer;

    function GetModeSelectionEntites(): TModeSelectionEntites;


  end;

implementation
uses DGCDummyUnit;
// seuil de visibilité des graphismes, exprimé en mètres
// Si la longueur de la diagonale (FEtendueXY) de la zone de tracé est inférieure
// à ces seuils, les objets correspondants sont dessinés.
const SEUIL_VISIBILITE_COURBES_POLYLIGNES = 750.00;
const SEUIL_VISIBILITE_SIMPLES_LIGNES     = 300.00;
const SEUIL_VISIBILITE_POLYGONES          = 500.00;
const SEUIL_VISIBILITE_SYMBOLES           = 500.00;
const SEUIL_VISIBILITE_TEXTES             = 500.00;

const TAB = #9;
const FMT_LIST_ZONES = '%d' + TAB + '%s' + TAB + '%.2f' + TAB +  '%.2f' + TAB + '%.2f' + TAB + '%.2f';

{$R *.lfm}

//------------------------------------------------------------------------------

procedure TCadreDessinBGRA.AffecterActionABouton(const btn: TSpeedButton; const Actn: TAction);
begin
  btn.Glyph  := nil;
  btn.Action := Actn;
end;
procedure TCadreDessinBGRA.chkBaseStationLockedChange(Sender: TObject);
begin
  FDoLockCurrentStation         := chkBaseStationLocked.Checked;
  FNbSommetsAjoutesAvecPtLocked := 0;
end;

procedure TCadreDessinBGRA.chkMetafiltreActifChange(Sender: TObject);
begin
  SetMetaFiltre(trim(editFiltre.Text));
end;

procedure TCadreDessinBGRA.MenuItem19Click(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------
// affectation de groupes aux objets
procedure TCadreDessinBGRA.acScrapAffecterCurrGrpExecute(Sender: TObject);
var
  EWE: TScrap;
begin
  EWE := FDocumentDessin.GetScrap(FCurrentScrapIdx);
  EWE.IDGroupe := FCurrentGroupeIdx;
  FDocumentDessin.PutScrap(FCurrentScrapIdx, EWE);
  SetModeTravail(mtSELECT_SCRAP);
  // contrôle
  EWE := FDocumentDessin.GetScrap(FCurrentScrapIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.acCourbeAffecterCurrGrpExecute(Sender: TObject);
var
  EWE: TCourbe;
begin
  EWE := FDocumentDessin.GetCourbe(FCurrentCurveIdx);
  EWE.IDGroupe := FCurrentGroupeIdx;
  FDocumentDessin.PutCourbe(FCurrentCurveIdx, EWE);
  SetModeTravail(mtSELECT_COURBE);
  // contrôle
  EWE := FDocumentDessin.GetCourbe(FCurrentCurveIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;
procedure TCadreDessinBGRA.acPolylignAffecterCurrGrpExecute(Sender: TObject);
var
  EWE: TPolyLigne;
begin
  EWE := FDocumentDessin.GetPolyligne(FCurrentPolylineIdx);
  EWE.IDGroupe := FCurrentGroupeIdx;
  FDocumentDessin.PutPolyligne(FCurrentPolylineIdx, EWE);
  SetModeTravail(mtSELECT_POLYLIGNE);
  // contrôle
  EWE := FDocumentDessin.GetPolyligne(FCurrentPolylineIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;
procedure TCadreDessinBGRA.acLineAffecterCurrGrpExecute(Sender: TObject);
var
  EWE: TSimpleLigne;
begin
  EWE := FDocumentDessin.GetSimpleLigne(FCurrentSimpleLineIdx);
  EWE.IDGroupe := FCurrentGroupeIdx;
  FDocumentDessin.PutSimpleLigne(FCurrentSimpleLineIdx, EWE);
  SetModeTravail(mtSELECT_LIGNE);
  // contrôle
  EWE := FDocumentDessin.GetSimpleLigne(FCurrentSimpleLineIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.acLockCurrentStationExecute(Sender: TObject);
begin
  FDoLockCurrentStation := Not(FDoLockCurrentStation);
  if (FDoLockCurrentStation) then FNbSommetsAjoutesAvecPtLocked := 0;
  chkBaseStationLocked.Checked := FDoLockCurrentStation;
  lbIDBaseStation.Color := IIF(FDoLockCurrentStation, clRed, $008EF2FD);
end;

procedure TCadreDessinBGRA.acMasquerImagesExecute(Sender: TObject);
var
  EWE: TParamsVue2D;
begin
  EWE := GetParametresVue2D();
  if (tedIMAGES in EWE.ElementsDrawn) then EWE.ElementsDrawn := EWE.ElementsDrawn - [tedIMAGES]
                                      else EWE.ElementsDrawn := EWE.ElementsDrawn + [tedIMAGES];
  SetParametresVue2D(EWE);
end;

procedure TCadreDessinBGRA.acTexteAffecterCurrGrpExecute(Sender: TObject);
var
  EWE: TTextObject;
begin
  EWE := FDocumentDessin.GetTexte(FCurrentTextIdx);
  EWE.IDGroupe := FCurrentGroupeIdx;
  FDocumentDessin.PutTexte(FCurrentTextIdx, EWE);
  SetModeTravail(mtSELECT_TEXTE);
  // contrôle
  EWE := FDocumentDessin.GetTexte(FCurrentTextIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;
procedure TCadreDessinBGRA.acPolygonAffecterCurrGrpExecute(Sender: TObject);
var
  EWE: TPolygone;
begin
  EWE := FDocumentDessin.GetPolygone(FCurrentPolygonIdx);
  EWE.IDGroupe := FCurrentGroupeIdx;
  FDocumentDessin.PutPolygone(FCurrentPolygonIdx, EWE);
  SetModeTravail(mtSELECT_POLYGONE);
  // contrôle
  EWE := FDocumentDessin.GetPolygone(FCurrentPolygonIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;
procedure TCadreDessinBGRA.acSymboleAffecterCurrGrpExecute(Sender: TObject);
var
  EWE: TSymbole;
begin
  EWE := FDocumentDessin.GetSymbole(FCurrentSymboleIdx);
  EWE.IDGroupe := FCurrentGroupeIdx;
  FDocumentDessin.PutSymbole(FCurrentSymboleIdx, EWE);
  SetModeTravail(mtSELECT_SYMBOLE);
  // contrôle
  EWE := FDocumentDessin.GetSymbole(FCurrentSymboleIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;
//******************************************************************************

procedure TCadreDessinBGRA.acScrapAffecterGroupeExecute(Sender: TObject);
var
  WU: TGroupeEntites;
  EWE: TScrap;
begin
  EWE := FDocumentDessin.GetScrap(FCurrentScrapIdx);
  EWE.IDGroupe := FDocumentDessin.GetIndexGroupeByInternalIdx(cmbGroupeObjet.ItemIndex);
  FDocumentDessin.PutScrap(FCurrentScrapIdx, EWE);
  // contrôle
  EWE := FDocumentDessin.GetScrap(FCurrentScrapIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.acSectionTransversaleExecute(Sender: TObject);
begin
  EditerSectionTransversale(FDocumentDessin, FCurrentBasePoint, 1.234, 5.678);

end;
// définir le groupe courant d'après le dernier objet sélectionné
procedure TCadreDessinBGRA.acSetCurrentGroupeFromThisScrapExecute(Sender: TObject);
var
  EWE: TScrap;
begin
  EWE := FDocumentDessin.GetScrap(FCurrentScrapIdx);
  ShowMessageFmt('FCurrentScrapIdx: %d - Groupe: %d', [FCurrentScrapIdx, EWE.IDGroupe]);
  self.SetCurrentGroupeByIDGroupe(EWE.IDGroupe);
end;

procedure TCadreDessinBGRA.acSetCurrentGroupeFromThisCourbeExecute(Sender: TObject);
var
  EWE: TCourbe;
begin
  EWE := FDocumentDessin.GetCourbe(FCurrentCurveIdx);
  ShowMessageFmt('FCurrentCourbeIdx: %d - Groupe: %d', [FCurrentCurveIdx, EWE.IDGroupe]);
  self.SetCurrentGroupeByIDGroupe(EWE.IDGroupe);
end;

procedure TCadreDessinBGRA.acSetCurrentGroupeFromThisPolylineExecute(Sender: TObject);
var
  EWE: TPolyLigne;
begin
  EWE := FDocumentDessin.GetPolyligne(FCurrentPolylineIdx);
  ShowMessageFmt('FCurrentPolylineIdx: %d - Groupe: %d', [FCurrentPolylineIdx, EWE.IDGroupe]);
  self.SetCurrentGroupeByIDGroupe(EWE.IDGroupe);
end;

procedure TCadreDessinBGRA.acSetCurrentGroupeFromThisPolygoneExecute(Sender: TObject);
var
  EWE: TPolygone;
begin
  EWE := FDocumentDessin.GetPolygone(FCurrentPolygonIdx);
  ShowMessageFmt('FCurrentPolygonIdx: %d - Groupe: %d', [FCurrentPolygonIdx, EWE.IDGroupe]);
  self.SetCurrentGroupeByIDGroupe(EWE.IDGroupe);
end;
procedure TCadreDessinBGRA.acSetCurrentGroupeFromThisSimpleLineExecute(Sender: TObject);
var
  EWE: TSimpleLigne;
begin
  EWE := FDocumentDessin.GetSimpleLigne(FCurrentSimpleLineIdx);
  ShowMessageFmt('FCurrentSimpleLineIdx: %d - Groupe: %d', [FCurrentSimpleLineIdx, EWE.IDGroupe]);
  self.SetCurrentGroupeByIDGroupe(EWE.IDGroupe);
end;
procedure TCadreDessinBGRA.acSetCurrentGroupeFromThisSymboleExecute(Sender: TObject);
var
  EWE: TSymbole;
begin
  EWE := FDocumentDessin.GetSymbole(FCurrentSymboleIdx);
  ShowMessageFmt('FCurrentSymboleIdx: %d - Groupe: %d', [FCurrentSymboleIdx, EWE.IDGroupe]);
  self.SetCurrentGroupeByIDGroupe(EWE.IDGroupe);
end;
procedure TCadreDessinBGRA.acSetCurrentGroupeFromThisTextObjectExecute(Sender: TObject);
var
  EWE: TTextObject;
begin
  EWE := FDocumentDessin.GetTexte(FCurrentTextIdx);
  ShowMessageFmt('FCurrentTextIdx: %d - Groupe: %d', [FCurrentTextIdx, EWE.IDGroupe]);
  self.SetCurrentGroupeByIDGroupe(EWE.IDGroupe);
end;

//******************************************************************************
procedure TCadreDessinBGRA.acCourbeAffecterGroupeExecute(Sender: TObject);
var
  EWE: TCourbe;
begin
  EWE := FDocumentDessin.GetCourbe(FCurrentCurveIdx);
  EWE.IDGroupe := FDocumentDessin.GetIndexGroupeByInternalIdx(cmbGroupeObjet.ItemIndex);
  FDocumentDessin.PutCourbe(FCurrentCurveIdx, EWE);
  // contrôle
  EWE := FDocumentDessin.GetCourbe(FCurrentCurveIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;
procedure TCadreDessinBGRA.acPolygoneAffecterGroupeExecute(Sender: TObject);
var
  EWE: TPolygone;
begin
  EWE := FDocumentDessin.GetPolygone(FCurrentPolygonIdx);
  EWE.IDGroupe := FDocumentDessin.GetIndexGroupeByInternalIdx(cmbGroupeObjet.ItemIndex);
  FDocumentDessin.PutPolygone(FCurrentPolygonIdx, EWE);
  SetModeTravail(mtSELECT_POLYGONE);
  // contrôle
  EWE := FDocumentDessin.GetPolygone(FCurrentPolygonIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;
procedure TCadreDessinBGRA.acPolyLineAffecterGroupeExecute(Sender: TObject);
var
  EWE: TPolyLigne;
begin
  EWE := FDocumentDessin.GetPolyligne(FCurrentPolylineIdx);
  EWE.IDGroupe := FDocumentDessin.GetIndexGroupeByInternalIdx(cmbGroupeObjet.ItemIndex);
  FDocumentDessin.PutPolyligne(FCurrentPolylineIdx, EWE);
  SetModeTravail(mtSELECT_POLYLIGNE);
  // contrôle
  EWE := FDocumentDessin.GetPolyligne(FCurrentPolylineIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;
procedure TCadreDessinBGRA.acLigneAffecterGroupeExecute(Sender: TObject);
var
  EWE: TSimpleLigne;
begin
  EWE := FDocumentDessin.GetSimpleLigne(FCurrentSimpleLineIdx);
  EWE.IDGroupe := FDocumentDessin.GetIndexGroupeByInternalIdx(cmbGroupeObjet.ItemIndex);
  FDocumentDessin.PutSimpleLigne(FCurrentSimpleLineIdx, EWE);
  SetModeTravail(mtSELECT_LIGNE);
  // contrôle
  EWE := FDocumentDessin.GetSimpleLigne(FCurrentSimpleLineIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;


procedure TCadreDessinBGRA.acSymboleAffecterGroupeExecute(Sender: TObject);
var
  EWE: TSymbole;
begin
  EWE := FDocumentDessin.GetSymbole(FCurrentSymboleIdx);
  EWE.IDGroupe := FDocumentDessin.GetIndexGroupeByInternalIdx(cmbGroupeObjet.ItemIndex);
  FDocumentDessin.PutSymbole(FCurrentSymboleIdx, EWE);
  SetModeTravail(mtSELECT_SYMBOLE);
  // contrôle
  EWE := FDocumentDessin.GetSymbole(FCurrentSymboleIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.acTexteAffecterGroupeExecute(Sender: TObject);
var
  EWE: TTextObject;
begin
  EWE := FDocumentDessin.GetTexte(FCurrentTextIdx);
  EWE.IDGroupe := FDocumentDessin.GetIndexGroupeByInternalIdx(cmbGroupeObjet.ItemIndex);
  FDocumentDessin.PutTexte(FCurrentTextIdx, EWE);
  SetModeTravail(mtSELECT_TEXTE);
  // contrôle
  EWE := FDocumentDessin.GetTexte(FCurrentTextIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(EWE.IDGroupe);
  Vue.Invalidate; // RedessinEcran(false);
end;


//------------------------------------------------------------------------------

procedure TCadreDessinBGRA.acDeletePolylineExecute(Sender: TObject);
begin
  if (FCurrentPolylineIdx >= 0) then
  begin
    if (QuestionOuiNon(format(rsMSG_DELETE_POLYLIGNE, [FCurrentPolylineIdx]))) then
    begin
      FDocumentDessin.DeletePolyligne(FCurrentPolylineIdx);
      FCourbePolygoneProvisoire.ClearVertex;
      SetCurrentPolylineIdx(0);
      Vue.Invalidate; // RedessinEcran(false);
    end;
  end;
end;

procedure TCadreDessinBGRA.acDeleteScrapExecute(Sender: TObject);
begin
  if (FCurrentScrapIdx >= 0) then
  begin
    if (QuestionOuiNon(format(rsMSG_DELETE_SCRAP, [FCurrentScrapIdx]))) then
    begin
      FDocumentDessin.DeleteScrap(FCurrentScrapIdx);
      FCourbePolygoneProvisoire.ClearVertex;
      SetCurrentScrapIdx(0);
      Vue.Invalidate; // RedessinEcran(false);
    end;
  end;
end;

procedure TCadreDessinBGRA.acDisplayIDStationsExecute(Sender: TObject);
begin
  if (tedIDSTATIONS in FParamsVue2D.ElementsDrawn) then FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedIDSTATIONS] else
                                                        FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedIDSTATIONS];
  acDisplayIDStations.Checked := tedIDSTATIONS in FParamsVue2D.ElementsDrawn;

  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.acDoCreateFichePointTopoExecute(Sender: TObject);
begin
  CreateFichePointTopo(FDocumentDessin,
                       FTexturesPolygones,
                       FCurrentBasePoint,
                       FParamsVue2D);
end;

procedure TCadreDessinBGRA.acLocaliserGroupeExecute(Sender: TObject);
begin

end;

procedure TCadreDessinBGRA.acEditScrapExecute(Sender: TObject);
var
  SC: TScrap;
begin
  SC := FDocumentDessin.GetScrap(FCurrentScrapIdx);
  if (EditerScrap(FDocumentDessin, SC)) then
  begin
    FDocumentDessin.PutScrap(FCurrentScrapIdx, SC);
  end;
  SetModeTravail(mtNONE);
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.acApplyMetaFiltreExecute(Sender: TObject);
begin
  SetMetaFiltre(trim(editFiltre.Text));
end;


// affectation de styles
procedure TCadreDessinBGRA.acCourbeAffecterStyleExecute(Sender: TObject);
var
  EWE: TCourbe;
begin
  EWE := FDocumentDessin.GetCourbe(FCurrentCurveIdx);
  EWE.IDStyleCourbe := TNatureObjetCourbe(cmbNatureObjet.ItemIndex);
  FDocumentDessin.PutCourbe(FCurrentCurveIdx, EWE);
  // controle
  EWE := FDocumentDessin.GetCourbe(FCurrentCurveIdx);
  cmbNatureObjet.ItemIndex := Ord(EWE.IDStyleCourbe);
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.acLigneAffecterStyleExecute(Sender: TObject);
var
  EWE: TSimpleLigne;
begin
  EWE := FDocumentDessin.GetSimpleLigne(FCurrentSimpleLineIdx);
  EWE.IDStyleLigne := TNatureObjetLigne(cmbNatureObjet.ItemIndex);
  FDocumentDessin.PutSimpleLigne(FCurrentSimpleLineIdx, EWE);
  // controle
  EWE := FDocumentDessin.GetSimpleLigne(FCurrentSimpleLineIdx);
  cmbNatureObjet.ItemIndex := Ord(EWE.IDStyleLigne);
  Vue.Invalidate; // RedessinEcran(false);
end;



procedure TCadreDessinBGRA.acEmptyActionExecute(Sender: TObject);
begin
  ;
end;

procedure TCadreDessinBGRA.acPanVueExecute(Sender: TObject);
begin
  SetModeTravail(mtPAN_PREMIER_POINT);
end;

procedure TCadreDessinBGRA.acPolygoneAffecterStyleExecute(Sender: TObject);
var
  EWE: TPolygone;
begin
  EWE := FDocumentDessin.GetPolygone(FCurrentPolygonIdx);
  EWE.IDStylePolygone := TNatureObjetPolygone(cmbNatureObjet.ItemIndex);
  FDocumentDessin.PutPolygone(FCurrentPolygonIdx, EWE);
  // controle
  EWE := FDocumentDessin.GetPolygone(FCurrentPolygonIdx);
  cmbNatureObjet.ItemIndex := Ord(EWE.IDStylePolygone);
  Vue.Invalidate; // RedessinEcran(false);
end;


procedure TCadreDessinBGRA.acPolylineAffecterStyleExecute(Sender: TObject);
var
  EWE: TPolyLigne;
begin
  EWE := FDocumentDessin.GetPolyligne(FCurrentPolylineIdx);
  EWE.IDStylePolyLine := TNatureObjetCourbe(cmbNatureObjet.ItemIndex);
  FDocumentDessin.PutPolyligne(FCurrentPolylineIdx, EWE);
  // controle
  EWE := FDocumentDessin.GetPolyligne(FCurrentPolylineIdx);
  cmbNatureObjet.ItemIndex := Ord(EWE.IDStylePolyLine);
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.acPrintInfosStationExecute(Sender: TObject);
var
  OT : TTextObject;
  WU : Byte;
  EWE: Boolean;
begin
  OT.IDBaseSt  := FCurrentBasePoint.IDStation;
  OT.IDGroupe  := FCurrentGroupeIdx;
  OT.Offset.setFrom(FMyPos.X - FCurrentBasePoint.PosStation.X,
                    FMyPos.Y - FCurrentBasePoint.PosStation.Y,
                    0.00);
  WU := ProposerTextAlignment(OT.Offset.X, OT.Offset.Y);
  OT.Alignment := WU;
  OT.IDStyleTexte := notDEBUG;
  OT.MarkToDelete := false;
  OT.LastModified := Now();
  EWE := (Trim(FCurrentBasePoint.IDTerrain) = '') or
         (Trim(FCurrentBasePoint.IDTerrain) = '--');
  if (EWE) then OT.Text := '%i' else OT.Text := '%i %s';
  OT.MaxLength    := Length(OT.Text);
  FDocumentDessin.AddTexte(OT, True);
  Vue.Invalidate;
end;

procedure TCadreDessinBGRA.acReverseCourbeExecute(Sender: TObject);
var
  QCourbeProvisoire: TCourbePolygoneProvisoire;
  EWE: TCourbe;
begin
  EWE := FDocumentDessin.GetCourbe(FCurrentCurveIdx);
  QCourbeProvisoire := TCourbePolygoneProvisoire.Create;
  try
    QCourbeProvisoire.SetDocDessin(FDocumentDessin);
    QCourbeProvisoire.LoadCourbe(EWE, false);
    QCourbeProvisoire.StripVertex;
    if (QCourbeProvisoire.GenerateCourbe(False, -1, -1, EWE)) then
       FDocumentDessin.PutCourbe(FCurrentCurveIdx, EWE);
    Vue.Invalidate; // RedessinEcran(false);
  finally
    FreeAndNil(QCourbeProvisoire);//QCourbeProvisoire.Free;
  end;
end;

procedure TCadreDessinBGRA.acReversePolylineExecute(Sender: TObject);
var
  QCourbeProvisoire: TCourbePolygoneProvisoire;
  EWE: TPolyLigne;
begin
  EWE := FDocumentDessin.GetPolyligne(FCurrentPolylineIdx);
  QCourbeProvisoire := TCourbePolygoneProvisoire.Create;
  try
    QCourbeProvisoire.SetDocDessin(FDocumentDessin);
    QCourbeProvisoire.LoadPolyligne(EWE, false);
    QCourbeProvisoire.StripVertex;
    if (QCourbeProvisoire.GeneratePolyLine(EWE)) then FDocumentDessin.PutPolyligne(FCurrentPolylineIdx, EWE);
    Vue.Invalidate; // RedessinEcran(false);
  finally
    FreeAndNil(QCourbeProvisoire);//QCourbeProvisoire.Free;
  end;
end;




procedure TCadreDessinBGRA.acSetCurrGroupeByObjectExecute(Sender: TObject);
begin

end;


procedure TCadreDessinBGRA.acSymboleAffecterStyleExecute(Sender: TObject);
var
  EWE: TSymbole;
begin
  EWE := FDocumentDessin.GetSymbole(FCurrentSymboleIdx);
  EWE.TypeObject := TNatureObjetSymbole(cmbNatureObjet.ItemIndex);
  FDocumentDessin.PutSymbole(FCurrentSymboleIdx, EWE);
  // controle
  EWE := FDocumentDessin.GetSymbole(FCurrentSymboleIdx);
  cmbNatureObjet.ItemIndex := Ord(EWE.TypeObject);
  Vue.Invalidate; // RedessinEcran(false);
end;


procedure TCadreDessinBGRA.acTexteAffecterStyleExecute(Sender: TObject);
var
  EWE: TTextObject;
begin
  EWE := FDocumentDessin.GetTexte(FCurrentTextIdx);
  EWE.IDStyleTexte := TNatureObjetTexte(cmbNatureObjet.ItemIndex);
  FDocumentDessin.PutTexte(FCurrentTextIdx, EWE);
  // controle
  EWE := FDocumentDessin.GetTexte(FCurrentTextIdx);
  cmbNatureObjet.ItemIndex := Ord(EWE.IDStyleTexte);
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.acTransformerCourbeEnPolyligneExecute(Sender: TObject);
var
  MyCourbe: TCourbe;
  PP: TPolyLigne;
begin
  MyCourbe := FDocumentDessin.GetCourbe(FCurrentCurveIdx);
  //FCourbePolygoneProvisoire.ClearVertex();
  FCourbePolygoneProvisoire.LoadCourbe(MyCourbe, false);
  if (FCourbePolygoneProvisoire.GeneratePolyLine(PP)) then
  begin
    PP.Closed := false;
    FDocumentDessin.AddPolyLigne(PP, True);
    FDocumentDessin.DeleteCourbe(FCurrentCurveIdx);
    SetCurrentCourbeIdx(0);
    SetModeTravail(mtNONE);
    Vue.Invalidate;
  end;
end;

procedure TCadreDessinBGRA.AddZoomParameters(const Z: TZoomParameters);
begin

end;

//------------------------------------------------------------------------------
procedure TCadreDessinBGRA.acDeleteCourbeExecute(Sender: TObject);
begin
  if (FCurrentCurveIdx >= 0) then
  begin
    if (QuestionOuiNon(format(rsMSG_DELETE_COURBE, [FCurrentCurveIdx]))) then
    begin
      FDocumentDessin.DeleteCourbe(FCurrentCurveIdx);
      FCourbePolygoneProvisoire.ClearVertex;
      SetCurrentCourbeIdx(0);
      Vue.Invalidate; // RedessinEcran(false);
    end;
  end;
end;


procedure TCadreDessinBGRA.acDeleteLigneExecute(Sender: TObject);
begin
  if (FCurrentSimpleLineIdx >= 0) then
  begin
    if (QuestionOuiNon(format(rsMSG_DELETE_LINE, [FCurrentSimpleLineIdx]))) then
    begin
      FDocumentDessin.DeleteSimpleLigne(FCurrentSimpleLineIdx);
      SetCurrentLigneIdx(0);
      Vue.Invalidate; // RedessinEcran(false);
    end;
  end;
end;





procedure TCadreDessinBGRA.acDeletePolygoneExecute(Sender: TObject);
begin
  if (FCurrentPolygonIdx >= 0) then
  begin
    if (QuestionOuiNon(format(rsMSG_DELETE_POLYGONE, [FCurrentPolygonIdx]))) then
    begin
      FDocumentDessin.DeletePolygone(FCurrentPolygonIdx);
      FCourbePolygoneProvisoire.ClearVertex;
      SetCurrentPolygoneIdx(0);
      Vue.Invalidate; // RedessinEcran(false);
    end;
  end;
end;

procedure TCadreDessinBGRA.acDeleteSymboleExecute(Sender: TObject);
begin
  if (FCurrentSymboleIdx >= 0) then
  begin
    if (QuestionOuiNon(format(rsMSG_DELETE_SYMBOLE, [FCurrentSymboleIdx]))) then
    begin
      FDocumentDessin.DeleteSymbole(FCurrentSymboleIdx);
      SetCurrentSymboleIdx(0);
      Vue.Invalidate; // RedessinEcran(false);
    end;
  end;
end;

procedure TCadreDessinBGRA.acDeleteTexteExecute(Sender: TObject);
begin
  if (FCurrentTextIdx >= 0) then
  begin
    if (QuestionOuiNon(format(rsMSG_DELETE_TEXTE, [FCurrentTextIdx]))) then
    begin
      FDocumentDessin.DeleteTexte(FCurrentTextIdx);
      SetCurrentTexteIdx(0);
      Vue.Invalidate; // RedessinEcran(false);
    end;
  end;
end;

procedure TCadreDessinBGRA.acDispCodeGHCaveDrawObjetExecute(Sender: TObject);
var
  WU: String;
  QIdxGrp: TIDGroupeEntites;
  QIdxObj: Integer;
  LS: TStringList;
  MyScrap: TScrap;
  MyCourbe: TCourbe;
  MyPolygone: TPolygone;
  MyPolyligne: TPolyLigne;
  MySymbole: TSymbole;
  MyTexte: TTextObject;
  MyImage: TImageObject;
  MySimpleligne: TSimpleLigne;
  j: Integer;
  V: TVertexPolygon;
  A: TArcCourbe;
begin
  QIdxGrp := GetCurrentGroupeIdx();
  QIdxObj := GetCurrentIdxObjectForModeSelection();
  WU := ChooseString(Ord(FModeSelectionEntites),
                       ['mseNONE',
                        'mseSCRAPS',
                        'mseCOURBES',
                        'msePOLYLIGNES',
                        'msePOLYGONES',
                        'mseLIGNES',
                        'mseSYMBOLES',
                        'mseTEXTES',
                        'mseIMAGES'
                       ]);
   ShowMessage(Format('Mode: %s - Groupe: %d - Idx: %d', [WU, QIdxGrp, QIdxObj]));
   LS := TStringList.Create;
   try
     LS.Clear;
     case FModeSelectionEntites of
       mseNONE:
         begin
           ;
         end;
       mseSCRAPS:
         begin
          MyScrap := FDocumentDessin.GetScrap(QIdxObj);
          LS.Add(GenererLigneEnteteScrap(QIdxObj, MyScrap));
          for j := 0 to High(MyScrap.Sommets) do
          begin
            V := MyScrap.Sommets[j];
            LS.Add(GenererLigneVertexPolygon(j, V));
          end;
          LS.Add(GenererLigneEndScrap());
         end;
       mseCOURBES:
         begin
           MyCourbe      := FDocumentDessin.GetCourbe(QIdxObj);
           LS.Add(GenererLigneEnteteCourbe(QIdxObj, MyCourbe));
           for j := 0 to High(MyCourbe.Arcs) do
           begin
             A := MyCourbe.Arcs[j];
             LS.Add(GenererLigneArcCourbe(j, A));
           end;
           LS.Add(GenererLigneEndCourbe());
         end;
       msePOLYLIGNES:
         begin
           MyPolyligne   := FDocumentDessin.GetPolyligne(QIdxObj);
           LS.Add(GenererLigneEntetePolyligne(QIdxObj, MyPolyligne));
           for j := 0 to High(MyPolyligne.Sommets) do
           begin
             V := MyPolyligne.Sommets[j];
             LS.Add(GenererLigneVertexPolygon(j, V));
           end;
           LS.Add(GenererLigneEndPolyligne());
         end;
       msePOLYGONES:
         begin
           MyPolygone    := FDocumentDessin.GetPolygone(QIdxObj);
           LS.Add(GenererLigneEntetePolygone(QIdxObj, MyPolygone));
           for j := 0 to High(MyPolygone.Sommets) do
           begin
             V := MyPolygone.Sommets[j];
             LS.Add(GenererLigneVertexPolygon(j, V));
           end;
           LS.Add(GenererLigneEndPolygone());
         end;
       mseLIGNES:
         begin
           MySimpleligne := FDocumentDessin.GetSimpleLigne(QIdxObj);
         end;
       mseSYMBOLES:
         begin
           MySymbole     := FDocumentDessin.GetSymbole(QIdxObj);
           WU := GenererLigneSymbole(QIdxObj, MySymbole);
           LS.Add(WU);
         end;
       mseTEXTES:
         begin
           MyTexte       := FDocumentDessin.GetTexte(QIdxObj);
           WU := GenererLigneTexte(QIdxObj, MyTexte);
         end;
       mseIMAGES:
         begin
           MyImage       := FDocumentDessin.GetImage(QIdxObj);
         end;
     else
       pass;
     end;
     DisplayEditeurTexte(LS,
                         Format('Mode: %s - Object #%d', [WU, QIdxObj]),
                         FModeSelectionEntites);

   finally
     FreeAndNil(LS);//LS.Free;
   end;
end;



procedure TCadreDessinBGRA.acEditSymboleExecute(Sender: TObject);
var
  QAbsPos : TPoint3Df;
  OP: TSymbole;
begin
  // QAbsPos n'est pas utilisé en mode édition
  QAbsPos.setFrom(-1.00, -1.00, -1.00);
  OP := FDocumentDessin.GetSymbole(FCurrentSymboleIdx);
  EditerSymboleObject(FDocumentDessin,
                      FCurrentSymboleIdx,
                      medMODIF,
                      FCurrentGroupeIdx,
                      FCurrentNatureSymbole,
                      OP,
                      QAbsPos,
                      RafraichirVue);
  //SetModeTravail(mtNONE);
  SetModeTravail(mtSELECT_SYMBOLE);  // pour pouvoir éditer un autre symbole

  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.acEditTexteExecute(Sender: TObject);
var
  QAbsPos : TPoint3Df;
  OT: TTextObject;
begin
  // QAbsPos n'est pas utilisé en mode édition
  QAbsPos.setFrom(-1.00, -1.00, -1.00);
  OT := FDocumentDessin.GetTexte(FCurrentTextIdx);
  EditerTextObject(FDocumentDessin,
                   FCurrentTextIdx, medMODIF,
                   FCurrentGroupeIdx,
                   FCurrentNatureTexte,
                   OT,
                   QAbsPos,
                   RafraichirVue);
  //SetModeTravail(mtNONE);
  SetModeTravail(mtSELECT_TEXTE);
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.mnuFILTRES_COULEURSClick(Sender: TObject);
begin
  editFiltre.Text := Format('COULEUR=$%.6X;', [FCurrentBasePoint.Couleur]);
  acApplyMetaFiltre.Execute;
end;

procedure TCadreDessinBGRA.mnuFILTRES_SERIEClick(Sender: TObject);
var
  EWE: TToporobotIDStation;
begin
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  {$ELSE}
  EWE := GetToporobotIDStation(FCurrentBasePoint);
  editFiltre.Text := Format('SERIE=%d;', [EWE.aSerie]);
  acApplyMetaFiltre.Execute;
  {$ENDIF TIDBASEPOINT_AS_TEXT}
end;

procedure TCadreDessinBGRA.mnuFILTRES_STATIONClick(Sender: TObject);
var
  EWE: TToporobotIDStation;
begin
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  {$ELSE}
  EWE := GetToporobotIDStation(FCurrentBasePoint);
  editFiltre.Text := Format('STATION=%d.%d;', [EWE.aSerie, EWE.aStation]);
  acApplyMetaFiltre.Execute;
  {$ENDIF TIDBASEPOINT_AS_TEXT}

end;

procedure TCadreDessinBGRA.pnlTypeObjetClick(Sender: TObject);
begin

end;

procedure TCadreDessinBGRA.PopUpGeneralPopup(Sender: TObject);
var
  EWE: TToporobotIDStation;
begin
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  {$ELSE}
  EWE := GetToporobotIDStation(FCurrentBasePoint);
  acLockCurrentStation.Caption := Format(rsCDR_DRW_ACN_LOCK_CURR_STATION, [EWE.aSerie, EWE.aStation, EWE.aIDTerrain]);
  acLockCurrentStation.Hint    := acLockCurrentStation.Caption;
  {$ENDIF TIDBASEPOINT_AS_TEXT}


end;

procedure TCadreDessinBGRA.PopupNotifier1Close(Sender: TObject; var CloseAction: TCloseAction);
begin

end;


procedure TCadreDessinBGRA.mnuAddSerieAtFiltreClick(Sender: TObject);
var
  EWE: TToporobotIDStation;
begin
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  {$ELSE}
  EWE := GetToporobotIDStation(FCurrentBasePoint);
  editFiltre.Text := editFiltre.Text + Format('SERIE=%d;', [EWE.aSerie]);
  acApplyMetaFiltre.Execute;
  {$ENDIF TIDBASEPOINT_AS_TEXT}


end;

procedure TCadreDessinBGRA.mnuAddStationAtFiltreClick(Sender: TObject);
var
  EWE: TToporobotIDStation;
begin
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  {$ELSE}
  EWE := GetToporobotIDStation(FCurrentBasePoint);
  editFiltre.Text := editFiltre.Text + Format('STATION=%d.%d;', [EWE.aSerie, EWE.aStation]);
  acApplyMetaFiltre.Execute;
  {$ENDIF TIDBASEPOINT_AS_TEXT}
end;


procedure TCadreDessinBGRA.mnuAddColorAtFiltreClick(Sender: TObject);
begin
  editFiltre.Text := editFiltre.Text + Format('COULEUR=$%.6X;', [FCurrentBasePoint.Couleur]);
  acApplyMetaFiltre.Execute;
end;

procedure TCadreDessinBGRA.mnuCancelClick(Sender: TObject);
begin
  SetModeTravail(mtNONE);
end;

// définit le métafiltre
procedure TCadreDessinBGRA.SetMetaFiltre(const MF: string);
begin
  if (chkMetafiltreActif.Checked) then
  begin
    editFiltre.Text := MF;
    FDocumentDessin.MetaFiltre(Trim(MF));
  end
  else
    FDocumentDessin.MetaFiltre('');
  Vue.Invalidate; // RedessinEcran(false);
end;
function TCadreDessinBGRA.GetMetaFiltre(): string;
begin
  Result := Trim(editFiltre.Text);
end;


// initialisation  et finalisation
function TCadreDessinBGRA.CdrDrwInitialize(): boolean;
var
  InitialDir: string;
  n: integer;
  Z: TZoomParameters;
  G: TGroupeEntites;
  EWE: TBaseStation;
  procedure S666(const Acn: TAction; const S: string);
  begin
    ACN.Caption := GetResourceString(S);
    ACN.Hint    := Acn.Caption;
  end;
  procedure ParametrerBitmap(var BT: TBGRABitmap; const FichierBMP: string);
  begin
    //affichermessage(FichierBMP);
    BT.LoadFromFile(InitialDir + FichierBMP);
    BT.ApplyGlobalOpacity(128);
  end;
begin
  Result := False;
  FEditableDrawing := false;
  self.PopupMenu := nil;
  // diverses captions et hints
  btnAssignFiltreGrpCourant.Hint     := GetResourceString(rsMISC_ASSIGN_FILTRE_TO_GROUPE);
  btnRecupereFiltreDepuisGroupe.Hint := GetResourceString(rsMISC_RETRIEVE_FILTRE_FROM_GROUPE);
  btnEraseFiltre.Hint                := GetResourceString(rsMISC_ERASE_FILTRE_FROM_GROUPE);
  // double tampon
  self.DoubleBuffered := true;
  // captions
  S666(acVue, rsCDR_DRW_ACN_VUE);
    S666(acRedess, rsCDR_DRW_ACN_VUE_REDESS);
    S666(acZoomTout, rsCDR_DRW_ACN_VUE_ZOOM_ALL);
    S666(acZoomWindow, rsCDR_DRW_ACN_VUE_ZOOM_WND);
    S666(acPanVue, rsCDR_DRW_ACN_VUE_ZOOM_PAN);
  //S666(acDeleteObjects, rsCDR_DRW_ACN_DELETE);
  S666(acCourbeAffecterGroupe, rsCDR_DRW_ACN_SET_GROUPE_IDX);
  S666(acLigneAffecterGroupe, rsCDR_DRW_ACN_SET_GROUPE_IDX);
  S666(acPolygoneAffecterGroupe, rsCDR_DRW_ACN_SET_GROUPE_IDX);
  S666(acSymboleAffecterGroupe, rsCDR_DRW_ACN_SET_GROUPE_IDX);
  S666(acTexteAffecterGroupe, rsCDR_DRW_ACN_SET_GROUPE_IDX);
  S666(acCourbeAffecterStyle, rsCDR_DRW_ACN_SET_NATURE_OBJET);
  S666(acLigneAffecterStyle, rsCDR_DRW_ACN_SET_NATURE_OBJET);
  S666(acPolygoneAffecterStyle, rsCDR_DRW_ACN_SET_NATURE_OBJET);
  S666(acSymboleAffecterStyle, rsCDR_DRW_ACN_SET_NATURE_OBJET);
  S666(acTexteAffecterStyle, rsCDR_DRW_ACN_SET_NATURE_OBJET);
  S666(acEditScrap, rsCDR_DRW_ACN_EDIT_OBJET);
  S666(acEditSymbole, rsCDR_DRW_ACN_EDIT_OBJET);
  S666(acEditTexte, rsCDR_DRW_ACN_EDIT_OBJET);
  S666(acReverseCourbe, rsCDR_DRW_ACN_REVERSE_COURBE);
  S666(acDispCodeGHCaveDrawObjet, rsCDR_DRW_ACN_DISP_GHCD_CODE_OBJ);
  S666(acPrintInfosStation, rsCDR_DRW_ACN_ADD_TEXT_W_INFO_ST);

  // pop up général
  mnuFiltres.Caption := rsCDR_DRW_MNU_FILTRES;
  acApplyMetaFiltre.Caption    := rsCDR_DRW_BTN_METAFILTRE;
  // index du dernier point trouvé

  {$IFDEF TIDBASEPOINT_AS_TEXT}
  FLastStationFound.IDStation   := '';
  {$ELSE}
  FLastStationFound.IDStation   := 0;
  {$ENDIF TIDBASEPOINT_AS_TEXT}

  FNbSommetsAjoutesAvecPtLocked := 0;
  // armement des drapeaux
  FDoDraw                      := False;  // dessin valide ?
  FDoLockCurrentStation        := False;  // verrouillage du point courant
  //paramètres de la vue 2D


  FVertexModified              := False;  // Drapeau Sommet de polygone modifié
  FAttenduSecondPoint          := False;  // Drapeau Attend un deuxmième point
  FUnObjetEstSelectionne       := False;  // Drapeau Un objet est sélectionné
  FDoAddVertexCourbePolygone   := False;  // Drapeau Ajouter un vertex ?
  FDoFreeHand                  := False;  // Drapeau Courbe à main levée
  // styles d'objets
  FCurrentNatureCourbe         := nocPAROI;
  FCurrentNaturePolygone       := nopDEFAULT;
  FCurrentNatureLigne          := nolFLECHE;
  FCurrentNatureSymbole        := nosPHOTO;
  FCurrentNatureTexte          := notDEBUG;
  //try
    AfficherMessage(Format('%s.CdrDrwInitialize()', [ClassName]));
    // pour les zooms
    FModeSelectionEntites := mseNONE;
    Result := False;
    AfficherMessage('--> Structure de stockage provisoire pour édition de courbes');
    FCourbePolygoneProvisoire := TCourbePolygoneProvisoire.Create;
    FCourbePolygoneProvisoire.ClearVertex;
    AfficherMessage('--> Structure de stockage provisoire pour courbe à main levée');
    FCourbeFreehand := TFreeHandCourbe.Create;
    FCourbeFreehand.Initialise();

    AfficherMessage('--> Construction du document de dessin');
    FDocumentDessin := TDocumentDessin.Create;
    AfficherMessage('--> Initialisation du document de dessin');
    FDocumentDessin.Initialize;
    AfficherMessage('--> Initialisation du document de dessin OK');
    //lbMessages.Caption     := '';
    // valeurs par défaut
    {$IFDEF TIDBASEPOINT_AS_TEXT}
    EWE.IDStation := '';
    {$ELSE}
    EWE.IDStation := -1;
    {$ENDIF TIDBASEPOINT_AS_TEXT}

    EWE.IDTerrain   := '---';
    SetCurrentBaseStation(EWE, False);
    // index groupe courant
    FCurrentGroupeIdx       := 0;
    // index d'entités courantes
    SetCurrentCourbeIdx(0);
    SetCurrentPolylineIdx(0);
    SetCurrentLigneIdx(0);
    SetCurrentPolygoneIdx(0);
    SetCurrentSymboleIdx(0);
    SetCurrentTexteIdx(0);

    // bitmaps pour les remplissages
    FTexturesPolygones.bmpClay       := TBGRABitmap.Create;
    FTexturesPolygones.bmpSand       := TBGRABitmap.Create;
    FTexturesPolygones.bmpEboulis    := TBGRABitmap.Create;
    FTexturesPolygones.bmpGalets     := TBGRABitmap.Create;
    FTexturesPolygones.bmpSnow       := TBGRABitmap.Create;
    FTexturesPolygones.bmpClayGalets := TBGRABitmap.Create;

    // textures  /!\ RAPPEL: Les fonctions CreateXxxTexture créent l'objet
    //                       Ne pas utiliser .Create
    FTexturesPolygones.bmpTextureFlotte  := CreateWaterTexture(100, 100);
    FTexturesPolygones.bmpTextureEboulis := CreateStoneTexture(100, 100);

    InitialDir := GetIconesTexturesDirectory(); // GetGHCaveDrawDirectory() + 'Icones' + PathDelim;
    ParametrerBitmap(FTexturesPolygones.bmpClay      , 'clay.bmp');
    ParametrerBitmap(FTexturesPolygones.bmpSand      , 'sand.bmp');
    ParametrerBitmap(FTexturesPolygones.bmpEboulis   , 'eboulis.bmp');
    ParametrerBitmap(FTexturesPolygones.bmpGalets    , 'galets.bmp');
    ParametrerBitmap(FTexturesPolygones.bmpSnow      , 'snow.bmp');
    ParametrerBitmap(FTexturesPolygones.bmpClayGalets, 'clay_galets.bmp');

    // ++++++++++++++++++++++++++
    lbModeTravail.Caption  := Format('%s (' + FORMAT_BASEPOINT + ')', [GetResourceString(rsMSG_READY), FCurrentBasePoint.IDStation]) ;
    SetMsgInLbMessage(errNONE, GetResourceString(rsMSG_READY));
    ResetViewLimits();

    Vue.Invalidate; // RedessinEcran(false);
    FRedessinInProcess := False;
    Result := True;
  //except
  //end;
end;




function TCadreDessinBGRA.CdrDrwFinalize(): boolean;
begin
  FDoDraw := False;
  AfficherMessage(Format('%s.CdrDrwFinalize()', [ClassName]));
  Result := False;
  try
    AfficherMessage('--> Liberation du document de dessin');
    FDocumentDessin.Finalize;
    AfficherMessage('--> Liberation de la courbe tampon');
    FCourbePolygoneProvisoire.ClearVertex;
    AfficherMessage('--> Liberation de la courbe main levée');
    FCourbeFreehand.Finalise();
    Result := True;
  finally
    FreeAndNil(FTexturesPolygones.bmpClay);//FTexturesPolygones.bmpClay.free;
    FreeAndNil(FTexturesPolygones.bmpSand);//FTexturesPolygones.bmpSand.free;
    FreeAndNil(FTexturesPolygones.bmpEboulis);//FTexturesPolygones.bmpEboulis.free;
    FreeAndNil(FTexturesPolygones.bmpGalets);//FTexturesPolygones.bmpGalets.Free;
    FreeAndNil(FTexturesPolygones.bmpSnow);//FTexturesPolygones.bmpSnow.free;
    FreeAndNil(FTexturesPolygones.bmpClayGalets);//FTexturesPolygones.bmpClayGalets.free;
    // libère textures
    FreeAndNil(FTexturesPolygones.bmpTextureFlotte);//FTexturesPolygones.bmpTextureFlotte.Free;
    FreeAndNil(FTexturesPolygones.bmpTextureEboulis);//FTexturesPolygones.bmpTextureEboulis.Free;

    FreeAndNil(FDocumentDessin);//FDocumentDessin.Free;
    FreeAndNil(FCourbePolygoneProvisoire);//FCourbePolygoneProvisoire.Free;
    FreeAndNil(FCourbeFreehand);//FCourbeFreehand.Free;
    FreeAndNil(FListePtsFreeHand);//FListePtsFreeHand.Free;
  end;
end;



// attraper les limites du dessin
function TCadreDessinBGRA.GetDrwCoinBasGauche(): TPoint2Df;
begin
  Result.setFrom(FRXMini, FRYMini);
end;
function TCadreDessinBGRA.GetDrwCoinHautDroit(): TPoint2Df;
begin
  Result.setFrom(FRXMaxi, FRYMaxi);
end;


procedure TCadreDessinBGRA.VolatileTraceTexte(const Cnv: TCanvas; const XX,
  YY: Double; const T: string);
var
  PM: TPoint2Df;
  PP: TPOINT;
begin
  PM.setFrom(XX, YY);
  PP := GetCoordsPlan(PM);
  Cnv.TextOut(PP.X, PP.Y, T);
end;


procedure TCadreDessinBGRA.TraceMarqueurDirectionnel(const Cnv: TCanvas; const XX, YY: double; const Angle: double);
const R = 16;
var
  sa: float;
  ca: float;
  PP : Array [0..2] of TPoint;
  PP0: TPOINT;
  HalfR: Integer;
begin
  sincos(Angle, sa, ca);
  HalfR := R div 2;
  PP0 := GetCoordsPlan(XX, YY);
  PP[0] := MakeTPoint(PP0.X + round(R * ca)    , PP0.Y - round(R * sa));
  PP[1] := MakeTPoint(PP0.X - round(HalfR * sa), PP0.Y - round(HalfR * ca));
  PP[2] := MakeTPoint(PP0.X + round(HalfR * sa), PP0.Y + round(HalfR * ca));
  Cnv.Polygon(PP);
end;
procedure TCadreDessinBGRA.VolatileDrawTriangle(const Cnv: TCanvas; const P1, P2, P3: TPoint2Df);
var
  Sommets: array[0..2] of TPoint;
begin
  Sommets[0] := GetCoordsPlan(P1);
  Sommets[1] := GetCoordsPlan(P2);
  Sommets[2] := GetCoordsPlan(P3);
  Cnv.Brush.Style  := bsSolid;
  Cnv.Polygon(Sommets)
end;
procedure TCadreDessinBGRA.VolatileDrawBasePoint(const Cnv: TCanvas; const BP: TBaseStation);
const
  CC = 8;
  DD = CC div 2;
var
  PP: TPOINT;

  {$IFDEF TIDBASEPOINT_AS_TEXT}
  WU: TIDBaseStation;
  {$ELSE}
  WU: TToporobotIDStation;
  {$ENDIF TIDBASEPOINT_AS_TEXT}
begin

  PP := GetCoordsPlan(BP.PosStation.X, BP.PosStation.Y);
  Cnv.Pen.Color    := clBlue;
  Cnv.Pen.Width    := 0;
  Cnv.Brush.Style  := bsSolid;
  Cnv.Brush.Color  := clLime;
  Cnv.EllipseC(PP.X, PP.Y, CC, CC);
  Cnv.Brush.Color  := clFuchsia;
  Cnv.EllipseC(PP.X, PP.Y, DD, DD);
  Cnv.Brush.Color  := clSilver;
  Cnv.Font.Color := clRed;
  Cnv.Font.Size  := 8;
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  WU := BP.IDStation;
  Cnv.TextOut(PP.X + 2 + CC, PP.Y + 2 + CC, Format(FORMAT_BASEPOINT, [WU]));
  {$ELSE}
  WU := GetToporobotIDStation(BP);
  Cnv.TextOut(PP.X + 2 + CC, PP.Y + 2 + CC, Format('%d.%d', [WU.aSerie, WU.aStation]));
  {$ENDIF TIDBASEPOINT_AS_TEXT}


end;

procedure TCadreDessinBGRA.RefreshVue();
begin
  Vue.Invalidate;
end;



procedure TCadreDessinBGRA.VolatileTraceCercle(const Cnv: TCanvas; const XX, YY: Double; const R: integer);
var
  PP: TPoint;
  Q : integer;
begin
  Q := R shr 1;
  PP := GetCoordsPlan(XX, YY);
  Cnv.EllipseC(PP.X, PP.Y, Q, Q);
end;
// test de visibilité d'un groupe dans la vue courante
function TCadreDessinBGRA.GroupeIsInView(const QGroupe: TGroupeEntites): boolean;
var
  R0: TRect2Df;
  R1: TRect2Df;
begin
  Result := false;
  R0.setFrom(FRXMini, FRYMini, FRXMaxi, FRYMaxi);
  R1.setFrom(QGroupe.BoundingBox.C1, QGroupe.BoundingBox.C2);
  result := IntersectRectangles(R0, R1);
end;
// une station est visible ?
function TCadreDessinBGRA.BasePointIsVisible(const BP: TBaseStation): boolean;
var
  R0: TRect2Df;
  P1, P2: TPoint2Df;
begin
  R0.setFrom(FRXMini, FRYMini, FRXMaxi, FRYMaxi);
  P1.setFrom(BP.PosExtr0.X, BP.PosExtr0.Y);
  P2.setFrom(BP.PosStation.X, BP.PosStation.Y);
  result := PointIsInRectangle(P1, R0) OR PointIsInRectangle(P2, R0);
end;

// charger et initialiser fichier dessin
function TCadreDessinBGRA.ChargerInitialiserDessin(const FichierDessin: TStringDirectoryFileName; const Editable: boolean): boolean;
var
  n: integer;
  G: TGroupeEntites;
  Z: TZoomParameters;
begin
  Result := False;
  FEditableDrawing := Editable;
  SetMsgInLbMessage(errBUSY, 'Ouverture du dessin ...');
  if (FDocumentDessin.LoadFromFile(FichierDessin)) then
  begin
    Result := PreparerCtxt(FParamsVue2D);
    SetMsgInLbMessage(errNONE, '');
    pnlTypeObjet.Visible := FEditableDrawing;
  end;
end;


function TCadreDessinBGRA.PreparerCtxt(const FP: TParamsVue2D): boolean;
begin
  result := false;
  FDoDraw := false;
  try
    ResetViewLimits;
    FParamsVue2D := FP;
    // cadrage du dessin
    FParamsVue2D.GrdTypeQuadrillage := tqGRID;
    FParamsVue2D.GrdSpcMainGrid := ProposerEquidistance(FDocumentDessin.GetCoordsMini, FDocumentDessin.GetCoordsMaxi);
    FParamsVue2D.GrdSpcSecGrid  := FParamsVue2D.GrdSpcMainGrid / 10;
    FParamsVue2D.GrdCrossSize   := FParamsVue2D.GrdSpcMainGrid / 20;
    //FParametresGrilles.GrdSpcSecGrid := FParametresGrilles.GrdSpcMainGrid / 10;
    // affecter dessin à la courbe provisoire
    FCourbePolygoneProvisoire.SetDocDessin(FDocumentDessin);
    FDocumentDessin.CalcBoundingBoxAllGroupes;
    // actualiser liste des groupes
    ActualiserCmbListeDesGroupes();
    // accrochage des popups
    if (FEditableDrawing) then self.PopupMenu := PopUpGeneral else self.PopupMenu := nil;
    FDoDraw := True;
    Result := True;
  except
  end;
end;

procedure TCadreDessinBGRA.ActualiserCmbListeDesGroupes();
var
  n: Integer;
  G: TGroupeEntites;
  Nb: Integer;
begin
  Nb := FDocumentDessin.GetNbGroupes();
  if (Nb > 0) then
  begin
    cmbGroupeObjet.Clear;
    for n := 0 to Nb - 1 do
    begin
      G := FDocumentDessin.GetGroupe(n);
      cmbGroupeObjet.Items.Add(Format('%d - %s', [G.IDGroupeEntites, G.NomGroupe]));
    end;
    cmbGroupeObjet.ItemIndex := 0;
  end;
  Vue.Invalidate;
end;

// ajout d'une entité courbe
procedure TCadreDessinBGRA.AjouterUneCourbe();
var
  C: TCourbe;
begin
  FCourbePolygoneProvisoire.SetIDGroupe(FCurrentGroupeIdx);
  FCourbePolygoneProvisoire.SetIDStyleCourbe(FCurrentNatureCourbe);
  if (FCourbePolygoneProvisoire.GenerateCourbe(True, -1, -1, C)) then
  begin
    C.IDGroupe      := FCurrentGroupeIdx;
    C.IDStyleCourbe := FCurrentNatureCourbe; //FDocumentDessin.GetLineStyle(FCurrentLineStyle);
    FDocumentDessin.AddCourbe(C, True);
  end;
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.AjouterUnePolyligne();
var
  P: TPolyLigne;
begin
  FCourbePolygoneProvisoire.SetIDGroupe(FCurrentGroupeIdx);
  FCourbePolygoneProvisoire.SetIDStyleCourbe(FCurrentNatureCourbe);
  if (FCourbePolygoneProvisoire.GeneratePolyLine(P)) then FDocumentDessin.AddPolyLigne(P, True);
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.AjouterUnPolygone();
var
  P: TPolygone;
begin
  FCourbePolygoneProvisoire.SetIDGroupe(FCurrentGroupeIdx);
  FCourbePolygoneProvisoire.SetIDStylePolygone(FCurrentNaturePolygone);
  if (FCourbePolygoneProvisoire.GeneratePolygon(P)) then FDocumentDessin.AddPolygone(P, True);
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.AjouterUnScrap();
var
  S: TScrap;
begin
  if (FCourbePolygoneProvisoire.GenerateScrap(S)) then
  begin
    S.IDGroupe := FCurrentGroupeIdx;
    S.Nom      := Format('Scrap%d', [FDocumentDessin.GetNbScraps()]);
    S.Couleur  := clSilver;
    S.Opacite  := 128;
    FDocumentDessin.AddScrap(S, True);
  end;
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.btnAssignFiltreGrpCourantClick(Sender: TObject);
var
  EWE: TGroupeEntites;
  n: Integer;
  S: String;
begin
  n := FDocumentDessin.GetInternalIdxGroupe(FCurrentGroupeIdx);
  EWE := FDocumentDessin.GetGroupe(n);
  S := Trim(EWE.Filtres);
  if (not (Trim(S) = '')) then
  begin
    if (QuestionOuiNon(Format('Le groupe contient le filtre "%s" - Remplacer', [S]))) then
    begin
      EWE.Filtres := trim(editFiltre.Text);
      FDocumentDessin.PutGroupe(n, EWE);
    end;
  end
  else
  begin
    EWE.Filtres := trim(editFiltre.Text);
    FDocumentDessin.PutGroupe(n, EWE);
  end;
end;

procedure TCadreDessinBGRA.btnEraseFiltreClick(Sender: TObject);
begin
  editFiltre.Text := '';
  SetMetaFiltre(trim(editFiltre.Text));
end;

// Paramétrage de la zone de dessin
procedure TCadreDessinBGRA.SetParametresVue2D(const PG: TParamsVue2D);
begin
  FParamsVue2D := PG;
  Vue.Invalidate;
end;
function TCadreDessinBGRA.GetParametresVue2D(): TParamsVue2D;
begin
  Result := FParamsVue2D;
end;
// zoom tout
procedure  TCadreDessinBGRA.ResetViewLimits();
var
  M: double;
  L: double;
  W: double;
  DX, DY: double;
  PM0, PM1: TPoint2Df;
  PP: TPoint;
  cnMini, cnMaxi: TPoint3Df;
begin
  AfficherMessage('ResetViewLimits');
  PP := MakeTPoint(Vue.Width, Vue.Height);
  PM1 := GetCoordsMonde(PP);
  PP := MakeTPoint(0, 0);
  PM0 := GetCoordsMonde(PP);
  with FDocumentDessin do
  begin
    cnMini := GetCoordsMini;
    cnMaxi := GetCoordsMaxi;
    DX := cnMaxi.X - cnMini.X;
    DY := cnMaxi.Y - cnMini.Y;
    M  := 0.02 * Hypot(DX, DY);
    L  := IIF(DX > DY, DX, DY);
    DX := PM1.X - PM1.X;
    DY := PM1.Y - PM0.Y;
    // TODO: A valider
    //L := L * (Vue.Width / Vue.Height);
    SetViewLimits(cnMini.X-M, cnMini.Y-M, cnMini.X + L + M, cnMini.Y + L + M);
  end;
end;



procedure TCadreDessinBGRA.SaveVueAsImage(const FichierImage: string);
begin
  FFichierImage := FichierImage;
  RedessinEcran(True);
end;

procedure TCadreDessinBGRA.DeplacerVue(const DepX, DepY: integer);
var
  P : TPoint;
  dq, dx, dy: Double;
begin
  P := MakeTPoint(0, 0);
  FZC1:=GetCoordsMonde(P);
  P := MakeTPoint(Vue.Width, Vue.Height);
  FZC2:=GetCoordsMonde(P);
  dq  := 0.02 * (FZC2.X - FZC1.X);
  dx  :=  dq * DepX;
  dy  := -dq * DepY;
  SetViewLimits(FRXMini+dx, FRYMini+dy, FRXMaxi+dx,FRYMaxi+dy);
end;
// groupe courant
procedure TCadreDessinBGRA.SetLabelsGroupe(const GP: TGroupeEntites);
begin
  lbColorGroupe.Color := GP.CouleurGroupe;
  lbCurrentGroupe.Caption := Format('%d - %s',[FCurrentGroupeIdx, GP.NomGroupe]);
  SetModeTravail(mtNONE);
end;

procedure TCadreDessinBGRA.SetCurrentGroupeByIDGroupe(const IDGrp: TIDGroupeEntites);
var
  GP: TGroupeEntites;
begin
  try
    FCurrentGroupeIdx := IDGrp;
    GP := FDocumentDessin.GetGroupeByIDGroupe(IDGRP);
    SetLabelsGroupe(GP);
  except
    AfficherMessageErreur('Aucun dessin');
  end;
end;



procedure TCadreDessinBGRA.SetCurrentGroupeByIdx(const Idx: integer);
var
  GP: TGroupeEntites;
begin
  try
    GP := FDocumentDessin.GetGroupe(Idx);
    FCurrentGroupeIdx := GP.IDGroupeEntites;
    SetLabelsGroupe(GP);
  except
    AfficherMessageErreur('Aucun dessin');
  end;
end;


function TCadreDessinBGRA.GetCurrentGroupeIdx(): TIDGroupeEntites;
begin
  Result := FCurrentGroupeIdx;
end;

function TCadreDessinBGRA.GetCurrentIdxObjectForModeSelection(): integer;
begin
  case self.FModeSelectionEntites of
    mseSCRAPS          : Result := self.GetCurrScrapIdx();
    mseCOURBES         : Result := self.GetCurrCourbeIdx();
    msePOLYLIGNES      : Result := self.GetCurrPolylineIdx();
    mseLIGNES          : Result := self.GetCurrSimpleLineIdx();
    msePOLYGONES       : Result := self.GetCurrPolygoneIdx();
    mseSYMBOLES        : Result := self.GetCurrSymboleIdx();
    mseTEXTES          : Result := self.GetCurrTextIdx();
    mseIMAGES          : Result := self.GetCurrImgIdx();
  else
    Result := -1;
  end;
end;

function TCadreDessinBGRA.GetCurrImgIdx(): integer;
begin
  Result := FCurrentImageIdx;
end;

function TCadreDessinBGRA.GetModeSelectionEntites(): TModeSelectionEntites;
begin
  Result := FModeSelectionEntites;
end;
// modes de travail
procedure TCadreDessinBGRA.SetModeTravail(const MT: TModeTravailCanvas);
  procedure QSetPen(const QPM: TPenMode; const QPS: TPenStyle; const QPW: integer; const QPC: TColor);
  begin
    Vue.Canvas.Pen.Mode := QPM;
    Vue.Canvas.Pen.Style:= QPS;
    Vue.Canvas.Pen.Color:= QPC;
    Vue.Canvas.Pen.Width:= QPW;
  end;
  procedure Rearmer();
  begin
    FUnObjetEstSelectionne     := False;
    FVertexModified            := False;
    FAttenduSecondPoint        := False;
    FDoAddVertexCourbePolygone := False;
    FDoMovePoint               := false;
    FMovingPoint               := false;
    QSetPen(pmCopy, psSolid, 0, clGray);
  end;
var
  WU: String;
  QScrap       : TScrap;
  QCourbe      : TCourbe;
  QPolyligne   : TPolyLigne;
  QPolygone    : TPolygone;
  QSimpleLigne : TSimpleLigne;
  QSymbole: TSymbole;
  QTextObject: TTextObject;
begin
  // on vide les boutons du panneau d'édition rapide
  AffecterActionABouton(btnEditObject, acEmptyAction);
  AffecterActionABouton(btnDeleteObject, acEmptyAction);
  AffecterActionABouton(btnAffecterGroupeByCombo, acEmptyAction);
  AffecterActionABouton(btnAffecterAuGroupeCourant, acEmptyAction);
  AffecterActionABouton(btnAffecterNatureByCombo, acEmptyAction);
  AffecterActionABouton(btnEditObjectSpecial, acEmptyAction);
  AffecterActionABouton(btnDispCodeGHCaveDraw, acEmptyAction);
  mnuAffecterGroupeFromObject.Action := acEmptyAction;
  FModeTravailCanvas    := MT;
  lbModeTravail.Caption      := Format('%s (' + FORMAT_BASEPOINT + ')',[GetResourceString(rsMSG_READY), FCurrentBasePoint.IDStation]) ;
  case MT of
    mtNONE:
      begin
        // mode de sélection d'entités par défaut = mseNONE
        FModeSelectionEntites := mseNONE;
        Rearmer();
        FCourbeFreehand.ViderListe();
        FModeEditPoint             := tepNONE;

        SetMsgInLbMessage(errNONE, '');
      end;
    // TODO: Du code factorisable ici
    mtPAN_PREMIER_POINT:
      begin
        FDoAddVertexCourbePolygone := False;
        FVertexModified            := False;
        FAttenduSecondPoint := True;
        QSetPen(pmCopy, psSolid, 0, clGray);
        SetMsgInLbMessage(errBUSY, 'Pan: Premier point');
      end;
    mtPAN_SECOND_POINT:
      begin
        QSetPen(pmNotXor, psSolid, 0, clGray);
        SetMsgInLbMessage(errBUSY, 'Pan: Deuxième point');
      end;
    mtZOOM_PREMIER_COIN:
      begin
        FAttenduSecondPoint := True;
        QSetPen(pmCopy, psSolid, 0, clGray);
        SetMsgInLbMessage(errBUSY, 'Zoom: Premier coin');
        lbMessages.Caption  := '' ;
      end;
    mtZOOM_SECOND_COIN:
      begin
        QSetPen(pmNotXor, psSolid, 0, clGray);
        SetMsgInLbMessage(errBUSY, 'Zoom: Deuxième coin');
      end;
    mtLIGNE_PREMIER_POINT:
      begin
        FAttenduSecondPoint := True;
        QSetPen(pmCopy, psSolid, 0, clGray);
        SetMsgInLbMessage(errBUSY, 'Ligne: Premier point');
      end;
    mtLIGNE_SECOND_POINT:
      begin
        QSetPen(pmNotXor, psSolid, 0, clGray);
        SetMsgInLbMessage(errBUSY, 'Ligne: Deuxième point');
      end;
    mtDRAW_FREEHAND_COURBE:
       begin
         AfficherMessage('freehand mode selected');
         FCourbeFreehand.ViderListe();
         Rearmer;
         lbModeTravail.Caption      := 'Courbe à main levée';
         SetMsgInLbMessage(errBUSY, 'Du point');
       end;
    mtDRAW_SCRAP:
       begin
         Rearmer();
         lbModeTravail.Caption      := 'Scrap';
         SetMsgInLbMessage(errBUSY, 'Du point');
       end;
    mtDRAW_COURBE:
       begin
         Rearmer();
         lbModeTravail.Caption      := 'Courbe';
         SetMsgInLbMessage(errBUSY, 'Du point');
       end;
    mtDRAW_POLYLINE:
       begin
         Rearmer();
         lbModeTravail.Caption      := 'Polyligne';
         SetMsgInLbMessage(errBUSY, 'Du point');
       end;
    mtMESURE_DISTANCE_SURFACE:
       begin
         Rearmer();
         lbModeTravail.Caption      := 'Mesure distance';
         SetMsgInLbMessage(errBUSY, 'Du point');
       end;
    mtDRAW_POLYGONE:
       begin
         Rearmer();
         lbModeTravail.Caption      := 'Polygone';
         SetMsgInLbMessage(errBUSY, 'Du point');
       end;
    //-----------------------------------------------
    mtSELECT_SCRAP: //mtCHOIX_COURBE:
       begin
         Rearmer();
         lbModeTravail.Caption      := 'Edit Scrap';
         SetMsgInLbMessage(errBUSY, 'Choisir un scrap');
         FModeSelectionEntites      := mseSCRAPS;
         btnDispCodeGHCaveDraw.Action := acDispCodeGHCaveDrawObjet;
       end;
    mtSELECT_COURBE: //mtCHOIX_COURBE:
       begin
         Rearmer();
         lbModeTravail.Caption      := 'Edit Courbe';
         SetMsgInLbMessage(errBUSY, 'Choisir une courbe');
         FModeSelectionEntites      := mseCOURBES;
       end;
    mtSELECT_POLYLIGNE: //mtCHOIX_POLYLIGNE:
       begin
         Rearmer();
         lbModeTravail.Caption      := 'Edit Polyligne';
         SetMsgInLbMessage(errBUSY, 'Choisir une polyligne');
         FModeSelectionEntites      := msePOLYLIGNES;
       end;
    mtSELECT_POLYGONE: //mtCHOIX_POLYGONE:
       begin
         Rearmer();
         lbModeTravail.Caption      := 'Edit Polygone';
         SetMsgInLbMessage(errBUSY, 'Choisir un polygone');
         FModeSelectionEntites      := msePOLYGONES;
       end;

    mtSELECT_LIGNE: //mtCHOIX_LIGNE:
       begin
         Rearmer();
         lbModeTravail.Caption      := 'Edit Ligne';
         SetMsgInLbMessage(errBUSY, 'Choisir une ligne');
         FModeSelectionEntites      := mseLIGNES;
       end;
    mtSELECT_SYMBOLE: //mtCHOIX_SYMBOLE:
       begin
         Rearmer();
         lbModeTravail.Caption      := 'Edit Symbole';
         SetMsgInLbMessage(errBUSY, 'Choisir un symbole');
         FModeSelectionEntites      := mseSYMBOLES;
       end;
    mtSELECT_TEXTE: //mtCHOIX_TEXTE:
       begin
         Rearmer();
         lbModeTravail.Caption      := 'Edit Texte';
         SetMsgInLbMessage(errBUSY, 'Choisir un texte');
         FModeSelectionEntites      := mseTEXTES;
       end;
    mtSELECT_IMAGE:
       begin
         FModeSelectionEntites      := mseIMAGES;
       end;
    mtSCRAP_EDIT_SOMMET:
      begin
        lbModeTravail.Caption := Format('Edit scrap: %d', [FCurrentScrapIdx]);
        SetMsgInLbMessage(errBUSY, 'Choix d''un sommet');
      end;
    mtSCRAP_MOVE_SOMMET:
      begin
        lbModeTravail.Caption := Format('Edit scrap %d', [FCurrentScrapIdx]);
        SetMsgInLbMessage(errBUSY, Format('Nouvelle position du sommet %d', [FNoVertex]));
        QSetPen(pmNotXor, psSolid, 0, clGray);
      end;
    mtCOURBE_EDIT_SOMMET:
      begin
        lbModeTravail.Caption := Format('Edit courbe: %d', [FCurrentCurveIdx]);
        SetMsgInLbMessage(errBUSY, 'Choix d''un sommet');
      end;
    mtCOURBE_MOVE_SOMMET:
      begin
        lbModeTravail.Caption := Format('Edit courbe %d', [FCurrentCurveIdx]);
        QSetPen(pmNotXor, psSolid, 0, clGray);
        // TODO: Support des tangentes
        WU := 'Vertex';
        (*
        case FPoignee of
          0: WU := 'Vertex';
          1: WU := 'Tangente droite';
          2: WU := 'Tangente gauche';
        end;
        //*)
        SetMsgInLbMessage(errBUSY, Format('Nouvelle position du sommet %d (%s)', [FNoVertex, WU]));
        QSetPen(pmNotXor, psSolid, 0, clGray)
      end;
    mtPOLYGON_EDIT_SOMMET:
      begin
        lbModeTravail.Caption := Format('Edit polygone: %d', [FCurrentPolygonIdx]);
        SetMsgInLbMessage(errBUSY, 'Choix d''un sommet');
      end;
    mtPOLYGON_MOVE_SOMMET:
      begin
        lbModeTravail.Caption := Format('Edit polygone %d', [FCurrentPolygonIdx]);
        SetMsgInLbMessage(errBUSY, Format('Nouvelle position du sommet %d', [FNoVertex]));
        QSetPen(pmNotXor, psSolid, 0, clGray);
      end;
    mtPOLYLIN_EDIT_SOMMET:
      begin
        lbModeTravail.Caption := Format('Edit polyligne: %d', [FCurrentPolylineIdx]);
        SetMsgInLbMessage(errBUSY, 'Choix d''un sommet');
      end;
    mtPOLYLIN_MOVE_SOMMET:
      begin
        lbModeTravail.Caption := Format('Edit polyligne %d', [FCurrentPolylineIdx]);
        SetMsgInLbMessage(errBUSY, Format('Nouvelle position du sommet %d', [FNoVertex]));
        QSetPen(pmNotXor, psSolid, 0, clGray)
      end;
  end;
  // affectation des boutons d'édition d'objets
  case MT of
    mtSELECT_SCRAP,
    mtSCRAP_EDIT_SOMMET,
    mtSCRAP_MOVE_SOMMET:
      begin
        AffecterActionABouton(btnEditObject, acEditScrap);
        AffecterActionABouton(btnDeleteObject, acDeleteScrap);
        AffecterActionABouton(btnAffecterAuGroupeCourant, acScrapAffecterGroupe);
        AffecterActionABouton(btnAffecterGroupeByCombo, acScrapAffecterCurrGrp);
        AffecterActionABouton(btnDispCodeGHCaveDraw, acDispCodeGHCaveDrawObjet);
        QScrap := FDocumentDessin.GetScrap(FCurrentScrapIdx);
        acSetCurrentGroupeFromThisScrap.Caption := Format('Définir groupe courant depuis le scrap #%d - Groupe #%d', [FCurrentScrapIdx, QScrap.IDGroupe]);
        mnuAffecterGroupeFromObject.Action := acSetCurrentGroupeFromThisScrap;
      end;
    mtSELECT_COURBE,
    mtCOURBE_EDIT_SOMMET,
    mtCOURBE_MOVE_SOMMET:
      begin
        AffecterActionABouton(btnDeleteObject, acDeleteCourbe);
        AffecterActionABouton(btnAffecterGroupeByCombo, acCourbeAffecterGroupe);
        AffecterActionABouton(btnAffecterAuGroupeCourant, acCourbeAffecterCurrGrp);
        AffecterActionABouton(btnAffecterNatureByCombo, acCourbeAffecterStyle);
        AffecterActionABouton(btnEditObjectSpecial    , acReverseCourbe);
        AffecterActionABouton(btnDispCodeGHCaveDraw, acDispCodeGHCaveDrawObjet);
        QCourbe := FDocumentDessin.GetCourbe(FCurrentCurveIdx);
        acSetCurrentGroupeFromThisCourbe.Caption := Format('Définir groupe courant depuis la courbe #%d - Groupe #%d', [FCurrentCurveIdx, QCourbe.IDGroupe]);
        mnuAffecterGroupeFromObject.Action := acSetCurrentGroupeFromThisCourbe;
      end;
    mtSELECT_POLYLIGNE,
    mtPOLYLIN_EDIT_SOMMET,
    mtPOLYLIN_MOVE_SOMMET:
      begin
        AffecterActionABouton(btnDeleteObject, acDeletePolyline); // Bug fixé
        AffecterActionABouton(btnAffecterGroupeByCombo, acPolyLineAffecterGroupe);
        AffecterActionABouton(btnAffecterAuGroupeCourant, acPolylignAffecterCurrGrp);
        AffecterActionABouton(btnAffecterNatureByCombo, acPolylineAffecterStyle);
        AffecterActionABouton(btnEditObjectSpecial    , acReversePolyline);
        AffecterActionABouton(btnFermerCourbePolyligne, acClosePolylign);
        AffecterActionABouton(btnDispCodeGHCaveDraw, acDispCodeGHCaveDrawObjet);
        QPolyligne := FDocumentDessin.GetPolyligne(FCurrentPolylineIdx);
        acSetCurrentGroupeFromThisPolyline.Caption := Format('Définir groupe courant depuis la polyligne #%d - Groupe #%d', [FCurrentPolylineIdx, QPolyligne.IDGroupe]);
        mnuAffecterGroupeFromObject.Action := acSetCurrentGroupeFromThisPolyline;

      end;
    mtSELECT_POLYGONE,
    mtPOLYGON_EDIT_SOMMET,
    mtPOLYGON_MOVE_SOMMET:
      begin
        AffecterActionABouton(btnDeleteObject, acDeletePolygone);
        AffecterActionABouton(btnAffecterGroupeByCombo, acPolygoneAffecterGroupe);
        AffecterActionABouton(btnAffecterAuGroupeCourant, acPolygonAffecterCurrGrp);
        AffecterActionABouton(btnAffecterNatureByCombo, acPolygoneAffecterStyle);
        AffecterActionABouton(btnDispCodeGHCaveDraw, acDispCodeGHCaveDrawObjet);
        QPolygone := FDocumentDessin.GetPolygone(FCurrentPolygonIdx);
        acSetCurrentGroupeFromThisPolygone.Caption := Format('Définir groupe courant depuis le polygone #%d - Groupe #%d', [FCurrentPolygonIdx, QPolygone.IDGroupe]);
        mnuAffecterGroupeFromObject.Action := acSetCurrentGroupeFromThisPolygone;
      end;
    mtSELECT_LIGNE:
      begin
        AffecterActionABouton(btnDeleteObject, acDeleteLigne);
        AffecterActionABouton(btnAffecterGroupeByCombo, acLigneAffecterGroupe);
        AffecterActionABouton(btnAffecterAuGroupeCourant, acLineAffecterCurrGrp);
        AffecterActionABouton(btnAffecterNatureByCombo, acLigneAffecterStyle);
        AffecterActionABouton(btnDispCodeGHCaveDraw, acDispCodeGHCaveDrawObjet);
        QSimpleLigne := FDocumentDessin.GetSimpleLigne(FCurrentSimpleLineIdx);
        acSetCurrentGroupeFromThisSimpleLine.Caption := Format('Définir groupe courant depuis la ligne #%d - Groupe #%d', [FCurrentSimpleLineIdx, QSimpleLigne.IDGroupe]);
        mnuAffecterGroupeFromObject.Action := acSetCurrentGroupeFromThisSimpleLine;
      end;
    mtSELECT_SYMBOLE:
      begin
        AffecterActionABouton(btnDeleteObject, acDeleteSymbole);
        AffecterActionABouton(btnAffecterGroupeByCombo, acSymboleAffecterGroupe);
        AffecterActionABouton(btnAffecterAuGroupeCourant, acSymboleAffecterCurrGrp);
        AffecterActionABouton(btnAffecterNatureByCombo, acSymboleAffecterStyle);
        AffecterActionABouton(btnDispCodeGHCaveDraw, acDispCodeGHCaveDrawObjet);
        QSymbole := FDocumentDessin.GetSymbole(FCurrentSymboleIdx);
        acSetCurrentGroupeFromThisSymbole.Caption := Format('Définir groupe courant depuis le symbole #%d - Groupe #%d', [FCurrentSymboleIdx, QSymbole.IDGroupe]);
        mnuAffecterGroupeFromObject.Action := acSetCurrentGroupeFromThisSymbole;
      end;
    mtSELECT_TEXTE:
      begin
        AffecterActionABouton(btnDeleteObject, acDeleteTexte);
        AffecterActionABouton(btnAffecterAuGroupeCourant, acTexteAffecterCurrGrp);
        AffecterActionABouton(btnDispCodeGHCaveDraw, acDispCodeGHCaveDrawObjet);
        QTextObject := FDocumentDessin.GetTexte(FCurrentTextIdx);
        acSetCurrentGroupeFromThisTextObject.Caption := Format('Définir groupe courant depuis le symbole #%d - Groupe #%d', [FCurrentTextIdx, QTextObject.IDGroupe]);
        mnuAffecterGroupeFromObject.Action := acSetCurrentGroupeFromThisTextObject;
      end;
  end;
end;



procedure TCadreDessinBGRA.SetMsgInLbMessage(const ErrCode: TErrorCodeGeneric; const S: string);
begin
  lbMessages.Color := clSilver;
  case errcode of
    errNONE     : lbMessages.Color := clSilver;
    errBUSY     : lbMessages.Color := clYellow;
    errOK       : lbMessages.Color := clGreen;
    errWARNING  : lbMessages.Color := RGBToColor(255, 159, 0);
    errERROR    : lbMessages.Color := clRed;
    // Erreur fatale: Violet, repris de la signalisation ferroviaire
    // le carré violet est prioritaire sur le carré classique
    errFATAL    : lbMessages.Color := RGBToColor(255, 0, 255);
  end;
  lbMessages.Caption := S;
end;

procedure TCadreDessinBGRA.SetTailleEchelle(const T: double);
const QMIN_TAILLE_ECHELLE = 5.00;
begin
  FParamsVue2D.TailleEchelle := IIF(T < QMIN_TAILLE_ECHELLE, QMIN_TAILLE_ECHELLE, T);;
  Vue.Invalidate; // RedessinEcran(false);
end;

function TCadreDessinBGRA.GetModeTravail(): TModeTravailCanvas;
begin
  Result := FModeTravailCanvas;
end;
procedure TCadreDessinBGRA.SetCurrentBaseStation(const BS: TBaseStation; const STLock: boolean);
var
  L, A, P: double;
  WU: TToporobotIDStation;
begin
  FCurrentBasePoint := BS;

  FDoLockCurrentStation  := STLock;
  chkBaseStationLocked.Checked := FDoLockCurrentStation;
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  chkBaseStationLocked.Caption := Format('Lock %s - %s', [FCurrentBasePoint.IDStation, FCurrentBasePoint.IDTerrain]);
  lbIDBaseStation.Caption := Format(FORMAT_BASEPOINT + ': %.2f, %.2f, %.2f',
                                   [FCurrentBasePoint.IDStation,
                                    FCurrentBasePoint.PosStation.X,
                                    FCurrentBasePoint.PosStation.Y,
                                    FCurrentBasePoint.PosStation.Z
                                    //L, A, P, FCurrentBasePoint.PosStation.Z
                                   ]);
  {$ELSE}
  WU := GetToporobotIDStation(FCurrentBasePoint);
  chkBaseStationLocked.Caption := Format('Lock %d - %s', [FCurrentBasePoint.IDStation, FCurrentBasePoint.IDTerrain]);
  lbIDBaseStation.Caption := Format('%d.%d: %s - %.2f, %.2f, %.2f',
                                   [WU.aSerie, WU.aStation, FCurrentBasePoint.IDTerrain,
                                    FCurrentBasePoint.PosStation.X,
                                    FCurrentBasePoint.PosStation.Y,
                                    FCurrentBasePoint.PosStation.Z
                                    //L, A, P, FCurrentBasePoint.PosStation.Z
                                   ]);
  {$ENDIF TIDBASEPOINT_AS_TEXT}
  GetBearingInc(FCurrentBasePoint.PosStation.X - FCurrentBasePoint.PosExtr0.X,
                FCurrentBasePoint.PosStation.Y - FCurrentBasePoint.PosExtr0.Y,
                FCurrentBasePoint.PosStation.Z - FCurrentBasePoint.PosExtr0.Z,
                L, A, P, 360.00, 360.00);

end;

procedure TCadreDessinBGRA.SetCurrentNatureCourbe(const WU: TNatureObjetCourbe);
begin
  FCurrentNatureCourbe := WU;
end;

procedure TCadreDessinBGRA.SetCurrentNaturePolyline(const WU: TNatureObjetCourbe);
begin
  FCurrentNatureCourbe := WU;
end;

procedure TCadreDessinBGRA.SetCurrentNaturePolygone(const WU: TNatureObjetPolygone);
begin
  FCurrentNaturePolygone := WU;
end;

procedure TCadreDessinBGRA.SetCurrentNatureLigne(const WU: TNatureObjetLigne);
begin
  FCurrentNatureLigne:= WU;
end;

procedure TCadreDessinBGRA.SetCurrentNatureSymbole(const WU: TNatureObjetSymbole);
begin
  FCurrentNatureSymbole := WU;
end;

procedure TCadreDessinBGRA.SetCurrentNatureTexte(const WU: TNatureObjetTexte);
begin
  FCurrentNatureTexte := WU;
end;

procedure TCadreDessinBGRA.SetCurrentCourbeIdx(const QIdx: integer);
var
  WU: TCourbe;
begin
  if (Not FDoDraw) then Exit;
  FCurrentCurveIdx := QIdx;
  if (FDocumentDessin.GetNbCourbes() = 0) then Exit;
  WU := FDocumentDessin.GetCourbe(FCurrentCurveIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(WU.IDGroupe);
  acDeleteCourbe.Caption := Format(rsCDR_DRW_ACN_VUE_DEL_COURBE, [FCurrentCurveIdx]);
  acDeleteCourbe.Hint := acDeleteCourbe.Caption;
  lbCurrentObject.Caption := Format('Courbe %d',[FCurrentCurveIdx]);
  // boutons de la fenêtre d'état coin haut droit
  AffecterActionABouton(btnEditObject, acEmptyAction);
  AffecterActionABouton(btnDeleteObject, acDeleteCourbe);
  AffecterActionABouton(btnAffecterGroupeByCombo, acCourbeAffecterGroupe);
  AffecterActionABouton(btnAffecterNatureByCombo, acCourbeAffecterStyle);
  // combos
  RemplirComboTypesCourbes(cmbNatureObjet, Ord(WU.IDStyleCourbe));
  // infos diverses
  lbInfosEntite.Caption := Format('%d vertexes', [Length(WU.Arcs)]);
end;

procedure TCadreDessinBGRA.SetCurrentPolylineIdx(const QIdx: integer);
var
  WU: TPolyLigne;
begin
  if (Not FDoDraw) then Exit;
  FCurrentPolylineIdx := QIdx;
  if (FDocumentDessin.GetNbPolylignes() = 0) then Exit;
  WU := FDocumentDessin.GetPolyligne(FCurrentPolylineIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(WU.IDGroupe);
  acDeletePolyline.Caption := Format(rsCDR_DRW_ACN_VUE_DEL_POLYLINE, [FCurrentPolylineIdx]);
  acDeletePolyline.Hint := acDeletePolyline.Caption;
  lbCurrentObject.Caption := Format('Polyligne %d',[FCurrentPolylineIdx]);
  // boutons de la fenêtre d'état coin haut droit
  AffecterActionABouton(btnEditObject, acEmptyAction);
  AffecterActionABouton(btnDeleteObject, acDeletePolyline);
  AffecterActionABouton(btnAffecterGroupeByCombo, acPolyLineAffecterGroupe);
  AffecterActionABouton(btnAffecterNatureByCombo, acPolylineAffecterStyle);
  // combos
  RemplirComboTypesCourbes(cmbNatureObjet, Ord(WU.IDStylePolyLine));
  // infos diverses
  lbInfosEntite.Caption := Format('A: %f m2, P.: %.2f m, %d vx', [WU.Area, WU.Perimeter, Length(WU.Sommets)]);
end;

procedure TCadreDessinBGRA.SetCurrentScrapIdx(const QIdx: integer);
var
  WU: TScrap;
begin
  if (Not FDoDraw) then Exit;
  FCurrentScrapIdx := QIdx;
  if (FDocumentDessin.GetNbScraps() = 0) then Exit;
  WU := FDocumentDessin.GetScrap(FCurrentScrapIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(WU.IDGroupe);
  acDeleteScrap.Caption := Format(rsCDR_DRW_ACN_VUE_DEL_SCRAP, [FCurrentScrapIdx]);
  acDeleteScrap.Hint := acDeleteScrap.Caption;
  lbCurrentObject.Caption := Format('Scrap %d',[FCurrentScrapIdx]);
  // infos diverses
  lbInfosEntite.Caption := Format('A: %f m2, P.: %.2f m, %d vx', [WU.Area, WU.Perimeter, Length(WU.Sommets)]);
  // boutons de la fenêtre d'état coin haut droit
  AffecterActionABouton(btnEditObject, acEditScrap);
  AffecterActionABouton(btnDeleteObject, acDeleteScrap);
end;

procedure TCadreDessinBGRA.SetCurrentLigneIdx(const QIdx: integer);
var
  WU: TSimpleLigne;
begin
  if (Not FDoDraw) then Exit;
  FCurrentSimpleLineIdx := QIdx;
  if (FDocumentDessin.GetNbSimpleLignes() = 0) then Exit;
  WU := FDocumentDessin.GetSimpleLigne(FCurrentSimpleLineIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(WU.IDGroupe);
  acDeleteLigne.Caption := Format(rsCDR_DRW_ACN_VUE_DEL_LIGNE, [FCurrentSimpleLineIdx]);
  acDeleteLigne.Hint := acDeleteLigne.Caption;
  lbCurrentObject.Caption := Format('Ligne %d',[FCurrentSimpleLineIdx]);
  // boutons de la fenêtre d'état coin haut droit
  AffecterActionABouton(btnEditObject, acEmptyAction);
  AffecterActionABouton(btnDeleteObject, acDeleteLigne);
  AffecterActionABouton(btnAffecterGroupeByCombo, acLigneAffecterGroupe);
  AffecterActionABouton(btnAffecterNatureByCombo, acLigneAffecterStyle);
  // combos
  RemplirComboTypesSimplesLignes(cmbNatureObjet, Ord(WU.IDStyleLigne));
end;

procedure TCadreDessinBGRA.SetCurrentPolygoneIdx(const QIdx: integer);
var
  WU: TPolygone;
begin
  if (Not FDoDraw) then Exit;
  if (FDocumentDessin.GetNbPolygones() = 0) then Exit;
  FCurrentPolygonIdx := QIdx;
  WU := FDocumentDessin.GetPolygone(FCurrentPolygonIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(WU.IDGroupe);
  acDeletePolygone.Caption := Format(rsCDR_DRW_ACN_VUE_DEL_POLYGONE, [FCurrentPolygonIdx]);
  acDeletePolygone.Hint    := acDeletePolygone.Caption;
  lbCurrentObject.Caption  := Format('Polygone %d',[FCurrentPolygonIdx]);
  // boutons de la fenêtre d'état coin haut droit
  AffecterActionABouton(btnEditObject, acEmptyAction);
  AffecterActionABouton(btnDeleteObject, acDeletePolygone);
  AffecterActionABouton(btnAffecterGroupeByCombo, acPolygoneAffecterGroupe);
  AffecterActionABouton(btnAffecterNatureByCombo, acPolygoneAffecterStyle);
  // infos diverses
  lbInfosEntite.Caption := Format('A: %f m2, P.: %.2f m, %d vx', [WU.Area, WU.Perimeter, Length(WU.Sommets)]);
  // combos
  RemplirComboTypesPolygones(cmbNatureObjet, Ord(WU.IDStylePolygone));
end;

procedure TCadreDessinBGRA.SetCurrentSymboleIdx(const QIdx: integer);
var
  WU: TSymbole;
begin
  if (Not FDoDraw) then Exit;
  if (FDocumentDessin.GetNbSymboles() = 0) then Exit;
  FCurrentSymboleIdx := QIdx;

  WU := FDocumentDessin.GetSymbole(FCurrentSymboleIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(WU.IDGroupe);
  acDeleteSymbole.Caption := Format(rsCDR_DRW_ACN_VUE_DEL_SYMBOLE, [FCurrentSymboleIdx]);
  acDeleteSymbole.Hint := acDeleteSymbole.Caption;
  lbCurrentObject.Caption := Format('Symbole %d',[FCurrentSymboleIdx]);
  // boutons de la fenêtre d'état coin haut droit
  AffecterActionABouton(btnEditObject, acEditSymbole);
  AffecterActionABouton(btnDeleteObject, acDeleteSymbole);
  AffecterActionABouton(btnAffecterGroupeByCombo, acSymboleAffecterGroupe);
  AffecterActionABouton(btnAffecterNatureByCombo, acSymboleAffecterStyle);
  // combo styles d'objet
  RemplirComboTypesSymboles(cmbNatureObjet, Ord(WU.TypeObject));

end;

procedure TCadreDessinBGRA.SetCurrentTexteIdx(const QIdx: integer);
var
  WU: TTextObject;
begin
  if (Not FDoDraw) then Exit;
  if (FDocumentDessin.GetNbTextes() = 0) then Exit;
  FCurrentTextIdx := QIdx;
  WU := FDocumentDessin.GetTexte(FCurrentTextIdx);
  cmbGroupeObjet.ItemIndex := FDocumentDessin.GetInternalIdxGroupe(WU.IDGroupe);
  acDeleteTexte.Caption    := Format(rsCDR_DRW_ACN_VUE_DEL_TEXTE, [FCurrentTextIdx]);
  acDeleteTexte.Hint       := acDeleteTexte.Caption;
  lbCurrentObject.Caption  := Format('Texte %d',[FCurrentTextIdx]);
  // boutons de la fenêtre d'état coin haut droit
  AffecterActionABouton(btnEditObject, acEditTexte);
  AffecterActionABouton(btnDeleteObject, acDeleteTexte);
  AffecterActionABouton(btnAffecterGroupeByCombo, acTexteAffecterGroupe);
  AffecterActionABouton(btnAffecterNatureByCombo, acTexteAffecterStyle);
  // combos
  RemplirComboTypesTextes(cmbNatureObjet, Ord(WU.IDStyleTexte));
end;

procedure TCadreDessinBGRA.SetFlagDrawBoundingBoxGroupes(const Q: boolean);
begin
  if (Q) then FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedBBXGroupes]
         else FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedBBXGroupes];
  Vue.Invalidate; // RedessinEcran(false);
end;



procedure TCadreDessinBGRA.RechercherStation(const Cle: string);
var
  EWE: TBaseStation;
  dx2, dy2: double;
begin
  if (Trim(Cle) = '') then Exit;
  if (FDocumentDessin.FindBasePoint(Trim(Cle), EWE)) then
  begin
    FLastStationFound := EWE;
    // centrer sur la station
    dx2 := 0.50 * (FZC2.X - FZC1.X);
    dy2 := 0.50 * (FZC2.Y - FZC1.Y);
    SetViewLimits(EWE.PosStation.X - dx2, EWE.PosStation.Y - dy2,
                  EWE.PosStation.X + dx2, EWE.PosStation.Y + dy2);
  end
  else
  begin
    ShowMessagefmt(rsIDSTATION_NOT_FOUND, [Cle]);
  end;
end;




procedure TCadreDessinBGRA.SetColorDefaultScrapsMonochromes(const C: TColor);
begin
  FParamsVue2D.ColorScrapMonochrome := C;
  Vue.Invalidate;
end;

procedure TCadreDessinBGRA.SetLimiteDispPente(const T: double);
begin
  FParamsVue2D.PenteLimiteDisp := T;
  Vue.Invalidate;
end;

procedure TCadreDessinBGRA.SetFlagDrawTexteDebug(const Q: boolean);
begin
  if (Q) then FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedDEBUG_TEXTS]
         else FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedDEBUG_TEXTS];
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.SetFlagDrawTextObjects(const Q: Boolean);
begin
  if (Q) then FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedTEXTES]
         else FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedTEXTES];
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.SetFlagScrapsMonochromes(const Q: boolean);
begin
  FParamsVue2D.DoDrawScrapsMonochromes := Q;
  Vue.Invalidate;
end;

procedure TCadreDessinBGRA.SetFlagDrawIDStations(const Q: Boolean);
begin
  if (Q) then FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedIDSTATIONS]
         else FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedIDSTATIONS];
  Vue.Invalidate;
end;

procedure TCadreDessinBGRA.SetFlagDrawImages(const Q: boolean);
begin
  if (Q) then FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedIMAGES]
         else FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedIMAGES];
  Vue.Invalidate;
end;

procedure TCadreDessinBGRA.SetFlagDrawPentes(const Q: boolean);
begin
  if (Q) then FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedDISP_PENTES]
         else FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedDISP_PENTES];
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.SetFlagDrawQuadrilles(const Q: Boolean);
begin
  if (Q) then FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedQUADRILLES]
         else FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedQUADRILLES];
  Vue.Invalidate;
end;

procedure TCadreDessinBGRA.SetTypeQuadrillage(const Q: TTypeQuadrillage);
begin
  FParamsVue2D.GrdTypeQuadrillage := Q;
  Vue.Invalidate;
end;

procedure TCadreDessinBGRA.SetGrdSpcGrid(const QdrSpcMain, QdrSpcSec: double);
begin
  FParamsVue2D.GrdSpcMainGrid := QdrSpcMain;
  FParamsVue2D.GrdSpcSecGrid  := QdrSpcSec;
  Vue.Invalidate;
end;

procedure TCadreDessinBGRA.SetFlagDrawCenterLine(const Q: Boolean);
begin
  if (Q) then FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedCENTERLINES]
         else FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedCENTERLINES];
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.SetFlagDrawEchelle(const Q: boolean; const TailleEchelle: double);
begin
  if (Q) then FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedECHELLE_NORD]
         else FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedECHELLE_NORD];
  SetTailleEchelle(TailleEchelle);
end;
procedure TCadreDessinBGRA.SetFlagDrawCadresPhotos(const Q: boolean);
begin
  if (Q) then FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedCADRES_PHOTO]
         else FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedCADRES_PHOTO];
  Vue.Invalidate;
end;
procedure TCadreDessinBGRA.SetFlagDrawScraps(const Q: boolean);
begin
  if (Q) then FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedSCRAPS]
         else FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedSCRAPS];
  Vue.Invalidate;
end;


//******************************************************************************
procedure TCadreDessinBGRA.SetWindowView(const Fact: double; const OffFactorX, OffFactorY: double);
var
  P1: TPoint2Df;
  P2: TPoint2Df;
  Marge, OffX, OffY: double;
begin
  P1 := self.GetDrwCoinBasGauche;
  P2 := self.GetDrwCoinHautDroit;
  AfficherMessage(Format('%s.ZoomPlus: %f, %f -> %f, %f', [ClassName, P1.X, P1.Y, P2.Y, P2.Y]));
  Marge := (Fact - 1) * (P2.X - P1.X);
  OffX := OffFactorX * (P2.X - P1.X);
  OffY := OffFactorY * (P2.Y - P1.Y);
  self.SetViewLimits(P1.X + Marge + OffX,
                     P1.Y + Marge + OffY,
                     P2.X - Marge + OffX,
                     P2.Y - Marge + OffY);

end;





//******************************************************************************
// procedure TCadreDessinBGRA.AfficherDebugVariablesInternes;
// Cette procédure est très volumineuse, aussi, elle est déportée dans un fichier à inclure
//{$INCLUDE CadreDessinAfficherDebugVariables.inc}
//******************************************************************************

// zooms et limites de vue
procedure TCadreDessinBGRA.SetViewLimits(const X1, Y1, X2, Y2: double);
const Epsilon=1e-2;
var
  qX1, qX2, qY1, qY2: Double;
  d1, d2: double;
begin
  AfficherMessage(Format('%s.SetViewLimits(%.2f, %.2f, %.2f, %.2f', [ClassName, X1, Y1, X2, Y2]));
  qX1 := Math.Min(X1, X2);
  qY1 := Math.Min(Y1, Y2);
  qX2 := Math.Max(X1, X2);
  qY2 := Math.Max(Y1, Y2);
  d1  := qX2-qX1;
  d2:=qY2-qY1;

  if ((Abs(d1) < Epsilon) or (Abs(d2) < Epsilon)) then Exit;   // si zone trop étroite, abandonner


  FRXMini := qX1;
  FRXMaxi := qX2;
  FRYMini := qY1;
  FRYMaxi := qY2;
  // Etendue
  FEtendueX    := FRXMaxi - FRXMini;
  FEtendueY    := FRYMaxi - FRYMini;
  FEtendueXY   := Hypot(FEtendueX, FEtendueY);
  // Redéfinition de la hauteur maxi
  FRYMaxi:=GetRYMaxi;
  // redessine
  Vue.Invalidate; // RedessinEcran(false);
end;
// attrape la hauteur de vue
function TCadreDessinBGRA.GetRYMaxi(): double;
// Cette fonction calcule également d'autres paramètres
begin
  // calcul du rapport Hauteur/largeur de vue
  FRappHLVue   := Vue.Height / Vue.Width;
  // calcul du rapport Ecran/Réel
  FRappScrReal := Vue.Width / (FRXMaxi-FRXMini);
  FInvRappScrReal:=1/FRappScrReal;
  // calcul de la hauteur de visualisation
  Result:=FRYMini + (FRXMaxi-FRXMini) * FRappHLVue;
end;
// conversion de coordonnées
function TCadreDessinBGRA.GetCoordsPlan (const PM: TPoint2Df): TPoint;
begin
  Result.X:=Round((PM.X - FRXMini) * FRappScrReal);
  Result.Y:=Round((FRYMaxi-PM.Y) * FRappScrReal);
end;



function TCadreDessinBGRA.GetCoordsMonde(const PP: TPoint): TPoint2Df;
begin
  Result.X:=  FInvRappScrReal * PP.X + FRXMini;
  Result.Y:= -FInvRappScrReal * PP.Y + FRYMaxi;
end;

function TCadreDessinBGRA.GetCoordsPlan(const QX, QY: double): TPoint;
var
  P1: TPoint2Df;
begin
  P1.setFrom(QX, QY);
  Result := GetCoordsPlan(P1);
end;

//******************************************************************************

// Evenement intercepté par OnMouseDown()
procedure TCadreDessinBGRA.VueClick(Sender: TObject);
begin
  pass;
end;



// édition et dessin des courbes
//******************************************************************************
// dessin de la courbe provisoire
procedure TCadreDessinBGRA.DessinerCourbeProvisoire(const Cnv: TCanvas);
var
  i: integer;
  dx, dy: double;
  V: TVertexCourbe;
  EWE: TBaseStation;
  WU: Boolean;
begin
  WU := (FModeTravailCanvas = mtDRAW_SCRAP) OR
        (FModeTravailCanvas = mtDRAW_COURBE) OR
        (FModeTravailCanvas = mtDRAW_POLYLINE) OR
        (FModeTravailCanvas = mtDRAW_POLYGONE) OR
        (FModeTravailCanvas = mtMESURE_DISTANCE_SURFACE);
  if (WU AND FDoAddVertexCourbePolygone) then
  begin
    V := FCourbePolygoneProvisoire.GetVertex(0);
    // /!\ Ne pas ici utiliser SetCurrentBaseStation !!
    if (FDocumentDessin.GetBasePointByIndex(V.IDStation, EWE)) then
    begin
      dx := EWE.PosStation.X + V.Offset.X;
      dy := EWE.PosStation.y + V.Offset.y;
      Cnv.Pen.Color   := clGray;
      Cnv.Brush.Color := clBlue;
      Cnv.Brush.Style := bsSolid;
      VolatileTraceVers(Cnv, dx, dy, False);

      for i:= 1 to FCourbePolygoneProvisoire.GetNbVertex() - 1 do
      begin
        V := FCourbePolygoneProvisoire.GetVertex(i);
        if (FDocumentDessin.GetBasePointByIndex(V.IDStation, EWE)) then
        begin
          dx  := EWE.PosStation.X + V.Offset.X;
          dy  := EWE.PosStation.y + V.Offset.y;
          VolatileTraceVers(Cnv, dx, dy, True);
        end;
      end;
    end;
  end;
end;

procedure TCadreDessinBGRA.VolatileTraceVers(const Cnv: TCanvas; const XX, YY: Double; const Drawn: boolean);
var
  PP: TPoint;
begin
  PP := GetCoordsPlan(XX, YY);
  if (Drawn) then Cnv.LineTo(PP.X, PP.Y)
             else Cnv.MoveTo(PP.X, PP.Y);
end;

//******************************************************************************
//procedure TCadreDessinBGRA.RedessinEcran(const DoGenererImage: boolean);
// Cette procédure est très volumineuse, aussi, elle est déportée dans un fichier à inclure
{$INCLUDE CadreDessinBGRARedessinEcran.inc}
//******************************************************************************

procedure TCadreDessinBGRA.VueDblClick(Sender: TObject);
begin
  if (FDoDraw) then
  begin
     // réinit crayons
     Vue.Canvas.Pen.Mode   := pmCopy;
     Vue.Canvas.Pen.Style  := psSolid;
     case FModeTravailCanvas of
        mtDRAW_SCRAP,
        mtDRAW_COURBE,
        mtDRAW_POLYLINE,
        mtDRAW_POLYGONE,
        mtMESURE_DISTANCE_SURFACE:
          begin
            if (FDoAddVertexCourbePolygone) then acValidateCourbe.Execute;
          end;
        mtDRAW_SYMBOLE:
          ;
        mtDRAW_TEXTE:
          ;
     else
        pass;
     end;
     {
        mtSELECT_SCRAP:
          begin
            //QRechercheEtTraceScrap(q);
          end;
        mtSELECT_COURBE:
          begin
            //QRechercheEtTraceCourbe(q);
          end;
        mtSELECT_LIGNE:
          begin
            //QRechercheEtTraceLigne(q);
          end;
        mtSELECT_POLYLIGNE:
          begin
            //QRechercheEtTracePolyligne(q);
          end;
        mtSELECT_POLYGONE:
          begin
            //QRechercheEtTracePolygone(q);
          end;
        mtMOVE_OBJET:
          begin
            ;
          end;
        mtSELECT_SYMBOLE:
          begin  // objets isolés
            q := FDocumentDessin.FindObject(FMyPos.X, FMyPos.Y, mseSYMBOLES);
            if (q >= 0) then
            begin
              SetCurrentSymboleIdx(Q);
              OP := FDocumentDessin.GetSymbole(Q);
              FProcGetPonctObject(OP, Q);
              QAbsPos := MakeTPoint3Df(FMyPos.X, FMyPos.Y, -1.00);
            end;
          end;
        mtSELECT_TEXTE:
          begin
            q := FDocumentDessin.FindObject(FMyPos.X, FMyPos.Y, mseTEXTES);
            if (q >= 0) then
            begin
              SetCurrentTexteIdx(Q);
              OT := FDocumentDessin.GetTexte(Q);
              FProcGetTextObject(OT, Q);
              //DrawTexte(Vue.Canvas, OT, True, -1);
              QAbsPos := MakeTPoint3Df(FMyPos.X, FMyPos.Y, -1.00);
            end;
          end;
     end;
     //}
  end;
end;

procedure TCadreDessinBGRA.VueMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
const
  Q_WHEEL_VERTICAL   = 0;
  Q_WHEEL_HORIZONTAL = 1;
  Q_WHEEL_ZOOM       = 2;
var
  QModeStr: String;
  QModeDeplVue: Integer;
  QZoom, Qdx, Qdy: Extended;
begin
  // Valeur de WheelDelta retournée: = 120 ; négatif en tirant, positif en poussant
  // Utilisation des touches: convention Inkscape
  // Shift: []         = zoom
  //        [ssShift]  = pan horizontal
  //        [ssCtrl]   = pan vertical
  QModeStr := 'Pan vertical';
  if      (ssShift in Shift) then QModeDeplVue := Q_WHEEL_HORIZONTAL  // pan H
  else if (ssCtrl  in Shift) then QModeDeplVue := Q_WHEEL_VERTICAL    // zoom
  else                            QModeDeplVue := Q_WHEEL_ZOOM;       // pan V
  QModeStr := ChooseString(QModeDeplVue, ['Pan vertical', 'Pan horizontal', 'Zoom']);

  AfficherMessage(Format('Wheel [%s]: %d', [QModeStr, WheelDelta]));
  QZoom := 1.00;
  Qdx   := 0.00;
  Qdy   := 0.00;
  case QModeDeplVue of
     Q_WHEEL_VERTICAL:
     begin
       Qdy := 0.05 * IIF(WheelDelta < 0, -1.0, 1.0);
     end;
     Q_WHEEL_HORIZONTAL:
     begin
       Qdx := 0.05 * IIF(WheelDelta < 0, -1.0, 1.0);
     end;
     Q_WHEEL_ZOOM:
     begin
       QZoom := 1 + 0.05 * IIF(WheelDelta < 0, -1.0, 1.0);
     end;
  end;
  SetWindowView(QZoom, Qdx, Qdy);
end;

procedure TCadreDessinBGRA.VuePaint(Sender: TObject);
begin
  RedessinEcran(false);
end;

procedure TCadreDessinBGRA.VueMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  PP: TPoint;
  dx, dy: double;
  procedure DrawLigne(const P1, P2: TPoint); inline;
  begin
    Vue.Canvas.Line(P1.X,P1.Y, P2.X, P2.Y);
  end;
  procedure DrawRectangle(const P1, P2: TPoint); inline;
  begin
    Vue.Canvas.Rectangle(P1.X, P1.Y, P2.X, P2.Y);
  end;
  procedure DrawShapeElastique(const QX, QY: integer; const SType: byte);
  begin
    case SType of
      1: DrawLigne(FZP1, FZP2);
      2: DrawRectangle(FZP1, FZP2);
    end;
    FZP2 := MakeTPoint(QX, QY);
    case SType of
      1: DrawLigne(FZP1, FZP2);
      2: DrawRectangle(FZP1, FZP2);
    end;
  end;
begin
  PP := MakeTPoint(X, Y);
  FPP := MakeTPoint(X, Y);
  FMyPos := GetCoordsMonde(PP);
  lbMousePos.Caption := Format('%.2f, %.2f',[FMyPos.X, FMyPos.Y]);
  case FModeTravailCanvas of
    mtMESURE_DISTANCE_SURFACE:
      begin
        lbMessages.Caption := Format('Distance %.2f m', [FCourbePolygoneProvisoire.GetLongueurDeveloppee(True, FMyPos.X, FMyPos.Y)]);
        //DrawShapeElastique(X, Y, 1);
      end;
    mtDRAW_FREEHAND_COURBE: // mode spécial main levée
      begin
        if (ssLeft in Shift) then
        begin
          if (FDoFreeHand) then
          begin
            Vue.Canvas.Pen.Color := clGray;
            Vue.Canvas.EllipseC(X, Y, 2, 2);
            FCourbeFreehand.AddPoint2D(FMyPos);
          end;
        end;
      end;
    mtPAN_SECOND_POINT:
      begin
        if (Not FAttenduSecondPoint) then Exit;
        DrawShapeElastique(X, Y, 1);
      end;
    mtZOOM_SECOND_COIN:
      begin
        if (Not FAttenduSecondPoint) then Exit;
        DrawShapeElastique(X, Y, 2);
      end;
    mtLIGNE_SECOND_POINT:
      begin
        if (Not FAttenduSecondPoint) then Exit;
        DrawShapeElastique(X, Y, 1);
      end;
    mtSELECT_COURBE: //, mtCHOIX_COURBE:
      begin
        if (FDoMovePoint) then
        begin
          FZP1 := GetCoordsPlan(FOffsetPoint0.X, FOffsetPoint0.Y);
          DrawShapeElastique(X, Y, 1);
          FOffsetPoint1   := FMyPos;
        end;
      end;
    mtCOURBE_MOVE_SOMMET,
    mtPOLYGON_MOVE_SOMMET,
    mtPOLYLIN_MOVE_SOMMET:
      begin
        FZP1 := GetCoordsPlan(FOffsetPoint0.X, FOffsetPoint0.Y);
        DrawShapeElastique(X, Y, 1);
        FOffsetPoint1   := FMyPos;
      end;
  else
    pass;
  end; // case FModeTravailCanvas
end;

procedure TCadreDessinBGRA.VueMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CV: TCourbe;
  QCP: TArrayPoints2Df; // pour test ; à supprimer ensuite
begin
  case FModeTravailCanvas of
  mtDRAW_FREEHAND_COURBE:
    begin
      FDoFreeHand := False;
      FCourbeFreehand.FreeHandListeMakeTArrayPts2Df(0.05, QCP);
      if (FCourbePolygoneProvisoire.GenerateCurveBySuiteDePoints(QCP, FDoLockCurrentStation, FIDGroupeEditing, FIDStyleCurveEditing, 0)) then
      begin
        AjouterUneCourbe();
      end;
      FCourbeFreehand.ViderListe();    // vider la liste des points main levée
      Vue.Invalidate;
      SetModeTravail(mtNONE);          // indispensable pour réarmer le contexte
      SetModeTravail(mtDRAW_FREEHAND_COURBE); // pour éditer un autre objet
    end;
  mtSELECT_COURBE: //, mtCHOIX_COURBE:
    begin
      pass;
    end;
  mtSELECT_POLYLIGNE: //, mtCHOIX_POLYGONE:
   begin
     pass;
   end;
  mtSELECT_POLYGONE: //, mtCHOIX_POLYGONE:
   begin
     pass;
   end;
  else
    pass;
  end;
end;

procedure TCadreDessinBGRA.VueMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  QIdx, QQ : integer;
  dy, dx: double;
  PP: TPoint;
  PM: TPoint2Df;
  QAbsPos: TPoint3Df;
  V     : TVertexCourbe;
  EWE: TBaseStation;
  QZ    : TZoomParameters;
  OP    : TSymbole;
  OT    : TTextObject;
  QSerSp, WU, QAT: TToporobotIDStation;
  PV, PG: TPolygone;
  SC: TScrap;
  MyImage: TImageObject;
  CV, QC1, QC2, QCourbe: TCourbe;
  PL: TPolyLigne;
  QIdxSymboleFound: Int64;
  procedure QSetPenMode(const QM: TPenMode; const QC: TColor);
  begin
    Vue.Canvas.Pen.Mode:= QM;
    Vue.Canvas.Pen.Color:= QC;
    PP := GetCoordsPlan(PM);
    FZP1:=PP;
    FZP2:=PP;
  end;
begin
  PopupNotifier1.Visible := false;
  // réassignation du pop-up par défaut
  if (FEditableDrawing) then self.PopupMenu := PopUpGeneral;
  // menu Filtres
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  {$ELSE}
  QSerSp := GetToporobotIDStation(FCurrentBasePoint);
  mnuFILTRES_SERIE.Caption := Format(rsCDR_DRW_MNU_FILTRES_SERIE, [QSerSp.aSerie]);
  mnuFILTRES_COULEURS.Caption := Format(rsCDR_DRW_MNU_FILTRES_COULEUR, [FCurrentBasePoint.Couleur]);
  WU := GetToporobotIDStation(FCurrentBasePoint);
  mnuFILTRES_STATION.Caption := Format(rsCDR_DRW_MNU_FILTRES_STATION, [WU.aSerie, WU.aStation, WU.aIDTerrain]);
  {$ENDIF TIDBASEPOINT_AS_TEXT}



  // coordonnées de souris et station courante
  PP := MakeTPoint(X, Y);
  FMyPos := GetCoordsMonde(PP);
  if (FDoDraw) then
  begin
    EWE := FDocumentDessin.GetNearBasepoint(FMyPos.X, FMyPos.Y, FCurrentBasePoint, FDoLockCurrentStation);
    SetCurrentBaseStation(EWE, FDoLockCurrentStation);
    QIdxSymboleFound := FDocumentDessin.FindObject(FMyPos.X, FMyPos.Y, mseSYMBOLES);
    if (QIdxSymboleFound >= 0) then
    begin
      OP := FDocumentDessin.GetSymbole(QIdxSymboleFound);
      if (nosPOINT_REMARQUABLE = OP.TypeObject) then
      begin
        {$IFDEF TIDBASEPOINT_AS_TEXT}
        PopupNotifier1.Title := EWE.IDStation;
        {$ELSE}
        QAT := GetToporobotIDStation(EWE);
        PopupNotifier1.Title := Format('%d.%d %s', [QAT.aSerie, QAT.aStation, QAT.aIDTerrain]);
        {$ENDIF TIDBASEPOINT_AS_TEXT}


        PopupNotifier1.Text  := Format('X = %.0f, Y = %.0f, Z = %.0f', [EWE.PosStation.X, EWE.PosStation.Y, EWE.PosStation.Z])+
                                #13#10 +
                                #13#10 +
                                OP.TagTexte;

        PopupNotifier1.ShowAtPos(X, Y);
      end;
    end;
  end;

  case FModeTravailCanvas of
    mtNONE:
       pass;
    // pan par sélection de deux points
    mtPAN_PREMIER_POINT:
       begin
         FZC1.setFrom(FMyPos.X, FMyPos.Y);
         FZP1 := FPP;
         FZP2 := FPP;
         SetModeTravail(mtPAN_SECOND_POINT);
       end;
    mtPAN_SECOND_POINT:
       begin
         FZC2 := FMyPos;
         dx:=-(FZC2.X-FZC1.X);
         dy:=-(FZC2.Y-FZC1.Y);
         QZ.Caption := '';
         QZ.X1 := FRXMini+dx;
         QZ.Y1 := FRYMini+dy;
         QZ.X2 := FRXMaxi+dx;
         QZ.Y2 := FRYMaxi+dy;
         SetViewLimits(QZ.X1, QZ.Y1, QZ.X2, QZ.Y2);
         AddZoomParameters(QZ);
         Vue.Invalidate; // RedessinEcran(false);
         SetModeTravail(mtNONE);
       end;
    // zoom par sélection de deux points
    mtZOOM_PREMIER_COIN:
       begin
         FZC1.setFrom(FMyPos.X, FMyPos.Y);
         FZP1 := FPP;
         FZP2 := FPP;
         SetModeTravail(mtZOOM_SECOND_COIN);
       end;
    mtZOOM_SECOND_COIN:
       begin
         FZC2 := FMyPos;
         QZ.Caption := ''; //Format('Vue_%d',[FTableauZooms.Count]);

         QZ.X1 := FZC1.X;
         QZ.Y1 := FZC1.Y;
         QZ.X2 := FZC2.X;
         QZ.Y2 := FZC2.Y;

         if (hypot(QZ.X2 - QZ.X1, QZ.Y2 - QZ.Y1) > 50.00) then
         begin
           SetViewLimits(QZ.X1, QZ.Y1, QZ.X2, QZ.Y2);
           AddZoomParameters(QZ);
         end;
         //if Assigned(FProcActualiserOnglet) then FProcActualiserOnglet(editFiltre.Text);
         Vue.Invalidate; // RedessinEcran(false);
         SetModeTravail(mtNONE);
       end;
    // ligne par pointage de deux points
    mtLIGNE_PREMIER_POINT:
       begin
         FZC1.setFrom(FMyPos.X, FMyPos.Y);
         FZP1 := FPP;
         FZP2 := FPP;
         FProcGetInfoBasePoint(FCurrentBasePoint);
         FCurrentSimpleLine.IDBaseStExt1    := FCurrentBasePoint.IDStation;
         FCurrentSimpleLine.IDStyleLigne    := FCurrentNatureLigne;
         FCurrentSimpleLine.IDGroupe        := FCurrentGroupeIdx;
         FCurrentSimpleLine.OffsetExtr1.X   := FMyPos.X - FCurrentBasePoint.PosStation.X;
         FCurrentSimpleLine.OffsetExtr1.Y   := FMyPos.Y - FCurrentBasePoint.PosStation.Y;
         FCurrentSimpleLine.OffsetExtr1.Z   := FCurrentBasePoint.PosStation.Z;
         SetModeTravail(mtLIGNE_SECOND_POINT);
       end;
    mtLIGNE_SECOND_POINT:
       begin
         //lbIDBaseStation.Caption := Format('%d',[FCurrentBasePoint.IDStation]);
         FProcGetInfoBasePoint(FCurrentBasePoint);
         FCurrentSimpleLine.IDBaseStExt2    := FCurrentBasePoint.IDStation;
         FCurrentSimpleLine.OffsetExtr2.X   := FMyPos.X - FCurrentBasePoint.PosStation.X;
         FCurrentSimpleLine.OffsetExtr2.Y   := FMyPos.Y - FCurrentBasePoint.PosStation.Y;
         FCurrentSimpleLine.OffsetExtr2.Z   := FCurrentBasePoint.PosStation.Z;
         FDocumentDessin.AddSimpleLigne(FCurrentSimpleLine, True);
         // pour ajout d'autres objets
         SetModeTravail(mtLIGNE_PREMIER_POINT);     //SetModeTravail(mtNONE);
         Vue.Invalidate; // RedessinEcran(false);
       end;
    // dessin d'objets ponctuels et de textes
    mtDRAW_SYMBOLE:
      begin
        QAbsPos.setFrom(FMyPos.X, FMyPos.Y, -1.00);
        EditerSymboleObject(FDocumentDessin, -1, medCREATION, FCurrentGroupeIdx, FCurrentNatureSymbole, OP, QAbsPos, RafraichirVue);
        // pour ajout d'autres objets
        SetModeTravail(mtDRAW_SYMBOLE);     //SetModeTravail(mtNONE);
        Vue.Invalidate; // RedessinEcran(false);
      end;
    mtDRAW_TEXTE:
       begin
         QAbsPos.setFrom(FMyPos.X, FMyPos.Y, 0.00);
         EditerTextObject(FDocumentDessin, -1, medCREATION, FCurrentGroupeIdx, FCurrentNatureTexte, OT, QAbsPos, RafraichirVue);
         // pour ajout d'autres objets
         SetModeTravail(mtDRAW_TEXTE);     //SetModeTravail(mtNONE);
         Vue.Invalidate; // RedessinEcran(false);
       end;
    mtDRAW_IMAGE:
      begin
        QAbsPos.setFrom(FMyPos.X, FMyPos.Y, 0.00);
        if (DoDialogTImageObject(FDocumentDessin, MyImage, FDocumentDessin.GetDossierContenantDoc(), QAbsPos, True)) then
        begin
          ShowMessage(MyImage.SrcFilename + '; ' + MyImage.Description);
          FDocumentDessin.AddImage(MyImage);
          Vue.Invalidate;
          if (Assigned(FProcRefreshImages)) then FProcRefreshImages(0);
        end;
      end;
    // scraps
    mtSELECT_SCRAP:
      begin
        FUnObjetEstSelectionne := QRechercheEtTraceScrap(QIdx);
        if (FUnObjetEstSelectionne) then SetModeTravail(mtSCRAP_EDIT_SOMMET);
      end;
    mtSCRAP_EDIT_SOMMET:
      begin
        FNoVertex := FCourbePolygoneProvisoire.GetIDVertexByXY(FMyPos.X, FMyPos.Y);
        if (FNoVertex > -1) then
        begin
          if (Shift = [ssShift, ssRight]) then
          begin
            if (QuestionOuiNon(Format('Effacer vertex %d',[FNoVertex]))) then
            begin
              FCourbePolygoneProvisoire.SetIDGroupe(FIDGroupeEditing);
              FCourbePolygoneProvisoire.DeleteVertex(FNoVertex);
              if (FCourbePolygoneProvisoire.GenerateScrap(SC)) then
              begin
                SC.IDGroupe := FIDGroupeEditing;
                SC := FDocumentDessin.CalcBoundingBoxScrap(SC);
                FDocumentDessin.PutScrap(FCurrentScrapIdx, SC);
              end;
              Vue.Invalidate; // RedessinEcran(false);
              SetModeTravail(mtNONE);
              Exit;
            end;
          end;
          if (Shift = [ssCtrl, ssRight]) then
          begin
            if (FCourbePolygoneProvisoire.AddVertexNearToPoint(FMyPos.X, FMyPos.Y)) then
            begin
              if (FCourbePolygoneProvisoire.GenerateScrap(SC)) then
              begin
                SC.IDGroupe := FIDGroupeEditing;
                SC := FDocumentDessin.CalcBoundingBoxScrap(SC);
                FDocumentDessin.PutScrap(FCurrentScrapIdx, SC);
              end;
              Vue.Invalidate; // RedessinEcran(false);
              SetModeTravail(mtNONE);
              Exit;
            end;
          end;
          FCurrVtxCurveProv := FCourbePolygoneProvisoire.GetVertex(FNoVertex);
          FOffsetPoint0     := FMyPos;
          SetModeTravail(mtSCRAP_MOVE_SOMMET);
        end;
        //*)
      end;
    mtSCRAP_MOVE_SOMMET:
      begin
        FOffsetPoint1 := FMyPos;
        FCurrVtxCurveProv.Offset.X += FOffsetPoint1.X - FOffsetPoint0.X;
        FCurrVtxCurveProv.Offset.Y += FOffsetPoint1.Y - FOffsetPoint0.Y;

        FCourbePolygoneProvisoire.PutVertex(FNoVertex, FCurrVtxCurveProv);
        if (FCourbePolygoneProvisoire.GenerateScrap(SC)) then
        begin
          SC.IDGroupe := FIDGroupeEditing;
          SC := FDocumentDessin.CalcBoundingBoxScrap(SC);
          FDocumentDessin.PutScrap(FCurrentScrapIdx, SC);
        end;
        // redessin écran
        Vue.Invalidate; // RedessinEcran(false);
        SetModeTravail(mtNONE);            // indispensable pour réarmer le contexte
      end;
    // courbes
    mtSELECT_COURBE: //mtCHOIX_COURBE:
      begin
        //*************************************************************************************

        FUnObjetEstSelectionne := QRechercheEtTraceCourbe(QIdx);
        AfficherMessageErreur(Format(' -- %s: Objet %s trouvé: %d', ['mtSELECT_COURBE', IIF(FUnObjetEstSelectionne, '', 'NON'), QIdx]));
        if (FUnObjetEstSelectionne) then SetModeTravail(mtCOURBE_EDIT_SOMMET);

      end;
    mtCOURBE_EDIT_SOMMET:
      begin
        QQ := FCourbePolygoneProvisoire.GetIDVertexTgtsByXY(FMyPos.X, FMyPos.Y);
        FNoVertex := QQ div 10;
        FPoignee  := QQ mod 10;
        if (FNoVertex > -1) then
        begin
          if (Shift = [ssShift, ssRight]) then
          begin
            if (QuestionOuiNon(Format('Effacer vertex %d',[FNoVertex]))) then
            begin
              FCourbePolygoneProvisoire.DeleteVertex(FNoVertex);
              FCourbePolygoneProvisoire.SetIDGroupe(FIDGroupeEditing);
              FCourbePolygoneProvisoire.SetIDStyleCourbe(FIDStyleCurveEditing);
              if (FCourbePolygoneProvisoire.GenerateCourbe(true, -1, -1, CV)) then
              begin
                CV := FDocumentDessin.CalcBoundingBoxCourbe(CV);
                CV.IDStyleCourbe := FIDStyleCurveEditing;
                // update de la courbe
                FDocumentDessin.PutCourbe(FCurrentCurveIdx, CV);
              end;
              // redessin écran
              Vue.Invalidate; // RedessinEcran(false);
              SetModeTravail(mtNONE);
              Exit;
            end;
          end;
          if (Shift = [ssCtrl, ssRight]) then
          begin
            if (QuestionOuiNon(Format('Couper la courbe %d au vertex %d',[FCurrentCurveIdx, FNoVertex]))) then
            begin
              QCourbe := FDocumentDessin.GetCourbe(FCurrentCurveIdx);

              AfficherMessage(Format('-- Fractionnement courbe %d (groupe: %d) apres arc %d', [FCurrentCurveIdx, QCourbe.IDGroupe, FNoVertex]));
              if (CasserUneCourbe(QCourbe, FNoVertex, QC1, QC2)) then
              begin
                AfficherMessage(Format('-- Marquage de la courbe %d pour effacement', [FCurrentCurveIdx]));
                FDocumentDessin.MarquerCourbesAEffacer(FCurrentCurveIdx);
                AfficherMessage('-- Nettoyage de la base');
                FDocumentDessin.NettoyerCourbes();
                // on ajoute les courbes obtenues
                AfficherMessage('-- Ajout courbe resultante 1');
                FDocumentDessin.AddCourbe(QC1, True);
                AfficherMessage('-- Ajout courbe resultante 1');
                FDocumentDessin.AddCourbe(QC2, True);
                vue.Invalidate;
                SetModeTravail(mtNONE);
                Exit;
              end;
            end;
          end;
          FCurrVtxCurveProv := FCourbePolygoneProvisoire.GetVertex(FNoVertex);
          FOffsetPoint0     := FMyPos;
          SetModeTravail(mtCOURBE_MOVE_SOMMET);
        end;
      end;
    mtCOURBE_MOVE_SOMMET:
      begin
        FOffsetPoint1 := FMyPos;
        FCurrVtxCurveProv.Offset.X += FOffsetPoint1.X - FOffsetPoint0.X;
        // dans cette version, le déplacement d'un sommet lance un relissage de la courbe
        // l'implémentation du déplacement des tangentes viendra après
        FCourbePolygoneProvisoire.SetIDGroupe(FIDGroupeEditing);
        FCourbePolygoneProvisoire.SetIDStyleCourbe(FIDStyleCurveEditing);
        FCurrVtxCurveProv.Offset.Y += FOffsetPoint1.Y - FOffsetPoint0.Y;
        FCourbePolygoneProvisoire.PutVertex(FNoVertex, FCurrVtxCurveProv);
        if (FCourbePolygoneProvisoire.GenerateCourbe(true, -1, -1, CV)) then
        begin
          CV := FDocumentDessin.CalcBoundingBoxCourbe(CV);
          // update de la courbe
          FDocumentDessin.PutCourbe(FCurrentCurveIdx, CV);
        end;
        // redessin écran
        Vue.Invalidate; // RedessinEcran(false);
        SetModeTravail(mtNONE);            // indispensable pour réarmer le contexte
      end;

    // polygones
    mtSELECT_POLYGONE:
      begin
        FUnObjetEstSelectionne := QRechercheEtTracePolygone(QIdx);
        AfficherMessageErreur(Format(' -- %s: Objet %s trouvé: %d', ['mtSELECT_POLYGONE', IIF(FUnObjetEstSelectionne, '', 'NON'), QIdx]));
        if (FUnObjetEstSelectionne) then SetModeTravail(mtPOLYGON_EDIT_SOMMET);
      end;
    mtPOLYGON_EDIT_SOMMET:
      begin
        FNoVertex := FCourbePolygoneProvisoire.GetIDVertexByXY(FMyPos.X, FMyPos.Y);
        if (FNoVertex > -1) then
        begin
          if (Shift = [ssShift, ssRight]) then
          begin
            if (QuestionOuiNon(Format('Effacer vertex %d',[FNoVertex]))) then
            begin
              FCourbePolygoneProvisoire.SetIDGroupe(FIDGroupeEditing);
              FCourbePolygoneProvisoire.SetIDStylePolygone(FIDStylePolygoneEditing);
              FCourbePolygoneProvisoire.DeleteVertex(FNoVertex);
              if (FCourbePolygoneProvisoire.GeneratePolygon(PV)) then
              begin
                PV := FDocumentDessin.CalcBoundingBoxPolygone(PV);
                PV.IDStylePolygone := FIDStylePolygoneEditing;
                // update de la courbe
                FDocumentDessin.PutPolygone(FCurrentPolygonIdx, PV);
              end;
              // redessin écran
              Vue.Invalidate; // RedessinEcran(false);
              SetModeTravail(mtNONE);
              Exit;
            end;
          end;
          FCurrVtxCurveProv := FCourbePolygoneProvisoire.GetVertex(FNoVertex);
          FOffsetPoint0     := FMyPos;
          SetModeTravail(mtPOLYGON_MOVE_SOMMET);
        end;
      end;
    mtPOLYGON_MOVE_SOMMET:
      begin
        FOffsetPoint1 := FMyPos;
        FCurrVtxCurveProv.Offset.X += FOffsetPoint1.X - FOffsetPoint0.X;
        FCurrVtxCurveProv.Offset.Y += FOffsetPoint1.Y - FOffsetPoint0.Y;
        FCourbePolygoneProvisoire.SetIDGroupe(FIDGroupeEditing);
        FCourbePolygoneProvisoire.SetIDStylePolygone(FIDStylePolygoneEditing);
        FCourbePolygoneProvisoire.PutVertex(FNoVertex, FCurrVtxCurveProv);
        if (FCourbePolygoneProvisoire.GeneratePolygon(PG)) then
        begin
          PG := FDocumentDessin.CalcBoundingBoxPolygone(PG);
          FDocumentDessin.PutPolygone(FCurrentPolygonIdx, PG);
        end;
        // redessin écran
        Vue.Invalidate; // RedessinEcran(false);
        SetModeTravail(mtNONE);            // indispensable pour réarmer le contexte
      end;
    // polyligne
    mtSELECT_POLYLIGNE:
      begin
        FUnObjetEstSelectionne := QRechercheEtTracePolyligne(QIdx);
        AfficherMessageErreur(Format(' -- %s: Objet %s trouvé: %d', ['mtSELECT_POLYLIGNE', IIF(FUnObjetEstSelectionne, '', 'NON'), QIdx]));
        if (FUnObjetEstSelectionne) then SetModeTravail(mtPOLYLIN_EDIT_SOMMET);
      end;
    mtPOLYLIN_EDIT_SOMMET:
      begin
        FNoVertex := FCourbePolygoneProvisoire.GetIDVertexByXY(FMyPos.X, FMyPos.Y);
        if (FNoVertex > -1) then
        begin
          if (Shift = [ssShift, ssRight]) then
          begin
            if QuestionOuiNon(Format('Effacer vertex %d',[FNoVertex])) then
            begin
              FCourbePolygoneProvisoire.SetIDGroupe(FIDGroupeEditing);
              FCourbePolygoneProvisoire.SetIDStyleCourbe(FIDStyleCurveEditing);
              FCourbePolygoneProvisoire.DeleteVertex(FNoVertex);
              if (FCourbePolygoneProvisoire.GeneratePolyLine(PL)) then
              begin
                PL := FDocumentDessin.CalcBoundingBoxPolyligne(PL);
                PL.IDStylePolyLine := FIDStyleCurveEditing;
                // update de la courbe
                FDocumentDessin.PutPolyligne(FCurrentPolylineIdx, PL);
              end;
              // redessin écran
              Vue.Invalidate; // RedessinEcran(false);
              SetModeTravail(mtNONE);
              Exit;
            end;
          end;
          if (Shift = [ssCtrl, ssRight]) then
          begin
            {
            if (QuestionOuiNon(Format('Couper la polyligne au vertex %d',[FNoVertex]))) then
            begin
              FCourbePolygoneProvisoire.SetIDGroupe(FIDGroupeEditing);
              FCourbePolygoneProvisoire.SetIDStyleCourbe(FIDStyleCurveEditing);
              (*
              if (FCourbePolygoneProvisoire.FissionnerPolyligne(FNoVertex, QP1, QP2)) then
              begin

                // détruire la courbe sélectionnée
                FDocumentDessin.DeletePolyligne(FCurrentPolylineIdx);
                // ajouter les deux courbes créées
                FDocumentDessin.AddPolyLigne(QP1, True);
                FDocumentDessin.AddPolyLigne(QP2, True);
                // redessin écran
                Vue.Invalidate; // RedessinEcran(false);
                SetModeTravail(mtNONE);
                Exit;
              end;
              //*)
            end;
            //*}
          end;
          FCurrVtxCurveProv := FCourbePolygoneProvisoire.GetVertex(FNoVertex);
          FOffsetPoint0     := FMyPos;
          SetModeTravail(mtPOLYLIN_MOVE_SOMMET);
        end;
      end;

    mtPOLYLIN_MOVE_SOMMET:
      begin
        FOffsetPoint1 := FMyPos;
        FCurrVtxCurveProv.Offset.X += FOffsetPoint1.X - FOffsetPoint0.X;
        FCurrVtxCurveProv.Offset.Y += FOffsetPoint1.Y - FOffsetPoint0.Y;
        FCourbePolygoneProvisoire.SetIDGroupe(FIDGroupeEditing);
        FCourbePolygoneProvisoire.SetIDStyleCourbe(FIDStyleCurveEditing);

        FCourbePolygoneProvisoire.PutVertex(FNoVertex, FCurrVtxCurveProv);
        if (FCourbePolygoneProvisoire.GeneratePolyLine(PL)) then
        begin
          PL := FDocumentDessin.CalcBoundingBoxPolyligne(PL);
          FDocumentDessin.PutPolyligne(FCurrentPolylineIdx, PL);
        end;

        Vue.Invalidate;    // redessin écran
        SetModeTravail(mtNONE);            // indispensable pour réarmer le contexte
      end;
    //--------------------------------
    mtSELECT_LIGNE: //mtCHOIX_LIGNE:
      begin
        FUnObjetEstSelectionne := QRechercheEtTraceLigne(QIdx);
        AfficherMessageErreur(Format(' -- %s: Objet %s trouvé: %d', ['mtSELECT_LIGNE', IIF(FUnObjetEstSelectionne, '', 'NON'), QIdx]));
      end;
    mtSELECT_SYMBOLE: //mtCHOIX_SYMBOLE:
      begin
        if (QIdxSymboleFound >= 0) then
        begin
          SetCurrentSymboleIdx(QIdxSymboleFound);
          DrawVolatileSymbole(Vue.Canvas, OP, QIdxSymboleFound, false, true);
          FProcGetPonctObject(OP, QIdxSymboleFound);
        end;
      end;
    mtSELECT_TEXTE: //mtCHOIX_TEXTE:
      begin
        QIdx := FDocumentDessin.FindObject(FMyPos.X, FMyPos.Y, mseTEXTES);
        AfficherMessageErreur(Format(' -- %s: Objet %s trouvé: %d', ['mtSELECT_TEXTE', IIF((QIdx>=0), '', 'NON'), QIdx]));
        if (QIdx >= 0) then
        begin
          SetCurrentTexteIdx(QIdx);
          OT := FDocumentDessin.GetTexte(QIdx);
          DrawVolatileTexte(Vue.Canvas, OT, QIdx, false, True);
          FProcGetTextObject(OT, QIdx);
        end;
      end;
    //-----------------------------------
    // dessin des objets utilisant l'objet TCourbePolygoneProvisoire
    mtDRAW_SCRAP,
    mtDRAW_COURBE,
    mtDRAW_POLYLINE,
    mtDRAW_POLYGONE,
    mtMESURE_DISTANCE_SURFACE:
      begin
         if (FEditableDrawing) then self.PopupMenu := mnuCourbe;  // réassignation du pop-up (si le dessin est éditable)
         if (Not FDoAddVertexCourbePolygone) then
         begin

           AfficherMessage(Format('Début courbe ou poly: Du point: ' + FORMAT_BASEPOINT, [FCurrentBasePoint.IDStation]));
           FProcGetInfoBasePoint(FCurrentBasePoint);
           FCourbePolygoneProvisoire.ClearVertex;

           V.IDStation := FCurrentBasePoint.IDStation;  // premier vertex
           V.Offset.setFrom(FMyPos.X - FCurrentBasePoint.PosStation.X,
                            FMyPos.Y - FCurrentBasePoint.PosStation.y,
                            FCurrentBasePoint.PosStation.Z);
           FNoVertex := 0;
           FNoPtCtrl := 0;
           FCourbePolygoneProvisoire.AddVertex(V);
           FDoAddVertexCourbePolygone := True;    // on passe en mode ajout de points
         end
         else  // C'est le premier point
         begin
           // réassignation du pop-up
          if (FEditableDrawing) then self.PopupMenu := mnuCourbe;  // réassignation du pop-up (si le dessin est éditable)
           AfficherMessage(Format('Poly : Au point: ' + FORMAT_BASEPOINT, [FCurrentBasePoint.IDStation]));
           FProcGetInfoBasePoint(FCurrentBasePoint);
           V.IDStation := FCurrentBasePoint.IDStation;
           V.Offset.setFrom(FMyPos.X - FCurrentBasePoint.PosStation.X,
                            FMyPos.Y - FCurrentBasePoint.PosStation.Y,
                            0.00);
           FCourbePolygoneProvisoire.AddVertex(V);
           Inc(FNoVertex);
           Inc(FNoPtCtrl);
           DessinerCourbeProvisoire(Vue.Canvas);
         end;
         // compteur d'avertissement - il est possible d'outrepasser cet avertissement.
         FNbSommetsAjoutesAvecPtLocked += 1;
         if ((FDoLockCurrentStation) and
             (FNbSommetsAjoutesAvecPtLocked > 10)) then
         begin
           ShowMessageFmt(rsADVICE_GD_NB_VERTEX_BASEPT, [FNbSommetsAjoutesAvecPtLocked]);
           FNbSommetsAjoutesAvecPtLocked := 0;
         end;
      end;
    mtDRAW_FREEHAND_COURBE:    // dessin à main levée
      begin
        AfficherMessage('--> La minuterie est armée');
        FDoFreeHand := True;
      end;
  end;
end;

procedure TCadreDessinBGRA.btnEditPolyClick(Sender: TObject);
begin
  AjouterUneCourbe;
  Vue.Invalidate;
  SetModeTravail(mtNONE);
end;

procedure TCadreDessinBGRA.btnRecupereFiltreDepuisGroupeClick(Sender: TObject);
var
  n: Integer;
  EWE: TGroupeEntites;
begin
  n := FDocumentDessin.GetInternalIdxGroupe(FCurrentGroupeIdx);
  EWE := FDocumentDessin.GetGroupe(n);
  editFiltre.Text := EWE.Filtres;
end;

procedure TCadreDessinBGRA.Button1Click(Sender: TObject);
var
  WU: Integer;
  IdGrp: TIDGroupeEntites;
begin
  if (QuestionOuiNon(GetResourceString('Confirmer cette action'))) then
  begin
    WU   := GetCurrentIdxObjectForModeSelection();
    IdGrp := GetCurrentGroupeIdx();
    if (WU >= 0) then FDocumentDessin.ReattribBasePointsDeUnObjet(FModeSelectionEntites, WU, IdGrp);
    Vue.Invalidate;
  end;
end;

procedure TCadreDessinBGRA.editFiltreKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
  begin
    FDocumentDessin.MetaFiltre(Trim(editFiltre.Text));
    if (Assigned(FProcActualiserOnglet)) then FProcActualiserOnglet(editFiltre.Text);
    Vue.Invalidate;
  end;
end;
// la factorisation de ce code avec la fonction TCadreDessinBGRA.RedessinEcran()
// demande le passage de nombreux paramètres supplémentaires.
function TCadreDessinBGRA.ExporterTopoEnImage(const QFileName: string;
                                              const LimitesDessin: TRect2Df;
                                              const ImgWidth: integer): boolean;
var
  dx, dy: Extended;
  ImgHeight: Int64;
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
  NbImages: Integer;
  MyImage: TImageObject;
begin
  if (Not FDoDraw) then Exit;
  dx := LimitesDessin.X2 - LimitesDessin.X1;
  dy := LimitesDessin.Y2 - LimitesDessin.Y1;
  ImgHeight := round(ImgWidth * (dy/dx));
  AfficherMessage(Format('%s.("%s", (%.2f, %.2f)->(%.2f, %.2f) - w=%d, h=%d',
                        [self.ClassName,
                         QFileName,
                         LimitesDessin.X1, LimitesDessin.Y1,
                         LimitesDessin.X2, LimitesDessin.Y2,
                         ImgWidth, ImgHeight]));


  //***************************
  if (FRedessinInProcess) then
  begin
    SetMsgInLbMessage(errBUSY, 'REDESS');
    AfficherMessageErreur(Format('[FRedessinInProcess = %s] Redessin en cours', [BoolToStr(FRedessinInProcess)]));
    exit;
  end;
  FRedessinInProcess := True;    // Armement du sémaphore
  try
    TmpBuffer:= TGHCaveDrawDrawingContext.Create(ImgWidth, ImgHeight, BGRA(Red(FParamsVue2D.BackGroundColor),
                                                 Green(FParamsVue2D.BackGroundColor),
                                                 Blue(FParamsVue2D.BackGroundColor),
                                                 255));
    try
      // paramétrage
      TmpBuffer.SetDocuTopo(FDocumentDessin);
      TmpBuffer.SetParamDessin(FParamsVue2D);
      TmpBuffer.SetTextures(FTexturesPolygones);
      TmpBuffer.SetBounds(LimitesDessin.X1, LimitesDessin.Y1, LimitesDessin.X2, LimitesDessin.Y2);

      // début conventionnel
      TmpBuffer.BeginDrawing();
      AfficherMessage(Format('%s.Redessiner(): %s', [ClassName, IIF(FDoDraw, '', '** Pas de document chargé **')]));
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

      // les images de fond
      // Ce type d'image n'est pas indispensable au dessin
      // On ignore le tracé et on passe à la suite
      if (tedIMAGES in FParamsVue2D.ElementsDrawn) then
      begin
        NbImages       := FDocumentDessin.GetNbImages();
        if (NbImages > 0) then
        begin
          for ii := 0 to NbImages - 1 do
          begin
            try
              MyImage := FDocumentDessin.GetImage(ii);
              TmpBuffer.DrawImage(MyImage);
            except
              AfficherMessageErreur(Format('Erreur en dessin de l''image: %d [%s]', [ii, MyImage.SrcFilename]));
            end;
          end;
        end;
      end;
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
          if (tedDETAILS in FParamsVue2D.ElementsDrawn) then
          begin
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
        for ii:= 0 to FDocumentDessin.GetNbBaseStations - 1 do
          TmpBuffer.DessinerBaseStation(FDocumentDessin.GetBaseStation(ii), 12);
      end;
      // et enfin l'échelle
      if (tedECHELLE_NORD in FParamsVue2D.ElementsDrawn) then TmpBuffer.DrawEchelleNord(ImgWidth, ImgHeight);
      // fin conventionnelle
      TmpBuffer.EndDrawing(); // 1A11831244785
    except
    end;
    // ne pas utiliser le SaveToFile de TmpBuffer
    TmpBuffer.SaveToFile(QFileName);      // OK (Charentais: 1.2 Mo)
    //TmpBuffer.Bitmap.SaveToFile(QFileName); // OK mais ne compresse pas l'image (Charentais: 44.1 Mo :-O )
  finally
    TmpBuffer.Free;
    FRedessinInProcess := False; // libération du sémaphore
    SetMsgInLbMessage(errNONE, 'DONE');
    // ne pas mettre cette ligne avant la libération du sémaphore
    Application.ProcessMessages;
  end;
end;



procedure TCadreDessinBGRA.acValidateCourbeExecute(Sender: TObject);
var
  EWE, QPerimetre, QAire: Double;
begin
  AfficherMessage('Pret pour enregistrer');
  case FModeTravailCanvas of
   mtDRAW_SCRAP:
     begin
       AjouterUnScrap();
       SetModeTravail(mtDRAW_SCRAP);
     end;
   mtDRAW_COURBE:
     begin
       AjouterUneCourbe();
       SetModeTravail(mtDRAW_COURBE);
     end;
   mtDRAW_POLYLINE:
     begin
       AjouterUnePolyligne();
       SetModeTravail(mtDRAW_POLYLINE);
     end;
   mtDRAW_POLYGONE:
     begin
       AjouterUnPolygone();
       SetModeTravail(mtDRAW_POLYGONE);
     end;
   mtMESURE_DISTANCE_SURFACE:
     begin
       EWE := FCourbePolygoneProvisoire.GetLongueurDeveloppee(false, FMypos.X, FMyPos.Y);
       if (FCourbePolygoneProvisoire.GetPerimetreAndAire(QPerimetre, QAire)) then
         ShowMessageFmt('Longueur du parcours: %.2f m - Périmètre: %.2f m - Aire: %.2f m2', [EWE, QPerimetre, QAire])
       else
         ShowMessageFmt('Longueur du parcours: %.2f m', [EWE]);
       SetModeTravail(mtNONE);
       vue.Invalidate;
     end;
  end;
end;

procedure TCadreDessinBGRA.acCancelCourbeExecute(Sender: TObject);
begin
  SetModeTravail(mtNONE);
end;

procedure TCadreDessinBGRA.acClosePolylignExecute(Sender: TObject);
var
  EWE: TPolyLigne;
begin
   if (FCurrentPolylineIdx >= 0) then
   begin
     EWE := FDocumentDessin.GetPolyligne(FCurrentPolylineIdx);
     if (FDocumentDessin.FermerPolyligne(EWE)) then
     begin
       FDocumentDessin.PutPolyligne(FCurrentPolylineIdx, EWE);
       Vue.Invalidate;
     end;
   end;
end;

//******************************************************************************

procedure TCadreDessinBGRA.acRedessExecute(Sender: TObject);
begin
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreDessinBGRA.acZoomToutExecute(Sender: TObject);
begin
  SetModeTravail(mtNONE);
  ResetViewLimits;
end;

procedure TCadreDessinBGRA.acZoomWindowExecute(Sender: TObject);
begin
  SetModeTravail(mtZOOM_PREMIER_COIN);
end;

procedure TCadreDessinBGRA.acDeleteLastObjectExecute(Sender: TObject);
begin
  //DeleteLastObject(FModeSelectionEntites);
end;

procedure TCadreDessinBGRA.Couleurcourante1Click(Sender: TObject);
begin
  SetMetaFiltre(Format('COULEUR=$%X',[FCurrentBasePoint.Couleur]));
end;

procedure TCadreDessinBGRA.Sriecourante1Click(Sender: TObject);
var
  Q: integer;
begin
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  {$ELSE}
  Q := ExtractNumSerieFromTIDBaseStation(FCurrentBasePoint.IDStation);
  SetMetaFiltre(Format('SERIE=%d',[Q]));
  {$ENDIF TIDBASEPOINT_AS_TEXT}
end;

procedure TCadreDessinBGRA.acDisplayDebugTextsExecute(Sender: TObject);
begin
  if (tedDEBUG_TEXTS in FParamsVue2D.ElementsDrawn) then  FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn - [tedDEBUG_TEXTS]
                                                    else  FParamsVue2D.ElementsDrawn := FParamsVue2D.ElementsDrawn + [tedDEBUG_TEXTS];
  //FDoDisplayDebugTexts := Not (FDoDisplayDebugTexts);
  acDisplayDebugTexts.Checked := (tedDEBUG_TEXTS in FParamsVue2D.ElementsDrawn); // FDoDisplayDebugTexts;
  Vue.Invalidate; // RedessinEcran(false);
end;
// callback de rafraichissement
procedure TCadreDessinBGRA.RafraichirVue;
begin
  //AfficherMessageErreur('Callback de refresh dans TCadreDessin');
  Vue.Invalidate; // RedessinEcran(false);
end;

function TCadreDessinBGRA.GetDocDessin(): TDocumentDessin;
begin
  Result := FDocumentDessin;
end;

//------------------------------------------------------------------------------
function TCadreDessinBGRA.QRechercheEtTraceCourbe(out QIdx: integer): boolean;
var
  CV  : TCourbe;
  BBX : TBoundingBox;
  QPM : TPoint2Df;
  q: Int64;
begin
  QIdx := -1;
  Result := False;
  if (0 = FDocumentDessin.GetNbCourbes()) then Exit;
  q := FDocumentDessin.FindObject(FMyPos.X, FMyPos.Y, mseCOURBES);
  if (q >= 0) then
  begin
    SetCurrentCourbeIdx(q);
    CV := FDocumentDessin.GetCourbe(q);
    FProcGetCourbe(CV, q);
    BBX := CV.BoundingBox; // on sauvegarde la BoundingBox
    QPM.setFrom(CV.BoundingBox.C1.X, CV.BoundingBox.C1.Y);
    QPM.setFrom(CV.BoundingBox.C2.X, CV.BoundingBox.C2.Y);
    try
       FCourbePolygoneProvisoire.ClearVertex;
       // récupération du groupe et du style
       FIDGroupeEditing  := CV.IDGroupe;
       FIDStyleCurveEditing:= CV.IDStyleCourbe;
       FCourbePolygoneProvisoire.LoadCourbe(CV, false);  // chargement courbe
       FCourbePolygoneProvisoire.GenerateCourbe(False, -1, -1, CV);
       CV.BoundingBox := BBX; // restauration de la BoundingBox
       DrawVolatileCourbe(vue.Canvas, CV, q, True, True);
       // affecter le bouton de strip
       AffecterActionABouton(btnEditObjectSpecial, acReverseCourbe);
       QIdx := Q;
       Result := True;
     except
     end;
  end;
end;
function TCadreDessinBGRA.QRechercheEtTracePolyligne(out QIdx: integer): boolean;
var
  PO: TPolyLigne;
  q: Int64;
begin
  QIdx := -1;
  Result := False;
  if (0 = FDocumentDessin.GetNbPolylignes()) then Exit;
  q := FDocumentDessin.FindObject(FMyPos.X, FMyPos.Y, msePOLYLIGNES);
  if (q >= 0) then
  begin
    SetCurrentPolylineIdx(Q);
    PO := FDocumentDessin.GetPolyligne(q);
    FProcGetPolyLigne(PO, Q);
    DrawVolatilePolyligne(Vue.Canvas, PO, Q, True, True, -1);
    try       // courbe provisoire
      FCourbePolygoneProvisoire.ClearVertex;
      // récupération du groupe et du style
      FIDGroupeEditing        := PO.IDGroupe;
      FIDStyleCurveEditing    := PO.IDStylePolyLine;
      FCourbePolygoneProvisoire.LoadPolyligne(PO, false); // récupération de la polyligne
      QIdx := Q;
      Result  := True;
    except
    end;
  end;
end;
function TCadreDessinBGRA.QRechercheEtTracePolygone(out QIdx: integer): boolean;
var
  PO: TPolygone;
  q: Int64;
begin
  QIdx := -1;
  Result := False;
  if (0 = FDocumentDessin.GetNbPolygones()) then Exit;
  q := FDocumentDessin.FindObject(FMyPos.X, FMyPos.Y, msePOLYGONES);
  if (q >= 0) then
  begin
    SetCurrentPolygoneIdx(Q);
    PO := FDocumentDessin.GetPolygone(q);
    FProcGetPolygon(PO, Q);
    DrawVolatilePolygone(Vue.Canvas, PO, Q, True, True, -1);
    try      // courbe provisoire
      FCourbePolygoneProvisoire.ClearVertex;
      // récupération du groupe et du style
      FIDGroupeEditing        := PO.IDGroupe;
      FIDStylePolygoneEditing := PO.IDStylePolygone;
      FCourbePolygoneProvisoire.LoadPolygone(PO);   // récupération du polygone
      QIdx := Q;
      Result  := True;
    except
    end;
  end;
end;
function TCadreDessinBGRA.QRechercheEtTraceScrap(out QIdx: integer): boolean;
var
  PO: TScrap;
  q: Int64;
begin
  QIdx := -1;
  Result := false;
  if (0 = FDocumentDessin.GetNbScraps()) then Exit;
  q := FDocumentDessin.FindObject(FMyPos.X, FMyPos.Y, mseSCRAPS);
  if (q >= 0) then
  begin
    SetCurrentScrapIdx(Q);
    PO := FDocumentDessin.GetScrap(q);
    FProcGetScrap(PO, Q);
    DrawVolatileScrap(Vue.Canvas, PO, Q, True, True, -1);
    // courbe provisoire
    try
      FCourbePolygoneProvisoire.ClearVertex;
      // récupération du groupe et du style
      FIDGroupeEditing        := PO.IDGroupe;
      FIDStylePolygoneEditing := nopDEFAULT; // PO.IDStylePolygone;
      FCourbePolygoneProvisoire.LoadScrap(PO);   // récupération du polygone
      QIdx := Q;
      Result  := True;
    except
    end;
  end;
end;
function TCadreDessinBGRA.QRechercheEtTraceLigne(out QIdx: integer): boolean;
var
  LS: TSimpleLigne;
  q: Int64;
begin
  QIdx := -1;
  Result := false;
  if (0 = FDocumentDessin.GetNbSimpleLignes()) then Exit;
  q := FDocumentDessin.FindObject(FMyPos.X, FMyPos.Y, mseLIGNES);
  if (q >= 0) then
  begin
    SetCurrentLigneIdx(Q);
    LS := FDocumentDessin.GetSimpleLigne(q);
    FProcGetSimpleline(LS, Q);
    DrawVolatileSimpleLigne(Vue.Canvas, LS, Q, True, True);
    QIdx := Q;
    Result := true;
  end;
end;
//******************************************************************************
// Objets volatiles
procedure TCadreDessinBGRA.DrawVolatileScrap(const Cnv: TCanvas;
                                             const MyScrap: TScrap;
                                             const IdxScrap: Int64;
                                             const DoDrawSommets: boolean;
                                             const DoDrawBasePoints: boolean;
                                             const QIdxGroupe: integer);
const
  C = 4;
var
  i, NB: integer; // NB Points
  V: TVertexPolygon;
  PM: TPoint2Df;
  PTSV: array of TPoint;
  CP  : TPoint;  // point central du polygone
  //GP  : TGroupeEntites;
  errCode: integer;
  QStylePolygone: TStylePolygone;
  WU: Boolean;
  OO: TPoint3Df;
  BS1: TBaseStation;
begin
  if (Not FDoDraw) then Exit;
  if (Not (tedSCRAPS in FParamsVue2D.ElementsDrawn)) then Exit; //FDoDisplayScraps) then Exit;
  WU := (QIdxGroupe = -1) OR
        (QIdxGroupe = MyScrap.IDGroupe);
  if (not WU) then Exit;

  NB := High(MyScrap.Sommets);
  if (Nb < 1) then Exit;
  SetLength(PTSV, NB+1);
  Cnv.pen.Width := 0;
  Cnv.Pen.Color := clRed;
  Cnv.Brush.Color := SCRAP_COLOR;
  for i:=0 to NB do
  begin
    V := MyScrap.Sommets[i];
    FDocumentDessin.GetCoordsGCS(V.IDStation, MyScrap.IDGroupe, V.Offset, PM, errCode);
    if (ErrCode = -1) then Exit;
    PTSV[i] := GetCoordsPlan(PM);
  end;
  if (DoDrawSommets) then // dessiner les sommets
  begin
    Cnv.Pen.Style   := psSolid;
    Cnv.Brush.Color := clAqua;
    Cnv.Pen.Color   := clBlue;
    Cnv.Pen.Style   := psSolid;
    for i:= 0 to High(PTSV) do
    begin
      Cnv.Rectangle(PTSV[i].X - C, PTSV[i].Y - C,
                    PTSV[i].X + C, PTSV[i].Y + C);
    end;
    Cnv.Brush.Color := clAqua;
    Cnv.Pen.Color   := clGray;
    Cnv.Pen.Style   := psSolid;
    VolatileDrawRectangle(Cnv, MyScrap.BoundingBox.C1, MyScrap.BoundingBox.C2);
    VolatileTraceTexte(Cnv,
                       MyScrap.BoundingBox.C1.X, MyScrap.BoundingBox.C2.Y,
                       Format('[%d] %s', [IdxScrap, MyScrap.Nom]));
  end else
  begin
    Cnv.Polygon(PTSV);
  end;
  Cnv.Polyline(PTSV);
  SetLength(PTSV, 0);
  if (DoDrawBasePoints) then
  begin
    AfficherMessageErreur('DoDrawBasePoints');
    OO.Empty();
    for i:=0 to NB do
    begin
      V := MyScrap.Sommets[i];
      // tracé du premier basepoint
      if (FDocumentDessin.GetBasePointByIndex(V.IDStation, BS1)) then VolatileDrawBasePoint(Cnv, BS1);
    end;
    Cnv.Brush.Style := bsClear;
    Cnv.Brush.Color := clWhite;
  end;
end;
//------------------------------------------------------------------------------
procedure TCadreDessinBGRA.DrawVolatileCourbe(const Cnv: TCanvas;
                                              const MyCourbe: TCourbe;
                                              const IdxCourbe: Int64;
                                              const DoDrawPtsCtrls: boolean;
                                              const DoDrawBasePoints: boolean);
var
  QErrCode: integer;
  i, NB: integer; // NB Points
  AC    : TArcCourbe;
  AB    : TBezierArc;
  QStyleCourbe: TStyleCourbe;
  BS1, BS2: TBaseStation;
  OO: TPoint3Df;
  PM1: TPoint2Df;
  errCode: integer;
  QRM22: Integer;
  QC1: TPoint2Df;
  QC2: TPoint2Df;
  procedure GetBezierArc(const A: TArcCourbe; out QResult: TBezierArc; out errCode: integer);
  begin
    errCode := 0;
    try
      FDocumentDessin.GetCoordsGCS(A.IDStationP1, MyCourbe.IDGroupe, A.OffsetP1, QResult.PT1, errCode);
      if (ErrCode = -1) then Exit;
      QResult.Tgt1.setFrom(A.TangP1.X, A.TangP1.Y);
      FDocumentDessin.GetCoordsGCS(A.IDStationP2, MyCourbe.IDGroupe, A.OffsetP2, QResult.PT2, errCode);
      if (ErrCode = -1) then Exit;
      QResult.Tgt2.setFrom(A.TangP2.X, A.TangP2.Y);
      QResult.Pas    :=  QStyleCourbe.LongBarbules; // longueur d'un segment de courbe = longueur des barbules
    except
      errCode := -1;
    end;
  end;
  procedure QDrawBezierArc(const B: TBezierArc);
  var
    QArcBezier: array[0..3] of TPoint;
    PC1: TPoint2Df;
    PC2: TPoint2Df;
    toto: float;
  begin
    PC1.setFrom(B.PT1.X + B.Tgt1.X, B.PT1.Y + B.Tgt1.Y);
    PC2.setFrom(B.PT2.X + B.Tgt2.X, B.PT2.Y + B.Tgt2.Y);
    QArcBezier[0] := GetCoordsPlan(B.PT1);
    QArcBezier[1] := GetCoordsPlan(PC1);
    QArcBezier[2] := GetCoordsPlan(PC2);
    QArcBezier[3] := GetCoordsPlan(B.PT2);
    Cnv.PolyBezier(QArcBezier);
    if (DoDrawPtsCtrls) then // Afficher Points de contrôle ?
    begin
      // points de contrôle
      Cnv.Pen.Width := 0;
      Cnv.Pen.Color := clGray;
      VolatileTraceVers(Cnv, B.PT1.X, B.PT1.Y, False);
      VolatileTraceVers(Cnv, PC1.X, PC1.Y, True);
      VolatileTraceVers(Cnv, PC2.X, PC2.Y, True);
      VolatileTraceVers(Cnv, B.PT2.X, B.PT2.Y, True);
      Cnv.Brush.Color := clRed;
      toto := arctan2(PC1.Y - B.PT1.Y,
                      PC1.X - B.PT1.X);
      VolatileTraceCercle(Cnv, B.PT2.X, B.PT2.Y, 8);
      TraceMarqueurDirectionnel(Cnv, B.PT1.X, B.PT1.Y, toto);
      VolatileTraceCercle(Cnv, PC1.X, PC1.Y, 4);
      VolatileTraceCercle(Cnv, PC2.X, PC2.Y, 4);
    end;
  end;
begin
  try
    if (Not FDoDraw) then Exit;
    NB := High(MyCourbe.Arcs) + 1;
    if (Nb < 1) then Exit;
    Cnv.Pen.Style  := psSolid;
    Cnv.Pen.Color  := clRed;
    Cnv.Pen.Width  := 2;
    for i:=0 to NB - 1 do
    begin
      AC := MyCourbe.Arcs[i];
      GetBezierArc(AC, AB, QErrCode);
      if (QErrCode = 0) then QDrawBezierArc(AB)
                        else AfficherMessageErreur(Format('Erreur en dessin de la courbe #%d, arc: %d/%d', [IdxCourbe, i, Nb]));
    end;
    Cnv.Pen.Width  := 0;
    if (DoDrawPtsCtrls) then
    begin
      AfficherMessage(Format('Trace courbe ID = %d - Arcs = %d',[IdxCourbe, NB]));
      AfficherMessage(Format('BB = %%f %f %f %f ',[MyCourbe.BoundingBox.C1.X , MyCourbe.BoundingBox.C1.Y,
                                                   MyCourbe.BoundingBox.C2.X, MyCourbe.BoundingBox.C2.Y]));
      Cnv.Pen.Color   := clGray;
      Cnv.Pen.Width   := 0;
      Cnv.Brush.Color := clYellow;
      VolatileDrawRectangle(Cnv, MyCourbe.BoundingBox.C1, MyCourbe.BoundingBox.C2);
      VolatileTraceTexte(Cnv,
                         MyCourbe.BoundingBox.C1.X, MyCourbe.BoundingBox.C2.Y,
                         IntToStr(IdxCourbe));
    end;
    if (DoDrawBasePoints) then
    begin
      AfficherMessageErreur('DoDrawBasePoints');
      OO.Empty();
      for i:=0 to NB - 1 do
      begin
        AC := MyCourbe.Arcs[i];
        // tracé du premier basepoint
        if (i = 0) then
        begin
          if (FDocumentDessin.GetBasePointByIndex(AC.IDStationP1, BS1)) then VolatileDrawBasePoint(Cnv, BS1);
        end;
        if (FDocumentDessin.GetBasePointByIndex(AC.IDStationP2, BS2)) then VolatileDrawBasePoint(Cnv, BS2);
      end;
      Cnv.Brush.Style := bsClear;
      Cnv.Brush.Color := clWhite;
    end;
  except
    AfficherMessageErreur(Format('Erreur en dessin de la courbe #%d', [IdxCourbe]));
  end;
end;
//------------------------------------------------------------------------------
procedure TCadreDessinBGRA.DrawVolatilePolygone(const Cnv: TCanvas;
                                                const MyPolygon: TPolygone;
                                                const IdxPolygone: Int64;
                                                const DoDrawSommets: boolean;
                                                const DoDrawBasePoints: boolean;
                                                const QIdxGroupe: integer);
const
  C = 4;
var
  i, NB: integer; // NB Points
  V: TVertexPolygon;
  PM: TPoint2Df;
  PTSV: array of TPoint;
  CP  : TPoint;  // point central du polygone
  //GP  : TGroupeEntites;
  errCode: integer;
  QStylePolygone: TStylePolygone;
  WU: Boolean;
  OO: TPoint3Df;
  BS1: TBaseStation;
begin
  if Not (FDoDraw) then Exit;
  WU := (QIdxGroupe = -1) OR
        (QIdxGroupe = MyPolygon.IDGroupe);
  if (not WU) then Exit;

  NB := High(MyPolygon.Sommets);
  if (Nb < 1) then Exit;
  SetLength(PTSV, NB+1);
  Cnv.Pen.Color := clBlack;
  Cnv.Pen.Width := 2;
  Cnv.Brush.Style := bsClear;
  for i:=0 to NB do
  begin
    V := MyPolygon.Sommets[i];
    FDocumentDessin.GetCoordsGCS(V.IDStation, MyPolygon.IDGroupe, V.Offset, PM, errCode);
    if (ErrCode = -1) then Exit;
    PTSV[i] := GetCoordsPlan(PM);
  end;
  if (DoDrawSommets) then // dessiner les sommets
  begin
    Cnv.Pen.Style   := psSolid;
    Cnv.Brush.Color := clAqua;
    Cnv.Pen.Color := clBlue;
    for i:= 0 to High(PTSV) do
    begin
      Cnv.Rectangle(PTSV[i].X - C, PTSV[i].Y - C,
                    PTSV[i].X + C, PTSV[i].Y + C);
    end;
    Cnv.Brush.Color := clAqua;
    Cnv.Pen.Color   := clGray;
    Cnv.Pen.Style   := psSolid;
    VolatileDrawRectangle(Cnv, MyPolygon.BoundingBox.C1, MyPolygon.BoundingBox.C2);
    VolatileTraceTexte(Cnv,
                       MyPolygon.BoundingBox.C1.X, MyPolygon.BoundingBox.C2.Y,
                       IntToStr(IdxPolygone));
  end else
  begin
    Cnv.Polygon(PTSV);
    // si le polygone est un parcours, dessiner le contour
    if (MyPolygon.IDStylePolygone = nopCHEMINS) then  Cnv.Pen.Style   := psSolid;
  end;
  Cnv.Polyline(PTSV);
  SetLength(PTSV, 0);
  if (DoDrawBasePoints) then
  begin
    AfficherMessageErreur('DoDrawBasePoints');
    OO.Empty();
    for i:=0 to NB do
    begin
      V := MyPolygon.Sommets[i];
      // tracé du premier basepoint
      if (FDocumentDessin.GetBasePointByIndex(V.IDStation, BS1)) then  VolatileDrawBasePoint(Cnv, BS1);
    end;
    Cnv.Brush.Style := bsClear;
    Cnv.Brush.Color := clWhite;
  end;
end;
//------------------------------------------------------------------------------
procedure TCadreDessinBGRA.DrawVolatilePolyligne(const Cnv: TCanvas;
                                                 const MyPolyLine: TPolyLigne;
                                                 const IdxPolygone: Int64;
                                                 const DoDrawSommets: boolean;
                                                 const DoDrawBasePoints: boolean;
                                                 const QIdxGroupe: integer);
const C = 4;
var
  i, NB         : integer; // NB Points
  errCode       : integer;
  PTSV          : array of TPoint;
  BS1           : TBaseStation;
  V0, V1, V     : TVertexPolygon;
  WU            : Boolean;
  PM0, PM1      : TPoint2Df;
  PM, PMMedian  : TPoint2Df;
  toto          : double;
begin
  if (Not FDoDraw) then Exit;
  WU := (QIdxGroupe = -1) OR
        (QIdxGroupe = MyPolyLine.IDGroupe);
  if (not WU) then Exit;

  NB := High(MyPolyLine.Sommets);
  if (Nb < 1) then Exit;
  SetLength(PTSV, NB+1);

  for i:=0 to NB do
  begin
    V := MyPolyLine.Sommets[i];
    FDocumentDessin.GetCoordsGCS(V.IDStation, MyPolyLine.IDGroupe, V.Offset, PM, errCode);
    if (ErrCode = -1) then Exit;
    PTSV[i] := GetCoordsPlan(PM);
  end;
  if (DoDrawSommets) then // dessiner les sommets
  begin
    Cnv.Pen.Style   := psSolid;
    Cnv.Brush.Color := clAqua;
    Cnv.Pen.Color := clBlue;
    for i:= 0 to High(PTSV) do
    begin
      Cnv.Rectangle(PTSV[i].X - C, PTSV[i].Y - C,
                    PTSV[i].X + C, PTSV[i].Y + C);
    end;
    Cnv.Brush.Color := clAqua;
    Cnv.Pen.Color   := clGray;
    Cnv.Pen.Style   := psSolid;
    VolatileDrawRectangle(Cnv, MyPolyLine.BoundingBox.C1, MyPolyLine.BoundingBox.C2);
    VolatileTraceTexte(Cnv,
                       MyPolyLine.BoundingBox.C1.X, MyPolyLine.BoundingBox.C2.Y,
                       IntToStr(IdxPolygone));
  end;
  Cnv.Pen.Color := clRed;
  Cnv.Pen.Width := 2;
  Cnv.Polyline(PTSV);
  if (MyPolyLine.Closed) then
  begin
    Cnv.Brush.Color := clSilver;
    Cnv.Polygon(PTSV);
  end;
  // les marqueurs
  Cnv.Brush.Color := clRed;
  Cnv.Pen.Color   := clRed;
  Cnv.Pen.Width   := 0;
  for i := 1 to NB do
  begin
    V0 := MyPolyLine.Sommets[i-1];
    V1 := MyPolyLine.Sommets[i];

    FDocumentDessin.GetCoordsGCS(V0.IDStation, MyPolyLine.IDGroupe, V0.Offset, PM0, errCode);
    FDocumentDessin.GetCoordsGCS(V1.IDStation, MyPolyLine.IDGroupe, V1.Offset, PM1, errCode);
    PMMedian.setFrom(0.50*(PM0.X + PM1.X), 0.50*(PM0.Y + PM1.Y));

    toto := arctan2(PM1.Y - PM0.Y, PM1.X - PM0.X);
    TraceMarqueurDirectionnel(Cnv, PMMedian.X, PMMedian.Y, toto);
  end;
  SetLength(PTSV, 0);
  if (DoDrawBasePoints) then
  begin
    AfficherMessageErreur('DoDrawBasePoints');
    for i:=0 to NB do
    begin
      V := MyPolyLine.Sommets[i];
      // tracé du premier basepoint
      if (FDocumentDessin.GetBasePointByIndex(V.IDStation, BS1)) then VolatileDrawBasePoint(Cnv, BS1);
    end;
    Cnv.Brush.Style := bsClear;
    Cnv.Brush.Color := clWhite;
    Cnv.Font.Color := clBlack;
  end;
end;
//------------------------------------------------------------------------------
procedure TCadreDessinBGRA.VolatileDrawRectangle(const Cnv: TCanvas;
                                                 const C1, C2: TPoint2Df);
var
  PP1: TPOINT;
  PP2: TPOINT;
begin
  PP1 := GetCoordsPlan(C1);
  PP2 := GetCoordsPlan(C2);
  Cnv.MoveTo(PP1.X, PP1.Y);
  Cnv.LineTo(PP2.X, PP1.Y);
  Cnv.LineTo(PP2.X, PP2.Y);
  Cnv.LineTo(PP1.X, PP2.Y);
  Cnv.LineTo(PP1.X, PP1.Y);
end;
//------------------------------------------------------------------------------
procedure TCadreDessinBGRA.DrawVolatileSimpleLigne(const Cnv: TCanvas;
                                                   const SL: TSimpleLigne;
                                                   const Index: integer;
                                                   const DoDrawHands: boolean;
                                                   const DoDrawBasePoints: boolean);
const
  RR = 3;
var
  E1, E2: TPoint2Df;
  PP: TPoint;
  errCode: integer;
  BS1, BS2: TBaseStation;
begin
  Cnv.pen.Width := 0;
  Cnv.Pen.Color := clRed;
  FDocumentDessin.GetCoordsGCS(SL.IDBaseStExt1, SL.IDGroupe, SL.OffsetExtr1, E1, errCode);
  if (ErrCode = -1) then Exit;
  FDocumentDessin.GetCoordsGCS(SL.IDBaseStExt2, SL.IDGroupe, SL.OffsetExtr2, E2, errCode);
  if (ErrCode = -1) then Exit;
  VolatileTraceVers(Cnv, E1.X, E1.Y, False);
  VolatileTraceVers(Cnv, E2.X, E2.Y, True);
  // dessin des poignées
  if (DoDrawHands) then
  begin
    Cnv.Brush.Color := clRed;
    Cnv.Pen.Color := clRed;
    PP := GetCoordsPlan(E1);
    Cnv.Ellipse(PP.X - RR, PP.Y - RR, PP.X + RR, PP.Y + RR);
    PP := GetCoordsPlan(E2);
    Cnv.Ellipse(PP.X - RR, PP.Y - RR, PP.X + RR, PP.Y + RR);
  end;
  if (DoDrawBasePoints) then
  begin
    AfficherMessageErreur('DoDrawBasePoints');
    if (FDocumentDessin.GetBasePointByIndex(SL.IDBaseStExt1, BS1) and
        FDocumentDessin.GetBasePointByIndex(SL.IDBaseStExt2, BS2)) then
    begin
      VolatileDrawBasePoint(Cnv, BS1);
      VolatileDrawBasePoint(Cnv, BS2);
    end;
    Cnv.Brush.Style := bsClear;
    Cnv.Brush.Color := clWhite;
    Cnv.Font.Color := clBlack;
  end;
end;

procedure TCadreDessinBGRA.DrawVolatileSymbole(const Cnv: TCanvas; const SL: TSymbole; const Index: integer; const DoDrawHands: boolean; const DoDrawBasePoints: boolean);
var
  BS1: TBaseStation;
begin
  if (DoDrawBasePoints) then
  begin
    if (FDocumentDessin.GetBasePointByIndex(SL.IDBaseStation, BS1)) then VolatileDrawBasePoint(Cnv, BS1);
    Cnv.Brush.Style := bsClear;
    Cnv.Brush.Color := clWhite;
    Cnv.Font.Color := clBlack;
  end;
end;

procedure TCadreDessinBGRA.DrawVolatileTexte(const Cnv: TCanvas; const TT: TTextObject; const Index: integer; const DoDrawHands: boolean; const DoDrawBasePoints: boolean);
var
  BS1: TBaseStation;
begin
  if (DoDrawBasePoints) then
  begin
    if (FDocumentDessin.GetBasePointByIndex(TT.IDBaseSt, BS1)) then VolatileDrawBasePoint(Cnv, BS1);
    Cnv.Brush.Style := bsClear;
    Cnv.Brush.Color := clWhite;
    Cnv.Font.Color := clBlack;
  end;
end;

function TCadreDessinBGRA.CalcFontHeightFromHauteurEnMetres(const Hauteur: double): integer;
var
  c1      : TPoint3Df;
  PP1, PP2: TPoint;
begin
  try
    c1 := FDocumentDessin.GetCoordsMini();
    PP1 := GetCoordsPlan(c1.X, c1.Y);
    PP2 := GetCoordsPlan(c1.X, c1.Y - Hauteur);
    Result := PP2.Y - PP1.Y;
  except
    Result := 6;
  end;
end;
// destruction des derniers objets
procedure TCadreDessinBGRA.DeleteLastObject(const M: TModeSelectionEntites);
var
  LastIdx: Integer;
  EWE: String;
begin
  if (not FDoDraw) then Exit;
  // index du dernier élément
  case M of
    mseSCRAPS         : LastIdx := FDocumentDessin.GetNbScraps() - 1;
    mseCOURBES        : LastIdx := FDocumentDessin.GetNbCourbes() - 1;
    msePOLYLIGNES     : LastIdx := FDocumentDessin.GetNbPolylignes() - 1;
    msePOLYGONES      : LastIdx := FDocumentDessin.GetNbPolygones() - 1;
    mseLIGNES         : LastIdx := FDocumentDessin.GetNbSimpleLignes() - 1;
    mseSYMBOLES       : LastIdx := FDocumentDessin.GetNbSymboles() - 1;
    mseTEXTES         : LastIdx := FDocumentDessin.GetNbScraps() - 1;
  else
    pass;
  end;
  // message de confirmation
  case M of
    mseSCRAPS         : EWE := rsMSG_DELETE_SCRAP;
    mseCOURBES        : EWE := rsMSG_DELETE_COURBE;
    msePOLYLIGNES     : EWE := rsMSG_DELETE_POLYLIGNE;
    msePOLYGONES      : EWE := rsMSG_DELETE_POLYGONE;
    mseLIGNES         : EWE := rsMSG_DELETE_LINE;
    mseSYMBOLES       : EWE := rsMSG_DELETE_SYMBOLE;
    mseTEXTES         : EWE := rsMSG_DELETE_TEXTE;
  else
    pass;
  end;
  if (Not QuestionOuiNon(Format(EWE, [LastIdx]))) then Exit;
  // effacement
  case M of
    mseSCRAPS         : FCurrentScrapIdx      := FDocumentDessin.RemoveLastObject(M);
    mseCOURBES        : FCurrentCurveIdx      := FDocumentDessin.RemoveLastObject(M);
    msePOLYLIGNES     : FCurrentPolylineIdx   := FDocumentDessin.RemoveLastObject(M);
    msePOLYGONES      : FCurrentPolygonIdx    := FDocumentDessin.RemoveLastObject(M);
    mseLIGNES         : FCurrentSimpleLineIdx := FDocumentDessin.RemoveLastObject(M);
    mseSYMBOLES       : FCurrentSymboleIdx    := FDocumentDessin.RemoveLastObject(M);
    mseTEXTES         : FCurrentTextIdx       := FDocumentDessin.RemoveLastObject(M);
  else
    pass;
  end;
  Vue.Invalidate;
end;




function TCadreDessinBGRA.GetTexturesPolygones(): TTexturesPolygonObject;
begin
  Result := FTexturesPolygones;
end;


// getter idx objets
function TCadreDessinBGRA.GetCurrCourbeIdx(): integer;
begin
  Result := FCurrentCurveIdx;
end;
function TCadreDessinBGRA.GetCurrPolygoneIdx(): integer;
begin
  Result := FCurrentPolygonIdx;
end;

function TCadreDessinBGRA.GetCurrPolylineIdx(): integer;
begin
  Result := FCurrentPolylineIdx;
end;

function TCadreDessinBGRA.GetCurrScrapIdx(): integer;
begin
  Result := FCurrentScrapIdx;
end;

function TCadreDessinBGRA.GetCurrSimpleLineIdx(): integer;
begin
  Result := FCurrentSimpleLineIdx;
end;

function TCadreDessinBGRA.GetCurrSymboleIdx(): integer;
begin
  Result := FCurrentSymboleIdx;
end;

function TCadreDessinBGRA.GetCurrTextIdx(): integer;
begin
  Result := FCurrentTextIdx;
end;
end.


//------------------------------------------------------------------------------

