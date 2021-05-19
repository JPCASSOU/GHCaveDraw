// 29/04/2013: Ajout du support des super-groupes
// 10/06/2013: ID de terrain des stations pour TBaseStation
// 26/06/2015: Support des Scraps
// 30/09/2015: Support des Images

{$INCLUDE CompilationParameters.inc}

unit GHCD_Types;
interface
uses
  Classes,
  Graphics,
  BGRABitmap;

const INSTRUCTION_INCLUDE_GCP = '$INCLUDE_GCP:';

const NB_MAX_OBJ_PAR_CAVITE = 1000000;            // nb max d'objets par table et par cavité
const MULT_IDX_CAVITE       = 1000000000000;      // facteur de positionnement de l'Idx d'une cavité

const FMT_DIRECTION_FAILLE  = 'N%.3d°';


const MULTIPLICATEUR_SERIE   = 100000; //

const MULTIPLICATEUR_STATION = 10;
const DIV_SR_CENTERLINE = MULTIPLICATEUR_SERIE; // pour décomposition en série station d'un TIDBasestation
const DIV_SR_ANTENNE    = MULTIPLICATEUR_SERIE; // * 100; // pour décomposition en série station d'un TIDBasestation


//const MENTIONS_COPYRIGHT = '(c) ARSMA 2003 .. 2016';
const MENTION_CC_BY_SA   = 'CC-BY-SA Creative Commons';

const INFINITE = 1E20;

const SCRAP_COLOR = TColor($D0D0D0);
// seuil de fusion de deux vertex
const DIST_MERGE_VERTEX = 0.04;
const QDR_MIN_SPACING = 10.00; // espacement de grille minimal;

const PI_2            = PI/2;
const PI_4            = PI/4;

const PI_180          = PI/180;
const INV_PI_180      = 180/PI;



const MAX_SIZE_PARAM_ARRAY = 128;
const DEFAULT_FONT_NAME = 'Arial';
// constantes d'erreur génériques
type TErrorCodeGeneric = (errNONE, errBUSY, errOK, errWARNING, errERROR, errFATAL);

// noms de fichiers
type TStringDirectoryFileName = type UTF8String;

// mode d'export vers logiciels de SIG
type TWithExportGIS = set of (gisWITH_ENTRANCES, gisWITH_CENTERLINES, gisWITH_POI);
// liste des styles d'objets
type TLesStylesObjets = (sdoCURVES, sdoLINES, sdoPOLYGONS, sdoTEXTES);


type TCourbesOrPolylignes = (tcpCOURBE, tcpPOLYLIGNE);
type TTypeQuadrillage = (tqGRID, tqCROSS);
type TELementsDrawn = set of (tedECHELLE_NORD            // dessin de l'échelle et du nord
                             ,tedCENTERLINES                    // dessin des centerlines
                             ,tedIDSTATIONS                   // dessiner les stations
                             ,tedQUADRILLES                     // dessiner les quadrillages
                             ,tedDEBUG_TEXTS                    // afficher les textes de débug.
                             ,tedTEXTES                         // afficher les textes du dessin
                             ,tedCADRES_PHOTO                 // afficher les cadres des photos
                             ,tedSCRAPS                         // afficher les scraps
                             ,tedDETAILS                        // détails
                             ,tedBBXGroupes         // bounding box groupes
                             ,tedDISP_PENTES        // pentes supérieures à une limite donnée
                             ,tedIMAGES             // images (typiquement des fonds de cartes)
                       );



// modes d'édition des dialogues de groupes, objets, etc ..
type TModeEdition = (medCREATION, medMODIF);
// gestion des zooms
const MAX_ARRAY_ZOOMS = 15;
type TZoomParameters = record
  ID      : integer;
  Caption : string;
  X1      : double;
  Y1      : double;
  X2      : double;
  Y2      : double;
end;

// rectangle 2D
type TRect2Df = record
  X1, Y1: double;
  X2, Y2: double;
end;
// couple série station au format TOPOROBOT + ID de terrain
type TToporobotIDStation = record
  aSerie: integer;
  aStation: integer;
  aIDTerrain: string;
end;
// barbules
// TODO: Procédure d'ajout d'un nouveau type d'objet: Préciser ici le nouvel objet
type TBarbule = (tbNONE,
                 tbRESSAUT,
                 tbSURPLOMB,
                 tbCHENAL_VOUTE,
                 tbMINI_RESSAUT);

// tableaux de strings

// TStringArray est désormais un type de données de la FCL >3.0.2 . Remplacé par TGHStringArray
type TGHStringArray   = array[0..MAX_SIZE_PARAM_ARRAY] of string;
type TGHShortStringArray = array[0 .. 7] of string;

{$IFDEF TIDBASEPOINT_AS_TEXT}
type TIDBaseStation = string;
{$ELSE}
type TIDBaseStation = type Int64;
{$ENDIF TIDBASEPOINT_AS_TEXT}
type TTypeStation   = byte; //(tsENTRANCE, tsFIXPOINT, tsSHOT);
type TPoint3Df = record
  X: double;
  Y: double;
  Z: double;
end;
type TPoint2Df = record
  X: double;
  Y: double;
end;
type TLstSommets = array of TPoint2Df;
type TDroite = record
  PT1: TPoint2Df;
  PT2: TPoint2Df;
end;
type TArrayIdxObjets = array of Int64;

// tableau de points 2D
//type TArrayPt2Df = array of TPoint2Df;
type TArrayPoints2Df = array of TPoint2Df;
// boites englobantes
type TBoundingBox = record
  C1 : TPoint2Df;
  C2 : TPoint2Df;
end;


// pour le support du travail collaboratif
// TODO: Le dessinateur est un attribut d'objet ou de groupe ? A définir
type TIDDessinateur = type integer;
type TDessinateur = record
  IDDessinateur: TIDDessinateur;
  Nom          : string;
end;


// groupes de points === parties de réseaus,
// utilisés notamment pour l'extractions de parties de réseaux
type TIDGroupeEntites     = type Int64;
type TIDSuperGroupe       = type Int64;
type TArrayOfIdxGroupes = array of TIDGroupeEntites;
// super-groupes === réseaux

type TSuperGroupe = record
  NomSuperGroupe: string;
  Displayed     : boolean;
  //Locked        : boolean;
  //Decalage      : TPoint3Df;
  ListeGroupes  : TArrayOfIdxGroupes;
end;
type TGroupeEntites   = record
  IDGroupeEntites : TIDGroupeEntites;
  // IDSuperGroupeEntites;   /!\ NON ! Un groupe peut appartenir à plusieurs super-groupes
  NomGroupe       : string;
  CouleurGroupe   : TColor;
  Decalage        : TPoint3Df; // décalage à appliquer aux coordonnées des points du groupe
  DecalageActif   : boolean;   // activer ce décalage
  Visible         : boolean;
  BoundingBox     : TBoundingBox;
  DateLastModif   : TDateTime;      // date de modification
  ZOrder          : double;
  Filtres         : string;
  NombreObjets    : integer;
end;


// stations calculées par GHTopo = station de référence, indexées par les entités
type TBaseStation = record
  IDStation      : TIDBaseStation;
  IDTerrain      : string; // étiquette de terrain
  TypeStation    : TTypeStation;
  Couleur        : TColor;
  PosExtr0       : TPoint3Df; // départ visée
  PosStation     : TPoint3Df; // coordonnées de référence
  PosPG          : TPoint3Df; //
  PosPD          : TPoint3Df;
  Enabled        : boolean; // activé par le MétaFiltre
end;

// Infos générales des sections transversales
type TGPInfosCrossSection = record
  IDSection      : Int64;
  IDGroupe       : TIDGroupeEntites;
  IDBaseStation  : TIDBaseStation;
  Caption        : string;
end;
// styles d'objets
// styles de remplissage
type TMotif = array[0..7, 0..7] of byte; // motif 8x8 de remplissage
type TStyleFill = record
  IDFillStyle   : INT64;
  NameSVGStyle  : string;
  FillColor     : TColor;
  Motif         : TMotif;
end;
// Facteur d'échelle, exprimé sous forme décimale (eg: Ech 1/1000 -> facteur de 0.001
type TFacteurEchelle  = double;
// Seuil d'échelle au-dessus duquel l'objet est tracé
// A la dimension d'un facteur d'échelle
type TSeuilVisibilite = TFacteurEchelle;

const IDX_STYLE_COURBE_PAROIS = 1;


type TStyleCourbe = record // pour lignes, polylignes et courbes
  IDStyle     : integer;
  DescStyle   : string;
  NameSVGStyle: string;
  SeuilVisibilite: TSeuilVisibilite;

  LineWidth   : integer;
  PrintLineWidth: double;
  LineColor   : TColor;
  LineStyle   : TPenStyle;
  Smooth      : boolean;
  Barbules    : TBarbule;
  LongBarbules: double;
end;

type TStyleLigne = record // pour  lignes
  IDStyle     : integer;
  DescStyle   : string;
  NameSVGStyle: string;
  SeuilVisibilite: TSeuilVisibilite;

  LineWidth   : integer;
  PrintLineWidth: double;
  LineColor   : TColor;
  LineStyle   : TPenStyle;
end;
//type TPolygonStyle = set of (psFILLED, psCONTOURED, psSMOOTH);
type TStylePolygone = record // pour blocs, concrétions massives
  IDStyle     : integer;
  DescStyle   : string;
  NameSVGStyle: string;
  SeuilVisibilite: TSeuilVisibilite;
  LineWidth   : double;
  LineColor   : TColor;
  LineStyle   : TPenStyle;
  FillColor   : TColor;
  Style       : byte;
end;

type TStyleTexte = record // styles de texte
  IDStyle     : integer;
  DescStyle   : string;
  NameSVGStyle: string;
  SeuilVisibilite: TSeuilVisibilite;

  FontName    : string;
  FontPrnHeight: double; // en mm
  FontColor   : TColor;
  FontStyle   : TFontStyles; // gras, etc ...
end;

// styles d'objets ponctuels
type TStyleSymboles = record
  IDStyle     : integer;
  DescStyle   : string;
  NameSVGStyle: string;
  SeuilVisibilite: TSeuilVisibilite;
  Color       : TColor;
end;


// sinusoide pour représentation des arrivées d'eau
type TSinusoide = array[0..9] of TPoint2Df;
// échelle de couleurs
type TArrayColors = array of TColor;
// type de point de courbe
type TVertexCourbe = record
  IDStation      : TIDBaseStation;
  Offset         : TPoint3Df;
  TangGauche     : TPoint3Df;
  TangDroite     : TPoint3Df;
end;
type TCrossSectionVertexCourbe = record
  Position       : TPoint2Df;
  TangGauche     : TPoint2Df;
  TangDroite     : TPoint2Df;
end;
type TArrayVertexCourbe = array of TVertexCourbe;
// type de point de courbe
type TVertexPolygon = record
  IDStation      : TIDBaseStation;
  Offset         : TPoint3Df;
end;
type TVertexPolygonArray = array of TVertexPolygon;
//*)
// type d'arc de courbe
// GHCaveDraw individualise les arcs de courbes
type TArcCourbe = record
  IDStationP1      : TIDBaseStation;
  //IDGroupe         : Int64;
  OffsetP1         : TPoint3Df;
  TangP1           : TPoint3Df; // tangente = dx, dy. C'est un vecteur, pas une position
  IDStationP2      : TIDBaseStation;
  OffsetP2         : TPoint3Df;
  TangP2           : TPoint3Df;
end;
type TArcsCourbesArray = array of TArcCourbe;

// mode de sélection de la nature des objets (parois, ...)
type TNatureObjetCourbe   = (nocDEFAULT, nocPAROI, nocPAROIS_CACHEE,
                             nocECOULEMENT, nocLIGNES_PENTE,
                             nocRESSAUT, nocSURPLOMB,
                             nocCHENAL_VOUTE, nocMARCHE,
                             nocPAROI_INCERTAINE,
                             nocMUR_MACONNE,
                             nocPAROI_FRACASSEE
                             );
type TNatureObjetLigne    = (nolDEFAULT, nolFLECHE, nolSUITE_RESEAU, nolFRACTURE, nolPENTE, nolPENDAGE);
type TNatureObjetPolygone = (nopDEFAULT, nopLAC,
                             nopARGILE, nopSABLE, nopEBOULIS, nopGALETS, nopNEIGE,
                             nopSILHOUETTE, nopGROS_BLOC, nopGOUR, nopSIPHON,
                             nopARGILES_GALETS, nopCHEMINS,
                             nopMASQUES,
                             nopSCRAP
                             );
type TNatureObjetSymbole  = (nosPHOTO, nosENTREE,
                             nosPOINT_TOPO, nosPOINT_FIXE, nosCORRESPONDANCE,
                             nosFISTULEUSE, nosCONCRETION_PAROI, nosEXCENTRIQUES,
                             nosSTALACTITES, nosCOLONNES, nosSTALAGMITES,
                             nosCRISTAUX, nosFRACTURES,
                             nosCUPULES, nosZEFF, nosARRIVEE_EAU, nosPERTE,
                             nosDESOB, nosDANGER,
                             nosGOUFFRE_SURFACE, nosGROTTE_SURFACE, nosPOINT_REMARQUABLE);
type TNatureObjetTexte    = (notDEBUG,
                             notTITRES, notSOUS_TITRES, notCOTATION,
                             notTEXTE1, notTEXTE2, notLIEU_DIT, notCOTATION_EXTERIEURE);


// TODO: Voir si on peut chaîner plusieurs courbes
type
  pTCourbe = ^TCourbe;
  TCourbe = record
  IDGroupe       : TIDGroupeEntites;
  IDStyleCourbe  : TNatureObjetCourbe;//integer; //TStyleCourbe;
  BoundingBox    : TBoundingBox;
  Arcs           : TArcsCourbesArray;
  LastModified   : TDateTime;      // date de modification
  Closed         : boolean;
  MarkToDelete   : boolean;

end;
type TArrayOfTCourbe = array of TCourbe;

type TScrap = record
  IDGroupe       : TIDGroupeEntites;
  Nom            : string;
  Couleur        : TColor;
  Opacite        : byte; // 0..255
  Area           : double;
  Perimeter      : double;
  BoundingBox    : TBoundingBox;
  Sommets        : TVertexPolygonArray;
  LastModified   : TDateTime;      // date de modification
  MarkToDelete   : boolean;
end;
type TPolygone = record
  IDGroupe       : TIDGroupeEntites;
  IDStylePolygone: TNatureObjetPolygone; //integer;
  BoundingBox    : TBoundingBox;
  Area           : double;
  Perimeter      : double;
  Sommets        : TVertexPolygonArray;
  LastModified   : TDateTime;      // date de modification
  MarkToDelete   : boolean;
end;
type TArrayOfTPolygone = array of TPolygone;
// Nouveauté 2015: Polyligne (notamment pour les plans de carrières souterraines)
// Une polyligne est une restriction de la Courbe
type TPolyLigne = record
  IDGroupe       : TIDGroupeEntites;
  IDStylePolyLine: TNatureObjetCourbe;
  BoundingBox    : TBoundingBox;
  Area           : double;
  Perimeter      : double;
  Sommets        : TVertexPolygonArray;
  LastModified   : TDateTime;      // date de modification
  Closed         : boolean;
  MarkToDelete   : boolean;
end;
type TArrayOfTPolyligne = array of TPolyLigne;

type TSymbole = record
  //ID             : int64;
  IDGroupe       : TIDGroupeEntites;
  TypeObject     : TNatureObjetSymbole; //byte; //TTypePonctualObject;

  Couleur        : TColor;
  IDBaseStation  : TIDBaseStation; // station topo de référence
  Offset         : TPoint3Df;      // décalage
  AngleRot       : double;         // angle de rotation
  ScaleX,
  ScaleY         : double;         // échelle X, Y ; le symbole étant dans un carré unitaire
  TagTexte       : string;         // texte additionnel ou nom de fichier (photo)
  UnTag          : integer;        // pour utilisation selon le type d'objet
                                   // ex: Alignement d'une photo
  PhotoDisplayed : boolean;        // pour affichage de photos
  LastModified   : TDateTime;      // date de modification
  MarkToDelete   : boolean;
end;

// objets Lignes Simples
type TExtremiteLigne = byte; // flèche,

type TSimpleLigne = record
  IDGroupe      : TIDGroupeEntites;
  IDStyleLigne  : TNatureObjetLigne; //integer;
  BoundingBox    : TBoundingBox;

  IDBaseStExt1,
  IDBaseStExt2   : TIDBaseStation;
  OffsetExtr1,
  OffsetExtr2    : TPoint3Df;
  ExtrLin1,
  ExtrLin2       : TExtremiteLigne; // byte
  LastModified   : TDateTime;      // date de modification
  MarkToDelete   : boolean;
end;
// textes
type TTextObject = record
  IDStyleTexte  : TNatureObjetTexte; //integer;
  IDGroupe      : TIDGroupeEntites;
  IDBaseSt      : TIDBaseStation;
  Offset        : TPoint3Df;
  Alignment     : byte;
  Text          : string;
  MaxLength     : integer;
  LastModified   : TDateTime;      // date de modification
  MarkToDelete   : boolean;
end;



// arcs de Bézier
type TBezierArc = record
  PT1    : TPoint2Df; // premier point
  Tgt1   : TPoint2Df; // tangente
  PT2    : TPoint2Df; // second point
  Tgt2   : TPoint2Df; // tangente
  Pas    : double;    // pas de t en m

end;
// objets images pour le layer d'aides au dessin
// ne pas utiliser ce layer pour inclure des photos, qui sont du ressort de TSymbole
type TImageObject = record
  SrcFilename     : string;
  IsReady         : boolean;
  ConteneurImg    : TBGRABitmap;
  Displayed       : boolean;
  PositionCoinsImage: TRect2Df; // position des coins opposés de l'image
  Opacite         : byte;
  Description     : string;
end;



// contexte pour la grille de dessin

type TParamsVue2D = record
  BackGroundColor: TColor;
  ElementsDrawn     : TELementsDrawn;
  PenteLimiteDisp      : double;
  TailleEchelle        : double;
  DoDrawScrapsMonochromes: boolean;        // dessiner les scraps en monochrome transparent?
  ColorScrapMonochrome   : TColor;
  GrdTypeQuadrillage: TTypeQuadrillage;
  GrdCrossSize      : double;
  GrdMainGridColor  : TColor;     // grille principale
  GrdSecGridColor   : TColor;     // grille secondaire
  GrdSpcMainGrid    : double;     // espacement grille ppale
  GrdSpcSecGrid     : double;     // espacement grille sec
  GrdDispMainGrid   : boolean;     // afficher grille ppale
  GrdDispSecGrid    : boolean;     // afficher grille sec
  GrdDoDrawCoords   : boolean;
  GroupeBackColor   : TColor;      // couleur de fond des
  RegleDisplayed    : boolean;
  DoListerPOI    : boolean;
end;
  // mode de sélection des entités
type TModeSelectionEntites = (mseNONE,
                              mseSCRAPS,
                              mseCOURBES,
                              msePOLYLIGNES,
                              msePOLYGONES,
                              mseLIGNES,
                              mseSYMBOLES,
                              mseTEXTES,
                              mseIMAGES
                             );
type TTypeObjet = (tobjSCRAP,
                   tobjCOURBE,
                   tobjPOLYLIGNE,
                   tobjPOLYGONE,
                   tobjLIGNE,
                   tobjSYMBOLE,
                   tobjTEXTE,
                   tobjNONE,
                   tobjUNKNOWN);

type TRetourTypeObjet = (rtoNOT_FOUND,
                         rtoSCRAP,
                         rtoCOURBE,
                         rtoPOLYLIGNE,
                         rtoPOLYGONE,
                         rtoLIGNE,
                         rtoSYMBOLE,
                         rtoTEXTE);


// fonctions de rappel
type TProcedureWithStringOfObject = procedure(const S: string) of object;
type TProcAbortQuestionON     = function(): boolean of Object;
type TProcAfficherProgression = procedure(const Etape: string; const Min, Max, Position: integer) of object;

type TProcActualiserOnglet = procedure(const MyFiltre: string) of object;
type TProcGetInfoBasePoint = procedure(const B: TBaseStation) of object;
type TProcGetScrap         = procedure(const S: TScrap;           const IdxScrap: Int64) of object;
type TProcGetCourbe        = procedure(const C: TCourbe;          const IdxCourbe: Int64) of object;
type TProcGetPolyLigne     = procedure(const P: TPolyLigne;       const IdxPolygone: Int64) of object;


type TProcGetPolygon       = procedure(const P: TPolygone;        const IdxPolygone: Int64) of object;
type TProcGetTextObject    = procedure(const T: TTextObject;      const IdxTxtObj: Int64) of object;
type TProcGetSimpleline    = procedure(const L: TSimpleLigne;     const IdxLine: Int64) of object;
type TProcGetPonctObject   = procedure(const O: TSymbole;  const IdxObj: Int64) of object;

type TProcSetCurrentGrpIdx = procedure(const Idx: TIDGroupeEntites) of object;
type TProcExportCurrGroupe = procedure(const Idx: TIDGroupeEntites) of object;
type TProcRefreshVues      = procedure of object;
type TProcDeleteShape      = procedure of object;

type TProcUpdateDrwCtxt    = procedure of object;

type TProcRefreshStyles    = procedure of object;
type TProcDisplayAvancement= procedure of object;

type TProcRefreshImages    = procedure(const Idx: integer) of object;
type TProcTransmitIdxGroupe= procedure(const Idx: TIDGroupeEntites) of object;

type TProcHandleTTreeviewItem = procedure() of object;
type TProcUseGroupe           = procedure(const Grp: TGroupeEntites) of object;


// mode de travail de la zone de dessin
type TModeTravailCanvas = (mtNONE,
                           //mtDRAW_LIGNE,    // OK
                           mtDRAW_SCRAP,
                           mtDRAW_COURBE,   // OK
                           mtDRAW_FREEHAND_COURBE,
                           mtDRAW_POLYLINE,
                           mtDRAW_POLYGONE, // OK
                           mtDRAW_SYMBOLE,
                           mtDRAW_TEXTE,
                           mtDRAW_IMAGE,

                           mtSELECT_SCRAP,
                           mtSELECT_LIGNE,
                           mtSELECT_POLYLIGNE,
                           mtSELECT_COURBE,   // OK
                           mtSELECT_POLYGONE, // OK
                           mtSELECT_SYMBOLE,
                           mtSELECT_TEXTE,
                           mtSELECT_IMAGE,

                           mtMOVE_OBJET,
                           // nouvelles méthodes de pan et de zoom
                           mtZOOM_PREMIER_COIN,
                           mtZOOM_SECOND_COIN,
                           mtPAN_PREMIER_POINT,
                           mtPAN_SECOND_POINT,
                           // dessin de lignes
                           mtLIGNE_PREMIER_POINT,
                           mtLIGNE_SECOND_POINT,
                           // déplacement de sommets
                           mtCOURBE_EDIT_SOMMET, mtCOURBE_MOVE_SOMMET,
                           mtPOLYGON_EDIT_SOMMET, mtPOLYGON_MOVE_SOMMET,
                           mtPOLYLIN_EDIT_SOMMET, mtPOLYLIN_MOVE_SOMMET,
                           mtSCRAP_EDIT_SOMMET, mtSCRAP_MOVE_SOMMET,
                           // mesure de valeurs sur le polygone provisoire: longueur, périmètre et aire
                           mtMESURE_DISTANCE_SURFACE
                           );


// MétaFiltre
// constantes des filtres
const
  kFLT_ERR      = -1;           //
  kFLT_NIL      =  0;           //  NIL               NIL
  kFLT_ALL      =  1;           //  ALL               ALL
  kFLT_ID       =  2;           //  ID                ID
  kFLT_LONGUEUR =  3;           //  LONGUEUR          LONGUEUR
  kFLT_AZIMUT   =  4;           //  AZIMUT            AZIMUT
  kFLT_PENTE    =  5;           //  PENTE             PENTE
  kFLT_DATE     =  6;           //  DATE              DATE
  kFLT_COULEUR  =  7;           //  COULEUR           COULEUR
  kFLT_X        =  8;           //  X                 X
  kFLT_Y        =  9;           //  Y                 Y
  kFLT_Z        = 10;           //  Z                 Z
  kFLT_LARGEUR  = 11;           //  LARGEUR           LARGEUR
  kFLT_HAUTEUR  = 12;           //  HAUTEUR           HAUTEUR
  kFLT_DATES    = 13;           //  DATES             DATES
  kFLT_COULEURS = 14;           //  COULEURS          COULEURS
  kFLT_SERIE    = 15;           //  SERIE;
  kFLT_RESEAU   = 16;           //  Réseau
  kFLT_SECTEUR  = 17;           //  Secteur ** NEW
  kFLT_CODE     = 18;           //  code
  kFLT_EXPE     = 19;           //  Séance (expé)
  kFLT_TYPEVISEE= 20;           //  type de visée
  kFLT_STATION  = 21;           //  Station (point topo) ** NEW
type TCompOperateur = byte;
type TConnectorFiltres = (ftERROR, ftAND, ftOR);
type TTypeFiltre = (ftINVALID, ftSCALAR, ftINTERVAL, ftFLAGS);
type TFiltre = record
  Caption  : string;
  TypeFiltre: TTypeFiltre;
  Filter   : integer;
  Operateur: TCompOperateur;
  Basculed : Boolean;
  Valeur   : string;
  BorneInf : string;
  BorneSup : string;
  Flags    : string;
  ConnectedWithPrevious: TConnectorFiltres;
end;

type TFlagGroupe = (fgVISIBLE, fgDO_OFFSET);

const
  FMT_APPLICATION_TITLE = '%s - GHCaveDraw %s [Basepoints notation: %s]';

// format d'une ligne Basepoint
const
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  FORMAT_BASEPOINT = '%s';
  {$ELSE}
  FORMAT_BASEPOINT = '%d';
  {$ENDIF TIDBASEPOINT_AS_TEXT}
  FMT_COORDS   = '%.2f' + #9 +'%.2f' + #9 + '%.2f';
  FMT_BASE_STS = '   ' + FORMAT_BASEPOINT + #9 + '%s' + #9 +      // BP.IDStation   BP.Caption
                    '%d'+ #9 + '%d' + #9 +      // BP.TypeStation BP.Couleur
                    FMT_COORDS + #9 +           // BP.PosExtr0.X   BP.PosExtr0.Y  BP.PosExtr0.Z
                    FMT_COORDS + #9 +           // BP.PosStation.X BP.PosStation.Y BP.PosStation.Z
                    FMT_COORDS + #9 +           // BP.PosPG.X BP.PosPG.Y BP.PosPG.Z
                    FMT_COORDS + #9 +           // BP.PosPD.X BP.PosPD.Y BP.PosPD.Z
                    '%s';

const
  NOENTITIES   = '# **** NO ENTITIES IN %s section ***';
  ENTITYARCBEZIER    = '    arc'+ #9 + FORMAT_BASEPOINT + #9 +   // ID Base Station
                       FMT_COORDS + #9 +   // Offset P1
                       FMT_COORDS + #9 +   // Tan P1
                       FORMAT_BASEPOINT + #9 +
                       FMT_COORDS + #9 +   // Offset P2
                       FMT_COORDS;          // Tan P2

  ENTITYVERTEXPOLYGON= '    vertex'+ #9 + FORMAT_BASEPOINT +
                                     #9 + FMT_COORDS;


// mots_clés du langage GHCaveDraw
const
  ENDSEC             = 'end';
  // mots clés pour les styles d'objets
  STYLEPREFIX = 'style';
  STYLESSECTION      = STYLEPREFIX + 'ssection';
    STYLECURVE    = STYLEPREFIX + 'curve';
    STYLELINE     = STYLEPREFIX + 'line';
    STYLEFILL     = STYLEPREFIX + 'fill';
    STYLEPOLYGON  = STYLEPREFIX + 'polygon';
    STYLETEXT     = STYLEPREFIX + 'text';
    STYLEPONCTOBJ = STYLEPREFIX + 'ponctualobject';

    STYLECURVES    = STYLECURVE + 's';
    STYLELINES     = STYLELINE + 's';
    STYLEFILLS     = STYLEFILL + 's';
    STYLEPOLYGONS  = STYLEPOLYGON + 's';
    STYLETEXTS     = STYLETEXT + 's';
    STYLEPONCTOBJs = STYLEPONCTOBJ + 's';

    ENDSTYLECURVES  = ENDSEC + STYLECURVES;
    ENDSTYLELINES   = ENDSEC + STYLELINES;
    ENDSTYLEFILLS    = ENDSEC + STYLEFILL;
    ENDSTYLEPOLYGONS = ENDSEC + STYLEPOLYGON;
    ENDSTYLETEXTS    = ENDSEC + STYLETEXT;
    ENDSTYLEPONCTOBJS= ENDSEC + STYLEPONCTOBJ;
  ENDSTYLESSECTION   = ENDSEC + STYLESSECTION;

  // mots clés pour points de base
  BASEPOINTMARKER    = 'basepoints';
  ENDBASEPOINTMARKER = ENDSEC + BASEPOINTMARKER;
  SUPERGROUPESSECTION     = 'supergroupes';
    SUPERGROUPE_ITEM      = 'supergroupe';
  ENDSUPERGROUPESSECTION  = ENDSEC + SUPERGROUPESSECTION;
  GROUPESSECTION     = 'groupes';
    GROUPE_ITEM      = 'groupe';
  ENDGROUPESSECTION  = ENDSEC + GROUPESSECTION;
  // images de fond
  IMAGESSECTION      = 'images';
    IMAGE      = 'bitmap';
      FMT_IMAGE  = #9 + '%d' +
                   #9 + '%.2f' + #9 + '%.2f' + //
                   #9 + '%.2f' + #9 + '%.2f' + // PositionCoinsImage: TRect2Df
                   #9 + '%d' +                               // Opacite         : byte;
                   #9 + '%s' +                               // SrcFilename     : string;
                   #9 + '%s'                                 // Description     : string;
                   ;
    ENDIMAGE   = ENDSEC + IMAGE;
  ENDIMAGESSECTION   = ENDSEC + IMAGESSECTION;
  // scraps
  SCRAPSSECTION      = 'morceaux'; // traduction littérale de 'scrap'
    SCRAP  = 'scrap';
      VERTEX = 'vertex';
      FMT_SCRAP      = #9 + '%d' + #9  + // IDScrap
                       '%d' + #9 +       // IDGroupe
                       '%d' + #9 + '%d' + #9 + '%d' + #9 +  // RGB values of Couleur
                       '%d' + #9 +       // Transparence
                       '%s';              // Nom

    ENDSCRAP   = ENDSEC + SCRAP;
  ENDSCRAPSSECTION   = ENDSEC + SCRAPSSECTION;

  // courbes
  CURVESSECTION      = 'curveobjects';
    ENTITYCURVE      = 'polybezier';
      FMT_CURVE      = #9 + '%d' + #9 + '%d' + #9 + '%d' + #9 + '%d'; // + #9 + '%s';
      CURVE_ARC      = 'arc';
    ENDENTITYCURVE   = ENDSEC + ENTITYCURVE;
  ENDCURVESSECTION   = ENDSEC + CURVESSECTION;
  // polylignes
  POLYLINESSECTION      = 'polylineobjects';
    ENTITYPOLYLINE      = 'polyln';
      FMT_POLYLINE      = FMT_CURVE;
    ENDENTITYPOLYLINE   = ENDSEC + ENTITYPOLYLINE;
  ENDPOLYLINESSECTION   = ENDSEC + POLYLINESSECTION;
  // lignes simples
  LINESECTION        = 'lineobjects';
  ENDLINESECTION     = ENDSEC + LINESECTION;
  // polygones
  POLYGONSECTION     = 'polygonobjects';
    ENTITYPOLYGON       = 'polyentity';
      FMT_POLYGON    = #9 + '%d' + #9 + '%d' + #9 + '%d';
    ENDENTITYPOLYGON    = ENDSEC + ENTITYPOLYGON;
  ENDPOLYGONSECTION  = ENDSEC + POLYGONSECTION;


  TEXTSECTION        = 'textobjects';
    TEXT_ITEM        = 'text';
  ENDTEXTSECTION     = ENDSEC + TEXTSECTION;
  PONCTOBJSECTION        = 'ponctualobjects';
    PONCT_OBECT_ITEM = 'ponctualobject';
  ENDPONCTOBJSECTION     = ENDSEC + PONCTOBJSECTION;
  // mots-clés pour les sections transversales
  CROSS_PROFILES_SECTION    = 'crossprofiles';
    CROSS_SECTION               = 'crosssection';
      CROSS_SECTION_SCRAP_ENGLOBANT     = 'CSBoundingScrap';
        CROSS_SECTION_VERTEX_SCRAP_ENGLOBANT = 'CSVertexScrap';
      END_CROSS_SECTION_SCRAP_ENGLOBANT = ENDSEC + CROSS_SECTION_SCRAP_ENGLOBANT;

      CROSS_SECTION_COURBES        = 'CSCourbes';
        CROSS_SECTION_A_COURBE        = 'CSPolybezier';
          CROSS_SECTION_ARC_COURBE      = 'CSArcCourbe';
        END_CROSS_SECTION_A_COURBE    = ENDSEC + CROSS_SECTION_A_COURBE;
      END_CROSS_SECTION_COURBES    = ENDSEC + CROSS_SECTION_COURBES;
      CROSS_SECTION_POLYGONES      = 'CSPolygones';
        CROSS_SECTION_A_POLYGONE        = 'CSPolygon';
          CROSS_SECTION_POLY_VERTEX       = 'CSVertex';
        END_CROSS_SECTION_A_POLYGONE    = ENDSEC + CROSS_SECTION_A_POLYGONE;
      END_CROSS_SECTION_POLYGONES  = ENDSEC + CROSS_SECTION_POLYGONES;
      CROSS_SECTION_LINES          = 'CSLines';
        CROSS_SECTION_SIMPLELINE        = 'CSSimpleLine';
      END_CROSS_SECTION_LINES      = ENDSEC + CROSS_SECTION_LINES;
      CROSS_SECTION_TEXTES         = 'CSTexts';
        CROSS_SECTION_SIMPLETEXT   = 'CSSimpleText';
      END_CROSS_SECTION_TEXTES  = ENDSEC + CROSS_SECTION_TEXTES;
    END_CROSS_SECTION           = ENDSEC + CROSS_SECTION;
  END_CROSS_PROFILES_SECTION = ENDSEC + CROSS_PROFILES_SECTION;
// svg_odg_styles.inc
// Paramètres de noms de styles pour exportations vers logiciels de dessin
// styles
const
  STYLES_CENTERLINES_VISEES   = 'centerline-shots';
  STYLES_CENTERLINES_ANTENNES = 'centerline-antennas';
  STYLES_CENTERLINES_SECTIONS = 'centerline-sections';
  STYLES_CENTERLINES_ENTREES  = 'centerline-entrances';
  GROUPE_CARTOUCHE            = 'GroupeCartouche';
  GROUPE_ECHELLE              = 'GroupeEchelle';
  GROUPE_CENTERLINES          = 'GroupeCenterlines';
  GROUPE_CENTERLINES_ENTREES  = 'GroupeCenterlinesEntrees';
  // styles de scrap
  STYLE_SCRAP                 = 'ScrapStyle';
  // styles de courbes
  STYLE_COURBE_DEFAULT        = 'standard';
  STYLE_COURBE_PAROIS         = 'CourbeParois';
  STYLE_COURBE_PAROIS_CACHEES = 'CourbeParoisCachees';
  STYLE_COURBE_ECOULEMENTS    = 'CourbeEcoulement';
  STYLE_COURBE_RESSAUTS       = 'CourbeRessauts';
  STYLE_COURBE_MINI_RESSAUTS  = 'CourbeMiniRessauts';
  STYLE_COURBE_PENTES         = 'CourbePente';
  STYLE_COURBE_SURPLOMB       = 'CourbeSurplomb';
  STYLE_COURBE_CHENAL         = 'CourbeChenal';
  // styles de polygones
  STYLE_POLYGONE_DEFAUT       = 'standard';
  STYLE_POLYGONE_LAC          = 'PolygonLac';
  STYLE_POLYGONE_ARGILE       = 'PolygonArgile';
  STYLE_POLYGONE_SABLE        = 'PolygonSable';
  STYLE_POLYGONE_EBOULIS      = 'PolygonBlocs';
  STYLE_POLYGONE_GALETS       = 'PolygonGalets';
  STYLE_POLYGONE_NEIGE        = 'PolygonNeige';
  STYLE_POLYGONE_SILHOUETTE   = 'PolygonSilhouette';
  STYLE_POLYGONE_GROS_BLOC    = 'PolygonGrosBloc';
  STYLE_POLYGONE_GOUR         = 'PolygonGour';
  STYLE_POLYGONE_SIPHON       = 'PolygonSiphon';
  STYLE_POLYGONE_VARVES       = 'PolygonVarves';
  STYLE_POLYGONE_CHEMIN       = 'PolygonChemin';
  // styles de lignes
  STYLE_LIGNE_DEFAULT         = 'standard';
  STYLE_LIGNE_FLECHE          = 'LigneFleche';
  STYLE_LIGNE_SUITE_RESEAU    = 'LigneContinuation';
  STYLE_LIGNE_FRACTURE        = 'LigneFracture';
  STYLE_LIGNE_PENTE           = 'LignePente';
  // styles de texte
  STYLE_TEXTE_DEFAULT         = 'standard';
  STYLE_TEXTE_TITRES          = 'TexteTitres';
  STYLE_TEXTE_SOUS_TITRES     = 'TexteSousTitres';
  STYLE_TEXTE_COTATION        = 'TexteCotation';
  STYLE_TEXTE_ORDINAIRE_1     = 'TexteOrdinaire1';
  STYLE_TEXTE_ORDINAIRE_2     = 'TexteOrdinaire2';
  STYLE_TEXTE_DEBUG           = 'TexteDebug';
  STYLE_TEXTE_LIEU_DIT        = 'TexteLieuDit';


  // **********************************************
  //  Types de données pour les outils graphiques
  // **********************************************
  // coordonnées

  type TsvgpsPoint2Df = record
    X : double;
    Y : double;
  end;
  type TsvgpsArrayPts2Df = array of TsvgpsPoint2Df;

  // Fontes pour PostScript
  type TFontPSProperties = record
    Name  : string;
    Size  : integer;
    Height: integer;
    Color : TColor;
    Style : TFontStyles;
  end;
  type TPenPSProperties = record
    Name    : string;
    Color   : TColor;
    fWidth  : double;
    nWidth  : integer;
    Style   : TPenStyle;
  end;
  type TBrushPSProperties = record
    Color   : TColor;
    Alpha   : integer;
    Style   : TBrushStyle;
  end;
// constantes d'erreur
//------------------------------------------------------------------------------

const errMERGE_NO_HAVE_OBJECTS              = -4;
const errMERGE_SCRAPS_OK                    =  0;
const errMERGE_SCRAPS_ANY_ERROR             = -1;
const errMERGE_SCRAPS_GROUPES_MISMATCH      = -2;
const errMERGE_SCRAPS_NO_INTERSECT          = -3;

const errMERGE_POLYGONES_OK                 = errMERGE_SCRAPS_OK;
const errMERGE_POLYGONES_ANY_ERROR          = errMERGE_SCRAPS_ANY_ERROR;
const errMERGE_POLYGONES_GROUPES_MISMATCH   = errMERGE_SCRAPS_GROUPES_MISMATCH;
const errMERGE_POLYGONES_NO_INTERSECT       = errMERGE_SCRAPS_NO_INTERSECT;

const errMERGE_POLYLIGNES_OK                = errMERGE_SCRAPS_OK;
const errMERGE_POLYLIGNES_ANY_ERROR         = errMERGE_SCRAPS_ANY_ERROR;
const errMERGE_POLYLIGNES_GROUPES_MISMATCH  = errMERGE_SCRAPS_GROUPES_MISMATCH;
const errMERGE_POLYLIGNES_NO_INTERSECT      = errMERGE_SCRAPS_NO_INTERSECT;

const errMERGE_SCRAP_AND_POLYGON_OK                = errMERGE_SCRAPS_OK;
const errMERGE_SCRAP_AND_POLYGON_ANY_ERROR         = errMERGE_SCRAPS_ANY_ERROR;
const errMERGE_SCRAP_AND_POLYGON_GROUPES_MISMATCH  = errMERGE_SCRAPS_GROUPES_MISMATCH;
const errMERGE_SCRAP_AND_POLYGON_NO_INTERSECT      = errMERGE_SCRAPS_NO_INTERSECT;



const errCONCAT_COURBES_OK                  = errMERGE_SCRAPS_OK;
const errCONCAT_COURBES_EMPTY_LIST          = errMERGE_SCRAPS_ANY_ERROR;
const errCONCAT_COURBES_ANY_ERROR           = errMERGE_SCRAPS_ANY_ERROR;
const errCONCAT_COURBES_GROUPES_MISMATCH    = errMERGE_SCRAPS_GROUPES_MISMATCH;
const errCONCAT_COURBES_NO_INTERSECT        = errMERGE_SCRAPS_NO_INTERSECT;
const errCONCAT_POLYLIGNES_OK               = errMERGE_SCRAPS_OK;
const errCONCAT_POLYLIGNES_ANY_ERROR        = errMERGE_SCRAPS_ANY_ERROR;
const errCONCAT_POLYLIGNES_GROUPES_MISMATCH = errMERGE_SCRAPS_GROUPES_MISMATCH;
const errCONCAT_POLYLIGNES_NO_INTERSECT     = errMERGE_SCRAPS_NO_INTERSECT;
const errGEN_SCRAP_FROM_COURBES_OK                       = errMERGE_SCRAPS_OK;
const errGEN_SCRAP_FROM_COURBES_ANY_ERROR                = errMERGE_SCRAPS_ANY_ERROR;
const errGEN_SCRAP_FROM_COURBES_GROUPES_MISMATCH         = errMERGE_SCRAPS_GROUPES_MISMATCH;
const errGEN_SCRAP_OR_POLYGONE_FROM_COURBES_ANY_ERROR    = errMERGE_SCRAPS_ANY_ERROR;
const errGEN_SCRAP_FROM_POLYLIGNES_OK                    = errMERGE_SCRAPS_OK;
const errGEN_SCRAP_FROM_POLYLIGNES_ANY_ERROR             = errMERGE_SCRAPS_ANY_ERROR;
const errGEN_SCRAP_FROM_POLYLIGNES_GROUPES_MISMATCH      = errMERGE_SCRAPS_GROUPES_MISMATCH;

const errGEN_POLYGONE_FROM_COURBES_OK                       = errMERGE_SCRAPS_OK;
const errGEN_POLYGONE_FROM_COURBES_ANY_ERROR                = errMERGE_SCRAPS_ANY_ERROR;
const errGEN_POLYGONE_FROM_COURBES_GROUPES_MISMATCH         = errMERGE_SCRAPS_GROUPES_MISMATCH;
const errGEN_POLYGONE_OR_POLYGONE_FROM_COURBES_ANY_ERROR    = errMERGE_SCRAPS_ANY_ERROR;
const errGEN_POLYGONE_FROM_POLYLIGNES_OK                    = errMERGE_SCRAPS_OK;
const errGEN_POLYGONE_FROM_POLYLIGNES_ANY_ERROR             = errMERGE_SCRAPS_ANY_ERROR;
const errGEN_POLYGONE_FROM_POLYLIGNES_GROUPES_MISMATCH      = errMERGE_SCRAPS_GROUPES_MISMATCH;


const errGEN_SCRAP_OR_POLYGONE_FROM_POLYLIGNES_ANY_ERROR = errMERGE_SCRAPS_ANY_ERROR;

//*************************************
// pour les convertisseurs de coordonnées
// type de projection (systeme de coordonnées)
type TCodeEPSG = type Integer;
type TLabelSystemesCoordsEPSG = record
  CodeEPSG: TCodeEPSG;
  Nom     : string;
end;

 { Point record and analoga }
type TProjUV = Record
  U: Double;
  V: Double;
End;
type TProjXY = TProjUV;


const
  CODE_EPSG_WGS84           = 4326;
  CODE_EPSG_LAMBERT_93      = 2154;    NOM_LAMBERT_93 = 'Lambert 93';
  // anciens codes Lambert
  CODE_EPSG_LAMBERT_I_NTF_DEPRECATED   =  27561 ; // 'NTF (Paris) / Lambert North France' Obsolete
  CODE_EPSG_LAMBERT_II_NTF_DEPRECATED  =  27562 ; // 'NTF (Paris) / Lambert Centre France'
  CODE_EPSG_LAMBERT_III_NTF_DEPRECATED =  27563 ; //  'NTF (Paris) / Lambert South France'
  CODE_EPSG_LAMBERT_IV_NTF_DEPRECATED  =  27564 ; //  'NTF (Paris) / Lambert Corsica'
  // codes actuels
  CODE_EPSG_LAMBERT_I                  =  27571;
  CODE_EPSG_LAMBERT_II                 =  27572;
  CODE_EPSG_LAMBERT_III                =  27573; NOM_LAMBERT_III = 'Lambert T3';
  CODE_EPSG_LAMBERT_IV                 =  27574;
  // lambert 9 zones
  CODE_EPSG_CC_ZONE_0                  =  3940;
  // codes UTM
  CODE_EPSG_UTM_ZONE_0_NORTH           =  32600;
  CODE_EPSG_UTM_ZONE_0_SOUTH           =  32700;


  CODE_EPSG_GOOGLE                     = 379009;
  DEFAULT_SYSTEME_COORDONNEES_NOM       = NOM_LAMBERT_93;       // évolutif en LT93
  DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG = CODE_EPSG_LAMBERT_93; // évolutif en LT93
// structure contenant les textures de polygones
type TTexturesPolygonObject = record
  bmpClay   : TBGRABitmap;
  bmpSand   : TBGRABitmap;
  bmpEboulis: TBGRABitmap;
  bmpGalets : TBGRABitmap;
  bmpSnow   : TBGRABitmap;
  bmpClayGalets: TBGRABitmap;
  // Textures procédurales
  bmpTextureFlotte : TBGRABitmap;
  bmpTextureEboulis: TBGRABitmap;
end;

// cavité ou réseau stocké en BDD
type TCaviteInBDD = record
  IDCavite       : Int64;
  NomCavite      : string;
  CodeEPSG       : integer;
  IDUserCreateur : integer;
  DateAjout	     : TDateTime;
  IDLastUser     : integer;
  DateLastModif	 : TDateTime;
  IsEditing      : boolean;


  NbGroupes      : integer;
  NbScraps       : integer;
  NbCourbes      : integer;
  NbPolygones    : integer;
  NbPolylignes   : integer;
  NbSimplesLignes: integer;
  NbSymboles     : integer;
  NbTextes       : integer;
end;

// utilisateur OpenCaveMap
type TUtilisateurOCM = record
  IDUtilisateur    : Int64;
  NomUtilisateur   : string;
  Email            : string;
  Login            : string;
  DateInscription  : TDateTime;
end;

// sections transversales




//**********************************************
// Types pour l'éditeur de sections
//**********************************************
type TCrossSectionArcCourbe = record
  CoordsP1       : TPoint2Df;
  TangP1         : TPoint2Df;
  CoordsP2       : TPoint2Df;
  TangP2         : TPoint2Df;
end;
type TCrossSectionCourbe = record
  IDStyleCourbe  : TNatureObjetCourbe;
  Arcs           : array of TCrossSectionArcCourbe;
end;
type TCrossSectionPolygone = record
  IDStylePolygone: TNatureObjetPolygone;
  Vertexes       : array of TPoint2Df;
end;

type TCrossSectionSimpleLigne = record
  IDStyleLigne: TNatureObjetLigne;
  Extr1: TPoint2Df;
  Extr2: TPoint2Df;
end;
// textes pour les sections transversales
type TCrossSectionTexte = record
  IDStyleTexte   : TNatureObjetTexte;
  Alignment      : byte;
  PosX, PosY     : double;
  Text           : string;
end;


const
  KML_OPENGIS_WEBSITE     = 'http://www.opengis.net/kml/2.2';
  KML_GOOGLE_KML_WEBSITE  = 'http://www.google.com/kml/ext/2.2';
  W3C_W3_WEBSITE          = 'http://www.w3.org/2005/Atom';
  W3C_XML_SCHEMA_WEBSITE  = 'http://www.w3.org/2001/XMLSchema-instance';
  GPX_TOPOGRAPHIX_WEBSITE = 'http://www.topografix.com/GPX/1/0';


// nom de styles de scraps par défault (pour silhouettes
const
  SILHOUETTE_STYLE_SCRAP  : string = 'Silhouette0';

//******************************************************************************
// Variables pour les scripts JS dans les exports vers SIG
//******************************************************************************


implementation

end.

