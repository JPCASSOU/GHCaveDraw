// 29/04/2013: Ajout du support des super-groupes
// 10/06/2013: ID de terrain des stations pour TBaseStation
// 26/06/2015: Support des Scraps
// 30/09/2015: Support des Images

{$INCLUDE CompilationParameters.inc}

unit GHCD_Types;
interface
uses
  SysUtils,
  Classes,
  Graphics,
  BGRABitmap;
////////////////////////////////////////////////////////////////////////////////
// Constantes d'inclusion et de copyright
const INSTRUCTION_INCLUDE_GCP  = '$INCLUDE_GCP:';
const MENTION_CC_BY_SA         = 'CC-BY-SA Creative Commons';

// Constantes d'URL
const  KML_OPENGIS_WEBSITE     = 'http://www.opengis.net/kml/2.2';
const  KML_GOOGLE_KML_WEBSITE  = 'http://www.google.com/kml/ext/2.2';
const  W3C_W3_WEBSITE          = 'http://www.w3.org/2005/Atom';
const  W3C_XML_SCHEMA_WEBSITE  = 'http://www.w3.org/2001/XMLSchema-instance';
const  GPX_TOPOGRAPHIX_WEBSITE = 'http://www.topografix.com/GPX/1/0';
// Constantes EPSG
const CODE_EPSG_WGS84                  = 4326;
const CODE_EPSG_LAMBERT_93             = 2154;    NOM_LAMBERT_93 = 'Lambert 93';
const // anciens codes Lambert
  CODE_EPSG_LAMBERT_I_NTF_DEPRECATED   =  27561 ; // 'NTF (Paris) / Lambert North France' Obsolete
  CODE_EPSG_LAMBERT_II_NTF_DEPRECATED  =  27562 ; // 'NTF (Paris) / Lambert Centre France'
  CODE_EPSG_LAMBERT_III_NTF_DEPRECATED =  27563 ; //  'NTF (Paris) / Lambert South France'
  CODE_EPSG_LAMBERT_IV_NTF_DEPRECATED  =  27564 ; //  'NTF (Paris) / Lambert Corsica'
const  // codes actuels
  CODE_EPSG_LAMBERT_I                  =  27571;
  CODE_EPSG_LAMBERT_II                 =  27572;
  CODE_EPSG_LAMBERT_III                =  27573; NOM_LAMBERT_III = 'Lambert T3';
  CODE_EPSG_LAMBERT_IV                 =  27574;
  // lambert 9 zones
const CODE_EPSG_CC_ZONE_0                  =  3940;
// codes UTM
const CODE_EPSG_UTM_ZONE_0_NORTH           =  32600;
const CODE_EPSG_UTM_ZONE_0_SOUTH           =  32700;
// code Google
const CODE_EPSG_GOOGLE                     = 379009;
const
  DEFAULT_SYSTEME_COORDONNEES_NOM       = NOM_LAMBERT_93;       // évolutif en LT93
  DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG = CODE_EPSG_LAMBERT_93; // évolutif en LT93

////////////////////////////////////////////////////////////////////////////////
// Typages renforcés
type TStringDirectoryFileName = type UTF8String;    // noms de fichiers
{$IFDEF TIDBASEPOINT_AS_TEXT}
type TIDBaseStation = string;
{$ELSE}
type TIDBaseStation        = type Int64;
{$ENDIF TIDBASEPOINT_AS_TEXT}
type TTypeStation          = byte; //(tsENTRANCE, tsFIXPOINT, tsSHOT);
type TCodeEPSG             = type Integer;
type TIDDessinateur        = type integer;
type TIDGroupeEntites      = type Int64;
type TIDSuperGroupe        = type Int64;
type TFacteurEchelle       = type double;  // Facteur d'échelle, exprimé sous forme décimale (eg: Ech 1/1000 -> facteur de 0.001
type TSeuilVisibilite      = TFacteurEchelle;  // Seuil d'échelle au-dessus duquel l'objet est tracé; a la dimension d'un facteur d'échelle

////////////////////////////////////////////////////////////////////////////////
// Tableaux
const MAX_SIZE_PARAM_ARRAY   = 128;
type TGHStringArray      = array[0..MAX_SIZE_PARAM_ARRAY] of string;  // TStringArray est désormais un type de données de la FCL >3.0.2 . Remplacé par TGHStringArray
type TGHShortStringArray = array[0 .. 7] of string;
type TArrayIdxObjets     = array of Int64;
type TMotif              = array[0..7, 0..7] of byte; // motif 8x8 de remplissage
type TArrayColors        = array of TColor;  // échelle de couleurs
////////////////////////////////////////////////////////////////////////////////
const IDX_STYLE_COURBE_PAROIS = 1;
const DEFAULT_FONT_NAME      = 'Arial';
const SCRAP_COLOR            = TColor($D0D0D0); // couleur de scrap par défaut: gris clair
const SILHOUETTE_STYLE_SCRAP = 'Silhouette0';          // nom de styles de scraps par défault (pour silhouettes

// constantes de calcul
const INFINITE = 1E20;
const DIST_MERGE_VERTEX    = 0.04;  // seuil de fusion de deux vertex
const QDR_MIN_SPACING      = 10.00; // espacement de grille minimal;
// Constantes pour le TIDBaseStation
const MULTIPLICATEUR_SERIE   = 100000;
const MULTIPLICATEUR_STATION = 10;
const DIV_SR_CENTERLINE      = MULTIPLICATEUR_SERIE; // pour décomposition en série station d'un TIDBasestation
const DIV_SR_ANTENNE         = MULTIPLICATEUR_SERIE; // * 100; // pour décomposition en série station d'un TIDBasestation
// Pour la BDD SQL (OpenCaveMap)
const NB_MAX_OBJ_PAR_CAVITE  = 1000000;            // nb max d'objets par table et par cavité
const MULT_IDX_CAVITE        = 1000000000000;      // facteur de positionnement de l'Idx d'une cavité
const MAX_ARRAY_ZOOMS        = 15;                 // gestion des zooms
// Constantes angulaires
const PI_2            = PI/2;
const PI_4            = PI/4;
const PI_180          = PI/180;
const INV_PI_180      = 180/PI;
// Constantes de formatage
const FMT_DIRECTION_FAILLE  = 'N%.3d°';  // Direction des failles (ex: N025°)

////////////////////////////////////////////////////////////////////////////////
// Enumérations
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
// constantes d'erreur génériques
type TErrorCodeGeneric = (errNONE,
                          errBUSY,
                          errOK,
                          errWARNING,
                          errERROR,
                          errFATAL);
// liste des styles d'objets
type TLesStylesObjets = (sdoCURVES,
                         sdoLINES,
                         sdoPOLYGONS,
                         sdoTEXTES);
// barbules  // TODO: Procédure d'ajout d'un nouveau type d'objet: Préciser ici le nouvel objet
type TBarbule = (tbNONE,
                 tbRESSAUT,
                 tbSURPLOMB,
                 tbCHENAL_VOUTE,
                 tbMINI_RESSAUT);
type TCourbesOrPolylignes = (tcpCOURBE, tcpPOLYLIGNE);
type TTypeQuadrillage     = (tqGRID, tqCROSS);
type TModeEdition         = (medCREATION, medMODIF);     // modes d'édition des dialogues de groupes, objets, etc ..
////////////////////////////////////////////////////////////////////////////////
// Ensembles (sets)
type TWithExportGIS = set of (gisWITH_ENTRANCES,            // mode d'export vers logiciels de SIG
                              gisWITH_CENTERLINES,
                              gisWITH_POI,
                              gisWITH_METADATA);
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
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// records

type

{ TArrayOfIdxGroupes }

 TArrayOfIdxGroupes  = record
  private
    M: array of TIDGroupeEntites;
    procedure AddElement(const IDG: TIDGroupeEntites);
  public
    procedure Empty();
    procedure SetCapacity(const N: integer);
    function  AddElementAndSort(const Idx: TIDGroupeEntites): boolean;
    procedure SetElement(const Idx: integer; const IDG: TIDGroupeEntites);
    function  GetElement(const Idx: integer): TIDGroupeEntites;
    function  GetNbElements(): integer;
    function  fromString(const S: string): boolean;
    function  toString(): string;
end;

type TZoomParameters = record
  ID      : integer;
  Caption : string;
  X1      : double;
  Y1      : double;
  X2      : double;
  Y2      : double;
  procedure setFrom(const QIdx: integer; const QX1, QY1, QX2, QY2: double; const QCaption: string = '');
end;

type

{ TPoint3Df }

 TPoint3Df = record
  X: double;
  Y: double;
  Z: double;
  procedure setFrom(const QX, QY, QZ: double); overload;
  procedure setFrom(const QX, QY, QZ: string); overload;
  procedure Empty();
  function DebugString(const QCaption: string = ''): string;
end;
type TPoint2Df = record
  X: double;
  Y: double;
  procedure setFrom(const QX, QY: double);
  procedure Empty();
end;
type TSinusoide          = array[0..9] of TPoint2Df;   // sinusoide pour représentation des arrivées d'eau
type TArrayPoints2Df     = array of TPoint2Df;

type TDroite = record
  PT1: TPoint2Df;
  PT2: TPoint2Df;
  procedure setFrom(const P1, P2: TPoint2Df); overload;
  procedure setFrom(const X1, Y1, X2, Y2: double); overload;
end;
// boites englobantes
type TBoundingBox = record
  C1 : TPoint2Df;
  C2 : TPoint2Df;
  procedure Reset();
  procedure setFrom(const QC1, QC2: TPoint2Df); overload;
  procedure setFrom(const X1, Y1, X2, Y2: double); overload;
  procedure updateFromPoint(const PT: TPoint2Df); overload;
  procedure updateFromPoint(const QX, QY: double); overload;
  function IsValid(): boolean;
end;

// rectangle 2D
type TRect2Df = record
  X1, Y1: double;
  X2, Y2: double;
  procedure setFrom(const QX1, QY1, QX2, QY2: double); overload;
  procedure setFrom(const C1, C2: TPoint2Df); overload;
  procedure setFrom(const C1, C2: TPoint3Df); overload;
end;
 // couple série station au format TOPOROBOT + ID de terrain
type TToporobotIDStation = record
  aSerie: integer;
  aStation: integer;
  aIDTerrain: string;
  procedure setFrom(const QaSerie, QaStation: integer; const QaIDTerrain: string = '');
end;

// super-groupes === réseaux
type TSuperGroupe = record
  NomSuperGroupe: string;
  Displayed     : boolean;
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
  function getToporobotIDStation(): TToporobotIDStation;
  function getToporobotIDStationAsString(const WithIDTerrain: boolean = false): string;
  function getToporobotIDStationWithIDTerrainPriority(): string;
end;

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

// type de point de courbe
type

{ TVertexCourbe }

 TVertexCourbe = record
  IDStation      : TIDBaseStation;
  Offset         : TPoint3Df;
  TangGauche     : TPoint3Df;
  TangDroite     : TPoint3Df;
  function DebugString(): string;
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
// Ne pas transformer en quasi-classe: débogage pénible ++++
type TArrayVertexPolygon = array of TVertexPolygon;
//*)
// type d'arc de courbe
// GHCaveDraw individualise les arcs de courbes
type TArcCourbe = record
  IDStationP1      : TIDBaseStation;
  OffsetP1         : TPoint3Df;
  TangP1           : TPoint3Df; // tangente = dx, dy. C'est un vecteur, pas une position
  IDStationP2      : TIDBaseStation;
  OffsetP2         : TPoint3Df;
  TangP2           : TPoint3Df;
end;
type TArrayArcsCourbes = array of TArcCourbe;
// arcs de Bézier (différents des TArcCourbe)
type TBezierArc = record
  PT1    : TPoint2Df; // premier point
  Tgt1   : TPoint2Df; // tangente
  PT2    : TPoint2Df; // second point
  Tgt2   : TPoint2Df; // tangente
  Pas    : double;    // pas de t en m
end;


// TODO: Voir si on peut chaîner plusieurs courbes
type
  pTCourbe = ^TCourbe;

  { TCourbe }

  TCourbe = record
  private

  public
  Arcs           : TArrayArcsCourbes;
  IDGroupe       : TIDGroupeEntites;
  IDStyleCourbe  : TNatureObjetCourbe;//integer; //TStyleCourbe;
  BoundingBox    : TBoundingBox;

  LastModified   : TDateTime;      // date de modification
  Closed         : boolean;
  MarkToDelete   : boolean;
  procedure Empty();
  procedure setCapacity(const n: integer);
  function  getNbArcs(): integer;
  function  getArc(const Idx: integer): TArcCourbe;
  procedure setArc(const Idx: integer; const A: TArcCourbe);
end;
type TArrayOfTCourbe = array of TCourbe;

type

{ TScrap }

 TScrap = record
  IDGroupe       : TIDGroupeEntites;
  Nom            : string;
  Couleur        : TColor;
  Opacite        : byte; // 0..255
  Area           : double;
  Perimeter      : double;
  BoundingBox    : TBoundingBox;

  LastModified   : TDateTime;      // date de modification
  MarkToDelete   : boolean;
  VertexOrderedCounterClockWise: boolean;
  Sommets        : TArrayVertexPolygon;
  procedure Empty();
  procedure setCapacity(const n: integer);
  function  getNbVertex(): integer;
  function  getVertex(const idx: integer): TVertexPolygon;
  procedure putVertex(const idx: integer; const V: TVertexPolygon);
  function  ReverseVertex(): boolean;
end;
type TPolygone = record

  IDGroupe       : TIDGroupeEntites;
  IDStylePolygone: TNatureObjetPolygone; //integer;
  BoundingBox    : TBoundingBox;
  Area           : double;
  Perimeter      : double;

  LastModified   : TDateTime;      // date de modification
  MarkToDelete   : boolean;
  Sommets        : TArrayVertexPolygon;
  VertexOrderedCounterClockWise: boolean;
  procedure Empty();
  procedure setCapacity(const n: integer);
  function  getNbVertex(): integer;
  function  getVertex(const idx: integer): TVertexPolygon;
  procedure putVertex(const idx: integer; const V: TVertexPolygon);
  function  ReverseVertex(): boolean;
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

  LastModified   : TDateTime;      // date de modification
  Closed         : boolean;
  MarkToDelete   : boolean;
  VertexOrderedCounterClockWise: boolean;
  Sommets        : TArrayVertexPolygon;
  procedure Empty();
  procedure setCapacity(const n: integer);
  function  getNbVertex(): integer;
  function  getVertex(const idx: integer): TVertexPolygon;
  procedure putVertex(const idx: integer; const V: TVertexPolygon);
  function  ReverseVertex(): boolean;
end;
type TArrayOfTPolyligne = array of TPolyLigne;

type TSymbole = record
  IDGroupe       : TIDGroupeEntites;
  TypeObject     : TNatureObjetSymbole; //byte; //TTypePonctualObject;
  Couleur        : TColor;
  IDBaseStation  : TIDBaseStation; // station topo de référence
  Offset         : TPoint3Df;      // décalage
  AngleRot       : double;         // angle de rotation
  ScaleX         : double;
  ScaleY         : double;         // échelle X, Y ; le symbole étant dans un carré unitaire
  TagTexte       : string;         // texte additionnel ou nom de fichier (photo)
  UnTag          : integer;        // pour utilisation selon le type d'objet; ex: Alignement d'une photo
  PhotoDisplayed : boolean;        // pour affichage de photos
  LastModified   : TDateTime;      // date de modification
  MarkToDelete   : boolean;
end;

// objets Lignes Simples
type TExtremiteLigne = byte; // flèche,

type TSimpleLigne = record
  IDGroupe       : TIDGroupeEntites;
  IDStyleLigne   : TNatureObjetLigne; //integer;
  BoundingBox    : TBoundingBox;
  IDBaseStExt1   : TIDBaseStation;
  IDBaseStExt2   : TIDBaseStation;
  OffsetExtr1    : TPoint3Df;
  OffsetExtr2    : TPoint3Df;
  ExtrLin1       : TExtremiteLigne;
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
  LastModified  : TDateTime;      // date de modification
  MarkToDelete  : boolean;
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
type TQuadrillage = record
  TypeQuadrillage: TTypeQuadrillage;
  CrossSize      : double;
  Color          : TColor;
  Spacing        : double;
  DoDisplay      : boolean;
  DoDrawCoords   : boolean;
  procedure setFrom(const QTypeQuadrillage: TTypeQuadrillage;
                    const QColor: TColor;
                    const QSpacing, QCrossSize: double;
                    const QDoDisplay, QDoDrawCoords: boolean);
end;
type TParamsVue2D = record
  BackGroundColor        : TColor;
  ElementsDrawn          : TELementsDrawn;
  PenteLimiteDisp        : double;
  TailleEchelle          : double;
  DoDrawScrapsMonochromes: boolean;        // dessiner les scraps en monochrome transparent?
  ColorScrapMonochrome   : TColor;
  MainGrid               : TQuadrillage;   // Quadrillage principal
  SecGrid                : TQuadrillage;   // Quadrillage secondaire
  GroupeBackColor        : TColor;      // couleur de fond des
  RegleDisplayed         : boolean;
  DoListerPOI            : boolean;
end;

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
  FMT_COORDS   = '%s' + #9 +'%s' + #9 + '%s';
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
                   #9 + '%s' + #9 + '%s' + //
                   #9 + '%s' + #9 + '%s' + // PositionCoinsImage: TRect2Df
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

type TLabelSystemesCoordsEPSG = record
  CodeEPSG: TCodeEPSG;
  Nom     : string;
  procedure setFrom(const QCodeEPSG: TCodeEPSG; const QNom: string);
end;

 { Point record and analoga }
type TProjUV = Record
  U: Double;
  V: Double;
  procedure setFrom(const QU, QV: double);
End;
type TProjXY = TProjUV;



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






//******************************************************************************
// Variables pour les scripts JS dans les exports vers SIG
//******************************************************************************


implementation
uses
  DGCDummyUnit,
  GeneralFunctions;

{ TVertexCourbe }

function TVertexCourbe.DebugString(): string;
begin
  Result := format('VertexCourbe ID = %d', [self.IDStation]);
end;

{ TCourbe }

procedure TCourbe.Empty();
begin
  setlength(self.Arcs, 0);
end;

procedure TCourbe.setCapacity(const n: integer);
begin
  Setlength(self.Arcs, n);
end;

function TCourbe.getNbArcs(): integer;
begin
  result := Length(self.Arcs);
end;

function TCourbe.getArc(const Idx: integer): TArcCourbe;
begin
  Result := self.Arcs[Idx];
end;

procedure TCourbe.setArc(const Idx: integer; const A: TArcCourbe);
begin
  self.Arcs[Idx] := A;
end;

{ TScrap }

procedure TScrap.Empty();
begin
  SetLength(self.Sommets, 0);
end;

procedure TScrap.setCapacity(const n: integer);
begin
  SetLength(self.Sommets, n);
end;

function TScrap.getNbVertex(): integer;
begin
  Result := length(self.Sommets);
end;

function TScrap.getVertex(const idx: integer): TVertexPolygon;
begin
  Result := self.Sommets[Idx];
end;

procedure TScrap.putVertex(const idx: integer; const V: TVertexPolygon);
begin
  self.Sommets[idx] := V;
end;

function TScrap.ReverseVertex(): boolean;
begin
  result := ReverseVertexesOfPoly(self.Sommets);
end;


{ TPolygone }

procedure TPolygone.Empty();
begin
  SetLength(self.Sommets, 0);
end;

procedure TPolygone.setCapacity(const n: integer);
begin
  SetLength(self.Sommets, n);
end;

function TPolygone.getNbVertex(): integer;
begin
  Result := length(self.Sommets);
end;

function TPolygone.getVertex(const idx: integer): TVertexPolygon;
begin
    Result := self.Sommets[Idx];
end;

procedure TPolygone.putVertex(const idx: integer; const V: TVertexPolygon);
begin
  self.Sommets[idx] := V;
end;

function TPolygone.ReverseVertex(): boolean;
begin
  result := ReverseVertexesOfPoly(self.Sommets);
end;

{ TPolyLigne }

procedure TPolyLigne.Empty();
begin
  SetLength(self.Sommets, 0);
end;

procedure TPolyLigne.setCapacity(const n: integer);
begin
  SetLength(self.Sommets, n);
end;

function TPolyLigne.getNbVertex(): integer;
begin
  Result := length(self.Sommets);
end;

function TPolyLigne.getVertex(const idx: integer): TVertexPolygon;
begin
  Result := self.Sommets[Idx];
end;

procedure TPolyLigne.putVertex(const idx: integer; const V: TVertexPolygon);
begin
  self.Sommets[idx] := V;
end;

function TPolyLigne.ReverseVertex(): boolean;
begin
  result := ReverseVertexesOfPoly(self.Sommets);
end;



{ TArrayOfIdxGroupes }

procedure TArrayOfIdxGroupes.Empty();
begin
  SetLength(self.M, 0);
end;

procedure TArrayOfIdxGroupes.SetCapacity(const N: integer);
begin
  SetLength(self.M, N);
end;

procedure TArrayOfIdxGroupes.AddElement(const IDG: TIDGroupeEntites);
var
  n: Integer;
begin
  n := length(self.M);
  SetLength(self.M, n+1);
  self.M[n] := IDG;
end;

procedure TArrayOfIdxGroupes.SetElement(const Idx: integer; const IDG: TIDGroupeEntites);
begin
  self.M[Idx] := IDG;
end;

function TArrayOfIdxGroupes.GetElement(const Idx: integer): TIDGroupeEntites;
begin
  Result := self.M[Idx];
end;

function TArrayOfIdxGroupes.GetNbElements(): integer;
begin
  result := length(self.M);
end;

function TArrayOfIdxGroupes.fromString(const S: string): boolean;
var
  EWE: TStringArray;
  i: Integer;
  QIdx: Int64;
begin
  EWE := SplitEx(Trim(S), [';'], true);
  self.Empty();
  for i := 0 to High(EWE) do
  begin
    QIdx := StrToInt64Def(EWE[i], -1);
    if (QIdx >= 0) then self.AddElement(QIdx);
  end;
end;

function TArrayOfIdxGroupes.toString(): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(self.M) do Result += Format('%d;',[self.M[i]]);
end;
function TArrayOfIdxGroupes.AddElementAndSort(const Idx: TIDGroupeEntites): boolean;
var
  i, n: Integer;
  procedure QExchange(const n1, n2: integer);
  var
    tmp: TIDGroupeEntites;
  begin
    tmp := self.M[n1];
    self.M[n1] := self.M[n2];
    self.M[n2] := tmp;
  end;
  procedure QSort(const lidx, ridx: integer);
  var
    k, e, mid: integer;
    Q1, Q2: TIDGroupeEntites;
  begin
    if (lidx >= ridx) then Exit;
    mid := (lidx + ridx) div 2;
    QExchange(lidx, ridx);
    e:=lidx;
    for k:=lidx+1 to ridx do
    begin
      Q1 := self.M[k];
      Q2 := self.M[lidx];
      if (Q1 < Q2)  then
      begin
        Inc(e);
        QExchange(e, k);
      end;
    end;
    QExchange(lidx, e);
    QSort(lidx, e-1);
    QSort(e+1, ridx);
  end;
begin
  Result := false;
  if (Idx < 0) then exit;
  // vérifier si le groupe existe déjà
  n := Length(self.M);
  for i := 0 to n-1 do if (Idx = self.M[i]) then exit;
  try
    self.AddElement(Idx);
    QSort(0, High(self.M));
    Result := True;
  except

  end;
end;

{ TDroite }

procedure TDroite.setFrom(const P1, P2: TPoint2Df);
begin
  self.PT1 := P1;
  self.PT2 := P2;
end;

procedure TDroite.setFrom(const X1, Y1, X2, Y2: double);
begin
  self.Pt1.setFrom(X1, Y1);
  self.Pt2.setFrom(X2, Y2);
end;

{ TQuadrillage }

procedure TQuadrillage.setFrom(const QTypeQuadrillage: TTypeQuadrillage; const QColor: TColor; const QSpacing, QCrossSize: double; const QDoDisplay, QDoDrawCoords: boolean);
begin
  self.TypeQuadrillage        := QTypeQuadrillage;
  self.Color                  := QColor;
  self.Spacing                := QSpacing;
  self.CrossSize              := QCrossSize;
  self.DoDisplay              := QDoDisplay;
  self.DoDrawCoords           := QDoDrawCoords;
end;

{ TToporobotIDStation }

procedure TToporobotIDStation.setFrom(const QaSerie, QaStation: integer; const QaIDTerrain: string);
begin
  self.aSerie    := QaSerie;
  self.aStation  := QaStation;
  self.aIDTerrain:= QaIDTerrain;
end;

{ TBaseStation }

function TBaseStation.getToporobotIDStation(): TToporobotIDStation;
var
  S1, S2: Int64;
begin
  if (Self.IDStation > 0) then
  begin
    S1 := Self.IDStation div DIV_SR_CENTERLINE;
    S2 := Self.IDStation mod DIV_SR_CENTERLINE;
    Result.setFrom(S1, S2 div 10, self.IDTerrain);
  end
  else
  begin // les IDStation des antennes sont au format -SSSSSPPPPPN, eg: -144000220 pour les antennes de la station 144.22
    S1 := Abs(Self.IDStation) div DIV_SR_ANTENNE;
    S2 := Abs(Self.IDStation) mod DIV_SR_ANTENNE;
    Result.setFrom(S1, S2 div 1000, self.IDTerrain);
  end;
end;

function TBaseStation.getToporobotIDStationAsString(const WithIDTerrain: boolean): string;
var
  EWE: TToporobotIDStation;
begin
  EWE := self.getToporobotIDStation(); //GetToporobotIDStation(ST);
  Result := Format('%d.%d', [EWE.aSerie, EWE.aStation]);
  if (WithIDTerrain) then Result += Format('[%s]', [EWE.aIDTerrain]);
end;

function TBaseStation.getToporobotIDStationWithIDTerrainPriority(): string;
var
  EWE: TToporobotIDStation;
begin
  EWE := self.getToporobotIDStation(); //GetToporobotIDStation(ST);
  if (Trim(EWE.aIDTerrain) <> '') then Result := EWE.aIDTerrain
                                  else Result := Format('%d.%d', [EWE.aSerie, EWE.aStation]);
end;

{ TProjUV }

procedure TProjUV.setFrom(const QU, QV: double);
begin
  self.U := QU;
  self.V := QV;
end;

{ TLabelSystemesCoordsEPSG }

procedure TLabelSystemesCoordsEPSG.setFrom(const QCodeEPSG: TCodeEPSG; const QNom: string);
begin
  self.CodeEPSG := QCodeEPSG;
  self.Nom      := QNom;
end;

{ TZoomParameters }

procedure TZoomParameters.setFrom(const QIdx: integer; const QX1, QY1, QX2, QY2: double; const QCaption: string);
begin
  ID       := QIdx;
  Caption  := QCaption;
  X1       := QX1;
  Y1       := QY1;
  X2       := QX2;
  Y2       := QY2;
end;

{ TRect2Df }

procedure TRect2Df.setFrom(const QX1, QY1, QX2, QY2: double);
begin
  self.X1 := QX1;
  self.Y1 := QY1;
  self.X2 := QX2;
  self.Y2 := QY2;
end;

procedure TRect2Df.setFrom(const C1, C2: TPoint2Df);
begin
  self.X1 := C1.X;
  self.Y1 := C1.Y;
  self.X2 := C2.X;
  self.Y2 := C2.Y;
end;

procedure TRect2Df.setFrom(const C1, C2: TPoint3Df);
begin
  self.X1 := C1.X;
  self.Y1 := C1.Y;
  self.X2 := C2.X;
  self.Y2 := C2.Y;
end;

{ TBoundingBox }

procedure TBoundingBox.Reset();
begin
  Self.C1.setFrom( INFINITE,  INFINITE);
  Self.C2.setFrom(-INFINITE, -INFINITE);
end;

procedure TBoundingBox.setFrom(const QC1, QC2: TPoint2Df);
begin
  self.C1 := QC1;
  self.C2 := QC2;
end;

procedure TBoundingBox.setFrom(const X1, Y1, X2, Y2: double);
begin
  Self.C1.setFrom(X1, Y2);
  Self.C2.setFrom(X2, Y2);
end;

procedure TBoundingBox.updateFromPoint(const PT: TPoint2Df); overload;
begin
  if (PT.X < Self.C1.X) then Self.C1.X := PT.X;
  if (PT.Y < Self.C1.Y) then Self.C1.Y := PT.Y;
  if (PT.X > Self.C2.X) then Self.C2.X := PT.X;
  if (PT.Y > Self.C2.Y) then Self.C2.Y := PT.Y;
end;
procedure TBoundingBox.updateFromPoint(const QX, QY: double); overload;
var
  PT: TPoint2Df;
begin
  PT.setFrom(QX, QY);
  self.updateFromPoint(PT);
end;

function TBoundingBox.IsValid(): boolean;
begin
  Result := (Abs(C2.X - C1.X) < GHCD_Types.INFINITE) and
            (Abs(C2.Y - C1.Y) < GHCD_Types.INFINITE);
end;


{ TPoint2Df }

procedure TPoint2Df.setFrom(const QX, QY: double);
begin
  self.X := QX;
  self.Y := QY;
end;
procedure TPoint2Df.Empty();
begin
  self.X := 0.00;
  self.Y := 0.00;
end;

{ TPoint3Df }

procedure TPoint3Df.setFrom(const QX, QY, QZ: double);
begin
  self.X := QX;
  self.Y := QY;
  self.Z := QZ;
end;

procedure TPoint3Df.setFrom(const QX, QY, QZ: string);
begin
  self.X := ConvertirEnNombreReel(QX, 0.00);
  self.Y := ConvertirEnNombreReel(QY, 0.00);
  self.Z := ConvertirEnNombreReel(QZ, 0.00);

end;

procedure TPoint3Df.Empty();
begin
  self.X := 0.00;
  self.Y := 0.00;
  self.Z := 0.00;
end;

function TPoint3Df.DebugString(const QCaption: string = ''): string;
begin
  Result := Format('%s: %.3f, %.3f, %.3f', [QCaption, self.X, self.Y, self.Z]);
end;

end.

