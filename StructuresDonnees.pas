unit StructuresDonnees;
// Date: 19/04/2012
// Statut: Fonctionnel
// Les fichiers propriétaires de travail (e.g.: *.top) sont
// alignés sur 4 octets pour compatibilité WinCE

//{$mode delphi}{$H+}
//{$PACKRECORDS 4}
// 04/10/2013 : Conversions UTF8 <> ANSI fixed
// 14/04/2014: TElementsDrawn: Elément ajouté: Altitudes
// 12/02/2015: Ajout de deux attributs à TEntiteEtendue
// 22/04/2015: Suppression des champs d'index pour les entrées, réseaux, secteurs
// 20/07/2016: Les étiquettes de terrain et alias de stations topo sont gérés dans une table à part entière
// 29/11/2018: Type TStation supprimé -

{$INCLUDE CompilationParameters.inc}
{$ERROR Ce fichier n'a rien a faire ici}
interface
uses
  Classes
  , SysUtils
  , Graphics
  ;
// année pivot
const ANNEE_PIVOT: word = 50; // année 1950 = pivot pour les années sur deux chiffres
// nombre max de séries par cavité
const NB_MAXI_SERIES_PAR_CAVITE = 100000;
const MULTIPLICATEUR_STATION    = 10;

// dossiers de service
const MON_DOSSIER_CHIFFRES_TOPO = '0_Mes_Donnees_Topo';
const MON_DOSSIER_CROQUIS       = '0_MesCroquis';
const MON_DOSSIER_QSAVES        = '0_Sauvegardes_Horodatees';

const MON_DOSSIER_RPI_DOCS      = '0_Docs_GHTopoRPI';
// fichier INI
CONST GHTOPO_STD_INI_FILENAME   = 'GHTopo_settings.ini';
CONST GHTOPO_RPI_INI_FILENAME   = 'GHTopoRPI_settings.ini';

// mot-clé pour infiquer un point d'intérêt
CONST KEYWORD_POI = '$poi:';
const
  INI_CURRENT_GCS                = 'CurrentCoordinatesSystem';
  INI_CODE_EPSG                  = 'CodeEPSG';
  INI_NOM_EPSG                   = 'NomEPSG';

  INI_SETTINGS_WINDOWS_POSITIONS = 'WindowsPositions';
  INI_SECTION_USER_FOLDERS       = 'UserAdditionalFolders';
  INI_SECTION_RECENT_FILES       = 'RecentFiles';
  INI_SECTION_LAST_FILE_OPENED   = 'LastFile';
  INI_KEY_LAST_FILE_OPENED       = 'LastOpened';
  NB_MAX_RECENT_FILES            = 10;
// pour GHCaveDraw
type TIDBaseStation = type Int64;

// spécifique à certains utilisateurs
{$IFDEF GROS_MINET}
const DFT_OPERATEUR    = 'CLEMENT Sylvestre';
const DFT_CLUB_SPELEO  = 'SC Comminges';
const DFT_REPORTER     = '';
{$ELSE}
const DFT_OPERATEUR    = 'CASSOU Jean-Pierre';
const DFT_CLUB_SPELEO  = '';
const DFT_REPORTER     = '';
{$ENDIF}
// type de chaînes pour les noms de fichier
type TStringDirectoryFilename = type RawByteString; //type UnicodeString;
// types d'entiers pour les numéros (qui peuvent être différents de leurs index dans les tables)
type TNumeroEntrance = type Integer;
type TNumeroReseau   = type integer;
type TNumeroSecteur  = type integer;
type TNumeroSerie    = type Integer;
type TNumeroExpe     = type integer;
type TNumeroCode     = type integer;
type TIdxCouleur     = type Integer; // index sur palette de couleurs TOPOROBOT

type TAxeCoordonnees = (axisX, axisY, axisZ);


const
  // 01/10/2013: Nouveaux instruments
  // code instruments pour les lasermètres-clinomètres de type bâtiment
  // Données recueillies: - Lp (longueur projetée de la visée) -> Longueur
  //                      - dZ (dénivellation de la visée)     -> Pente
  //                      - Angle                              -> Va dans les commentaires
  UNITE_CLINO_LASERMETRE_STANLEY_TLM330_DIR = 800; // Stanley TLM330
  UNITE_CLINO_LASERMETRE_STANLEY_TLM330_INV = UNITE_CLINO_LASERMETRE_STANLEY_TLM330_DIR - 10;
const INFINI: double = 1E24;
const // fichiers temporaires
  TEMP_COORDS_FILE = '_TmpCoords.xyz';
const
  CODE_EPSG_WGS84           = 4326;
  CODE_EPSG_LAMBERT_93      = 2154;
  // anciens codes Lambert
  CODE_EPSG_LAMBERT_I_NTF_DEPRECATED   =  27561 ; // 'NTF (Paris) / Lambert North France' Obsolete
  CODE_EPSG_LAMBERT_II_NTF_DEPRECATED  =  27562 ; // 'NTF (Paris) / Lambert Centre France'
  CODE_EPSG_LAMBERT_III_NTF_DEPRECATED =  27563 ; //  'NTF (Paris) / Lambert South France'
  CODE_EPSG_LAMBERT_IV_NTF_DEPRECATED  =  27564 ; //  'NTF (Paris) / Lambert Corsica'
  // codes actuels
  CODE_EPSG_LAMBERT_I       = 27571;
  CODE_EPSG_LAMBERT_II      = 27572;
  CODE_EPSG_LAMBERT_III     = 27573;
  CODE_EPSG_LAMBERT_IV      = 27574;
  // lambert 9 zones
  CODE_EPSG_CC_ZONE_0       = 3940;
  // codes UTM
  CODE_EPSG_UTM_ZONE_0_NORTH = 32600;
  CODE_EPSG_UTM_ZONE_0_SOUTH = 32700;


  CODE_EPSG_GOOGLE           = 379009;

  DEFAULT_SYSTEME_COORDONNEES_NOM       = 'Lambert III'; // évolutif en LT93

  DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG = CODE_EPSG_LAMBERT_III; // évolutif en LT93 (2154)

const
  MAX_LENGTH_VISEE_4_TOPOROBOT = 160.00; // longueur maxi de visée pour TOPOROBOT

  TAB                      = #9;
  SEPARATOR_TAB            : char = #9;
  MAX_LINES_LOG            = 1000; // nb max de lignes de l'historique des opérations
  MAX_LINES_ERRORS_LOG     = 2000; // nb max de lignes de l'historique des opérations


  DEFAULT_FONT_NAME        = 'Arial';
  NAME_FOR_DEFAULT_FONT    = 'font_default';

// couleur du réseau par défaut:
const
  COULEUR_RESEAU_0             = $000080FF;   // orange
  COULEUR_VISEE_ANTENNE        = clGray;      // couleur des visées en antenne

// export GHCaveDraw
(*
const
  FMT_BASEPOINTS = '    %d'+TAB+ '%s'+ // ID de la visée
        TAB+'%d'+TAB+'%d' + // attributs
        TAB+'%s'+TAB+'%s'+TAB+'%s' +  // point de départ
        TAB+'%s'+TAB+'%s'+TAB+'%s' +  // point d'arrivée
        TAB+'%s'+TAB+'%s'+TAB+'%s' +  // point droit  (avec Z du sol)
        TAB+'%s'+TAB+'%s'+TAB+'%s';   // point gauche (avec Z du plafond)           //*)
//----------------------------------
// format standard TOPOROBOT Série + Point
const FMTSERST      = '%d.%d';
const FMT_NDSER_PT  = '%d-%d';
const FMTSERSTVTOPO = '%d_%d';
// mode ajout, modif,
type TModeAjoutOuModif = (mamAJOUT, mamMODIFICATION);
// mode d'ouverture d'un fichier Qave ou PocketTopo
type TModeOuvertureFichierQavePocketTopo = (mfqtQAVE, mfqtPOCKETTOPO);
// tableau de valeurs réelles
type TArrayOfFloats = array of double;


// notes de version
type TReleaseNotes = record
  Date        : TDateTime;
  Author      : string;
  Description : string;
end;

// pour support du DistoX
type TPacketOfBytes = array[0..3] of byte;
type TMemoryMapDistoX = array[0..65535] of byte;
type TErrorConstruireUneViseeValide = set of (errCVV_NOT_ENOUGHT_NB_SHOTS, errCVV_OUTBOUNDS_LONGUEUR, errCVV_OUTBOUNDS_AZIMUT, errCVV_OUTBOUNDS_PENTE, errCVV_OTHER);
type TDistoXConnectedInMode = (dxcmDISCONNECTED, dxcmPHYSICAL, dxcmEMULATED);

//type TProcOfObject = procedure of object;
type TProcOfObjectWithOneBoolParameter = procedure(const P: boolean) of object;
type TProcOfObjectWithOneIntParameter  = procedure(const P: integer) of object;
type TProcOfObjectReturnsAsBoolean     = function: boolean of object;

// pour le parcours des graphes
type TModeExplorerGraphe = (mpgEN_LARGEUR, mpgEN_PROFONDEUR);
// pour la recherche de stations topo
type TStationMatchFound = record
  Serie        : integer;
  Station      : Integer;
  Match        : string;
end;
type TArrayStationsMatchFound = array of TStationMatchFound;
// pour export VRML, X3D
type TModeExportFormat3D = (me3dX3D, me3dVRML, me3dDXF);

// pour l'export ODG:

// type de projection (systeme de coordonnées)
type TCodeEPSG = type Integer;
type TLabelSystemesCoordsEPSG = record
  CodeEPSG: TCodeEPSG;
  NomEPSG : string;
end;
// type de projection (systeme de coordonnées)


// type de points pour Proj4s
type TProjUV = Record
  U: Double;
  V: Double;
End;
type TProjXY = TProjUV;

type TTypeDeVisee = (tgDEFAULT,         // X défaut = galerie fossile
                     tgENTRANCE,
                     tgFOSSILE,
                     tgVADOSE,
                     tgENNOYABLE,
                     tgSIPHON,
                     tgFIXPOINT,
                     tgSURFACE,
                     tgTUNNEL,
                     tgMINE,
                     tgVISEE_RADIANTE);
type TModeTOptionsCenterLine = set of (mclCENTERLINE, mclCROSS_SECTIONS, mclSTATIONS, mclLBL_STATIONS);
// vecteurs
type TVecteurDouble = array of double;
// tableaux de strings
const MAX_SIZE_PARAM_ARRAY         =  63;
const MAX_NB_LIGNES_TSTRINGARRAY2D = 511;
// TStringArray est un nouveau type de données de FPC 3.0.2
type TGHStringArray   = array[0 ..MAX_SIZE_PARAM_ARRAY] of string;
// tableau 2D utilisé par le frontal de saisie des points topo
type TGHStringArray2D = array[0 .. MAX_NB_LIGNES_TSTRINGARRAY2D] of TGHStringArray;
// valeurs d'erreur sur les visées
const
  SEUIL_LONGUEUR_NULLE = 0.001;
  SEUIL_LONGUEUR_MAXI  = 320.00;
type TErrorVisee = set of (errvSECTEUR, errvTYPEVISEE, errvCODE, errvEXPE, errvLONGUEUR_NULLE, errvLONGUEUR_OVERFLOW, errvAZIMUT, errvPENTE, errvLG, errvLD, errvHZ, errvHN);
// pour la sélection du mode vue 3D: GDI ou OpenGL
type TTypeVue3D = (tv3dGDI, tv3dOPENGL);
// pour la sélection dans les dialogues listes
type TModeSelectionListe = (mslENTRANCES, mslRESEAUX, mslSECTEURS, mslCODE, mslEXPE, mslSERIE, mslTYPE_GALERIE, mslDATE,  // pour la BDD
                            mslSTATIONS_CALCULEES);

// graphiques 2D: éléments dessinés
type TElementsDrawn = set of (edPolygonals, edStations, edIDStations, edAltitudes, edCotes,
                              edWalls, edCrossSections, edFillGalerie,
                              edQuadrilles, edReferentiel, edBounds, edVolumes, edMaillages,
                              edENTRANCES, edCROQUIS, edANTENNES,
                              edJONCTIONS, edPOI);


// mode de représentation: par séances, réseaux, gris
type TModeRepresentationGaleries = (rgENTRANCES, rgRESEAUX, rgSECTEURS, rgSEANCES, rgGRAY, rgDEPTH, rgTYPES_VISEES);


// attributs de texte
type TTexteAttributs = record
  StyleName    : string;
  FontName     : string;
  FontColor    : TColor;
  BackColor    : TColor;
  FontStyle    : TFontStyles;
  HauteurTexte : double;
  Position     : byte;  // position du texte; cf organisation d'un clavier numérique pour le point d'accrochage
  AngleRot     : integer;
end;


type TVector = array of double;


type TMatrix = array of array of double;
type TIncidenceMatrix = array of array of ShortInt;

type TPoint3Df = record
  X:  double;
  Y:  double;
  Z:  double;
end;
// vertex avec un attribut de tri
// (typiquement utilisé pour la construction de profils topo)
type TPoint3DfOrderedByP = record
  X:  double;
  Y:  double;
  Z:  double;
  P:  double;
end;
type TPointOfConduitTransected = record
  X:  double;
  Y:  double;
  Z:  double;
  P:  double;
  Color  : TColor;
  Caption: string;
end;
type TPoint2Df = record
  X:  double;
  Y:  double;
end;
type TArrayPoints2Df = array of TPoint2Df;
type TArrayPoints3Df = array of TPoint3Df;

type TMatrix2x2 = array[1..2, 1..2] of double;
type TMatrix3x3 = array[1..3, 1..3] of double;


// couleur TOPOROBOT: couple Index - Couleur, évitant un appel de TPalette256
type TToporobotCouleur = record
  Index: integer;
  Couleur: TColor;
end;

// couple de points de sections transversale
// contenant les coordonnées X, Y des points extremes d'une section transversale
type TPtsSectionTransversale = record
  PosStation  : TPoint3Df;
  ParoiGaucheX: double;
  ParoiGaucheY: double;
  ParoiDroiteX: double;
  ParoiDroiteY: double;
end;

  type TIDLitteralPoint = array[0..15] of char;
// format de sauvegarde:
// mtabEXTENDEDTAB =  format étendu XTB, natif de GHTopo
// mtabTOPOROBOT   =  pour compat(err)ibilité TOPOROBOT
type TModeSaveTAB =(mtabEXTENDEDTAB, mtabTOPOROBOT);

// gestion des fins de lignes
// des standards PC, UNIX, Macintache
type TTextFileFormat=(tfWINDOWS, tfUNIX, tfMAC);
//***************************************************************************
//****************************
// Types de données TOPOROBOT
//****************************
// entrées
type TEntrance    = record
  eNomEntree: string;
  eIDTerrain: string;
  eXEntree  : double;
  eYEntree  : double;
  eZEntree  : double;
  eRefSer   : integer;
  eRefSt    : integer;
  eCouleur  : TColor;
  eObserv   : string;
end;

// réseau ou secteur spéléologique
// Section -8 du fichier xtb
// Une série ne peut faire partie de plusieurs réseaux.

type TReseau = record
  ColorReseau  : TColor;
  TypeReseau   : integer; // type de réseau:
  NomReseau    : string;
  ObsReseau    : string;
end;

// secteurs
type TSecteur = record
  NomSecteur     : string;
  CouleurSecteur : TColor;
end;
// expés
type TExpe = record
    IDExpe      : TNumeroExpe;
    JourExpe    : integer;
    MoisExpe    : integer;
    AnneeExpe   : integer;
    Operateur   : String;
    ClubSpeleo  : string;
    ModeDecl    : integer; // deprecated;
    Declinaison : double;
    IdxCouleur  : TIdxCouleur;
    Commentaire : string;
end;
// fonctions trigonométriques de correction angulaire
type TParamFoncCorrectionAngulaire = record
  Co        : double; // constante d'erreur systématique (e.g: cercle d'un compas bien centré mais avec le zéro non positionné sur le barreau aimanté)
  ErreurMax : double; // erreur maximale
  PosErrMax : double; // Angle (azimut ou pente) correspondant à l'erreur maximale
end;

// codes
type TCode = record
    IDCode           : TNumeroCode;
    GradAz           : double;
    GradInc          : double;
    PsiL             : double;
    PsiAz            : double;
    PsiP             : double;
    FactLong         : double;
    AngLimite        : double;
    ErreurTourillon  : double;  // NOUVEAU 09/2018: Support de l'erreur de tourillon (typiquement lors du travail aux cônes)
    Commentaire      : string;
    // Fonctions de correction
    ParamsFuncCorrAz : TParamFoncCorrectionAngulaire;
    ParamsFuncCorrInc: TParamFoncCorrectionAngulaire;
end;
// sens de dessin des séries en coupe développée
type TSensTraceCoupeDev = (stcdVERS_DROITE, stcdVERS_GAUCHE);
// visées
// TODO: Fusionnet TUneVisee et TUneStation
type TUneVisee = record
    NoVisee   : integer; // Indispensable pour les branches et les antennes             //
    TypeVisee : TTypeDeVisee;                                                           //   TypeVisee           : TTypeDeVisee;
    IDSecteur : TNumeroSecteur;                                                         //   stSecteur           : TNumeroSecteur;
    Code      : TNumeroCode;                                                            //   stCode              : TNumeroCode;
    Expe      : TNumeroExpe;                                                            //   stExpe              : TNumeroExpe;
    Longueur  : double;                                                                 //   Longueur            : double;
    Azimut    : double;                                                                 //   Azimut              : double;
    Pente     : double;                                                                 //   Pente               : double;
    LD        : double;                                                                 //   LD                  : double;
    LG        : double;                                                                 //   LG                  : double;
    HZ        : double;                                                                 //   HZ                  : double;
    HN        : double;                                                                 //   HN                  : double;
    IDTerrainStation: string;                                                           //   IDTerrainStation    : string;
    Commentaires    : string;                                                           //   Commentaire         : string;
    DeltaX    : double;                                                                 //
    DeltaY    : double;                                                                 //
    DeltaZ    : double;                                                                 //
    DeltaP    : double;                                                                 //
    AccroissP : double;                                                                 //
    AccroissZ : double;                                                              // PtArrivee           : integer;    //        'Arrivée visée
end;
// visées en antenne
type
  pViseeAntenne = ^TViseeAntenne;
  TViseeAntenne = record
   IDViseeAntenne      : integer;
   EntranceRatt        : integer;
   Reseau              : integer;
   Secteur             : integer;
   SerieDepart         : TNumeroSerie;
   PtDepart            : integer;
   Code                : integer;
   Expe                : integer;
   IDTerrainStation    : string;
   Longueur            : double;      //         'Longueur
   Azimut              : double;      //         'Azimut
   Pente               : double;      //         'Pente
   Commentaires        : string;
end;
type TArrayOfTViseeAntenne = array of TViseeAntenne;


// couple série station au format TOPOROBOT + ID de terrain
type TToporobotIDStation = record
  eIdxNameSpace: integer;
  aSerie: TNumeroSerie;
  aStation: integer;
  aIDTerrain: string;
end;

// structure pour croquis et export GHCavedraw
type TBaseStation = record
   // serie et point
   IDStation         : TIDBaseStation;
   IDTerrain         : string;
   Entite_Serie      : TNumeroSerie;            // Série
   Entite_Station    : integer;                 // Station
   Enabled           : boolean;     // activé par le MétaFiltre
   IsPOI                : boolean;  // l'entité est un point d'intérêt (POI) ?
   // secteurs, type de visée, réseaux, codes, expés
   Type_Entite       : TTypeDeVisee;  // byte dans GHCaveDraw
   // valeur calculée: Couleur en fonction de la profondeur
   // stockée ici en raison du grand nombre de calculs pour le dégradé
   Couleur           : TColor;
   eEntrance         : integer;
   eSecteur          : integer;
   eReseau           : integer;
   eCode             : TNumeroCode;
   eExpe             : TNumeroExpe;
   DateLeve          : TDateTime;
   // valeurs initiales: Long, Az, P
   oLongueur         : double;
   oAzimut           : double;
   oPente            : double;
   oLG               : double;
   oLD               : double;
   oHZ               : double;
   oHN               : double;
   PosExtr0          : TPoint3Df;    // valeurs calculées: centerline
   PosStation        : TPoint3Df;
   PosOPG            : TPoint3Df;   // valeurs calculées: silhouette
   PosOPD            : TPoint3Df;
   PosPG             : TPoint3Df;
   PosPD             : TPoint3Df;
   // champs pour le même usage que l'attribut Tag des TForm.
   TagInteger           : Int64;
   TagDouble            : double;
   // champs texte => en fin de ligne
   oCommentaires        : string;
end;
//*)

// coordonnées WGS84
type TPointWGS84 = record
  Longitude : double;
  Latitude  : double;
end;
// angle
type TUniteAngles = (uaRADIANS, uaDEGRES, uaGRADES);

// modes d'exportation pour logiciels de carto
// gisNONE: le FilterIndex du TOpenDialog démarre à 1 et non 0
type TGISOutputMode     = (gisCENTERLINES, gisSILHOUETTES);
type TGISColorByItems   = (gisCOLOR_UNIQUE, gisCOLOR_ENTRANCES, gisCOLOR_RESEAUX);

type TGISOutputFormat = (gisKML, gisGPX);
type TKMLPlaceMark = record
  Latitude  : double;
  Longitude : double;
  Etiquette : string;
end;


type TPolyMode = (tpmENTETE_POLY, tpmSTART_POLY, tpmPOINT_POLY, tpmEND_POLY);
// marqueurs sur la carte
type TMarker = record
  Displayed: boolean;
  X        : double;
  Y        : double;
  Couleur  : TColor;
  Caption  : string;
  //...
end;




// couleurs 32 bits
type TColorRGBA = record
  R: byte;
  G: byte;
  B: byte;
  A: byte;
end;

type TPoint3DVRML = record
  X: Int64;
  Y: Int64;
  Z: Int64;
end;

// Couleurs Macintosh
type TMacintoshColor = record
  R : word;
  G : word;
  B : word;
end;


// pour stats spéléometriques
type TVentilationSpeleometrie = record
  Fossiles  : double;
  Vadoses   : double;
  Ennoyables: double;
  Siphons   : double;
  Tunnels   : double;
  Filons    : double;
  Speciaux  : double;
end;
type TTableauVentilationSpeleometrie = array of TVentilationSpeleometrie;
// type rectangle
type TRect2Df = record
  X1: double;
  Y1: double;
  X2: double;
  Y2: double;
end;
// spéléométrie simplifiée: Dév, XYZ Maxi et mini, Nb de visées
type TSpeleometrieReseauOuSecteur = record
  DonneesValides   : boolean;
  IDReseauSecteur  : integer;
  NomReseauSecteur : string;
  Developpement    : double;
  CoordMini        : TPoint3Df;
  CoordMaxi        : TPoint3Df;
  Etendue          : TPoint3Df;
  NbVisees         : integer;
end;
type TArraySpeleometrieReseauOuSecteur = array of TSpeleometrieReseauOuSecteur;


// procédure d'affichage de la progression d'une opération
type TProcDisplayProgression = procedure (const Etape: string; const Done, Starting, Ending: integer) of object;



//type TOpenGLColor = array[0..3] of GLFloat;
type TIsoHypse = record
  Cote: double;
  Color: TColor;
end;
type TMaillageVertex = record
  ID         : integer;
  X          : double;
  Y          : double;
  Z          : double;
  NormX      : double;
  NormY      : double;
  NormZ      : double;
  Norme      : double;
end;
(*
type TTriangleABC = record
   Numero  :  integer;
   PointA  :  integer;
   PointB  :  integer;
   PointC  :  integer;
   //TriangleVoisinAB: integer;
   //TriangleVoisinBC: integer;
   //TriangleVoisinCA: integer;
end;
//*)
// Paramètres EPSG
// 01.10.2013: ProjAPI non nécessaire
type TParametresEPSG = record
  CodeEPSG   : integer;
  Parameters : string;
  Comments   : string;
end;

// onglets pour les vues 2D
const NB_MAX_ONGLETS = 10;
type  TModesTravail  = (mtREADY,
                        mtPAN_PREMIER_POINT, mtPAN_SECOND_POINT,
                        mtZOOM_PREMIER_COIN, mtZOOM_SECOND_COIN,
                        mtMETAFILTRE_ZONE_PREMIER_COIN, mtMETAFILTRE_ZONE_SECOND_COIN,
                        mtDISTANCE_PREMIER_POINT, mtDISTANCE_SECOND_POINT,
                        mtADD_SERIE_PREMIER_POINT, mtADD_SERIE_SECOND_POINT,
                        mtBOUCLER_PREMIER_POINT, mtBOUCLER_SECOND_POINT,
                        mtPROFIL_MNT_PREMIER_POINT, mtPROFIL_MNT_SECOND_POINT,
                        mtNEW_ANNOTATION, mtSELECT_ANNOTATION, mtDELETE_ANNOTATION,
                        mtNEW_POLYLINE, mtADD_VERTEX_POLYLINE, mtDELETE_POLYLINE
                        );
type  TQdrType       = (qtNONE, qtGRID, qtCROSS, qtPOINTS);

type TVue2DParams  = record
  ongName: string;
  ongC1  : TPoint2Df;
  ongC2  : TPoint2Df;
  ongVueFiltres: string;
  ongBackGround: TColor;
  ongQdrSpc    : double;
  ongQdrType   : TQdrType;
  ongQdrColor   : TColor;
  ongElementsDrawn: TElementsDrawn;
  ongDegradeStart : TColor;
  ongDegradeStop  : TColor;
  ongViseesLargeur: integer;
  ongModeRepresentation : TModeRepresentationGaleries;
  ongFillOpacite      : byte;  // 00..255
  ongTailleTexteIDStation: double;
  ongTailleTexteAltitudes: double;
  ongTailleTexteCotation : double;
  ongCouleurIDStation    : TColor;
  ongCouleurAltitudes    : TColor;
  ongCouleurCotation     : TColor;
end;
type TVue3DParams  = record
  Name : string;
  Filtres: string;
  DoFiltrer : boolean;
  ElementsDrawn: TElementsDrawn;
  Theta: Double;         // angle de rotation   (degré)
  Phi  : double;         // angle d'inclinaison (degré)
  FovOrZoom  : double;         // angle de champ (OpenGL) ou Zoom
  CoefMagnification:  double;
  ColorBackGround: TColor;
  ColorCube : TColor;
  ColorReferentiel: TColor;
  FillOpacity: byte;
  ModeRepresentation : TModeRepresentationGaleries;
  //ElementsDrawn      : TElementsDrawn;
  ColorZMini  : TColor;
  ColorZMaxi  : TColor;
  ViseesLargeur: integer;

end;
type TCoupeDeveloppeeParams  = record
  Name : string;
  BackGround: TColor;
  ModeRepresentation : TModeRepresentationGaleries;
  ElementsDrawn      : TElementsDrawn;
  QdrSpc             : double;
  QdrType            : TQdrType;
  QdrColor           : TColor;
  ColorZMini         : TColor;
  ColorZMaxi         : TColor;
  ViseesLargeur      : integer;
end;

type TArrOngletsParams = array[0..NB_MAX_ONGLETS] of TVue2DParams;

// mode de fonctionnement des navigateurs de la BDD
type TModeBDD = (mbddDISABLED,
                 mbddENTRANCES,
                 mbddRESEAUX,
                 mbddSECTEURS,
                 mbddCODES,
                 mbddEXPES,
                 mbddPOI,
                 mbddCHECK_ERRORS);

// procédures de conversions GCS->Ecran
type TProcGCSToSRC = function(const PM: TPoint2Df): TPoint of Object;
type TProcRefreshControlsBy3DView = procedure of object;
// demander aux modules parents d'effectuer une action sur l'appelant
type TProcActionModuleParentSurAppelant = procedure of object;




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
  Opacity : byte;
  Style   : TBrushStyle;
end;
// Pour les coupes developpees
type TPointCoupeDeveloppee = record
  P: double;
  Z: double;
end;
// Coupe développée
type TTypeJonction = (tjENTRANCE, tjCARREFOUR, tjEXTREMITE_CONDUIT);
type TJonctionCoupeDeveloppee = record
  NumeroJonction : Int64;
  TypeJonction   : TTypeJonction;
  IDJonction     : string;
  NoSerie        : integer;
  NoPoint        : integer;
  Abscisse       : double;
  Cote           : double;
end;

// sections du fichier d'aide
// objet Section
type THelpFileSection = record
  Index   : Integer;
  Topics  : string;
  Title   : string;
  Texte   : string;
end;

// mode de paramétrage pour la boite de dialogue du paramétrage des vues
type TModeDialogParamVues = (mpdVUE2D, mpdVUE3D, mpdCOUPE_DEVELOPPEE);

// Pour les grilles de stations

const
  GHTOPO_GRID_NB_LINES           = 1001; // 1 + Capa mémoire du DistoX
  LARGEUR_COLONNES_DISTOX_ID     = 60;
  LARGEUR_COLONNES_BLOCK_SEGMENT = 80;
  LARGEUR_COLONNES_HORODATE      = 200;
  LARGEUR_COLONNES_LAP           = 100;
  LARGEUR_COLONNES_LRUD          = 50;
  LARGEUR_COLONNES_TRAME_HEXA    = 320;



// Maillages
type TMNTGridArray = array of array of Double;
type TMNTModeDessinMaillage = (M3D_NONE, M3D_WIRE_FRAME, M3D_MESH, M3D_BLEND, M3D_MIXTE);
type TMNTTypeMaillage = (tmUNKNOWN, tmREGULAR_GRID, tmTRIANGLES);
type TMNTBoundingBox = record
  C1: TPoint3Df;
  C2: TPoint3Df;
end;
type TVertex = record
  ID         : Int64;
  X          : double;
  Y          : double;
  Z          : double;
  NormX      : double;
  NormY      : double;
  NormZ      : double;
  Norme      : double;
end;

type TMNTTriangleABC = record
   Numero  :  integer;
   PointA  :  integer;
   PointB  :  integer;
   PointC  :  integer;
   Marked  :  boolean;
   BoundingBox: TMNTBoundingBox;
end;

// pour le DistoX
type TTypeMesureDistoX = (tmdxUNKNOWN, tmdxCHEMINEMENT, tmdxLRUD, tmdxANTENNE, tmdxSIGNALISATION);

type TMesureViseeDistoX = record
  TypeMesure : TTypeMesureDistoX;
  DistoXSerialNumber: integer;
  SerieCourante  : TNumeroSerie;
  StationCourante: integer;
  Block      : integer;
  Segment    : integer;
  Longueur   : double;
  Azimut     : double;
  Pente      : double;
  IsMarked   : boolean;
  HexaData   : string;
  TimeStamp  : TDateTime; // horodatage de la visée
end;
type TProcTransmitMesureVisee = procedure (const V: TMesureViseeDistoX) of object;
type TArrayMesureViseeDistoX  = array of TMesureViseeDistoX;
type TNodeNum = 0 .. maxLongint;
// Jonctions Toporobot
(*
type TJonctionXYZ = record
   NoNoeud     : TNodeNum;
   NoSer       : integer;
   NoSt        : integer;
   IDJonction  : string;
end;
//*)
type TJonctionXYZ = record
   NoNoeud     : TNodeNum;
   NoSer       : TNumeroSerie;
   NoSt        : integer;
   X           : double;
   Y           : double;
   Z           : double;
   NbRefs      : integer;
   IDJonction  : string;
end;
//*)


// Table des branches
type TBrancheXYZ = record
  NoEntranceRatt: integer;
  NoSerie       : integer;
  NoReseau      : integer;
  NoBranche     : integer;
  NomBranche    : string;
  NoeudDepart   : TNodeNum;
  NoeudArrivee  : TNodeNum;
  Rigidite      : double;  // module de raideur, égal à 1.00 par défaut
  PointsTopo    : array of TUneVisee; //TListePointsTopo; //TListeSimple<TUneVisee>;
  NbPoints      : integer;
  DeltaX        : double;
  DeltaY        : double;
  DeltaZ        : double;
  XDepart       : double;
  YDepart       : double;
  ZDepart       : double;
  XArrivee      : double;
  YArrivee      : double;
  ZArrivee      : double;
end;




type TModeClavierVirtuel = (mkbdvALPHA_MINUSCULES, mkbdvALPHA_MAJUSCULES, mkbdvINTEGER, mkbdvDOUBLE);

// pour le DistoX

type TModeAttenteDonneesRX = (mrxNONE, mrxDEPILAGE_MESURE);
type TProtoSerialError = (PROTO_OK,
                          PROTO_READ, PROTO_WRITE,
                          PROTO_COMMAND,  PROTO_ADDR,  PROTO_PACKET,
                          PROTO_CONNECT, PROTO_TIMEOUT, PROTO_MAX);



// pour le XML
const
  XML_CHAR_LT       = '&lt;';   // &lt; -> "<" (lt = lower than = inférieur à = "<" )
  XML_CHAR_GT       = '&gt;';   // &gt; -> ">" (gt = greater than = supérieur à = ">")
  XML_CHAR_AMP      = '&amp;';  // &amp; -> "&" (comme dit fraoustin - amp = ampersand)
  XML_CHAR_APOS     = '&apos;'; // &apos; -> ' (apos = apostrophe)
  XML_CHAR_QUOT     = '&quot;'; // &quot; -> " (quot = quotation mark = quillemet)
// types d'événement
type TNewEvent = procedure (Sender: TObject; Param1: string) of object;

// modes de remplacement
type TModeRemplacementStations = (mrsSECTEURS, mrsCODES, mrsEXPES);
type TModeRemplacementAntennes = set of (mraSERIES, mraRESEAUX, mraSECTEURS, mraCODES, mraEXPES);

// quads pour vue 3D
type TQuad = record
   VertexA           : TPoint3Df;
   VertexB           : TPoint3Df;
   VertexC           : TPoint3Df;
   VertexD           : TPoint3Df;
   //Normale           : TPoint3Df;
   FacetteVisible    : boolean;
   Depth             : Double;
   Vertex2D_A        : TPoint;
   Vertex2D_B        : TPoint;
   Vertex2D_C        : TPoint;
   Vertex2D_D        : TPoint;
end;
const NBFACESBYVISEE = 6;
type TPortionTubeVisee = record
   Visible           : Boolean;
   Drawn             : Boolean; // dessiné en fonction du MétaFiltre;
   Couleur           : TColor;
   Type_Entite       : TTypeDeVisee;
   IdxEntrance       : integer;
   IdxReseau         : integer;
   IdxSecteur        : integer;
   IdxExpe           : integer;
   Facettes: array[1 .. NBFACESBYVISEE] of TQuad;
   DepthField        : double;
end;

// Filtres nommés
type TFiltrePersonnalise = record
  NomFiltre      : string;
  CouleurFiltre  : TColor;
  Expression     : string;
  Description    : string;
end;
// Points d'intérêt
type TPointOfInterest = record
  Serie         : TNumeroSerie;
  Station       : integer;
  Coordinates   : TPoint3Df; // pour accélérer certains calculs
  Couleur       : TColor;
  LabelTerrain  : string;
  Description   : string;
end;
type TCriticiteMessaqeErreurGHTopo = (cmeNOTE, cmeWARNING, cmeERROR, cmeCRITICAL, cmeERROR_AUTOFIXED);
type TTableExaminee                = (tmeENTRANCES, tmeRESEAUX, tmeSECTEURS, tmeCODES, tmeEXPES, tmeSERIES);
type TMessaqeErreurGHTopoCompiler = record
  TableExaminee : TTableExaminee;
  Index         : Int64;
  Criticite     : TCriticiteMessaqeErreurGHTopo;
  Couleur       : TColor;
  Message       : string;
end;

// constantes des filtres
const
  kFLT_NIL         = 0;      //  NIL               NIL
  kFLT_ALL         = 1;      //  ALL               ALL
  kFLT_ID          = 2;      //  ID                ID
  kFLT_LONGUEUR    = 3;      //  LONGUEUR          LONGUEUR
  kFLT_AZIMUT      = 4;      //  AZIMUT            AZIMUT
  kFLT_PENTE       = 5;      //  PENTE             PENTE
  kFLT_DATE        = 6;      //  DATE              DATE
  kFLT_COULEUR     = 7;      //  COULEUR           COULEUR
  kFLT_X           = 8;      //  X                 X
  kFLT_Y           = 9;      //  Y                 Y
  kFLT_Z           = 10;     //  Z                 Z
  kFLT_LARGEUR     = 11;     //  LARGEUR           LARGEUR
  kFLT_HAUTEUR     = 12;     //  HAUTEUR           HAUTEUR
  kFLT_DATES       = 13;     //  DATES             DATES
  kFLT_COULEURS    = 14;     //  COULEURS          COULEURS
  kFLT_SERIE       = 15;     //  SERIE;
  kFLT_RESEAU      = 16;     //  Réseau
  kFLT_CODE        = 17;     //  code
  kFLT_EXPE        = 18;     //  Séance (expé)
  kFLT_TYPEVISEE   = 19;     //  type de visée
  kFLT_SECTEUR     = 20;     // secteur
  kFLT_ENTRANCE_RATT = 21; // entrée de rattachement de

  //-----------------------------------------------

// pour les croquis (simplifié: polygones et polylignes. On ne fait pas un GHCaveDraw !
type TKrobardIDStylePolyligne  = type integer;
type TKrobardIDStyleAnnotation = type integer;
type TKrobardPolyVertex = record
  IDBaseStation: TIDBaseStation;
  Offset       : TPoint3Df;
end;
type TKrobardStylePolyligne = record
  Name       : string;
  LineColor  : TColor;
  LineOpacity: byte;
  LineStyle  : TPenStyle;
  LineWidth  : integer;
  FillColor  : TColor;
  FillOpacity: byte;
  Closed     : boolean;
  Filled     : boolean;
end;
type TKrobardAnnotation = record
  IDStyle  : TKrobardIDStyleAnnotation;
  Position : TKrobardPolyVertex;
  Alignment: byte;
  Texte    : string;
end;
type TKrobardPolyligne = record
  IDStyle: TKrobardIDStylePolyligne;
  Sommets: array of TKrobardPolyVertex;
  BoundingBox: TRect2Df;
end;
implementation





end.

