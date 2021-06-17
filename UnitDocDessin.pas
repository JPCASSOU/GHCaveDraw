// Unité principale de GHCaveDraw: gestion du dessin
// Date: 30/07/2009
// 29/04/2013: Ajout du support des super-groupes
// 16/11/2013: Nettoyage du code du MetaFiltre
// 17/11/2013: Découplage de la gestion des centerlines
//             (qui va dans UnitCenterlinesBasePoints)
// 02/02/2014: La gestion des styles se fait par des listes et non plus par des tableaux.
// 02/02/2014: Sauvegarde des styles
// 17/02/2014: Suppression de GetABaseStation: doublonne avec GetBaseStation
// 09/04/2014: Remplacement des affectations .X et .Y par MakeTPoint2Df()
// 10/04/2014: Génération de l'échelle dans l'export SVG
// 01/12/2014: Sécurisation de GetCoordsGCSPonctObj(): devient une procédure
//             et retourne un code d'erreur à tester pour la suite du traitement
// 05/01/2015: Support de la Polyligne, utilise les styles de courbes
//             (e.g: pour les parois des carrières souterraines)
// 19/02/2015: Fonction de nettoyage des objets invalides
//             (ie: dont au moins un des points d'accrochage est invalide)
// 19/02/2015: Les fonctions GetBasePointByIndex() et FindBasePoint() retournent désormais un booléen + le point de base
// 10/06/2015: ID de terrain des stations pour TBaseStation
// 15/06/2015: Support du mode Décalage de groupe actif/inactif.
// 16/06/2015: Toutes les fonctions de sélection d'objets et de calcul de
//             BoundingBox utilisent GetCoordsGCSWithoutGroupe()
// 16/06/2015: Les propriétés FNb... ont été remplacées par des fonctions GetNb...()
// 22/06/2015: Utilitaires de purge des objets
// 23/06/2015: Support des polylignes fermées OK (usage: piliers tournés, etc ...)
// 24/06/2015: Calcul de superficie et de périmètres de polygones (utilisés pour calculer
//             la superficie des carrières)
// 25/06/2015: Support des scraps. Un scrap est un polygone appartenant à un groupe.
//             Il est affiché avant tous les objets du groupe
// 26/06/2015: Ajout de l'attribut ZOrder pour les groupes, et du tri associé
// 29/06/2015: Export SVG et ODG des scraps
// 02/07/2015: Calcul du nombre d'objets pour un groupe
// 21/07/2015: Nouveau style de courbes: parois incertaines (en tireté)
// 23/07/2015: Les constructions de suppression de la forme "while (i < GetNbGroupes() - 1) do"
//             sont remplacées par des constructions "for i := GetNbGroupes() - 1 downto 0 do"
// 25/07/2015: Les fonctions ne nécessitant pas TDocumentDessin partent dans UnitGraphismesProvisoires
// 26/07/2015: Les fonctions de recherche d'objets partent dans UnitFindObjects
// 31/07/2015: Ajout d'objets recalcule la bounding-box de son groupe
// 03/08/2015: Export des scraps en format KML et OSM
// 05/08/2015: Les groupes ont maintenant un champ 'Filtres'
// 12/08/2015: Sécurisation de la lecture des nombres réels (point décimal et virgule indifféremment supportés)
// 25/08/2015: Déplacement dans des fichiers .inc de fonctions volumineuses
// 07/10/2015: Nouveau type de textes: Lieux-dits
// 28/10/2015: Réassignation de basepoints pour les objets:
//   function ReattribuerBasePointsCourbe(const QIdx: integer): integer; -- OK
//   function ReattribuerBasePointsPolygone(const QIdx: integer): integer; -- OK
//   function ReattribuerBasePointsPolyLigne(const QIdx: integer): integer; -- OK
//   function ReattribuerBasePointsScrap(const QIdx: integer): integer; -- OK
//   function ReattribuerBasePointSymbole(const QIdx: integer): integer; -- A tester
//   function ReattribuerBasePointTexte(const QIdx: integer): integer;  -- A tester
// 02/12/2015: Restauration du dernier objet effacé
// 19/01/2016: Sécurisation des fonctions ConvertirGHCD2StdPolygon() et ConvertirGHCD2StdBezierCourbe()
//             Correction de bugs
// 15/03/2016: Les listes passent aux génériques
// 29/06/2016: Support des directives $INCLUDE dans le chargeur de topo GCD
// 03/01/2017: Ajout du type de courbe/polyligne nocMUR_MACONNE pour les murs maçonnés
// 01/12/2017: Optimisation de GetGroupeByIDGroupe()
// 29/01/2019: Les objets sont libérés par FreeAndNil(MonObjet); et non par MonObjet.Free;


// Cette unité comporte les dépendances suivantes:
// - docdessinloadfromfile.inc
// - DocDessinSaveToFile.inc
// - DocDessinExportToSVG.inc
{$INCLUDE CompilationParameters.inc}
unit UnitDocDessin;
interface
uses
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GHCD_Types
  ,UnitListesSimplesWithGeneriques
  ,SysUtils, Classes, Math, Graphics, ExtCtrls
  ,Dialogs
  ,UnitCenterlinesBasePoints
  ,GeneralFunctions
  ,FileUtil
  ,SVGCanvasUnit
  ,ConvertisseurJPC
  ,BGRABitmap
  ,UnitKMLExport
  ,UnitLeafletExport
  ,UnitExportGeoJSON
  ,paszlib
  ;
type

{ TDocumentDessin }

 TDocumentDessin = class
    // lecture du nom de fichier
    function  GetFullFilename(): string;
    function  GetDocumentName(): string;
    // obtenir le point de base
    function  GetBaseStation(const Idx: integer): TBaseStation;
    //--------------------------------------------------------------------------
    // chargement et enregistrement de document GHCaveDraw
    function  LoadFromFile(const FichierGCD: TStringDirectoryFileName): boolean;
    procedure SaveToFile(const FichierGCD: TStringDirectoryFileName;
                         const GroupeExported: TIDGroupeEntites;
                         const CenterlinesInFichierSepareGCP: boolean);
    procedure SaveEncryptedFile(const QFilename: TStringDirectoryFileName; const Password: string);
    function  LoadEncryptedFile(const QFilename: TStringDirectoryFileName; const Password: string): boolean;
    // import et export de la section 'basepoints'
    function  ImportBasePoints(const FichierGCP: TStringDirectoryFileName): boolean;
    function  ExportBasePoints(const FichierGCP: TStringDirectoryFileName): boolean;
    //--------------------------------------------------------------------------
    // export vers d'autres formats
    function ExportToSVG(const MyFileName: TStringDirectoryFileName;
                         const ExportScrapsOnly: boolean;
                         const DoXHTML: boolean;
                         const QTitre, QDesc: string): boolean;
    function ExportToODG(const MyFileName: TStringDirectoryFileName;
                         const ExportScrapsOnly: boolean): boolean;

    //--------------------------------------------------------------------------
    // calculs de coordonnées
    procedure GetCoordsGCS(const IDBS: TIDBaseStation;
                           const IDGroupe: TIDGroupeEntites;
                           const Offset: TPoint3Df;
                           out   PtResult: TPoint2Df;
                           out   ErrCode : integer);
    procedure GetCoordsGCSWithoutGroupe(const IDBS: TIDBaseStation;
                                        const Offset: TPoint3Df; out PtResult: TPoint2Df; out ErrCode: integer);
    procedure GetCoordsGCSPonctObj(const qEP: TSymbole;
                                   out PtResult: TPoint2Df;
                                   out errCode: integer);
    //--------------------------------------------------------------------------
    // Gestion de la section BasePoints
    function  GetNbBaseStations(): integer;
    procedure AddABasePoint(const BP: TBaseStation); inline;
    procedure AddABasePointFromString(const S: String);
    procedure TrierBasePointsParIDBaseStation();
    // attraper l'élément point le plus proche d'un point (x, y)
    function GetNearBasepoint(const X, Y: double;
                              const OldBasePoint: TBaseStation;
                              const StLocked: boolean): TBaseStation;

    procedure SetMiniEtMaxi();
    function  GetCoordsMini(): TPoint3Df;
    function  GetCoordsMaxi(): TPoint3Df;
    //--------------------------------------------------------------------------
    // gestion des styles  -- GENERIQUES OK
    procedure SetDefaultStyles;

    procedure AddStyleLigne(const LS: TStyleLigne); inline;
    function  GetStyleLigne(const Idx: integer): TStyleLigne; inline;
    procedure PutStyleLigne(const Idx: integer; const LS: TStyleLigne); inline;
    function  GetNbStylesLigne(): integer; inline;

    procedure AddStyleCourbe(const LS: TStyleCourbe); inline;
    function  GetStyleCourbe(const Idx: integer): TStyleCourbe; inline;
    procedure PutStyleCourbe(const Idx: integer; const LS: TStyleCourbe); inline;
    function  GetNbStylesCourbe(): integer; inline;

    procedure AddStyleTexte(const LS: TStyleTexte); inline;
    function  GetStyleTexte(const Idx: integer): TStyleTexte; inline;
    procedure PutStyleTexte(const Idx: integer; const LS: TStyleTexte); inline;
    function  GetNbStylesTexte(): integer; inline;


    procedure AddStylePolygone(const LS: TStylePolygone); inline;
    function  GetStylePolygone(const Idx: integer): TStylePolygone; inline;
    procedure PutStylePolygone(const Idx: integer; const LS: TStylePolygone); inline;
    function  GetNbStylesPolygones(): integer; inline;


    procedure AddStyleSymbole(const LS: TStyleSymboles); inline;
    function  GetStyleSymbole(const Idx: integer): TStyleSymboles; inline;
    procedure PutStyleSymbole(const Idx: integer; const LS: TStyleSymboles); inline;
    function  GetNbStylesSymbole(): integer; inline;


    // sauvegarde du fichier de styles
    function  SaveStylesInFile(const QFilename: string): boolean;
    function  LoadStylesFromFile(const QFilename: string): boolean;
    //--------------------------------------------------------------------------
    // MétaFiltre
    function  MetaFiltre(const Filtre: string): integer;
    //--------------------------------------------------------------------------

    //--------------------------------------------------------------------------
    // liste des super-groupes -- GENERIQUES OK
    procedure AddSuperGroupe(const G: TSuperGroupe);
    function  GetSuperGroupe(const Idx: integer): TSuperGroupe;
    procedure PutSuperGroupe(const Idx: integer; const Grp: TSuperGroupe);
    procedure DeleteSuperGroupe(const Idx: integer);
    function  GetNbSuperGroupes(): integer;


    // liste des groupes
    procedure AddGroupe(const G: TGroupeEntites; const DoCalcBoundingBox: boolean);
    function  GetNbGroupes(): integer; inline;
    function  GetMaxIdxGroupe(const WithNewGroupeCreated: boolean): TIDGroupeEntites;
    function  GetGroupe(const Idx: integer): TGroupeEntites; inline;
    function  ExtractGroupeFromIdxGroupe(const Idx: TIDGroupeEntites; out pGrp: TGroupeEntites): boolean;

    function  GetGroupeByIDGroupe(const Idx: TIDGroupeEntites): TGroupeEntites;
    procedure PutGroupe(const Idx: integer; const Grp: TGroupeEntites); inline;
    procedure DeleteGroupe(const Idx: integer); inline;
    function  GetInternalIdxGroupe(const Idx: TIDGroupeEntites): integer;
    function  ReassignerEntitesGroupes(const OldGrp, NewGrp: integer): integer;

    function  GetIndexGroupeByInternalIdx(const Idx: integer): TIDGroupeEntites;
    function  CalcGrpBoundingBox(const Grp: TGroupeEntites): TGroupeEntites;
    procedure CalcGrpBoundingBoxAndUptdateGdpByIDX(const Idx: TIDGroupeEntites);
    procedure MarquerVisibiliteOfAllGroupes(const B: boolean);
    procedure BasculerVisibiliteOfAllGroupes();

    function  TrierGroupesByZOrder(): boolean;

    // liste des scraps
    // Un scrap est un polygone appartenant à un groupe. Il est affiché avant tous les objets du groupe
    // Les propriétés essentielles d'un scrap sont:
    // - La profondeur (ZOrder)
    // - La transparence
    procedure AddScrap(const P: TScrap; const DoRecalcBBX: boolean);
    function  GetScrap(const Idx: integer): TScrap; inline;
    procedure PutScrap(const Idx: integer; const P: TScrap);
    procedure MarquerScrapAEffacer(const Idx: integer);
    procedure NettoyerScraps();
    function  CalcBoundingBoxScrap(const P: TScrap): TScrap;
    procedure DeleteScrap(const Idx: integer);
    function  GetNbScraps(): integer; inline;

    // Liste des éléments ponctuels: Gestion
    procedure AddSymbole(const EP: TSymbole; const DoRecalcBBX: boolean);
    function  GetSymbole(const Idx: integer): TSymbole; inline;
    procedure PutSymbole(const Idx: integer; const EP: TSymbole);
    procedure DeleteSymbole(const Idx: integer);
    function  GetNbSymboles(): integer; inline;


    // Liste des éléments ligne: Gestion
    procedure AddSimpleLigne(const L: TSimpleLigne; const DoRecalcBBX: boolean);
    function  GetSimpleLigne(const Idx: integer): TSimpleLigne; inline;
    procedure PutSimpleLigne(const Idx: integer; const SL: TSimpleLigne);
    procedure DeleteSimpleLigne(const Idx: integer);
    function  CalcBoundingBoxLigne(const L: TSimpleLigne): TSimpleLigne;
    function  GetNbSimpleLignes(): integer; inline;

    // Liste des éléments texte: Gestion
    procedure AddTexte(const T: TTextObject; const DoRecalcBBX: boolean);
    function  GetTexte(const Idx: integer): TTextObject; inline;
    procedure PutTexte(const Idx: integer; const T: TTextObject);
    procedure DeleteTexte(const Idx: integer);
    function  GetNbTextes(): integer; inline; inline;

    // Liste des éléments courbes: Gestion

    procedure AddCourbe(const C: TCourbe; const DoRecalcBBX: boolean);
    function  GetCourbe(const Idx: integer): TCourbe; inline;
    procedure PutCourbe(const Idx: integer; const C: TCourbe); inline;
    function  CalcBoundingBoxCourbe(const C: TCourbe): TCourbe;
    procedure DeleteCourbe(const Idx: integer);
    function  CouperCourbe(const IdxCourbe: integer; const NoPoint): boolean;
    function  GetNbCourbes(): integer; inline;
    procedure MarquerCourbesAEffacer(const Idx: integer);
    procedure NettoyerCourbes();


    // Liste des éléments Polyligne

    procedure AddPolyLigne(const P: TPolyligne; const DoRecalcBBX: boolean);
    function  GetPolyligne(const Idx: integer): TPolyligne; inline;
    procedure PutPolyligne(const Idx: integer; const P: TPolyligne);
    function  CalcBoundingBoxPolyligne(const P: TPolyligne): TPolyligne;
    procedure DeletePolyligne(const Idx: integer);
    function  GetNbPolylignes(): integer; inline;
    procedure MarquerPolylignesAEffacer(const Idx: integer);
    procedure NettoyerPolylignes();

    // Liste des polygones

    procedure AddPolygone(const P: TPolygone; const DoRecalcBBX: boolean);
    function  GetPolygone(const Idx: integer): TPolygone; inline;
    procedure PutPolygone(const Idx: integer; const P: TPolygone);
    function  CalcBoundingBoxPolygone(const P: TPolygone): TPolygone;
    procedure DeletePolygone(const Idx: integer);
    function  GetNbPolygones(): integer; inline;
    procedure MarquerPolygoneAEffacer(const Idx: integer);
    procedure NettoyerPolygones();

    // Liste des images
    procedure AddImage(var Img: TImageObject);
    function  GetImage(const Idx: integer): TImageObject; inline;
    procedure PutImage(const Idx: integer; const Img: TImageObject);
    procedure DeleteImage(const Idx: integer);
    procedure PermuterImages(const Idx1, Idx2: integer); inline;
    function  GetNbImages(): integer; inline;



    //--------------------------------------------------------------------------
    // Recherche et sélection d'objets
    function  FindObject(const X,Y: double; const ModeSelection: TModeSelectionEntites): Int64;
    // vérifier si le groupe ne contient pas des entités
    // 0 = groupe vide
    // sinon, nombre d'entités
    function  GetNbEntitiesGroupe(const GrpIdx: TIDGroupeEntites): integer;

    // calculer les boites englobantes des groupes
    procedure CalcBoundingBoxAllGroupes();
    // supprimer les objets du groupe
    procedure SupprimerObjetsGroupe(const QGroupe: TGroupeEntites);

    // export des scraps vers logiciels de SIG
    function ExporterScrapsToKML(const QFileName: string;
                                 const DoUseDefaultStyle: boolean;
                                 const DefaultColor: TColor;
                                 const DefaultOpacity: byte;
                                 const WithExportGIS: TWithExportGIS): boolean;
    function GenererCarteLeaflet(const QFileName: string;
                                 const MapWidth, MapHeight: integer;
                                 const DoUseDefaultStyle: boolean;
                                 const DefaultColor: TColor;
                                 const DefaultOpacity: byte;
                                 const WithExportGIS: TWithExportGIS): boolean;
    function ExporterScrapsToGeoJSON(const QFileName: string;
                                     const DoUseDefaultStyle: boolean;
                                     const DefaultColor: TColor;
                                     const DefaultOpacity: byte;
                                     const WithExportGIS: TWithExportGIS): boolean;


    // retourne le dossier contenant le document
    function GetDossierContenantDoc(): string;
    // réattribue les basepoints d'un objet en recherchant les basepoints métaliftrés les plus proches d'un vertex
    function ReattribBasePointsDeUnObjet(const MT: TModeSelectionEntites;
                                         const QIndexObjet: integer;
                                         const QIdxGrp: TIDGroupeEntites): boolean;
    // réattribuer tous les objets d'un groupe
    function ReattribBasepointsAllObjetsGroupe(const Grp: TGroupeEntites; const QFiltres: string): integer;
    // détruire TOUS les objets d'un groupe
    function DeleteAllObjectsOfGroupe(const Grp: TGroupeEntites): integer;
    // calcul de l'aire d'un polygone ou d'un scrap
    procedure CalcAreaPerimetrePolyOrScrap(const IDGrp: TIDGroupeEntites; const P: TVertexPolygonArray; out Area, Perimeter: double);

  private
    // convertisseur de coordonnées
    FConversionSysteme: TConversionSysteme;
    // IDX groupe courant
    FCurrentIdxGroupe  : TIDGroupeEntites;
    FCurrentGroupe     : TGroupeEntites;
    // callback pour affichage de la progression d'un travail
    FProcAfficherProgression: TProcAfficherProgression;
    // nom du document
    FDocumentName      : string;
    // dossier contenant le document
    FDossierDuDocument : string;
    // les centerlines sont dans un fichier séparé ?
    FCenterlinesIsIncludeFileGCP: boolean;
    FFichierCenterlines: TStringDirectoryFileName;
    // système de coordonnées
    FCodeEPSG          : TCodeEPSG;
    // centerlines (basepoints)
    FCenterLines       : TCenterLines;
    // mini et maxi
    FCoordsMini,
    FCoordsMaxi        : TPoint3Df;
    // Derniers objets supprimés
    FLastScrapDeleted      : TScrap;           // Dernier scrap supprimé
    FLastCourbeDeleted     : TCourbe;          // Derniere courbe supprimée
    FLastPolylineDeleted   : TPolyLigne;       // Derniere polyligne supprimée
    FLastPolygoneDeleted   : TPolygone;        // Dernier polygone supprimé
    FLastSimpleLigneDeleted: TSimpleLigne;     // Derniere ligne supprimée
    FLastSymboleDeleted    : TSymbole;         // Dernier symbole supprimé
    FLastTexteDeleted      : TTextObject;      // Dernier texte supprimé
    FLastImageDeleted      : TImageObject;     // Derniere image supprimée

    // listes des styles d'objets
    FListeLinesStyles   : TListeSimple<TStyleLigne>;
    FListeCurveStyles   : TListeSimple<TStyleCourbe>;
    FListePolygonStyles : TListeSimple<TStylePolygone>;
    FListeTextStyles    : TListeSimple<TStyleTexte>;
    FListeSymbolStyles  : TListeSimple<TStyleSymboles>;
    // listes de groupes
    FListeSuperGroupes  : TListeSimple<TSuperGroupe>;        // liste des super-groupes
    FListeGroupes       : TListeSimple<TGroupeEntites>;      // liste des groupes

    FListeScraps        : TListeSimple<TScrap>;              // liste des scraps
    FListeCourbes       : TListeSimple<TCourbe>;             // liste des courbes
    FListePolyLignes    : TListeSimple<TPolyLigne>;          // liste des polylignes
    FListeSymboles      : TListeSimple<TSymbole>;            // liste des symboles
    FListeSimplesLignes : TListeSimple<TSimpleLigne>;        // liste des objets lignes
    FListeTextObjects   : TListeSimple<TTextObject>;         // liste des textes
    FListePolygones     : TListeSimple<TPolygone>;           // liste des polygones
    FListeImages        : TListeSimple<TImageObject>;        // liste des images
    // index objet trouvé, ou -1 si inexistant
    FIdxObjectFound: Integer;
    // purge des listes
    procedure PurgerListeSuperGroupes();
    procedure PurgerListeGroupes();
    procedure PurgerListeScraps();
    procedure PurgerListeImages();
    procedure PurgerListeCourbes();
    procedure PurgerListePolylignes();
    procedure PurgerListePolygones();
    procedure PurgerListeSimplesLignes();
    procedure PurgerListeSymboles();
    procedure PurgerListeTextes();

    // extraction d'objets depuis une chaine
    function CasserUneCourbe(const PC: TCourbe; const PointCoupure: TIDBaseStation; out PC1, PC2: TCourbe): boolean;
    function CasserUnePolyligne(const PC: TPolyLigne; const PointCoupure: TIDBaseStation; out PC1, PC2: TPolyLigne): boolean;

    function ExtractArcPolyFromStr(const S: string): TArcCourbe;
    function ExtractGroupeFromStr(const S: string): TGroupeEntites;

    function ExtractObjTexteFromStr(const S: string): TTextObject;
    function ExtractSimpleLineFromStr(const NoLigneFichier: integer; const S: string): TSimpleLigne;
    function ExtractSuperGroupeFromStr(const S: string): TSuperGroupe;
    function ExtractSymbolFromStr(const S: string): TSymbole;
    function ExtractVertexPolyFromStr(const S: string): TVertexPolygon;
    function ExtractImageFromStr(const S: string): TImageObject;

    // contrôle de validité des objets
    function ScrapEstValide(const QP: TScrap): boolean;
    function SimpleLigneEstValide(const QL: TSimpleLigne): boolean;
    function CourbeEstValide(const myCourbe: TCourbe): boolean;
    function PolygoneEstValide(const QP: TPolygone): boolean;
    function PolylineEstValide(const QP: TPolyLigne): boolean;
    function SymboleEstValide(const QS: TSymbole): boolean;
    function TexteEstValide(const QT: TTextObject): boolean;


    function FusionnerDeuxCourbes(const PC1, PC2: TCourbe; out PCFusionnee: TCourbe): boolean;
    function FusionnerDeuxPolylignes(const PC1, PC2: TPolyLigne; out PCFusionnee: TPolyLigne): boolean;
    function FusionnerDeuxPolylignesByIdx(const IdxPC1, IdxPC2: integer): boolean;

    function ReattribuerBasePointsCourbe(const QIdx: integer; const QIdxGrp: TIDGroupeEntites): integer;
    function ReattribuerBasePointsPolygone(const QIdx: integer; const QIdxGrp: TIDGroupeEntites): integer;
    function ReattribuerBasePointsPolyLigne(const QIdx: integer; const QIdxGrp: TIDGroupeEntites): integer;
    function ReattribuerBasePointsScrap(const QIdx: integer; const QIdxGrp: TIDGroupeEntites): integer;
    function ReattribuerBasePointsSimpleLigne(const QIdx: integer; const QIdxGrp: TIDGroupeEntites): integer;
    function ReattribuerBasePointSymbole(const QIdx: integer; const QIdxGrp: TIDGroupeEntites): integer;
    function ReattribuerBasePointTexte(const QIdx: integer; const QIdxGrp: TIDGroupeEntites): integer;
    // convertir en tableau de points 2D une courbe de bézier au format GHCD
    function  ConvertirGHCD2StdPolygon(const P: TVertexPolygonArray; out arrPt2Df: TArrayPoints2Df): boolean;
    // convertir en tableau de points 2D une courbe de bézier
    function  ConvertirGHCD2StdBezierCourbe(const Arcs: TArcsCourbesArray; out arrPt2Df: TArrayPoints2Df): boolean;
    // purge des listes
    procedure PurgerListeStyles();
  public
    function  Initialize(): boolean;
    function  ResetAll(): boolean;
    procedure Finalize();
    // sytème de coordonnées
    function  GetConvertisseurCoordonnees(): TConversionSysteme;
    function  GetCodeEPSG(): TCodeEPSG;
    procedure SetCodeEPSG(const C: TCodeEPSG);
    // fonction de rappel pour affichage de la progression d'un travail
    procedure SetProcAfficherProgression(const P: TProcAfficherProgression);
    // nom du document
    procedure SetDocumentName(const N: string);
    // centerlines dans un fichiers externe ?
    function  CenterlinesIsIncludeFileGCP(): boolean;
    // purger toutes les listes
    procedure PurgerListeCenterlines(const CenterlinesIsIncludeFileGCP: boolean);
    procedure PurgerListesObjets();
    // rechercher une station  (versions sécurisées)
    function GetBasePointByIndex(const Idx: TIDBaseStation; out BP: TBaseStation) : boolean;
    function FindBasePoint(const Cle: string; out BP: TBaseStation): boolean;
    // paramètres d'une photo
    function  CalcPhotoData(const EP: TSymbole): TSymbole;
    procedure NettoyerObjetsProblematiques();
    // fermer une polyligne
    function FermerPolyligne(var P: TPolyLigne): boolean;
    // calcul de périmètre et de surface
    function CalcSuperficieScrap(const P: TScrap; out Superficie: double; out Perimetre: double): boolean;
    function CalcSuperficiePerimetre(const P: TVertexPolygonArray; out Superficie: double; out Perimetre: double): boolean;
    function CalcSuperficiePolygone(const P: TPolygone; out Superficie: double; out Perimetre: double): boolean;
    function CalcSuperficiePolyligne(const P: TPolyLigne; out Superficie: double; out Perimetre: double): boolean;
    // utilitaires
    procedure NettoyerGroupes;
    // Nb d'objets pour un groupe
    function GetNbCourbesPourUnGroupe(const QIdxGroupe: TIDGroupeEntites):integer;
    function GetNbScrapsPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
    function GetNbPolygonesPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
    function GetNbPolylignesPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
    function GetNbSimplesLignesPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
    function GetNbSymbolesPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
    function GetNbTextesPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;

    function GetNbObjetsPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;

    procedure CalcNbObjetsGroupes();
    // retirer le dernier objet
    function RemoveLastObject(const M: TModeSelectionEntites): integer;
    // extrait les noms de groupes visibles dans une vue
    function ExtractNomsGroupes(const LimitesVues: TRect2Df; out LS: TStringList): boolean;
    // nb de visées
    procedure GetNbVisees(out NbViseesCheminement, NbViseesAntennes: integer);


    // restauration des derniers objets effacés
    procedure RestoreLastCourbeDeleted();
    procedure RestoreLastImageDeleted();
    procedure RestoreLastPolygoneDeleted();
    procedure RestoreLastPolylineDeleted();
    procedure RestoreLastScrapDeleted();
    procedure RestoreLastSimpleLineDeleted();
    procedure RestoreLastSymboleDeleted();
    procedure RestoreLastTexteDeleted();
    // retourne le pointeur sur les centerlines
    function GetPtrCenterlines(): TCenterLines;
    function CalcSuperficieVides(const MyGroupe: TGroupeEntites): double;
    // clipping des polygones contenus dans un scrap
    procedure TrimPolygonsOfGroupe(const IdxGroupe: TIDGroupeEntites);
    function  TrimPolygonByScrap(const IdxScrap, IdxPolygone: integer): boolean;
    // générer un GCD pour un groupe désigné par son index
    procedure ExporterUnGroupeEnGCD(const IDGroupe: TIDGroupeEntites; const QFilename: TStringDirectoryFileName);
    // réorientation des courbes d'un groupe
    function ReorienterCourbesOfGroupe(const Groupe: TGroupeEntites): boolean;
    function ReorienterPolylignesOfGroupe(const Groupe: TGroupeEntites): boolean;
end;

implementation
uses
  DGCDummyUnit
  , UnitExportToODG
  , UnitGraphismesProvisoires
  , UnitFindObjects
  , unitClippingPolygonesByScrap, GeometryPolygonesUtils
  ;
const

  // noms de styles SVG
  SVG_NAME_CADRE_MASSICOT         = 'cadre-massicot';
  SVG_NAME_CADRE_PERIMETRIQUE     = 'cadre-perimetrique';
  SVG_NAME_POLYGONALE_CENTERLINE  = 'polygonale-centerline';
  SVG_NAME_POLYGONALE_SECTIONS    = 'polygonale-sections';
  SVG_NAME_CARTOUCHE_REGLE_CADRE  = 'cartouche-cadre';
  SVG_NAME_CARTOUCHE_REGLE_GRADU  = 'cartouche-gradu';
  SVG_NAME_CARTOUCHE_REGLE_TEXTE  = 'cartouche-cadre';
  SVG_NAME_MARQUE_STATION_FILL          = 'fill-marque-station';
  SVG_NAME_MARQUE_STATION_CENTREFILL    = 'fill-marque-centre-station';
  SVG_NAME_SCRAP                        = 'fill-scrap';
// constantes pour sauvegardes



// Trier les visées par ZOrder
function SortGroupesByZOrder(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TGroupeEntites;
  bidon1: double;
  bidon2: double;
begin
  E1:=Item1;
  E2:=Item2;

  bidon1 := E1^.ZOrder;
  bidon2 := E2^.ZOrder;
  if      (bidon1 < bidon2) then Result := -1
  else if (bidon1 = bidon2) then Result := 0
  else                           Result := 1;
end;

//------------------------------------------------------------------------------
function TDocumentDessin.Initialize(): boolean;
begin
  Result := False;
  FConversionSysteme := TConversionSysteme.Create;
  try
    // démarrage du convertisseur
    FConversionSysteme.Initialiser();
    // Callback pour affichage de la progression d'un travail: Nil par défaut
    FProcAfficherProgression := nil;
    // les centerlines sont dans le fichier GCD par défaut
    FCenterlinesIsIncludeFileGCP := false;
    FFichierCenterlines := '';
    FDocumentName := '';
    FCodeEPSG     := DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG;
    AfficherMessage(Format('%s.Initialize()', [ClassName]));
    // outil de recherche
    //FFindingObjects := TFindingObjects.
    // centerlines
    FCenterLines := TCenterLines.Create;

    // initialisation des styles
    FListeLinesStyles   := TListeSimple<TStyleLigne>.Create;
    FListeCurveStyles   := TListeSimple<TStyleCourbe>.Create;
    FListePolygonStyles := TListeSimple<TStylePolygone>.Create;
    FListeTextStyles    := TListeSimple<TStyleTexte>.Create;
    FListeSymbolStyles  := TListeSimple<TStyleSymboles>.Create;

    FListeSuperGroupes      := TListeSimple<TSuperGroupe>.Create;  // liste des super-groupes
    FListeGroupes           := TListeSimple<TGroupeEntites>.Create;  // liste des groupes
    FListeScraps            := TListeSimple<TScrap>.Create;  // liste des scraps
    FListeSymboles          := TListeSimple<TSymbole>.Create;  // liste éléments ponctuels (ou symboles)
    FListeCourbes           := TListeSimple<TCourbe>.Create;  // liste courbes
    FListePolyLignes        := TListeSimple<TPolyLigne>.Create;  // liste polylignes
    FListePolygones         := TListeSimple<TPolygone>.Create;  // liste des polygones
    FListeSimplesLignes     := TListeSimple<TSimpleLigne>.Create;  // liste des lignes simples
    FListeTextObjects       := TListeSimple<TTextObject>.Create;  // liste des objets texte
    FListeImages            := TListeSimple<TImageObject>.Create;

    Result := ResetAll();
  except;
  end;
end;
function TDocumentDessin.ResetAll(): boolean;
begin
  Result := false;
  // variables pour undo
  FLastScrapDeleted.IDGroupe := -1;
  FLastCourbeDeleted.IDGroupe := -1;
  FLastPolygoneDeleted.IDGroupe := -1;
  FLastPolylineDeleted.IDGroupe := -1;
  FLastSimpleLigneDeleted.IDGroupe := -1;
  FLastTexteDeleted.IDGroupe := -1;
  FLastSymboleDeleted.IDGroupe := -1;
  FLastImageDeleted.IsReady := false;

  try
    // Purge des listes
    FCenterLines.InitialiseCenterLines();
    PurgerListeStyles();
    SetDefaultStyles();
    PurgerListesObjets();
    // Groupe courant
    FCurrentIdxGroupe := 0;
    FCurrentGroupe := GetGroupe(0);
    Result := True;
  except
  end;
end;

procedure TDocumentDessin.Finalize();
begin
  AfficherMessage(Format('%s.Finalize()', [ClassName]));
  PurgerListeStyles;   // destruction tableaux de styles

  // destruction liste des éléments ponctuels
  try
    FCenterLines.FinaliseCenterLines;
    PurgerListeScraps;
    PurgerListeSuperGroupes();
    PurgerListeGroupes();
    PurgerListeSymboles();
    PurgerListeCourbes();
    PurgerListePolylignes();
    PurgerListePolygones();
    PurgerListeSimplesLignes();
    PurgerListeTextes();
    PurgerListeStyles();
    PurgerListeImages();
    FProcAfficherProgression := nil;
    FConversionSysteme.Finaliser();
  finally
    FreeAndNil(FListeScraps);//FListeScraps.Free;
    FreeAndNil(FListeLinesStyles);//FListeLinesStyles.Free;
    FreeAndNil(FListeCurveStyles);//FListeCurveStyles.Free;
    FreeAndNil(FListePolygonStyles);//FListePolygonStyles.Free;
    FreeAndNil(FListeTextStyles);//FListeTextStyles.Free;
    FreeAndNil(FListeSymbolStyles);//FListeSymbolStyles.Free;

    FreeAndNil(FCenterLines);//FCenterLines.Free;
    FreeAndNil(FListeSuperGroupes);//FListeSuperGroupes.Free;
    FreeAndNil(FListeGroupes);//FListeGroupes.Free;
    FreeAndNil(FListeSymboles);//FListeSymboles.Free;
    FreeAndNil(FListeCourbes);//FListeCourbes.Free;
    FreeAndNil(FListePolyLignes);//FListePolyLignes.Free;
    FreeAndNil(FListePolygones);//FListePolygones.Free;
    FreeAndNil(FListeSimplesLignes);//FListeSimplesLignes.Free;
    FreeAndNil(FListeTextObjects);//FListeTextObjects.Free;
    FreeAndNil(FListeImages);//FListeImages.Free;
    FreeAndNil(FConversionSysteme);//FConversionSysteme.Free;

  end;
end;

function TDocumentDessin.GetConvertisseurCoordonnees(): TConversionSysteme;
begin
  Result := FConversionSysteme;
end;

// restaure le dernier objet effacé
procedure TDocumentDessin.RestoreLastScrapDeleted();
begin
  if (FLastScrapDeleted.IDGroupe = -1) then Exit;
  self.AddScrap(FLastScrapDeleted, True);
  FLastScrapDeleted.IDGroupe := -1;
end;
procedure TDocumentDessin.RestoreLastCourbeDeleted();
begin
  if (FLastCourbeDeleted.IDGroupe = -1) then Exit;
  self.AddCourbe(FLastCourbeDeleted, True);
  FLastCourbeDeleted.IDGroupe := -1;
end;
procedure TDocumentDessin.RestoreLastPolylineDeleted();
begin
  if (FLastPolylineDeleted.IDGroupe = -1) then Exit;
  self.AddPolyLigne(FLastPolylineDeleted, True);
  FLastPolylineDeleted.IDGroupe := -1;
end;
procedure TDocumentDessin.RestoreLastPolygoneDeleted();
begin
  if (FLastPolygoneDeleted.IDGroupe = -1) then Exit;
  self.AddPolygone(FLastPolygoneDeleted, True);
  FLastPolygoneDeleted.IDGroupe := -1;
end;
procedure TDocumentDessin.RestoreLastSimpleLineDeleted();
begin
  if (FLastSimpleLigneDeleted.IDGroupe = -1) then Exit;
  self.AddSimpleLigne(FLastSimpleLigneDeleted, True);
  FLastSimpleLigneDeleted.IDGroupe := -1;
end;
procedure TDocumentDessin.RestoreLastSymboleDeleted();
begin
  if (FLastSymboleDeleted.IDGroupe = -1) then Exit;
  self.AddSymbole(FLastSymboleDeleted, True);
  FLastSymboleDeleted.IDGroupe := -1;
end;
procedure TDocumentDessin.RestoreLastTexteDeleted();
begin
  if (FLastTexteDeleted.IDGroupe = -1) then Exit;
  self.AddTexte(FLastTexteDeleted, True);
  FLastTexteDeleted.IDGroupe := -1;
end;
procedure TDocumentDessin.RestoreLastImageDeleted();
begin
  if (not FLastImageDeleted.Displayed) then Exit;
  self.AddImage(FLastImageDeleted);
  FLastImageDeleted.Displayed := false;
end;
// tri par index de la table de basestations
procedure TDocumentDessin.TrierBasePointsParIDBaseStation();
begin
  FCenterLines.TrierBasePointsParIDBaseStation();
end;

function TDocumentDessin.TrierGroupesByZOrder(): boolean;
begin
  Result := False;
  AfficherMessage(Format('%s.TrierGroupesByZOrder', [ClassName]));
  FListeGroupes.Sort(SortGroupesByZOrder);
end;



// MétaFiltre
function  TDocumentDessin.MetaFiltre(const Filtre: string): integer;
begin
  FCenterLines.MetaFiltrer(Filtre);
end;

procedure TDocumentDessin.NettoyerCourbes();
var
  i, n: Integer;
  EWE: TCourbe;
begin
  AfficherMessage(Format('%s.NettoyerCourbes: %d', [ClassName, GetNbCourbes()]));
  i := 0;
  n := GetNbCourbes();
  for i := n - 1 downto 0 do
  begin
    EWE := GetCourbe(i);
    if (EWE.MarkToDelete) then DeleteCourbe(i);
  end;
  AfficherMessage(Format('-- %d courbes deleted', [n - GetNbCourbes()]));
end;


// importer un fichier de basepoints
function  TDocumentDessin.ImportBasePoints(const FichierGCP: TStringDirectoryFileName): boolean;
begin
  try
    Result := FCenterLines.ImporterBasePoints(FichierGCP);
    // mini et maxi
    SetMiniEtMaxi;
    // tri
    TrierBasePointsParIDBaseStation();
    Result := True;
  except
  end;
end;

function TDocumentDessin.ExportBasePoints(const FichierGCP: TStringDirectoryFileName): boolean;
begin
  Result := FCenterLines.ExporterBasePoints(FichierGCP, self.GetDocumentName());
end;



// calculer les boites englobantes des groupes
procedure TDocumentDessin.CalcBoundingBoxAllGroupes();
var
  i: integer;
  Grp: TGroupeEntites;
  Nb: Integer;
begin
  Nb := GetNbGroupes;
  AfficherMessage(Format('%s.CalcBoundingBoxAllGroupes(%d groupes)',[ClassName, Nb]));
  for i := 0 to Nb - 1 do
  begin
    try
      Grp := CalcGrpBoundingBox(GetGroupe(i));
      PutGroupe(i, Grp);
    except
      on E:Exception do AfficherMessageErreur(Format('--Groupe: %d (%s) - Erreur: %s', [Grp.IDGroupeEntites, Grp.NomGroupe, E.Message]));
    end;
  end;
end;



//******************************************************************************
function TDocumentDessin.ExtractSuperGroupeFromStr(const S: string): TSuperGroupe;
var
  EWE: TGHStringArray;

begin
  EWE := Split(S, #9);
  // Le mot clé est dans l'argument 0
  // Parameters: IDSuperGroupe, NomGroupe, Offset XYZ components, Displayed, Locked
  // supergroupe	0	SuperGroupe_0	1	1	0.00	0.00	0.00
  //Result.ID        := StrToIntDef(EWE[1], 0);
  Result.NomSuperGroupe := Trim(EWE[2]);
  Result.Displayed := (StrToIntDef(EWE[3], 1) = 1);
  //Result.Locked    := (StrToIntDef(EWE[4], 0) = 0);
   Result.ListeGroupes := StrToArrayOfIdxGroupes(EWE[8]);
end;
function TDocumentDessin.ExtractGroupeFromStr(const S: string): TGroupeEntites;
var
  EWE: TGHStringArray;
begin
  Result.DecalageActif := True;
  EWE := Split(S, #9);
           //groupe	0	Groupe0	16711680	0.00	0.00	0.00
           //  groupe	0	Groupe0	16711680	0.00	0.00	0.00	4516452057324008	29/06/2016 15:44:43	1	0.00
           //  //  groupe	18	Méga Scrap du Charentais	4210816	0.00	0.00	0.00	0	24/05/2013 00:05:42	1	180.00
           // Le mot clé est dans l'argument 0
  Result.IDGroupeEntites := StrToIntDef(EWE[1], 0);
  Result.NomGroupe     := Trim(EWE[2]);
  Result.CouleurGroupe := TColor(StrToIntDef(EWE[3], 0));
  Result.Decalage.setFrom(ConvertirEnNombreReel(EWE[4], 0.00),
                          ConvertirEnNombreReel(EWE[5], 0.00),
                          ConvertirEnNombreReel(EWE[6], 0.00));

  //Result.IDSuperGroupe := 0; // StrToIntDef(EWE[7], 0);
  Result.Visible       := True;
  //Result.Locked        := False;
  Result.DateLastModif := StrToDateTimeDef(EWE[8], Now());
  Result.DecalageActif := (StrToIntDef(EWE[9], 1) = 1); // décalage actif par défault
  Result.ZOrder        := ConvertirEnNombreReel(EWE[10], Result.IDGroupeEntites * 10.00);
  Result.Filtres       := Trim(EWE[11]);
  //AfficherMessage(Format('%d %s %f %f %f',[G.IDGroupeEntites, G.NomGroupe, G.Decalage.X, G.Decalage.Y, G.Decalage.Z]));
end;

function TDocumentDessin.ExtractImageFromStr(const S: string): TImageObject;
var
  EWE: TGHStringArray;
begin
  EWE := Split(S, #9);
  //     0    1   2         3          4          5          6     7               8
  //   bitmap	0	401091.26	3278578.06	401174.81	3278665.81	128	photo_bidon_3.jpg
  Result.Displayed             := True;
  Result.ConteneurImg          := nil; // ce champ est un pointeur TBGRABitmap
  Result.PositionCoinsImage.X1 := ConvertirEnNombreReel(EWE[2], -1.00);
  Result.PositionCoinsImage.Y1 := ConvertirEnNombreReel(EWE[3], -1.00);
  Result.PositionCoinsImage.X2 := ConvertirEnNombreReel(EWE[4], -1.00);
  Result.PositionCoinsImage.Y2 := ConvertirEnNombreReel(EWE[5], -1.00);
  Result.Opacite               := StrToIntDef(EWE[6], 255);
  Result.SrcFilename           := Trim(EWE[7]); // FDossierDuDocument + PathDelim + Trim(EWE[7]);
  //AfficherMessageErreur(Result.SrcFilename);
  Result.Description           := Trim(EWE[8]);
end;

function TDocumentDessin.ExtractArcPolyFromStr(const S: string): TArcCourbe;
var
  EWE: TGHStringArray;
begin
  EWE := Split(S, #9);
  //AfficherMessage('---> 666 >'+ LS.Strings[i]);
  //arc	200030	0.48	-2.59	0.00	0.30	1.06	-1.00	200030	1.39	0.59	0.00	0.05	-1.10	-2.00
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  Result.IDStationP1 := Trim(EWE[1]);
  {$ELSE}
  Result.IDStationP1 := StrToInt64Def(EWE[1], -1);
  {$ENDIF TIDBASEPOINT_AS_TEXT}

  Result.OffsetP1.setFrom(ConvertirEnNombreReel(EWE[2], 0.00),
                          ConvertirEnNombreReel(EWE[3], 0.00),
                          ConvertirEnNombreReel(EWE[4], 0.00));
  Result.TangP1.setFrom(  ConvertirEnNombreReel(EWE[5], 0.00),
                          ConvertirEnNombreReel(EWE[6], 0.00),
                          0.00);
  // argument 7 zappé
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  Result.IDStationP2 := Trim(EWE[8]);
  {$ELSE}
  Result.IDStationP2 := StrToInt64Def(EWE[8], -1);
  {$ENDIF TIDBASEPOINT_AS_TEXT}

  Result.OffsetP2.setFrom(ConvertirEnNombreReel(EWE[9], 0.00),
                          ConvertirEnNombreReel(EWE[10], 0.00),
                          ConvertirEnNombreReel(EWE[11], 0.00));
  Result.TangP2.setFrom(  ConvertirEnNombreReel(EWE[12], 0.00),
                          ConvertirEnNombreReel(EWE[13], 0.00),
                          0.00);
end;
function TDocumentDessin.ExtractVertexPolyFromStr(const S: string): TVertexPolygon;
var
  EWE: TGHStringArray;
begin
  EWE := Split(S, #9);
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  Result.IDStation := EWE[1];
  {$ELSE}
  Result.IDStation := StrToInt64Def(EWE[1], -1);
  {$ENDIF TIDBASEPOINT_AS_TEXT}

  Result.Offset.setFrom(ConvertirEnNombreReel(EWE[2], 0.00),
                        ConvertirEnNombreReel(EWE[3], 0.00),
                        ConvertirEnNombreReel(EWE[4], 0.00));
end;
function TDocumentDessin.ExtractSimpleLineFromStr(const NoLigneFichier: integer; const S: string): TSimpleLigne;
var
  EWE: TGHStringArray;
  WU: Integer;
begin
  EWE := Split(S, #9);
  Result.IDGroupe      := StrToIntDef(EWE[2], 0);
  Result.IDStyleLigne  := TNatureObjetLigne(StrToIntDef(EWE[3], 0));
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  Result.IDBaseStExt1 := EWE[4];
  {$ELSE}
  Result.IDBaseStExt1  := StrToIntDef(EWE[4], 0);
  {$ENDIF TIDBASEPOINT_AS_TEXT}
  Result.OffsetExtr1.setFrom(ConvertirEnNombreReel(EWE[5], 0.00),
                             ConvertirEnNombreReel(EWE[6], 0.00),
                             ConvertirEnNombreReel(EWE[7], 0.00));
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  Result.IDBaseStExt2 := EWE[8];
  {$ELSE}
  Result.IDBaseStExt2  := StrToIntDef(EWE[8], 0);
  {$ENDIF TIDBASEPOINT_AS_TEXT}

  Result.OffsetExtr2.setFrom(ConvertirEnNombreReel(EWE[9], 0.00),
                             ConvertirEnNombreReel(EWE[10], 0.00),
                             ConvertirEnNombreReel(EWE[11], 0.00));
  Result.MarkToDelete  := false;
  Result.LastModified  := StrToDateTimeDef(EWE[12], Now());
  WU := Ord(Result.IDStyleLigne);
  if (WU > 4) then AfficherMessageErreur(Format('[%d]: Invalid linestyle: %d ; max = %d', [NoLigneFichier, WU, High(Result.IDStyleLigne)]));
end;
function TDocumentDessin.ExtractSymbolFromStr(const S: string): TSymbole;
var
  EWE: TGHStringArray;
  WU: Integer;
begin
  EWE := Split(S, #9);
  Result.IDGroupe      := StrToIntDef(EWE[2], 0);
  Result.TypeObject    := TNatureObjetSymbole(StrToIntDef(EWE[3], 0));
  Result.Couleur       := StrToIntDef(EWE[4], 0);
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  Result.IDBaseStation := EWE[5];
  {$ELSE}
  Result.IDBaseStation := StrToInt64Def(EWE[5], 0);
  {$ENDIF TIDBASEPOINT_AS_TEXT}

  Result.Offset.setFrom(ConvertirEnNombreReel(EWE[6], 0.00),
                        ConvertirEnNombreReel(EWE[7], 0.00),
                        ConvertirEnNombreReel(EWE[8], 0.00));
  Result.AngleRot      := ConvertirEnNombreReel(EWE[9], 0.00);
  Result.ScaleX        := ConvertirEnNombreReel(EWE[10], 1.00);
  Result.ScaleY        := ConvertirEnNombreReel(EWE[11], 1.00);
  Result.TagTexte        := Trim(EWE[12]);
  Result.UnTag           := StrToIntDef(EWE[13], 1);
  Result.PhotoDisplayed  := (StrToIntDef(EWE[14], 1) = 1);
  Result.LastModified  := StrToDateTimeDef(EWE[15], Now());
  Result.MarkToDelete  := false;
  if (Result.TypeObject = nosPHOTO) then Result := CalcPhotoData(Result); // photo ?
  WU := Ord(Result.TypeObject);

end;
function TDocumentDessin.ExtractObjTexteFromStr(const S: string): TTextObject;
var
  EWE: TGHStringArray;
begin
  EWE := Split(S, #9);
  Result.IDGroupe      := StrToIntDef(EWE[2], 0);
  Result.IDStyleTexte  := TNatureObjetTexte(StrToIntDef(EWE[3], 0));
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  Result.IDBaseSt      := EWE[4];
  {$ELSE}
  Result.IDBaseSt      := StrToInt64Def(EWE[4], 0);
  {$ENDIF TIDBASEPOINT_AS_TEXT}

  Result.Offset.setFrom(ConvertirEnNombreReel(EWE[5], 0.00),
                        ConvertirEnNombreReel(EWE[6], 0.00),
                        ConvertirEnNombreReel(EWE[7], 0.00));
  Result.Alignment     := StrToIntDef(EWE[8], 0);
  Result.MaxLength     := StrToIntDef(EWE[9], -1);
  Result.Text          := Trim(EWE[10]);
  Result.MarkToDelete  := false;
  Result.LastModified  := StrToDateTimeDef(EWE[11], Now());
end;
// cette fonction est volumineuse --> déportée dans DocDessinLoadFromFile.inc
//function TDocumentDessin.LoadFromFile(const FichierGCD: string): boolean;
{$INCLUDE DocDessinLoadFromFile.inc}
// sauvegarde
//procedure TDocumentDessin.SaveToFile(const FichierGCD: string; const GroupeExported: integer; const DoWriteCenterlinesSection: boolean);
// cette fonction est volumineuse --> déportée dans DocDessinSaveToFile.inc
{$INCLUDE DocDessinSaveToFile.inc}
//------------------------------------------------------------------------------
procedure TDocumentDessin.SetMiniEtMaxi();
const
  FMT0 = '%s: X = %12.2f Y = %12.2f Z = %12.2f';
  MARGE = 20.00;
var
  i: integer;
  BP: TBaseStation;
begin
  AfficherMessage(Format('%s.SetMiniEtMaxi()', [ClassName]));
  FCoordsMini.setFrom( INFINITE,  INFINITE,  INFINITE);
  FCoordsMaxi.setFrom(-INFINITE, -INFINITE, -INFINITE);
  for i:= 0 to FCenterLines.GetNbBasePoints - 1 do
  begin
    BP := FCenterLines.GetBasePoint(i);
    if (BP.PosStation.X <  FCoordsMini.X ) then  FCoordsMini.X := BP.PosStation.X;
    if (BP.PosStation.Y <  FCoordsMini.Y ) then  FCoordsMini.Y := BP.PosStation.Y;
    if (BP.PosStation.Z <  FCoordsMini.Z ) then  FCoordsMini.Z := BP.PosStation.Z;
    if (BP.PosStation.X >  FCoordsMaxi.X ) then  FCoordsMaxi.X := BP.PosStation.X;
    if (BP.PosStation.Y >  FCoordsMaxi.Y ) then  FCoordsMaxi.Y := BP.PosStation.Y;
    if (BP.PosStation.Z >  FCoordsMaxi.Z ) then  FCoordsMaxi.Z := BP.PosStation.Z;
  end;
  // marges
  FCoordsMini.X  :=  FCoordsMini.X - MARGE;
  FCoordsMini.Y  :=  FCoordsMini.Y - MARGE;
  FCoordsMini.Z  :=  FCoordsMini.Z - MARGE;
  FCoordsMaxi.X  :=  FCoordsMaxi.X + MARGE;
  FCoordsMaxi.Y  :=  FCoordsMaxi.Y + MARGE;
  FCoordsMaxi.Z  :=  FCoordsMaxi.Z + MARGE;
end;

procedure TDocumentDessin.SetProcAfficherProgression(const P: TProcAfficherProgression);
begin
  FProcAfficherProgression := P;
end;

function TDocumentDessin.GetCoordsMini(): TPoint3Df;
begin
  Result := FCoordsMini;
end;
function TDocumentDessin.GetCoordsMaxi(): TPoint3Df;
begin
  Result := FCoordsMaxi;
end;
//------------------------------------------------------------------------------
// attraper un élément de la liste des points de base
// Recherche infructueuse => Result = 0
function TDocumentDessin.GetBaseStation(const Idx: integer): TBaseStation;
begin
  try
    Result := FCenterLines.GetBasePoint(Idx);
  except
    {$IFDEF TIDBASEPOINT_AS_TEXT}
    Result.IDStation := '';
    {$ELSE}
    Result.IDStation := 0;
    {$ENDIF TIDBASEPOINT_AS_TEXT}
  end;
end;

function TDocumentDessin.GetCodeEPSG(): TCodeEPSG;
begin
  Result := FCodeEPSG;
end;

// Recherche infructueuse => Result = 0
function TDocumentDessin.GetBasePointByIndex(const Idx: TIDBaseStation; out BP: TBaseStation) : boolean;
begin
  Result := FCenterLines.FindBasePointByIndex(Idx, BP);
end;
// recherche de station
function TDocumentDessin.FindBasePoint(const Cle: string; out BP: TBaseStation): boolean;
var
  i, n: Integer;
  S, S1, S2: string;
  EWE: TBaseStation;
  WU: SizeInt;
  qSer, qSt: Integer;
  Idx: TIDBaseStation;
begin
  Result := FCenterLines.FindBasePointByCle(Cle, BP);
end;

// attraper l'élément point le plus proche d'un point (x, y)
function TDocumentDessin.GetNearBasepoint(const X, Y: double; const OldBasePoint: TBaseStation; const StLocked: boolean): TBaseStation;
begin
  Result := FCenterLines.GetNearBasePointA(X, Y, OldBasePoint, StLocked);
end;

function TDocumentDessin.GetNbBaseStations(): integer;
begin
  Result := FCenterLines.GetNbBasePoints();
end;

// nombre de visées en antenne
procedure TDocumentDessin.GetNbVisees(out NbViseesCheminement, NbViseesAntennes: integer);
var
  i, Nb: Integer;
  EWE: TBaseStation;
begin
  NbViseesCheminement := 0;
  NbViseesAntennes    := 0;
  Nb     := self.GetNbBaseStations();
  for i := 0 to Nb - 1 do
  begin
    EWE := self.GetBaseStation(i);
    {$IFDEF TIDBASEPOINT_AS_TEXT}
    if (EWE.IDStation[1] = '-')
    {$ELSE}
    if (EWE.IDStation < 0)
    {$ENDIF TIDBASEPOINT_AS_TEXT}
    then NbViseesAntennes     += 1 // antennes
                           else NbViseesCheminement  += 1;
  end;
end;



function TDocumentDessin.GetNbCourbes(): integer;
begin
  Result := FListeCourbes.GetNbElements();
end;
procedure TDocumentDessin.AddABasePoint(const BP: TBaseStation);
begin
  FCenterLines.AddBasePoint(BP);
end;

procedure TDocumentDessin.AddABasePointFromString(const S: String); inline;
begin
  FCenterLines.AddBasePoint(FCenterLines.ExtractBasePointFromLine(S));
end;
//******************************************************************************
// SCRAPS
procedure TDocumentDessin.PurgerListeScraps();
var
  Nb: integer;
begin
  Nb := GetNbScraps();
  AfficherMessage(Format('%s.PurgerListeScraps() - %d elements',[ClassName, Nb]));
  FListeScraps.ClearListe();
end;

procedure TDocumentDessin.AddScrap(const P: TScrap; const DoRecalcBBX: boolean);
var
  pP: TScrap;
begin
  if (not ScrapEstValide(P)) then Exit;
  pP := P;
  pP := CalcBoundingBoxScrap(pP);
  CalcAreaPerimetrePolyOrScrap(P.IDGroupe, P.Sommets, pP.Area, pP.Perimeter);
  pP.MarkToDelete := false;
  FListeScraps.AddElement(pP);
  if (DoRecalcBBX) then CalcGrpBoundingBoxAndUptdateGdpByIDX(pP.IDGroupe);
end;

function TDocumentDessin.GetScrap(const Idx: integer): TScrap;
begin
 Result := FListeScraps.GetElement(Idx);
end;

procedure TDocumentDessin.PutScrap(const Idx: integer; const P: TScrap);
var
 PP: TScrap;
begin
 PP := P;
 CalcAreaPerimetrePolyOrScrap(pP.IDGroupe, pP.Sommets, PP.Area, pP.Perimeter);
 FListeScraps.PutElement(Idx, PP);
 CalcGrpBoundingBoxAndUptdateGdpByIDX(PP.IDGroupe);
end;
//------------------------------------------------------------------------------
procedure TDocumentDessin.MarquerScrapAEffacer(const Idx: integer);
var
  EWE: TScrap;
begin
  EWE := GetScrap(Idx);
  EWE.MarkToDelete := True;
  PutScrap(Idx, EWE);
end;
procedure TDocumentDessin.NettoyerScraps();
var
  i, n: Integer;
  EWE: TScrap;
begin
  AfficherMessage(Format('%s.NettoyerScraps: %d scraps', [ClassName, GetNbScraps()]));
  i := 0;
  n :=  GetNbScraps();
  for i := n - 1 downto 0 do
  begin
    EWE := GetScrap(i);
    if (EWE.MarkToDelete) then DeleteScrap(i);
  end;
  AfficherMessage(Format('-- %d scraps deleted', [n - GetNbScraps()]));
  CalcBoundingBoxAllGroupes;
end;

procedure TDocumentDessin.PermuterImages(const Idx1, Idx2: integer);
begin
  FListeImages.ExchangeElements(Idx1, Idx2);
end;

procedure TDocumentDessin.MarquerPolygoneAEffacer(const Idx: integer);
var
  EWE: TPolygone;
begin
  EWE := GetPolygone(Idx);
  EWE.MarkToDelete := True;
  PutPolygone(Idx, EWE);
end;
procedure TDocumentDessin.NettoyerPolygones();
var
  i, n: Integer;
  EWE: TPolygone;
begin
  AfficherMessage(Format('%s.NettoyerPolygones: %d', [ClassName, GetNbPolygones()]));
  i := 0;
  n := GetNbPolygones();
  for i := n - 1 downto 0 do
  begin
    EWE := GetPolygone(i);
    if (EWE.MarkToDelete) then DeletePolygone(i);
  end;
  AfficherMessage(Format('-- %d polygones deleted', [n - GetNbPolygones()]));
  CalcBoundingBoxAllGroupes;
end;

procedure TDocumentDessin.PurgerListeImages();
var
  i, NbObj: Integer;
  EWE: TImageObject;
begin
  NbObj := GetNbImages();
  AfficherMessage(Format('%s.PurgerListeImages() - %d elements', [ClassName, NbObj]));
  if (NbObj = 0) then AfficherMessage('--> Aucune entite')
  else
  begin
    for i:=0 to NbObj - 1 do
    begin
      EWE := GetImage(i); // récupération de l'image pour détruire son conteneur
      try
        FreeAndNil(EWE.ConteneurImg);//     EWE.ConteneurImg.Free;
      except
      end;
    end;
  end;
  FListeImages.ClearListe();
end;
// Particularité: Le conteneur de l'image est créé ici et
// nécessite de modifier la variable Img
// L'image doit être présente dans le dossier contenant le document
// Semble OK
// En cas d'erreur du chargement de l'image, on ajoute quand même l'objet
// (on conserve ainsi les paramètres de l'image si le fichier image est absent)
procedure TDocumentDessin.AddImage(var Img: TImageObject);
var
  pImg: TImageObject;
  WU: String;
begin
  try
    // création du conteneur et chargement de l'image
    WU := FDossierDuDocument + PathDelim + img.SrcFilename;

    Img.ConteneurImg := nil;
    Img.IsReady := false;
    if (FileExists(WU)) then
    begin
      try
        Img.ConteneurImg := TBGRABitmap.Create(WU);
        Img.IsReady := True;
      except
        Img.Description := '/!\ IMAGE LOAD ERROR ! - ' + Img.Description;
      end;
    end
    else
      Img.Description := '/!\ IMAGE NOT FOUND ! - ' + Img.Description;
    pImg := Img;
    FListeImages.AddElement(pImg);
  except
  end;
end;

function TDocumentDessin.GetImage(const Idx: integer): TImageObject;
begin
  Result :=  FListeImages.GetElement(Idx);
end;

procedure TDocumentDessin.PutImage(const Idx: integer; const Img: TImageObject);
begin
  FListeImages.PutElement(Idx, Img);
end;

procedure TDocumentDessin.DeleteImage(const Idx: integer);
var
  EWE: TImageObject;
begin
  // on récupère l'image pour détruire son conteneur BGRA
  EWE := GetImage(Idx);
  FLastImageDeleted := EWE; // mise à jour de la variable pour undo
  try
    FreeAndNil(EWE.ConteneurImg);//     EWE.ConteneurImg.Free;
  except
  end;
  FListeImages.RemoveElement(Idx);
end;

function TDocumentDessin.GetNbImages(): integer;
begin
  Result := FListeImages.GetNbElements();
end;

function TDocumentDessin.GetNbObjetsPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
var
  NbScraps: Integer;
  NbCourbes: Integer;
  NbPolylignes: Integer;
  NbPolygones: Integer;
  NbSimplesLignes: Integer;
  NbSymboles: Integer;
  NbTextes: Integer;
begin
  NbScraps       := self.GetNbScrapsPourUnGroupe(QIdxGroupe);
  NbCourbes      := self.GetNbCourbesPourUnGroupe(QIdxGroupe);
  NbPolylignes   := self.GetNbPolylignesPourUnGroupe(QIdxGroupe);
  NbPolygones    := self.GetNbPolygonesPourUnGroupe(QIdxGroupe);
  NbSimplesLignes:= self.GetNbSimplesLignesPourUnGroupe(QIdxGroupe);
  NbSymboles     := self.GetNbSymbolesPourUnGroupe(QIdxGroupe);
  NbTextes       := self.GetNbTextesPourUnGroupe(QIdxGroupe);
  Result         := NbScraps + NbCourbes + NbPolylignes + NbPolygones +
                    NbSimplesLignes + NbSymboles + NbTextes;

end;
procedure TDocumentDessin.CalcNbObjetsGroupes();
var
  i, Nb, NbObjets: Integer;
  Grp: TGroupeEntites;
begin
  Nb := GetNbGroupes();
  AfficherMessage(Format('%s.CalcNbObjetsGroupes: %d', [ClassName, Nb]));
  for i := 0 to Nb - 1 do
  begin
    Grp := GetGroupe(i);
    NbObjets := GetNbObjetsPourUnGroupe(Grp.IDGroupeEntites);
    Grp.NombreObjets := NbObjets;
    PutGroupe(i, Grp);
  end;
end;

procedure TDocumentDessin.MarquerPolylignesAEffacer(const Idx: integer);
var
  EWE: TPolyLigne;
begin
  EWE := GetPolyligne(Idx);
  EWE.MarkToDelete := True;
  PutPolyligne(Idx, EWE);
end;
procedure TDocumentDessin.NettoyerPolylignes();
var
  i, n: Integer;
  EWE: TPolyLigne;
begin
  AfficherMessage(Format('%s.NettoyerPolyglignes: %d', [ClassName, GetNbPolylignes()]));
  i := 0;
  n := GetNbPolylignes();
  for i := n - 1 downto 0 do
  begin
    EWE := GetPolyligne(i);
    if (EWE.MarkToDelete) then DeletePolyligne(i);
  end;
  AfficherMessage(Format('-- %d polyglines deleted', [n - GetNbPolylignes()]));
  CalcBoundingBoxAllGroupes;
end;
//------------------------------------------------------------------------------


function TDocumentDessin.CalcBoundingBoxScrap(const P: TScrap): TScrap;
var
  errCode: Integer;
  i: Integer;
  V: TVertexPolygon;
  PT: TPoint2Df;
begin
  errCode := 0;
  Result := P;

  Result.BoundingBox.Reset();
  for i := 0 to High(P.Sommets) do
  begin
    V  := P.Sommets[i];
    GetCoordsGCSWithoutGroupe(V.IDStation, V.Offset, PT, ErrCode);
    if (ErrCode = -1) then Exit;
    result.BoundingBox.updateFromPoint(PT);
  end;
end;
procedure TDocumentDessin.DeleteScrap(const Idx: integer);
begin
  FLastScrapDeleted := GetScrap(Idx); // mise à jour de la variable pour undo
  FListeScraps.RemoveElement(Idx);
end;



//-GROUPES ---------------------------------------------------------------------
procedure TDocumentDessin.PurgerListeGroupes();
var  i, Nb: integer;
begin
  Nb := GetNbGroupes();
  AfficherMessage(Format('%s.PurgerListeGroupes - %d elements',[ClassName, Nb]));
  FListeGroupes.ClearListe();
end;
procedure TDocumentDessin.AddGroupe(const G: TGroupeEntites; const DoCalcBoundingBox: boolean);
var
  WU: TGroupeEntites;
begin
  WU := G;
  if (DoCalcBoundingBox) then WU := CalcGrpBoundingBox(WU);
  FListeGroupes.AddElement(WU);
end;
function  TDocumentDessin.GetGroupe(const Idx: integer): TGroupeEntites;
begin
  Result := FListeGroupes.GetElement(Idx);
end;
function  TDocumentDessin.GetIndexGroupeByInternalIdx(const Idx: integer): TIDGroupeEntites;
var
  GRP: TGroupeEntites;
begin
  Result := -1;
  try
    GRP := GetGroupe(Idx);
    Result := GRP.IDGroupeEntites;
  except
  end;
end;

function TDocumentDessin.GetGroupeByIDGroupe(const Idx: TIDGroupeEntites): TGroupeEntites;
  procedure MiouMiou(const Miou: TIDGroupeEntites);
  var
    pGRP: TGroupeEntites;
    i, Nb: Integer;
  begin
    pGRP := FListeGroupes.GetElement(0);
    Nb := FListeGroupes.GetNbElements();
    for i := 0 to Nb - 1 do
    begin
      pGRP := FListeGroupes.GetElement(i);
      if (Miou = pGRP.IDGroupeEntites) then
      begin
        Result := pGRP;
        FCurrentGroupe := pGRP;
        FCurrentIdxGroupe := pGRP.IDGroupeEntites;
        break;
      end;
    end;
  end;
begin
  // On attrappe TOUJOURS le groupe 0 sans optimisation
  (*
  if (0 = Idx) then
  begin
    MiouMiou(0);
    exit;
  end;
  //*)
  // Optimisation: ne rechercher le groupe que si l'index courant n'a pas été modifié
  // (vitesse de reconstruction du dessin doublée)
  if (Idx <> FCurrentIdxGroupe) then MiouMiou(Idx);
  Result := FCurrentGroupe;
end;

function TDocumentDessin.ExtractGroupeFromIdxGroupe(const Idx: TIDGroupeEntites; out pGrp: TGroupeEntites): boolean;
var
  i, Nb: integer;
begin
  result := false;
  Nb := FListeGroupes.GetNbElements();
  for i := 0 to Nb - 1 do
  begin
    pGRP := FListeGroupes.GetElement(i);
    if (Idx = pGRP.IDGroupeEntites) then exit(True);
  end;
end;

procedure TDocumentDessin.PutGroupe(const Idx: integer; const Grp: TGroupeEntites);
begin
  FListeGroupes.PutElement(Idx, Grp);
end;

procedure TDocumentDessin.DeleteGroupe(const Idx: integer);
begin
  FListeGroupes.RemoveElement(Idx);
end;

function  TDocumentDessin.CalcGrpBoundingBox(const Grp: TGroupeEntites): TGroupeEntites;
var
  dx, dy: double;
  i : integer;
  CC: TCourbe;
  PP: TPolygone;
  LL: TSimpleLigne;
  TT: TTextObject;
  NbObj: Integer;
  SC: TScrap;
  PL: TPolyLigne;
  function GrandBoundingBox(const BoxTeste, BoxInitial: TBoundingBox): TBoundingBox; overload;
  begin
    Result := BoxInitial;
    if (BoxTeste.C1.X < BoxInitial.C1.X) then Result.C1.X := BoxTeste.C1.X;
    if (BoxTeste.C1.Y < BoxInitial.C1.Y) then Result.C1.Y := BoxTeste.C1.Y;
    if (BoxTeste.C2.X > BoxInitial.C2.X) then Result.C2.X := BoxTeste.C2.X;
    if (BoxTeste.C2.Y > BoxInitial.C2.Y) then Result.C2.Y := BoxTeste.C2.Y;
  end;
  function GrandBoundingBox(const X, Y: double; const BoxInitial: TBoundingBox): TBoundingBox; overload;
  begin
    Result := BoxInitial;
    if (X < BoxInitial.C1.X) then Result.C1.X := X;
    if (Y < BoxInitial.C1.Y) then Result.C1.Y := Y;
    if (X > BoxInitial.C2.X) then Result.C2.X := X;
    if (Y > BoxInitial.C2.Y) then Result.C2.Y := Y;
  end;
  function ExpandBoundingBoxByText(const OT: TTextObject; const BoxInitial: TBoundingBox): TBoundingBox;
  var
    SP: TPoint2Df;
    GP: TGroupeEntites;
    BS: TBaseStation;
  begin
    if (GetBasePointByIndex(OT.IDBaseSt, BS)) then
    begin;
      GP := GetGroupeByIDGroupe(OT.IDGroupe);
      SP := MakeTPoint2DfWithGroupeAndBaseStation(BS, GP, 0.00, 0.00);
      Result := GrandBoundingBox(SP.X, SP.Y, BoxInitial);
    end;
  end;
begin
  Result := Grp;
  // box initiale = carré unitaire au centre du dessin
  Result.BoundingBox.Reset();
  // parcours des tables
  NbObj := GetNbScraps();
  if (NbObj > 0) then begin // scraps
    for i:= 0 to NbObj - 1 do
    begin
      SC := GetScrap(i);
      if (SC.IDGroupe = Grp.IDGroupeEntites) then
        Result.BoundingBox := GrandBoundingBox(SC.BoundingBox, Result.BoundingBox);
    end;
  end;
  NbObj := GetNbCourbes();
  if (NbObj > 0) then begin // courbes
    for i:= 0 to NbObj - 1 do
    begin
      CC := GetCourbe(i);
      if (CC.IDGroupe = Grp.IDGroupeEntites) then
        Result.BoundingBox := GrandBoundingBox(CC.BoundingBox, Result.BoundingBox);
    end;
  end;
  NbObj := GetNbPolylignes();
  if (NbObj > 0) then begin // polylignes
    for i:= 0 to NbObj - 1 do
    begin
      PL := GetPolyligne(i);
      if (PL.IDGroupe = Grp.IDGroupeEntites) then
        Result.BoundingBox := GrandBoundingBox(PL.BoundingBox, Result.BoundingBox);
    end;
  end;
  NbObj := GetNbPolygones();
  if (NbObj > 0) then
  begin // polygones
    for i:= 0 to NbObj - 1 do begin
      PP := GetPolygone(i);
      if (PP.IDGroupe = Grp.IDGroupeEntites) then
        Result.BoundingBox := GrandBoundingBox(PP.BoundingBox, Result.BoundingBox);
    end;
  end;
  NbObj := GetNbSimpleLignes();
  if (NbObj > 0) then
  begin // lignes
    for i:= 0 to NbObj - 1 do
    begin
      LL := GetSimpleLigne(i);
      if (LL.IDGroupe = Grp.IDGroupeEntites) then
        Result.BoundingBox := GrandBoundingBox(LL.BoundingBox, Result.BoundingBox);
    end;
  end;
  NbObj := GetNbTextes();
  if (NbObj > 0) then
  begin // textes isolés
    for i:= 0 to NbObj - 1 do
    begin
      TT := GetTexte(i);
      if (TT.IDGroupe = Grp.IDGroupeEntites) then Result.BoundingBox := ExpandBoundingBoxByText(TT, Result.BoundingBox);
    end;
  end;
end;

procedure TDocumentDessin.CalcGrpBoundingBoxAndUptdateGdpByIDX(const Idx: TIDGroupeEntites);
var
  EWE: TGroupeEntites;
  n: Integer;
begin
  n := GetInternalIdxGroupe(Idx);
  if (n < 0) then Exit;
  try
    EWE := GetGroupe(n);
    EWE := CalcGrpBoundingBox(EWE);
    PutGroupe(n, EWE);
  except
    pass;
  end;
end;

procedure TDocumentDessin.MarquerVisibiliteOfAllGroupes(const B: boolean);
var
  NbGroupes, i: Integer;
  MyGRP: TGroupeEntites;
begin
  // dé-marquer tous les groupes
  NbGroupes := GetNbGroupes();
  for i := 0 to NbGroupes - 1 do
  begin
    MyGRP := GetGroupe(i);
    MyGRP.Visible := B;
    PutGroupe(i, MyGRP);
  end;
end;

procedure TDocumentDessin.BasculerVisibiliteOfAllGroupes();
var
  NbGroupes, i: Integer;
  MyGRP: TGroupeEntites;
begin
  // dé-marquer tous les groupes
  NbGroupes := GetNbGroupes();
  for i := 0 to NbGroupes - 1 do
  begin
    MyGRP := GetGroupe(i);
    MyGRP.Visible := not MyGRP.Visible;
    PutGroupe(i, MyGRP);
  end;

end;

// Liste des éléments ponctuels: Gestion
procedure TDocumentDessin.PurgerListeSymboles();
var
  i, NbObj: integer;
begin
  NbObj := GetNbSymboles();
  AfficherMessage(Format('%s.PurgerListeSymboles() - %d elements',[ClassName, NbObj]));
  FListeSymboles.ClearListe();
end;
procedure TDocumentDessin.AddSymbole(const EP: TSymbole; const DoRecalcBBX: boolean);
begin
  FListeSymboles.AddElement(EP);
  if (DoRecalcBBX) then CalcGrpBoundingBoxAndUptdateGdpByIDX(EP.IDGroupe);
end;
function  TDocumentDessin.GetSymbole(const Idx: integer): TSymbole;
begin
  Result := FListeSymboles.GetElement(Idx);
end;
procedure TDocumentDessin.PutSymbole(const Idx: integer; const EP: TSymbole);
begin
  try
    FListeSymboles.PutElement(Idx, EP);
    CalcGrpBoundingBoxAndUptdateGdpByIDX(EP.IDGroupe);
  except
  end;
end;
procedure TDocumentDessin.DeleteSymbole(const Idx: integer);
begin
  FLastSymboleDeleted := GetSymbole(Idx); // mise à jour de la variable pour undo
  FListeSymboles.RemoveElement(Idx);
end;

 // Liste des éléments courbes: Gestion
procedure TDocumentDessin.PurgerListeCourbes();
var
  NbObj: Integer;
begin
  NbObj := GetNbCourbes();
  AfficherMessage(Format('%s.PurgerListeCourbes() - %d elements',[ClassName, NbObj]));
  FListeCourbes.ClearListe();
end;
// ajoute une courbe et calcule sa bounding-box
procedure TDocumentDessin.AddCourbe(const C: TCourbe; const DoRecalcBBX: boolean);
var
  i: integer;
  pC: TCourbe;
begin
  try
    if (Length(C.Arcs) = 0) then Exit;
    pC := C;
    pC := CalcBoundingBoxCourbe(pC); //TODO: A remettre en place après
    pC.MarkToDelete := false;
    FListeCourbes.AddElement(pC);
    if (DoRecalcBBX) then CalcGrpBoundingBoxAndUptdateGdpByIDX(pC.IDGroupe);
  except
    pass;
  end;
end;
function  TDocumentDessin.GetCourbe(const Idx: integer): TCourbe;
begin
  Result := FListeCourbes.GetElement(Idx);
end;
procedure TDocumentDessin.PutCourbe(const Idx: integer; const C: TCourbe);
var
  pC: ^TCourbe;
begin
  FListeCourbes.PutElement(Idx, C);
end;
procedure TDocumentDessin.DeleteCourbe(const Idx: integer);
var
  i: integer;
begin
  FLastCourbeDeleted := GetCourbe(Idx); // mise à jour de la variable pour undo
  FListeCourbes.RemoveElement(Idx);
end;


function  TDocumentDessin.CalcBoundingBoxCourbe(const C: TCourbe): TCourbe;
var
  i: integer;
  procedure SetBoundingBox(const A: TArcCourbe);
  var
    s: integer;
    //P:  TBaseStation;
    Pt: TPoint2Df;
    //AB: TBezierArc;
    Bezier: TArrayPoints2Df;
    P0, P1, P2, P3: TPoint2Df;
    ErrCode: integer;
  begin
    GetCoordsGCSWithoutGroupe(A.IDStationP1, A.OffsetP1, P0, ErrCode);
    if (ErrCode = -1) then Exit;
    P1.setFrom(P0.X + A.TangP1.X, P0.Y + A.TangP1.Y);
    GetCoordsGCSWithoutGroupe(A.IDStationP2, A.OffsetP2, P3, ErrCode);
    if (ErrCode = -1) then Exit;
    P2.setFrom(P3.X + A.TangP2.X, P3.Y + A.TangP2.Y);
    Bezier := CalcBezierCurve(P0, P1, P2, P3, 20);
    for s := 0 to High(Bezier) do
    begin
      PT.setFrom(Bezier[s].X, Bezier[s].Y);
      if (Pt.X > Result.BoundingBox.C2.X) then Result.BoundingBox.C2.X := PT.X;
      if (Pt.Y > Result.BoundingBox.C2.Y) then Result.BoundingBox.C2.Y := PT.Y;
      if (Pt.X < Result.BoundingBox.C1.X) then Result.BoundingBox.C1.X := PT.X;
      if (Pt.Y < Result.BoundingBox.C1.Y) then Result.BoundingBox.C1.Y := PT.Y;
    end;
  end;
begin
  Result := C;
  Result.BoundingBox.C1.setFrom( INFINITE,  INFINITE);
  Result.BoundingBox.C2.setFrom(-INFINITE, -INFINITE);
  for i := 0 to High(C.Arcs) do SetBoundingBox(C.Arcs[i]);
end;
// convertir en tableau de points 2D une courbe de bézier au format GHCD
function TDocumentDessin.ConvertirGHCD2StdPolygon(const P: TVertexPolygonArray; out arrPt2Df: TArrayPoints2Df): boolean;
const LONG_MAX_COTE_POLY = 100.00;
var
  i, Nb, q : integer;
  BS1   : TBaseStation;
  V0, V1: TPoint2Df;
  d: float;
begin
  result := False;
  Nb := High(P);
  AfficherMessageErreur(Format('%s.ConvertirGHCD2StdPolygon(): %d points',[ClassName, Nb]));
  SetLength(arrPt2Df, 0);
  SetLength(arrPt2Df, Nb+1);
  for i := 0 to Nb do
  begin
    if (not GetBasePointByIndex(P[i].IDStation, BS1)) then Exit;
    arrPt2Df[i].setFrom(BS1.PosStation.X + P[i].Offset.X, BS1.PosStation.Y + P[i].Offset.Y);
  end;
  // contrôle: on jarte les polygones dégénérés
  for i := 0 to Nb do
  begin
    q := i + 1;
    if (q > Nb) then q := 0;
    V0 := arrPt2Df[i];
    V1 := arrPt2Df[q];
    d := Hypot(V1.X - V0.X, V1.Y - V0.Y);
    if (d > LONG_MAX_COTE_POLY) then exit(false);
  end;
  Result := true;
end;

function TDocumentDessin.CouperCourbe(const IdxCourbe: integer; const NoPoint): boolean;
var
  CourbeACouper: TCourbe;
begin
  result := false;
  CourbeACouper := GetCourbe(IdxCourbe);
end;

// convertir en tableau de points 2D une courbe de bézier au format GHCD
function TDocumentDessin.ConvertirGHCD2StdBezierCourbe(const Arcs: TArcsCourbesArray; out arrPt2Df: TArrayPoints2Df): boolean;
var
  i, Nb, Idx: integer;
  BS1, BS2: TBaseStation;
  P1, P2, P3, P4: TPoint2Df;
begin
  Result := false;
  Nb := 1 + 3 * (1 + High(Arcs));
  SetLength(arrPt2Df, 0);
  SetLength(arrPt2Df, Nb);
  Idx := 0;
  for i := 0 to High(Arcs) do
  begin
    if (not GetBasePointByIndex(Arcs[i].IDStationP1, BS1)) then Exit;
    if (not GetBasePointByIndex(Arcs[i].IDStationP2, BS2)) then Exit;
    // Rappel: Le décalage des groupes est du ressort de la balise <g>
    // point d'ancrage 1:
    P1.setFrom(BS1.PosStation.X + Arcs[i].OffsetP1.X,
               BS1.PosStation.Y + Arcs[i].OffsetP1.Y);
    // points de contrôle
    P2.setFrom(BS1.PosStation.X + Arcs[i].OffsetP1.X + Arcs[i].TangP1.X,
               BS1.PosStation.Y + Arcs[i].OffsetP1.Y + Arcs[i].TangP1.Y);
    P3.setFrom(BS2.PosStation.X + Arcs[i].OffsetP2.X + Arcs[i].TangP2.X,
               BS2.PosStation.Y + Arcs[i].OffsetP2.Y + Arcs[i].TangP2.Y);
    // point d'ancrage 1:
    P4.setFrom(BS2.PosStation.X + Arcs[i].OffsetP2.X,
               BS2.PosStation.Y + Arcs[i].OffsetP2.Y);
    if (i = 0) then
    begin  // point de départ de la courbe
      arrPt2Df[Idx].setFrom(P1.X, P1.Y);
      Inc(Idx);
    end;
    arrPt2Df[Idx].setFrom(P2.X, P2.Y);
    Inc(Idx);
    arrPt2Df[Idx].setFrom(P3.X, P3.Y);
    Inc(Idx);
    arrPt2Df[Idx].setFrom(P4.X, P4.Y);
    Inc(Idx);
  end;
  Result := true;
  // contrôle
  //for i := 0 to High(Result) do AfficherMessage(Format('  Point: %d - (%.2f, %.2f)',[i, Result[i].X, Result[i].Y]));
end;

procedure TDocumentDessin.PurgerListePolylignes();
var
  i, NbObj: Integer;
begin
  NbObj := GetNbPolylignes();
  AfficherMessage(Format('%s.PurgerListePolylignes() - %d elements',[ClassName, NbObj]));
  FListePolyLignes.ClearListe();
end;

procedure TDocumentDessin.AddPolyLigne(const P: TPolyligne; const DoRecalcBBX: boolean);
var
  pP : TPolyLigne;
  S: double;
  QP: double;
begin
  if (Length(P.Sommets) = 0) then Exit;
  pP := P;
  pP := CalcBoundingBoxPolyligne(pP);
  CalcAreaPerimetrePolyOrScrap(PP.IDGroupe, PP.Sommets, pP.Area, pP.Perimeter);
  pP.MarkToDelete := false;

  FListePolyLignes.AddElement(pP);
  if (DoRecalcBBX) then CalcGrpBoundingBoxAndUptdateGdpByIDX(pP.IDGroupe);
end;


function TDocumentDessin.GetPolyligne(const Idx: integer): TPolyligne;
begin
  Result := FListePolyLignes.GetElement(Idx);
end;

function TDocumentDessin.GetPtrCenterlines(): TCenterLines;
begin
  Result := FCenterLines;
end;



procedure TDocumentDessin.PutPolyligne(const Idx: integer; const P: TPolyligne);
var
  PP: TPolyLigne;
begin
  PP := P;
  CalcAreaPerimetrePolyOrScrap(PP.IDGroupe, PP.Sommets, pP.Area, pP.Perimeter);
  FListePolyLignes.PutElement(Idx, PP);
  CalcGrpBoundingBoxAndUptdateGdpByIDX(PP.IDGroupe);
end;



function TDocumentDessin.CalcBoundingBoxPolyligne(const P: TPolyligne): TPolyligne;
var
  i: integer;
  V: TVertexPolygon;
  Pt: TPoint2Df;
  errCode: Integer;
begin
  errCode := 0;
  Result := P;
  Result.BoundingBox.Reset();
  for i := 0 to High(P.Sommets) do
  begin
    V  := P.Sommets[i];
    GetCoordsGCSWithoutGroupe(V.IDStation, V.Offset, PT, ErrCode);
    if (ErrCode = -1) then Exit;
    if (PT.X < Result.BoundingBox.C1.X) then Result.BoundingBox.C1.X := PT.X;
    if (PT.Y < Result.BoundingBox.C1.Y) then Result.BoundingBox.C1.Y := PT.Y;
    if (PT.X > Result.BoundingBox.C2.X) then Result.BoundingBox.C2.X := PT.X;
    if (PT.Y > Result.BoundingBox.C2.Y) then Result.BoundingBox.C2.Y := PT.Y;
  end;
end;



procedure TDocumentDessin.DeletePolyligne(const Idx: integer);
begin
  FLastPolylineDeleted := GetPolyligne(Idx); // mise à jour de la variable pour undo
  FListePolyLignes.RemoveElement(Idx);
end;



// Liste des polygones
procedure TDocumentDessin.PurgerListePolygones();
var
   NbObj: integer;
begin
  NbObj := GetNbPolygones();
  AfficherMessage(Format('%s.PurgerListePolygones - %d elements',[ClassName, NbObj]));
  FListePolygones.ClearListe();
end;
procedure TDocumentDessin.AddPolygone(const P: TPolygone; const DoRecalcBBX: boolean);
var
  i: integer;
  pP: TPolygone;
begin
  if (Length(P.Sommets) = 0) then Exit;
  pP := P;
  pP := CalcBoundingBoxPolygone(pP);
  CalcAreaPerimetrePolyOrScrap(PP.IDGroupe, PP.Sommets, pP.Area, pP.Perimeter);
  pP.MarkToDelete := false;
  FListePolygones.AddElement(pP);
  if (DoRecalcBBX) then CalcGrpBoundingBoxAndUptdateGdpByIDX(pP.IDGroupe);
end;
function  TDocumentDessin.GetPolygone(const Idx: integer): TPolygone;
begin
  Result := FListePolygones.GetElement(Idx);
end;
procedure TDocumentDessin.PutPolygone(const Idx: integer; const P: TPolygone);
var
  PP: TPolygone;
begin
  PP := P;
  CalcAreaPerimetrePolyOrScrap(PP.IDGroupe, PP.Sommets, pP.Area, pP.Perimeter);
  FListePolygones.PutElement(Idx, PP);
  CalcGrpBoundingBoxAndUptdateGdpByIDX(PP.IDGroupe);
end;
function  TDocumentDessin.CalcBoundingBoxPolygone(const P: TPolygone): TPolygone;
var
  i: integer;
  V: TVertexPolygon;
  Pt: TPoint2Df;
  errCode: Integer;
begin
  errCode := 0;
  Result := P;
  Result.BoundingBox.Reset();
  //SetBoundingBox(C.Arcs[0], True); // extrémité 1 de la courbe
  for i := 0 to High(P.Sommets) do
  begin
    V  := P.Sommets[i];
    GetCoordsGCS(V.IDStation, P.IDGroupe, V.Offset, PT, ErrCode);
    if (ErrCode = -1) then Exit;
    if (PT.X < Result.BoundingBox.C1.X) then Result.BoundingBox.C1.X := PT.X;
    if (PT.Y < Result.BoundingBox.C1.Y) then Result.BoundingBox.C1.Y := PT.Y;
    if (PT.X > Result.BoundingBox.C2.X) then Result.BoundingBox.C2.X := PT.X;
    if (PT.Y > Result.BoundingBox.C2.Y) then Result.BoundingBox.C2.Y := PT.Y;
  end;
end;
procedure TDocumentDessin.DeletePolygone(const Idx: integer);
begin
  FLastPolygoneDeleted := GetPolygone(Idx); // mise à jour de la variable pour undo
  FListePolygones.RemoveElement(Idx);
end;


// liste des éléments lignes simples
procedure TDocumentDessin.PurgerListeSimplesLignes();
var
  i, NbObj: integer;
begin
  NbObj := GetNbSimpleLignes();
  AfficherMessage(Format('%s.PurgerListeSimplesLignes() - %d elements',[ClassName, NbObj]));
  FListeSimplesLignes.ClearListe();
end;
procedure TDocumentDessin.AddSimpleLigne(const L: TSimpleLigne; const DoRecalcBBX: boolean);
var
  pL: TSimpleLigne;
begin
  pL := L;
  pL := CalcBoundingBoxLigne(L);
  pL.MarkToDelete := false;
  FListeSimplesLignes.AddElement(pL);
  if (DoRecalcBBX) then CalcGrpBoundingBoxAndUptdateGdpByIDX(pL.IDGroupe);
end;
function TDocumentDessin.GetSimpleLigne(const Idx: integer): TSimpleLigne;
begin
  result := FListeSimplesLignes.GetElement(Idx);
end;
procedure TDocumentDessin.PutSimpleLigne(const Idx: integer; const SL: TSimpleLigne);
begin
  FListeSimplesLignes.PutElement(Idx, SL);
  CalcGrpBoundingBoxAndUptdateGdpByIDX(SL.IDGroupe);
end;

procedure TDocumentDessin.DeleteSimpleLigne(const Idx: integer);
begin
  FLastSimpleLigneDeleted := GetSimpleLigne(Idx); // mise à jour de la variable pour undo
  FListeSimplesLignes.RemoveElement(Idx);
end;

function  TDocumentDessin.CalcBoundingBoxLigne(const L: TSimpleLigne): TSimpleLigne;
const MG = 0.30;
var
  errCode: Integer;
begin
  errCode := 0;
  Result := L;
  GetCoordsGCSWithoutGroupe(L.IDBaseStExt1, L.OffsetExtr1, Result.BoundingBox.C1, errCode);
  if (ErrCode = -1) then Exit;
  GetCoordsGCSWithoutGroupe(L.IDBaseStExt2, L.OffsetExtr2, Result.BoundingBox.C2, errCode);
  if (ErrCode = -1) then Exit;
  if (Result.BoundingBox.C1.X > Result.BoundingBox.C2.X) then Swap(Result.BoundingBox.C1.X, Result.BoundingBox.C2.X);
  if (Result.BoundingBox.C1.Y > Result.BoundingBox.C2.Y) then Swap(Result.BoundingBox.C1.Y, Result.BoundingBox.C2.Y);
  // on ajoute une marge autour (pour pouvoir sélectionner si la ligne est horizontale ou verticale)
  Result.BoundingBox.C1.X := Result.BoundingBox.C1.X - MG;
  Result.BoundingBox.C1.Y := Result.BoundingBox.C1.Y - MG;
  Result.BoundingBox.C2.X := Result.BoundingBox.C2.X + MG;
  Result.BoundingBox.C2.Y := Result.BoundingBox.C2.Y + MG;
end;

// Liste des éléments texte: Gestion
procedure TDocumentDessin.PurgerListeTextes();
begin
  AfficherMessage(Format('%s.PurgerListeTextes() - %d elements',[ClassName, FListeTextObjects.Count]));
  FListeTextObjects.ClearListe();
end;

procedure TDocumentDessin.PurgerListesObjets();
begin
  PurgerListeSuperGroupes();
  PurgerListeGroupes;
  PurgerListeScraps();
  PurgerListeImages();
  PurgerListePolygones();
  PurgerListeCourbes();
  PurgerListePolylignes();
  PurgerListeSimplesLignes();
  PurgerListeSymboles();
  PurgerListeTextes();
end;

procedure TDocumentDessin.AddTexte(const T: TTextObject; const DoRecalcBBX: boolean);
var
  pT: TTextObject;
begin
  if (not TexteEstValide(T)) then Exit;
  pT := T;
  pT.MarkToDelete := false;
  FListeTextObjects.AddElement(pT);
  if (DoRecalcBBX) then CalcGrpBoundingBoxAndUptdateGdpByIDX(pT.IDGroupe);
end;
// calcul de l'aire et du périmètre d'un polygone, scrap ou polyligne
// utilisé essentiellement pour la topo des carrières souterraines ou des grandes salles
procedure TDocumentDessin.CalcAreaPerimetrePolyOrScrap(const IDGrp: TIDGroupeEntites; const P: TVertexPolygonArray; out Area, Perimeter: double);
var
  Nb, i, j, ErrCode: Integer;
  V0, V1: TVertexPolygon;
  Grp: TGroupeEntites;
  PC0, PC1: TPoint2Df;
begin
  Area      := 0.00;
  Perimeter := 0.00;
  Nb := Length(P);
  Grp := GetGroupeByIDGroupe(IDGrp);
  if (Nb < 3) then Exit;
  j := Nb - 1;
  for i := 0 to Nb - 1 do
  begin
    V0 := P[i];
    V1 := P[j];
    GetCoordsGCS(V0.IDStation, IDGrp, V0.Offset, PC0, ErrCode);
    GetCoordsGCS(V1.IDStation, IDGrp, V1.Offset, PC1, ErrCode);
    Area := Area + (PC1.X * PC0.Y) - (PC1.Y * PC0.X);
    Perimeter := Perimeter + Hypot(PC1.X - PC0.X, PC1.Y - PC0.Y);
    j := i;
  end;
  Area := Abs(Area * 0.5);
  //AfficherMessageErreur(Format('CalcAreaPolyOrScrap: Area: %f - Perimeter: %f', [Area, Perimeter]));
end;

function  TDocumentDessin.GetTexte(const Idx: integer): TTextObject;
begin
  Result := FListeTextObjects.GetElement(Idx);
end;
procedure TDocumentDessin.PutTexte(const Idx: integer; const T: TTextObject);
begin
  FListeTextObjects.PutElement(Idx, T);
  CalcGrpBoundingBoxAndUptdateGdpByIDX(T.IDGroupe);
end;
procedure TDocumentDessin.DeleteTexte(const Idx: integer);
begin
  try
    FLastTexteDeleted := GetTexte(Idx); // mise à jour de la variable pour undo
    FListeTextObjects.RemoveElement(Idx);
  except
  end;
end;

// purger les listes de styles
procedure TDocumentDessin.PurgerListeStyles();
begin
  FListeTextStyles.ClearListe();
  FListeSymbolStyles.ClearListe();
  FListeCurveStyles.ClearListe();
  FListeLinesStyles.ClearListe();
  FListePolygonStyles.ClearListe();
end;



//------------------------------------------------------------------------------
// recherche d'éléments
function  TDocumentDessin.FindObject(const X,Y: double; const ModeSelection: TModeSelectionEntites): Int64;
var
  FFindingObjects: TFindingObjects;
begin
  Result := -1;
  FFindingObjects := TFindingObjects.Create;
  try
    FFindingObjects.Initialise(self);
    FIdxObjectFound := FFindingObjects.FindObject(X, Y, ModeSelection);
    Result := FFindingObjects.GetIDNearest();
  finally
    FreeAndNil(FFindingObjects);//     FFindingObjects.Free;
  end;
end;

// calcul de la position d'un point dans le GCS
// La ligne suivant un appel de cette fonction doit être de la forme:
// if (errCode = -1) then Exit;
procedure TDocumentDessin.GetCoordsGCS(const IDBS: TIDBaseStation;
                                       const IDGroupe: TIDGroupeEntites;
                                       const Offset: TPoint3Df;
                                       out   PtResult: TPoint2Df;
                                       out   ErrCode : integer);
var
  BS : TBaseStation;
  GP : TGroupeEntites;
begin
  ErrCode := -1;
  if (GetBasePointByIndex(IDBS, BS)) then
  begin

    GP := GetGroupeByIDGroupe(IDGroupe);
    //if (GP.DecalageActif) then DG := MakeTPoint2Df(GP.Decalage.X, GP.Decalage.Y) else DG := MakeTPoint2Df(0.00, 0.00);
    PtResult := MakeTPoint2DfWithGroupeAndBaseStation(BS, GP, Offset.X, Offset.Y);
    ErrCode := 0;
  end
  else
  begin
    AfficherMessageErreurBasepointNotFound(IDBS);
    ErrCode := -1;
  end;
end;
procedure TDocumentDessin.GetCoordsGCSWithoutGroupe(const IDBS: TIDBaseStation;
                                                    const Offset: TPoint3Df;
                                                    out   PtResult: TPoint2Df;
                                                    out   ErrCode : integer);
var
  BS : TBaseStation;
begin
  ErrCode := -1;
  if (GetBasePointByIndex(IDBS, BS)) then
  begin
    PtResult := MakeTPoint2DfWithBaseStationOnly(BS, Offset.X, Offset.Y);
    ErrCode := 0;
  end
  else
  begin
    AfficherMessageErreurBasepointNotFound(IDBS);
    ErrCode := -1;
  end;
end;
// conversion de coordonnées (objets ponctuels)
procedure TDocumentDessin.GetCoordsGCSPonctObj(const qEP: TSymbole;
                                               out PtResult: TPoint2Df;
                                               out errCode: integer);
var
  G: TGroupeEntites;
  BS: TBaseStation;
begin
  errCode := -1;
  G := GetGroupeByIDGroupe(qEP.IDGroupe);
  if (GetBasePointByIndex(qEP.IDBaseStation, BS)) then
  begin
    PtResult := MakeTPoint2DfWithGroupeAndBaseStation(BS, G, qEP.Offset.X, qEP.Offset.Y);
    ErrCode := 0;
  end
  else
  begin
    AfficherMessageErreurBasepointNotFound(qEP.IDBaseStation);
    ErrCode := -1;
  end;
end;
// vérifier si le groupe ne contient pas des entités
function TDocumentDessin.GetNbEntitiesGroupe(const GrpIdx: TIDGroupeEntites): integer;
var
  i: integer;
  C: TCourbe;
  L: TSimpleLigne;
  P: TPolygone;
  T: TTextObject;
  O: TSymbole;
  NbObj: Integer;
  PL: TPolyLigne;
begin
  AfficherMessage(Format('%s.IsEmptyEntitiesGroupe - %d',[ClassName, GrpIdx]));
  Result := 0;
  // courbes
  NbObj := GetNbCourbes();
  if (NbObj > 0) then
  begin
    for i:= 0 to NbObj - 1 do
    begin
      C := GetCourbe(i);
      if (C.IDGroupe = GrpIdx) then Inc(Result);
    end;
  end;
  // polylignes
  NbObj := GetNbPolylignes();
  if (NbObj > 0) then
  begin
    for i:= 0 to NbObj - 1 do
    begin
      PL := GetPolyligne(i);
      if (PL.IDGroupe = GrpIdx) then Inc(Result);
    end;
  end;
  // lignes
  NbObj := GetNbSimpleLignes();
  if NbObj > 0 then
  begin
    for i:= 0 to NbObj - 1 do
    begin
      L := GetSimpleLigne(i);
      if (L.IDGroupe = GrpIdx) then Inc(Result);
    end;
  end;
  // polygones
  NbObj := GetNbPolygones();
  if (NbObj > 0) then
  begin
    for i:= 0 to NbObj - 1 do
    begin
      P := GetPolygone(i);
      if (P.IDGroupe = GrpIdx) then Inc(Result);
    end;
  end;
  // objets
  NbObj := GetNbSymboles();
  if (NbObj > 0) then
  begin
    for i:= 0 to NbObj - 1 do
    begin
      O := GetSymbole(i);
      if (O.IDGroupe = GrpIdx) then Inc(Result);
    end;
  end;
  // textes
  NbObj := GetNbTextes();
  if (NbObj > 0) then
  begin
    for i:= 0 to NbObj - 1 do
    begin
      T := GetTexte(i);
      if (T.IDGroupe = GrpIdx) then Inc(Result);
    end;
  end;
end;
//------------------------------------------------------------------------------
function TDocumentDessin.GetNbGroupes(): integer;
begin
  Result := FListeGroupes.GetNbElements();
end;
// retourne l'ID maxi des groupes; fixe un bug lié à la suppression d'un groupe
// Si WithNewGroupeCreated armé: donne l'ID du nouveau groupe créé
function TDocumentDessin.GetMaxIdxGroupe(const WithNewGroupeCreated: boolean): TIDGroupeEntites;
var
  n, i: Integer;
  G: TGroupeEntites;
begin
  Result := -1;
  n := GetNbGroupes();
  if (n = 0) then Exit;
  for i := 0 to n - 1 do
  begin
    G := GetGroupe(i);
    if (G.IDGroupeEntites > Result) then Result := G.IDGroupeEntites;
  end;
  if (WithNewGroupeCreated) then Result := Result + 1;
end;

function TDocumentDessin.GetNbPolygones(): integer;
begin
  Result := FListePolygones.GetNbElements();
end;
function TDocumentDessin.GetNbPolylignes(): integer;
begin
  Result := FListePolyLignes.GetNbElements();
end;

function TDocumentDessin.GetNbScraps(): integer;
begin
  Result := FListeScraps.GetNbElements();
end;

function TDocumentDessin.GetNbSimpleLignes(): integer;
begin
  Result := FListeSimplesLignes.GetNbElements();
end;

function TDocumentDessin.GetNbStylesCourbe(): integer;
begin
  Result := FListeCurveStyles.GetNbElements();
end;
function TDocumentDessin.GetNbStylesLigne(): integer;
begin
  Result := FListeLinesStyles.GetNbElements();
end;
function TDocumentDessin.GetNbStylesPolygones(): integer;
begin
  Result := FListePolygonStyles.GetNbElements();
end;
function TDocumentDessin.GetNbStylesSymbole(): integer;
begin
  Result := FListeSymbolStyles.GetNbElements();
end;

function TDocumentDessin.GetNbStylesTexte(): integer;
begin
  Result := FListeTextStyles.GetNbElements();
end;

function TDocumentDessin.GetNbSuperGroupes(): integer;
begin
  Result := FListeSuperGroupes.GetNbElements();
end;

function TDocumentDessin.GetNbSymboles(): integer;
begin
  Result := FListeSymboles.GetNbElements();
end;
function TDocumentDessin.GetNbTextes(): integer;
begin
  Result := FListeTextObjects.GetNbElements();
end;
// réassigner les objets d'un groupe à un autre
function TDocumentDessin.ReassignerEntitesGroupes(const OldGrp, NewGrp: integer): integer;
var
  i, NbObj: integer;
  C: TCourbe;
  L: TSimpleLigne;
  P: TPolygone;
  O: TSymbole;
  T: TTextObject;
  PL: TPolyLigne;
begin
  AfficherMessage(Format('%s.ReassignerEntitesGroupes (%d -> %d)',[ClassName, OldGrp, NewGrp]));
  Result := -1;
  if (OldGrp = NewGrp) then
  begin
    AfficherMessage('--> Groupes source et cible identiques ');
    Result := 0;
    Exit;
  end;
  // réassignation
  try
    // courbes
    NbObj := GetNbCourbes();
    if (NbObj > 0) then
    begin
      AfficherMessage('--> Courbes');
      for i:= 0 to NbObj - 1 do
      begin
        C := GetCourbe(i);
        if (C.IDGroupe = OldGrp) then
        begin
          C.IDGroupe := NewGrp;
          PutCourbe(i, C);
        end;
      end;
    end;
    // polylignes
    NbObj := GetNbPolylignes();
    if (NbObj > 0) then
    begin
      AfficherMessage('--> polylignes');
      for i:= 0 to NbObj - 1 do
      begin
        PL := GetPolyligne(i);
        if (PL.IDGroupe = OldGrp) then
        begin
          PL.IDGroupe := NewGrp;
          PutPolyligne(i, PL);
        end;
      end;
    end;
    // lignes
    NbObj := GetNbSimpleLignes();
    if (NbObj > 0) then
    begin
      AfficherMessage('--> Lignes');
      for i:= 0 to NbObj - 1 do
      begin
        L := GetSimpleLigne(i);
        if (L.IDGroupe = OldGrp) then
        begin
          L.IDGroupe := NewGrp;
          PutSimpleLigne(i, L);
        end;
      end;
    end;
    // polygones
    NbObj := GetNbPolygones();
    if (NbObj > 0) then
    begin
      AfficherMessage('--> Polygones');
      for i:= 0 to NbObj - 1 do
      begin
        P := GetPolygone(i);
        if (P.IDGroupe = OldGrp) then
        begin
          P.IDGroupe := NewGrp;
          PutPolygone(i, P);
        end;
      end;
    end;
    // objets
    NbObj := GetNbSymboles();
    if (NbObj > 0) then
    begin
      for i:= 0 to NbObj - 1 do
      begin
        O := GetSymbole(i);
        if (O.IDGroupe = OldGrp) then
        begin
          O.IDGroupe := NewGrp;
          PutSymbole(i, O);
        end;
      end;
    end;
    // textes
    NbObj := GetNbTextes();
    if (NbObj > 0) then
    begin
      AfficherMessage('--> Textes');
      for i:= 0 to NbObj - 1 do
      begin
        T := GetTexte(i);
        if (P.IDGroupe = OldGrp) then
        begin
          T.IDGroupe := NewGrp;
          PutTexte(i, T);
        end;
      end;
    end;
    Result := 1;
  except
    pass;
  end;
end;

function TDocumentDessin.ReattribBasePointsDeUnObjet(const MT: TModeSelectionEntites;
                                                     const QIndexObjet: integer;
                                                     const QIdxGrp: TIDGroupeEntites): boolean;
var
  WU: String;
begin
  Result := false;
  if (QIndexObjet < 0) then Exit;
  case MT of
    mseSCRAPS     : ReattribuerBasePointsScrap(QIndexObjet, QIdxGrp);
    mseCOURBES    : ReattribuerBasePointsCourbe(QIndexObjet, QIdxGrp);
    msePOLYLIGNES : ReattribuerBasePointsPolyLigne(QIndexObjet, QIdxGrp);
    msePOLYGONES  : ReattribuerBasePointsPolygone(QIndexObjet, QIdxGrp);
    mseLIGNES     : ReattribuerBasePointsSimpleLigne(QIndexObjet, QIdxGrp);
    mseSYMBOLES   : ReattribuerBasePointSymbole(QIndexObjet, QIdxGrp);
    mseTEXTES     : ReattribuerBasePointTexte(QIndexObjet, QIdxGrp);
  end;

end;


function TDocumentDessin.ReattribBasepointsAllObjetsGroupe(const Grp: TGroupeEntites; const QFiltres: string): integer;
var
  OldFiltres: String;

  procedure ReattribObjGrp(const MT: TModeSelectionEntites);
  var
    WU: String;
    i, Nb: Integer;
  begin
    WU := ChooseString(Ord(MT),
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
    //WU := System.Delete(WU, 0, 3);
    case MT of
      mseSCRAPS      : Nb := GetNbScraps();
      mseCOURBES     : Nb := GetNbCourbes();
      msePOLYLIGNES  : Nb := GetNbPolylignes();
      msePOLYGONES   : Nb := GetNbPolygones();
      mseLIGNES      : Nb := GetNbSimpleLignes();
      mseSYMBOLES    : Nb := GetNbSymboles();
      mseTEXTES      : Nb := GetNbTextes();
    else
      Exit;
    end;
    //AfficherMessage(Format('--- Reattribution des %d %s', [Nb, WU]));
    if (Nb > 0) then
    begin
      for i := 0 to Nb - 1 do ReattribBasePointsDeUnObjet(MT, i, Grp.IDGroupeEntites);
    end;
  end;

begin
  Result := -1;
  AfficherMessage(Format('%s.ReattribuerBasepointsAllObjetsGroupe: %d - %s', [ClassName, Grp.IDGroupeEntites, Grp.NomGroupe]));
  AfficherMessage('-- Filtres: ' + QFiltres);
  OldFiltres := QFiltres;
  // une passe de MétaFiltre
  MetaFiltre(QFiltres);
  ReattribObjGrp(mseSCRAPS);
  ReattribObjGrp(mseCOURBES);
  ReattribObjGrp(msePOLYLIGNES);
  ReattribObjGrp(msePOLYGONES);
  ReattribObjGrp(mseLIGNES);
  ReattribObjGrp(mseSYMBOLES);
  ReattribObjGrp(mseTEXTES);
  // et on restaure avec l'ancien filtre
  MetaFiltre(OldFiltres);
end;

function TDocumentDessin.ReattribuerBasePointsScrap(const QIdx: integer; const QIdxGrp: TIDGroupeEntites): integer;
var
  EWE: TScrap;
  i, Nb: Integer;
  V: TVertexPolygon;
  ErrCode: integer;
  PM: TPoint2Df;
  BS: TBaseStation;
  OldIdxBasePt: TIDBaseStation;
begin
  //AfficherMessage(Format('%s.ReattribuerBasePointsScrap: (%d) %d', [ClassName, QIdxGrp, QIdx]));
  Result := 0;
  EWE := GetScrap(QIdx);
  // si l'objet n'est pas dans le même groupe, on zappe
  if (QIdxGrp = EWE.IDGroupe) then
  begin
    try
      Result := 0;
      Nb := 1 + High(EWE.Sommets);
      for i := 0 to Nb - 1 do
      begin
        V := EWE.Sommets[i];
        // récupère l'ancienne basepoint
        if (GetBasePointByIndex(V.IDStation, BS)) then
        begin
          OldIdxBasePt := BS.IDStation;
          // calcul des coordonnées absolues du sommet
          GetCoordsGCS(V.IDStation, EWE.IDGroupe, V.Offset, PM, ErrCode);
          if (ErrCode > -1) then
          begin
            // on recherche le basepoint le plus proche (cette recherche n'agit que sur les basepoints métafiltrés
            // En cas d'échec, GetNearBasepoint retourne le basepoint passé en paramètre
            BS := GetNearBasepoint(PM.X, PM.Y, BS, false);
            if (BS.IDStation <> OldIdxBasePt) then Result += 1;
            V.IDStation := BS.IDStation;                                                         // nouvelle ID
            V.Offset.setFrom(PM.X - BS.PosStation.X, PM.Y - BS.PosStation.Y, 0.00);     // calcul du décalage
            EWE.Sommets[i] := V;                                                                               // et mise à jour
          end;
        end;

      end;
      // et on actualise avec le scrap modifié
      PutScrap(QIdx, EWE);
    except
      Result := -1;
    end;
  end;
end;
function TDocumentDessin.ReattribuerBasePointsPolygone(const QIdx: integer; const QIdxGrp: TIDGroupeEntites): integer;
var
  EWE: TPolygone;
  i, Nb: Integer;
  V: TVertexPolygon;
  ErrCode: integer;
  PM: TPoint2Df;
  BS: TBaseStation;
  OldIdxBasePt: TIDBaseStation;
begin
  //AfficherMessage(Format('%s.ReattribuerBasePointsPolygone: %d', [ClassName, QIdx]));
  Result := 0;
  EWE := GetPolygone(QIdx);
  // si l'objet n'est pas dans le même groupe, on zappe
  if (QIdxGrp = EWE.IDGroupe) then
  begin
    try
      Nb := 1 + High(EWE.Sommets);
      for i := 0 to Nb - 1 do
      begin
        V := EWE.Sommets[i];
        // récupère l'ancienne basepoint
        if (GetBasePointByIndex(V.IDStation, BS)) then
        begin
          OldIdxBasePt := BS.IDStation;
          // calcul des coordonnées absolues du sommet
          GetCoordsGCS(V.IDStation, EWE.IDGroupe, V.Offset, PM, ErrCode);
          if (ErrCode > -1) then
          begin
            // on recherche le basepoint le plus proche (cette recherche n'agit que sur les basepoints métafiltrés
            // En cas d'échec, GetNearBasepoint retourne le basepoint passé en paramètre
            BS := GetNearBasepoint(PM.X, PM.Y, BS, false);
            // comptage du nombre de points modifiés
            if (BS.IDStation <> OldIdxBasePt) then Result += 1;
            V.IDStation := BS.IDStation;                            // nouvelle ID
            V.Offset.setFrom(PM.X - BS.PosStation.X,       // calcul du décalage
                             PM.Y - BS.PosStation.Y,
                             0.00);
            EWE.Sommets[i] := V;                                    // et mise à jour

          end;
        end;
      end;
      // et on actualise avec le scrap modifié
      PutPolygone(QIdx, EWE);
    except
      Result := -1;
    end;
  end;
end;
function TDocumentDessin.ReattribuerBasePointsPolyLigne(const QIdx: integer;
                                                        const QIdxGrp: TIDGroupeEntites): integer;
var
  EWE: TPolyLigne;
  i, Nb: Integer;
  V: TVertexPolygon;
  ErrCode: integer;
  PM: TPoint2Df;
  BS: TBaseStation;
  OldIdxBasePt: TIDBaseStation;
begin
  //AfficherMessage(Format('%s.ReattribuerBasePointsPolygone: %d', [ClassName, QIdx]));
  Result := 0;
  EWE := GetPolyligne(QIdx);
  // si l'objet n'est pas dans le même groupe, on zappe
  if (QIdxGrp = EWE.IDGroupe) then
  begin
    try
      Nb := 1 + High(EWE.Sommets);
      for i := 0 to Nb - 1 do
      begin
        V := EWE.Sommets[i];
        // récupère l'ancienne basepoint
        if (GetBasePointByIndex(V.IDStation, BS)) then
        begin
          OldIdxBasePt := BS.IDStation;
          // calcul des coordonnées absolues du sommet
          GetCoordsGCS(V.IDStation, EWE.IDGroupe, V.Offset, PM, ErrCode);
          if (ErrCode > -1) then
          begin
            // on recherche le basepoint le plus proche (cette recherche n'agit que sur les basepoints métafiltrés
            // En cas d'échec, GetNearBasepoint retourne le basepoint passé en paramètre
            BS := GetNearBasepoint(PM.X, PM.Y, BS, false);
            // comptage du nombre de points modifiés
            if (BS.IDStation <> OldIdxBasePt) then Result += 1;
            // nouvelle ID
            V.IDStation := BS.IDStation;
            // calcul du décalage
            V.Offset.setFrom(PM.X - BS.PosStation.X,
                             PM.Y - BS.PosStation.Y,
                             0.00);
            // et mise à jour
            EWE.Sommets[i] := V;
          end;
        end;
      end;
      // et on actualise avec le scrap modifié
      PutPolyligne(QIdx, EWE);
    except
      Result := -1;
    end;
  end;
end;
function TDocumentDessin.ReattribuerBasePointsSimpleLigne(const QIdx: integer;
                                                          const QIdxGrp: TIDGroupeEntites): integer;
var
  EWE: TSimpleLigne;
  BS1, BS2: TBaseStation;
  PM1, PM2: TPoint2Df;
  ErrCode: integer;
begin
  Result := -1;
  //AfficherMessage(Format('%s.ReattribuerBasePointsSimpleLigne: %d', [ClassName, QIdx]));
  EWE := GetSimpleLigne(QIdx);
  // si l'objet n'est pas dans le même groupe, on zappe
  if (QIdxGrp = EWE.IDGroupe) then
  begin
    try
      if (GetBasePointByIndex(EWE.IDBaseStExt1, BS1) AND
          GetBasePointByIndex(EWE.IDBaseStExt2, BS2)) then
      begin
        GetCoordsGCS(BS1.IDStation, EWE.IDGroupe, EWE.OffsetExtr1, PM1, ErrCode);
        GetCoordsGCS(BS2.IDStation, EWE.IDGroupe, EWE.OffsetExtr2, PM2, ErrCode);
        // on recherche le basepoint le plus proche (cette recherche n'agit que sur les basepoints métafiltrés
        // En cas d'échec, GetNearBasepoint retourne le basepoint passé en paramètre
        BS1 := GetNearBasepoint(PM1.X, PM1.Y, BS1, false);
        BS2 := GetNearBasepoint(PM2.X, PM2.Y, BS2, false);
        // nouvelle ID
        EWE.IDBaseStExt1 := BS1.IDStation;
        EWE.IDBaseStExt2 := BS2.IDStation;
        EWE.OffsetExtr1.setFrom(PM1.X - BS1.PosStation.X,
                                PM1.Y - BS1.PosStation.Y,
                                0.00);
        EWE.OffsetExtr2.setFrom(PM2.X - BS2.PosStation.X,
                                PM2.Y - BS2.PosStation.Y,
                                0.00);
        // et on actualise avec le scrap modifié
        PutSimpleLigne(QIdx, EWE);
      end;
    except
      Result := -1;
    end;
  end;
end;

function TDocumentDessin.ReattribuerBasePointSymbole(const QIdx: integer;
                                                     const QIdxGrp: TIDGroupeEntites): integer;
var
  EWE: TSymbole;
  ErrCode: integer;
  PM: TPoint2Df;
  BS: TBaseStation;
  OldIdxBasePt: TIDBaseStation;
begin
  //AfficherMessage(Format('%s.ReattribuerBaseSymbole: %d', [ClassName, QIdx]));
  Result := 0;
  EWE := GetSymbole(QIdx);
  // si l'objet n'est pas dans le même groupe, on zappe
  if (QIdxGrp = EWE.IDGroupe) then
  begin
    try
      if (GetBasePointByIndex(EWE.IDBaseStation, BS)) then
      begin
        OldIdxBasePt := BS.IDStation;
        GetCoordsGCS(BS.IDStation, EWE.IDGroupe, EWE.Offset, PM, ErrCode);
        if (ErrCode > -1) then
        begin
          // on recherche le basepoint le plus proche (cette recherche n'agit que sur les basepoints métafiltrés
          // En cas d'échec, GetNearBasepoint retourne le basepoint passé en paramètre
          BS := GetNearBasepoint(PM.X, PM.Y, BS, false);
          // comptage du nombre de points modifiés
          if (BS.IDStation <> OldIdxBasePt) then Result += 1;
          // nouvelle ID
          EWE.IDBaseStation := BS.IDStation;
          // calcul du décalage
          EWE.Offset.setFrom(PM.X - BS.PosStation.X,
                             PM.Y - BS.PosStation.Y,
                             0.00);
        end;
        // et on actualise avec le scrap modifié
        PutSymbole(QIdx, EWE);
      end;
    except
      Result := -1;
    end;
  end;
end;
function TDocumentDessin.ReattribuerBasePointTexte(const QIdx: integer;
                                                   const QIdxGrp: TIDGroupeEntites): integer;
var
  EWE: TTextObject;
  ErrCode: integer;
  PM: TPoint2Df;
  BS: TBaseStation;
  OldIdxBasePt: TIDBaseStation;
begin
  //AfficherMessage(Format('%s.ReattribuerBaseSymbole: %d', [ClassName, QIdx]));
  Result := 0;
  EWE := GetTexte(QIdx);
  // si l'objet n'est pas dans le même groupe, on zappe
  if (QIdxGrp = EWE.IDGroupe) then
  begin
    try
      if (GetBasePointByIndex(EWE.IDBaseSt, BS)) then
      begin
        OldIdxBasePt := BS.IDStation;
        GetCoordsGCS(BS.IDStation, EWE.IDGroupe, EWE.Offset, PM, ErrCode);
        if (ErrCode > -1) then
        begin
          // on recherche le basepoint le plus proche (cette recherche n'agit que sur les basepoints métafiltrés
          // En cas d'échec, GetNearBasepoint retourne le basepoint passé en paramètre
          BS := GetNearBasepoint(PM.X, PM.Y, BS, false);
          // comptage du nombre de points modifiés
          if (BS.IDStation <> OldIdxBasePt) then Result += 1;
          // nouvelle ID
          EWE.IDBaseSt := BS.IDStation;
          // calcul du décalage
          EWE.Offset.setFrom(PM.X - BS.PosStation.X,
                             PM.Y - BS.PosStation.Y,
                             0.00);
        end;
        // et on actualise avec l'objet modifié
        PutTexte(QIdx, EWE);
      end;
    except
      Result := -1;
    end;
  end;
end;
function TDocumentDessin.ReattribuerBasePointsCourbe(const QIdx: integer;
                                                     const QIdxGrp: TIDGroupeEntites): integer;
var
  EWE: TCourbe;
  i, Nb: Integer;
  V: TVertexPolygon;
  ErrCode: integer;
  QArc: TArcCourbe;
  BS1, BS2: TBaseStation;
  PM1, PM2: TPoint2Df;
begin
  //AfficherMessage(Format('%s.ReattribuerBasePointsCourbe: %d', [ClassName, QIdx]));
  Result := 0;
  EWE := GetCourbe(QIdx);
  if (QIdxGrp = EWE.IDGroupe) then  // si l'objet n'est pas dans le même groupe, on zappe
  begin
    try
      Nb := 1 + High(EWE.Arcs);
      for i := 0 to Nb - 1 do
      begin
        QArc := EWE.Arcs[i];
        // on récupère les anciens basepoints
        if (GetBasePointByIndex(QArc.IDStationP1, BS1) AND
            GetBasePointByIndex(QArc.IDStationP2, BS2)) then
        begin
          // calcul des coordonnées absolues du sommet
          GetCoordsGCS(QArc.IDStationP1, EWE.IDGroupe, QArc.OffsetP1, PM1, ErrCode);
          GetCoordsGCS(QArc.IDStationP2, EWE.IDGroupe, QArc.OffsetP2, PM2, ErrCode);
          if (ErrCode > -1) then
          begin
            // on recherche le basepoint le plus proche (cette recherche n'agit que sur les basepoints métafiltrés
            // En cas d'échec, GetNearBasepoint retourne le basepoint passé en paramètre
            BS1 := GetNearBasepoint(PM1.X, PM1.Y, BS1, false);
            BS2 := GetNearBasepoint(PM2.X, PM2.Y, BS2, false);

            QArc.IDStationP1 := BS1.IDStation;                        // nouvelle ID
            QArc.IDStationP2 := BS2.IDStation;
            QArc.OffsetP1.setFrom(PM1.X - BS1.PosStation.X,  // calcul du décalage
                                  PM1.Y - BS1.PosStation.Y,
                                  0.00);
            QArc.OffsetP2.setFrom(PM2.X - BS2.PosStation.X,
                                  PM2.Y - BS2.PosStation.Y,
                                  0.00);
            EWE.Arcs[i] := QArc;                                      // et mise à jour
          end;
          PutCourbe(QIdx, EWE);        // et on actualise avec l'objet modifié
        end;
      end;
    except
      Result := -1;
    end;
  end;
end;

// exporter le dessin au format SVG
// function TDocumentDessin.ExportToSVG(const MyFileName: TStringDirectoryFileName;
//                                      const ExportScrapsOnly: boolean;
//                                      const DoXHTML: boolean;
//                                      const QTitre, QDesc: string): boolean;
// Fonction volumineuse --> déportée dans DocDessinExportToSVG.inc
{$INCLUDE DocDessinExportToSVG.inc}

// exporter le dessin en format ODG
function TDocumentDessin.ExportToODG(const MyFileName: TStringDirectoryFileName;
                                     const ExportScrapsOnly: boolean): boolean;
var
  ODGExport: TExportOdgUtils;
begin
  Result := False;
  AfficherMessage(Format('%s.ExportToODG(%s)', [ClassName, MyFileName]));
  ODGExport := TExportOdgUtils.Create;
  try
    if (ODGExport.Initialiser(self, MyFileName, ExportScrapsOnly)) then
    begin
      ODGExport.LancerExportODG(50);
      ODGExport.Finaliser;
    end;
  finally
    FreeAndNil(ODGExport);//     ODGExport.Free;
  end;
end;

function TDocumentDessin.GetDocumentName(): string;
begin
  Result := ExtractFileName(FDocumentName);
end;

function TDocumentDessin.GetFullFilename(): string;
begin
  Result := FDocumentName;
end;
//
procedure TDocumentDessin.AddSuperGroupe(const G: TSuperGroupe);
begin
  FListeSuperGroupes.AddElement(G);
end;

procedure TDocumentDessin.DeleteSuperGroupe(const Idx: integer);
begin
  FListeSuperGroupes.RemoveElement(Idx);
end;

function TDocumentDessin.GetSuperGroupe(const Idx: integer): TSuperGroupe;
begin
  Result := FListeSuperGroupes.GetElement(Idx);
end;
procedure TDocumentDessin.PurgerListeSuperGroupes();
var
  i, Nb: integer;
begin
  Nb := FListeSuperGroupes.Count;
  AfficherMessage(Format('%s.PurgerListeSuperGroupes() - %d elements',[ClassName, Nb]));
  FListeSuperGroupes.ClearListe();
end;

procedure TDocumentDessin.PutSuperGroupe(const Idx: integer; const Grp: TSuperGroupe);
begin
  FListeSuperGroupes.PutElement(Idx, Grp);
end;
//******************************************************************************
function TDocumentDessin.CalcPhotoData(const EP: TSymbole): TSymbole;
var
  s: string;
  PhotoCtxt  : TImage;
  Ratio      : Double;
begin
  Result := EP;
  Result.PhotoDisplayed := False;
  s := ExtractFilePath(FDocumentName) + Trim(Result.TagTexte);
  if (not FileExists(s)) then
  begin
    AfficherMessage(Format('CalcPhotoData: %s not found',[s]));
    Result.PhotoDisplayed := False;
    Exit;
  end;
  PhotoCtxt:=TImage.Create(nil);
  // ceci est destiné à récupérer les paramètres de la photo
  try
    PhotoCtxt.AutoSize:=False;
    PhotoCtxt.Stretch :=True;
    PhotoCtxt.Picture.LoadFromFile(s);
    Ratio := PhotoCtxt.Picture.Height / PhotoCtxt.Picture.Width;
    Result.ScaleX := Abs(Result.ScaleX); // toujours > 0 pour une photo
    if (Result.ScaleX < 5.00) then Result.ScaleX := 5.00;  //taille minimale pour la photo
    Result.AngleRot := 0.00;
    Result.ScaleY := Result.ScaleX * Ratio;
    //Result.PhotoDisplayed := True;
    AfficherMessage(Format('CalcPhotoData: %s OK',[s]));
    Result.PhotoDisplayed := True;
  finally
    FreeAndNil(PhotoCtxt);//     PhotoCtxt.Free;
  end;
end;

//******************************************************************************
// GESTION DES STYLES
//******************************************************************************
// définition des styles par défaut
// TODO: Procédure d'ajout d'un nouveau type d'objet: Incrémenter de 1 la constante NB_CUR_STY
procedure TDocumentDessin.SetDefaultStyles;
const
  LONG_BARB  = 0.2;
  // seuil de visibilite
  SEUIL_TOUJOURS_VISIBLE = 1E-9;
  SEUIL_PPAUX_DETAILS    = 1 / 5000;
  SEUIL_TOUS_DETAILS     = 1 / 1000;
var
  Q, i: integer;
  procedure QSetStyleLigne(const id: integer;
                          const svgs : string;
                          const descs: string;
                          const LC: TColor;
                          const LW: integer;
                          const LPW: double;
                          const LS: TPenStyle;
                          const SeuilDeVisibilite: double);
  var
    WU: TStyleLigne;
  begin
    with WU do
    begin
      IDStyle        := id;
      NameSVGStyle   := svgs;
      DescStyle      := descs;
      LineColor      := LC;
      LineWidth      := LW;
      PrintLineWidth := LPW;
      LineStyle      := LS;
      SeuilVisibilite:= SeuilDeVisibilite;

    end;
    AddStyleLigne(WU);
  end;
  //*)
  procedure QSetStyleCourbe(const id: integer;
                           const svgs : string;
                           const descs: string;
                           const LC: TColor;
                           const LW: integer;
                           const LPW: double;
                           const LS: TPenStyle;
                           const SM: boolean;
                           const BB: TBarbule;
                           const LB: double;
                           const SeuilDeVisibilite: double);
  var
    WU: TStyleCourbe;
  begin
    with WU do
    begin
      IDStyle        := id;
      NameSVGStyle   := svgs;
      DescStyle      := descs;
      LineColor      := LC;
      LineWidth      := LW;
      PrintLineWidth := LPW;
      LineStyle      := LS;
      Smooth         := SM;
      Barbules       := BB;
      LongBarbules   := LB;
      SeuilVisibilite:= SeuilDeVisibilite;
    end;
    AddStyleCourbe(WU);
  end;
  procedure QSetStylePolygone(const id: integer;
                         const svgs : string;
                         const descs: string;
                         const LW: integer;
                         const LC: TColor;
                         const LS: TPenStyle;
                         const FC: TColor;
                         const PS: byte;
                         const SeuilDeVisibilite: double);
  var
    WU: TStylePolygone;
  begin
    with WU do
    begin
      IDStyle     := id;
      NameSVGStyle:= svgs;
      DescStyle   := descs;
      LineWidth   := LW;
      LineColor   := LC;
      LineStyle   := LS;
      FillColor   := FC;
      Style       := PS;
      SeuilVisibilite := SeuilDeVisibilite;
    end;
    AddStylePolygone(WU);
  end;
  procedure QSetStyleTexte(const id: integer;
                           const svgs : string;
                           const descs: string;
                           const FN: string;
                           const FPH: double;
                           const FC: TColor;
                           const FS: TFontStyles;
                           const SeuilDeVisibilite: double);
  var
    WU : TStyleTexte;
  begin
    with WU do
    begin
      IDStyle       := ID;
      NameSVGStyle  := svgs;
      DescStyle     := descs;
      FontName      := FN;
      FontPrnHeight := FPH;
      FontColor     := FC;
      FontStyle     := FS;
      SeuilVisibilite := SeuilDeVisibilite;
    end;
    AddStyleTexte(WU);
  end;
  procedure QSetStyleSymbole(const id: integer;
                            const svgs : string;
                            const descs: string;
                            const OC: TColor;
                            const SeuilDeVisibilite: double);
  var
    WU: TStyleSymboles;
  begin
    with WU do
    begin
      IDStyle       := id;
      NameSVGStyle  := svgs;
      DescStyle     := descs;
      Color         := OC;
      SeuilVisibilite := SeuilDeVisibilite;
    end;
    AddStyleSymbole(WU);
  end;
begin
  AfficherMessage(Format('%s.SetDefaultStyles()', [ClassName]));
  PurgerListeStyles();
  // lignes
  QSetStyleLigne(0, LowerCase('LINE-DEFAULT')            , ListeStylesLignes[Ord(nolDEFAULT)     ], clBlack    , 0, 0.03, psSolid, SEUIL_TOUS_DETAILS);
  QSetStyleLigne(1, LowerCase('LINE-FLECHES')            , ListeStylesLignes[Ord(nolFLECHE)      ], clBlack    , 0, 0.03, psSolid, SEUIL_TOUS_DETAILS);
  QSetStyleLigne(2, LowerCase('LINE-FRACTURES')          , ListeStylesLignes[Ord(nolSUITE_RESEAU)], clBlue     , 0, 0.10, psSolid, SEUIL_PPAUX_DETAILS);
  QSetStyleLigne(3, LowerCase('LINE-SUITE-RESEAUX')      , ListeStylesLignes[Ord(nolFRACTURE)    ], clRed      , 0, 0.05, psSolid, SEUIL_PPAUX_DETAILS);
  QSetStyleLigne(4, LowerCase('LINE-PENTES')             , ListeStylesLignes[Ord(nolPENTE)       ], clGray     , 0, 0.03, psSolid, SEUIL_TOUS_DETAILS);
  QSetStyleLigne(5, LowerCase('LINE-PENDAGES')           , ListeStylesLignes[Ord(nolPENDAGE)     ], clGray     , 0, 0.03, psSolid, SEUIL_TOUS_DETAILS);

  // courbes
  QSetStyleCourbe( 0, LowerCase('CURVE-DEFAULT')         , ListeStylesCourbes[Ord(nocDEFAULT)           ], clBlack    , 0, 0.10, psSolid, True, tbNONE, LONG_BARB, SEUIL_TOUJOURS_VISIBLE);
  QSetStyleCourbe( 1, LowerCase('CURVE-WALLS')           , ListeStylesCourbes[Ord(nocPAROI)             ], clMaroon   , 2, 0.25, psSolid, True, tbNONE, LONG_BARB, SEUIL_TOUJOURS_VISIBLE);
  QSetStyleCourbe( 2, LowerCase('CURVE-HIDDEN_WALLS')    , ListeStylesCourbes[Ord(nocPAROIS_CACHEE)     ], clGray     , 2, 0.15, psSolid, True, tbNONE, LONG_BARB, SEUIL_TOUJOURS_VISIBLE);
  QSetStyleCourbe( 3, LowerCase('CURVE-FLOODINGS')       , ListeStylesCourbes[Ord(nocECOULEMENT)        ], clBlue     , 3, 0.35, psSolid, True, tbNONE, LONG_BARB, SEUIL_TOUJOURS_VISIBLE);
  QSetStyleCourbe( 4, LowerCase('CURVE-PENTES')          , ListeStylesCourbes[Ord(nocLIGNES_PENTE)      ], clGray     , 0, 0.03 , psSolid, True, tbNONE, LONG_BARB, SEUIL_TOUS_DETAILS);
  QSetStyleCourbe( 5, LowerCase('CURVE-RESSAUTS')        , ListeStylesCourbes[Ord(nocRESSAUT)           ], clMaroon   , 0, 0.03 , psSolid, True, tbRESSAUT, LONG_BARB, SEUIL_PPAUX_DETAILS);
  QSetStyleCourbe( 6, LowerCase('CURVE-SURPLOMBS')       , ListeStylesCourbes[Ord(nocSURPLOMB)          ], clGreen    , 0, 0.03 , psSolid, True, tbSURPLOMB, LONG_BARB, SEUIL_PPAUX_DETAILS);
  QSetStyleCourbe( 7, LowerCase('CURVE-CHENAL_VOUTE')    , ListeStylesCourbes[Ord(nocCHENAL_VOUTE)      ], clNavy     , 0, 0.03 , psSolid, True, tbCHENAL_VOUTE, LONG_BARB, SEUIL_TOUS_DETAILS);
  QSetStyleCourbe( 8, LowerCase('CURVE-MINI-RESSAUTS')   , ListeStylesCourbes[Ord(nocMARCHE)            ], clBlue     , 0, 0.03 , psSolid, True, tbMINI_RESSAUT, LONG_BARB / 2, SEUIL_TOUS_DETAILS);
  QSetStyleCourbe( 9, LowerCase('CURVE-PAROI-INCERTAINE'), ListeStylesCourbes[Ord(nocPAROI_INCERTAINE) ], clMaroon   , 0, 0.25 , psDash, True, tbNONE, LONG_BARB, SEUIL_TOUJOURS_VISIBLE);
  QSetStyleCourbe(10, LowerCase('CURVE-MUR-MACONNE'     ), ListeStylesCourbes[Ord(nocMUR_MACONNE)      ], clYellow   , 2, 0.55 , psDash, True, tbNONE, LONG_BARB, SEUIL_TOUJOURS_VISIBLE);

  // polygones
  QSetStylePolygone( 0, LowerCase('POLYGON-DEFAULT')       , ListeStylesPolygones[Ord(nopDEFAULT)     ] , 0, clBlack  , psSolid, clSilver, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 1, LowerCase('POLYGON-LAKE')          , ListeStylesPolygones[Ord(nopLAC)         ] , 0, clNavy  ,  psSolid, clBlue, 0, SEUIL_PPAUX_DETAILS);
  QSetStylePolygone( 2, LowerCase('POLYGON-ARGILE')        , ListeStylesPolygones[Ord(nopARGILE)      ] , 0, clMaroon, psSolid, clMaroon, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 3, LowerCase('POLYGON-SABLE')         , ListeStylesPolygones[Ord(nopSABLE)       ] , 0, clMaroon, psSolid, clMaroon, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 4, LowerCase('POLYGON-BLOCKS')        , ListeStylesPolygones[Ord(nopEBOULIS)     ] , 0, clMaroon, psSolid, clMaroon, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 5, LowerCase('POLYGON-GALETS')        , ListeStylesPolygones[Ord(nopGALETS)      ] , 0, clMaroon, psSolid, clMaroon, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 6, LowerCase('POLYGON-NEIGE')         , ListeStylesPolygones[Ord(nopNEIGE)       ] , 0, clMaroon, psSolid, clMaroon, 0, SEUIL_PPAUX_DETAILS);
  QSetStylePolygone( 7, LowerCase('POLYGON-SILHOUETTES')   , ListeStylesPolygones[Ord(nopSILHOUETTE)  ] , 0, clSilver, psSolid, clSilver, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 8, LowerCase('POLYGON-GROS-BLOC')     , ListeStylesPolygones[Ord(nopGROS_BLOC)   ] , 0, clBlack , psSolid, clSilver, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone( 9, LowerCase('POLYGON-GOUR')          , ListeStylesPolygones[Ord(nopGOUR)        ] , 0, clBlue  , psSolid, clAqua  , 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone(10, LowerCase('POLYGON-SIPHON')        , ListeStylesPolygones[Ord(nopSIPHON)      ], 0, clNavy  , psSolid, clNavy  , 0, SEUIL_PPAUX_DETAILS);
  QSetStylePolygone(11, LowerCase('POLYGON-ARGILE-GALETS') , ListeStylesPolygones[Ord(nopARGILES_GALETS)], 0, clMaroon, psSolid, clMaroon, 0, SEUIL_TOUS_DETAILS);
  QSetStylePolygone(12, LowerCase('POLYGON-PARCOURS')      , ListeStylesPolygones[Ord(nopCHEMINS)       ], 0, clMaroon, psSolid, clYellow, 0, SEUIL_TOUS_DETAILS);
  // textes
  QSetStyleTexte( 0, LowerCase('TEXT-DEBUG')               , ListeStylesTextes[Ord(notDEBUG)               ], 'Arial',  1.5, clRed   , [], SEUIL_TOUS_DETAILS);
  QSetStyleTexte( 1, LowerCase('TEXT-TITRES')              , ListeStylesTextes[Ord(notTITRES)              ], 'Arial',  8.0, clBlack , [fsBold, fsUnderline], SEUIL_TOUJOURS_VISIBLE);
  QSetStyleTexte( 2, LowerCase('TEXT-SOUS-TITRES')         , ListeStylesTextes[Ord(notSOUS_TITRES)         ], 'Arial',  6.0, clBlack , [fsBold, fsItalic], SEUIL_TOUJOURS_VISIBLE);
  QSetStyleTexte( 3, LowerCase('TEXT-COTATION')            , ListeStylesTextes[Ord(notCOTATION)            ], 'Arial',  2.5, clBlue  , [], SEUIL_TOUS_DETAILS);
  QSetStyleTexte( 4, LowerCase('TEXT-ORDINAIRE1')          , ListeStylesTextes[Ord(notTEXTE1)              ], 'Arial',  1.5, clBlack , [], SEUIL_TOUS_DETAILS);
  QSetStyleTexte( 5, LowerCase('TEXT-ORDINAIRE2')          , ListeStylesTextes[Ord(notTEXTE2)              ], 'Arial',  1.2, clBlack , [fsItalic], SEUIL_TOUS_DETAILS);
  QSetStyleTexte( 6, LowerCase('TEXT-LIEUDIT')             , ListeStylesTextes[Ord(notLIEU_DIT)            ], 'Arial',  1.0, clBlack , [fsItalic, fsUnderline], SEUIL_TOUS_DETAILS);
  QSetStyleTexte( 7, LowerCase('TEXT-COTATION-EXTERIEUR')  , ListeStylesTextes[Ord(notCOTATION_EXTERIEURE) ], 'Arial',  2.5, clGreen , [], SEUIL_TOUS_DETAILS);


  // objets ponctuels
  QSetStyleSymbole( 0, LowerCase('OBJECT-PHOTO')           , ListeNatureSymboles[Ord(nosPHOTO)            ] , clBlack, SEUIL_PPAUX_DETAILS);              // Photo
  QSetStyleSymbole( 1, LowerCase('OBJECT-ENTRANCE')        , ListeNatureSymboles[Ord(nosENTREE)           ] , clBlue, SEUIL_TOUJOURS_VISIBLE);               // Entrée
  QSetStyleSymbole( 2, LowerCase('OBJECT-STATION')         , ListeNatureSymboles[Ord(nosPOINT_TOPO)       ] , clRed, SEUIL_TOUS_DETAILS);                // Point topo
  QSetStyleSymbole( 3, LowerCase('OBJECT-FIXPOINT')        , ListeNatureSymboles[Ord(nosPOINT_FIXE)       ] , clGreen, SEUIL_TOUS_DETAILS);              // Point fixe''
  QSetStyleSymbole( 4, LowerCase('OBJECT-GRP-RELATION')    , ListeNatureSymboles[Ord(nosCORRESPONDANCE)   ] , clGray, SEUIL_TOUS_DETAILS);               // Correspondance entre groupes
  QSetStyleSymbole( 5, LowerCase('OBJECT-FISTULEUSES')     , ListeNatureSymboles[Ord(nosFISTULEUSE)       ] , clBlack, SEUIL_TOUS_DETAILS);              // Fistuleuses'
  QSetStyleSymbole( 6, LowerCase('OBJECT-CONCRETIONS')     , ListeNatureSymboles[Ord(nosCONCRETION_PAROI) ] , clBlack, SEUIL_TOUS_DETAILS);              // Concrétions de paroi'
  QSetStyleSymbole( 7, LowerCase('OBJECT-HELICTITES')      , ListeNatureSymboles[Ord(nosEXCENTRIQUES)     ] , clBlack, SEUIL_TOUS_DETAILS);              // Excentriques
  QSetStyleSymbole( 8, LowerCase('OBJECT-STALACTITES')     , ListeNatureSymboles[Ord(nosSTALACTITES)      ] , clBlack, SEUIL_TOUS_DETAILS);              // Stalactites
  QSetStyleSymbole( 9, LowerCase('OBJECT-COLUMNS')         , ListeNatureSymboles[Ord(nosCOLONNES)         ] , clBlack, SEUIL_TOUS_DETAILS);              // Colonnes
  QSetStyleSymbole(10, LowerCase('OBJECT-STALAGMITES')     , ListeNatureSymboles[Ord(nosSTALAGMITES)      ] , clBlack, SEUIL_TOUS_DETAILS);              // Stalagmites
  QSetStyleSymbole(11, LowerCase('OBJECT-ARAGONITES')      , ListeNatureSymboles[Ord(nosCRISTAUX)         ] , clBlack, SEUIL_TOUS_DETAILS);              // Cristaux d'aragonite
  QSetStyleSymbole(12, LowerCase('OBJECT-FAILLES')         , ListeNatureSymboles[Ord(nosFRACTURES)        ] , clBlack, SEUIL_TOUS_DETAILS);
  QSetStyleSymbole(13, LowerCase('OBJECT-CUPULES')         , ListeNatureSymboles[Ord(nosCUPULES)          ] , clBlack, SEUIL_TOUS_DETAILS);
  QSetStyleSymbole(14, LowerCase('OBJECT-ZEFF')            , ListeNatureSymboles[Ord(nosZEFF)             ] , clBlack, SEUIL_TOUS_DETAILS);
  QSetStyleSymbole(15, LowerCase('OBJECT-ARRIVEE-EAU')     , ListeNatureSymboles[Ord(nosARRIVEE_EAU)      ] , clBlack, SEUIL_TOUS_DETAILS);
  QSetStyleSymbole(16, LowerCase('OBJECT-PERTE')           , ListeNatureSymboles[Ord(nosPERTE)            ] , clBlack, SEUIL_TOUS_DETAILS);
  QSetStyleSymbole(17, LowerCase('OBJECT-DESOB')           , ListeNatureSymboles[Ord(nosDESOB)            ] , clBlack, SEUIL_TOUS_DETAILS);
  QSetStyleSymbole(18, LowerCase('OBJECT-DANGER')          , ListeNatureSymboles[Ord(nosDANGER)           ] , clBlack, SEUIL_TOUJOURS_VISIBLE);
  QSetStyleSymbole(19, LowerCase('OBJECT-GOUFFRE')         , ListeNatureSymboles[Ord(nosGOUFFRE_SURFACE)  ] , clBlack, SEUIL_TOUJOURS_VISIBLE);
  QSetStyleSymbole(20, LowerCase('OBJECT-GROTTE')          , ListeNatureSymboles[Ord(nosGROTTE_SURFACE)   ] , clBlack, SEUIL_TOUJOURS_VISIBLE);
  QSetStyleSymbole(21, LowerCase('OBJECT-PT-REMARQUABLE')  , ListeNatureSymboles[Ord(nosPOINT_REMARQUABLE)] , clBlack, SEUIL_TOUJOURS_VISIBLE);
end;

procedure TDocumentDessin.SetDocumentName(const N: string);
begin
  FDocumentName := N;
end;

procedure TDocumentDessin.AddStyleLigne(const LS: TStyleLigne);
begin
  FListeLinesStyles.AddElement(LS);
end;

function  TDocumentDessin.GetStyleLigne(const Idx: integer): TStyleLigne;
begin
  Result := FListeLinesStyles.GetElement(Idx);
end;

procedure TDocumentDessin.PutStyleLigne(const Idx: integer; const LS: TStyleLigne);
begin
  FListeLinesStyles.PutElement(Idx, LS);
end;
//----------------
procedure TDocumentDessin.AddStyleCourbe(const LS: TStyleCourbe);
begin
  FListeCurveStyles.AddElement(LS);
end;
function  TDocumentDessin.GetStyleCourbe(const Idx: integer): TStyleCourbe;
begin
  Result := FListeCurveStyles.GetElement(Idx);
end;

procedure TDocumentDessin.PutStyleCourbe(const Idx: integer; const LS: TStyleCourbe);
begin
  FListeCurveStyles.PutElement(Idx, LS);
end;
procedure TDocumentDessin.AddStyleTexte(const LS: TStyleTexte);
begin
  FListeTextStyles.AddElement(LS);
end;
function  TDocumentDessin.GetStyleTexte(const Idx: integer): TStyleTexte;
begin
  Result := FListeTextStyles.GetElement(Idx);
end;

procedure TDocumentDessin.PutStyleTexte(const Idx: integer; const LS: TStyleTexte);
begin
  FListeTextStyles.PutElement(Idx, LS);
end;
procedure TDocumentDessin.AddStylePolygone(const LS: TStylePolygone);
begin
  FListePolygonStyles.AddElement(LS);
end;
function  TDocumentDessin.GetStylePolygone(const Idx: integer): TStylePolygone;
begin
  Result := FListePolygonStyles.GetElement(Idx);
end;

procedure TDocumentDessin.PutStylePolygone(const Idx: integer; const LS: TStylePolygone);
begin
  FListePolygonStyles.PutElement(Idx, LS);
end;

procedure TDocumentDessin.AddStyleSymbole(const LS: TStyleSymboles);
begin
  FListeSymbolStyles.AddElement(LS);
end;

function TDocumentDessin.GetStyleSymbole(const Idx: integer): TStyleSymboles;
begin
  Result := FListeSymbolStyles.GetElement(Idx);
end;

procedure TDocumentDessin.PutStyleSymbole(const Idx: integer; const LS: TStyleSymboles);
begin
  FListeSymbolStyles.PutElement(Idx, LS);
end;


// sauvegarde et restauration de styles
//******************************************************************************
function TDocumentDessin.SaveStylesInFile(const QFilename: string): boolean;
var
  WU: string;
  fp: TextFile;
  i: integer;
  MyStyleCourbes  : TStyleCourbe;
  MyStyleLignes   : TStyleLigne;
  StyleFill       : TStyleFill;
  MyStylePolygon  : TStylePolygone;
  MyStyleTexte    : TStyleTexte;
  StyleSymbole    : TStyleSymboles;
  Nb: Integer;
begin
  Result := False;
  AfficherMessage(Format('%s.SaveStylesInFile(%s)', [self.ClassName, QFileName]));
  assignfile(fp, QFilename);
  try
    ReWrite(fp);
    WriteLn(fp, '# Fichier de feuilles de style: ' + QFileName);
    WriteLn(fp, '# Date: ' + DateTimeToStr(Now));
    WriteLn(fp, STYLESSECTION);
    WU := 'Styles de courbes';
    AfficherMessage(' -- ' + WU);
    WriteLn(fp, '  # '+ WU);
    WriteLn(fp, '  ' + STYLECURVES);
    Writeln(fp, '    # ' + #9 + 'IDStyle' + #9 + 'SVG Style' + #9 +
                            'LineColor' + #9 +
                            'Screen Linewidth' + #9 + 'Printer linewidth' + #9 +
                            'Barbules style' + #9 + 'Barbules size' + #9 +
                            'Visibility limit' + #9 +
                            'Description');
    Nb := GetNbStylesCourbe;
    for i:= 0 to Nb -1 do
    begin
      MyStyleCourbes := self.GetStyleCourbe(i);
      WU := '    ' + STYLECURVE + #9;
      WU := WU + Format('%d' + #9 + '%s' + #9 +
                        '%d' + #9 +
                        '%d' + #9 + '%.2f' + #9 +
                        '%d' + #9 + '%.2f' + #9 +
                        '%f' + #9 +
                        '%s',
                        [MyStyleCourbes.IDStyle,
                         MyStyleCourbes.NameSVGStyle,
                         MyStyleCourbes.LineColor,
                         MyStyleCourbes.LineWidth, MyStyleCourbes.PrintLineWidth,
                         Ord(MyStyleCourbes.Barbules), MyStyleCourbes.LongBarbules,
                         MyStyleCourbes.SeuilVisibilite,
                         MyStyleCourbes.DescStyle
                        ]);
      WriteLn(fp, WU);
    end;
    WriteLn(fp, '  ' + ENDSTYLECURVES);

    WU := 'Styles de lignes';
    AfficherMessage(' -- ' + WU);
    WriteLn(fp, '  # '+ WU);
    WriteLn(fp, '  ' + STYLELINES);
    Writeln(fp, '    # ' + #9 + 'IDStyle' + #9 + 'SVG Style' + #9 +
                            'LineColor' + #9 +
                            'Screen Linewidth' + #9 + 'Printer linewidth' + #9 +
                            'Visibility limit' + #9 +
                            'Description');
    Nb := GetNbStylesLigne;
    for i:= 0 to Nb -1 do
    begin
      MyStyleLignes := self.GetStyleLigne(i);
      WU := '    ' + STYLELINE + #9;
      WU := WU + Format('%d' + #9 + '%s' + #9 +
                        '%d' + #9 +
                        '%d' + #9 + '%.2f' + #9 +
                        '%f' + #9 +
                        '%s',
                       [MyStyleLignes.IDStyle,
                        MyStyleLignes.NameSVGStyle,
                        MyStyleLignes.LineColor,
                        MyStyleLignes.LineWidth, MyStyleLignes.PrintLineWidth,
                        MyStyleLignes.SeuilVisibilite,
                        MyStyleLignes.DescStyle
                       ]);
      WriteLn(fp, WU);
    end;

    WriteLn(fp, '  ' + ENDSTYLELINES);

    WU := 'Styles de polygones';
    AfficherMessage(' -- ' + WU);
    WriteLn(fp, '  # '+ WU);
    WriteLn(fp, '  ' + STYLEPOLYGONS);
    Writeln(fp, '    # ' + #9 + 'IDStyle' + #9 + 'SVG Style' + #9 +
                                'Polygon Fill Style' + #9 +
                                'Polygon Fillcolor' + #9 + 'Screen linewidth' + #9 +
                                'Visibility limit' + #9 +
                                'Description');
    Nb := GetNbStylesPolygones;
    for i:= 0 to Nb -1 do
    begin
      MyStylePolygon := self.GetStylePolygone(i);
      WU := '    ' + STYLEPOLYGON;
      WU := WU + Format('%d' + #9 + '%s' + #9 +
                        '%d' + #9 + '%d' + #9 + '%.2f' + #9 +
                        '%f' + #9 +
                        '%s',
                       [MyStylePolygon.IDStyle,
                        MyStylePolygon.NameSVGStyle,
                        MyStylePolygon.Style, MyStylePolygon.FillColor, MyStylePolygon.LineWidth,
                        MyStylePolygon.SeuilVisibilite,
                        MyStylePolygon.DescStyle
                       ]);
      WriteLn(fp, WU);
    end;
    WriteLn(fp, '  ' + ENDSTYLEPOLYGONS);
    WU := 'Styles de texte';
    AfficherMessage(' -- ' + WU);
    WriteLn(fp, '  # '+ WU);
    WriteLn(fp, '  ' + STYLETEXTS);
    Writeln(fp, '    # ' + #9 + 'IDStyle' + #9 + 'SVG Style' + #9 +
                                'FontName' + #9 + 'FontColor' + #9 +
                                'Screen FontHeight' + #9 + 'Printer FontHeight' + #9 +
                                'FontAttr' + #9 +
                                'Visibility limit' + #9 +
                                'Description');
    Nb := GetNbStylesTexte;
    for i:= 0 to Nb -1 do
    begin
      MyStyleTexte := self.GetStyleTexte(i);
      WU := '    ' + STYLETEXT;
      WU := WU + Format('%d' + #9 + '%s' + #9 +
                        '%s' + #9 + '%d' + #9 +
                        '%d' + #9 + '%.2f' + #9 +
                        '%d' +
                        '%f' + #9 +
                        '%s',
                       [MyStyleTexte.IDStyle,
                        MyStyleTexte.NameSVGStyle,
                        MyStyleTexte.FontName, MyStyleTexte.FontColor,
                        10, MyStyleTexte.FontPrnHeight, // Hauteur texte en points: 10
                        AttributsFontePas2GHCD(MyStyleTexte.FontStyle),
                        MyStyleTexte.SeuilVisibilite,
                        MyStyleTexte.DescStyle
                       ]);
      WriteLn(fp, WU);
    end;
    WriteLn(fp, '  ' + ENDSTYLETEXTS);
    WriteLn(fp, ENDSTYLESSECTION);
    Result := True;
  finally
    CloseFile(fp);
  end;
end;

function TDocumentDessin.LoadStylesFromFile(const QFilename: string): boolean;
var
  fp: TextFile;
  NoLine: Integer;
  MyLine: String;
  // StringLists temporaires: on se réserve la possibilité de faire de nouveaux styles
  qLSStylesCourbes   : TStringList;
  qLSStylesLignes    : TStringList;
  qLSStylesPolygones : TStringList;
  qLSStylesTextes    : TStringList;

  function LireLigne: string;
  begin
    ReadLn(fp, Result);
    Result := Trim(Result);
    Inc(NoLine);
  end;
  function LireSection(const MyStyleObjet: TLesStylesObjets): boolean;
  var
    MySect: String;
    MyEndSect: String;
    MyListeStyles: TStringList;
    procedure qEWE(const S1, S2: string; const LSD: TStringList);
    begin
      MySect    := S1;
      MyEndSect := S2;
      MyListeStyles := LSD;
    end;
  begin
    Result := False;
    case MyStyleObjet of
      sdoCURVES   : qEWE(STYLECURVES  , ENDSTYLECURVES  , qLSStylesCourbes);
      sdoLINES    : qEWE(STYLELINES   , ENDSTYLELINES   , qLSStylesLignes);
      sdoPOLYGONS : qEWE(STYLEPOLYGONS, ENDSTYLEPOLYGONS, qLSStylesPolygones);
      sdoTEXTES   : qEWE(STYLETEXTS   , ENDSTYLETEXTS   , qLSStylesTextes);
    end;
    if (MyLine = MySect) then
    begin
      try
        AfficherMessage(Format('--> (%d) - ''%s'' section found',[NoLine, MySect]));
        while (True) do
        begin
          MyLine := LireLigne;
          if (MyLine = '') then Continue;          // lignes vides
          if (Pos('#', MyLine) > 0) then Continue; // commentaires
          if (MyLine = MyEndSect) then
          begin
            AfficherMessage('--> '''+ MyEndSect +''' endsection found');
            Break;
          end;
          AfficherMessage('--- ' + MyLine);

        end; // while (True) do begin
        Result := True;
      except
        AfficherMessageErreur(Format('Error in %d: %s',[NoLine, MyLine]));
      end;
    end;
  end;
  //
  procedure qSetListeStyles(const MS: TLesStylesObjets);
  begin
    pass;
  end;
begin
  Result := False;
  NoLine := 0;
  qLSStylesCourbes   := TStringList.Create;
  qLSStylesLignes    := TStringList.Create;
  qLSStylesPolygones := TStringList.Create;
  qLSStylesTextes    := TStringList.Create;
  AfficherMessage(Format('%s.LoadStylesFromFile(%s)', [self.ClassName, QFileName]));
  if (not FileExists(QFilename)) then
  begin
    AfficherMessage(format('--> %s introuvable', [QFilename]));
    Exit;
  end;
  try
    // vidage des listes temporaires
    qLSStylesCourbes.Clear;
    qLSStylesLignes.Clear;
    qLSStylesPolygones.Clear;
    qLSStylesTextes.Clear;
    //************************************************************
    // lecture du fichier
    AssignFile(fp, QFilename);
    try
      ReSet(fp);
      while (not EOF(fp)) do
      begin
        MyLine := LireLigne;
        if (MyLine = '') then Continue;
        if (Pos('#', MyLine) > 0) then Continue; // commentaires
        // lecture de la section STYLES
        if (MyLine = STYLESSECTION) then
        begin
          AfficherMessage(Format('--> (%d) - ''%s'' section found',[NoLine, STYLESSECTION]));
          while (True) do
          begin
            MyLine := LireLigne;
            if (MyLine ='') then Continue;
            if (Pos('#', MyLine) > 0) then Continue; // commentaires
            if (MyLine = ENDSTYLESSECTION) then
            begin
              AfficherMessage(Format('--> (%d) - ''%s'' endsection found',[NoLine, ENDSTYLESSECTION]));
              Break;
            end;
            if (LireSection(sdoCURVES))   then qSetListeStyles(sdoCURVES);           // section STYLECURVES;
            if (LireSection(sdoLINES))    then qSetListeStyles(sdoLINES);            // section STYLELINES;
            if (LireSection(sdoPOLYGONS)) then qSetListeStyles(sdoPOLYGONS);         // section STYLEPOLYGONS;
            if (LireSection(sdoTEXTES))   then qSetListeStyles(sdoTEXTES);           // section STYLETEXTS;
          end;
        end; //if (Line = STYLESSECTION) then begin
        Result := True;
      end; // while not eof
    except
    end;
  finally
    CloseFile(fp);
    FreeAndNil(qLSStylesCourbes);  //     qLSStylesCourbes.Free;
    FreeAndNil(qLSStylesLignes);   //     qLSStylesLignes.Free;
    FreeAndNil(qLSStylesPolygones);//     qLSStylesPolygones.Free;
    FreeAndNil(qLSStylesTextes);   //     qLSStylesTextes.Free;
  end;
end;

procedure TDocumentDessin.MarquerCourbesAEffacer(const Idx: integer);
var
  EWE: TCourbe;
begin
  EWE := GetCourbe(Idx);
  EWE.MarkToDelete := True;
  PutCourbe(Idx, EWE);
end;


// supprimer les objets problématiques
procedure TDocumentDessin.NettoyerObjetsProblematiques();
  // nettoyage des courbes
  procedure NettoyerLesCourbes();
  var
    MyCourbe: TCourbe;
    NbDeleted: Integer;
    i, Nb: Integer;
  begin
    AfficherMessage('---> Courbes');
    // Nettoyage des courbes
    Nb := GetNbCourbes();
    for i := 0 to Nb -1 do
    begin
      MyCourbe := GetCourbe(i);
      if (PurgerCourbe(self, MyCourbe)) then PutCourbe(i, MyCourbe);
    end;
    NbDeleted := 0;
    // et suppression des objets invalides
    for i := GetNbCourbes() - 1 downto 0 do
    begin
      MyCourbe := self.GetCourbe(i);
      if (not CourbeEstValide(MyCourbe)) then
      begin
        AfficherMessageErreur(Format('Courbe %d invalide. Suppression', [i]));
        self.DeleteCourbe(i);
        NbDeleted += 1;
      end;
    end;
    AfficherMessage(Format('%d objets supprimes', [NbDeleted]));
  end;
  // nettoyage des scraps
  procedure NettoyerLesScraps();
  var
    NbDeleted: Integer;
    myScrap: TScrap;
    i, Nb: Integer;
  begin
    AfficherMessage('---> Scraps');
    // Passe 1: purge des scraps
    Nb := GetNbScraps();
    for i := 0 to Nb -1 do
    begin
      myScrap := GetScrap(i);
      if (PurgerScrap(self, myScrap)) then PutScrap(i, myScrap);
    end;
    // Passe 2: Suppression des objets invalides
    NbDeleted := 0;
    for i := GetNbScraps() - 1 downto 0 do // while (idx < GetNbScraps()) do
    begin
      myScrap := self.GetScrap(i);
      if (not ScrapEstValide(myScrap)) then
      begin
        AfficherMessageErreur(Format('Scrap %d invalide. Suppression', [i]));
        self.DeleteScrap(i);
        NbDeleted += 1;
      end;
    end;
    AfficherMessage(Format('%d objets supprimes', [NbDeleted]));
  end;

  // nettoyage des polygones
  procedure NettoyerLesPolygones();
  var
    NbDeleted: Integer;
    myPolygone: TPolygone;
    i, Nb: Integer;
  begin
    AfficherMessage('---> Polygones');
    // Passe 1: Purge des polygones
    Nb := GetNbPolygones();
    for i := 0 to Nb -1 do
    begin
      myPolygone := GetPolygone(i);
      if (PurgerPolyGone(self, myPolygone)) then PutPolygone(i, myPolygone);
    end;
    // Passe 2: Suppression des objets invalides
    NbDeleted := 0;
    for i := GetNbPolygones() - 1 downto 0 do
    begin
      myPolygone := self.GetPolygone(i);
      if (not PolygoneEstValide(myPolygone)) then
      begin
        AfficherMessageErreur(Format('Polygone %d invalide. Suppression', [i]));
        self.DeletePolygone(i);
        NbDeleted += 1;
      end;
    end;
    AfficherMessage(Format('%d objets supprimes', [NbDeleted]));
  end;
  // nettoyage des polylignes
  procedure NettoyerLesPolylignes();
  var
    NbDeleted: Integer;
    myPolyline: TPolyLigne;
    i, Nb: Integer;
  begin
    AfficherMessage('---> Polylines');
    // Passe 1: Purge des polylignes
    Nb := GetNbPolylignes();
    for i := 0 to Nb -1 do
    begin
      myPolyline := GetPolyligne(i);
      if (PurgerPolyligne(self, myPolyline)) then PutPolyligne(i, myPolyline);
    end;
    // Passe 2: Suppression des objets invalides
    NbDeleted := 0;
    for i := GetNbPolylignes() - 1 downto 0 do
    begin
      myPolyline := self.GetPolyligne(i);
      if (not PolylineEstValide(myPolyline)) then
      begin
        AfficherMessageErreur(Format('Polyline %d invalide. Suppression', [i]));
        self.DeletePolyligne(i);
        NbDeleted += 1;
      end;
    end;
    AfficherMessage(Format('%d objets supprimes', [NbDeleted]));
  end;
  // nettoyage des lignes
  procedure NettoyerLesSimplesLignes();
  var
    i: Integer;
    NbDeleted: Integer;
    mySimpleLine: TSimpleLigne;
  begin
    AfficherMessage('---> Simples lignes');
    // Passe unique: Suppression des objets invalides
    NbDeleted := 0;
    for i := GetNbSimpleLignes() - 1 downto 0 do
    begin
      mySimpleLine := self.GetSimpleLigne(i);
      if (not SimpleLigneEstValide(mySimpleLine)) then
      begin
        AfficherMessageErreur(Format('Ligne %d invalide. Suppression', [i]));
        self.DeleteSimpleLigne(i);
        NbDeleted += 1;
      end;
    end;
    AfficherMessage(Format('%d objets supprimes', [NbDeleted]));
  end;
   // nettoyage des symboles
  procedure NettoyerLesSymboles();
  var
    i: Integer;
    NbDeleted: Integer;
    mySymbole: TSymbole;
  begin
    AfficherMessage('---> Symboles');
    // Passe unique: Suppression des objets invalides
    NbDeleted := 0;
    for i := GetNbSymboles() - 1 downto 0 do
    begin
      mySymbole := self.GetSymbole(i);
      if (not SymboleEstValide(mySymbole)) then
      begin
        AfficherMessageErreur(Format('Symbole %d invalide. Suppression', [i]));
        self.DeleteSymbole(i);
        NbDeleted += 1;
      end;
    end;
    AfficherMessage(Format('%d objets supprimes', [NbDeleted]));
  end;
  // nettoyage des textes
  procedure NettoyerLesTextes();
  var
    i: Integer;
    NbDeleted: Integer;
    MyTexte: TTextObject;
  begin
    AfficherMessage('---> Textes');
    // Passe unique: Suppression des objets invalides
    NbDeleted := 0;
    for i := GetNbTextes() - 1 downto 0 do
    begin
      MyTexte := self.GetTexte(i);
      if (not TexteEstValide(MyTexte)) then
      begin
        AfficherMessageErreur(Format('Objet texte %d invalide. Suppression', [i]));
        self.DeleteTexte(i);
        NbDeleted += 1;
      end;
    end;
    AfficherMessage(Format('%d objets supprimes', [NbDeleted]));
  end;
begin
  AfficherMessage(Format('%s.NettoyerObjetsProblematiques()', [self.ClassName]));
  NettoyerLesScraps();
  NettoyerLesCourbes();
  NettoyerLesPolygones();
  NettoyerLesPolylignes();
  NettoyerLesSimplesLignes();
  NettoyerLesSymboles();
  NettoyerLesTextes();
  CalcBoundingBoxAllGroupes();
end;

//******************************************************************************
// Objets dégénérés ?
function TDocumentDessin.ScrapEstValide(const QP: TScrap): boolean;
var
  s, QNb, QQ: Integer;
  QVertex, V0, V1: TVertexPolygon;
  BS1, BS0: TBaseStation;
  Q1: Boolean;
  PP0, PP1: TPoint2Df;
  d: float;
begin
  Result := false;
  QNb := Length(QP.Sommets);
  if (QNb = 0) then Exit; // nombre de sommets nul => sortie
  for s := 0 to QNb - 1 do // les basepoints sont OK ?
  begin
   try
     QVertex := QP.Sommets[s];
     // si un des basepoints est introuvable ==> sortie (évaluation parresseuse)
     if (Not GetBasePointByIndex(QVertex.IDStation, BS1)) then Exit;
   except
     Result := false;
   end;
  end;
  // check des scraps dégénérés; critère: Longueur d'un côté > 200 m
  for s := 0 to QNb - 1 do
  begin
    QQ := s + 1;
    if (QQ > (QNb - 1)) then QQ := 0;
    V0 := QP.Sommets[s];
    V1 := QP.Sommets[qq];
    if (GetBasePointByIndex(V0.IDStation, BS0) AND
        GetBasePointByIndex(V1.IDStation, BS1)) then
    begin
      PP0.setFrom(BS0.PosStation.X + V0.Offset.X, BS0.PosStation.Y + V0.Offset.Y);
      PP1.setFrom(BS1.PosStation.X + V1.Offset.X, BS1.PosStation.Y + V1.Offset.Y);
      d := Hypot(PP1.X - PP0.X, PP1.Y - PP0.Y);
      if (d > 200.00) then Exit;
    end;
  end;
  // passage ici = OK
  Result := True;
end;

procedure TDocumentDessin.SetCodeEPSG(const C: TCodeEPSG);
begin
  FCodeEPSG := C;
end;

// Test 1: Validité ou existence des Points de base
function TDocumentDessin.CourbeEstValide(const myCourbe: TCourbe): boolean;
var
 a, QNb: Integer;
 QArc: TArcCourbe;
 BS1: TBaseStation;
 BS2: TBaseStation;
 Q1, Q2, QT: Boolean;
begin
 Result := false;
 QNb := Length(myCourbe.Arcs);
 if (QNb = 0) then Exit; // nombre d'arcs nul => sortie
 for a := 0 to QNb - 1 do
 begin
   try
     QArc := myCourbe.Arcs[a];
     // si les basepoints sont introuvables ==> sortie
     Q1   := GetBasePointByIndex(QArc.IDStationP1, BS1);
     Q2   := GetBasePointByIndex(QArc.IDStationP2, BS2);
     QT   := Q1 or Q2;
     if (not QT) then
     begin
       AfficherMessageErreur('Courbe dégénérée');
       Exit;
     end;
   except
     Result := false;
   end;
 end;
 Result := true;
end;

function TDocumentDessin.DeleteAllObjectsOfGroupe(const Grp: TGroupeEntites): integer;
var
  i, Nb: Integer;
  SC: TScrap;
  CC: TCourbe;
  PL: TPolyLigne;
  PG: TPolygone;
  SL: TSimpleLigne;
  SB: TSymbole;
  TT: TTextObject;
begin
  AfficherMessage(Format('%s.DeleteAllObjectsOfGroupe: %d - %s', [ClassName, Grp.IDGroupeEntites, Grp.NomGroupe]));
  Nb := GetNbScraps();
  if (Nb > 0) then
  begin
    for i := Nb - 1  downto 0 do
    begin
      SC := GetScrap(i);
      if (Grp.IDGroupeEntites = SC.IDGroupe) then DeleteScrap(i);
    end;
  end;
  Nb := GetNbCourbes();
  if (Nb > 0) then
  begin
    for i := Nb - 1  downto 0 do
    begin
      CC := GetCourbe(i);
      if (Grp.IDGroupeEntites = CC.IDGroupe) then DeleteCourbe(i);
    end;
  end;
  Nb := GetNbPolylignes();
  if (Nb > 0) then
  begin
    for i := Nb - 1  downto 0 do
    begin
      PL := GetPolyligne(i);
      if (Grp.IDGroupeEntites = PL.IDGroupe) then DeletePolyligne(i);
    end;
  end;
  Nb := GetNbPolygones();
  if (Nb > 0) then
  begin
    for i := Nb - 1  downto 0 do
    begin
      PG := GetPolygone(i);
      if (Grp.IDGroupeEntites = PG.IDGroupe) then DeletePolygone(i);
    end;
  end;
  Nb := GetNbSimpleLignes();
  if (Nb > 0) then
  begin
    for i := Nb - 1  downto 0 do
    begin
      SL := GetSimpleLigne(i);
      if (Grp.IDGroupeEntites = SL.IDGroupe) then DeleteSimpleLigne(i);
    end;
  end;
  Nb := GetNbSymboles();
  if (Nb > 0) then
  begin
    for i := Nb - 1  downto 0 do
    begin
      SB := GetSymbole(i);
      if (Grp.IDGroupeEntites = SB.IDGroupe) then DeleteSymbole(i);
    end;
  end;
  Nb := GetNbTextes();
  if (Nb > 0) then
  begin
    for i := Nb - 1  downto 0 do
    begin
      TT := GetTexte(i);
      if (Grp.IDGroupeEntites = TT.IDGroupe) then DeleteTexte(i);
    end;
  end;
end;

function TDocumentDessin.PolygoneEstValide(const QP: TPolygone): boolean;
var
 s, QNb: Integer;
 QVertex: TVertexPolygon;
 BS1: TBaseStation;
begin
 Result := false;  // Entité supposée dégénérée
 QNb := Length(QP.Sommets);
 if (QNb = 0) then Exit; // nombre de sommets nul => sortie
 for s := 0 to QNb - 1 do
 begin
   try
     QVertex := QP.Sommets[s];
     // si un des basepoints est introuvable ==> sortie (évaluation parresseuse)
     if (not GetBasePointByIndex(QVertex.IDStation, BS1)) then Exit;
   except
     Result := false;
   end;
 end;
 Result := True;
end;

function TDocumentDessin.PolylineEstValide(const QP: TPolyLigne): boolean;
var
 s, QNb: Integer;
 QVertex: TVertexPolygon;
 BS1: TBaseStation;
begin
 Result := false;  // Entité supposée dégénérée
 QNb := Length(QP.Sommets);
 if (QNb = 0) then Exit; // nombre de sommets nul => sortie
 for s := 0 to QNb - 1 do
 begin
   try
     QVertex := QP.Sommets[s];
     // si un des basepoints est introuvable ==> sortie (évaluation parresseuse)
     if (not GetBasePointByIndex(QVertex.IDStation, BS1)) then exit;
   except
     Result := false;
   end;
 end;
 Result := True;
end;

procedure TDocumentDessin.PurgerListeCenterlines(const CenterlinesIsIncludeFileGCP: boolean);
begin
  FCenterLines.ClearListe();
  FCenterlinesIsIncludeFileGCP := CenterlinesIsIncludeFileGCP;
end;
// Dans ces fonctions, on vérifie tout d'abord si les Basepoints sont corrects
function TDocumentDessin.TexteEstValide(const QT: TTextObject): boolean;
var
 BS1: TBaseStation;
begin
 Result := false;  // Entité supposée dégénérée
 if ('' = Trim(QT.Text)) then Exit; // texte vide ? OK, je sort
 try
   if (not GetBasePointByIndex(QT.IDBaseSt, BS1)) then Exit;
   // TODO: autres tests de validité
 except
 end;
 Result := True;
end;

function TDocumentDessin.SymboleEstValide(const QS: TSymbole): boolean;
var
 BS1: TBaseStation;
begin
 Result := false;  // Entité supposée dégénérée
 try
   if (not GetBasePointByIndex(QS.IDBaseStation, BS1)) then Exit;
   // TODO: autres tests de validité
 except
 end;
 Result := True;
end;

function TDocumentDessin.SimpleLigneEstValide(const QL: TSimpleLigne): boolean;
var
 BS1, BS2: TBaseStation;
 Q1, Q2: Boolean;
begin
 Result := false;
 try
   Q1 := GetBasePointByIndex(QL.IDBaseStExt1, BS1);
   Q2 := GetBasePointByIndex(QL.IDBaseStExt2, BS2);
   if (Not (Q1 or Q2)) then Exit;
   // TODO: autres tests de validité
 except
 end;
 Result := True;
end;
//******************************************************************************


//******************************************************************************
// fermer une polyligne
function TDocumentDessin.FermerPolyligne(var P: TPolyLigne): boolean;
var
  N: Integer;
begin
  Result := False;
  try
    AfficherMessage('Fermeture polyligne');
    N := High(P.Sommets);
    P.Sommets[N] := P.Sommets[0];
    P.Closed := True;
    P := CalcBoundingBoxPolyligne(P);
    Result := True;
  except
  end;
end;

//******************************************************************************
// fusionner deux courbes ou polylignes
// L'objet résultant hérite des propriétés de PC1
// La polyligne résultat est supposée partir du dernier vertex de P1
function TDocumentDessin.FusionnerDeuxCourbes(const PC1, PC2: TCourbe; out PCFusionnee: TCourbe): boolean;
begin
  Result := false;
  PCFusionnee.Closed          := PC1.Closed;
  PCFusionnee.IDGroupe        := PC1.IDGroupe;
  PCFusionnee.IDStyleCourbe   := PC1.IDStyleCourbe;
  PCFusionnee.LastModified    := Now();
end;
function TDocumentDessin.FusionnerDeuxPolylignes(const PC1, PC2: TPolyLigne; out PCFusionnee: TPolyLigne): boolean;
var
  i, NbS, Nb1, Nb2: Integer;
begin
  Result := false;
  try
    PCFusionnee.Closed          := PC1.Closed;
    PCFusionnee.IDGroupe        := PC1.IDGroupe;
    PCFusionnee.IDStylePolyLine := PC1.IDStylePolyLine;
    PCFusionnee.LastModified    := Now();
    Nb1 := 1 + High(PC1.Sommets);
    Nb2 := 0 + High(PC2.Sommets);
    // TODO: Contrôler la taille du tableau résultat
    SetLength(PCFusionnee.Sommets, Nb1 + Nb2);
    NbS := 0;
    for i := 0 to High(PC1.Sommets) do
    begin
      PCFusionnee.Sommets[NbS] := PC1.Sommets[i];
      NbS += 1;
    end;
    // ne pas copier le premier sommet de P2, qui est aussi le dernier sommet de P1
    for i := 1 to High(PC2.Sommets) do
    begin
      PCFusionnee.Sommets[NbS] := PC2.Sommets[i];
      NbS += 1;
    end;
    PCFusionnee := CalcBoundingBoxPolyligne(PCFusionnee);
    Result := True;
  except
  end;
end;
function TDocumentDessin.FusionnerDeuxPolylignesByIdx(const IdxPC1, IdxPC2: integer): boolean;
var
  EWE1, EWE2   : Boolean;
  PCR, PC1, PC2: TPolyLigne;
begin
  Result := false;
  try
    if (IdxPC1 = IdxPC2) then Exit; // polylignes identiques ? -->  [ ]
    EWE1 := Not IsInRange(IdxPC1, 0, GetNbPolylignes() - 1);
    EWE2 := Not IsInRange(IdxPC2, 0, GetNbPolylignes() - 1);
    if (EWE1 or EWE2) then Exit; // mauvais index ? -->  [ ]

    PC1 := GetPolyligne(IdxPC1);
    PC2 := GetPolyligne(IdxPC2);
    if (Not FusionnerDeuxPolylignes(PC1, PC2, PCR)) then
    begin
      self.DeletePolyligne(IdxPC1); // suppression polyligne 1 à fusionner
      self.DeletePolyligne(IdxPC1); // suppression polyligne 2 à fusionner
      self.AddPolyLigne(PCR, True);       // on ajoute la polyligne résultat
    end;
    Result := true;
  except

  end;
end;



// Couper une polyligne ou courbe en deux
function TDocumentDessin.CasserUnePolyligne(const PC: TPolyLigne; const PointCoupure: TIDBaseStation; out PC1, PC2: TPolyLigne): boolean;
var
  Nb: Integer;
  NoPtCoupure: Integer;
  // recherche de l'index de sommet correspondant au pt de coupure
  function FindIdxSommetCoupure: integer;
  var
    V: TVertexPolygon;
    BP: TBaseStation;
    s: Integer;
  begin
    Result := -1;
    for s := 0 to Nb - 1 do
    begin
      V := PC.Sommets[s];
      if (GetBasePointByIndex(V.IDStation, BP)) then
      begin
        if (PointCoupure = BP.IDStation) then
        begin
          Result := S;
          Exit;
        end;
      end;
    end;
  end;
begin
  Result := false;
  Nb := 1 + High(PC.Sommets);
  NoPtCoupure := FindIdxSommetCoupure;
  if (NoPtCoupure <= 0) then Exit; // un des points de base de vertex introuvable ou PointCoupure pointe sur le premier sommet: -->[ ]
  if (NoPtCoupure > (Nb-1)) then Exit; // PointCoupure pointe sur le dernier sommet: -->[ ]
  PC1.Closed          := PC.Closed;
  PC1.IDGroupe        := PC.IDGroupe;
  PC1.IDStylePolyLine := PC.IDStylePolyLine;
  PC2.Closed          := PC.Closed;
  PC2.IDGroupe        := PC.IDGroupe;
  PC2.IDStylePolyLine := PC.IDStylePolyLine;

end;

function TDocumentDessin.CenterlinesIsIncludeFileGCP(): boolean;
begin
  result := FCenterlinesIsIncludeFileGCP;
end;

function TDocumentDessin.CasserUneCourbe(const PC: TCourbe; const PointCoupure: TIDBaseStation; out PC1, PC2: TCourbe): boolean;
begin
  Result := false;
end;
//******************************************************************************
// calcul de la superficie d'un polygone ou d'une polyligne
function TDocumentDessin.CalcSuperficiePerimetre(const P: TVertexPolygonArray; out Superficie: double; out Perimetre: double): boolean;
var
  i, n, m: Integer;
  BP: TBaseStation;
  MyPolygoneFerme: array of TPoint2Df;
  V1: TPoint2Df;
  V2: TPoint2Df;
  CG: TPoint2Df;
  WU: Extended;
begin
  AfficherMessage(Format('%s.CalcSuperficiePerimetre()', [ClassName]));
  Result := false;
  Superficie := 0.00;
  Perimetre  := 0.00;
  CG.Empty();
  try
    n := High(P);
    if (n < 2) then Exit; // polygone de moins de trois sommets = aucun sens
    // construction du polygone fermé correspondant
    SetLength(MyPolygoneFerme, n+2);
    m := High(MyPolygoneFerme);
    AfficherMessage(Format('n = %d, m= %d', [n, m]));
    for i := 0 to n do
    begin
      if (Not GetBasePointByIndex(P[i].IDStation, BP)) then Exit;
      MyPolygoneFerme[i].setFrom(BP.PosStation.X + P[i].Offset.X,  BP.PosStation.Y + P[i].Offset.Y);
    end;
    // fermer
    MyPolygoneFerme[m] := MyPolygoneFerme[0];
    for i := 0 to m do AfficherMessage(Format('%d - %.2f, %.2f', [i, MyPolygoneFerme[i].X, MyPolygoneFerme[i].Y]));
    // calcul des valeurs
    for i := 0 to m - 1 do
    begin
      V1 := MyPolygoneFerme[i];
      V2 := MyPolygoneFerme[i+1];
      Perimetre  += Hypot(V2.X - V1.X, V2.Y - V1.Y);
      Superficie += (V1.X * V2.Y) - (V2.X * V1.Y);
    end;
    Superficie := 0.5 * Abs(Superficie) ;
    AfficherMessage(Format('Superficie: %.2f, Perimetre = %.2f', [Superficie, Perimetre]));
    // centre de gravité  https://en.wikipedia.org/wiki/Polygon#Properties
    for i := 0 to m - 1 do
    begin
      V1 := MyPolygoneFerme[i];
      V2 := MyPolygoneFerme[i+1];
      CG.X  += (V1.X + V2.X) * (V1.X * V2.Y - V2.X * V1.Y);
      CG.Y  += (V1.Y + V2.Y) * (V1.X * V2.Y - V2.X * V1.Y);
    end;
    WU := 1/(6*Superficie);
    CG.X := WU * CG.X;
    CG.Y := WU * CG.Y;
    AfficherMessage(Format('Centre de gravité: X = %.2f Y = %.2f', [CG.X, CG.Y]));
    Result := True;
 except
 end;
end;

function TDocumentDessin.CalcSuperficieScrap(const P: TScrap; out Superficie: double; out Perimetre: double): boolean;
begin
  Result := CalcSuperficiePerimetre(P.Sommets, Superficie, Perimetre);
end;

function TDocumentDessin.CalcSuperficiePolygone(const P: TPolygone; out Superficie: double; out Perimetre: double): boolean;
begin
  Result := CalcSuperficiePerimetre(P.Sommets, Superficie, Perimetre);
end;

function TDocumentDessin.CalcSuperficiePolyligne(const P: TPolyLigne; out Superficie: double; out Perimetre: double): boolean;
begin
  Result := CalcSuperficiePerimetre(P.Sommets, Superficie, Perimetre);
end;

function TDocumentDessin.GetInternalIdxGroupe(const Idx: TIDGroupeEntites): integer;
var
  i, Nb: integer;
  GRP: TGroupeEntites;
begin
  //AfficherMessage(Format('%s.GetInternalIdxGroupe: %d', [ClassName, Idx]));
  Result := -1;
  Nb := FListeGroupes.Count;
  for i := 0 to Nb -1 do
  begin
    GRP := GetGroupe(i);
    if (Idx = GRP.IDGroupeEntites) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;
//******************************************************************************
// NETTOYAGE DES GROUPES
//******************************************************************************
// Nombre d'objets pour un groupe
function TDocumentDessin.GetNbScrapsPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
var
  EE: TScrap;
  i, Nb: Integer;
begin
   Result := 0;
   Nb := self.GetNbScraps();
   if (Nb = 0) then Exit;
   for i := 0 to Nb -1 do
   begin
     EE := GetScrap(i);
     if (QIdxGroupe = EE.IDGroupe) then Result += 1;
   end;
end;
function TDocumentDessin.GetNbCourbesPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
var
  EE: TCourbe;
  i, Nb: Integer;
begin
   Result := 0;
   Nb := self.GetNbCourbes();
   if (Nb = 0) then Exit;
   for i := 0 to Nb -1 do
   begin
     EE := GetCourbe(i);
     if (QIdxGroupe = EE.IDGroupe) then Result += 1;
   end;
end;
function TDocumentDessin.GetNbPolylignesPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
var
  EE: TPolyLigne;
  i, Nb: Integer;
begin
   Result := 0;
   Nb := self.GetNbPolylignes();
   if (Nb = 0) then Exit;
   for i := 0 to Nb -1 do
   begin
     EE := GetPolyligne(i);
     if (QIdxGroupe = EE.IDGroupe) then Result += 1;
   end;
end;
function TDocumentDessin.GetNbPolygonesPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
var
  EE: TPolygone;
  i, Nb: Integer;
begin
   Result := 0;
   Nb := self.GetNbPolygones();
   if (Nb = 0) then Exit;
   for i := 0 to Nb -1 do
   begin
     EE := GetPolygone(i);
     if (QIdxGroupe = EE.IDGroupe) then Result += 1;
   end;
end;
function TDocumentDessin.GetNbSimplesLignesPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
var
  EE: TSimpleLigne;
  i, Nb: Integer;
begin
   Result := 0;
   Nb := self.GetNbSimpleLignes();
   if (Nb = 0) then Exit;
   for i := 0 to Nb -1 do
   begin
     EE := GetSimpleLigne(i);
     if (QIdxGroupe = EE.IDGroupe) then Result += 1;
   end;
end;
function TDocumentDessin.GetNbSymbolesPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
var
  EE: TSymbole;
  i, Nb: Integer;
begin
   Result := 0;
   Nb := self.GetNbSymboles();
   if (Nb = 0) then Exit;
   for i := 0 to Nb -1 do
   begin
     EE := GetSymbole(i);
     if (QIdxGroupe = EE.IDGroupe) then Result += 1;
   end;
end;
function TDocumentDessin.GetNbTextesPourUnGroupe(const QIdxGroupe: TIDGroupeEntites): integer;
var
  EE: TTextObject;
  i, Nb: Integer;
begin
   Result := 0;
   Nb := self.GetNbTextes();
   if (Nb = 0) then Exit;
   for i := 0 to Nb -1 do
   begin
     EE := GetTexte(i);
     if (QIdxGroupe = EE.IDGroupe) then Result += 1;
   end;
end;
//******************************************************************************
procedure TDocumentDessin.NettoyerGroupes;
var
  i, Nb: Integer;
  MyGroupe: TGroupeEntites;
  QNbScraps: Integer;
  QNbCourbes: Integer;
  QNbPolylignes: integer;
  QNbPolygones: integer;
  QNbSimplesLignes: integer;
  QNbSymboles: integer;
  QNbTextes: integer;
  QNbEntites: Integer;
  NbGroupesDeleted: Integer;
begin
  AfficherMessage(Format('%s.NettoyerGroupes', [self.ClassName]));

  Nb := GetNbGroupes();
  for i := 0 to Nb - 1 do
  begin
    MyGroupe        := GetGroupe(i);
    QNbScraps       := GetNbScrapsPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbCourbes      := GetNbCourbesPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbPolylignes   := GetNbPolylignesPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbPolygones    := GetNbPolygonesPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbSimplesLignes:= GetNbSimplesLignesPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbSymboles     := GetNbSymbolesPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbTextes       := GetNbTextesPourUnGroupe(MyGroupe.IDGroupeEntites);
    AfficherMessage(Format('%d: %d (%s): %d scraps, %d courbes; %d polylignes, %d polygones, %d lignes, %d symboles, %d textes', [
                    i, MyGroupe.IDGroupeEntites, MyGroupe.NomGroupe,
                    QNbScraps,
                    QNbCourbes,
                    QNbPolylignes,
                    QNbPolygones,
                    QNbSimplesLignes,
                    QNbSymboles,
                    QNbTextes
                   ]));
  end;
  // nettoyage des groupes
  NbGroupesDeleted := 0;
  for i := GetNbGroupes() - 1 downto 0 do //while (i < GetNbGroupes() - 1) do
  begin
    MyGroupe        := GetGroupe(i);
    QNbScraps       := GetNbScrapsPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbCourbes      := GetNbCourbesPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbPolylignes   := GetNbPolylignesPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbPolygones    := GetNbPolygonesPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbSimplesLignes:= GetNbSimplesLignesPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbSymboles     := GetNbSymbolesPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbTextes       := GetNbTextesPourUnGroupe(MyGroupe.IDGroupeEntites);
    QNbEntites := QNbScraps +
                  QNbCourbes +
                  QNbPolylignes +
                  QNbPolygones +
                  QNbSimplesLignes +
                  QNbSymboles +
                  QNbTextes;
    if (QNbEntites = 0) then
    begin
      AfficherMessage(Format('%d: %d (%s) %.2f: Groupe sans entités', [i, MyGroupe.IDGroupeEntites, MyGroupe.NomGroupe, MyGroupe.ZOrder]));
      self.DeleteGroupe(i);
      NbGroupesDeleted += 1;
    end;
  end;
  AfficherMessage(Format('-- %d groupes effacés', [NbGroupesDeleted]));
end;

procedure TDocumentDessin.SupprimerObjetsGroupe(const QGroupe: TGroupeEntites);
var
  NbCourbesDeleted: Integer;
  NbScrapsDeleted: Integer;
  NbPolylinesDeleted: Integer;
  NbPolygonesDeleted: Integer;
  NbSimplesLignesDeleted: Integer;
  NbSymbolesDeleted: Integer;
  NbTextesDeleted: Integer;
  function QDeleteScraps(): integer;
  var
    i: integer;
    EWE: TScrap;
  begin
    Result := 0;
    for i := GetNbScraps() - 1 downto 0 do // while (i < GetNbScraps() - 1) do
    begin
      EWE := GetScrap(i);
      if (QGroupe.IDGroupeEntites = EWE.IDGroupe) then begin DeleteScrap(i); Inc(Result); end;
    end;
  end;
  function QDeleteCourbes(): integer;
  var
    i: integer;
    EWE: TCourbe;
  begin
    Result := 0;
    for i := GetNbCourbes() - 1 downto 0 do // while (i < GetNbCourbes() - 1) do
    begin
      EWE := GetCourbe(i);
      if (QGroupe.IDGroupeEntites = EWE.IDGroupe) then begin DeleteCourbe(i); Inc(Result); end;
    end;
  end;
  function QDeletePolygones(): integer;
  var
    i: integer;
    EWE: TPolygone;
  begin
    Result := 0;
    for i := GetNbPolygones() - 1 downto 0 do // while (i < GetNbPolygones() - 1) do
    begin
      EWE := GetPolygone(i);
      if (QGroupe.IDGroupeEntites = EWE.IDGroupe) then begin DeletePolygone(i); Inc(Result); end;
    end;
  end;
  function QDeletePolylignes(): integer;
  var
    i: integer;
    EWE: TPolyLigne;
  begin
    Result := 0;
    i := 0;
    for i := GetNbPolylignes() - 1 downto 0 do // while (i < GetNbPolylignes() - 1) do
    begin
      EWE := GetPolyligne(i);
      if (QGroupe.IDGroupeEntites = EWE.IDGroupe) then begin DeletePolyligne(i); Inc(Result); end;
    end;
  end;
  function QDeleteSimplesLignes(): integer;
  var
    i: integer;
    EWE: TSimpleLigne;
  begin
    Result := 0;
    for i := GetNbSimpleLignes() - 1 downto 0 do // while (i < GetNbSimpleLignes() - 1) do
    begin
      EWE := GetSimpleLigne(i);
      if (QGroupe.IDGroupeEntites = EWE.IDGroupe) then begin DeleteSimpleLigne(i); Inc(Result); end;
    end;
  end;
  function QDeleteSymboles(): integer;
  var
    i: integer;
    EWE: TSymbole;
  begin
    Result := 0;
    for i := GetNbSymboles() - 1 downto 0 do // while (i < GetNbSymboles() - 1) do
    begin
      EWE := GetSymbole(i);
      if (QGroupe.IDGroupeEntites = EWE.IDGroupe) then begin DeleteSymbole(i); Inc(Result); end;
    end;
  end;
  function QDeleteTextes(): integer;
  var
    i: integer;
    EWE: TTextObject;
  begin
    Result := 0;
    for i := GetNbTextes() - 1 downto 0 do //  while (i < GetNbTextes() - 1) do
    begin
      EWE := GetTexte(i);
      if (QGroupe.IDGroupeEntites = EWE.IDGroupe) then begin  DeleteTexte(i); Inc(Result); end;
    end;
  end;
begin
  AfficherMessage(Format('%s.SupprimerObjetsGroupe: %d - %s', [ClassName, QGroupe.IDGroupeEntites, QGroupe.NomGroupe]));
  NbScrapsDeleted        := QDeleteScraps();
  NbCourbesDeleted       := QDeleteCourbes();
  NbPolylinesDeleted     := QDeletePolylignes();
  NbPolygonesDeleted     := QDeletePolygones();
  NbSimplesLignesDeleted := QDeleteSimplesLignes();
  NbSymbolesDeleted      := QDeleteSymboles();
  NbTextesDeleted        := QDeleteTextes();
end;
// effacement des derniers objets
// retourne l'index du dernier élément
function TDocumentDessin.RemoveLastObject(const M: TModeSelectionEntites): integer;
begin
  Result := 0;
  try
   case M of
     mseNONE      : ;
     mseSCRAPS    : FListeScraps.RemoveLastElement();
     mseCOURBES   : FListeCourbes.RemoveLastElement();
     msePOLYLIGNES: FListePolyLignes.RemoveLastElement();
     mseLIGNES    : FListeSimplesLignes.RemoveLastElement();
     msePOLYGONES : FListePolygones.RemoveLastElement();
     mseSYMBOLES  : FListeSymboles.RemoveLastElement();
     mseTEXTES    : FListeTextObjects.RemoveLastElement();
   end;
   case M of
     mseNONE      : ;
     mseSCRAPS    : Result := self.GetNbScraps() - 1;
     mseCOURBES   : Result := self.GetNbCourbes() - 1;
     msePOLYLIGNES: Result := self.GetNbPolylignes() - 1;
     mseLIGNES    : Result := self.GetNbSimpleLignes() - 1;
     msePOLYGONES : Result := self.GetNbPolygones() - 1;
     mseSYMBOLES  : Result := self.GetNbSymboles() - 1;
     mseTEXTES    : Result := self.GetNbTextes() - 1;
   end;
 except
   AfficherMessage('');
 end;
end;
// exporte des scraps au format OSM et KML
// OSM: OK
// function TDocumentDessin.ExporterScrapsToKMLorOSM(const QFileName: string; const QMode: TModeExportGIS): boolean;
// Fonction volumineuse --> part dans DocDessinExporterScrapsToKMLorOSM.inc
//{$INCLUDE DocDessinExporterScrapsToKMLorOSM.inc}
//{$INCLUDE DocDessinExporterScrapsToKMLorOSM.inc}
// Ce fichier est une dépendance de UnitDocDessin.pas

function TDocumentDessin.ExporterScrapsToKML(const QFileName: string;
                                             const DoUseDefaultStyle: boolean;
                                             const DefaultColor: TColor;
                                             const DefaultOpacity: byte;
                                             const WithExportGIS: TWithExportGIS): boolean;
const
  SCRAP_BY_DEFAULT = 'ScrapDefault';
  FOLDER_Entrances = 'Entrances';
  FOLDER_Scraps    = 'Scraps';
  FOLDER_POI       = 'POIs';
var
  MyKMLExport: TKMLExport;
  NbScraps, ss: Integer;
  MonScrap: TScrap;
  procedure ExportCenterlines();
  begin
    MyKMLExport.BeginFolder(4, 'MyCenterlines');
    MyKMLExport.EndFolder(4, 'MyCenterlines');
  end;
  procedure WriteAScrap(const IdxScrap: integer; const AScrap: TScrap);
  var
    EWE, WU: String;
    v: Integer;
    VX: TVertexPolygon;
    BS: TBaseStation;
    PP: TPoint2Df;
    P1, P2: TProjUV;
  begin
    EWE := IIF (gisWITH_METADATA in WithExportGIS, Format('Scrap%d', [IdxScrap]), 'Metadata unavailable');
    WU  := IIF(DoUseDefaultStyle, SCRAP_BY_DEFAULT, EWE);
    MyKMLExport.BeginPolygon(EWE, WU);
    for v := 0 to High(AScrap.Sommets) do
    begin
      VX := AScrap.Sommets[v];
      if (GetBasePointByIndex(VX.IDStation, BS)) then
      begin
        PP.setFrom(BS.PosStation.X + VX.Offset.X, BS.PosStation.Y + VX.Offset.Y);
        P1.U := PP.X;  P1.V := PP.Y;
        P2 := FConversionSysteme.ConversionSyst1ToSyst2EPSG(FCodeEPSG, CODE_EPSG_WGS84, P1);
        MyKMLExport.AddVertex(P2.U, P2.V, BS.PosStation.Z);
      end;
    end;
    MyKMLExport.EndPolygon();
  end;
  procedure WriteAEntrance(const ASymbole: TSymbole);
  var
    EWE: Boolean;
    P1, P2: TProjUV;
    BP: TBaseStation;
    WU: String;
  begin
    EWE := (nosENTREE          = ASymbole.TypeObject) OR
           (nosGROTTE_SURFACE  = ASymbole.TypeObject) OR
           (nosGOUFFRE_SURFACE = ASymbole.TypeObject);
    if (not EWE) then exit;

    if (GetBasePointByIndex(ASymbole.IDBaseStation, BP)) then
    begin
      if (Trim(ASymbole.TagTexte) = '') then
        {$IFDEF TIDBASEPOINT_AS_TEXT}
        WU := BP.IDStation
        {$ELSE}
        WU := GetToporobotIDStationAsString(BP, false)
        {$ENDIF TIDBASEPOINT_AS_TEXT}
      else
        WU := ASymbole.TagTexte;
      P1.U := BP.PosStation.X;
      P1.V := BP.PosStation.Y;
      P2 := FConversionSysteme.ConversionSyst1ToSyst2EPSG(FCodeEPSG, CODE_EPSG_WGS84, P1);
      MyKMLExport.AddPoint(P2.U, P2.V, BP.PosStation.Z, WU, Format('<BR><BR>X = %.0f - Y = %.0f - Z = %.0f', [BP.PosStation.X, BP.PosStation.Y, BP.PosStation.Z]));
    end;

  end;
   procedure WriteAPOI(const ASymbole: TSymbole);
  var
    EWE: Boolean;
    P1, P2: TProjUV;
    BP: TBaseStation;
    WU: String;
  begin
    if (ASymbole.TypeObject <> nosPOINT_REMARQUABLE ) then exit;

    if (GetBasePointByIndex(ASymbole.IDBaseStation, BP)) then
    begin
      if (Trim(ASymbole.TagTexte) = '') then
        {$IFDEF TIDBASEPOINT_AS_TEXT}
        WU := BP.IDStation
        {$ELSE}
        WU := GetToporobotIDStationAsString(BP, false)
        {$ENDIF TIDBASEPOINT_AS_TEXT}

      else
        WU := ASymbole.TagTexte;
      P1.U := BP.PosStation.X;
      P1.V := BP.PosStation.Y;
      P2 := FConversionSysteme.ConversionSyst1ToSyst2EPSG(FCodeEPSG, CODE_EPSG_WGS84, P1);
      MyKMLExport.AddPoint(P2.U, P2.V, BP.PosStation.Z, WU, Format('<BR><BR>X = %.0f - Y = %.0f - Z = %.0f', [BP.PosStation.X, BP.PosStation.Y, BP.PosStation.Z]));
    end;

  end;
  procedure WriteEntrances();
  var
    ee, NbSymboles: Integer;
  begin
    NbSymboles := Self.GetNbSymboles();
    if (0 = NbSymboles) then Exit;
    for ee := 0 to NbSymboles - 1 do WriteAEntrance(self.GetSymbole(ee));
  end;
  procedure WritePOIs();
  var
    ee, NbSymboles: Integer;
  begin
    NbSymboles := Self.GetNbSymboles();
    if (0 = NbSymboles) then Exit;
    for ee := 0 to NbSymboles - 1 do WriteAPOI(self.GetSymbole(ee));
  end;

begin
  Result := false;
  AfficherMessageErreur(Format('%s.ExporterScrapsToKML: EPSG:%d; %s ', [ClassName, FCodeEPSG, QFileName]));
  NbScraps := self.GetNbScraps();
  if (0 = NbScraps) then Exit;
  MyKMLExport := TKMLExport.Create;
  try
    if (MyKMLExport.Initialiser(QFileName)) then
    begin
      // style de scrap par défaut
      MyKMLExport.DefineStylePoly(SCRAP_BY_DEFAULT, 1.0, DefaultColor, DefaultColor, 255, DefaultOpacity);
      // styles de scraps
      if (not DoUseDefaultStyle) then
      begin
        for ss := 0 to NbScraps - 1 do
        begin
          MonScrap := GetScrap(ss);
          MyKMLExport.DefineStylePoly(Format('Scrap%d', [ss]), 1.0, MonScrap.Couleur, MonScrap.Couleur, 255, MonScrap.Opacite);
        end;
      end;
      // les scraps de la cavité
      MyKMLExport.BeginFolder(4, FOLDER_Scraps);
      for ss := 0 to NbScraps - 1 do
      begin
        MonScrap := GetScrap(ss);
        WriteAScrap(ss, MonScrap);
        if (assigned(FProcAfficherProgression)) then
        FProcAfficherProgression(Format('Scrap %d / %d: %s', [ss, NbScraps, MonScrap.Nom]), 0, NbScraps - 1, ss);
      end;

      MyKMLExport.EndFolder(4, FOLDER_Scraps);
      // les entrées (seulement si avec métadonnées activées)
      if (gisWITH_METADATA in WithExportGIS) then
      begin
        if (gisWITH_ENTRANCES in WithExportGIS) then
        begin
          MyKMLExport.BeginFolder(4, FOLDER_Entrances);
            WriteEntrances();
          MyKMLExport.EndFolder(4, FOLDER_Entrances);
        end;
        // les POI
        if (gisWITH_POI in WithExportGIS) then
        begin
          MyKMLExport.BeginFolder(4, FOLDER_POI);
            WritePOIs();
          MyKMLExport.EndFolder(4, FOLDER_POI);
        end;
      end;
      MyKMLExport.Finaliser();
    end;
    Result := True;
  finally
    FreeAndNil(MyKMLExport);
  end;
end;

function TDocumentDessin.GetDossierContenantDoc(): string;
begin
  Result := FDossierDuDocument;
end;
//------------------------------------------------------------------------------
function TDocumentDessin.GenererCarteLeaflet(const QFileName: string;
                                             const MapWidth, MapHeight: integer;
                                             const DoUseDefaultStyle: boolean;
                                             const DefaultColor: TColor;
                                             const DefaultOpacity: byte;
                                             const WithExportGIS: TWithExportGIS): boolean;
const
  IDX_LAYER_OSM_MAPNIK    = 0;
  IDX_LAYER_OSM_TOPO      = 1;
  IDX_LAYER_GOOGLE_SAT    = 2;
  IDX_LAYER_ENTRANCES     = 3; VAR_LAYER_ENTRANCES     = 'LayerEntrances';
  IDX_LAYER_SCRAPS        = 4; VAR_LAYER_SCRAPS        = 'LayerScraps';
  IDX_LAYER_CENTERLINES   = 5; VAR_LAYER_CENTERLINES   = 'LayerCenterlines';
  IDX_LAYER_POI           = 6; VAR_LAYER_POI           = 'LayerPOIs';
  IDX_LAYER_STATIONS      = 7; VAR_LAYER_PointsTopo    = 'LayerPointsTopo';
var
  MyLeafletExport: TLeafletExport;
  Coin1, Coin2 , Centroide: TPoint3Df;
  QCentroideXY , QCentroideLonLat: TProjUV;
  i, NbScraps, NbSymboles: Integer;
  MyScrap: TScrap;
  MySymbole: TSymbole;
  MyBaseStation: TBaseStation;
  VarCond: String;
  function MiouMiou(const QX, QY: double): TProjUV;
  var
    PT: TProjUV;
  begin
    PT.U := QX;
    PT.V := QY;
    Result := FConversionSysteme.ConversionSyst1ToSyst2EPSG(FCodeEPSG, CODE_EPSG_WGS84, PT);
  end;
  procedure DessinerScrap(const IdxScrap: int64;
                          const QScrap: TScrap;
                          const TagString: string;
                          const TagNum: Int64);
  var
    ii, Nb: Integer;
    V: TVertexPolygon;
    BS: TBaseStation;P2: TProjUV;
    MyGroupe: TGroupeEntites;
    QAT: String;
  begin
    if (gisWITH_METADATA in WithExportGIS) then MyLeafletExport.WrtLinFmt(' // Scrap%d - %s', [IdxScrap, QScrap.Nom]);
    //MyLeafletExport.DefineStylePoly(0.05, MyScrap.Couleur, MyScrap.Couleur, 255, MyScrap.Opacite);
    MyLeafletExport.DefineStylePoly(1.00, DefaultColor, DefaultColor, 255, 255);

    MyGroupe := GetGroupeByIDGroupe(QScrap.IDGroupe);
    QAT := IIF (gisWITH_METADATA in WithExportGIS, MyGroupe.NomGroupe, 'Metadata unavailable');
    MyLeafletExport.BeginPolygon(QAT, '');

    Nb := Length(QScrap.Sommets) - 1;
    for ii := 0 to Nb - 1 do
    begin
      V := QScrap.Sommets[ii];
      if (GetBasePointByIndex(V.IDStation, BS)) then
      begin
        P2 := MiouMiou(BS.PosStation.X + V.Offset.X, BS.PosStation.Y + V.Offset.Y);
        MyLeafletExport.AddVertex(P2.U, P2.V, BS.PosStation.Z, ii < (Nb-1));
      end;
    end;
    MyLeafletExport.EndPolygon();
  end;
  procedure DessinerSymbole(const IdxSymbole: int64; const QSymbole: TSymbole);
  var
    BS: TBaseStation;
    P2: TProjUV;
    EWE: string;
  begin
    if (GetBasePointByIndex(QSymbole.IDBaseStation, BS)) then
    begin
      P2 := MiouMiou(BS.PosStation.X + QSymbole.Offset.X,
                     BS.PosStation.Y + QSymbole.Offset.Y);
    {$IFDEF TIDBASEPOINT_AS_TEXT}
    EWE := BS.IDStation;
    {$ELSE}
    EWE := GetToporobotIDStationAsString(BS);
    {$ENDIF TIDBASEPOINT_AS_TEXT}
      MyLeafletExport.AddPoint(P2.U, P2.V, BS.PosStation.Z, EWE, QSymbole.TagTexte);
    end;
  end;
  procedure DessinerBaseStation(const BS: TBaseStation);
  var
    P2: TProjUV;
    EWE: string;
  begin
    P2 := MiouMiou(BS.PosStation.X, BS.PosStation.Y);
    {$IFDEF TIDBASEPOINT_AS_TEXT}
    EWE := BS.IDStation;
    {$ELSE}
    EWE := GetToporobotIDStationAsString(BS);
    {$ENDIF TIDBASEPOINT_AS_TEXT}
    MyLeafletExport.DrawPoint(P2.U, P2.V, BS.PosStation.Z,
                              0.2,
                              EWE,
                              Format('X = %.0f, Y = %.0f, Z = %.2f', [BS.PosStation.X, BS.PosStation.Y, BS.PosStation.Z])
                              );

  end;
  procedure DessinerViseeCenterline(const BS: TBaseStation);
  var
    P0, P1: TProjUV;
  begin
    P0 := MiouMiou(BS.PosExtr0.X, BS.PosExtr0.Y);
    P1 := MiouMiou(BS.PosStation.X, BS.PosStation.Y);
    MyLeafletExport.DrawSegment(P0.U, P0.V, P1.U, P1.V);
  end;
begin
  result := false;
  AfficherMessageErreur(Format('%s.GenererCarteLeaflet: EPSG:%d; %s', [ClassName, FCodeEPSG, QFileName]));
  NbScraps := self.GetNbScraps();
  if (NbScraps = 0) then Exit;
  // calcul du centroide
  Coin1 := self.GetCoordsMini();
  Coin2 := self.GetCoordsMaxi();
  Centroide.setFrom(0.50 * (Coin1.X + Coin2.X),
                    0.50 * (Coin1.Y + Coin2.Y),
                    0.50 * (Coin1.Z + Coin2.Z));
  QCentroideLonLat := MiouMiou(Centroide.X, Centroide.Y); //FConversionSysteme.ConversionSyst1ToSyst2EPSG(FCodeEPSG, CODE_EPSG_WGS84, QCentroideXY);
  MyLeafletExport := TLeafletExport.Create;
  try
    if (MyLeafletExport.Initialiser(QFilename, 'Export Leaflet', MapWidth, MapHeight, QCentroideLonLat.U, QCentroideLonLat.V)) then
    begin
      MyLeafletExport.AddLayer(VAR_LAYER_ENTRANCES  , 'Entrées');
      MyLeafletExport.AddLayer(VAR_LAYER_SCRAPS     , 'Scraps');
      MyLeafletExport.AddLayer(VAR_LAYER_CENTERLINES, 'Centerlines');
      MyLeafletExport.AddLayer(VAR_LAYER_POI        , 'Points d''intérêt');
      MyLeafletExport.AddLayer(VAR_LAYER_PointsTopo , 'Stations topo');
      MyLeafletExport.WriteHeader();
        // les options WithExportGIS;
        MyLeafletExport.WriteLine(' // Options WithExportGIS: Scraps +');
        if (gisWITH_ENTRANCES   in WithExportGIS) then MyLeafletExport.WriteLine(' // Entrances +');
        if (gisWITH_POI         in WithExportGIS) then MyLeafletExport.WriteLine(' // POIs +');
        if (gisWITH_METADATA    in WithExportGIS) then MyLeafletExport.WriteLine(' // Metadata +');
        if (gisWITH_CENTERLINES in WithExportGIS) then MyLeafletExport.WriteLine(' // Centerlines +');
        MyLeafletExport.WriteLine(' // -----------------------');

        // scraps
        MyLeafletExport.setCurrentLayer(IDX_LAYER_SCRAPS);
        for i := 0 to NbScraps - 1 do
        begin
          MyScrap := GetScrap(i);
          DessinerScrap(i, MyScrap, '', 0);
          if (assigned(FProcAfficherProgression)) then FProcAfficherProgression(Format('Scrap %d / %d: %s', [i, NbScraps, MyScrap.Nom]), 0, NbScraps - 1, i);
        end;
        // Entrées, POIs, Points topo: exportés ssi métadonnées activées
        if (gisWITH_METADATA in WithExportGIS) then
        begin
          // entrées
          MyLeafletExport.setCurrentLayer(IDX_LAYER_ENTRANCES);
          NbSymboles := GetNbSymboles();
          if (NbSymboles > 0) then // and (gisWITH_ENTRANCES in WithExportGIS)) then
          begin
            //VarCond := MyLeafletExport.getLeafletVarNameDispEntrances();
            //MyLeafletExport.BeginConditionalSection(True, VarCond);
              for i := 0 to NbSymboles - 1 do
              begin
                MySymbole := GetSymbole(i);
                if (nosGROTTE_SURFACE = mySymbole.TypeObject) then DessinerSymbole(i, MySymbole);
              end;
            //MyLeafletExport.EndConditionalSection(VarCond);
          end;
          // POIs
          MyLeafletExport.setCurrentLayer(IDX_LAYER_POI);
          NbSymboles := GetNbSymboles();
          if (NbSymboles > 0) then // and (gisWITH_POI in WithExportGIS)) then
          begin
            for i := 0 to NbSymboles - 1 do
            begin
              MySymbole := GetSymbole(i);
              if (nosPOINT_REMARQUABLE = mySymbole.TypeObject) then DessinerSymbole(i, MySymbole);
            end;
          end;
          // Points topo
          MyLeafletExport.setCurrentLayer(IDX_LAYER_STATIONS);
          MyLeafletExport.DefineStylePoly(0.0, clRed, clRed, 255, 255);
          VarCond := MyLeafletExport.getLeafletVarNameDispStations();
          MyLeafletExport.BeginConditionalSection(True, VarCond);
            for i := 0 to GetNbBaseStations() - 1 do
            begin
              MyBaseStation:= GetBaseStation(i);
              {$IFDEF TIDBASEPOINT_AS_TEXT}
              if (MyBaseStation.IDStation <> '') then DessinerBaseStation(MyBaseStation);
              {$ELSE}
              if (MyBaseStation.IDStation > 0)   then DessinerBaseStation(MyBaseStation);
              {$ENDIF TIDBASEPOINT_AS_TEXT}
            end;
          MyLeafletExport.EndConditionalSection(VarCond);

          MyLeafletExport.setCurrentLayer(IDX_LAYER_CENTERLINES);
          MyLeafletExport.DefineStylePoly(0.0, clRed, clRed, 255, 255);
          VarCond := MyLeafletExport.getLeafletVarNameDispCenterlines();
          MyLeafletExport.BeginConditionalSection(True, VarCond);
            for i := 0 to GetNbBaseStations() - 1 do
            begin
              MyBaseStation:= GetBaseStation(i);
              {$IFDEF TIDBASEPOINT_AS_TEXT}
              if (MyBaseStation.IDStation <> '') then DessinerViseeCenterline(MyBaseStation);
              {$ELSE}
              if (MyBaseStation.IDStation > 0)   then DessinerViseeCenterline(MyBaseStation);
              {$ENDIF TIDBASEPOINT_AS_TEXT}
            end;
          MyLeafletExport.EndConditionalSection(VarCond);

        end; // if (gisWITH_METADATA in WithExportGIS) then
        MyLeafletExport.WriteFooter();
    end;
    MyLeafletExport.Finaliser();
    Result := True;
  finally
    FreeAndNil(MyLeafletExport);
  end;
end;

function TDocumentDessin.ExporterScrapsToGeoJSON(const QFileName: string; const DoUseDefaultStyle: boolean; const DefaultColor: TColor; const DefaultOpacity: byte; const WithExportGIS: TWithExportGIS): boolean;
var
  MyExportGeoJSON: TGeoJSONExport;
  NbScraps, i: Integer;
  Coin1, Coin2, Centroide: TPoint3Df;
  QCentroideLonLat: TProjUV;
  MyScrap: TScrap;
  function MiouMiou(const QX, QY: double): TProjUV;
  var
    PT: TProjUV;
  begin
    PT.U := QX;
    PT.V := QY;
    Result := FConversionSysteme.ConversionSyst1ToSyst2EPSG(FCodeEPSG, CODE_EPSG_WGS84, PT);
  end;
  procedure DessinerScrap(const IdxScrap: int64; const QScrap: TScrap; const TagString: string; const TagNum: Int64; const IsLastScrap: boolean);
  var
    ii, Nb: Integer;
    V: TVertexPolygon;
    BS: TBaseStation;P2: TProjUV;
    MyGroupe: TGroupeEntites;
    QAT: String;
  begin
    MyGroupe := GetGroupeByIDGroupe(QScrap.IDGroupe);
    QAT := IIF(gisWITH_METADATA in WithExportGIS, MyGroupe.NomGroupe, 'Metadata unavailable');
    MyExportGeoJSON.BeginPolygon(QAT, '');
    Nb := Length(QScrap.Sommets) - 1;
    for ii := 0 to Nb - 1 do
    begin
      V := QScrap.Sommets[ii];
      if (GetBasePointByIndex(V.IDStation, BS)) then
      begin
        P2 := MiouMiou(BS.PosStation.X + V.Offset.X, BS.PosStation.Y + V.Offset.Y);
        MyExportGeoJSON.AddVertex(P2.U, P2.V, BS.PosStation.Z, ii = (Nb - 1));
      end;
    end;
    MyExportGeoJSON.EndPolygon(IsLastScrap);
  end;
begin
  result := false;
  AfficherMessageErreur(Format('%s.ExporterScrapsToGeoJSON: EPSG:%d; %s', [ClassName, FCodeEPSG, QFileName]));
  NbScraps := self.GetNbScraps();
  if (NbScraps = 0) then Exit;
  // calcul du centroide
  Coin1 := self.GetCoordsMini();
  Coin2 := self.GetCoordsMaxi();
  Centroide.setFrom(0.50 * (Coin1.X + Coin2.X),
                    0.50 * (Coin1.Y + Coin2.Y),
                    0.50 * (Coin1.Z + Coin2.Z));
  QCentroideLonLat := MiouMiou(Centroide.X, Centroide.Y); //FConversionSysteme.ConversionSyst1ToSyst2EPSG(FCodeEPSG, CODE_EPSG_WGS84, QCentroideX
  MyExportGeoJSON := TGeoJSONExport.Create;
  try
    if (MyExportGeoJSON.Initialiser(QFilename, 'Export GeoJSON', QCentroideLonLat.U, QCentroideLonLat.V, DefaultColor, DefaultOpacity)) then
    begin
      MyExportGeoJSON.WriteHeader();
        NbScraps := GetNbScraps();
        for i := 0 to NbScraps - 1 do
        begin
          MyScrap := GetScrap(i);
          DessinerScrap(i, MyScrap, '', 0, (i = (NbScraps - 1)));
          if (assigned(FProcAfficherProgression)) then FProcAfficherProgression(Format('Scrap %d / %d: %s', [i, NbScraps, MyScrap.Nom]), 0, NbScraps - 1, i);
        end;
      MyExportGeoJSON.WriteFooter();
      MyExportGeoJSON.Finaliser();
    end;
  finally
    FreeAndNil(MyExportGeoJSON);
  end;
end;

//******************************************************************************
// extraction des noms de groupes contenus dans une vue
function TDocumentDessin.ExtractNomsGroupes(const LimitesVues: TRect2Df; out LS: TStringList): boolean;
var
  i, Nb: Integer;
  MyGroupe: TGroupeEntites;
  WU: TRect2Df;
begin
  Result := false;
  try
    LS.Clear;
    Nb := self.GetNbGroupes();
    for i := 0 to Nb - 1 do
    begin
      MyGroupe := GetGroupe(i);
      WU.setFrom(MyGroupe.BoundingBox.C1, MyGroupe.BoundingBox.C2);
      if (IntersectRectangles(WU, LimitesVues)) then LS.Add(Format('%d - %s', [MyGroupe.IDGroupeEntites, MyGroupe.NomGroupe]));
    end;
    Result := (LS.Count > 0);
  except

  end;
end;

// calcul de la superficie des vides
function TDocumentDessin.CalcSuperficieVides(const MyGroupe: TGroupeEntites): double;
var
  i, n: Integer;
  QScrap: TScrap;
  QArea, QPerimeter: double;
  MyPolyligne: TPolyLigne;
begin
  Result := 0.00;
  //AfficherMessage(Format('%s.CalcSuperficieVides(%d - %s)', [ClassName, MyGroupe.IDGroupeEntites, MyGroupe.NomGroupe]));
  // superficie des scraps
  n := GetNbScraps();
  if (n = 0) then Exit(0.00);
  for i := 0 to n - 1 do
  begin
    QScrap := GetScrap(i);
    if (QScrap.IDGroupe = MyGroupe.IDGroupeEntites) then
    begin
      CalcAreaPerimetrePolyOrScrap(QScrap.IDGroupe, QScrap.Sommets, QArea, QPerimeter);
      Result := Result + QArea;
    end;
  end;
  // A déduire: superficie des piliers tournés (spécifiés par l'attribut Closed des polylignes)
  n := GetNbPolylignes();
  if (n = 0) then Exit;
  for i := 0 to n - 1 do
  begin
    MyPolyligne := GetPolyligne(i);
    if ((MyPolyligne.IDGroupe = MyGroupe.IDGroupeEntites) and (MyPolyligne.Closed)) then
    begin
      CalcAreaPerimetrePolyOrScrap(MyPolyligne.IDGroupe, MyPolyligne.Sommets, QArea, QPerimeter);
      Result := Result - QArea;
    end;
  end;
end;

procedure TDocumentDessin.TrimPolygonsOfGroupe(const IdxGroupe: TIDGroupeEntites);
var
  NoScrap, NoPoly: Integer;
  MyScrap: TScrap;
  MyPolygone: TPolygone;
begin
  if (self.GetNbScraps() = 0) then Exit;
  if (self.GetNbPolygones() = 0) then Exit;
  for NoScrap := 0 to self.GetNbScraps() - 1 do
  begin
    MyScrap := self.GetScrap(NoScrap);
    if (IdxGroupe = MyScrap.IDGroupe) then
    begin
      for NoPoly := 0 to self.GetNbPolygones() - 1 do
      begin
        MyPolygone := self.GetPolygone(NoPoly);
        if (IdxGroupe = MyPolygone.IDGroupe) then
        begin
          case DelimiterUnPolygoneParUnScrap(self, MyScrap, MyPolygone) of
            errMERGE_SCRAP_AND_POLYGON_OK               : self.PutPolygone(NoPoly, MyPolygone);
            errMERGE_SCRAP_AND_POLYGON_GROUPES_MISMATCH : AfficherMessageErreur('--> Les objets sont dans des groupes différents');
            errMERGE_SCRAP_AND_POLYGON_NO_INTERSECT     : AfficherMessageErreur('--Intersection impossible entre objets disjoints ou imbriqués');
          else
            AfficherMessageErreur('--> Erreur lors de la délimitation de certains polygones');
          end;
        end;
      end;
    end;
  end; // for NoScrap := 0 to self.GetNbScraps() - 1 do
end;
function TDocumentDessin.TrimPolygonByScrap(const IdxScrap, IdxPolygone: integer): boolean;
var
  MyScrap: TScrap;
  MyPolygone: TPolygone;
begin
 if ((self.GetNbScraps() = 0) or (self.GetNbPolygones() = 0)) then Exit;
  // remplir un tronçon de galerie
  // Utilisation: 1: Dessiner un polygone dépassant du scrap
  //              2. Sélectionner un scrap
  //              3. Sélectionner un polygone
  //              4. Fusionner (intersection)
  AfficherMessage(Format('%s.TrimPolygonByScrap: polygone %d par le scrap %d', [ClassName, IdxPolygone, IdxScrap]));

  MyScrap    := self.GetScrap(IdxScrap);          // extraction du scrap
  MyPolygone := self.GetPolygone(IdxPolygone);    // extraction du polygone

  case DelimiterUnPolygoneParUnScrap(self, MyScrap, MyPolygone) of
    errMERGE_SCRAP_AND_POLYGON_OK               : self.PutPolygone(IdxPolygone, MyPolygone);
    errMERGE_SCRAP_AND_POLYGON_GROUPES_MISMATCH : AfficherMessageErreur('-- Les objets sont dans des goupes différents');
    errMERGE_SCRAP_AND_POLYGON_NO_INTERSECT     : AfficherMessageErreur('-- Intersection impossible entre objets disjoints ou imbriqués');
  else
    AfficherMessageErreur('Erreur lors de la délimitation');
  end;
end;

procedure TDocumentDessin.ExporterUnGroupeEnGCD(const IDGroupe: TIDGroupeEntites; const QFilename: TStringDirectoryFileName);
begin
  AfficherMessage(Format('%s.ExporterUnGroupeEnGCD(%d) -> %s', [ClassName, IDGroupe, QFilename]));
  self.SaveToFile(QFilename, IDGroupe, True);
end;

// réorganise les parois d'un groupe de manière à les rendre compatibles avec Therion
function TDocumentDessin.ReorienterCourbesOfGroupe(const Groupe: TGroupeEntites): boolean;
var
  Nb: Integer;
begin
  result := false;
  AfficherMessage(Format('%s.ReorienterCourbesOfGroupe(%d - %s)', [ClassName, Groupe.IDGroupeEntites, Groupe.NomGroupe]));
  Nb := GetNbCourbes();
  if (0 = Nb) then Exit;
end;

function TDocumentDessin.ReorienterPolylignesOfGroupe(const Groupe: TGroupeEntites): boolean;
var
  Nb: Integer;
begin
  result := false;
  AfficherMessage(Format('%s.ReorienterPolylignesOfGroupe(%d - %s)', [ClassName, Groupe.IDGroupeEntites, Groupe.NomGroupe]));
  Nb := GetNbPolylignes();
  if (0 = Nb) then Exit;
end;

//******************************************************************************





end.


