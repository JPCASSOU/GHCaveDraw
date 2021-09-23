unit UnitMessages_fr;
{$codepage UTF8}

interface
const ListeStylesCourbes: array[0 .. 11] of string = (
                                   'Défaut', // nocDEFAULT                      //rsBTN_COURBE_NONE               = 'Défaut';
                                   'Paroi', // nocPAROI                         //rsBTN_COURBE_PAROI              = 'Paroi';
                                   'Paroi cachée', // nocPAROIS_CACHEE          //rsBTN_COURBE_PAROI_CACHEE       = 'Paroi cachée';
                                   'Ecoulement', // nocECOULEMENT               //rsBTN_COURBE_ECOULEMENT         = 'Ecoulement';
                                   'Ligne de pente', // nocLIGNES_PENTE         //rsBTN_COURBE_LIGNES_PENTE       = 'Ligne de pente';
                                   'Ressaut', // nocRESSAUT                     //rsBTN_COURBE_RESSAUT            = 'Ressaut';
                                   'Surplomb', // nocSURPLOMB                     rsBTN_COURBE_SURPLOMB           = 'Surplomb';
                                   'Chenal de voûte', // nocCHENAL_VOUTE          rsBTN_COURBE_CHENAL             = 'Chenal de voûte';
                                   'Rupture de pente', // nocMARCHE               rsBTN_COURBE_MINI_RESSAUT       = 'Rupture de pente';
                                   'Paroi incertaine', // nocPAROI_INCERTAINE      rsBTN_COURBE_PAROI_INCERTAINE   = 'Paroi incertaine';
                                   'Mur maçonné',    // nocMUR_MACONNE
                                   'Paroi éclatée' // nocPAROI_FRACASSEE
                                  );

const ListeStylesLignes: array [0 .. 5] of string = (
                                  'Défaut',  // nolDEFAULT                             rsBTN_LIGNE_NONE
                                  'Flèches', // nolFLECHE                              rsBTN_LIGNE_FLECHE
                                  'Fractures',  // nolFRACTURE                         rsBTN_LIGNE_FRACTURES
                                  'Lignes de pentes',  // nolPENTE                      rsBTN_LIGNE_PENTE
                                  'Suites de réseaux', // nolSUITE_RESEAU              rsBTN_LIGNE_SUITE
                                  'Pendages'
                                  );


const ListeStylesPolygones: array [0 .. 13] of string = (
                                   'Défaut',         //  nopDEFAULT
                                   'Lac',            //  nopLAC                 rsBTN_POLYGONE_LAC
                                   'Argile',         //  nopARGILE              rsBTN_POLYGONE_ARGILE
                                   'Sable',          //  nopSABLE               rsBTN_POLYGONE_SABLE
                                   'Blocs',          //  nopBLOCS               rsBTN_POLYGONE_BLOCS
                                   'Galets',         //  nopGALETS              rsBTN_POLYGONE_GALETS
                                   'Neige',          //  nopNEIGE               rsBTN_POLYGONE_NEIGE
                                   'Silhouettes',    //  nopSILHOUETTE          rsBTN_POLYGONE_SILHOUETTES
                                   'Gros bloc',      //  nopGROS_BLOC           rsBTN_POLYGONE_GROS_BLOC
                                   'Gour',           //  nopGOUR                rsBTN_POLYGONE_GOUR
                                   'Siphon',         //  nopSIPHON              rsBTN_POLYGONE_SIPHON
                                   'Argile-galets',  //  nopVARVES              rsBTN_POLYGONE_VARVES
                                   'Parcours',       //  nopCHEMINS             rsBTN_POLYGONE_CHEMINS
                                   'Masque'
                                   );

const ListeStylesTextes: array [0 .. 7] of string = (
                                  'Débogage (non imprimé)',    // notDEBUG              rsBTN_TEXTE_DEBUG
                                  'Titres',                    // notTITRES             rsBTN_TEXTE_TITRES
                                  'Sous-titres',               // notSOUS_TITRES        rsBTN_TEXTE_SOUS_TITRES
                                  'Cotation',                  // notCOTATION           rsBTN_TEXTE_COTATION
                                  'Texte ordinaire 1',         // notTEXTE1             rsBTN_TEXTE_TEXTE_ORDINAIRE_1
                                  'Texte ordinaire 2',         // notTEXTE2             rsBTN_TEXTE_TEXTE_ORDINAIRE_2
                                  'Lieu-dit',                  // notLIEU_DIT           rsBTN_TEXTE_LIEU_DIT
                                  'Cotation extérieure'
                                  );
const ListeNatureSymboles: array [0 .. 21] of string = (
                                    'Photo',                        // nosPHOTO                          nosPHOTO
                                    'Entrée',                       // nosENTREE                         nosENTREE
                                    'Point topo',                   // nosPOINT_TOPO                     nosPOINT_TOPO
                                    'Point fixe',                   // nosPOINT_FIXE                     nosPOINT_FIXE
                                    'Correspondance entre groupes', // nosCORRESPONDANCE                 nosCORRESPONDANCE
                                    'Fistuleuses',                  // nosFISTULEUSE                     nosFISTULEUSE
                                    'Concrétions de paroi',         // nosCONCRETION_PAROI               nosCONCRETION_PAROI
                                    'Excentriques',                 // nosEXCENTRIQUES                   nosEXCENTRIQUES
                                    'Stalactites',                  // nosSTALACTITES                    nosSTALACTITES
                                    'Colonnes',                     // nosCOLONNES                       nosCOLONNES
                                    'Stalagmites',                  // nosSTALAGMITES                    nosSTALAGMITES
                                    'Cristaux',                     // nosCRISTAUX                       nosCRISTAUX
                                    'Fractures (deprecated)',       // nosFRACTURES; nosFRACTURESles fractures sont gérées via l'objet TListeSimple
                                    'Cupules',                      // nosCUPULES                        nosCUPULES
                                    'Courant d''air',               // nosZEFF                           nosZEFF
                                    'Arrivée d''eau',               // nosARRIVEE_EAU                    nosARRIVEE_EAU
                                    'Perte interne',                // nosPERTE                          nosPERTE
                                    'Désobstruction',               // nosDESOB                          nosDESOB
                                    'Zone dangereuse',              // nosDANGER
                                    'Aven de surface',              // nosGOUFFRE_SURFACE,
                                    'Grotte isolée',                // nosGROTTE_SURFACE,
                                    'Point remarquable'             // nosPOINT_REMARQUABLE
                                    );
const ListeTypesBarbules: array[0 .. 4] of string = (
                                   'Aucune',
                                   'Ressaut ou puits',
                                   'Surplomb',
                                   'Chenal de voûte',
                                   'Petit ressaut'
                                  );
const
  ListeTypesQuadrillages: array[0 .. 1]
                          of string = ('Grille', 'Croix');

resourcestring
  rsCODE_ISO_COUNTRY  = 'fr';
  rsGHCAVEDRAWEXENAME = 'GHCaveDraw';
  // version
  rsGHCD_VERSION  = 'Version %.5f du %.2d/%.2d/%.4d %.2d:%.2d';
  rsGHCD_AUTHOR   = 'Jean Pierre CASSOU';
  rsGHCD_LICENSE  = 'Logiciel sous licence GPL';
  rsTYPE_INTERFACE_TABLETTE = 'Version tablette';
  rsTYPE_INTERFACE_DESKTOP  = 'Version Desktop';
  // mots clefs du MétaFiltre
  rsHLPMETAFILTRE = 'METAFILTRE';
  rsMETAFILTRE_NIL = 'RIEN';          // 0
  rsMETAFILTRE_ALL = 'TOUT';          // 1
  rsMETAFILTRE_ID = 'ID';            // 2
  rsMETAFILTRE_LENGTH = 'LONGUEUR';      // 3
  rsMETAFILTRE_AZIMUTH = 'GISEMENT';      // 4
  rsMETAFILTRE_PENTE = 'PENTE';         // 5
  rsMETAFILTRE_DATE = 'DATE';          // 6
  rsMETAFILTRE_COLOR = 'COULEUR';       // 7
  rsMETAFILTRE_X = 'COORD_X';       // 8
  rsMETAFILTRE_Y = 'COORD_Y';       // 9
  rsMETAFILTRE_Z = 'COORD_Z';       // 10
  rsMETAFILTRE_LARGEUR = 'LARGEUR';       // 11
  rsMETAFILTRE_HAUTEUR = 'HAUTEUR';       // 12
  rsMETAFILTRE_DATES = 'DATES';         // 13
  rsMETAFILTRE_COLORS = 'COULEURS';      // 14
  rsMETAFILTRE_SERIE = 'SERIE';         // 15
  rsMETAFILTRE_RESEAU = 'RESEAU';        // 16
  rsMETAFILTRE_SECTEUR = 'SECTEUR';        // 17
  rsMETAFILTRE_CODE = 'CODE';          // 18
  rsMETAFILTRE_EXPE = 'SEANCE';        // 19
  rsMETAFILTRE_TYPEVISEE = 'TYPE_VISEE';  // 20
  rsMETAFILTRE_STATION   = 'STATION';
  // resourcestrings de l'utilitaire de fusion de polygones
  //  GetResourceString()
  rsMSG_MERGE_CONFIRM_CORRECT_RESULT        = 'Le résultat est-il correct';
  rsMSG_MERGE_CONFIRM_REMOVE_BUILD_COURBES  = 'Supprimer les courbes de construction';
  rsMSG_MERGE_POLY_SAME_OBJ                 = 'Les objets sont identiques';
  rsMSG_MERGE_POLY_TYPES_MISMATCH_QUESTION  = 'Les polygones sont de nature différente - Continuer';
  rsMSG_MERGE_POLY_ACTION_OK                = 'Fusion des objets OK';
  rsMSG_MERGE_POLY_GROUPES_MISMATCH         = 'Les objets sont dans deux groupes distincts';
  rsMSG_MERGE_POLY_OBJ_DISJOINTS            = 'Les polygones sont disjoints';
  rsMSG_MERGE_POLY_ANY_ERROR                = 'Echec de la fusion des objets';
  rsMSG_MERGE_CONCAT_COURBES_OK             = 'Concaténation des courbes OK';
  rsMSG_MERGE_CONCAT_POLYLIGNES_OK          = 'Concaténation des polylignes OK';
  rsMSG_MERGE_CONCAT_COURBES_FAIL           = 'Erreur en concaténation des courbes';
  rsMSG_MERGE_CONCAT_POLYLIGNES_FAIL        = 'Erreur en concaténation des polylignes';


  // resourcestrings du menu principal
  rsMNU_CENTERLINES_IN_EXTERNAL_FILE = 'Polygonales dans fichier séparé';
  rsMNU_FILE    = '&Fichier';
    rsMNU_NEW_DRAWING   = 'Nouveau dessin';
    rsMNU_OPEN          = '&Ouvrir';
    rsMNU_SAVE_AS       = '&Enregistrer sous ...';
    rsMNU_SAVE_A_COPY   = 'Enregistrer une copie ...';

    rsMNU_QUICK_SAVE    = 'Sauvegarde horodatée';
    rsMNU_CLOSE_DOC     = 'Fermer le document';
    rsMNU_PRINT         = 'Im&primer';
    rsMNU_SVG           = 'Exporter en SVG';
    rsMNU_SVG_SCRAPS_ONLY = 'Exporter en SVG (scraps uniquement)';
    rsMNU_ODG           = 'Exporter en LibreOffice Draw (odg)';
    rsMNU_PDF           = 'Générer un atlas PDF';
    rsMNU_POLYGO_GHTOPO = 'Charger canevas GHTopo';
    rsMNU_EXPORT_GCP    = 'Exporter le canevas topo';
    rsMNU_EXPORT_SCRAPS_KML = 'Exporter les scraps vers Google Earth';
    rsMNU_EXPORT_SCRAPS_OSM = 'Exporter les scraps vers OpenStreetMap';
    rsMNU_EXPORT_IMAGE  = 'Exporter en image';
      rsMNU_EXPORT_IMG_ALL      = 'Tout le réseau';
      rsMNU_EXPORT_IMG_CURR_VUE = 'La vue courante';
    rsMNU_EXPORT_LEAFLET = 'Générer une carte dynamique OSM Leaflet';
    rsMNU_VUE_IMAGE     = 'Exporter vue courante en image';
    rsMNU_GENERER_ATLAS = 'Générer un atlas HTML';
    rsMNU_EXPORT_VERS_SIG = 'Export vers SIG';

    rsMNU_QUIT          = '&Quitter';

  rsMNU_DESSIN = '&Dessin';
  rsMNU_EDITION = '&Edition';
    rsMNU_EDIT_OBJETS   = 'Editer objets ...';
      rsMNU_EDIT_SCRAP       = 'Scrap';
      rsMNU_EDIT_COURBE      = 'Courbe';
      rsMNU_EDIT_POLYLIGNE   = 'Polyligne';
      rsMNU_EDIT_POLYGONE    = 'Polygone';
      rsMNU_EDIT_SIMPLE_LINE = 'Ligne simple';
      rsMNU_EDIT_SYMBOLE     = 'Symbole';
      rsMNU_EDIT_TEXTE       = 'Texte';
    rsMNU_DELETE_ALL_OBJ_CURR_GROUPE     = 'Effacer les objets du groupe courant';
    rsMSG_CONFIRM_DELETE_ALL_OBJ_GROUPE  = 'Ceci effacera TOUS les objets du groupe %d (%s) - Continuer ?';
    rsMNU_REATTRIB_BASEPOINTS_GROUPE     = 'Redéfinir points de base pour le groupe courant';
    rsMNU_SIMPLIFY_COURBES_GROUPE        = 'Simplifier les courbes du groupe courant';

    rsMNU_STYLES_OBJETS = 'Styles d''objets';
    rsMNU_RECHERCHER    = 'Rechercher une station';
    rsMNU_CALC_ALL_BB   = 'Recalculer les BoundingBox';
    rsMNU_GENERER_SCRAP_DEPUIS_PAROIS_GROUPE = 'Générer un scrap pour le groupe courant';
    rsMNU_RESTORE_LAST_OBJECTS = 'Restaurer le dernier objet';
    rsMNU_DELETE_LAST_OBJECTS = 'Supprimer le dernier objet';
      rsMNU_DELETE_SCRAP    = 'Scrap';
      rsMNU_DELETE_COURBE   = 'Courbe';
      rsMNU_DELETE_POLYLINE = 'Polyligne';
      rsMNU_DELETE_SLINE    = 'Ligne';
      rsMNU_DELETE_POLYGON  = 'Polygone';
      rsMNU_DELETE_SYMBOLE  = 'Symbole';
      rsMNU_DELETE_TEXTE    = 'Texte';
      rsMNU_DELETE_IMAGE    = 'Image';
    rsMNU_GESTION_SUPERGROUPES = 'Gestion des super-groupes';
  rsMNU_AFFICHAGE = '&Affichage';
    rsMNU_REDESS        = 'Redessin';
    rsMNU_ZOOMWND       = 'Zoom fenêtre';
    rsMNU_ZOOMALL       = 'Zoom tout';
    rsMNU_PANVUE        = 'Pan vue';
    rsMNU_ZOOM_PLUS     = 'Zoom avant';
    rsMNU_ZOOM_MOINS    = 'Zoom arrière';
    rsMNU_PAN_LEFT      = 'Pan gauche';
    rsMNU_PAN_RIGHT     = 'Pan droite';
    rsMNU_PAN_UP        = 'Pan haut';
    rsMNU_PAN_DOWN      = 'Pan bas';
    rsMNU_PARAM_VUE_2D  = 'Préférences de la vue';
    rsMNU_DISP_CONSOLE  = 'Afficher la console';


  rsMNU_AIDE            = '&Aide';
    rsMNU_APROPOS       = 'A propos de GHCaveDraw';
  // divers fenêtre principale
  rsCHK_DRAW_TEXTE_DEBUG = 'Afficher aides-mémoire';
  rsCHK_DRAW_ID_STATIONS = 'Afficher IDStations';

  rsBTN_HIDE_PNL_IMAGE    = 'Masquer';

  rsBTN_EDIT_IMAGE        = 'Editer';
  rsBTN_IMG_MOVE_UP       = 'Monter';
  rsBTN_IMG_MOVE_DOWN     = 'Descendre';

  rsBTN_DELETE_IMAGE      = 'Supprimer';


  rsDO_DELETE_IMAGE       = 'Supprimer l''image %d';

  rsBTN_MESURE_DISTANCE   = 'Mesurer une distance et une aire';
  // resourcestring de la console
  rsDLG_CONSOLE_TITLE                 = 'Console de suivi';
  rsDLG_CONSOLE_LB_JOURNAL            = 'Journal';
  rsDLG_CONSOLE_LB_CONSOLE_ERREUR     = 'Console de résultats et d''erreurs';
  rsDLG_CONSOLE_BTN_COPY              = 'Copier';
  rsDLG_CONSOLE_BTN_VOID              = 'Vider';

  // resourcestring des boutons de dessin
  rsBTN_DRW_HINT_SETMODE_0        = 'Mode Vue';
  rsBTN_DRW_HINT_NEW_OBJ          = 'Nouvel objet';
  rsBTN_DRW_HINT_EDIT_OBJ         = 'Edition objet';
  rsBTN_DRW_HINT_DELETE_OBJ       = 'Effacer un objet';
  // création d'objets
  rsBTN_DRW_HINT_NEW_SCRAP        = 'Scrap';
  rsBTN_DRW_HINT_NEW_COURBE       = 'Courbe';
  rsBTN_DRW_HINT_NEW_POLYLIGNE    = 'Polyligne';
  rsBTN_DRW_HINT_NEW_LIGNE        = 'Ligne';
  rsBTN_DRW_HINT_NEW_POLYGON      = 'Polygone';
  rsBTN_DRW_HINT_NEW_SYMBOLE      = 'Symbole';
  rsBTN_DRW_HINT_NEW_TEXTE        = 'Texte';
  rsBTN_DRW_HINT_NEW_IMAGE_FOND   = 'Image';
  // boutons de types d'objets
  // -- courbes
  rsBTN_COURBE_NONE               = 'Défaut';
  rsBTN_COURBE_PAROI              = 'Paroi';
  rsBTN_COURBE_PAROI_CACHEE       = 'Paroi cachée';
  rsBTN_COURBE_ECOULEMENT         = 'Ecoulement';
  rsBTN_COURBE_LIGNES_PENTE       = 'Ligne de pente';
  rsBTN_COURBE_RESSAUT            = 'Ressaut';
  rsBTN_COURBE_SURPLOMB           = 'Surplomb';
  rsBTN_COURBE_CHENAL             = 'Chenal de voûte';
  rsBTN_COURBE_MINI_RESSAUT       = 'Rupture de pente';
  rsBTN_COURBE_PAROI_INCERTAINE   = 'Paroi incertaine';
  rsBTN_COURBE_MUR_MACONNE        = 'Mur maçonné';
  rsBTN_COURBE_PAROI_FRACASSEE    = 'Paroi éclatée';

  // -- lignes
  rsBTN_LIGNE_NONE                = 'Défaut';
  rsBTN_LIGNE_FLECHE              = 'Flèche';
  rsBTN_LIGNE_PENTE               = 'Ligne de pente';
  rsBTN_LIGNE_FRACTURES           = 'Fracture';
  rsBTN_LIGNE_SUITE               = 'Suite de réseau';


  // -- polygones
  rsBTN_POLYGONE_NONE             = 'Défaut';
  rsBTN_POLYGONE_LAC              = 'Lac';
  rsBTN_POLYGONE_ARGILE           = 'Argile';
  rsBTN_POLYGONE_SABLE            = 'Sable';
  rsBTN_POLYGONE_BLOCS            = 'Blocs';
  rsBTN_POLYGONE_GALETS           = 'Galets';
  rsBTN_POLYGONE_NEIGE            = 'Névé';
  rsBTN_POLYGONE_SILHOUETTES      = 'Silhouettes de galeries';
  rsBTN_POLYGONE_GROS_BLOC        = 'Gros bloc';
  rsBTN_POLYGONE_GOUR             = 'Gour';
  rsBTN_POLYGONE_SIPHON           = 'Siphon';
  rsBTN_POLYGONE_VARVES           = 'Argile et galets';
  rsBTN_POLYGONE_CHEMINS          = 'Chemins';
  rsBTN_POLYGONE_MASQUES          = 'Masques';





  // -- symboles
  rsBTN_SYMBOLE_NONE              = 'Défaut';

  rsBTN_SYMBOLE_PHOTO             = 'Photo';
  rsBTN_SYMBOLE_ENTREE            = 'Entrée';
  rsBTN_SYMBOLE_POINT_TOPO        = 'Point topo';
  rsBTN_SYMBOLE_POINT_FIXE        = 'Point fixe';

  rsBTN_SYMBOLE_CORRESPONDANCE    = 'Correspondance entre galeries';
  rsBTN_SYMBOLE_FISTULEUSES       = 'Fistuleuses';
  rsBTN_SYMBOLE_CONCRETIONS_PAROI = 'Concrétions de paroi';
  rsBTN_SYMBOLE_EXCENTRIQUES      = 'Excentriques';

  rsBTN_SYMBOLE_STALACTITES       = 'Stalactites';
  rsBTN_SYMBOLE_COLONNES          = 'Colonnes';
  rsBTN_SYMBOLE_STALAGMITES       = 'Stalagmites';
  rsBTN_SYMBOLE_CRISTAUX          = 'Cristaux';

  rsBTN_SYMBOLE_FRACTURE          = 'Fracture (deprecated)';
  rsBTN_SYMBOLE_CUPULE            = 'Cupule';
  rsBTN_SYMBOLE_ZEFF              = 'Courant d''air';
  rsBTN_SYMBOLE_ARRIVEE_EAU       = 'Arrivée d''eau';
  rsBTN_SYMBOLE_PERTE             = 'Perte interne';
  rsBTN_SYMBOLE_DESOB             = 'Désobstruction';
  rsBTN_SYMBOLE_DANGER            = 'Danger';
  rsBTN_SYMBOLE_RANDOM_GROS_BLOC  = 'Bloc rocheux';

  rsBTN_SYMBOLE_SURFACE_GROTTE    = 'Entrée de grotte';
  rsBTN_SYMBOLE_SURFACE_GOUFFRE   = 'Gouffre en surface';
  rsBTN_SYMBOLE_PT_REMARQUABLE    = 'Point remarquable';


  // textes
  rsBTN_TEXTE_DEBUG               = 'Débogage (non imprimé)';
  rsBTN_TEXTE_TITRES              = 'Titres';
  rsBTN_TEXTE_SOUS_TITRES         = 'Sous-titres';
  rsBTN_TEXTE_COTATION            = 'Cotation';
  rsBTN_TEXTE_TEXTE_ORDINAIRE_1   = 'Texte ordinaire 1';
  rsBTN_TEXTE_TEXTE_ORDINAIRE_2   = 'Texte ordinaire 2';
  rsBTN_TEXTE_LIEU_DIT            = 'Lieu-dit';
  rsBTN_TEXTE_COTATION_EXT        = 'Cote en extérieur';


  // combinaison objets
  rsBTN_COMBINE_OBJ_MERGE_SCRAPS       = 'Combiner'; //Fusionner les scraps indiqués';
  rsBTN_COMBINE_OBJ_MERGE_LAST_SCRAPS  = 'Fusionner les deux derniers scraps créés';
  // affichage d'éléments du dessin
  rsCHK_DISP_CENTERLINE           = 'Polygonales';
  rsCHK_DISP_ID_STATIONS          = 'ID stations';
  rsCHK_DISP_ECHELLE              = 'Echelle et Nord';
  rsCHK_DISP_QUADRILLES           = 'Quadrillage';
  rsCHK_DISP_TEXTES               = 'Textes';
  rsCHK_DISP_PHOTOS               = 'Photos';
  rsCHK_DISP_TEXTE_DEBUG          = 'Aides-mémoire';
  rsCHK_DISP_SCRAPS               = 'Scraps';
  rsCHK_DISP_BOUNDING_BOX         = 'Limites de groupes';
  rsCHK_DISP_PENTES_SUP_A         = 'Pentes > ';
  // resourcestring du dialogue Groupes
  rsDLG_GRP_CAPTION_CREATION      = 'Nouveau groupe';
  rsDLG_GRP_CAPTION_MODIF         = 'Modification de groupe';
  rsDLG_GRP_LB_NOM_GROUPE         = 'Nom';
  rsDLG_GRP_LB_DECALAGE           = 'Décalage';
  rsDLG_GRP_CHK_DECALAGE_ACTIF    = 'Actif';
  rsDLG_GRP_CHK_VISIBLE           = 'Visible';
  rsDLG_GRP_CHK_LOCKED            = 'Verrouillé';
  rsDLG_GRP_ALTITUDE              = 'Altitude';
  rsDLG_GRP_FILTRES               = 'Filtres';
  rsDLG_GRP_DO_SET_CURR_GRP       = 'Définir comme groupe courant';
  rsDLG_GRP_DATE_LAST_MODIF       = 'Modifié le';


  // messages
  rsMSG_QUIT                     = 'Quitter GHCaveDraw';
  rsMSG_QUIT_VIEWER              = 'Quitter GHCaveDraw Viewer';
  rsMSG_ERR_PRINTING_UNAVAILABLE = 'Fonctionnalité d''impression disponible uniquement sous Windows';

  rsMSG_ERR_CTXT_SVG  = 'Erreur de démarrage du contexte SVG';
  rsMSG_ERR_EXPORT_KML  = 'Erreur en export vers KML';
  rsMSG_ERR_EXPORT_OSM  = 'Erreur en export vers OSM';

  //rsMSG_DELETE_OBJECT = 'Effacer l''objet %d (%s)';
  rsMSG_DELETE_SCRAP     = 'Effacer le scrap %d';
  rsMSG_DELETE_COURBE    = 'Effacer la courbe %d';
  rsMSG_DELETE_POLYLIGNE = 'Effacer la polyligne %d';
  rsMSG_DELETE_POLYGONE  = 'Effacer le polygone %d';
  rsMSG_DELETE_LINE      = 'Effacer la ligne %d';
  rsMSG_DELETE_SYMBOLE   = 'Effacer le symbole %d';
  rsMSG_DELETE_TEXTE     = 'Effacer le texte %d';

  // resourcestring du dialogue Textes
  rsDLG_TXT_TITRE = 'Edition de texte';
  rsDLG_TXT_GROUPES = 'Groupe';
  rsDLG_TXT_STYLE = 'Style';
  rsDLG_TXT_TEXTE = 'Texte';
  rsDLG_TXT_IDBASESTATION = 'Station topo';
  rsDLG_TXT_OFFSET = 'Décalage';
  rsDLG_TXT_ALIGNEMENT = 'Accrochage du texte';
  rsDLG_TXT_HINT       = 'Codes spéciaux: %i = ID station; %s = Nom station;' + #13#10 +
                         '%l = Longueur; %a = Azimut; %p = pente' + #13#10 +
                         '%z = Altitude';
  // resourcestring du dialogue Symboles
  rsDLG_SYM_TITRE = 'Edition de symboles';
  rsDLG_SYM_GROUPES                = 'Groupe';
  rsDLG_SYM_TYPEOBJET              = 'Type d''objet';
  rsDLG_SYM_TAGTEXTE               = 'Texte';
  rsDLG_SYM_PHOTODISPLAYED         = 'Photo affichée';
  rsDLG_SYM_IDBASESTATION          = 'Station topo';
  rsDLG_SYM_OFFSET                 = 'Décalage';
  rsDLG_SYM_DIMENSIONS             = 'Dimensions (m)';
  rsDLG_SYM_ANGLEROT               = 'Rotation (°)';
  // cadre de dessin
  rsCDR_DRW_ACN_VUE = 'Vue';
    rsCDR_DRW_ACN_VUE_REDESS       = 'Redessiner';
    rsCDR_DRW_ACN_VUE_ZOOM_ALL     = 'Zoom tout';
    rsCDR_DRW_ACN_VUE_ZOOM_WND     = 'Zoom fenêtre';
    rsCDR_DRW_ACN_VUE_ZOOM_PLUS    = 'Zoom avant';
    rsCDR_DRW_ACN_VUE_ZOOM_MOINS   = 'Zoom arrière';
    rsCDR_DRW_ACN_VUE_ZOOM_PAN     = 'Pan vue';
    rsCDR_DRW_ACN_VUE_PAN_LEFT     = 'Pan vers gauche';
    rsCDR_DRW_ACN_VUE_PAN_RIGHT    = 'Pan vers droite';
    rsCDR_DRW_ACN_VUE_PAN_UP       = 'Pan vers le haut';
    rsCDR_DRW_ACN_VUE_PAN_DOWN     = 'Pan vers les bas';
  rsCDR_DRW_ACN_DELETE = 'Suppression d''objets';
    rsCDR_DRW_ACN_VUE_DEL_SCRAP    = 'Scrap %d';
    rsCDR_DRW_ACN_VUE_DEL_COURBE   = 'Courbe %d';
    rsCDR_DRW_ACN_VUE_DEL_POLYLINE = 'Polyligne %d';
    rsCDR_DRW_ACN_VUE_DEL_LIGNE    = 'Ligne %d';
    rsCDR_DRW_ACN_VUE_DEL_POLYGONE = 'Polygone %d';
    rsCDR_DRW_ACN_VUE_DEL_SYMBOLE  = 'Symbole %d';

    rsCDR_DRW_ACN_VUE_DEL_TEXTE    = 'Texte %d';
  rsCDR_DRW_ACN_SET_GROUPE_IDX     = 'Affecter à l''objet';
  rsCDR_DRW_ACN_SET_NATURE_OBJET   = 'Affecter à l''objet';
  rsCDR_DRW_ACN_EDIT_OBJET         = 'Editer objet';
  rsCDR_DRW_ACN_REVERSE_COURBE     = 'Stripper la courbe';

  rsCDR_DRW_MNU_FILTRES            = 'Filtres';
    rsCDR_DRW_MNU_FILTRES_SERIE    = 'Série courante (%d)';
    rsCDR_DRW_MNU_FILTRES_COULEUR  = 'Couleur courante ($%.6X)';
    rsCDR_DRW_MNU_FILTRES_STATION  = 'Station courante (%.d.%.d: "%s")';

  rsCDR_DRW_BTN_METAFILTRE         = 'Appliquer';
  rsCDR_DRW_ACN_LOCK_CURR_STATION  = 'Verrouiller la station %d.%d (%s)';
  rsCDR_DRW_ACN_DISP_GHCD_CODE_OBJ = 'Afficher le code GHCaveDraw de l''objet';
  rsCDR_DRW_ACN_ADD_TEXT_W_INFO_ST = 'Ajouter un texte avec infos de la station';
  rsCDR_DRW_ACN_SQUARE_POLYLINE    = 'Equarrir polyligne';
  rsCDR_DRW_ACN_SQUARE_POLYGON     = 'Equarrir polygone';



  // resourcesstrings de l'éditeur de styles
  rsDLG_STYLES_TITRE                 = 'Feuilles de styles';
  rsDLG_STYLES_LB_SVG_STYLE_OBJET    = 'Nom du style';
  rsDLG_STYLES_LB_DESC_STYLE_OBJET   = 'Description';
  rsDLG_STYLES_LB_DESC_SEUIL_VISIBLE = 'Seuil de visibilité';
  rsDLG_STYLES_LB_DESC_SEUIL_HINT    = 'Echelle sous laquelle l''objet n''est plus visible';

  rsDLG_STYLES_TYPES_STYLESHEET_STD    = 'Cavité standard';
  rsDLG_STYLES_TYPES_STYLESHEET_01     = 'Grand réseau';
  rsDLG_STYLES_TYPES_STYLESHEET_02     = 'Petite cavité';




  rsDLG_STYLES_LB_LINEWIDTH_OBJET    = 'Largeur écran';
  rsDLG_STYLES_LB_PRNLINEWIDTH_OBJET = 'Largeur imprimante';
  rsDLG_STYLES_LB_COULEUR_OBJET      = 'Couleur';

  rsDLG_STYLES_LB_BARBULES_TYPE      = 'Barbules';
  rsDLG_STYLES_LB_BARBULES_TAILLE    = 'Largeur de barbule';
  rsDLG_STYLES_LB_TEXTE_ATTRS        = 'Attributs de texte';
  rsDLG_STYLES_LB_TEXTE_PRN_HAUTEUR  = 'Hauteur imprimante';

  rsDLG_STYLES_TAB_STYLE_COURBES   = 'Courbes';
  rsDLG_STYLES_TAB_STYLE_LIGNES    = 'Lignes';
  rsDLG_STYLES_TAB_STYLE_POLYGONES = 'Polygones';
  rsDLG_STYLES_TAB_STYLE_SYMBOLES  = 'Symboles';
  rsDLG_STYLES_TAB_STYLE_TEXTES    = 'Textes';

  // resourcestrings du centre d'impression
  rsDLG_PRINT_TITRE                = 'Centre d''impression';
  rsDLG_PRINT_GRBX_PRINTERS        = 'Imprimantes';
    rsDLG_PRINT_LB_PARAMETRES_PRN  = 'Configurer';
  rsDLG_PRINT_GRBX_ECHELLE         = 'Echelle et règle';
    rsDLG_PRINT_LB_SCALING         = 'Echelle';
    rsDLG_PRINT_LB_REGLE           = 'Dim. règle';
  rsDLG_PRINT_GRBX_QUADRILLAGE     = 'Quadrillage';
    rsDLG_PRINT_LB_TYPE_QDR        = 'Type';
    rsDLG_PRINT_LB_SPACING         = 'Espacement';
    rsDLG_PRINT_LB_TAILLE_CROIX    = 'Taille croix';
  rsDLG_PRINT_GRBX_ZONEDESSIN      = 'Zone d''impression';
    rsDLG_PRINT_DEFINIR_ZONE       = 'Définir zone';
  rsDLG_PRINT_GRBX_ELEMENTS_DRAWN  = 'Elements représentés';
    rsDLG_PRINT_BTN_STYLES_OBJETS  = 'Styles';
  // resourcestrings du configurateur d'atlas
  rsDLG_ATLAS_LB_DOSSIER_DEST      = 'Dossier de destination';
   rsDLG_ATLAS_CHK_DISP_MAIN_GRID  = 'Principale';
   rsDLG_ATLAS_CHK_DISP_SEC_GRID   = 'Secondaire';
   rsDLG_ATLAS_LB_SPACING_GRDS     = 'Esp. (m)';
   rsDLG_ATLAS_CHK_DISP_COPYRIGHT  = 'Copyright';
   rsDLG_ATLAS_CHK_DISP_LISTE_POI  = 'Liste des points d''intérêt';
   rsDLG_ATLAS_GRBX_OPTIONS_TRACE  = 'Options de dessin';
   rsDLG_ATLAS_USUALLY_RESOLS      = 'Résolutions usuelles';
   rsDLG_ATLAS_GRBX_TAILLE_ECRAN   = 'Taille de l''écran cible';
   rsDLG_ATLAS_GRBX_OPTIONS_ATLAS  = 'Options de l''atlas';
   rsDLG_ATLAS_LB_TAILLE_IMAGE     = 'Dimensions de l''image topo';

  // resourcestrings du système d'aide
  rsHLP_TITRE                      = 'A propos de GHCaveDraw';
  rsHLP_TAB_ABOUT                  = 'A propos';
  rsHLP_TAB_STATS                  = 'Statistiques du dessin';
  // resourcestring du cadre liste de groupes
   // resourcestring de la liste des groupes
  //rsLST_GRP_NEW           = 'Nouveau';
  //rsLST_GRP_GRP_ACTIF     = 'Activer';
  //rsLST_GRP_BASC_VISI     = 'Basculer visibilité';
  //rsLST_GRP_MASQUER       = 'Masquer tous';

  rsCDR_LST_GRP_NOUVEAU_GROUPE       = 'Nouveau groupe';
  rsCDR_LST_GRP_BASC_VISI_GROUPES    = 'Basculer la visibilité';
  rsCDR_LST_GRP_DEF_GROUPE_COURANT   = 'Définir comme groupe courant';
  rsCDR_LST_GRP_VISIBILITE_GROUPE    = 'Afficher / Masquer le groupe courant';

  rsCDR_LST_GRP_MASQUER_TOUS_GROUPES = 'Masquer tous les groupes';
  rsCDR_LST_GRP_DECALAGE_GROUPE      = 'Activer/désactiver le décalage du groupe courant';
  rsCDR_LST_GRP_LOCK_GROUPE          = 'Verrouiller / déverrouiller le groupe courant';
  rsCDR_LST_GRP_LOCALISER_GROUPE     = 'Localiser le groupe courant';



  rsCDR_LST_GRP_SELECT_GRP_VISIBLE   = 'Rendre visibles les groupes sélectionnés';
  rsCDR_LST_GRP_DELETE_OBJ_GROUPE    = 'Supprimer tous les objets du groupe courant';
  rsCDR_LST_GRP_DELETE_GROUPE_VIDE   = 'Supprimer ce groupe';

  // resourcestring de l'éditeur de sections transversales
  rsEDITOR_SECTIONS_MAIN_TITLE       = 'Editeur de sections transversales [Station]';
  rsEDITOR_SECTIONS_MNU_NOUVEAU      = 'Nouvelle section';
  rsEDITOR_SECTIONS_MNU_OPEN         = 'Ouvrir une section';
  rsEDITOR_SECTIONS_MNU_SAVE         = 'Enregistrer la section';
  rsEDITOR_SECTIONS_MNU_SERIALISE    = 'Afficher le code GHCaveDraw';
  rsEDITOR_SECTIONS_MNU_PARSE        = 'Construire à partir du code GHCaveDraw';
  rsEDITOR_SECTIONS_MNU_HELP         = 'Aide';
  rsEDITOR_SECTIONS_MNU_QUIT         = 'Quitter l''éditeur de sections transversales';
  rsEDITOR_SECTIONS_MSG_BEFORE_RESET_SECTION = 'Les modifications seront perdues' + #13#10 +
                                               'Continuer';



  // messages divers
  rsMISC_NOUVEAU_GROUPE            = 'Nouveau groupe';
  rsMISC_REPLACE_POLYGO_MSG_TITRE  = 'Remplacement de polygonale';
  rsMISC_SAUVEGARDE_CONSEILLEE     = 'Une sauvegarde du dessin est conseillée - Continuer';
  rsMISC_DLG_FIND_STATION_TITRE    = 'Recherche de station';
  rsMISC_DLG_FIND_STATION_PROMPT   = 'Nom de station ou couple série et station (séparateur: point)';
  rsMISC_DONE_WITH_SUCCESS         = 'Opération terminée';
  rsMISC_DRAWING_MUST_BE_CLOSED    = 'Le dessin courant doit être fermé avant d''effectuer cette action';
  rsADVICE_FOR_SCRAPS_FROM_COURBES = 'Les courbes de parois doivent avoir été saisies dans le bon ordre' +#13#10 +
                                     '(courbes tracées en aller paroi de droite puis retour paroi de gauche)' + #13#10+
                                     'Continuer';
  rsADVICE_GD_NB_VERTEX_BASEPT     = 'La station %d est verrouillée et rattache un grand nombre de vertex' + #13#10 +
                                     'Ceci est rarement une utilisation normale';
  rsMISC_ASSIGN_FILTRE_TO_GROUPE       = 'Assigne le filtre au groupe courant';
  rsMISC_RETRIEVE_FILTRE_FROM_GROUPE   = 'Récupère le filtre du groupe courant';
  rsMISC_ERASE_FILTRE_FROM_GROUPE      = 'Efface le filtre';

  rsMISC_BTN_DO_EXPORT_SIG             = 'Exporter';
  rsMISC_CHK_USE_DEFAULT_STYLE         = 'Utiliser la couleur par défaut';
  // messages d'erreur
  rsERR_MSG_LOAD_GCD_FAIL          = 'Echec en ouverture du dessin';
  rsERR_MSG_GROUPES_MISMATCH       = 'Les objets sont dans deux groupes distincts';
  rsERR_MSG_CREATION_SCRAP_FAILED  = 'Echec de la génération du scrap';
  rsERR_MSG_SAME_OBJECTS           = 'Les objets sont identiques';
  rsERR_MSG_IMPORT_CENTERLINE      = 'Erreur dans l''importation de la polygonale';
  rsERR_MSG_FILE_NOT_FOUND         = 'Fichier introuvable';

  // message PRET
  rsMSG_READY                      = 'Prêt';
   // resourcestring du centre d''impression
  rsPRN_NOPRINTER                            = 'Pas d''imprimante installée';
  rsPRN_TBPRINTER                            = 'Imprimante';
  rsPRN_TBDRAW                               = 'Dessin';
  rsPRN_TITLE                                = 'Centre d''impression [%s]';
  rsPRN_CHKPOLY                              = 'Polygonales';
  rsPRN_CHKFILL                              = 'Remplissage';
  rsPRN_CHKWALLS                             = 'Parois';
  rsPRN_CHKSECTS                             = 'Sections';
  rsPRN_CHKSTATIONS                          = 'Stations';
  rsPRN_CHKSTATIONS_LBL                      = 'No stations';
  rsPRN_CHKALTITUDE                          = 'Altitude';
  rsPRN_CHKCOTE                              = 'Cotes';
  rsPRN_CHKQUADRILLAGE                       = 'Quadrillage';
  rsPRN_CHKENTREES                           = 'Entrees';
  rsPRN_CHKANNOTATIONS                       = 'Annotations';
  rsPRM_LBANGLEROT                           = 'Angle de rotation';
  rsPRN_TYPEQDR                              = 'Type de quadrillage';
  rsPRN_QDNONE                               = 'Aucun';
  rsPRN_QDCROSS                              = 'Croix';
  rsPRN_QDQUADRILLES                         = 'Grille';
  rsPRN_QDPOINTS                             = 'Points';
  rsPRN_SCALING                              = 'Echelle';
  rsPRN_LBSPACING                            = 'Espacement';
  rsLANDSCAPE                                = 'Paysage';
  rsPORTRAIT                                 = 'Portrait';
  rsPRN_START_PRINTING                       = 'Démarrage de l''impression';
  rsPRN_PARAMS_VUE                           = 'Paramétrer la vue';

  rsPRN_PRINTING_DONE                        = 'Impression terminée';
  rsDLGIMP_TAB1                              = 'Aperçu';
  rsDLGIMP_TAB2                              = 'Imprimante';
  rsDLGIMP_TAB3                              = 'Options de dessin';
  rsDLGIMP_TAB4                              = 'Réseaux';
  rsGRBX_QUADRILLAGE                         = 'Quadrillage';
  rsQDR_MAIN                                 = 'Principal';
  rsQDR_SECONDARY                            = 'Secondaire';
  rsQDR_TYPE                                 = 'Type';

  rsQDR_SPACING                              = 'Espacement';
  rsQDR_COLOR                                = 'Couleur';
  rsECHELLE                                  = 'Echelle: 1 /';
  rsLAYERS                                   = 'Couches de dessin';
  rsPREVIEW                                  = 'Previsualisation';
  rsSTARTPRINTING                            = 'Lancer impression';
  rsREGLE                                    = 'Règle';
  rsIDSTATION_NOT_FOUND                      = 'Station "%s" introuvable';


  rsAC_DUMMY                                 = '---';
  rsACN_ADD_CURR_CURVE_AT_SCRAPLISTE         = 'Ajouter courbe courante';
  rsACN_ADD_CURR_POLYLIN_AT_SCRAPLISTE       = 'Ajouter polyligne courante';
  rsACN_GENERER_SCRAP_FROM_COURBE            = 'Depuis courbes';
  rsACN_GENERER_SCRAP_FROM_POLYLINE          = 'Depuis polylignes';
implementation

end.

