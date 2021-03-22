unit UnitMessages_he;
  // Fichier UnitMessages_fr.pas cree le 09/05/2014 a 19:28:30
  // Genere par les macros du classeur: file:///C:/0_Logiciels_JPC/0_GHTopoFPC_20140503/Outils_Calc/GestionResourceStrings.ods
  //********************************************************************************
  // /!\ DO NOT MODIFY - Generated automatically
  // Please use spreadsheet utility for modification of strings
  // Language: FR - Français
  // ================================================================================
  // NOUVEAUTE 2016: Ce fichier doit être encodé en UTF8 sans BOM
interface
uses
  StructuresDonnees; // pour le code instrument lasermetres

// CONST section
const
  NATIONAL_GEODESIC_SYSTEM_IDX = 1;
// RESOURCESTRING section
resourcestring
  // nom du logiciel
  rsGHTOPOEXENAME   = 'GHTopo';
  rsMAINMENUCAPTION = '%s - GHTopo';
  rsGHTOPOVERSION   = 'Version 3.14159265 du %.2d/%.2d/%.4d';
  rsGHTOPOLICENSE   = 'Logiciel sous licence GPL';
  rsGHTOPOAUTHOR    = '(c) 1989..%d Jean Pierre CASSOU';
  // infos sur la session
  rsGHTOPORESOL         = 'Résolution %d x %d';
  rsGHTOPOPlatFormLinux = 'Plateforme: Linux (Français)';
  rsGHTOPOPlatFormWin32 = 'Plateforme: Microsoft Windows (Français)';
  // Langue choisie
  rsCHOOSELANGUAGE      = 'Lancement en langue francaise';
  // fin GHTopo
  rsEND_OF_GHTOPO       = 'Fin GHTopo';
  // type d'interface
  rsTYPE_INTERFACE_TABLETTE = 'Version TabletPC';
  rsTYPE_INTERFACE_DESKTOP  = 'Version PC de bureau';
  // unités
  rsDESC_DEVICE_TLM330          = 'TLM 330';
  rsDESC_UNITE_ANGULAIRE_DEGRES = 'Degrés';
  rsDESC_UNITE_ANGULAIRE_GRADES = 'Grades';
  rsDESC_UNITE_PENTES_PERCENT   = 'Poucentage';
  rsDESC_UNITE_PENTE_DENIVELE   = 'Dénivelé';
  rsDESC_VISEE_DIRECTE          = 'Directe';
  rsDESC_VISEE_INVERSE          = 'Inverse';
  rsDESC_POS_ZERO_HZ            = 'Horizontal';
  rsDESC_POS_ZERO_ZENITHAL      = 'Zénithal';
  rsDESC_POS_ZERO_NADIRAL       = 'Nadiral';




  // *********************************************
  // resourcestring des libellés communs
  rsLBL_COMMENTS = 'Commentaires';
  rsSERIE_INITIALISATION = 'Serie 0 non modifiable';
  rsMSG_SERIE_INITIALISATION = 'La série 0 n''est pas modifiable';

  // Filtres de fichiers
  rsFILEFILTER_ALL = 'Tous (*.*)|*.*';
  rsGHTOPO_FILE_FILTER_WO_TEXT = 'Fichiers GHTopo (*.xtb)|*.xtb|' +
                         'Fichiers GHTopo XML (*.gtx)|*.gtx|' +
                         'Fichiers Toporobot Tab [deprecated] (*.Tab)|*.Tab|' +
                         'Tous (*.*)|*.*';
  rsGHTOPO_FILE_FILTER_W_TEXT = 'Fichiers GHTopo (*.xtb)|*.xtb|' +
                         'Fichiers GHTopo XML (*.gtx)|*.gtx|' +
                         'Fichiers Toporobot Tab [deprecated] (*.Tab)|*.Tab|' +
                         'Fichiers Toporobot Text [deprecated] (*.Text)|*.Text|' +
                         'Fichiers texte PocketTopo (*.txt)|*.txt|' +
                         'Tous (*.*)|*.*';
  rsKML_FILE_FILTER = 'Fichier Google Earth (*.kml)|*.xtb|Tous (*.*)|*.*';
  rsGCD_FILE_FILTER = 'Polygonale GHCaveDraw (*.gcd)|*.gcd|Tous (*.*)|*.*';
  rsGPX_FILE_FILTER = 'Fichier GPS (*.gpx)|*.gpx|Tous (*.*)|*.*';
  rsOSM_FILE_FILTER = 'Fichier travail OpenStreetMap (*.osm)|*.osm|Tous (*.*)|*.*';
  rsCSV_FILE_FILTER = 'Texte tabulé (*.csv)|*.csv|Tous (*.*)|*.*';
  rsVTOPO_FILE_FILTER = 'Document Visual Topo (*.tro)|*.tro|Tous (*.*)|*.*';
  rsTHERION_FILE_FILTER = 'Centerlines Therion (*.th)|*.th|Tous (*.*)|*.*';
  // *********************************************
  // resourcestring du menu principal
  rsMSGASSIGNCAPTIONS = 'Mise en place de l''interface';
  rsCALCURAPIDE = 'Calculette:';
  rsMNU_FILE = '&Fichier';
    rsNEW         = '&Nouveau';
    rsOPEN        = '&Ouvrir';
    rsSAVE        = '&Sauvegarder';
    rsSAVEAS      = '&Enregistrer sous';
    rsMNU_SAVELOG = '&Enregistrer historique';
    rsCLOSE       = '&Fermer le document';
    rsRECENT_DOCS = '&Documents récents';
    rsRELOAD      = 'Recharger le document';
    rsEDITTAB     = 'Editer fichier &Tab';
    rsPRINT       = 'Im&primer';
    rsVTOPO       = 'Export Visual Topo';
    rsEXPORT_THERION     = 'Export vers Therion';
    rsEXPORT_TOPOROBOT   = 'Export vers Toporobot Text (sans visées rayonnantes)';
    rsEXPORT_POCKETTOPO  = 'Export vers PocketTopo (avec visées rayonnantes)';
    rsEXPORT_TOPODROID   = 'Export vers Topodroid';
    rsEXPORT_COMPASS_PLT = 'Export vers Compass PLT';
    rsEXPORT_ABRIS       = 'Export vers Android ABRIS';
    rsGHCAVEDRAW         = 'Polygonale pour GHCaveDraw';
    rsEXPORT_SERIES_TAB_CROISE = 'Export des séries pour tableaux croisés';
    rsEXPORT_GIS         = 'Export vers logiciels de cartographie';
    rsEXPORTGRAPHIQUE    = 'E&xport graphique (PS, DXF et SVG)';
    rsERRINFOXTB         = '&Rapport d''erreur de lecture';
    rsGHTOPO_QUIT        = '&Quitter GHTopo';
    rsQUIT_EDITOR        = '&Quitter léditeur de texte';
    rsGENERER_DOSSIER_THERION = 'Générer un dossier complet Therion';
    rsFUSIONNER_TOPOS    = 'Fusionner des documents GHTopo (expérimental)';

  rsMNU_EDITION     = '&Edition';
  rsMNU_TOPOGRAPHIE = '&Topographie';
    rsCHECKBASE       = '&Vérifier les données';
    rsCOMPILE         = '&Calculer le réseau';
    rsVUEPLAN         = 'Vue en &Plan';
    rsVUE3D           = '&Vue 3D';
    rsRENDU3D         = 'Rendu &3D';
    rsSTATISTIQUES    = '&Statistiques';
    rsINFOCAVITE      = '&Info cavité';
    rsMAILLAGES_UTILS = 'Maillages de surface';
    rsNODESCOORDINATES = 'Coordonnées des &Noeuds';
  rsMNU_WINDOW     = '&Fenêtre';
  rsMNU_TOOLS      = '&Outils';
  rsMNU_HELP = '&Aide';
    rsHLPINDEX       = '&Index';
    rsHLPNEWS        = 'Nouveautés';
    rsABOUT          = '&A propos de GHTopo';
  rsMNU_STAY_ON_TOP = 'Mettre devant';
  // titres des fenêtres
  rsWND_DATABASE = 'Séries';
  rsWND_LISTES   = '&Listes (codes, séances, ...)';
  rsWND_VISEES_RADIANTES = 'Visées &rayonnantes';

  rsWND_CONSOLE  = '&Console';
  rsWND_DISTO_X  = 'Visées du &DistoX';
  rsWND_PLAN     = '&Vue en plan';

  // resourcestring de l''Assistant
  rsASSISTANT_SUCCESS = 'Document créé avec succès par l''assistant';
  rsASSISTANT_RELOAD = 'Rechargement du document';
  rsASSISTANT_ECHEC = 'Echec de l''assistant';
  rsASSIST_TITLE = 'Assistant Nouvelle Cavité';
  rsASSIST_BNGENERAL = 'Général';
  rsASSIST_BNENTRANCES = '1ère Entrée';
  rsASSIST_BNCODES = '1er Code';
  rsASSIST_BNEXPES = '1ère Séance';
  rsASSIST_BNSERIE = '1ère Série';
  rsASSIST_BNSAVE = 'Sauvegarder ...';
  rsASSIST_BNCANCEL = 'Annuler';
  rsASSIST_LBNOMETUDE = 'Nom de l''étude';
  rsASSIST_LBOBSETUDE = 'Commentaires étude';
  rsASSIST_LBNOMENTREE = 'Entrée principale';
  rsASSIST_LBCOORDS = 'Coordonnées';
  rsASSIST_LBOBSENTREE = 'Observations';
  rsASSIST_LBCONVERTISSEUR = 'Convertisseur ...';
  rsASSIST_LBREFSTATION = 'Station initiale';
  rsASSIST_LBREFSERIE = 'Série';
  rsASSIST_LBREFPOINT = 'Point';
  rsASSIST_LBCOMMENTAIRE = 'Commentaires';
  rsASSIST_LBSYSTGEO = 'Systèmes de coordonnées géographiques';
  // ********************************
  // resourcestring du visualisateur en plan
  rsVUE2D_TITLE = 'Vue en plan';
  rsVUE2D_DISPLAY_MASK            = 'Afficher / Masquer ';
  rsVUE2D_DISPLAY_PARAMETRES_VUE  = 'Paramètres d''affichage de la vue';
  rsVUE2D_DISPLAY_LISTE_SERIES    = 'Liste des séries';
  rsVUE2D_DISPLAY_DIAGRAMMES      = 'Diagrammes';
  rsVUE2D_REPRESENTATION_MODE     = 'Représentation';
  rsVUE2D_REPRESENTATION_NETWORKS = 'Réseaux';
  rsVUE2D_REPRESENTATION_SECTEURS = 'Secteurs';
  rsVUE2D_REPRESENTATION_EXPES    = 'Séances';
  rsVUE2D_REPRESENTATION_GRAY     = 'Gris';
  rsVUE2D_REPRESENTATION_DEPTH    = 'Profondeurs';
  rsVUE2D_REPRESENTATION_TYPES    = 'Nature visée';

  rsVUE2D_REPRESENTATION_STATIONS = 'Points topo';
  rsVUE2D_LOAD_MAILLAGE           = 'Charger un modèle numérique de terrain';
  rsVUE2D_DISTANCE                = 'Distance entre deux stations';
  rsVUE2D_FINDSTATION             = 'Rechercher une station';
  //
  //rsVUE2D_DEPTH_DIAGRAM = 'Histogramme altimétrique';

  rsVUE2D_METAFILTRE              = 'Filtres';
  rsVUE2D_PANVUE                  = 'Deplacer vue';

  rsVUE2D_PRINTING                = 'Imprimer le plan';
  rsVUE2D_REFRESH                 = 'Redessiner la vue';
  rsVUE2D_EXPORT_DXF              = 'Export au format DXF';
  rsVUE2D_STATISTIQUES            = 'Statistiques';
  rsVUE2D_VUE3D_GDI               = 'Vue 3D (sans OpenGL)';
  rsVUE2D_VUE3D_OPENGL            = 'Rendu 3D OpenGL';
  rsVUE2D_ZOOMALL                 = 'Zoom sur tout le réseau';
  rsVUE2D_ZOOMFENETRE             = 'Zoom sur fenêtre';

  rsVUE2D_FMT_INFOS_STATION_RAPIDE = 'Station: %s (%d.%d): X=%.2f Y=%.2f Z=%.2f';
  rsVUE2D_FMT_INFOS_ID_STATION     = '%d.%d [%s]';
  rsVUE2D_FMT_INFOS_MESURES        = 'L = %.2f, Az = %.2f, P =%.2f, G = %.2f, D = %.2f, H = %.2f, B = %.2f';
  rsVUE2D_FMT_INFOS_COORDONNEES    = 'X=%s, Y=%s, Z=%s; x=%s, y=%s, z=%s';
  rsVUE2D_EXPORT_XHTML             = 'Exporter au format XHTML';
  rsVUE2D_EXPORT_SVG               = 'Exporter au format SVG';
  // *******************************************************
  // resourcestring des onglets du Gestionnaire de Cavité
  rsTBS_GENERAL                    = 'Général';
  rsTBS_LISTES_SIMPLES             = 'Listes';
  rsTBS_DISTO_X                    = 'DistoX';
  rsTABLE                          = 'Liste:';
  rsBTN_FIND                       = 'Rechercher';
  rsTBS_GENERAL_NOM                = 'Nom du réseau';
  rsTBS_GENERAL_OBS                = 'Commentaires';
  rsTBS_GENERAL_SYST               = 'Système de coordonnées';
  rsTBS_TOUT                       = 'Tout';
  rsTBS_ENTRANCE                   = 'Entrées';
  rsTBS_SECTEURS                   = 'Secteurs';
  rsTBS_CODES                      = 'Codes';
  rsTBS_TRIPS                      = 'Séances';
  rsTBS_SERIES                     = 'Séries';
  rsTBS_RESEAUX                    = 'Réseaux';
  rsTBS_ANTENNES                   = 'Visées en antenne';
  rsTBS_ANNOTATIONS                = 'Annotations';
  rsTBS_SERIES_FLAT_TABLE          = 'Séries sous forme de tableau';
  rsTBS_SERIES_VUE_EN_PLAN         = 'Vue en plan';
  rsTBS_MAINTENANCE                = 'Maintenance';
  // *******************************************************
  // resourcestring de la section General
  rsLB_NOM_ETUDE                   = 'Nom de l''étude';
  rsLB_COMMENTAIRE_ETUDE           = 'Commentaires';
  rsLB_CODE_EPSG                   = 'Code EPSG du système de coordonnées';
  rsBTN_SELECT_EPSG                = 'Choisir ...';
  rsBTN_CALC_DECLIMAGS             = 'Calculer les déclinaisons magnétiques';
  // *******************************************************
  // resourcestring du cadre Entrées
  rsCDR_ENTR_NOENTRANCE            = 'Entrée';
  rsCDR_ENTR_ENTRNAME              = 'Nom de l''entrée';
  rsCDR_ENTR_ENTRCODE              = 'Code de l''entrée';

  rsCDR_ENTR_COORDINATES           = 'Coordonnées';
  rsCDR_ENTR_STATOFENTR            = 'Station de référence';
  rsCDR_ENTR_CALLCALCULETTE        = 'Convertisseur';


  // *******************************************************
  // resourcestrings du cadre Réseaux
  rsCDR_RESEAU_LBIDX               = 'Numero';
  rsCDR_RESEAU_NAME                = 'Nom';
  rsCDR_RESEAU_TYPE                = 'Type de réseau';
  rsCDR_RESEAU_CB0                 = 'Cavité naturelle';
  rsCDR_RESEAU_CB1                 = 'Cavité artificielle';
  rsCDR_RESEAU_CB2                 = 'Topo de surface';
  rsCDR_RESEAU_CB3                 = 'Thalweg';
  rsCDR_RESEAU_CB4                 = 'Route ou piste';
  rsCDR_RESEAU_CB5                 = 'Sentier';
  rsCDR_RESEAU_CB6                 = 'Autre';
  // *******************************************************
  // resourcestrings du cadre Secteurs
  rsCDR_SECTEUR_LBIDX              = 'Numero';
  rsCDR_SECTEUR_NAME               = 'Nom';
  rsCDR_SECTEUR_COLOUR             = 'Couleur';
  // *******************************************************
  // resourcestrings du cadre Expés
  rsCDR_EXPE_SEANCE                = 'Séance topo';
  rsCDR_EXPE_DATE                  = 'Date';
  rsCDR_EXPE_DECLIMAG              = 'Déclinaison';
  rsCDR_EXPE_INCLIN                = 'Inclinaison';
  rsCOLOR                          = 'Couleur';
  rsCDR_EXPE_SPELEOMETRE           = 'Opérateurs';
  rsCDR_EXPE_SPELEOGRAPHE          = 'Club';
  // *******************************************************
  // resourcestring du cadre Codes
  rsCDR_CODES_NUMERO               = 'Code';
  rsCDR_CODES_TYPEGALERIE          = 'Type de visée';
  // types de galeries
  rsTYPE_VISEE_DEFAULT             = 'Défaut';
  rsTYPE_VISEE_ENTRANCE            = 'Entrée';
  rsTYPE_VISEE_FOSSILE             = 'Fossile';
  rsTYPE_VISEE_VADOSE              = 'Ecoulement libre';
  rsTYPE_VISEE_ENNOYABLE           = 'Ennoyable';
  rsTYPE_VISEE_SIPHON              = 'Siphon';
  rsTYPE_VISEE_FIXPOINT            = 'Point fixe';
  rsTYPE_VISEE_SURFACE             = 'Visée de liaison (raccord)';
  rsTYPE_VISEE_TUNNEL              = 'Tunnel artificiel';
  rsTYPE_VISEE_MINE                = 'Filon minier';
  rsTYPE_VISEE_ANTENNE             = 'Visée rayonnante';
  rsTITRE_SELECTEUR_VISEE          = 'Choix du type de visée';
    rsCMBTYPE_D = '0 - Défaut (conduit fossile)';
    rsCMBTYPE_E = '1 - Entrée';
    rsCMBTYPE_B = '2 - Galerie fossile';
    rsCMBTYPE_V = '3 - Ecoulement libre';
    rsCMBTYPE_W = '4 - Siphon';
    rsCMBTYPE_C = '5 - Passage ennoyable';
    rsCMBTYPE_F = '6 - Point fixe';
    rsCMBTYPE_S = '7 - Topo de surface';
    rsCMBTYPE_A = '8 - Tunnel artificiel';
    rsCMBTYPE_M = '9 - Filon minier';
  rsCDR_CODES_VISEE_HZ       = 'Mesure Horizontale';
  rsCDR_CODES_VISEE_VT       = 'Verticale';
  rsCDR_CODES_VDIRECT        = 'Directe';
  rsCDR_CODES_VINVERSE       = 'Inverse';
  rsCDR_CODES_GRADCOMPAS     = 'Graduation du compas';
  rsCDR_CODES_GRADCLINO      = 'Graduation du clinomètre';
  rsCDR_CODES_CMBUNIT_0      = '400 - Grades';
  rsCDR_CODES_CMBUNIT_1      = '360 - Degrés';
  rsCDR_CODES_CMBUNIT_2      = '370 - Pourcentages';
  rsCDR_CODES_CMBUNIT_3      = '380 - Dénivellations';
  rsCDR_CODES_CMBUNIT_4      = '800 - Lasermètre/clinomètre de bâtiment';
  rsCDR_CODES_FACT           = 'Longueurs x';
  rsCDR_CODES_POSZERO        = 'Position du zéro:';
  rsCDR_CODES_CMBZERO_0      = 'Nadiral';
  rsCDR_CODES_CMBZERO_1      = 'Horizontal';
  rsCDR_CODES_CMBZERO_2      = 'Zénithal';
  rsCDR_CODES_ANGLIMIT       = 'Angle limite:';
  rsCDR_CODES_PRECISION      = 'Précision instruments';
  // *********************************************
  // resourcestring du cadre Séries
  rsDLG_PJMNGR_MOVETOSERIE      = 'Changer de série ?';
  rsDLG_PJMNGR_ADDSERIE         = 'Ajouter Série';
  rsCDR_SERIE_CHOOSE_SECTEUR    = 'Choisir un secteur';
  rsCDR_SERIE_SHOW_HIDE_HEADER  = 'Affiche / masque l''entête';
  rsCDR_SERIE_CHOOSE_TYPE_VISEE = 'Choisir un type de visée';
  rsCDR_SERIE_CHOOSE_CODE       = 'Choisir un code instruments';
  rsCDR_SERIE_CHOOSE_EXPE       = 'Choisir une séance topo';
  rsCDR_SERIE_LB_RESEAU         = 'Réseau';
  rsCDR_SERIE_ADDPHOTO          = 'Ajouter photo';
  rsCDR_SERIE_NUMERO            = 'Série';
  rsCDR_SERIE_NAME              = 'Nom';
  rsCDR_SERIE_DEPART            = 'Départ';
  rsCDR_SERIE_ARRIVEE           = 'Arrivée';
  rsCDR_SERIE_CHANCE            = 'Chance';
    rsCDR_SERIE_CHANCE0   = 'Aucune';
    rsCDR_SERIE_CHANCE1   = 'Faible';
    rsCDR_SERIE_CHANCE2   = 'Bonne';
    rsCDR_SERIE_CHANCE3   = 'Excellente';
    rsCDR_SERIE_OBSTACLE  = 'Obstacle';
    rsCDR_SERIE_OBSTACLE0 = 'Aucun';
    rsCDR_SERIE_OBSTACLE1 = 'Puits';
    rsCDR_SERIE_OBSTACLE2 = 'Cheminée';
    rsCDR_SERIE_OBSTACLE3 = 'Etroiture';
    rsCDR_SERIE_OBSTACLE4 = 'Lac';
    rsCDR_SERIE_OBSTACLE5 = 'Siphon';
    rsCDR_SERIE_OBSTACLE6 = 'Effondrement';
    rsCDR_SERIE_OBSTACLE7 = 'Concrétionnement';
    rsCDR_SERIE_OBSTACLE8 = 'Sédiments';
    rsCDR_SERIE_OBSTACLE9 = 'Autre';
    // spécifique à la France lol
    rsCDR_SERIE_OBSTACLE10 = 'Gaz toxiques';
    rsCDR_SERIE_OBSTACLE11 = 'Oies agressives';
    rsCDR_SERIE_OBSTACLE12 = 'Animaux dangereux';
    rsCDR_SERIE_OBSTACLE13 = 'Baisodrome';
    rsCDR_SERIE_COL_POINT      = 'Station';
    rsCDR_SERIE_COL_TYPE       = 'Type';
    rsCDR_SERIE_COL_ID_TERRAIN = 'Etiquette';
    rsCDR_SERIE_COL_SECTEUR    = 'Secteur';
    rsCDR_SERIE_COL_CODE       = 'Code';
    rsCDR_SERIE_COL_EXPE       = 'Session';
    rsCDR_SERIE_COL_LEGNTH     = 'Longueur';
    rsCDR_SERIE_COL_AZIMUTH    = 'Azimut';
    rsCDR_SERIE_COL_INCLIN     = 'Pente';
    rsCDR_SERIE_COL_LG         = 'Gauche';
    rsCDR_SERIE_COL_LD         = 'Droite';
    rsCDR_SERIE_COL_HZ         = 'Haut';
    rsCDR_SERIE_COL_HN         = 'Bas';
    rsCDR_SERIE_COL_COMMENTAIRE = 'Commentaire';

  rsCDR_SERIE_LOCKED            = 'Verrouillé';
  rsCDR_SERIE_NBLINES           = 'Nombre de lignes';

  rsCDR_SERIE_IMPLEMENT         = 'Valider';
  rsCDR_SERIE_ENTREE_RATT       = 'Entrée';
  rsINPUT_COMMENTAIRE_TITRE     = 'Commentaires station';
  rsINPUT_COMMENTAIRE_MSG       = 'Entrez un texte';

  rsCDR_SERIE_AC_GRD_SEL_COPY   = 'Copier la sélection';
  rsCDR_SERIE_AC_GRD_COPY       = 'Copier le tableau';
  rsCDR_SERIE_AC_GRD_PASTE      = 'Coller';

  rsCDR_SERIE_AC_ADD_LINE       = 'Insérer des lignes';
  rsCDR_SERIE_AC_DEL_LINE       = 'Supprimer des lignes';
  rsCDR_SERIE_AC_UNDOCOPY       = 'Recopier vers le bas';
  rsCDR_SERIE_AC_INC_UNDOCOPY   = 'Recopie vers le bas incrémentale';


  rsCDR_SERIE_AC_SELECT_RESEAU      = 'Sélectionner un réseau';
  rsCDR_SERIE_AC_SELECT_SECTEUR     = 'Sélectionner un secteur';
  rsCDR_SERIE_AC_SELECT_TYPE_VISEE  = 'Sélectionner un type de visée';
  rsCDR_SERIE_AC_SELECT_CODE        = 'Sélectionner un code';
  rsCDR_SERIE_AC_SELECT_EXPE        = 'Sélectionner une séance';
  rsCDR_SERIE_AC_EXTRACT_LABELS     = 'Extraire les ID terrain depuis le commentaire de la visée' + #10 +
                                      'et les copier dans la colonne ID Terrain';
  rsCDR_SERIE_AC_LAST_ENTRANCE = 'Associer à l''entrée %d - %s';
  rsCDR_SERIE_LB_NB_SERIES      = '%d séries';
  // messages d'erreur
  rsCDR_SERIE_MSG_ERR_ENTREE_NOT_FOUND = 'Entrée introuvable';
  rsCDR_SERIE_MSG_ERR_RESEAU_NOT_FOUND = 'Réseau introuvable';
  rsCDR_SERIE_MSG_ERR_CODE_NOT_FOUND   = 'Code instruments introuvable';
  rsCDR_SERIE_MSG_ERR_EXPE_NOT_FOUND   = 'Session topo introuvable';
  rsCDR_SERIE_MSG_ERR_SERIE_NOT_FOUND  = 'Série introuvable';
  rsCDR_SERIE_MSG_ERR_LONG             = 'La longueur doit être positive et inférieure à %.0f m';
  rsCDR_SERIE_MSG_ERR_LRUD             = 'La distance ne doit pas être négative';
  rsCDR_SERIE_MSG_WARN_LRUD            = '[Note]: LRUD disproportionnée - Vérifier ces valeurs';

  rsCDR_SERIE_NB_ANTENNES_MODIFIEES    = '%d visées rayonnantes modifiées';
  rsCDR_SERIES_MSG_ERROR_LONGUEURS_ADMISES   = ' %.3f m incorrecte; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_ATTRIBUTED_NO_SERIE = 'Numéro de série déjà attribué';
  rsCDR_SERIES_MSG_ERROR_ORPHAN_SERIE        = 'Série non raccordée au réseau';
  rsCDR_SERIES_MSG_ERROR_CHECKING_SERIE      = 'Vérification des valeurs';
  rsCDR_SERIES_MSG_ERROR_AZIMUT_OUT_OF_RANGE = 'Azimut %.3f invalide; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_PENTE_OUT_OF_RANGE  = 'Pente %.3f invalide; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_NONEXISTENT_SECTEUR = 'Secteur %d inexistant';
  rsCDR_SERIES_MSG_ERROR_NONEXISTENT_RESEAU  = 'Réseau %d inexistant';
  rsCDR_SERIES_MSG_ERROR_NONEXISTENT_CODE    = 'Code %d inexistant - Contrôles angulaires inopérants';
  rsCDR_SERIES_MSG_ERROR_NONEXISTENT_EXPE    = 'Séance %d inexistant';
  rsCDR_SERIES_MSG_ERROR_INVALID_TYPE_VISEE  = 'Type de visee %d incorrect; valeurs admises: %d à %d';
  rsCDR_SERIES_MSG_ERROR_INVALID_LONGUEUR    = 'Longueur %.3f m incorrecte; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_INVALID_LG          = 'LG %.3f m incorrecte; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_INVALID_LD          = 'LD %.3f m incorrecte; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_INVALID_HZ          = 'HZ %.3f m incorrecte; valeurs admises: %.3f à %.3f';
  rsCDR_SERIES_MSG_ERROR_INVALID_HN          = 'HN %.3f m incorrecte; valeurs admises: %.3f à %.3f';
  // *******************************************************
  // resoursestring du cadre Antennes
  rsCDR_ANTENNES_AC_ADDLINE                  = 'Ajouter une ligne';
  rsCDR_ANTENNES_AC_DELLINE                  = 'Supprimer une ligne';
  rsCDR_ANTENNES_AC_SAVEGRD                  = 'Valider les modifications';
  rsCDR_ANTENNES_DEL_LINE                    = 'Supprimer la ligne %d';
  rsCDR_ANTENNES_FIND_RESEAU                 = 'Réseau pour la ligne %d';
  rsCDR_ANTENNES_FIND_CODE                   = 'Code pour la ligne %d';
  rsCDR_ANTENNES_FIND_EXPE                   = 'Expé pour la ligne %d';
  // *******************************************************
  // resourcestring du cadre CdrNavigateurDB
  rsCDR_NAVIG_DB_DO_SORT                     = 'Trier';
  rsCDR_NAVIG_DB_DO_ADD                      = 'Ajouter élément';
  rsCDR_NAVIG_DB_DO_DELETE                   = 'Supprimer élément';
  // *********************************
  // resourcestring du dialogue Vtopo
  rsVTOPO_EDPREFIX                           = 'Préfixe des stations';
  rsVTOPO_LBIDPTDEP                          = 'No point de départ';
  rsVTOPO_LBREPORTER                         = 'Opérateur du report';
  rsLBFICHIER                                = 'Fichier';
  rsLBMAINENTRANCE                           = 'Entrée principale';
  rsLBIDMAINST                               = 'Station de départ';
  rsLBSTPREFIX                               = 'Préfixe des stations';
  rsLBREPORTER                               = 'Report';
  rsLBMAINENTCOORD                           = 'Coordonnées de l''entrée principale';
  rsLBCOLORDEFAULT                           = 'Couleur par défaut';
  // *******************************************************
  // resourcestring du visualisateur OpenGL
  rsOGLVIEWERTITLE                           = 'Visualisateur OpenGL [%s]';
  rsLBLBACKCOLOR                             = 'Arrière-plan';
  rsLBLCUBECOLOR                             = 'Cube';
  rsLBLREFCOLOR                              = 'Référentiel';
  rsLBLREFSIZE                               = 'Taille';
  rsOPENGLERROR                              = 'Erreur OpenGL';
  rsOGLVQUIT                                 = 'Quitter le visualisateur OpenGL';
  // *******************************************************
  // resourcestrings de l''outil d''exportation graphique

  rsDODISPDESELECTEDPARTS                    = 'Afficher les parties refusées par le MétaFiltre - Couleur';
  rsTAB_LAYERS                               = 'Couches';
  rsTAB_QUADR                                = 'Quadrillage';
  rsTAB_DESSIN                               = 'Dessin';
  rsTAB_TEXTES                               = 'Textes';
  rsTYPEQDR                                  = 'Type de quadrillage';
    rsQDNONE          = 'Aucun';
    rsQDCROSS         = 'Grille';
    rsQDQUADRILLES    = 'Croix';
  rsPSDXF_TITLE                              = 'Export graphique: [%s]';
    rsGRAPHICS_PS     = 'PostScript PS';
    rsGRAPHICS_DXF    = 'AutoCAD DXF';
    rsGRAPHICS_SVG    = 'Scalable Vector Graphics SVG';
    rsGRAPHICS_WMF    = 'Windows MetaFile WMF';
  rsDLGDXF_TITLE                             = 'Export DXF: %s';
  // *******************************************************
  // mots clefs du MétaFiltre
  rsHLPMETAFILTRE                            = 'METAFILTRE';
  rsMETAFILTRE_APPLY                         = 'Appliquer';
    rsMETAFILTRE_NIL         = 'RIEN';
    rsMETAFILTRE_ALL         = 'TOUT';
    rsMETAFILTRE_ID          = 'No';
    rsMETAFILTRE_LENGTH      = 'LONGUEUR';
    rsMETAFILTRE_AZIMUTH     = 'GISEMENT';
    rsMETAFILTRE_PENTE       = 'PENTE';
    rsMETAFILTRE_DATE        = 'DATE';
    rsMETAFILTRE_COLOR       = 'COULEUR';
    rsMETAFILTRE_X           = 'COORD_X';
    rsMETAFILTRE_Y           = 'COORD_Y';
    rsMETAFILTRE_Z           = 'COORD_Z';
    rsMETAFILTRE_LARGEUR     = 'LARGEUR';
    rsMETAFILTRE_HAUTEUR     = 'HAUTEUR';
    rsMETAFILTRE_DATES       = 'DATES';
    rsMETAFILTRE_COLORS      = 'COULEURS';
    rsMETAFILTRE_SERIE       = 'SERIE';
    rsMETAFILTRE_RESEAU      = 'RESEAU';
    rsMETAFILTRE_CODE        = 'CODE';
    rsMETAFILTRE_EXPE        = 'SEANCE';
    rsMETAFILTRE_TYPEVISEE   = 'TYPE_VISEE';
    rsMETAFILTRE_SECTEUR     = 'SECTEUR';
    rsMETAFILTRE_ENTREE_RATT = 'ENTREE_RATTACHEMENT';
  rsMSG_METAFILTRE_ALL_REJECTED              = 'Le MétaFiltre a rejeté toutes les visées';
  // *******************************************************
  // recherche de stations
  rsMSG_FIND_STATION_TITRE                   = 'Recherche de station';
  rsMSG_FIND_STATION_PROMPT                  = 'Entrer un repère terrain ou un couple série et station (séparateur: point décimal)';
  // *******************************************************
  // resourcestring du dialogue du MétaFiltre
  rsDLGMETAFILTRE_TBS1                       = 'Dates';
  rsDLGMETAFILTRE_TBS2                       = 'Couleurs';
  rsDLGMETAFILTRE_PERSO                      = 'Personnalisés';
  // ******************************************************
  // resourcestring du sélecteur de couleurs
  rsSELECTCOLORTITLE                         = 'Selection d''une couleur';
  rsLBLUSEDCOLORS                            = 'Dernières couleurs:';
  rsDLGCOULSAVEPAL                           = 'Enregistrer palette';
  rsDLGCOULRESTPAL                           = 'Restaurer palette';
  rsDLGCOULFILTERPAL                         = 'Fichiers de palette (*.pal)|*.pal|Tous (*.*)|*.*';
  rsDLGCOUDELPAL                             = 'Ecraser le fichier existant';
  rsPALETTENOTFOUNT                          = 'Palette introuvable';
  // ******************************************************
  // resourcestring de l''utilitaire d''export graphique
  rsDLGGRAPHIC_OUTPUTFMT                     = 'Format de sortie:';
  rsDLGGRAPHIC_LBFILENAME                    = 'Nom de fichier:';
  rsDLGGRAPHIC_LBOBS                         = 'Commentaires:';
  rsDLG_GRAPHIC_TABTITLE                     = 'Export graphique';
  rsDLGGRAPHIC_GBCROIX                       = 'Quadrillage';
  rsDLGGRAPHIC_GBCHEM                        = 'Cheminements et sections';
  rsDLGGRAPHIC_GBWALLS                       = 'Parois et couleurs';
  rsDLGGRAPHIC_SPACING                       = 'Espacement';
  rsDLGGRAPHIC_TYPEGRID                      = 'Type:';
  rsDLGGRAPHIC_CMBGRD2                       = 'Croix';
  rdDLGGRAPHIC_LAYER                         = 'Couche:';
  rdDLGGRAPHIC_WALLFILL                      = 'Remplissage';
  rsDLGGRAPHIC_WFILL1                        = 'Plein (1 couleur)';
  rsDLGGRAPHIC_WFILL2                        = 'Types de galeries';
  rsDLGGRAPHIC_WFILL3                        = 'Couleurs des visées';
  rsDLGGRAPHIC_WFILL4                        = 'Par réseaux';
  rsDLGGRAPHIC_WFILL5                        = 'Par dates';
  rsDLGGRAPHIC_CHKCHEM                       = 'Exporter cheminements';
  rsDLGGRAPHIC_CHKSECT                       = 'Exporter sections';
  // ******************************************************
  // resourcestring du sélecteur de listes
  rsSELECT_LISTE_ENTRANCE                    = 'Sélection d''une entrée';
  rsSELECT_LISTE_RESEAU                      = 'Sélection d''un réseau';
  rsSELECT_LISTE_SECTEUR                     = 'Sélection d''un secteur';
  rsSELECT_LISTE_CODE                        = 'Sélection d''un code instruments';
  rsSELECT_LISTE_EXPE                        = 'Sélection d''une séance';
  rsSELECT_LISTE_SERIE                       = 'Sélection d''une série';
  // ******************************************************
  // resourcestring du centre d''impression
  rsPRN_NOPRINTER                            = 'Pas d''imprimante installée';
  rsPRN_TBPRINTER                            = 'Imprimante';
  rsPRN_TBDRAW                               = 'Dessin';
  rsPRN_TITLE                                = 'Centre dimpression [%s]';
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
  rsPRN_PRINTING_DONE                        = 'Impression terminée';
  rsDLGIMP_TAB1                              = 'Aperçu';
  rsDLGIMP_TAB2                              = 'Imprimante';
  rsDLGIMP_TAB3                              = 'Options de dessin';
  rsDLGIMP_TAB4                              = 'Réseaux';
  rsQDRSPACING                               = 'Espacement';
  rsECHELLE                                  = 'Echelle: 1 /';
  rsLAYERS                                   = 'Couches de dessin';
  rsPREVIEW                                  = 'Previsualisation';
  rsSTARTPRINTING                            = 'Lancer impression';
  rsREGLE                                    = 'Règle';
  // -------------------------------------------------
  // resourcestring du système d'aide
  rsHLP_TITRE                                = 'Système d''aide de GHTopo';
  rsHLP_BNEDIT                               = 'Editer le fichier d''aide';
  rsHLP_DOCONTINUE                           = 'Souhaitez-vous continuer ?';
  rsHLP_TAB_RUBRIQUES                        = 'Rubriques d''aide';
  rsHLP_TAB_ABOUT                            = 'A propos de GHTopo';
  // -------------------------------------------------
  // resourcestring du dialogue Serie/station
  rsDLG_SERST_TITLE                          = 'Recherche de station';
  rsDLG_SERST_SERIE                          = 'Série';
  rsDLG_SERST_STATION                        = 'Station';
  rsDLG_SERST_CLE                            = 'Code terrain';
  rsDLG_SERST_LBSEARCH                       = 'Recherche';
  rsDLG_SERST_BYSERST                        = 'Par couple série / station';
  rsDLG_SERST_BYREFTER                       = 'Par référence de terrain';
  // -------------------------------------------------
  // resourcestring du dialogue Editeur d''annotations
  rsDLG_ANN_TITLE                            = 'Editeur d''annotations';
  rsDLG_ANN_LBTEXTE                          = 'Annotation';
  rsDLG_ANN_LBMAX                            = 'Longueur max';
  rsDLG_ANN_CHKTXTDISP                       = 'Dessiner cette annotation';
  rsDLG_ANN_GRBPOSTEXT                       = 'Positionnement du texte';
  rsDLG_ANN_GRBMETH0                         = 'Coordonnées absolues';
  rsDLG_ANN_GRBMETH1                         = 'Accroché à la station';
  rsDLG_ANN_LBPOSTEXTE                       = 'Point de base';
  rsDLG_ANN_LBSTATION                        = 'Station topo';
  rsDLG_ANN_LBOFFSET                         = 'Décalage (m) : X = %.2f m ; Y = %.2f m';
  rsDLG_ANN_GRBATTRTXT                       = 'Attributs de caractères';
  rsDLG_ANN_GRBBASEPT                        = 'Point de base du texte';
  // -------------------------------------------------
  // recherche de station par ID littéral
  rsDLG_FIND_PT_BY_ID_TITLE                  = 'Recherche par ID littéral';
  rsDLG_FIND_PT_BY_ID_PROMPT                 = 'Code terrain';
  rsDLG_FIND_PROMPT                          = 'Texte recherché';
  rsDLG_FIND_ENTRANCE_BY_TEXT                = 'Recherche d''une entrée';
  rsDLG_FIND_RESEAU_BY_TEXT                  = 'Recherche de réseau';
  rsDLG_FIND_CODE_BY_TEXT                    = 'Recherche de code instruments';
  rsDLG_FIND_EXPE_BY_TEXT                    = 'Recherche de séance topo';
  rsDLG_FIND_SERIE_BY_TEXT                   = 'Recherche de série';
  rsDLG_FIND_SECTEUR_BY_TEXT                 = 'Recherche de secteur';
  // accès par numéro
  rsDLG_GOTO_PROMPT                          = 'Numéro';
  rsDLG_GOTO_ENTRANCE_BY_NUMERO              = 'Aller à une entrée';
  rsDLG_GOTO_RESEAU_BY_NUMERO                = 'Aller à un réseau';
  rsDLG_GOTO_SECTEUR_BY_NUMERO               = 'Aller à un secteur';
  rsDLG_GOTO_CODE_BY_NUMERO                  = 'Aller à un code instruments';
  rsDLG_GOTO_EXPE_BY_NUMERO                  = 'Aller à une séance topo';
  rsDLG_GOTO_SERIE_BY_NUMERO                 = 'Aller à une série';

  // resourcestring de l'outil calculette/convertisseurs
  rsDLG_CALC_TITLE                           = 'Calculateurs et outils complémentaires';
  rsDLG_CALC_TAB_CAL                         = 'Calculatrice';
  rsDLG_CALC_EXPR                            = 'Entrer une expression';
  rsDLG_CALC_DOCALC                          = 'Calculer';
  rsDLG_CALC_CDR_CONVERT                     = 'Convertisseur de coordonnées';
  rsDLG_CALC_CDR_DECLIMAG                    = 'Déclinaison magnétique';
  rsDLG_CALC_BTN_CONVERT                     = 'Convertir';
  rsDLG_CALC_BTN_CONVERT_TABLEAU             = 'Convertir les points';
  rsDLG_CALC_BTN_ECHANGE                     = 'Echanger';
  rsDLG_CALC_BTN_OPEN_CSV                    = 'Charger fichier CSV';
  rsDLG_CALC_BTN_PASTE                       = 'Coller vers le tableau';
  rsDLG_CALC_BTN_COPY_TABLEAU                = 'Copier le tableau';
  rsDLG_CALC_BTN_EXPORT_CSV                  = 'Exporter le tableau en CSV';
  rsDLG_CALC_BTN_EXPORT_GIS                  = 'Exporter les points en KML, GPX ou OSM';
  rsDLG_CALC_BTN_QRCODE_PT                   = 'Générer le QRCode de ce point';
  rsDLG_CALC_BTN_QRCODE_SEL                  = 'Générer le QRCode de la sélection';
  rsDLG_CALC_BTN_COPY_COORDS_ISOLEES         = 'Copier ces coordonnées';
  rsDLG_CALC_BTN_QRCODE_FROM_TEXT            = 'Générer un QRCode depuis le texte';
  rsDLG_CALC_BTN_QRCODE_TO_SVG               = 'Créer un SVG depuis le QRCode';
  rsDLG_CALC_LB_SYST_SOURCE                  = 'Système source';
  rsDLG_CALC_LB_SYST_CIBLE                   = 'Système cible';
  rsDLG_CALC_HINT_GRD_CONVERSIONS            = 'Saisir des valeurs dans la grille ou coller depuis le presse-papiers';
  rsDLG_CALC_EXPRESSION                      = 'Expression';
  rsDLG_CALC_SYSCOORDS                       = 'Systèmes de coordonnées';
  rsDLG_CALC_LSB_FUNCTIONS                   = 'Liste des fonctions';
  rsDLG_CALC_LSB_CALCULS                     = 'Liste des calculs';
  rsDLG_CALC_COORDS_UNITAIRE                 = 'Coordonnées isolées';
  rsDLG_CALC_COORDS_TABLEAU                  = 'Conversion en rafale';
  rsDLG_CALC_DECL_UNITAIRE                   = 'Déclinaison magnétique';
  rsDLG_CALC_DECL_TABLEAU                    = 'Tableau de déclinaisons';
  rsDLG_CALC_TAB_QRCODE                      = 'Génération de QRCodes';
  rsDLG_CALC_TAB_PASCAL_SCRIPT               = 'PascalScript';
  rsDLG_CALC_TAB_TEXTFILES                   = 'Import de fichiers textes';
  rsDLG_CALC_TAB_COORDS_QRCODE_LISTE         = 'Générer un QRCode de ces coordonnées';

  // gestion du presse papiers pour l''import de données dans la grille Stations
  rsDLG_CLIPBRD_PTS_TITLE                    = 'Importation de points topo';

  // resourcestring du cadre Série
  rsCDR_SERIES_ADD_SERIE                     = 'Ajouter une série';
  rsCDR_SERIES_VALID_SERIE                   = 'Enregistrer les modifications de la série courante';
  rsCDR_SERIES_SORT_SERIES                   = 'Trier les séries dans l''ordre croissant';
  rsCDR_SERIES_EXPORT_CSV                    = 'Exporter les en-têtes dans un fichier CSV';
  rsCDR_SERIES_HELP                          = 'Aide';

  rsDLG_BDD_APPLY_MODIFS                     = 'Modifier la ligne courante';
  rsDLG_BDD_FIND                             = 'Rechercher';
  rsDLG_BDD_COTO_BY_NUMERO                   = 'Aller à l''item numéro n';
  rsDLG_BDD_SORT                             = 'Trier';
  rsDLG_BDD_EXPORT_CSV                       = 'Exporter la liste en CSV';
  rsDLG_BDD_HELP_LISTES                      = 'Aide';

  // resourcestring du cadre Listes simples

  rsDLG_BDD_REMOVE_ENTREE                    = 'Supprimer une entrée';
  rsDLG_BDD_REMOVE_RESEAU                    = 'Supprimer un réseau';
  rsDLG_BDD_REMOVE_SECTEUR                   = 'Supprimer un secteur';
  rsDLG_BDD_REMOVE_CODE                      = 'Supprimer un code instruments';
  rsDLG_BDD_REMOVE_EXPE                      = 'Supprimer une séance';
  rsDLG_BDD_REMOVE_SERIE                     = 'Supprimer une série';

  rsDLG_BDD_ADD_ENTREE                       = 'Ajouter une entrée';
  rsDLG_BDD_ADD_RESEAU                       = 'Ajouter un réseau';
  rsDLG_BDD_ADD_SECTEUR                      = 'Ajouter un secteur';
  rsDLG_BDD_ADD_CODE                         = 'Ajouter un code instruments';
  rsDLG_BDD_ADD_EXPE                         = 'Ajouter une séance';
  rsDLG_BDD_ADD_SERIE                        = 'Ajouter une série';

  rsDLG_BDD_INSERT_ENTREE                    = 'Insérer une entrée ?';
  rsDLG_BDD_INSERT_RESEAU                    = 'Insérer un réseau ?';
  rsDLG_BDD_INSERT_SECTEUR                   = 'Insérer un secteur ?';
  rsDLG_BDD_INSERT_CODE                      = 'Insérer un code instruments ?';
  rsDLG_BDD_INSERT_EXPE                      = 'Insérer une séance ?';
  rsDLG_BDD_INSERT_SERIE                     = 'Insérer une série ?';

  // messages divers
  rsMSG_HAS_ALREADY_DATA                     = 'Le tableau contient déjà des données. Continuer ?';
  rsDELETEITEM                               = 'Détruire élément ?';
  rsONCLOSEPRJMNGR                           = 'Ceci fermera le document courant - Sauvegarder les modifications ?';
  rsMSG_SEEALSO                              = 'Voir aussi ...';
  rsMSG_NDSNEEDED                            = 'Fichier noeuds inexistant - Recalculer le réseau';
  rsMSG_SAVECHANGES                          = 'Enregistrer les modifications';
  rsMSG_ERASESTATION                         = 'Ecraser la station %d ?';
  rsMSG_FILENOTFOUND                         = 'Fichier %s non trouvé';
  rsMSG_READY                                = 'PRET';
  rsMSG_NOFILEOPENED                         = 'Pas de fichier ouvert';
  rsDISPLAY_HELP_SYSTEM                      = 'Démarrage du système d''aide';
  rsHLPCENTER_TITLE                          = 'Système d''aide de GHTopo';
  rsMATCHNOTFOUND                            = 'Occurrence non trouvée';
  rsNOCANCLOSE                               = 'Quitter en sauvegardant';
  rsWARN_FILEALREADYOPEN                     = 'Un document est déjà ouvert - Poursuivre ?';
  rsSAVECHANGES                              = 'Enregistrer les modifications';
  rsDO_RELOAD_DOCUMENT                       = 'Recharger le document';
  rsNOCANCLOSEWND                            = 'Fenêtre permanente de GHTopo, ne peut être fermée';
  rsERASEEXISTNAMEDFILE                      = 'Ecraser le fichier %s ?';
  rsSAVESLOST                                = 'Les modifications seront perdues';
  rsCFGMTFLTR_UNABLE                         = 'Erreur dans le fichier de filtres personnalisés';
  rsERRORSTARTPRNCTR                         = 'Erreur de démarrage du Centre d''Impression';
  rsNOPRINTERINSTALLED                       = 'Aucune imprimante installée';
  rsERRORLOADINGTOP                          = 'Erreur en chargement du fichier TOP';
  rsIDSERIE_EXISTS                           = 'Index de série %d déjà attribué (série #%d)';
  // Messages d''erreur
  rsMSG_VUE3D_FAIL                           = 'Echec au démarrage de la vue 3D';
  rsMSG_VUE2D_FAIL                           = 'Echec au démarrage de la vue en plan';
  rsMSG_STATS_DLG_FAIL                       = 'Echec au démarrage du dialogue de statistiques';
  rsMSG_PRINT_CENTER_FAIL                    = 'Echec au démarrage du centre dimpression';
  rsMSG_PROJ_MANAGER_FAIL                    = 'Echec au démarrage du frontal de gestion de la base';
  rsMSG_PROJ4S_FAIL                          = 'Echec au démarrage du convertisseur de coordonnées Proj4s';
  rsMSG_DECLIMAG_FAIL                        = 'Echec au démarrage du calculateur de déclinaison magnétique';
  rsMSG_TABLE_ENTITY_FAIL                    = 'Echec au chargement de la table des entités';
  rsMSG_CODE_NOT_FOUND                       = 'Code introuvable';
  rsMSG_EXPE_NOT_FOUND                       = 'Expé introuvable';
  rsMSG_RESEAU_NOT_FOUND                     = 'Réseau introuvable';
  rsMSG_SECTEUR_NOT_FOUND                    = 'Secteur introuvable';
  rsMSG_ANGLE_OUT_OF_RANGE                   = '%s doit être compris entre %.2f et %.2f';
  rsMSG_WARN_DELETE_ITEM                     = 'La suppression de l''objet %d peut désorganiser la base. Continuer ?';
  // Messages dans TToporobotStructure.LoadFichierTab()
  rsRD_TAB_MSG_ERR                           = 'Erreur lors du traitement de la ligne';
  rsRD_TAB_D_MULTI_OBS                       = 'Début de commentaires multilignes en ligne %d';
  rsRD_TAB_F_MULTI_OBS                       = 'Fin de commentaires multilignes en ligne %d';
  rsRD_TAB_STOP_9999                         = '*** Line %d: Lecture arrêtée par le code -9999 ***';
  rsRD_TAB_LN_OBS                            = '-- La ligne %d est un commentaire';
  rsRD_TAB_LASTSAVES                         = 'Dernière sauvegarde: %s %s';
  rsRD_TAB_CLASSEURS                         = '-7 [Classeurs] Cette fonctionnalité est ignorée';
  rsRD_TAB_ENTRANCE                          = 'Entrée créée #%d';
  rsRD_TAB_ENTR_NOGEOREF                     = '[Avertissement] (Ligne %d): Entrée %d (%s) non géoréférencée';
  rsRD_TAB_ENTR_BADLINK                      = '[Avertissement] (%d): Entrée %d (%s): Raccordement incorrect [Série: %d - Station: %d]';
  rsRD_TAB_IGNORED_SEC                       = '[Information] Section ignorée';
  rsRD_TAB_BAD_TRIP                          = 'Séance incorrecte';
  rsRD_TAB_BAD_CODE                          = 'Code incorrect';
  rsRD_TAB_BAD_DATE                          = '[Avertissement] (%d) - Date incorrecte mise à la date actuelle';
  rsRD_TAB_BAD_IDXRES                        = '[Avertissement] (%d) - Index de réseau incorrect (%d) pour la série %d - Mis à 0';
  rsRD_TAB_IDEXTR_MM                         = '[Avertissement] (%d) - Les ID dextrémité de la série %d sont identiques';
  rsRD_TAB_SELF_CLOSURE                      = '[Avertissement] (%d) - Le terminus de la série %d se branche sur elle-même.';
  rsRD_TAB_NEG_LONG                          = '[Avertissement] (%d) - Longueur négative (%.2f m), changée de signe';
  rsRD_ERROR_LN                              = '*** Erreur en ligne #%d: %s';
  rsRD_CONTNT_LN                             = 'Contenu de la ligne: %s';
  rsRD_TAB_FIELD_VALUES                      = 'Valeurs des champs:';
  rsRD_TAB_NOFATAL_ERR                       = 'Le fichier ne comporte pas derreurs fatales';
  rsRD_TAB_WTFATAL_ERR                       = 'Le fichier comporte %d erreurs';
  // *************************************************************
  // resourcestring du code de calcul
  rsDEFAULT                                  = 'Défaut';
  rsFORFIXEDPOINTS                           = 'Item pour entrees et points fixes';
  rsWARNINGENTRYADDED                        = 'Le nombre d''entrées dans -6 est différent de celui de -5 Corrigé.';
  rsCALCULNODES                              = 'CALCUL DES COORDONNEES DES NOEUDS';
  rsLINEOFNB                                 = '---> Ligne %d / %d';
  rsBINDMATRIX                               = 'MATRICE D''INCIDENCE: R';
  rsNB_NON_NUL_TERMES                        = '%d termes non nuls dans la matrice R';
  rsPR100_NON_NULS                           = 'soit %.3f%% de (%d x %d) = %d termes';
  rsFINDING_BRANCHES                         = 'Recensement des branches';
  rsADDENTRANCES                             = 'Ajout des entrees';
  rsBUILDMATRICE                             = 'CONSTRUCTION DE LA MATRICE DE COMPENSATION: B = Rt.Wi.R';
  rsBUILDMATRICE_LAPLACIENNE                 = 'CONSTRUCTION DE LA MATRICE LAPLACIENNE: B = Rt.R';

  rsFIND_SUM_LIMITS                          = '-- Recherche des limites utiles de sommation';
  rsFACTORISEMATRICE                         = 'FACTORISATION DE LA MATRICE DE COMPENSATION';
  rsFACTORISEMATRICE_LAPLACIENNE             = 'FACTORISATION DE LA MATRICE LAPLACIENNE';
  rsAXIS                                     = 'Axe:';
  rsDESCENTE                                 = '--> Descente: V.V* = A';
  rsREMONTEE                                 = '--> Remontee du systeme';
  rsTRIANGULARISATION                        = '--> Triangularisation';
  rs2NDMEMBER                                = '- Second membre';
  rsCOMPESMATRIX                             = '- Matrice de compensation';
  rsNODECOORD                                = '- Factorisation: Coordonnées des noeuds';
  rsCOORDONNEES_OK                           = '--> Coordonnées des %d noeuds OK';
  rsDEL_TEMP_MATRIX                          = '--> Destruction des matrices temporaires';
  rsWRITE_NODES_COORDS                       = 'Ecriture des coordonnées des noeuds dans:';
  rsNODES_COORDS_FOR_DB                      = 'Coordonnées des noeuds pour:';
  rsCALCULCONTOURS                           = 'CALCUL CONTOURS GALERIES';
  rsCALCUL_ANTENNES                          = 'Calcul des visees en antenne';
  rsCALCUL_TERMINE                           = 'Calcul terminé';
  rsRECENSEM_JONC                            = '-- Recensement des jonctions';
  rsPURGE_TABLE_JONC                         = '-- Purge table des jonctions';
  rsPURGE_TABLE_BRCH                         = '-- Purge table des branches';
  rsNB_BRCHS                                 = '%d branches';
  rsSTEP_CALC_01                             = '->Etape %d / %d: Matrice d''assemblage R';
  rsSTEP_CALC_02                             = '->Etape %d / %d: Matrice de pondération W';
  rsSTEP_CALC_03                             = '->Etape %d / %d: Matrice de compensation B = Rt.W.R';
  rsSTEP_CALC_04                             = '->Etape %d / %d: Coordonnées des noeuds';
  rsSTEP_CALC_05                             = '->Etape %d / %d: Répartition des écarts';
  rsSTEP_CALC_06                             = '->Etape %d / %d: Libérations mémoire et fin de traitement';
  rsFREE_TEMP_VARS                           = 'LIBERATION DES VARIABLES TEMPORAIRES';
  rsREPARTIR_ECARTS                          = 'REPARTITION DES ECARTS';
  rsSCAN_BRCHS                               = '-- Balayage des branches';
  rsCHECK_VISEE_VALID_INTERVAL               = '%s doit être compris entre %.2f %s et %.2f %s';
  rsCHECK_VISEE_VALID_TYPE_VISEE             = 'Le type de visée doit être compris entre %d et %d';

  // lecture des fichiers
  rsSEPARATOR_LINE                           = '-------------------------------';
  rsWARNINGS_READFILE                        = 'Avertissements de lecture de:';
  rsCONV_TAB_MAC_UNIX                        = '-> Conversion des fichiers TAB pouvant venir du Mac ou d''Unix';
  // -------------------------------------------------

  // -------------------------------------------------
  // Misc
  rsSELECTALL                                = 'Tout sélectionner';
  rsDESELECTALL                              = 'Tout désélectionner';
  rsOBS                                      = 'Commentaires';
  rsFILTERS                                  = 'Filtres';
  rsCHKSELECTALL                             = 'Sél. tout';
  rsCHKDESELECTALL                           = 'Désél. tout';
  rsCHKREVERSE                               = 'Inv. sél.';
  rsMAIN_NETWORK                             = 'Réseau principal';
  // -------------------------------------------------
  // resourcestrings de dlgSelectDansListes
  rsSELECT_LISTE_GENERAL_ID                  = 'Numéro';
  rsSELECT_LISTE_NB_ELEMENTS                 = 'Liste des %d %s';
  rsSELECT_LISTE_OBSERV                      = 'Observations';
  rsSELECT_LISTE_ENTREES                     = 'Entrées';
  rsSELECT_LISTE_ENTREES_ID                  = 'Numéro';
  rsSELECT_LISTE_ENTREES_NOM                 = 'Entrée';
  rsSELECT_LISTE_ENTREES_REF                 = 'Réf.';
  rsSELECT_LISTE_ENTREES_X                   = 'X';
  rsSELECT_LISTE_ENTREES_Y                   = 'Y';
  rsSELECT_LISTE_ENTREES_Z                   = 'Z';
  rsSELECT_LISTE_RESEAUX                     = 'Réseaux';
  rsSELECT_LISTE_RESEAUX_ID                  = 'Numéro';
  rsSELECT_LISTE_RESEAUX_COLOR               = 'Couleur';
  rsSELECT_LISTE_RESEAUX_NOM                 = 'Réseau';
  rsSELECT_LISTE_SECTEURS                    = 'Secteurs';
  rsSELECT_LISTE_SECTEURS_ID                 = 'Numéro';
  rsSELECT_LISTE_SECTEURS_COLOR              = 'Couleur';
  rsSELECT_LISTE_SECTEURS_NOM                = 'Secteur';
  rsSELECT_LISTE_CODES                       = 'Codes';
  rsSELECT_LISTE_CODES_ID                    = 'Numéro';
  rsSELECT_LISTE_CODES_AZIMUTS               = 'Azimuts';
  rsSELECT_LISTE_CODES_PENTES                = 'Pentes';
  rsSELECT_LISTE_CODES_OBS                   = 'Commentaires';
  rsSELECT_LISTE_EXPES                       = 'Séances';
  rsSELECT_LISTE_EXPES_ID                    = 'Numéro';
  rsSELECT_LISTE_EXPES_COULEUR               = 'Couleur';
  rsSELECT_LISTE_EXPES_DATE                  = 'Date';
  rsSELECT_LISTE_EXPES_OPERATEUR_TOPO        = 'Opérateur 1';
  rsSELECT_LISTE_EXPES_CLUB_SPELEO           = 'Equipe ou club';
  rsSELECT_LISTE_EXPES_DECLINAISON           = 'Déclinaison';
  rsSELECT_LISTE_EXPES_OBS                   = 'Commentaires';
  rsSELECT_LISTE_SERIES                      = 'Séries';
  rsSELECT_LISTE_SERIES_ID                   = 'Numéro';
  rsSELECT_LISTE_SERIES_DEPART               = 'Départ';
  rsSELECT_LISTE_SERIES_ARRIVEE              = 'Arrivée';
  rsSELECT_LISTE_SERIES_NOM                  = 'Nom';
  rsSELECT_LISTE_SERIES_ENTREE               = 'Entrée';
  rsSELECT_LISTE_SERIES_RESEAU               = 'Réseau';
  rsSELECT_LISTE_SERIES_NBPOINTS             = 'Nb Points';
  rsSELECT_LISTE_DATES                       = 'Dates';
  rsSELECT_LISTE_UNE_DATE                    = 'Date';
  rsSELECT_LISTE_STATIONS                    = 'Points topo';
  rsSELECT_LISTE_STATIONS_ID                 = 'Numéro';
  rsSELECT_LISTE_STATIONS_NB                 = 'Nb stations';
  rsSELECT_LISTE_STATIONS_LABEL              = 'ID littéral';
  rsSELECT_LISTE_STATIONS_X                  = 'X';
  rsSELECT_LISTE_STATIONS_Y                  = 'Y';
  rsSELECT_LISTE_STATIONS_Z                  = 'Z';

  rsSELECT_LISTE_ANNOTATIONS_IDX             = 'No';
  rsSELECT_LISTE_ANNOTATIONS_SERIE_PT        = 'Station';
  rsSELECT_LISTE_ANNOTATIONS_OFFSET_X        = 'dx';
  rsSELECT_LISTE_ANNOTATIONS_OFFSET_Y        = 'dy';
  rsSELECT_LISTE_ANNOTATIONS_OFFSET_Z        = 'dz';
  rsSELECT_LISTE_ANNOTATIONS_IDX_STYLE       = 'Style';
  rsSELECT_LISTE_ANNOTATIONS_ALIGNMENT       = 'Align.';
  rsSELECT_LISTE_ANNOTATIONS_TEXT            = 'Texte';
  // -------------------------------------------------
  // resourcestrings de dlgExportVersSIGExt
  rsEXPORT_SIG_TITRE                         = 'Export vers SIG';
  rsEXPORT_SIG_SYST_COORDONNEES              = 'Système de coordonnées';
  rsEXPORT_SIG_SIG_CIBLE                     = 'Format de sortie';
  rsEXPORT_SIG_LB_FICHIER                    = 'Nom du fichier';
  rsEXPORT_SIG_USE_COLORS                    = 'Utiliser couleurs de groupes';
  rsEXPORT_SIG_SILHOUETTE                    = 'Silhouettes';
  rsEXPORT_SIG_BTN_GO                        = 'Générer';
  rsEXPORT_SIG_BTN_PREFIX                    = 'Préfixe';
  // -------------------------------------------------
  // resourcestrings de dlgStatistiquesExt
  rsSTATISTIQUES_TITRE                       = 'Statistiques';
  rsSTATISTIQUES_TAB_TABLEAUX                = 'Tableaux';
  rsSTATISTIQUES_TAB_SYNTHESE                = 'Synthèse';
  rsSTATISTIQUES_TAB_RESEAU_SECTEURS         = 'Réseaux et secteurs';
  rsSTATISTIQUES_TAB_DIAGRAMMES              = 'Diagrammes';
  rsSTATISTIQUES_TAB_CORDONNEES              = 'Coordonnées des stations';
  rsSTATISTIQUES_LBL_TITRE_ROSE_DIAGRAM      = 'Directions';
  rsSTATISTIQUES_LBL_TITRE_DEPTH_DIAGRAM     = 'Altitudes';
  rsSTATISTIQUES_LBL_LONG_MINI_VISEE         = 'Longueur mini visée';
  rsSTATISTIQUES_LBL_NB_PETALES              = 'Nb de pétales';
  rsSTATISTIQUES_LBL_NB_BARRES               = 'Nb de barres';
  rsSTATISTIQUES_BTN_REDESSINER              = 'Redessiner';
  rsSTATISTIQUES_CMB_MODE_TABLE0             = 'Réseaux';
  rsSTATISTIQUES_CMB_MODE_TABLE1             = 'Secteurs';
  rsSTATISTIQUES_CMB_MODE_TABLE2             = 'Codes instruments';
  rsSTATISTIQUES_CMB_MODE_TABLE3             = 'Séances';
  rsSTATISTIQUES_CMB_MODE_TABLE4             = 'Dates';
  rsSTATISTIQUES_NB_RESEAUX                  = '%d réseaux';
  rsSTATISTIQUES_NB_SECTEURS                 = '%d secteurs';
  rsSTATISTIQUES_NB_CODES                    = '%d codes';
  rsSTATISTIQUES_NB_SEANCES                  = '%d séances';
  rsSTATISTIQUES_NB_DATES                    = '%d dates';
  rsSTATISTIQUES_TYPE_SHOT_FOSSILES          = 'Fossiles';
  rsSTATISTIQUES_TYPE_SHOT_VADOSES           = 'Vadoses';
  rsSTATISTIQUES_TYPE_SHOT_ENNOYABLES        = 'Inondables';
  rsSTATISTIQUES_TYPE_SHOT_SIPHONS           = 'Siphons';
  rsSTATISTIQUES_TYPE_SHOT_NATURELS          = 'Naturels';
  rsSTATISTIQUES_TYPE_SHOT_SPECIAUX          = 'Spéciaux';
  rsSTATISTIQUES_TYPE_SHOT_TUNNELS           = 'Tunnels';
  rsSTATISTIQUES_TYPE_SHOT_MINES             = 'Filons';
  rsSTATISTIQUES_COPY_TABLEAU                = 'Copier le tableau';
  // resourcestrings de dlgUtilsDistoX
  rsUTILS_DISTOX_CAPTION                     = 'Utilitaires DistoX [%s]';
  // resourcestrings de dlgCoupeDeveloppee
  rsCOUPE_DEVEL_AC_OPEN_COUPE                = 'Ouvrir une coupe développée';
  rsCOUPE_DEVEL_AC_SAVE_COUPE                = 'Enregistrer la coupe développée';
  rsCOUPE_DEVEL_AC_EXPORT_SVG                = 'Exporter en SVG';
  rsCOUPE_DEVEL_AC_REVERSE_BRANCHE           = 'Pivoter branche';
  rsCOUPE_DEVEL_AC_ADD_SERIE                 = 'Ajouter une série';
  rsCOUPE_DEVEL_AC_REMOVE_SERIE              = 'Retirer la série';
  rsCOUPE_DEVEL_AC_MAKE_COUPE                = 'Construire la coupe développée';
  rsCOUPE_DEVEL_AC_DO_REMOVE_SERIE           = 'Retirer la série';

  // resourcestrings de CadreListeSeriesFlatMode
  rsCDR_SERIES_FLAT_TAB_GOTO_LINE            = 'Aller sur la ligne';
  rsCDR_SERIES_FLAT_TAB_FIND                 = 'Rechercher';
  rsCDR_SERIES_FLAT_TAB_LOAD_BUFFER          = 'Charger le buffer';
  rsCDR_SERIES_FLAT_TAB_SAVE_BUFFER          = 'Sauver le buffer';
  rsCDR_SERIES_FLAT_TAB_COPY_TABLEAU         = 'Copier le tableau';
  rsCDR_SERIES_FLAT_TAB_INSERT_LINE          = 'Insérer une ligne';
  rsCDR_SERIES_FLAT_TAB_REMOVE_LINE          = 'Supprimer la ligne';
  rsCDR_SERIES_FLAT_TAB_INSERT_SERIE_HERE    = 'Nouvelle série depuis ce point';
  rsCDR_SERIES_FLAT_TAB_CONTINUE_HERE        = 'Continuer cette série';
  rsCDR_SERIES_FLAT_TAB_PARSE_TABLEAU        = 'Analyser le tableau';
  rsCDR_SERIES_FLAT_TAB_SORT_TABLEAU         = 'Trier le tableau dans l''ordre des séries';
  rsCDR_SERIES_FLAT_EDIT_SERIE_BY_DIALOG     = 'Editer série';
  rsCDR_SERIES_FLAT_TAB_UNDOCOPY             = 'Recopier vers le bas';
  rsCDR_SERIES_FLAT_MSG_ROW_UNDELETABLE      = 'La ligne %d contient un en-tête de série et ne peut être supprimée';
  rsCDR_SERIES_FLAT_MSG_BUFFER_SAVED         = 'Buffer %s saved %s';
  rsCDR_SERIES_FLAT_MSG_REMOVE_LINE          = 'Supprimer %d lignes à partir de la ligne %d';
  rsCDR_SERIES_FLAT_MSG_REPLACE_BUFFER       = 'Remplacer les données de la grille';
  rsCDR_SERIES_FLAT_MSG_CONTINUE_THIS_ACTION = 'Effectuer cette action';
  rsCDR_SERIES_FLAT_NEW_SERIE                = 'Nouvelle série';
  rsCDR_SERIES_FLAT_MSG_PARSING              = 'Analyse en cours ...';
  rsCDR_SERIES_FLAT_MSG_PARSING_LINES        = 'Processing %d lines';
  rsCDR_SERIES_FLAT_MSG_DO_CREATE_SERIE_NTH  = 'Créer la série %d';
  rsCDR_SERIES_FLAT_MSG_NB_LINES             = 'Nombre de lignes';
  rsWARN_SERIE_REVERSE                       = 'La série %d arrive sur le réseau par la station %d.%d';
  rsERR_SERIE_START_STATION_NOT_FOUND        = 'Station de départ introuvable';
  rsERR_SERIE_END_STATION_NOT_FOUND          = 'Station d''arrivée introuvable';
  rsWARN_SERIE_CONNECTED_FIXPT               = 'La série %d arrive sur un point fixe ou sur le départ d''une série: %d.%d';
  rsERR_RESEAU_NOT_FOUND                     = 'Réseau introuvable';
  rsERR_ENTRANCE_NOT_FOUND                   = 'Entrée introuvable';


  // dlialogue Recherche de station
  rsDLG_FIND_STATION_TITLE                   = 'Recherche de station topo';
  rsDLG_FIND_STATION_PROMPT                  = 'Libellé du point topo';
  rsDLG_FIND_STATION_DO_MATCH                = 'Correspondance exacte';

  // resourcestring du navigateur séries
  rsCDR_NAVIG_SERIES_TO_FIRST                = 'Première série';
  rsCDR_NAVIG_SERIES_TO_PREV                 = 'Série précédente';
  rsCDR_NAVIG_SERIES_TO_NEXT                 = 'Série suivante';
  rsCDR_NAVIG_SERIES_TO_LAST                 = 'Dernière série';
  rsCDR_NAVIG_SERIES_GOTO_SERIE_NB           = 'Vers la série %d';
  rsCDR_NAVIG_SERIES_NEW                     = 'Nouvelle série';
  rsCDR_NAVIG_SERIES_IMPLEMENT_MODIFS        = 'Valider les modifications';
  rsCDR_NAVIG_SERIES_GOTO_SERIE_BY_NUMERO    = 'Aller à la série spécifiée par son numéro';
  rsCDR_NAVIG_SERIES_FIND_SERIE_BY_TEXT      = 'Rechercher une série contenant le texte';
  rsCDR_NAVIG_SERIES_TRIER                   = 'Trier les séries en ordre croissant';





  // resourcestring du cadre Visualisateur 2D
  rsVUE2D_WINDOW_TITLE                       = 'Vue en plan';
  rsCDR_METAFILTRE_PROMPT                    = 'Filtres';
  rsCDR_METAFILTRE_CHK_ACTIF                 = 'Actif';
  rsCDR_VUE2D_TAB_VUE_INITIALE               = 'Vue initiale';
  rsCDR_VUE2D_TAB_VUE                        = 'Vue %0.2d';
  rsCDR_VUE2D_AC_ZOOM_ALL                    = 'Zoom tout';
  rsCDR_VUE2D_AC_ZOOM_WINDOW                 = 'Zoom fenêtre';
  rsCDR_VUE2D_AC_PAN_VUE                     = 'Pan vue';
  rsCDR_VUE2D_AC_PARAM_ONGLET                = 'Préférences de la vue';
  rsCDR_VUE2D_AC_DISTANCE_BT_STATIONS        = 'Distance entre deux stations';
  rsCDR_VUE2D_MNU_FILTRES_STATION            = 'Filtres selon la station: %d.%d [%s]';

  rsCDR_VUE2D_ADD_SERIE_HERE                 = 'Insérer une nouvelle série ici';
  rsCDR_VUE2D_DISP_STATIONS_VOISINES         = 'Stations autour de ce point';
  rsCDR_VUE2D_ADDSERIE_BETWEEN_STATION       = 'Insérer une série entre deux stations';
  rsCDR_VUE2D_ISOVALEUR_FROM_Z               = 'Courbe de niveau pour l''altitude indiquée';
  rsCDR_VUE2D_ADD_VISEE_RADIANTE_HERE        = 'Insérer une visée radiante ici';



  // titres du sous-menu
  rsCDR_VUE2D_AC_METAFILTRE_SERIE            = 'Série %d';
  rsCDR_VUE2D_AC_METAFILTRE_TOPO_DU_JOUR     = 'Topo du %s';
  rsCDR_VUE2D_AC_METAFILTRE_ENTRANCE         = 'Entrée %d';
  rsCDR_VUE2D_AC_METAFILTRE_RESEAU           = 'Réseau %d';
  rsCDR_VUE2D_AC_METAFILTRE_SECTEUR          = 'Secteur %d';
  rsCDR_VUE2D_AC_METAFILTRE_CODE             = 'Code %d';
  rsCDR_VUE2D_AC_METAFILTRE_EXPE             = 'Séance %d';
  rsCDR_VUE2D_AC_METAFILTRE_YEAR             = 'Année %d';
  rsCDR_VUE2D_AC_METAFILTRE_CURRENT_VIEW     = 'Filtrer sur la vue actuelle';
  rsCDR_VUE2D_AC_ISOVALEUR_Z                 = 'Courbe de niveau pour l''altitude %.0f m';

  // message Recalculer le réseau
  rsMSG_QUESTION_RECALCULER                  = 'Recalculer le réseau';

  // fenêtre Visualisateur 2D
  rsVUE2D_GRBX_ELEMENTS_DRAWN                = 'Eléments dessinés';
    rsVUE2D_CHK_DRW_STATIONS        = 'Stations';
    rsVUE2D_CHK_DRW_ENTRANCES       = 'Entrées';
    rsVUE2D_CHK_DRW_WALLS           = 'Parois';
    rsVUE2D_CHK_DRW_CENTERLINES     = 'Polygonales';
    rsVUE2D_CHK_DRW_ALTITUDES       = 'Altitudes';
    rsVUE2D_CHK_DRW_COTES           = 'Cotes';
    rsVUE2D_CHK_DRW_ANTENNES        = 'Visées radiantes';
    rsVUE2D_CHK_DRW_IDStations      = 'Numéro';
    rsVUE2D_CHK_DRW_SECTIONS        = 'Sections';
    rsVUE2D_CHK_DRW_FILL            = 'Remplissage';
    rsVUE2D_CHK_DRW_QUADRILLAGE     = 'Quadrillage';
  rsVUE2D_GRBX_DEGRADE_ALTITUDES    = 'Dégradé d''altitudes';
  rsVUE2D_PRMS_DRW_BTN_APPLY        = 'Appliquer';
//================================================================================
implementation

end.
