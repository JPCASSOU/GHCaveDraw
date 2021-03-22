unit UnitMessages_it;
  // Fichier UnitMessages_it.pas cree le 09/05/2014 a 19:28:34
  // Genere par les macros du classeur: file:///C:/0_Logiciels_JPC/0_GHTopoFPC_20140503/Outils_Calc/GestionResourceStrings.ods
  //********************************************************************************
  // /!\ DO NOT MODIFY - Generated automatically
  // Please use spreadsheet utility for modification of strings
  // Language: IT - Italiano
  // ================================================================================
interface
uses
  StructuresDonnees; // pour le code instrument lasermetres

// CONST section
const
  NATIONAL_GEODESIC_SYSTEM_IDX = 2;
  DEFAULT_CODE_EPSG = 'UTM31';
// RESOURCESTRING section
resourcestring
  // Nome del programma
  rsGHTOPOEXENAME = 'GHTopo';
  rsMAINMENUCAPTION = '%s - GHTopo';
  rsGHTOPOVERSION = 'Version 3.1415926 du %.2d/%.2d/%.4d';
  rsGHTOPOLICENSE = '';
  rsGHTOPOAUTHOR = '(c) 1989..%d Jean Pierre CASSOU';
  // 
  rsGHTOPORESOL = 'Resoluzione%d x %d';
  rsGHTOPOPlatFormLinux = 'Plataforma: Linux (Italiano)';
  rsGHTOPOPlatFormWin32 = 'Plataforma: Microsoft Windows (Italiano)';
  // 
  rsCHOOSELANGUAGE = '';
  // 
  rsENDOFHADES = '';
  // *********************************************
  // Common labels resourcestrings
  rsLBL_COMMENTS = 'Osservaziones';
  rsSERIE_INITIALISATION = '';
  // Filtros de fichero
  rsFILEFILTER_ALL = 'Tutti (*.*)|*.*';
  rsGHTOPO_FILE_FILTER = 'GHTopo (*.xtb)|*.xtb|LimeLight Tab (*.Tab)|*.Tab|GHTopo XML (*.gtx)|*.gtx|Tutti (*.*)|*.*';
  rsKML_FILE_FILTER = 'Google Earth (*.kml)|*.xtb|Tutti (*.*)|*.*';
  rsGCD_FILE_FILTER = 'GHCaveDraw centerlines (*.gcd)|*.gcd|Tutti (*.*)|*.*';
  rsGPX_FILE_FILTER = 'Ficheros GPS (*.gpx)|*.gpx|Tutti (*.*)|*.*';
  rsOSM_FILE_FILTER = 'OpenStreetMap (*.osm)|*.osm|Tutti (*.*)|*.*';
  rsCSV_FILE_FILTER = 'Tabulated text (*.csv)|*.csv|Tutti (*.*)|*.*';
  rsVTOPO_FILE_FILTER = 'Visual Topo (*.tro)|*.tro|Tutti (*.*)|*.*';
  rsTHERION_FILE_FILTER = 'Therion centerlines (*.th)|*.th|Tutti (*.*)|*.*';
  // *********************************************
  // Main menu resourcestring
  rsMSGASSIGNCAPTIONS = '';
  rsCALCURAPIDE = '';
  rsMNU_FILE = '';
  rsNEW = '&Nuovo';
  rsOPEN = '';
  rsSAVE = '';
  rsSAVEAS = '';
  rsMNU_SAVELOG = '';
  rsCLOSE = '';
  rsRECENT_DOCS = '';
  rsRELOAD = '';
  rsEDITTAB = '';
  rsPRINT = '';
  rsVTOPO = '';
  rsEXPORT_THERION = 'Therion centerlines';
  rsGENERER_DOSSIER_THERION = '';
  rsEXPORT_ABRIS = '';
  rsGHCAVEDRAW = '';
  rsEXPORT_GIS = '';
  rsEXPORTGRAPHIQUE = '';
  rsERRINFOXTB = '';
  rsGHTOPO_QUIT = '';
  rsQUIT_EDITOR = '';
  rsMNU_EDITION = '';
  rsMNU_TOPOGRAPHIE = '';
  rsCHECKBASE = '';
  rsCOMPILE = '';
  rsVUEPLAN = '';
  rsVUE3D = '';
  rsRENDU3D = '';
  rsSTATISTIQUES = '';
  rsINFOCAVITE = '';
  rsNODESCOORDINATES = '';
  rsMNU_WINDOW = '';
  rsMNU_STAY_ON_TOP = '';
  rsWND_DATABASE = 'Data base';
  rsWND_PLAN = 'Guarda la mappa';
  rsWND_CONSOLE = '&Console';
  rsMNU_TOOLS = 'Strumenti';
  rsMNU_HELP = 'aiuto';
  rsHLPINDEX = 'indice';
  rsHLPNEWS = 'notizie';
  rsABOUT = 'Chi GHTopo';
  // Wizard resourcestrings
  rsASSISTANT_SUCCESS = 'Documento creato con successo da Wizard';
  rsASSISTANT_RELOAD = 'documento ricarica';
  rsASSISTANT_ECHEC = 'Wizard non riuscita.';
  rsASSIST_TITLE = 'Nuova procedura guidata grotta';
  rsASSIST_BNGENERAL = 'Generale';
  rsASSIST_BNENTRANCES = '1era Entrata';
  rsASSIST_BNCODES = '1ero Codice';
  rsASSIST_BNEXPES = '1era sessione';
  rsASSIST_BNSERIE = '1era Serie';
  rsASSIST_BNSAVE = 'Salva ...';
  rsASSIST_BNCANCEL = '  cancellare';
  rsASSIST_LBNOMETUDE = '  Nome di studio';
  rsASSIST_LBOBSETUDE = '  Commenti studio';
  rsASSIST_LBNOMENTREE = '  portone';
  rsASSIST_LBCOORDS = '  Contatto';
  rsASSIST_LBOBSENTREE = '  Commenti';
  rsASSIST_LBCONVERTISSEUR = '  Converter ...';
  rsASSIST_LBREFSTATION = '  stazione iniziale';
  rsASSIST_LBREFSERIE = '  serie';
  rsASSIST_LBREFPOINT = '  punto';
  rsASSIST_LBCOMMENTAIRE = '  Commenti';
  rsASSIST_LBSYSTGEO = '  Sistemi di coordinate geografiche';
  // ********************************
  // 
  rsVUE2D_TITLE = '';
  rsVUE2D_REPRESENTATION_MODE = '';
  rsVUE2D_REPRESENTATION_NETWORKS = 'Retes';
  rsVUE2D_REPRESENTATION_SECTEURS = 'Settores';
  rsVUE2D_REPRESENTATION_EXPES = '';
  rsVUE2D_REPRESENTATION_GRAY = '';
  rsVUE2D_REPRESENTATION_DEPTH = '';
  rsVUE2D_REPRESENTATION_STATIONS = '';
  rsVUE2D_LOAD_MAILLAGE = '';
  rsVUE2D_DISTANCE = '';
  rsVUE2D_FINDSTATION = '';
  rsVUE2D_ROSE_DIAGRAM = '';
  rsVUE2D_DEPTH_DIAGRAM = '';
  rsVUE2D_METAFILTRE = '';
  rsVUE2D_PANVUE = '';
  rsVUE2D_PARAMETRES_VUE = '';
  rsVUE2D_PRINTING = '';
  rsVUE2D_REFRESH = '';
  rsVUE2D_EXPORT_DXF = 'DXF export';
  rsVUE2D_STATISTIQUES = '';
  rsVUE2D_VUE3D_GDI = '';
  rsVUE2D_VUE3D_OPENGL = '';
  rsVUE2D_ZOOMALL = '';
  rsVUE2D_ZOOMFENETRE = '';
  rsVUE2D_FMT_INFOS_STATION_RAPIDE = 'Punto: %s (%d.%d): X=%.2f Y=%.2f Z=%.2f';
  rsVUE2D_FMT_INFOS_ID_STATION = '%d.%d [%s]';
  rsVUE2D_FMT_INFOS_MESURES = 'L = %.2f, Az = %.2f, P =%.2f, S = %.2f, D = %.2f, H = %.2f, B = %.2f';
  rsVUE2D_FMT_INFOS_COORDONNEES = 'X = %s, Y = %s, Z = %s';
  rsVUE2D_EXPORT_XHTML = '';
  rsVUE2D_EXPORT_SVG = '';
  // *******************************************************
  // Database manager (tabs)
  rsTBS_GENERAL = 'General';
  rsTBS_GENERAL_NOM = 'Nome di rete';
  rsTBS_GENERAL_OBS = '  Commenti';
  rsTBS_GENERAL_SYST = '  Sistema di coordinate';
  rsTBS_ENTRANCE = '  ingressi';
  rsTBS_SECTEURS = '  settori';
  rsTBS_CODES = '  codici';
  rsTBS_TRIPS = '  sessioni';
  rsTBS_SERIES = '  serie';
  rsTBS_RESEAUX = '  Networks';
  rsTBS_ANTENNES = '  Coperto di antenna';
  rsTBS_MAINTENANCE = 'Manutenzione';
  // *******************************************************
  // resourcestring de la section General
  rsLB_NOM_ETUDE = 'Cartella';
  rsLB_COMMENTAIRE_ETUDE = 'Osservaziones';
  rsLB_CODE_EPSG = 'Codice EPSG';
  rsBTN_SELECT_EPSG = 'Selezionare ...';
  rsBTN_CALC_DECLIMAGS = '  Calcolare la variazione magnetica';
  // *******************************************************
  // Entrances frame resourcestring
  rsCDR_ENTR_NOENTRANCE = 'Entrata';
  rsCDR_ENTR_ENTRNAME = '  Nome della entrata';
  rsCDR_ENTR_COORDINATES = 'Coordinates';
  rsCDR_ENTR_STATOFENTR = '  Stazione di riferimento:';
  // *******************************************************
  // Reseaux frame resourcestrings
  rsCDR_RESEAU_LBIDX = 'Numero';
  rsCDR_RESEAU_NAME = 'Nome';
  rsCDR_RESEAU_TYPE = '  Tipo di rete';
  rsCDR_RESEAU_CB0 = 'Cavità naturale';
  rsCDR_RESEAU_CB1 = 'Cavità artificiali';
  rsCDR_RESEAU_CB2 = '  Superficie Topo';
  rsCDR_RESEAU_CB3 = 'Thalweg';
  rsCDR_RESEAU_CB4 = '  Strada o pista';
  rsCDR_RESEAU_CB5 = 'Sentiero';
  rsCDR_RESEAU_CB6 = 'Altro';
  // *******************************************************
  // Sectors frame resourcestring
  rsCDR_SECTEUR_LBIDX = 'Numero';
  rsCDR_SECTEUR_NAME = 'Nome';
  rsCDR_SECTEUR_COLOUR = 'Colore';
  // *******************************************************
  // Expres frame resourcestring
  rsCDR_EXPE_SEANCE = 'Incontro topo';
  rsCDR_EXPE_DATE = 'Data';
  rsCDR_EXPE_DECLIMAG = 'Declinazione';
  rsCDR_EXPE_INCLIN = 'Inclinazione';
  rsCOLOR = 'Colore';
  rsCDR_EXPE_SPELEOMETRE = 'Equipaggio 1';
  rsCDR_EXPE_SPELEOGRAPHE = 'Equipaggio 2';
  // *******************************************************
  // Codes frame resourcestring
  rsCDR_CODES_NUMERO = 'Codice';
  rsCDR_CODES_TYPEGALERIE = '  Tipo di vista';
  rsTITRE_SELECTEUR_VISEE = 'Selectare tipo di vista';
  rsCMBTYPE_D = '  0 - Per difetto';
  rsCMBTYPE_E = '  1 - Entrata';
  rsCMBTYPE_B = '2 - Galleria Fossil';
  rsCMBTYPE_V = '  3 - Rio';
  rsCMBTYPE_W = '  4 - Sifone';
  rsCMBTYPE_C = '  5 - Passage ennoyable';
  rsCMBTYPE_F = '  6 - Punto Fisso';
  rsCMBTYPE_S = '  7 - superficie Topo';
  rsCMBTYPE_A = '  8 - galleria artificiale';
  rsCMBTYPE_M = '  9 - Mining Bonanza';
  rsCDR_CODES_VISEE_HZ = '  Misura orizzontale';
  rsCDR_CODES_VISEE_VT = 'Verticale';
  rsCDR_CODES_VDIRECT = 'Diretto';
  rsCDR_CODES_VINVERSE = '  Reverse';
  rsCDR_CODES_GRADCOMPAS = 'Graduazione della bussola';
  rsCDR_CODES_GRADCLINO = 'Graduazione del inclinometro';
  rsCDR_CODES_CMBUNIT_0 = '401 - Gon (GRA)';
  rsCDR_CODES_CMBUNIT_1 = '  360 – Gradi (DEG)';
  rsCDR_CODES_CMBUNIT_2 = '  370 - Percentuale';
  rsCDR_CODES_CMBUNIT_3 = '  Dislivello - 380';
  rsCDR_CODES_CMBUNIT_4 = '  Costruzione Lasermeter / inclinometro - 800';
  rsCDR_CODES_FACT = '  lunghezze x';
  rsCDR_CODES_POSZERO = '  Posizione zero:';
  rsCDR_CODES_CMBZERO_0 = '  nadir';
  rsCDR_CODES_CMBZERO_1 = '  orizzontale';
  rsCDR_CODES_CMBZERO_2 = '  zenitale';
  rsCDR_CODES_ANGLIMIT = '  Angolo limite:';
  rsCDR_CODES_PRECISION = '  strumenti di precisione';
  // *******************************************************
  // Series frame resourcestring
  rsDLG_PJMNGR_MOVETOSERIE = 'Cambiare serie?';
  rsDLG_PJMNGR_ADDSERIE = '  Aggiungi Series';
  rsCDR_SERIE_LB_RESEAU = 'Rete';
  rsCDR_SERIE_ADDPHOTO = '  Aggiungi una foto';
  rsCDR_SERIE_NUMERO = 'Serie';
  rsCDR_SERIE_NAME = 'Nome';
  rsCDR_SERIE_DEPART = 'Partenza';
  rsCDR_SERIE_ARRIVEE = 'Arrivo';
  rsCDR_SERIE_CHANCE = 'Possibilità';
  rsCDR_SERIE_CHANCE0 = 'Nome';
  rsCDR_SERIE_CHANCE1 = 'Basso';
  rsCDR_SERIE_CHANCE2 = 'Buono';
  rsCDR_SERIE_CHANCE3 = 'Eccellente';
  rsCDR_SERIE_OBSTACLE = 'Ostacolo';
  rsCDR_SERIE_OBSTACLE0 = 'No';
  rsCDR_SERIE_OBSTACLE1 = 'Bene';
  rsCDR_SERIE_OBSTACLE2 = 'Caminetto';
  rsCDR_SERIE_OBSTACLE3 = 'Restringimento';
  rsCDR_SERIE_OBSTACLE4 = 'Lago';
  rsCDR_SERIE_OBSTACLE5 = 'Sifone';
  rsCDR_SERIE_OBSTACLE6 = 'Crollo';
  rsCDR_SERIE_OBSTACLE7 = 'Concrezione';
  rsCDR_SERIE_OBSTACLE8 = 'Sedimento';
  rsCDR_SERIE_OBSTACLE9 = 'Altro';
  // Specifico alla Francia lol
  rsCDR_SERIE_OBSTACLE10 = 'Gas tossici';
  rsCDR_SERIE_OBSTACLE11 = 'Oche aggressivi';
  rsCDR_SERIE_OBSTACLE12 = 'Animali pericolosi';
  rsCDR_SERIE_OBSTACLE13 = 'Sex-club';
  rsCDR_SERIE_LOCKED = 'Bloccato';
  rsCDR_SERIE_INSERTLINE = '  Inserire righe';
  rsCDR_SERIE_NBLINES = '  Numero di linee';
  rsCDR_SERIE_DELETELINE = '  Cancellare le linee';
  rsCDR_SERIE_UNDOCOPY = '  Copia giù';
  rsCDR_SERIE_INC_UNDOCOPY = '  Copia giù incrementale';
  rsCDR_SERIE_IMPLEMENT = 'Convalidare';
  rsINPUT_COMMENTAIRE_TITRE = '  Commenti station';
  rsINPUT_COMMENTAIRE_MSG = '  Inserisci il testo';
  rsCDR_SERIE_PASTE = '  Importa tabella dalla clipboard';
  rsCDR_SERIE_BTN_GRD_COPY = '  Copiare il cancello';
  rsCDR_SERIE_MSG_ERR_ENTREE_NOT_FOUND = 'Entrata non trovata';
  rsCDR_SERIE_MSG_ERR_RESEAU_NOT_FOUND = 'Rete no trovata';
  rsCDR_SERIE_MSG_ERR_CODE_NOT_FOUND = '  Strumenti codice non trovato';
  rsCDR_SERIE_MSG_ERR_EXPE_NOT_FOUND = '  Sessione non trovata';
  rsCDR_SERIE_MSG_ERR_SERIE_NOT_FOUND = '  Serie non trovata';
  rsCDR_SERIE_MSG_ERR_LONG = '  La lunghezza deve essere positivo e minore di %.0f m';
  rsCDR_SERIE_MSG_ERR_LRUD = '  La distanza non deve essere negativo';
  // *******************************************************
  // resoursestring Antenna frame
  rsCDR_ANTENNES_AC_ADDLINE = 'Aggiungere una linea';
  rsCDR_ANTENNES_AC_DELLINE = '  Elimina riga';
  rsCDR_ANTENNES_AC_SAVEGRD = '  Conferma modifiche';
  rsCDR_ANTENNES_DEL_LINE = '  Rimuovere riga %d';
  rsCDR_ANTENNES_FIND_RESEAU = '  Rete per linea %d';
  rsCDR_ANTENNES_FIND_CODE = '  Codice per linea %d';
  rsCDR_ANTENNES_FIND_EXPE = '  Sessione per la linea %d';
  // *******************************************************
  // CdrNavigateurDB frame resourcestring
  rsCDR_NAVIG_DB_DO_SORT = 'Ordinare';
  rsCDR_NAVIG_DB_DO_ADD = '  Aggiungi elemento';
  rsCDR_NAVIG_DB_DO_DELETE = '  Elimina elemento';
  // *********************************
  // VTopo dialog resourcestring
  rsVTOPO_EDPREFIX = 'Stazioni Prefisso';
  rsVTOPO_LBIDPTDEP = '  ID punto di partenza';
  rsVTOPO_LBREPORTER = '  Relazione Operator';
  rsLBFICHIER = '  file';
  rsLBMAINENTRANCE = '  portone';
  rsLBIDMAINST = '  Stazione di partenza';
  rsLBSTPREFIX = '  Stazioni Prefisso';
  rsLBREPORTER = '  rinvio';
  rsLBMAINENTCOORD = ' Coordinate dell ingresso principale';
  rsLBCOLORDEFAULT = '  Colore predefinito';
  // *******************************************************
  // resourcestring du visualisateur OpenGL
  rsOGLVIEWERTITLE = '';
  rsLBLBACKCOLOR = '';
  rsLBLCUBECOLOR = '';
  rsLBLREFCOLOR = '';
  rsLBLREFSIZE = '';
  rsOPENGLERROR = '';
  rsOGLVQUIT = '';
  // *******************************************************
  // resourcestrings de graphic exportator
  rsPSDXF_TITLE = '';
  rsDODISPDESELECTEDPARTS = '';
  rsTAB_LAYERS = '';
  rsTAB_QUADR = '';
  rsTAB_DESSIN = '';
  rsTAB_TEXTES = '';
  rsTYPEQDR = '';
  rsQDNONE = '';
  rsQDCROSS = '';
  rsQDQUADRILLES = '';
  rsGRAPHICS_PS = 'PostScript PS';
  rsGRAPHICS_DXF = 'AutoCAD DXF';
  rsGRAPHICS_SVG = 'Scalable Vector Graphics SVG';
  rsGRAPHICS_WMF = 'Windows MetaFile WMF';
  rsDLGDXF_TITLE = 'DXF export: %s';
  // *******************************************************
  // MetaFiltre keywords
  rsHLPMETAFILTRE = 'METAFILTRE';
  rsMETAFILTRE_NIL = 'NIENTE';
  rsMETAFILTRE_ALL = 'TUTTI';
  rsMETAFILTRE_ID = '  ID';
  rsMETAFILTRE_LENGTH = '  LUNGHEZZA';
  rsMETAFILTRE_AZIMUTH = '  AZIMUT';
  rsMETAFILTRE_PENTE = '  PENDENZA';
  rsMETAFILTRE_DATE = '  DATA';
  rsMETAFILTRE_COLOR = '  COLORE';
  rsMETAFILTRE_X = '  COORD_X';
  rsMETAFILTRE_Y = '  COORD_Y';
  rsMETAFILTRE_Z = '  COORD_Z';
  rsMETAFILTRE_LARGEUR = '  WIDTH';
  rsMETAFILTRE_HAUTEUR = '  ALTEZZA';
  rsMETAFILTRE_DATES = '  DATE';
  rsMETAFILTRE_COLORS = '  COLORI';
  rsMETAFILTRE_SERIE = '  SERIE';
  rsMETAFILTRE_RESEAU = '  RETE';
  rsMETAFILTRE_CODE = '  CODE';
  rsMETAFILTRE_EXPE = '  SESSIONE';
  rsMETAFILTRE_TYPEVISEE = '  TYPE_VISEE';
  rsMETAFILTRE_SECTEUR = '  SETTORE';
  rsMETAFILTRE_APPLY = 'Applicare';
  // *******************************************************
  // Finding stations
  rsMSG_FIND_STATION_TITRE = '';
  rsMSG_FIND_STATION_PROMPT = '';
  // *******************************************************
  // Metafiltre dialog resourcestring
  rsDLGMETAFILTRE_TBS1 = 'Dati';
  rsDLGMETAFILTRE_TBS2 = 'Colori';
  rsDLGMETAFILTRE_PERSO = '';
  // ******************************************************
  // Palette chooser resourcestring
  rsSELECTCOLORTITLE = '';
  rsLBLUSEDCOLORS = '';
  rsDLGCOULSAVEPAL = '';
  rsDLGCOULRESTPAL = '';
  rsDLGCOULFILTERPAL = '';
  rsDLGCOUDELPAL = '';
  rsPALETTENOTFOUNT = '';
  // ******************************************************
  // Graphic export resourcestrings
  rsDLGGRAPHIC_OUTPUTFMT = '';
  rsDLGGRAPHIC_LBFILENAME = '';
  rsDLGGRAPHIC_LBOBS = 'Osservaziones';
  rsDLG_GRAPHIC_TABTITLE = '';
  rsDLGGRAPHIC_GBCROIX = '';
  rsDLGGRAPHIC_GBCHEM = '';
  rsDLGGRAPHIC_GBWALLS = '';
  rsDLGGRAPHIC_SPACING = '';
  rsDLGGRAPHIC_TYPEGRID = '';
  rsDLGGRAPHIC_CMBGRD2 = '';
  rdDLGGRAPHIC_LAYER = '';
  rdDLGGRAPHIC_WALLFILL = '';
  rsDLGGRAPHIC_WFILL1 = '';
  rsDLGGRAPHIC_WFILL2 = '';
  rsDLGGRAPHIC_WFILL3 = '';
  rsDLGGRAPHIC_WFILL4 = '';
  rsDLGGRAPHIC_WFILL5 = '';
  rsDLGGRAPHIC_CHKCHEM = '';
  rsDLGGRAPHIC_CHKSECT = '';
  // ******************************************************
  // Listbox resourcestring
  rsSELECT_LISTE_ENTRANCE = '';
  rsSELECT_LISTE_RESEAU = '';
  rsSELECT_LISTE_SECTEUR = '';
  rsSELECT_LISTE_CODE = '';
  rsSELECT_LISTE_EXPE = '';
  rsSELECT_LISTE_SERIE = '';
  // ******************************************************
  // resourcestring du centre dimpression
  rsPRN_NOPRINTER = '';
  rsPRN_TBPRINTER = '';
  rsPRN_TBDRAW = '';
  rsPRN_TITLE = '';
  rsPRN_CHKPOLY = '';
  rsPRN_CHKFILL = '';
  rsPRN_CHKWALLS = '';
  rsPRN_CHKSECTS = '';
  rsPRN_CHKSTATIONS = '';
  rsPRN_CHKSTATIONS_LBL = '';
  rsPRN_CHKALTITUDE = '';
  rsPRN_CHKCOTE = '';
  rsPRN_CHKQUADRILLAGE = '';
  rsPRN_CHKENTREES = '';
  rsPRN_CHKANNOTATIONS = '';
  rsPRM_LBANGLEROT = '';
  rsPRN_TYPEQDR = '';
  rsPRN_QDNONE = '';
  rsPRN_QDCROSS = '';
  rsPRN_QDQUADRILLES = '';
  rsPRN_QDPOINTS = '';
  rsPRN_SCALING = '';
  rsPRN_LBSPACING = '';
  rsLANDSCAPE = '';
  rsPORTRAIT = '';
  rsPRN_START_PRINTING = '';
  rsPRN_PRINTING_DONE = '';
  rsDLGIMP_TAB1 = '';
  rsDLGIMP_TAB2 = '';
  rsDLGIMP_TAB3 = '';
  rsDLGIMP_TAB4 = '';
  rsQDRSPACING = '';
  rsECHELLE = '';
  rsLAYERS = '';
  rsPREVIEW = '';
  rsSTARTPRINTING = '';
  rsREGLE = '';
  // -------------------------------------------------
  rsHLP_TITRE = '';
  rsHLP_BNEDIT = '';
  rsHLP_DOCONTINUE = '';
  rsHLP_TAB_RUBRIQUES = '';
  rsHLP_TAB_ABOUT = '';
  // -------------------------------------------------
  // resourcestring Serie/station dialog
  rsDLG_SERST_TITLE = '';
  rsDLG_SERST_SERIE = '';
  rsDLG_SERST_STATION = '';
  rsDLG_SERST_CLE = '';
  rsDLG_SERST_LBSEARCH = '';
  rsDLG_SERST_BYSERST = '';
  rsDLG_SERST_BYREFTER = '';
  // -------------------------------------------------
  // Annotations dialog resourcestrings
  rsDLG_ANN_TITLE = '';
  rsDLG_ANN_LBTEXTE = '';
  rsDLG_ANN_LBMAX = '';
  rsDLG_ANN_CHKTXTDISP = '';
  rsDLG_ANN_GRBPOSTEXT = '';
  rsDLG_ANN_GRBMETH0 = '';
  rsDLG_ANN_GRBMETH1 = '';
  rsDLG_ANN_LBPOSTEXTE = '';
  rsDLG_ANN_LBSTATION = '';
  rsDLG_ANN_LBOFFSET = '';
  rsDLG_ANN_GRBATTRTXT = '';
  rsDLG_ANN_GRBBASEPT = '';
  // -------------------------------------------------
  // Buscar por etiquete de punto
  rsDLG_FIND_PT_BY_ID_TITLE = '';
  rsDLG_FIND_PT_BY_ID_PROMPT = '';
  rsDLG_FIND_PROMPT = '';
  rsDLG_FIND_ENTRANCE_BY_TEXT = '';
  rsDLG_FIND_RESEAU_BY_TEXT = '';
  rsDLG_FIND_CODE_BY_TEXT = '';
  rsDLG_FIND_EXPE_BY_TEXT = '';
  rsDLG_FIND_SERIE_BY_TEXT = '';
  rsDLG_FIND_SECTEUR_BY_TEXT = '';
  // Aceso por número
  rsDLG_GOTO_PROMPT = '';
  rsDLG_GOTO_ENTRANCE_BY_NUMERO = '';
  rsDLG_GOTO_RESEAU_BY_NUMERO = '';
  rsDLG_GOTO_SECTEUR_BY_NUMERO = '';
  rsDLG_GOTO_CODE_BY_NUMERO = '';
  rsDLG_GOTO_EXPE_BY_NUMERO = '';
  rsDLG_GOTO_SERIE_BY_NUMERO = '';
  // Calculadora
  rsDLG_CALC_TITLE = '';
  rsDLG_CALC_TAB_CAL = '';
  rsDLG_CALC_EXPR = '';
  rsDLG_CALC_DOCALC = '';
  rsDLG_CALC_CDR_CONVERT = '';
  rsDLG_CALC_CDR_DECLIMAG = '';
  rsDLG_CALC_BTN_CONVERT = '';
  rsDLG_CALC_BTN_ECHANGE = '';
  rsDLG_CALC_LB_SYST_SOURCE = '';
  rsDLG_CALC_LB_SYST_CIBLE = '';
  rsDLG_CALC_HINT_GRD_CONVERSIONS = '';
  rsDLG_CALC_EXPRESSION = '';
  rsDLG_CALC_SYSCOORDS = '';
  rsDLG_CALC_LSB_FUNCTIONS = '';
  rsDLG_CALC_COORDS_UNITAIRE = '';
  rsDLG_CALC_COORDS_TABLEAU = '';
  rsDLG_CALC_DECL_UNITAIRE = '';
  rsDLG_CALC_DECL_TABLEAU = '';
  // Clipboard management
  rsDLG_CLIPBRD_PTS_TITLE = '';
  // Mensajes
  rsINSERTITEM = '';
  rsDELETEITEM = '';
  rsONCLOSEPRJMNGR = '';
  rsMSG_SEEALSO = '';
  rsMSG_NDSNEEDED = '';
  rsMSG_SAVECHANGES = '';
  rsMSG_ERASESTATION = '';
  rsMSG_FILENOTFOUND = '';
  rsMSG_READY = '';
  rsMSG_NOFILEOPENED = '';
  rsDISPLAY_HELP_SYSTEM = '';
  rsHLPCENTER_TITLE = '';
  rsMATCHNOTFOUND = '';
  rsNOCANCLOSE = '';
  rsWARN_FILEALREADYOPEN = '';
  rsSAVECHANGES = '';
  rsNOCANCLOSEWND = '';
  rsERASEEXISTNAMEDFILE = '';
  rsSAVESLOST = '';
  rsCFGMTFLTR_UNABLE = '';
  rsERRORSTARTPRNCTR = '';
  rsNOPRINTERINSTALLED = '';
  rsERRORLOADINGTOP = '';
  rsIDSERIE_EXISTS = '';
  // Mensajes de error
  rsMSG_VUE3D_FAIL = 'Impossibile iniziare visualizzatore 3D';
  rsMSG_VUE2D_FAIL = '  Impossibile iniziare map viewer 2D';
  rsMSG_STATS_DLG_FAIL = '  Impossibile iniziare finestra di statistiche';
  rsMSG_PRINT_CENTER_FAIL = '  Impossibile iniziare centro stampa';
  rsMSG_PROJ_MANAGER_FAIL = '  Impossibile database di partenza anteriore';
  rsMSG_PROJ4S_FAIL = '  Impossibile iniziare Proj4s coordinate convertitore';
  rsMSG_DECLIMAG_FAIL = 'Impossibile iniziare calcolatrice Declinazioni IGRF magnetica';
  rsMSG_TABLE_ENTITY_FAIL = '  Tavolo enti Errore durante il caricamento';
  rsMSG_CODE_NOT_FOUND = '';
  rsMSG_EXPE_NOT_FOUND = '';
  rsMSG_ANGLE_OUT_OF_RANGE = '';
  rsMSG_WARN_DELETE_ITEM = '';
  // messages in TToporobotStructure.LoadFichierTab()
  rsRD_TAB_MSG_ERR = '';
  rsRD_TAB_D_MULTI_OBS = '';
  rsRD_TAB_F_MULTI_OBS = '';
  rsRD_TAB_STOP_9999 = '';
  rsRD_TAB_LN_OBS = '';
  rsRD_TAB_LASTSAVES = '';
  rsRD_TAB_CLASSEURS = '';
  rsRD_TAB_ENTRANCE = '';
  rsRD_TAB_ENTR_NOGEOREF = '';
  rsRD_TAB_ENTR_BADLINK = '';
  rsRD_TAB_IGNORED_SEC = '';
  rsRD_TAB_BAD_TRIP = '';
  rsRD_TAB_BAD_CODE = '';
  rsRD_TAB_BAD_DATE = '';
  rsRD_TAB_BAD_IDXRES = '';
  rsRD_TAB_IDEXTR_MM = '';
  rsRD_TAB_SELF_CLOSURE = '';
  rsRD_TAB_NEG_LONG = '';
  rsRD_ERROR_LN = '';
  rsRD_CONTNT_LN = '';
  rsRD_TAB_FIELD_VALUES = '';
  rsRD_TAB_NOFATAL_ERR = '';
  rsRD_TAB_WTFATAL_ERR = '';
  // *************************************************************
  // Compiler resourcestring
  rsDEFAULT = 'Default';
  rsFORFIXEDPOINTS = 'Item for entrances and Fixed points';
  rsWARNINGENTRYADDED = 'Number entrances in -6 and -5 sections are differents - Corrected';
  rsCALCULNODES = 'CALCULATING NODES COORDINATES';
  rsLINEOFNB = '---> Line %d / %d';
  rsBINDMATRIX = 'Incidence Matrix';
  rsNB_NON_NUL_TERMES = '%d not null terms in R Matrix';
  rsPR100_NON_NULS = 'i.e: %.3f%% of (%d x %d) = %d terms';
  rsFINDING_BRANCHES = 'Finding branches';
  rsADDENTRANCES = 'Adding cave entrances';
  rsBUILDMATRICE = 'BUILDING COMPENSATION MATRIX';
  rsFIND_SUM_LIMITS = 'Finding not-zero limits';
  rsFACTORISEMATRICE = 'FACTORIZING COMPENSATION MATRIX';
  rsAXIS = 'Axis :';
  rsDESCENTE = '--> Down system: V.V* = A';
  rsREMONTEE = '--> Up system: Equation Unknowns found';
  rsTRIANGULARISATION = '--> Triangularisation';
  rs2NDMEMBER = '- Second member';
  rsCOMPESMATRIX = '- Compensation matrix';
  rsNODECOORD = '- Factorization: Nodes coordinates';
  rsCOORDONNEES_OK = 'Coordinates of the %d nodes OK';
  rsDEL_TEMP_MATRIX = '--> Deleting temporary matrices';
  rsWRITE_NODES_COORDS = 'Writing nodes coordinates in:';
  rsNODES_COORDS_FOR_DB = 'Nodes coordinates for:';
  rsCALCULCONTOURS = 'CALCULATING VOLUMES';
  rsCALCUL_ANTENNES = 'Calculating antenna shots';
  rsRECENSEM_JONC = 'Finding Junctions';
  rsPURGE_TABLE_JONC = 'Clear jonctions table';
  rsPURGE_TABLE_BRCH = 'Clear branches tables';
  rsNB_BRCHS = '%d branches';
  rsSTEP_CALC_01 = '->Step %d / %d: Connectivity matrix R';
  rsSTEP_CALC_02 = '->Step %d / %d: Weighting matrix W';
  rsSTEP_CALC_03 = '->Step %d / %d: Compensation matrix B = Rt.W.R';
  rsSTEP_CALC_04 = '->Step %d / %d: Nodes coordinates';
  rsSTEP_CALC_05 = '->Step %d / %d: Adjust deviations';
  rsSTEP_CALC_06 = '->Step %d / %d: Memory free and ending processing';
  rsFREE_TEMP_VARS = 'DELETING TEMPORARY VARIABLES';
  rsREPARTIR_ECARTS = 'ADJUST DEVIATIONS';
  rsSCAN_BRCHS = 'Scanning branches';
  // reading files
  rsSEPARATOR_LINE = '-------------------------------';
  rsWARNINGS_READFILE = 'Warnings in reading file:';
  rsCONV_TAB_MAC_UNIX = '';
  // -------------------------------------------------
  // Tipos de miras
  rsTYPE_VISEE_DEFAULT = 'Guasto';
  rsTYPE_VISEE_ENTRANCE = 'Entrata';
  rsTYPE_VISEE_FOSSILE = 'Fossile';
  rsTYPE_VISEE_VADOSE = 'Fiume';
  rsTYPE_VISEE_ENNOYABLE = 'Inondazione';
  rsTYPE_VISEE_SIPHON = 'Sifone';
  rsTYPE_VISEE_FIXPOINT = 'Punto fisso';
  rsTYPE_VISEE_SURFACE = '  Riferito vincolante';
  rsTYPE_VISEE_TUNNEL = 'Galleria artificiale';
  rsTYPE_VISEE_MINE = 'Galleria mineraria';
  rsTYPE_VISEE_ANTENNE = 'Antenna';
  // -------------------------------------------------
  // Misc
  rsSELECTALL = '';
  rsDESELECTALL = '';
  rsOBS = '';
  rsFILTERS = '';
  rsCHKSELECTALL = '';
  rsCHKDESELECTALL = '';
  rsCHKREVERSE = '';
  rsMAIN_NETWORK = '';
  // -------------------------------------------------
  // dlgSelectDansListes
  rsSELECT_LISTE_NB_ELEMENTS = '';
  rsSELECT_LISTE_ENTREES = '';
  rsSELECT_LISTE_ENTREES_ID = 'ID';
  rsSELECT_LISTE_ENTREES_NOM = '';
  rsSELECT_LISTE_ENTREES_REF = 'Ref.';
  rsSELECT_LISTE_ENTREES_X = 'X';
  rsSELECT_LISTE_ENTREES_Y = 'Y';
  rsSELECT_LISTE_ENTREES_Z = 'Z';
  rsSELECT_LISTE_RESEAUX = 'Retes';
  rsSELECT_LISTE_RESEAUX_ID = 'ID';
  rsSELECT_LISTE_RESEAUX_COLOR = 'Colore';
  rsSELECT_LISTE_RESEAUX_NOM = 'Rete';
  rsSELECT_LISTE_SECTEURS = '';
  rsSELECT_LISTE_SECTEURS_ID = 'ID';
  rsSELECT_LISTE_SECTEURS_COLOR = 'Colore';
  rsSELECT_LISTE_SECTEURS_NOM = '';
  rsSELECT_LISTE_CODES = 'Codices';
  rsSELECT_LISTE_CODES_ID = 'ID';
  rsSELECT_LISTE_CODES_AZIMUTS = '';
  rsSELECT_LISTE_CODES_PENTES = '';
  rsSELECT_LISTE_CODES_OBS = '';
  rsSELECT_LISTE_EXPES = 'Sessiones';
  rsSELECT_LISTE_EXPES_ID = 'ID';
  rsSELECT_LISTE_EXPES_COULEUR = 'Colore';
  rsSELECT_LISTE_EXPES_DATE = '';
  rsSELECT_LISTE_EXPES_SPELEOMETRE = '';
  rsSELECT_LISTE_EXPES_SPELEOGRAPHE = '';
  rsSELECT_LISTE_EXPES_DECLINAISON = '';
  rsSELECT_LISTE_EXPES_OBS = '';
  rsSELECT_LISTE_SERIES = 'Series';
  rsSELECT_LISTE_SERIES_ID = 'ID';
  rsSELECT_LISTE_SERIES_DEPART = '';
  rsSELECT_LISTE_SERIES_ARRIVEE = '';
  rsSELECT_LISTE_SERIES_NOM = '';
  rsSELECT_LISTE_SERIES_RESEAU = '';
  rsSELECT_LISTE_SERIES_NBPOINTS = '';
  rsSELECT_LISTE_DATES = '';
  rsSELECT_LISTE_UNE_DATE = '';
  rsSELECT_LISTE_STATIONS = '';
  rsSELECT_LISTE_STATIONS_ID = '';
  rsSELECT_LISTE_STATIONS_NB = '';
  rsSELECT_LISTE_STATIONS_LABEL = '';
  rsSELECT_LISTE_STATIONS_X = '';
  rsSELECT_LISTE_STATIONS_Y = '';
  rsSELECT_LISTE_STATIONS_Z = '';
  // -------------------------------------------------
  // dlgExportVersSIGExt
  rsEXPORT_SIG_TITRE = '';
  rsEXPORT_SIG_SYST_COORDONNEES = '';
  rsEXPORT_SIG_SIG_CIBLE = '';
  rsEXPORT_SIG_LB_FICHIER = '';
  rsEXPORT_SIG_USE_COLORS = '';
  rsEXPORT_SIG_SILHOUETTE = '';
  rsEXPORT_SIG_BTN_GO = '';
  rsEXPORT_SIG_BTN_PREFIX = '';
  // -------------------------------------------------
  // dlgStatistiquesExt
  rsSTATISTIQUES_TITRE = '';
  rsSTATISTIQUES_TAB_TABLEAUX = '';
  rsSTATISTIQUES_TAB_SYNTHESE = '';
  rsSTATISTIQUES_TAB_DIAGRAMMES = '';
  rsSTATISTIQUES_TAB_CORDONNEES = '';
  rsSTATISTIQUES_CMB_MODE_TABLE0 = '';
  rsSTATISTIQUES_CMB_MODE_TABLE1 = '';
  rsSTATISTIQUES_CMB_MODE_TABLE2 = '';
  rsSTATISTIQUES_CMB_MODE_TABLE3 = '';
  rsSTATISTIQUES_CMB_MODE_TABLE4 = '';
  rsSTATISTIQUES_NB_RESEAUX = '%d retes';
  rsSTATISTIQUES_NB_SECTEURS = '%d sectores';
  rsSTATISTIQUES_NB_CODES = '%d codices';
  rsSTATISTIQUES_NB_SEANCES = '%d seziones';
  rsSTATISTIQUES_NB_DATES = '%d datas';
  rsSTATISTIQUES_TYPE_SHOT_FOSSILES = '';
  rsSTATISTIQUES_TYPE_SHOT_VADOSES = '';
  rsSTATISTIQUES_TYPE_SHOT_ENNOYABLES = '';
  rsSTATISTIQUES_TYPE_SHOT_SIPHONS = '';
  rsSTATISTIQUES_TYPE_SHOT_NATURELS = '';
  rsSTATISTIQUES_TYPE_SHOT_SPECIAUX = '';
  rsSTATISTIQUES_TYPE_SHOT_TUNNELS = '';
  rsSTATISTIQUES_TYPE_SHOT_MINES = '';
  rsSTATISTIQUES_COPY_TABLEAU = '';
//================================================================================
implementation

end.
