unit UnitMessages_de;
  // Fichier UnitMessages_de.pas cree le 09/05/2014 a 19:28:36
  // Genere par les macros du classeur: file:///C:/0_Logiciels_JPC/0_GHTopoFPC_20140503/Outils_Calc/GestionResourceStrings.ods
  //********************************************************************************
  // /!\ DO NOT MODIFY - Generated automatically
  // Please use spreadsheet utility for modification of strings
  // Language: DE - Deutsch
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
  // Programm Namen
  rsGHTOPOEXENAME = 'GHTopo';
  rsMAINMENUCAPTION = '%s - %s';
  rsGHTOPOVERSION = 'Version 3.1415926 du %.2d/%.2d/%.4d';
  rsGHTOPOLICENSE = 'Programm unter GPL Licenz';
  rsGHTOPOAUTHOR = '(c) 1989..%d Jean Pierre CASSOU';
  // 
  rsGHTOPORESOL = 'Resolution %d x %d';
  rsGHTOPOPlatFormLinux = 'Plattform: Linux (Deutsch)';
  rsGHTOPOPlatFormWin32 = 'Plattform: Microsoft Windows (Deutsch)';
  // 
  rsCHOOSELANGUAGE = '';
  // 
  rsENDOFHADES = 'Ende GHTopo';
  // *********************************************
  // Common labels resourcestrings
  rsLBL_COMMENTS = 'Kommentare';
  rsSERIE_INITIALISATION = '';
  // Filtros de fichero
  rsFILEFILTER_ALL = 'Alles (*.*)|*.*';
  rsGHTOPO_FILE_FILTER = 'GHTopo (*.xtb)|*.xtb|LimeLight Tab (*.Tab)|*.Tab|GHTopo XML (*.gtx)|*.gtx|Alles (*.*)|*.*';
  rsKML_FILE_FILTER = 'Google Earth (*.kml)|*.xtb|Alles (*.*)|*.*';
  rsGCD_FILE_FILTER = 'GHCaveDraw centerlines (*.gcd)|*.gcd|Alles (*.*)|*.*';
  rsGPX_FILE_FILTER = 'Ficheros GPS (*.gpx)|*.gpx|Alles (*.*)|*.*';
  rsOSM_FILE_FILTER = 'OpenStreetMap (*.osm)|*.osm|Alles (*.*)|*.*';
  rsCSV_FILE_FILTER = 'Tabulated text (*.csv)|*.csv|Alles (*.*)|*.*';
  rsVTOPO_FILE_FILTER = 'Visual Topo (*.tro)|*.tro|Alles (*.*)|*.*';
  rsTHERION_FILE_FILTER = 'Therion centerlines (*.th)|*.th|Alles (*.*)|*.*';
  // *********************************************
  // Main menu resourcestring
  rsMSGASSIGNCAPTIONS = '';
  rsCALCURAPIDE = '';
  rsMNU_FILE = '&Datei';
  rsNEW = '&Neu';
  rsOPEN = '';
  rsSAVE = '';
  rsSAVEAS = '';
  rsMNU_SAVELOG = '';
  rsCLOSE = '';
  rsRECENT_DOCS = '';
  rsRELOAD = '';
  rsEDITTAB = '';
  rsPRINT = 'Drücken';
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
  rsMNU_TOPOGRAPHIE = '&Topografie';
  rsCHECKBASE = '';
  rsCOMPILE = '';
  rsVUEPLAN = '';
  rsVUE3D = '';
  rsRENDU3D = '';
  rsSTATISTIQUES = '';
  rsINFOCAVITE = '';
  rsNODESCOORDINATES = '';
  rsMNU_WINDOW = '&Fenster';
  rsMNU_STAY_ON_TOP = '';
  rsWND_DATABASE = 'Datenbasis';
  rsWND_PLAN = 'Karte anzeigen';
  rsWND_CONSOLE = '&Konsoll';
  rsMNU_TOOLS = 'Werkzeuge';
  rsMNU_HELP = '&Hilfe';
  rsHLPINDEX = 'Index';
  rsHLPNEWS = 'Nachrichten';
  rsABOUT = 'Über GHTopo';
  // Wizard resourcestrings
  rsASSISTANT_SUCCESS = 'Das Dokument wurde erfolgreich von Assistenten erstellt';
  rsASSISTANT_RELOAD = 'Nachladen Dokument';
  rsASSISTANT_ECHEC = 'Assistenten fehlgeschlagen.';
  rsASSIST_TITLE = 'Neue Höhle Assistenten';
  rsASSIST_BNGENERAL = 'General';
  rsASSIST_BNENTRANCES = '1. Eintrag';
  rsASSIST_BNCODES = '1. Code';
  rsASSIST_BNEXPES = '1. Sitzung';
  rsASSIST_BNSERIE = '1.Serie';
  rsASSIST_BNSAVE = 'Speichern ...';
  rsASSIST_BNCANCEL = ' Stornieren';
  rsASSIST_LBNOMETUDE = ' Name der Studie';
  rsASSIST_LBOBSETUDE = ' Kommentare Studie';
  rsASSIST_LBNOMENTREE = ' Haupteingang';
  rsASSIST_LBCOORDS = ' Kontakt';
  rsASSIST_LBOBSENTREE = ' Kommentare';
  rsASSIST_LBCONVERTISSEUR = ' Converter ...';
  rsASSIST_LBREFSTATION = ' Erste Station';
  rsASSIST_LBREFSERIE = ' Serie';
  rsASSIST_LBREFPOINT = ' Punkt';
  rsASSIST_LBCOMMENTAIRE = ' Kommentare';
  rsASSIST_LBSYSTGEO = ' Geographische Koordinatensysteme';
  // ********************************
  // 
  rsVUE2D_TITLE = '';
  rsVUE2D_REPRESENTATION_MODE = '';
  rsVUE2D_REPRESENTATION_NETWORKS = 'Netwerker';
  rsVUE2D_REPRESENTATION_SECTEURS = '';
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
  rsVUE2D_FMT_INFOS_STATION_RAPIDE = 'Punkt: %s (%d.%d): X=%.2f Y=%.2f Z=%.2f';
  rsVUE2D_FMT_INFOS_ID_STATION = '%d.%d [%s]';
  rsVUE2D_FMT_INFOS_MESURES = 'L = %.2f, Az = %.2f, P =%.2f, L = %.2f, R = %.2f, T = %.2f, N = %.2f';
  rsVUE2D_FMT_INFOS_COORDONNEES = 'X = %s, Y = %s, Z = %s';
  rsVUE2D_EXPORT_XHTML = '';
  rsVUE2D_EXPORT_SVG = '';
  // *******************************************************
  // Database manager (tabs)
  rsTBS_GENERAL = 'General';
  rsTBS_GENERAL_NOM = 'Netzwerkname';
  rsTBS_GENERAL_OBS = '  Kommentare';
  rsTBS_GENERAL_SYST = '  Koordinatensystem';
  rsTBS_ENTRANCE = '  Eingänge';
  rsTBS_SECTEURS = '  Branchen';
  rsTBS_CODES = '  Codes';
  rsTBS_TRIPS = '  Sessions';
  rsTBS_SERIES = '  Serie';
  rsTBS_RESEAUX = 'Netzwerks';
  rsTBS_ANTENNES = '  Antenne Dachte';
  rsTBS_MAINTENANCE = '  Wartung';
  // *******************************************************
  // resourcestring de la section General
  rsLB_NOM_ETUDE = 'Mappe';
  rsLB_COMMENTAIRE_ETUDE = 'Kommentare';
  rsLB_CODE_EPSG = 'EPSG Code';
  rsBTN_SELECT_EPSG = 'Wählen Sie ...';
  rsBTN_CALC_DECLIMAGS = '  Berechnen Sie die magnetische Abweichung';
  // *******************************************************
  // Entrances frame resourcestring
  rsCDR_ENTR_NOENTRANCE = 'Eintrag';
  rsCDR_ENTR_ENTRNAME = '  Name der Eintrag';
  rsCDR_ENTR_COORDINATES = '  Koordinaten';
  rsCDR_ENTR_STATOFENTR = '  Referenzstation:';
  // *******************************************************
  // Reseaux frame resourcestrings
  rsCDR_RESEAU_LBIDX = '  Anzahl';
  rsCDR_RESEAU_NAME = '  Name';
  rsCDR_RESEAU_TYPE = '  Netzwerktyp';
  rsCDR_RESEAU_CB0 = '  natürlichen Hohlraum';
  rsCDR_RESEAU_CB1 = '  künstliche Hohlraum';
  rsCDR_RESEAU_CB2 = '  Topo Oberfläche';
  rsCDR_RESEAU_CB3 = '  Thalweg';
  rsCDR_RESEAU_CB4 = '  Straße oder Piste';
  rsCDR_RESEAU_CB5 = '  Spur';
  rsCDR_RESEAU_CB6 = 'Andere';
  // *******************************************************
  // Sectors frame resourcestring
  rsCDR_SECTEUR_LBIDX = '  Anzahl';
  rsCDR_SECTEUR_NAME = 'Name';
  rsCDR_SECTEUR_COLOUR = 'Farbe';
  // *******************************************************
  // Expres frame resourcestring
  rsCDR_EXPE_SEANCE = 'Treffen topo';
  rsCDR_EXPE_DATE = '  Datum';
  rsCDR_EXPE_DECLIMAG = '  Deklination';
  rsCDR_EXPE_INCLIN = '  Tilt';
  rsCOLOR = 'Farbe';
  rsCDR_EXPE_SPELEOMETRE = 'Crewmember 1';
  rsCDR_EXPE_SPELEOGRAPHE = 'Crewmember 2';
  // *******************************************************
  // Codes frame resourcestring
  rsCDR_CODES_NUMERO = 'Code';
  rsCDR_CODES_TYPEGALERIE = ' Art der Sehenswürdigkeit';
  rsTITRE_SELECTEUR_VISEE = ' Die Wahl der Art der Ziel';
  rsCMBTYPE_D = ' 0 - Standard';
  rsCMBTYPE_E = ' 1 - Einstieg';
  rsCMBTYPE_B = ' 2 - Fossil Galerie';
  rsCMBTYPE_V = ' 3 - Free Flow';
  rsCMBTYPE_W = ' 4 - Sifon';
  rsCMBTYPE_C = ' 5 - Passage ennoyable';
  rsCMBTYPE_F = ' 6 - Fixed Point';
  rsCMBTYPE_S = ' 7 - Topo Oberfläche';
  rsCMBTYPE_A = ' 8 - Künstliche Tunnel';
  rsCMBTYPE_M = ' 9 - Bergbau Galerie';
  rsCDR_CODES_VISEE_HZ = ' Horizontale Messung';
  rsCDR_CODES_VISEE_VT = ' Vertikal';
  rsCDR_CODES_VDIRECT = ' Direkt';
  rsCDR_CODES_VINVERSE = ' Umge';
  rsCDR_CODES_GRADCOMPAS = ' Graduation Kompass';
  rsCDR_CODES_GRADCLINO = ' Graduation Neigungsmesser';
  rsCDR_CODES_CMBUNIT_0 = '402 - Gon (GRA)';
  rsCDR_CODES_CMBUNIT_1 = ' 360 - Grad (DEG)';
  rsCDR_CODES_CMBUNIT_2 = ' 370 - Prozent';
  rsCDR_CODES_CMBUNIT_3 = ' 380 - Höhenunterschied';
  rsCDR_CODES_CMBUNIT_4 = ' 800 - Lasermeter / Neigungsmesser Gebäude';
  rsCDR_CODES_FACT = ' Längen x';
  rsCDR_CODES_POSZERO = ' Nullstellung:';
  rsCDR_CODES_CMBZERO_0 = ' Nadir';
  rsCDR_CODES_CMBZERO_1 = ' Horizontal';
  rsCDR_CODES_CMBZERO_2 = ' Zenithal';
  rsCDR_CODES_ANGLIMIT = ' Beschränken Winkel:';
  rsCDR_CODES_PRECISION = ' Präzisionsinstrumente';
  // *******************************************************
  // Series frame resourcestring
  rsDLG_PJMNGR_MOVETOSERIE = 'Ändern Serie?';
  rsDLG_PJMNGR_ADDSERIE = '  Serie';
  rsCDR_SERIE_LB_RESEAU = '  Netzwerk';
  rsCDR_SERIE_ADDPHOTO = '  Fügen Sie ein Foto';
  rsCDR_SERIE_NUMERO = '  Serie';
  rsCDR_SERIE_NAME = '  Name';
  rsCDR_SERIE_DEPART = '  Abfahrt';
  rsCDR_SERIE_ARRIVEE = '  Ankunft';
  rsCDR_SERIE_CHANCE = '  Chance';
  rsCDR_SERIE_CHANCE0 = '  nicht';
  rsCDR_SERIE_CHANCE1 = '  niedrig';
  rsCDR_SERIE_CHANCE2 = '  gut';
  rsCDR_SERIE_CHANCE3 = '  ausgezeichnet';
  rsCDR_SERIE_OBSTACLE = '  Hindernis';
  rsCDR_SERIE_OBSTACLE0 = '  nicht';
  rsCDR_SERIE_OBSTACLE1 = '  gut';
  rsCDR_SERIE_OBSTACLE2 = '  Kamin';
  rsCDR_SERIE_OBSTACLE3 = '  Einengung';
  rsCDR_SERIE_OBSTACLE4 = '  See';
  rsCDR_SERIE_OBSTACLE5 = '  Siphon';
  rsCDR_SERIE_OBSTACLE6 = '  Zusammenbruch';
  rsCDR_SERIE_OBSTACLE7 = '  Konkretion';
  rsCDR_SERIE_OBSTACLE8 = '  Sediment';
  rsCDR_SERIE_OBSTACLE9 = '  Andere';
  //   Spezifisch für Frankreich lol
  rsCDR_SERIE_OBSTACLE10 = '  Giftige Gase';
  rsCDR_SERIE_OBSTACLE11 = '  Aggressive Gänse';
  rsCDR_SERIE_OBSTACLE12 = '  Gefährliche Tiere';
  rsCDR_SERIE_OBSTACLE13 = '  Baisodrome';
  rsCDR_SERIE_LOCKED = '  Verschlossen';
  rsCDR_SERIE_INSERTLINE = '  Zeilen einfügen';
  rsCDR_SERIE_NBLINES = '  Anzahl Zeilen';
  rsCDR_SERIE_DELETELINE = '  Löschleitungen';
  rsCDR_SERIE_UNDOCOPY = '  Kopieren nach unten';
  rsCDR_SERIE_INC_UNDOCOPY = '  Kopieren Sie unten inkrementelle';
  rsCDR_SERIE_IMPLEMENT = '  Bestätigen';
  rsINPUT_COMMENTAIRE_TITRE = '  Kommentare Station';
  rsINPUT_COMMENTAIRE_MSG = '  Geben Sie den Text';
  rsCDR_SERIE_PASTE = '  Import-Tabelle aus der Zwischenablage';
  rsCDR_SERIE_BTN_GRD_COPY = '  Kopieren Sie die Gate';
  rsCDR_SERIE_MSG_ERR_ENTREE_NOT_FOUND = '  Eintrag nicht gefunden';
  rsCDR_SERIE_MSG_ERR_RESEAU_NOT_FOUND = '  Netzwerk gefunden';
  rsCDR_SERIE_MSG_ERR_CODE_NOT_FOUND = '  Instrumente gefunden Code';
  rsCDR_SERIE_MSG_ERR_EXPE_NOT_FOUND = '  Session nicht gefunden topo';
  rsCDR_SERIE_MSG_ERR_SERIE_NOT_FOUND = '  Serie gefunden';
  rsCDR_SERIE_MSG_ERR_LONG = '  Die Länge muss positiv sein und weniger als %.0f m';
  rsCDR_SERIE_MSG_ERR_LRUD = '  Der Abstand sollte nicht negativ sein';
  // *******************************************************
  // resoursestring Antenna frame
  rsCDR_ANTENNES_AC_ADDLINE = 'In einer Linie';
  rsCDR_ANTENNES_AC_DELLINE = '  Zeile löschen';
  rsCDR_ANTENNES_AC_SAVEGRD = '  Commit Changes';
  rsCDR_ANTENNES_DEL_LINE = '  Zeile löschen%d';
  rsCDR_ANTENNES_FIND_RESEAU = '  Netzwerk für die Linie%d';
  rsCDR_ANTENNES_FIND_CODE = '  Code für die Linie%d';
  rsCDR_ANTENNES_FIND_EXPE = '  Erfahrung für die Linie %d';
  // *******************************************************
  // CdrNavigateurDB frame resourcestring
  rsCDR_NAVIG_DB_DO_SORT = 'Art';
  rsCDR_NAVIG_DB_DO_ADD = '  Element hinzufügen';
  rsCDR_NAVIG_DB_DO_DELETE = '  Artikel löschen';
  // *********************************
  // VTopo dialog resourcestring
  rsVTOPO_EDPREFIX = '';
  rsVTOPO_LBIDPTDEP = '';
  rsVTOPO_LBREPORTER = '';
  rsLBFICHIER = 'Datei';
  rsLBMAINENTRANCE = '';
  rsLBIDMAINST = '';
  rsLBSTPREFIX = '';
  rsLBREPORTER = '';
  rsLBMAINENTCOORD = '';
  rsLBCOLORDEFAULT = '';
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
  rsMETAFILTRE_NIL = 'NICHT';
  rsMETAFILTRE_ALL = '  ALL';
  rsMETAFILTRE_ID = 'ID';
  rsMETAFILTRE_LENGTH = '  LANGE';
  rsMETAFILTRE_AZIMUTH = '  AZIMUTH';
  rsMETAFILTRE_PENTE = '  SLOPE';
  rsMETAFILTRE_DATE = '  DATUM';
  rsMETAFILTRE_COLOR = '  COLOR';
  rsMETAFILTRE_X = '  COORD_X';
  rsMETAFILTRE_Y = '  COORD_Y';
  rsMETAFILTRE_Z = '  COORD_Z';
  rsMETAFILTRE_LARGEUR = '  BREITE';
  rsMETAFILTRE_HAUTEUR = '  HÖHE';
  rsMETAFILTRE_DATES = '  TERMINE';
  rsMETAFILTRE_COLORS = '  FARBEN';
  rsMETAFILTRE_SERIE = '  SERIE';
  rsMETAFILTRE_RESEAU = '  NETZWERK';
  rsMETAFILTRE_CODE = '  CODE';
  rsMETAFILTRE_EXPE = '  SESSION';
  rsMETAFILTRE_TYPEVISEE = '  TYPE_VISEE';
  rsMETAFILTRE_SECTEUR = '  SECTOR';
  rsMETAFILTRE_APPLY = 'Aanwenden';
  // *******************************************************
  // Finding stations
  rsMSG_FIND_STATION_TITRE = '';
  rsMSG_FIND_STATION_PROMPT = '';
  // *******************************************************
  // Metafiltre dialog resourcestring
  rsDLGMETAFILTRE_TBS1 = 'Datum';
  rsDLGMETAFILTRE_TBS2 = 'Farbe';
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
  rsDLGGRAPHIC_LBOBS = 'Kommentare';
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
  rsMSG_VUE3D_FAIL = 'Beginn 3D-Viewer fehlgeschlagen';
  rsMSG_VUE2D_FAIL = '  Beginn 2D Map Viewer fehlgeschlagen';
  rsMSG_STATS_DLG_FAIL = '  Dialog Statistiken ab fehlgeschlagen';
  rsMSG_PRINT_CENTER_FAIL = '  Beginn Druckzentrum fehlgeschlagen';
  rsMSG_PROJ_MANAGER_FAIL = '  Nach vorne Ausgangs Datenbank fehlgeschlagen';
  rsMSG_PROJ4S_FAIL = '  Beginn fehlgeschlagen Proj4s Koordinaten-Konverter';
  rsMSG_DECLIMAG_FAIL = '  Beginn IGRF Magnetic Deklinationen Rechner fehlgeschlagen';
  rsMSG_TABLE_ENTITY_FAIL = '  Fehler beim Laden Einheiten Tabelle';
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
  rsTYPE_VISEE_DEFAULT = 'Fehler';
  rsTYPE_VISEE_ENTRANCE = '  Eintrag';
  rsTYPE_VISEE_FOSSILE = 'Fossil';
  rsTYPE_VISEE_VADOSE = 'Fluss';
  rsTYPE_VISEE_ENNOYABLE = '  Hochwasser';
  rsTYPE_VISEE_SIPHON = '  Siphon';
  rsTYPE_VISEE_FIXPOINT = '  Festpunkt';
  rsTYPE_VISEE_SURFACE = '  Geworben verbindlich';
  rsTYPE_VISEE_TUNNEL = '  Ein künstlicher Tunnel';
  rsTYPE_VISEE_MINE = '  Lode Mining';
  rsTYPE_VISEE_ANTENNE = '  Antenne';
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
  rsSELECT_LISTE_RESEAUX = 'Netwerker';
  rsSELECT_LISTE_RESEAUX_ID = 'ID';
  rsSELECT_LISTE_RESEAUX_COLOR = 'Farbe';
  rsSELECT_LISTE_RESEAUX_NOM = 'Netzwerk';
  rsSELECT_LISTE_SECTEURS = '';
  rsSELECT_LISTE_SECTEURS_ID = 'ID';
  rsSELECT_LISTE_SECTEURS_COLOR = 'Farbe';
  rsSELECT_LISTE_SECTEURS_NOM = '';
  rsSELECT_LISTE_CODES = 'Codes';
  rsSELECT_LISTE_CODES_ID = 'ID';
  rsSELECT_LISTE_CODES_AZIMUTS = '';
  rsSELECT_LISTE_CODES_PENTES = '';
  rsSELECT_LISTE_CODES_OBS = '';
  rsSELECT_LISTE_EXPES = '';
  rsSELECT_LISTE_EXPES_ID = 'ID';
  rsSELECT_LISTE_EXPES_COULEUR = 'Farbe';
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
  rsSTATISTIQUES_NB_RESEAUX = '%d netwerken';
  rsSTATISTIQUES_NB_SECTEURS = '';
  rsSTATISTIQUES_NB_CODES = '';
  rsSTATISTIQUES_NB_SEANCES = '';
  rsSTATISTIQUES_NB_DATES = '';
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
