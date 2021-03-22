unit UnitMessages_en;
  // Fichier UnitMessages_en.pas cree le 09/05/2014 a 19:28:32
  // Genere par les macros du classeur: file:///C:/0_Logiciels_JPC/0_GHTopoFPC_20140503/Outils_Calc/GestionResourceStrings.ods
  //********************************************************************************
  // /!\ DO NOT MODIFY - Generated automatically
  // Please use spreadsheet utility for modification of strings
  // Language: EN - English
  // ================================================================================
interface
uses
  StructuresDonnees; // pour le code instrument lasermetres

// CONST section
const
  NATIONAL_GEODESIC_SYSTEM_IDX = 1;
  DEFAULT_CODE_EPSG = 'UTM29';
// RESOURCESTRING section
resourcestring
  // Software name
  rsGHTOPOEXENAME = 'GHTopo';
  rsMAINMENUCAPTION = '%s - %s';
  rsGHTOPOVERSION = 'Version 3.1415926 du %.2d/%.2d/%.4d';
  rsGHTOPOLICENSE = 'Software under GPLs license';
  rsGHTOPOAUTHOR = '(c) 1989..%d Jean Pierre CASSOU';
  // Sessin info
  rsGHTOPORESOL = 'Resolution %d x %d';
  rsGHTOPOPlatFormLinux = 'Platform: Linux (English)';
  rsGHTOPOPlatFormWin32 = 'Platform: Microsoft Windows (English)';
  // Language selected
  rsCHOOSELANGUAGE = 'Starting in English language';
  // End GHTopo
  rsENDOFHADES = 'Ending GHTopo';
  // *********************************************
  // Common labels resourcestrings
  rsLBL_COMMENTS = 'Observations';
  rsSERIE_INITIALISATION = 'serie 0 is read only';
  // File filters
  rsFILEFILTER_ALL = 'All (*.*)|*.*';
  rsGHTOPO_FILE_FILTER = 'GHTopo (*.xtb)|*.xtb|LimeLight Tab (*.Tab)|*.Tab|GHTopo XML (*.gtx)|*.gtx|All (*.*)|*.*';
  rsKML_FILE_FILTER = 'Google Earth (*.kml)|*.xtb|All (*.*)|*.*';
  rsGCD_FILE_FILTER = 'GHCaveDraw centerlines (*.gcd)|*.gcd|All (*.*)|*.*';
  rsGPX_FILE_FILTER = 'File GPS (*.gpx)|*.gpx|All (*.*)|*.*';
  rsOSM_FILE_FILTER = 'OpenStreetMap (*.osm)|*.osm|All (*.*)|*.*';
  rsCSV_FILE_FILTER = 'Tabulated text (*.csv)|*.csv|All (*.*)|*.*';
  rsVTOPO_FILE_FILTER = 'Visual Topo (*.tro)|*.tro|All (*.*)|*.*';
  rsTHERION_FILE_FILTER = 'Therion centerlines (*.th)|*.th|All (*.*)|*.*';
  // *********************************************
  // Main menu resourcestring
  rsMSGASSIGNCAPTIONS = 'Building interface';
  rsCALCURAPIDE = 'Calculator :';
  rsMNU_FILE = '&File';
  rsNEW = '&New';
  rsOPEN = '&Open';
  rsSAVE = '&Save';
  rsSAVEAS = 'Save &as';
  rsMNU_SAVELOG = '&Close';
  rsCLOSE = 'Save history';
  rsRECENT_DOCS = 'Recent docs';
  rsRELOAD = 'Reload this document';
  rsEDITTAB = 'Edit TAB file';
  rsPRINT = '&Print';
  rsVTOPO = '&Visual Topo export';
  rsEXPORT_THERION = 'Therion centerlines';
  rsGENERER_DOSSIER_THERION = 'Generate Therion’s folder';
  rsEXPORT_ABRIS = 'Android ABRIS export';
  rsGHCAVEDRAW = 'GHCaveDraw centerlines';
  rsEXPORT_GIS = 'Export to GIS software';
  rsEXPORTGRAPHIQUE = 'Graphical E&xport (PS, DXF, SVG)';
  rsERRINFOXTB = 'Error log';
  rsGHTOPO_QUIT = '&Quit GHTopo';
  rsQUIT_EDITOR = '&Quit text editor';
  rsMNU_EDITION = '&Edit';
  rsMNU_TOPOGRAPHIE = '&Survey';
  rsCHECKBASE = 'Check &database';
  rsCOMPILE = '&Calculate the network';
  rsVUEPLAN = '&Plan view';
  rsVUE3D = '3D &View';
  rsRENDU3D = '&3D Rendering';
  rsSTATISTIQUES = '&Statistics';
  rsINFOCAVITE = 'Cave &Info';
  rsNODESCOORDINATES = '&Nodes coordinates';
  rsMNU_WINDOW = '&Window';
  rsMNU_STAY_ON_TOP = 'Stay on &Top';
  rsWND_DATABASE = 'Database window';
  rsWND_PLAN = 'Plan window';
  rsWND_CONSOLE = '&Console';
  rsMNU_TOOLS = '&Tools';
  rsMNU_HELP = '&Help';
  rsHLPINDEX = '&Index';
  rsHLPNEWS = '&News';
  rsABOUT = '&About GHTopo';
  // Wizard resourcestrings
  rsASSISTANT_SUCCESS = 'Document successfully created by Wizard';
  rsASSISTANT_RELOAD = 'Reloading document';
  rsASSISTANT_ECHEC = 'Wizard failed.';
  rsASSIST_TITLE = 'New Cave Wizard';
  rsASSIST_BNGENERAL = 'General';
  rsASSIST_BNENTRANCES = '1st Entrance';
  rsASSIST_BNCODES = '1st Code';
  rsASSIST_BNEXPES = '1st Trip';
  rsASSIST_BNSERIE = '1st Serie';
  rsASSIST_BNSAVE = 'Save ...';
  rsASSIST_BNCANCEL = 'Cancel';
  rsASSIST_LBNOMETUDE = 'Survey name';
  rsASSIST_LBOBSETUDE = 'Survey comments';
  rsASSIST_LBNOMENTREE = 'Main entrance';
  rsASSIST_LBCOORDS = 'Coordinates';
  rsASSIST_LBOBSENTREE = 'Observations';
  rsASSIST_LBCONVERTISSEUR = 'Converter ...';
  rsASSIST_LBREFSTATION = 'Main station';
  rsASSIST_LBREFSERIE = 'Serie';
  rsASSIST_LBREFPOINT = 'Station';
  rsASSIST_LBCOMMENTAIRE = 'Comments';
  rsASSIST_LBSYSTGEO = 'Geographic coordinates systems';
  // ********************************
  // 2D View resourcestrings
  rsVUE2D_TITLE = 'Plan view';
  rsVUE2D_REPRESENTATION_MODE = 'Representation';
  rsVUE2D_REPRESENTATION_NETWORKS = 'Networks';
  rsVUE2D_REPRESENTATION_SECTEURS = 'Sectors';
  rsVUE2D_REPRESENTATION_EXPES = 'Sessions';
  rsVUE2D_REPRESENTATION_GRAY = 'Gray';
  rsVUE2D_REPRESENTATION_DEPTH = 'Depth';
  rsVUE2D_REPRESENTATION_STATIONS = 'Stations';
  rsVUE2D_LOAD_MAILLAGE = 'Load NTM';
  rsVUE2D_DISTANCE = 'Distance beetwen stations';
  rsVUE2D_FINDSTATION = 'Find a station';
  rsVUE2D_ROSE_DIAGRAM = 'Rose diagram';
  rsVUE2D_DEPTH_DIAGRAM = 'Depth histogram';
  rsVUE2D_METAFILTRE = 'Filters';
  rsVUE2D_PANVUE = 'Move view';
  rsVUE2D_PARAMETRES_VUE = 'Display parameters';
  rsVUE2D_PRINTING = 'Print the plan';
  rsVUE2D_REFRESH = 'Redraw view';
  rsVUE2D_EXPORT_DXF = 'DXF export';
  rsVUE2D_STATISTIQUES = 'Statistics';
  rsVUE2D_VUE3D_GDI = '3D view (without OpenGL)';
  rsVUE2D_VUE3D_OPENGL = '3D rendering OpenGL';
  rsVUE2D_ZOOMALL = 'Zoom all';
  rsVUE2D_ZOOMFENETRE = 'Zoom window';
  rsVUE2D_FMT_INFOS_STATION_RAPIDE = 'Station: %s (%d.%d): X=%.2f Y=%.2f Z=%.2f';
  rsVUE2D_FMT_INFOS_ID_STATION = '%d.%d [%s]';
  rsVUE2D_FMT_INFOS_MESURES = 'L = %.2f, Az = %.2f, P =%.2f, L = %.2f, R = %.2f, U = %.2f, D = %.2f';
  rsVUE2D_FMT_INFOS_COORDONNEES = 'X = %s, Y = %s, Z = %s';
  rsVUE2D_EXPORT_XHTML = 'Export XHTML';
  rsVUE2D_EXPORT_SVG = 'Export SVG';
  // *******************************************************
  // Database manager (tabs)
  rsTBS_GENERAL = 'General';
  rsTBS_GENERAL_NOM = 'Network name';
  rsTBS_GENERAL_OBS = 'Observations';
  rsTBS_GENERAL_SYST = 'Coordinate system';
  rsTBS_ENTRANCE = 'Entrances';
  rsTBS_SECTEURS = 'Sectors';
  rsTBS_CODES = 'Codes';
  rsTBS_TRIPS = 'Trips';
  rsTBS_SERIES = 'Series';
  rsTBS_RESEAUX = 'Networks';
  rsTBS_ANTENNES = 'Antenna shots';
  rsTBS_MAINTENANCE = 'Maintenance';
  // *******************************************************
  // resourcestring de la section General
  rsLB_NOM_ETUDE = 'Folder name';
  rsLB_COMMENTAIRE_ETUDE = 'Observations';
  rsLB_CODE_EPSG = 'EPSG code';
  rsBTN_SELECT_EPSG = 'Choose ...';
  rsBTN_CALC_DECLIMAGS = 'Calculate magnetic declinations';
  // *******************************************************
  // Entrances frame resourcestring
  rsCDR_ENTR_NOENTRANCE = 'Entrance';
  rsCDR_ENTR_ENTRNAME = 'Entrance name';
  rsCDR_ENTR_COORDINATES = 'Coordinates';
  rsCDR_ENTR_STATOFENTR = 'Reference station';
  // *******************************************************
  // Reseaux frame resourcestrings
  rsCDR_RESEAU_LBIDX = 'Numero';
  rsCDR_RESEAU_NAME = 'Name';
  rsCDR_RESEAU_TYPE = 'Network type';
  rsCDR_RESEAU_CB0 = 'Natural cave';
  rsCDR_RESEAU_CB1 = 'Anthropic cave';
  rsCDR_RESEAU_CB2 = 'Surface surveying';
  rsCDR_RESEAU_CB3 = 'Thalweg';
  rsCDR_RESEAU_CB4 = 'Road';
  rsCDR_RESEAU_CB5 = 'Track';
  rsCDR_RESEAU_CB6 = 'Other';
  // *******************************************************
  // Sectors frame resourcestring
  rsCDR_SECTEUR_LBIDX = 'Numero';
  rsCDR_SECTEUR_NAME = 'Name';
  rsCDR_SECTEUR_COLOUR = 'Colour';
  // *******************************************************
  // Expres frame resourcestring
  rsCDR_EXPE_SEANCE = 'Survey session';
  rsCDR_EXPE_DATE = 'Date';
  rsCDR_EXPE_DECLIMAG = 'Declination';
  rsCDR_EXPE_INCLIN = 'Inclination';
  rsCOLOR = 'Colour';
  rsCDR_EXPE_SPELEOMETRE = 'Surveyor 1';
  rsCDR_EXPE_SPELEOGRAPHE = 'Surveyor 2';
  // *******************************************************
  // Codes frame resourcestring
  rsCDR_CODES_NUMERO = 'Code';
  rsCDR_CODES_TYPEGALERIE = 'Shot Type';
  rsTITRE_SELECTEUR_VISEE = 'Choose shot type';
  rsCMBTYPE_D = '0 - Default';
  rsCMBTYPE_E = '1 - Entrance';
  rsCMBTYPE_B = '2 - Fossile Gallery';
  rsCMBTYPE_V = '3 - Free flow';
  rsCMBTYPE_W = '4 - Sump';
  rsCMBTYPE_C = '5 - Floodable conduit';
  rsCMBTYPE_F = '6 - Fixed point';
  rsCMBTYPE_S = '7 - Surface survey';
  rsCMBTYPE_A = '8 - Artificial Tunnel';
  rsCMBTYPE_M = '9 - Mine gallery';
  rsCDR_CODES_VISEE_HZ = 'Horizontal value';
  rsCDR_CODES_VISEE_VT = 'Vertical value';
  rsCDR_CODES_VDIRECT = 'Direct';
  rsCDR_CODES_VINVERSE = 'Invert';
  rsCDR_CODES_GRADCOMPAS = 'Compass Graduation';
  rsCDR_CODES_GRADCLINO = 'Clinometer Graduation';
  rsCDR_CODES_CMBUNIT_0 = '400 - Gon';
  rsCDR_CODES_CMBUNIT_1 = '360 - Degrees';
  rsCDR_CODES_CMBUNIT_2 = '370 - Percentages';
  rsCDR_CODES_CMBUNIT_3 = '380 - Slopes';
  rsCDR_CODES_CMBUNIT_4 = '800 - Lasermeter like "Stanley TLM330"';
  rsCDR_CODES_FACT = 'Lengths x';
  rsCDR_CODES_POSZERO = 'Zero position:';
  rsCDR_CODES_CMBZERO_0 = 'Nadiral';
  rsCDR_CODES_CMBZERO_1 = 'Horizontal';
  rsCDR_CODES_CMBZERO_2 = 'Zenithal';
  rsCDR_CODES_ANGLIMIT = 'Limit angle:';
  rsCDR_CODES_PRECISION = 'Instruments accuracy';
  // *******************************************************
  // Series frame resourcestring
  rsDLG_PJMNGR_MOVETOSERIE = 'Move to other serie ?';
  rsDLG_PJMNGR_ADDSERIE = 'Add Serie';
  rsCDR_SERIE_LB_RESEAU = 'Network';
  rsCDR_SERIE_ADDPHOTO = 'Add photo';
  rsCDR_SERIE_NUMERO = 'Serie';
  rsCDR_SERIE_NAME = 'Name';
  rsCDR_SERIE_DEPART = 'Start';
  rsCDR_SERIE_ARRIVEE = 'End';
  rsCDR_SERIE_CHANCE = 'Chance';
  rsCDR_SERIE_CHANCE0 = 'None';
  rsCDR_SERIE_CHANCE1 = 'Low';
  rsCDR_SERIE_CHANCE2 = 'Good';
  rsCDR_SERIE_CHANCE3 = 'Excellent';
  rsCDR_SERIE_OBSTACLE = 'Obstacle';
  rsCDR_SERIE_OBSTACLE0 = 'None';
  rsCDR_SERIE_OBSTACLE1 = 'Pit';
  rsCDR_SERIE_OBSTACLE2 = 'Chimney';
  rsCDR_SERIE_OBSTACLE3 = 'Squeeze';
  rsCDR_SERIE_OBSTACLE4 = 'Lake';
  rsCDR_SERIE_OBSTACLE5 = 'Sump';
  rsCDR_SERIE_OBSTACLE6 = 'Collapse';
  rsCDR_SERIE_OBSTACLE7 = 'Speleothems';
  rsCDR_SERIE_OBSTACLE8 = 'Sediments';
  rsCDR_SERIE_OBSTACLE9 = 'Other';
  // France only lol
  rsCDR_SERIE_OBSTACLE10 = 'Toxic gas';
  rsCDR_SERIE_OBSTACLE11 = 'Aggressives geese';
  rsCDR_SERIE_OBSTACLE12 = 'Dangerous animals';
  rsCDR_SERIE_OBSTACLE13 = 'Gang-bang site';
  rsCDR_SERIE_LOCKED = 'Locked';
  rsCDR_SERIE_INSERTLINE = 'Insert lines';
  rsCDR_SERIE_NBLINES = 'Number of lines';
  rsCDR_SERIE_DELETELINE = 'Remove lines';
  rsCDR_SERIE_UNDOCOPY = 'Undo copy';
  rsCDR_SERIE_INC_UNDOCOPY = 'Incremental undocopy';
  rsCDR_SERIE_IMPLEMENT = 'Validate';
  rsINPUT_COMMENTAIRE_TITRE = 'Station comments';
  rsINPUT_COMMENTAIRE_MSG = 'input a text';
  rsCDR_SERIE_PASTE = 'Paste from clipboard';
  rsCDR_SERIE_BTN_GRD_COPY = 'Copy grid';
  rsCDR_SERIE_MSG_ERR_ENTREE_NOT_FOUND = 'Entrance not found';
  rsCDR_SERIE_MSG_ERR_RESEAU_NOT_FOUND = 'Network not found';
  rsCDR_SERIE_MSG_ERR_CODE_NOT_FOUND = 'Code not found';
  rsCDR_SERIE_MSG_ERR_EXPE_NOT_FOUND = 'Session not found';
  rsCDR_SERIE_MSG_ERR_SERIE_NOT_FOUND = 'Serie not found';
  rsCDR_SERIE_MSG_ERR_LONG = 'Length shot must be positive and less than %.0f m';
  rsCDR_SERIE_MSG_ERR_LRUD = 'Distance must be positive';
  // *******************************************************
  // resoursestring Antenna frame
  rsCDR_ANTENNES_AC_ADDLINE = 'Add line';
  rsCDR_ANTENNES_AC_DELLINE = 'Remove ligne';
  rsCDR_ANTENNES_AC_SAVEGRD = 'Apply modifications';
  rsCDR_ANTENNES_DEL_LINE = 'Remove line %d';
  rsCDR_ANTENNES_FIND_RESEAU = 'Network from line %d';
  rsCDR_ANTENNES_FIND_CODE = 'Code from line %d';
  rsCDR_ANTENNES_FIND_EXPE = 'Session from line %d';
  // *******************************************************
  // CdrNavigateurDB frame resourcestring
  rsCDR_NAVIG_DB_DO_SORT = 'Sort';
  rsCDR_NAVIG_DB_DO_ADD = 'Add element';
  rsCDR_NAVIG_DB_DO_DELETE = 'Remove element';
  // *********************************
  // VTopo dialog resourcestring
  rsVTOPO_EDPREFIX = 'Stations prefix';
  rsVTOPO_LBIDPTDEP = 'Station ID';
  rsVTOPO_LBREPORTER = 'Report operator';
  rsLBFICHIER = 'File';
  rsLBMAINENTRANCE = 'Main entrance';
  rsLBIDMAINST = 'Start station';
  rsLBSTPREFIX = 'Stations prefix';
  rsLBREPORTER = 'Report';
  rsLBMAINENTCOORD = 'Main entrance coordinates';
  rsLBCOLORDEFAULT = 'Colour by default';
  // *******************************************************
  // OpenGL viewer resourcestring
  rsOGLVIEWERTITLE = 'OpenGL Viewer [%s]';
  rsLBLBACKCOLOR = 'Background color';
  rsLBLCUBECOLOR = 'Cube color';
  rsLBLREFCOLOR = 'Referential color';
  rsLBLREFSIZE = 'Size';
  rsOPENGLERROR = 'OpenGL error';
  rsOGLVQUIT = 'Quit OpenGL viewer';
  // *******************************************************
  // resourcestrings de graphic exportator
  rsPSDXF_TITLE = 'Graphical Export: [%s]';
  rsDODISPDESELECTEDPARTS = 'Display parts rejected by MetaFiltre - Color';
  rsTAB_LAYERS = 'Layers';
  rsTAB_QUADR = 'Criss-cross';
  rsTAB_DESSIN = 'Drawing';
  rsTAB_TEXTES = 'Texts';
  rsTYPEQDR = 'Criss-cross type';
  rsQDNONE = 'None';
  rsQDCROSS = 'Grid';
  rsQDQUADRILLES = 'Cross';
  rsGRAPHICS_PS = 'PostScript PS';
  rsGRAPHICS_DXF = 'AutoCAD DXF';
  rsGRAPHICS_SVG = 'Scalable Vector Graphics SVG';
  rsGRAPHICS_WMF = 'Windows MetaFile WMF';
  rsDLGDXF_TITLE = 'DXF export: %s';
  // *******************************************************
  // MetaFiltre keywords
  rsHLPMETAFILTRE = 'METAFILTRE';
  rsMETAFILTRE_NIL = 'NIL';
  rsMETAFILTRE_ALL = 'ALL';
  rsMETAFILTRE_ID = 'ID';
  rsMETAFILTRE_LENGTH = 'LENGTH';
  rsMETAFILTRE_AZIMUTH = 'AZIMUTH';
  rsMETAFILTRE_PENTE = 'SLOPE';
  rsMETAFILTRE_DATE = 'DATE';
  rsMETAFILTRE_COLOR = 'COLOR';
  rsMETAFILTRE_X = 'X_COORD';
  rsMETAFILTRE_Y = 'Y_COORD';
  rsMETAFILTRE_Z = 'Z_COORD';
  rsMETAFILTRE_LARGEUR = 'WIDTH';
  rsMETAFILTRE_HAUTEUR = 'HEIGHT';
  rsMETAFILTRE_DATES = 'DATES';
  rsMETAFILTRE_COLORS = 'COLORS';
  rsMETAFILTRE_SERIE = 'SERIE';
  rsMETAFILTRE_RESEAU = 'NETWORK';
  rsMETAFILTRE_CODE = 'CODE';
  rsMETAFILTRE_EXPE = 'TRIP';
  rsMETAFILTRE_TYPEVISEE = 'TYPE_SHOT';
  rsMETAFILTRE_SECTEUR = 'SECTOR';
  rsMETAFILTRE_APPLY = 'Apply';
  // *******************************************************
  // Finding stations
  rsMSG_FIND_STATION_TITRE = 'Find station';
  rsMSG_FIND_STATION_PROMPT = 'Enter shot label or pair of serie.station (dot as separator)';
  // *******************************************************
  // Metafiltre dialog resourcestring
  rsDLGMETAFILTRE_TBS1 = 'Dates';
  rsDLGMETAFILTRE_TBS2 = 'Colours';
  rsDLGMETAFILTRE_PERSO = 'Custom';
  // ******************************************************
  // Palette chooser resourcestring
  rsSELECTCOLORTITLE = 'Select a colour';
  rsLBLUSEDCOLORS = 'Last colours';
  rsDLGCOULSAVEPAL = 'Save palette';
  rsDLGCOULRESTPAL = 'Load palette';
  rsDLGCOULFILTERPAL = 'Palette files (*.pal)|*.pal|Tous (*.*)|*.*';
  rsDLGCOUDELPAL = 'Replace file';
  rsPALETTENOTFOUNT = 'Palette not found';
  // ******************************************************
  // Graphic export resourcestrings
  rsDLGGRAPHIC_OUTPUTFMT = 'Output format:';
  rsDLGGRAPHIC_LBFILENAME = 'File name:';
  rsDLGGRAPHIC_LBOBS = 'Observations';
  rsDLG_GRAPHIC_TABTITLE = 'Graphic export';
  rsDLGGRAPHIC_GBCROIX = 'Criss-cross';
  rsDLGGRAPHIC_GBCHEM = 'Paths and sections';
  rsDLGGRAPHIC_GBWALLS = 'Walls and colors';
  rsDLGGRAPHIC_SPACING = 'Spacing';
  rsDLGGRAPHIC_TYPEGRID = 'Type:';
  rsDLGGRAPHIC_CMBGRD2 = 'Cross';
  rdDLGGRAPHIC_LAYER = 'Layer:';
  rdDLGGRAPHIC_WALLFILL = 'Fill';
  rsDLGGRAPHIC_WFILL1 = 'Solid (1 colour)';
  rsDLGGRAPHIC_WFILL2 = 'Conduits types';
  rsDLGGRAPHIC_WFILL3 = 'Shots colour';
  rsDLGGRAPHIC_WFILL4 = 'By networks';
  rsDLGGRAPHIC_WFILL5 = 'By dates';
  rsDLGGRAPHIC_CHKCHEM = 'Export centerlines';
  rsDLGGRAPHIC_CHKSECT = 'Export sections';
  // ******************************************************
  // Listbox resourcestring
  rsSELECT_LISTE_ENTRANCE = 'Select an entrance';
  rsSELECT_LISTE_RESEAU = 'Select a network';
  rsSELECT_LISTE_SECTEUR = 'Select a secteur';
  rsSELECT_LISTE_CODE = 'Select a instruments code';
  rsSELECT_LISTE_EXPE = 'Select a trip';
  rsSELECT_LISTE_SERIE = 'Select a serie';
  // ******************************************************
  // Printing center resourcestring
  rsPRN_NOPRINTER = 'No printer installed';
  rsPRN_TBPRINTER = 'Printer';
  rsPRN_TBDRAW = 'Drawing';
  rsPRN_TITLE = 'Printing center [%s]';
  rsPRN_CHKPOLY = 'Polygonal';
  rsPRN_CHKFILL = 'Filling';
  rsPRN_CHKWALLS = 'Walls';
  rsPRN_CHKSECTS = 'Sections';
  rsPRN_CHKSTATIONS = 'Stations';
  rsPRN_CHKSTATIONS_LBL = 'ID stations';
  rsPRN_CHKALTITUDE = 'Elevations';
  rsPRN_CHKCOTE = 'Depth';
  rsPRN_CHKQUADRILLAGE = 'Gridding';
  rsPRN_CHKENTREES = 'Entrances';
  rsPRN_CHKANNOTATIONS = 'Annotations';
  rsPRM_LBANGLEROT = 'Rotation angle';
  rsPRN_TYPEQDR = 'Grid type';
  rsPRN_QDNONE = 'None';
  rsPRN_QDCROSS = 'Cross';
  rsPRN_QDQUADRILLES = 'Grid';
  rsPRN_QDPOINTS = 'Points';
  rsPRN_SCALING = 'Scale';
  rsPRN_LBSPACING = 'Spacing';
  rsLANDSCAPE = 'Landscape';
  rsPORTRAIT = 'Portrait';
  rsPRN_START_PRINTING = 'Start printing';
  rsPRN_PRINTING_DONE = 'Impression done';
  rsDLGIMP_TAB1 = 'Preview';
  rsDLGIMP_TAB2 = 'Printer';
  rsDLGIMP_TAB3 = 'Drawing options';
  rsDLGIMP_TAB4 = 'Networks';
  rsQDRSPACING = 'Spacing';
  rsECHELLE = 'Scale: 1 /';
  rsLAYERS = 'Layers';
  rsPREVIEW = 'Preview';
  rsSTARTPRINTING = 'Print';
  rsREGLE = 'Rule';
  // -------------------------------------------------
  rsHLP_TITRE = 'GHTopo Help System';
  rsHLP_BNEDIT = 'Edit help file';
  rsHLP_DOCONTINUE = 'Do you want to continue ?';
  rsHLP_TAB_RUBRIQUES = 'Help sections';
  rsHLP_TAB_ABOUT = 'About GHTopo';
  // -------------------------------------------------
  // resourcestring Serie/station dialog
  rsDLG_SERST_TITLE = 'Find station';
  rsDLG_SERST_SERIE = 'Serie';
  rsDLG_SERST_STATION = 'Station';
  rsDLG_SERST_CLE = 'Field code';
  rsDLG_SERST_LBSEARCH = 'Finding';
  rsDLG_SERST_BYSERST = 'By serie and station';
  rsDLG_SERST_BYREFTER = 'By field reference';
  // -------------------------------------------------
  // Annotations dialog resourcestrings
  rsDLG_ANN_TITLE = 'Captions Editor';
  rsDLG_ANN_LBTEXTE = 'Text';
  rsDLG_ANN_LBMAX = 'Max length';
  rsDLG_ANN_CHKTXTDISP = 'Draw this text';
  rsDLG_ANN_GRBPOSTEXT = 'Text positioning';
  rsDLG_ANN_GRBMETH0 = 'Absolute Coordinates';
  rsDLG_ANN_GRBMETH1 = 'Hooked at station';
  rsDLG_ANN_LBPOSTEXTE = 'Base point';
  rsDLG_ANN_LBSTATION = 'Station';
  rsDLG_ANN_LBOFFSET = 'Desfase (m) : X = %.2f m ; Y = %.2f m';
  rsDLG_ANN_GRBATTRTXT = 'Character attributes';
  rsDLG_ANN_GRBBASEPT = 'Text base point';
  // -------------------------------------------------
  // find by Station label
  rsDLG_FIND_PT_BY_ID_TITLE = 'Find by Literal ID';
  rsDLG_FIND_PT_BY_ID_PROMPT = 'Code';
  rsDLG_FIND_PROMPT = 'Found text';
  rsDLG_FIND_ENTRANCE_BY_TEXT = 'Find entrance';
  rsDLG_FIND_RESEAU_BY_TEXT = 'Find network';
  rsDLG_FIND_CODE_BY_TEXT = 'Find instruments code';
  rsDLG_FIND_EXPE_BY_TEXT = 'Find survey session';
  rsDLG_FIND_SERIE_BY_TEXT = 'Find serie';
  rsDLG_FIND_SECTEUR_BY_TEXT = 'Find sector';
  // Access by number
  rsDLG_GOTO_PROMPT = 'Number';
  rsDLG_GOTO_ENTRANCE_BY_NUMERO = 'Go to entrance';
  rsDLG_GOTO_RESEAU_BY_NUMERO = 'Go to network';
  rsDLG_GOTO_SECTEUR_BY_NUMERO = 'Go to sector';
  rsDLG_GOTO_CODE_BY_NUMERO = 'Go to instruments code';
  rsDLG_GOTO_EXPE_BY_NUMERO = 'Go to survey session';
  rsDLG_GOTO_SERIE_BY_NUMERO = 'Go to serie';
  // Integrated calculator
  rsDLG_CALC_TITLE = 'Calculator and coordinates converter';
  rsDLG_CALC_TAB_CAL = 'Calculator';
  rsDLG_CALC_EXPR = 'Input an expression';
  rsDLG_CALC_DOCALC = 'Calculate';
  rsDLG_CALC_CDR_CONVERT = 'Coordinates converter';
  rsDLG_CALC_CDR_DECLIMAG = 'Magnetic declination';
  rsDLG_CALC_BTN_CONVERT = 'Convert';
  rsDLG_CALC_BTN_ECHANGE = 'Exchange';
  rsDLG_CALC_LB_SYST_SOURCE = 'Source grid';
  rsDLG_CALC_LB_SYST_CIBLE = 'Target grid';
  rsDLG_CALC_HINT_GRD_CONVERSIONS = 'Input values in the grid or paste from clipboard';
  rsDLG_CALC_EXPRESSION = 'Expression';
  rsDLG_CALC_SYSCOORDS = 'Coordinates systems';
  rsDLG_CALC_LSB_FUNCTIONS = 'Functions';
  rsDLG_CALC_COORDS_UNITAIRE = 'Single coordinates';
  rsDLG_CALC_COORDS_TABLEAU = 'Multiples coordinates';
  rsDLG_CALC_DECL_UNITAIRE = 'Magnetic declination';
  rsDLG_CALC_DECL_TABLEAU = 'Declinations table';
  // Clipboard management
  rsDLG_CLIPBRD_PTS_TITLE = 'Import survey shots';
  // Messages
  rsINSERTITEM = 'Insert element ?';
  rsDELETEITEM = 'Delete element ?';
  rsONCLOSEPRJMNGR = 'This will be close current document - Save changes ?';
  rsMSG_SEEALSO = 'See also ...';
  rsMSG_NDSNEEDED = 'Nodes file not found - Recompile the document';
  rsMSG_SAVECHANGES = 'Save changes';
  rsMSG_ERASESTATION = 'Erase station %d ?';
  rsMSG_FILENOTFOUND = 'File %s not found';
  rsMSG_READY = 'READY';
  rsMSG_NOFILEOPENED = 'No file opened';
  rsDISPLAY_HELP_SYSTEM = 'Starting help system';
  rsHLPCENTER_TITLE = 'GHTopo Help system';
  rsMATCHNOTFOUND = 'Match not found';
  rsNOCANCLOSE = 'Quit with saves';
  rsWARN_FILEALREADYOPEN = 'Document already opened - Continue ?';
  rsSAVECHANGES = 'Save changes';
  rsNOCANCLOSEWND = 'This window cannot be closed';
  rsERASEEXISTNAMEDFILE = 'Erase the file %s ?';
  rsSAVESLOST = 'Changes will be lost';
  rsCFGMTFLTR_UNABLE = 'Error in file of personalized filters';
  rsERRORSTARTPRNCTR = 'Error stating printing center';
  rsNOPRINTERINSTALLED = 'No printer installed';
  rsERRORLOADINGTOP = 'Error loading TOP file';
  rsIDSERIE_EXISTS = 'Serie index %d already attributed (serie #%d)';
  // Error messages
  rsMSG_VUE3D_FAIL = 'Failed to starting 3D viewer';
  rsMSG_VUE2D_FAIL = 'Failed to starting 2D map viewer';
  rsMSG_STATS_DLG_FAIL = 'Failed to starting statistics dialog';
  rsMSG_PRINT_CENTER_FAIL = 'Failed to starting printing center';
  rsMSG_PROJ_MANAGER_FAIL = 'Failed to starting database frontal';
  rsMSG_PROJ4S_FAIL = 'Failed to starting Proj4s coordinates converter';
  rsMSG_DECLIMAG_FAIL = 'Failed to starting IGRF Magnetic Declinations calculator';
  rsMSG_TABLE_ENTITY_FAIL = 'Error loading entities table';
  rsMSG_CODE_NOT_FOUND = 'Code not found';
  rsMSG_EXPE_NOT_FOUND = 'Trip not found';
  rsMSG_ANGLE_OUT_OF_RANGE = '%s doit must between %.2f and %.2f';
  rsMSG_WARN_DELETE_ITEM = 'Deleting object #%d can be corrupt database - Continue ?';
  // messages in TToporobotStructure.LoadFichierTab()
  rsRD_TAB_MSG_ERR = 'Processing line error';
  rsRD_TAB_D_MULTI_OBS = 'Starting multiline comments at line #%d';
  rsRD_TAB_F_MULTI_OBS = 'Ending multiline comments at line #%d';
  rsRD_TAB_STOP_9999 = '*** Line %d: Reading aborted by -9999 code***';
  rsRD_TAB_LN_OBS = 'Line %d is a comment';
  rsRD_TAB_LASTSAVES = 'Last saves: %s %s';
  rsRD_TAB_CLASSEURS = '-7 [Folders] Functionnality disabled';
  rsRD_TAB_ENTRANCE = 'Created entrance #%d';
  rsRD_TAB_ENTR_NOGEOREF = '[Warning] (%d): Entrance #%d (%s): Ungeoreferenced';
  rsRD_TAB_ENTR_BADLINK = '[Warning] (%d): Entrance #%d (%s): Uncorrect linkage [Serie: %d - Station: %d]';
  rsRD_TAB_IGNORED_SEC = '[Information] Ignored section';
  rsRD_TAB_BAD_TRIP = 'Uncorrect trip';
  rsRD_TAB_BAD_CODE = 'Uncorrect code';
  rsRD_TAB_BAD_DATE = '[Warning] (%d) - Invalid date - System-date assigned';
  rsRD_TAB_BAD_IDXRES = '[Warning] (%d) - Network index unknown (%d) for serie %d - Value 0 assigned';
  rsRD_TAB_IDEXTR_MM = '[Warning] (%d) - Serie %d: Start and end with same ID';
  rsRD_TAB_SELF_CLOSURE = '[Warning] (%d) - Auto-loop in serie %d';
  rsRD_TAB_NEG_LONG = '[Warning] (%d) - Negative length (%.2f m) muliply by -1';
  rsRD_ERROR_LN = '*** Error in line #%d: %s';
  rsRD_CONTNT_LN = 'Line contents: %s';
  rsRD_TAB_FIELD_VALUES = 'Fields values:';
  rsRD_TAB_NOFATAL_ERR = 'No fatal errors in this file';
  rsRD_TAB_WTFATAL_ERR = '%d errors found in this file';
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
  // Shots types
  rsTYPE_VISEE_DEFAULT = 'Default';
  rsTYPE_VISEE_ENTRANCE = 'Entrance';
  rsTYPE_VISEE_FOSSILE = 'Fossile';
  rsTYPE_VISEE_VADOSE = 'Flow';
  rsTYPE_VISEE_ENNOYABLE = 'Floodable';
  rsTYPE_VISEE_SIPHON = 'Sump';
  rsTYPE_VISEE_FIXPOINT = 'Fix point';
  rsTYPE_VISEE_SURFACE = 'Special shot';
  rsTYPE_VISEE_TUNNEL = 'Tunnel';
  rsTYPE_VISEE_MINE = 'Mine';
  rsTYPE_VISEE_ANTENNE = 'Antenna';
  // -------------------------------------------------
  // Misc
  rsSELECTALL = 'Select All';
  rsDESELECTALL = 'Deselect All';
  rsOBS = 'Observations';
  rsFILTERS = 'Filters:';
  rsCHKSELECTALL = 'Sel. all';
  rsCHKDESELECTALL = 'Desel. all';
  rsCHKREVERSE = 'Rev. sel.';
  rsMAIN_NETWORK = 'Main network';
  // -------------------------------------------------
  // dlgSelectDansListes
  rsSELECT_LISTE_NB_ELEMENTS = 'List of %d %s';
  rsSELECT_LISTE_ENTREES = 'Entrances';
  rsSELECT_LISTE_ENTREES_ID = 'ID';
  rsSELECT_LISTE_ENTREES_NOM = 'Entrance';
  rsSELECT_LISTE_ENTREES_REF = 'Ref.';
  rsSELECT_LISTE_ENTREES_X = 'X';
  rsSELECT_LISTE_ENTREES_Y = 'Y';
  rsSELECT_LISTE_ENTREES_Z = 'Z';
  rsSELECT_LISTE_RESEAUX = 'Networks';
  rsSELECT_LISTE_RESEAUX_ID = 'ID';
  rsSELECT_LISTE_RESEAUX_COLOR = 'Colour';
  rsSELECT_LISTE_RESEAUX_NOM = 'Network';
  rsSELECT_LISTE_SECTEURS = 'Sectors';
  rsSELECT_LISTE_SECTEURS_ID = 'ID';
  rsSELECT_LISTE_SECTEURS_COLOR = 'Colour';
  rsSELECT_LISTE_SECTEURS_NOM = 'Secteur';
  rsSELECT_LISTE_CODES = 'Codes';
  rsSELECT_LISTE_CODES_ID = 'ID';
  rsSELECT_LISTE_CODES_AZIMUTS = 'Azimuths';
  rsSELECT_LISTE_CODES_PENTES = 'Slopes';
  rsSELECT_LISTE_CODES_OBS = 'Observations';
  rsSELECT_LISTE_EXPES = 'Sessions';
  rsSELECT_LISTE_EXPES_ID = 'ID';
  rsSELECT_LISTE_EXPES_COULEUR = 'Colour';
  rsSELECT_LISTE_EXPES_DATE = 'Date';
  rsSELECT_LISTE_EXPES_SPELEOMETRE = 'Surveyor 1';
  rsSELECT_LISTE_EXPES_SPELEOGRAPHE = 'Surveyor 2';
  rsSELECT_LISTE_EXPES_DECLINAISON = 'Declination';
  rsSELECT_LISTE_EXPES_OBS = 'Observations';
  rsSELECT_LISTE_SERIES = 'Series';
  rsSELECT_LISTE_SERIES_ID = 'ID';
  rsSELECT_LISTE_SERIES_DEPART = 'Start';
  rsSELECT_LISTE_SERIES_ARRIVEE = 'End';
  rsSELECT_LISTE_SERIES_NOM = 'Name';
  rsSELECT_LISTE_SERIES_RESEAU = 'Network';
  rsSELECT_LISTE_SERIES_NBPOINTS = 'Nb Stations';
  rsSELECT_LISTE_DATES = 'Dates';
  rsSELECT_LISTE_UNE_DATE = 'Date';
  rsSELECT_LISTE_STATIONS = 'Stations';
  rsSELECT_LISTE_STATIONS_ID = 'ID';
  rsSELECT_LISTE_STATIONS_NB = 'Nb stations';
  rsSELECT_LISTE_STATIONS_LABEL = 'Litteral ID';
  rsSELECT_LISTE_STATIONS_X = 'X';
  rsSELECT_LISTE_STATIONS_Y = 'Y';
  rsSELECT_LISTE_STATIONS_Z = 'Z';
  // -------------------------------------------------
  // dlgExportVersSIGExt
  rsEXPORT_SIG_TITRE = 'GIS Export';
  rsEXPORT_SIG_SYST_COORDONNEES = 'Coordinates system';
  rsEXPORT_SIG_SIG_CIBLE = 'Output format';
  rsEXPORT_SIG_LB_FICHIER = 'File name';
  rsEXPORT_SIG_USE_COLORS = 'Use groups colours';
  rsEXPORT_SIG_SILHOUETTE = 'Silhouettes';
  rsEXPORT_SIG_BTN_GO = 'Generate';
  rsEXPORT_SIG_BTN_PREFIX = 'Prefix';
  // -------------------------------------------------
  // dlgStatistiquesExt
  rsSTATISTIQUES_TITRE = 'Statistics';
  rsSTATISTIQUES_TAB_TABLEAUX = 'Tables';
  rsSTATISTIQUES_TAB_SYNTHESE = 'Synthesis';
  rsSTATISTIQUES_TAB_DIAGRAMMES = 'Diagrams';
  rsSTATISTIQUES_TAB_CORDONNEES = 'Stations coordinates';
  rsSTATISTIQUES_CMB_MODE_TABLE0 = 'Networks';
  rsSTATISTIQUES_CMB_MODE_TABLE1 = 'Sectors';
  rsSTATISTIQUES_CMB_MODE_TABLE2 = 'Instrument codes';
  rsSTATISTIQUES_CMB_MODE_TABLE3 = 'Sessions';
  rsSTATISTIQUES_CMB_MODE_TABLE4 = 'Dates';
  rsSTATISTIQUES_NB_RESEAUX = '%d networks';
  rsSTATISTIQUES_NB_SECTEURS = '%d sectors';
  rsSTATISTIQUES_NB_CODES = '%d codes';
  rsSTATISTIQUES_NB_SEANCES = '%d sessions';
  rsSTATISTIQUES_NB_DATES = '%d dates';
  rsSTATISTIQUES_TYPE_SHOT_FOSSILES = 'Fossiles';
  rsSTATISTIQUES_TYPE_SHOT_VADOSES = 'Vadoses';
  rsSTATISTIQUES_TYPE_SHOT_ENNOYABLES = 'Floodables';
  rsSTATISTIQUES_TYPE_SHOT_SIPHONS = 'Sumps';
  rsSTATISTIQUES_TYPE_SHOT_NATURELS = 'Naturals';
  rsSTATISTIQUES_TYPE_SHOT_SPECIAUX = 'Specials';
  rsSTATISTIQUES_TYPE_SHOT_TUNNELS = 'Tunnels';
  rsSTATISTIQUES_TYPE_SHOT_MINES = 'Mines';
  rsSTATISTIQUES_COPY_TABLEAU = 'Copy table';
//================================================================================
implementation

end.
