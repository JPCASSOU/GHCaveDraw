unit UnitMessages_es;
  // Fichier UnitMessages_es.pas cree le 09/05/2014 a 19:28:33
  // Genere par les macros du classeur: file:///C:/0_Logiciels_JPC/0_GHTopoFPC_20140503/Outils_Calc/GestionResourceStrings.ods
  //********************************************************************************
  // /!\ DO NOT MODIFY - Generated automatically
  // Please use spreadsheet utility for modification of strings
  // Language: ES - Espanol
  // ================================================================================
interface
// CONST section
const
  NATIONAL_GEODESIC_SYSTEM_IDX = 1;
  DEFAULT_CODE_EPSG = 'UTM30';
// RESOURCESTRING section
resourcestring
    // Nombre del programa
    rsGHTOPOEXENAME       = 'GHTopo';
    rsMAINMENUCAPTION     = '%s - GHTopo';
    rsGHTOPOVERSION       = 'Versión 3.14159265 del %.2d/%.2d/%.4d';
    rsGHTOPOLICENSE       = 'Software bajo GPL' ;
    rsGHTOPOAUTHOR        = '(c) 1989 .. %d Jean Pierre Cassou';
    // Sesión informativa
    rsGHTOPORESOL         = 'Resolución %dx%d';
    rsGHTOPOPlatFormLinux = 'Plataforma: Linux';
    rsGHTOPOPlatFormWin32 = 'Plataforma: Microsoft Windows';
    // La lengua materna
    rsCHOOSELANGUAGE      = 'Lanzamiento en lengua española';
    // Fin GHTopo
    rsEND_OF_GHTOPO       = 'Fin de sessión GHTopo';
    // Tipo de interfaz
    rsTYPE_INTERFACE_TABLETTE   = 'Versión TabletPC';
    rsTYPE_INTERFACE_DESKTOP    = 'Versión de escritorio';

    // *********************************************
    // Resourcestring etiquetas comunes
    rsLBL_COMMENTS              = 'Comentarios';
    rsSERIE_INITIALISATION      = 'La serie 0 es inmutable';
    rsMSG_SERIE_INITIALISATION  = 'La Serie no puede ser cambiada';

    // Filtres de fichiers
    rsFILEFILTER_ALL = 'Todos (*.*)|*.*';
    rsGHTOPO_FILE_FILTER_WO_TEXT = 'Archivos GHTopo (*.xtb)|*.xtb|' +
                           'Archivos GHTopo XML (*.gtx)|*.gtx|' +
                           'Archivos Toporobot Tab [deprecated] (*.Tab)|*.Tab|' +
                           'Todos (*.*)|*.*';
    rsGHTOPO_FILE_FILTER_W_TEXT = 'Archivos GHTopo (*.xtb)|*.xtb|' +
                           'Archivos GHTopo XML (*.gtx)|*.gtx|' +
                           'Archivos Toporobot Tab [deprecated] (*.Tab)|*.Tab|' +
                           'Archivos Toporobot Text [deprecated] (*.Text)|*.Text|' +
                           'Archivos de texto PocketTopo (*.txt)|*.txt|' +
                           'Todos (*.*)|*.*';
    rsKML_FILE_FILTER = 'Archivos Google Earth (*.kml)|*.xtb|Todos (*.*)|*.*';
    rsGCD_FILE_FILTER = 'Poligonales GHCaveDraw (*.gcd)|*.gcd|Todos (*.*)|*.*';
    rsGPX_FILE_FILTER = 'Archivos GPS (*.gpx)|*.gpx|Todos (*.*)|*.*';
    rsOSM_FILE_FILTER = 'Archivos de trabajo OpenStreetMap (*.osm)|*.osm|Todos (*.*)|*.*';
    rsCSV_FILE_FILTER = 'Texto tabulado(*.csv)|*.csv|Todos (*.*)|*.*';
    rsVTOPO_FILE_FILTER   = 'Documento Visual Topo (*.tro)|*.tro|Todos (*.*)|*.*';
    rsTHERION_FILE_FILTER = 'Centerlines Therion (*.th)|*.th|Todos (*.*)|*.*';
    // *********************************************
    // Resourcestring menú principal
    rsMSGASSIGNCAPTIONS = 'Instalación de la interfaz';
    rsCALCURAPIDE       = 'Calculadora';
    rsMNU_FILE          = '&Archivo';
      rsNEW             = '&Nuevo';
      rsOPEN            = '&Abrir';
      rsSAVE            = '&Guardar';
      rsSAVEAS          = 'Guardar como';
      rsMNU_SAVELOG     = 'Guardar el historico';
      rsCLOSE           = '&Cerrar';
      rsRECENT_DOCS     = 'Documentos recientes';
      rsRELOAD          = 'Reabrir el documento';
      rsEDITTAB         = 'Editar archivo TAB';
      rsPRINT           = 'Im&primir';
      rsVTOPO               = 'Exportar por Visual Topo';
      rsEXPORT_THERION      = 'Exportar por Therion';
      rsEXPORT_TOPOROBOT    = 'Exportar hacia texto Toporobot (obsoleto)';
      rsEXPORT_POCKETTOPO   = 'Exportar hacia PocketTopo';
      rsEXPORT_COMPASS_PLT  = 'Exportar hacia Compass PLT';
      rsEXPORT_ABRIS        = 'Exportar hacia ABRIS (Android)';
      rsGHCAVEDRAW          = 'Poligonal para GHCaveDraw';

      rsEXPORT_GIS          = 'Exportar hacia programas de cartográfia';
      rsEXPORTGRAPHIQUE     = 'Exportar hacia programas gráficos (PS, DXF y SVG)';
      rsERRINFOXTB          = 'Lista de errores de lectura';
      rsGHTOPO_QUIT         = 'Salir de GHTopo';
      rsQUIT_EDITOR         = 'Salir del editor de texto';
      rsGENERER_DOSSIER_THERION = 'Generar una carpeta completa al formado Therion';

    rsMNU_EDITION           = '&Edición';
    rsMNU_TOPOGRAPHIE       = '&Topografia';
      rsCHECKBASE           = '&Verificar los datos';
      rsCOMPILE             = '&Calcular la red';
      rsVUEPLAN             = 'Planta';
      rsVUE3D               = 'Vista en 3D (GDI)';
      rsRENDU3D             = 'Rendición en 3D OpenGL';
      rsSTATISTIQUES        = 'Estadísticas';
      rsINFOCAVITE          = 'Informaciones sobre la red topografica';
      rsMAILLAGES_UTILS     = 'Modelos Numericos de Terreno';
      rsNODESCOORDINATES    = 'Coordenadas de los nudos';
    rsMNU_WINDOW            = '&Ventana';
    rsMNU_TOOLS             = '&Herramientas';
    rsMNU_HELP              = '&Ayuda';
      rsHLPINDEX            = 'Index';
      rsHLPNEWS             = 'Novedades';
      rsABOUT               = 'A Proposito de GHTopo';
    rsMNU_STAY_ON_TOP       = 'Poner adelante';
    // Título de la ventana
    rsWND_DATABASE          = 'Base de datos';
    rsWND_PLAN              = 'Planta';
    rsWND_CONSOLE           = 'Monitor';
    rsWND_DISTO_X           = 'DistoX';
    // Resourcestring el Asistente
    rsASSISTANT_SUCCESS     = 'Documento creado con éxito por el asistente';
    rsASSISTANT_RELOAD      = 'Cargando el documento';
    rsASSISTANT_ECHEC       = 'Error del asistente ';
    rsASSIST_TITLE          = 'Asistente para nueva cavidad';
    rsASSIST_BNGENERAL      = 'General';
    rsASSIST_BNENTRANCES    = 'Primera entrada';
    rsASSIST_BNCODES        = 'Primero código';
    rsASSIST_BNEXPES        = 'Primera sesión';
    rsASSIST_BNSERIE        = 'primera serie';
    rsASSIST_BNSAVE         = 'Guardar ...';
    rsASSIST_BNCANCEL       = 'Cancelar';
    rsASSIST_LBNOMETUDE     = 'Nombre del proyecto';
    rsASSIST_LBOBSETUDE     = 'Comentarios proyecto';
    rsASSIST_LBNOMENTREE    = 'Entrada principal';
    rsASSIST_LBCOORDS       = 'Información';
    rsASSIST_LBOBSENTREE    = 'Comentarios';
    rsASSIST_LBCONVERTISSEUR = 'Convesión de coordenadas ...';
    rsASSIST_LBREFSTATION    = 'Estación Inicial ';
    rsASSIST_LBREFSERIE      = 'Serie';
    rsASSIST_LBREFPOINT      = 'Punto';
    rsASSIST_LBCOMMENTAIRE   = 'Comentarios';
    rsASSIST_LBSYSTGEO       = 'Sistemas de coordenadas geográficas';
    // ********************************
    // Resourcestring el espectador plan de
    rsVUE2D_TITLE            = 'Vista en planta';
    rsVUE2D_DISPLAY_MASK     = 'Mostrar / Ocultar';
    rsVUE2D_DISPLAY_PARAMETRES_VUE = 'Ajustes monitor';
    rsVUE2D_DISPLAY_LISTE_SERIES   = 'Lista de la serie';
    rsVUE2D_DISPLAY_DIAGRAMMES     = 'Diagramas';
    rsVUE2D_REPRESENTATION_MODE    = 'Representación';
    rsVUE2D_REPRESENTATION_NETWORKS = 'Redes';
    rsVUE2D_REPRESENTATION_SECTEURS = 'Sectores';
    rsVUE2D_REPRESENTATION_EXPES    = 'Sesiones';
    rsVUE2D_REPRESENTATION_GRAY     = 'Gris';
    rsVUE2D_REPRESENTATION_DEPTH    = 'Profundidades';
    rsVUE2D_REPRESENTATION_TYPES    = 'Tipô de medida';

    rsVUE2D_REPRESENTATION_STATIONS = 'Puntos topo';
    rsVUE2D_LOAD_MAILLAGE           = 'Cargar un modelo digital del terreno';
    rsVUE2D_DISTANCE                = 'Distancia entre dos estaciones';
    rsVUE2D_FINDSTATION             = 'Búscar un punto topo';
    //
    // RsVUE2D_DEPTH_DIAGRAM = 'altímetro histograma';

    rsVUE2D_METAFILTRE              = 'Filtros';
    rsVUE2D_PANVUE                  = 'Vista de movimiento';

    rsVUE2D_PRINTING                = 'Imprime el mapa';
    rsVUE2D_REFRESH                 = 'Redibujar la vista';
    rsVUE2D_EXPORT_DXF              = 'Exportar hacia DXF';
    rsVUE2D_STATISTIQUES            = 'Estadísticas';
    rsVUE2D_VUE3D_GDI               = 'Vista 3D (sin OpenGL)';
    rsVUE2D_VUE3D_OPENGL            = 'OpenGL 3D Rendering';
    rsVUE2D_ZOOMALL                 = 'Zoom sobre toda la red';
    rsVUE2D_ZOOMFENETRE             = 'Zoom sobre ventana ';


    rsVUE2D_FMT_INFOS_STATION_RAPIDE = 'Estación: %s (%d.%d): X=%.2f Y=%.2f Z=%.2f';
    rsVUE2D_FMT_INFOS_ID_STATION     = '%d.%d [%s]';
    rsVUE2D_FMT_INFOS_MESURES        = 'L = %.2f, Az = %.2f, P =%.2f, G = %.2f, D = %.2f, H = %.2f, B = %.2f';
    rsVUE2D_FMT_INFOS_COORDONNEES    = 'X=%s, Y=%s, Z=%s; x=%s, y=%s, z=%s';
    rsVUE2D_EXPORT_XHTML             = 'Exportar hacia XHTML';
    rsVUE2D_EXPORT_SVG               = 'Exportar hacia SVG';
    // ************************************************ *******
    // pestañas resourcestring Administrador de la cavidad

    rsTBS_GENERAL                    = 'General';
    rsTBS_LISTES_SIMPLES             = 'Listas simples';
    rsTBS_TOUT                       = 'Todo';

    rsTBS_GENERAL_NOM                = 'Nombre de la red';
    rsTBS_GENERAL_OBS                = 'Comentarios';
    rsTBS_GENERAL_SYST               = 'Sistema de coordenadas';
    rsTBS_ENTRANCE                   = 'Entradas';
    rsTBS_SECTEURS                   = 'Sectores';
    rsTBS_CODES                      = 'Códigos';
    rsTBS_TRIPS                      = 'Sesiones';
    rsTBS_SERIES                     = 'Series';
    rsTBS_RESEAUX                    = 'Redes';
    rsTBS_ANTENNES                   = 'Medidas radiantes';
    rsTBS_SERIES_FLAT_TABLE          = 'Edición de series (experimental)';
    rsTBS_MAINTENANCE                = 'Mantenimiento';
    // ************************************************ *******
    // Resourcestring la sección General
    rsLB_NOM_ETUDE                   = 'Nombre del proyecto';
    rsLB_COMMENTAIRE_ETUDE           = 'Comentarios';
    rsLB_CODE_EPSG                   = 'Sistema de coordenadas Código EPSG';
    rsBTN_SELECT_EPSG                = 'Seleccionar ...';
    rsBTN_CALC_DECLIMAGS             = 'Calcular la variación magnética';
    // ************************************************ *******
    // Resourcestring Entradas marco
    rsCDR_ENTR_NOENTRANCE   = 'Entrada';
    rsCDR_ENTR_ENTRNAME     = 'Nombre de la entrada';
    rsCDR_ENTR_COORDINATES  = 'Información';
    rsCDR_ENTR_STATOFENTR   = 'Estación de referencia';
    // ************************************************ *******
    // Redes marco resourcestrings
    rsCDR_RESEAU_LBIDX     = 'Número';
    rsCDR_RESEAU_NAME      = 'Nombre';
    rsCDR_RESEAU_TYPE      = 'Tipo de red';
    rsCDR_RESEAU_CB0       = 'Cavidad natural';
    rsCDR_RESEAU_CB1       = 'Cavidad artificial';
    rsCDR_RESEAU_CB2       = 'Topo de superficie';
    rsCDR_RESEAU_CB3       = 'Thalweg';
    rsCDR_RESEAU_CB4       = 'Camino';
    rsCDR_RESEAU_CB5       = 'Ruta asfaltada';
    rsCDR_RESEAU_CB6       = 'Otro';
    // ************************************************ *******
    // Marco Sectores resourcestrings
    rsCDR_SECTEUR_LBIDX       = 'Número';
    rsCDR_SECTEUR_NAME        = 'Nombre';
    rsCDR_SECTEUR_COLOUR      = 'Color';
    // ************************************************ *******
    // Expediciones marco resourcestrings
    rsCDR_EXPE_SEANCE         = 'Sesión topo';
    rsCDR_EXPE_DATE           = 'Fecha';
    rsCDR_EXPE_DECLIMAG       = 'Declinación';
    rsCDR_EXPE_INCLIN         = 'Pendiente';
    rsCOLOR                   = 'Color';
    rsCDR_EXPE_SPELEOMETRE    = 'Operator';
    rsCDR_EXPE_SPELEOGRAPHE   = 'Asistente';
    // ************************************************ *******
    // Resourcestring códigos de cuadro
    rsCDR_CODES_NUMERO        = 'Código';
    rsCDR_CODES_TYPEGALERIE   = 'Tipo de la medida';
    // Tipos de galerías
    rsTYPE_VISEE_DEFAULT      = 'Por defecto';
    rsTYPE_VISEE_ENTRANCE     = 'Entrada';
    rsTYPE_VISEE_FOSSILE      = 'Fósil';
    rsTYPE_VISEE_VADOSE       = 'flujo libre';
    rsTYPE_VISEE_ENNOYABLE    = 'Ennoyable';
    rsTYPE_VISEE_SIPHON       = 'Sifón';
    rsTYPE_VISEE_FIXPOINT     = 'Punto fijo';
    rsTYPE_VISEE_SURFACE      = 'Medida de enlace';
    rsTYPE_VISEE_TUNNEL       = 'Túnel artificial';
    rsTYPE_VISEE_MINE         = 'Galerie de mina';
    rsTYPE_VISEE_ANTENNE      = 'Medida radiante';
    rsTITRE_SELECTEUR_VISEE   = 'Elegir el tipo de medida';
      rsCMBTYPE_D = '0 - No (fósiles)';
      rsCMBTYPE_E = '1 - Entrada';
      rsCMBTYPE_B = '2 - Galería fósil';
      rsCMBTYPE_V = '3 - Flujo libre';
      rsCMBTYPE_W = '4 - Sifón';
      rsCMBTYPE_C = '5 - Pasaje ennoyable';
      rsCMBTYPE_F = '6 - Punto Fijo';
      rsCMBTYPE_S = '7 - Topo de superficie';
      rsCMBTYPE_A = '8 - Túnel artificial';
      rsCMBTYPE_M = '9 - Galeria de mina';
    rsCDR_CODES_VISEE_HZ = 'Horizontal';
    rsCDR_CODES_VISEE_VT = 'Vertical';
    rsCDR_CODES_VDIRECT  = 'Directa';
    rsCDR_CODES_VINVERSE = 'Inversa';
    rsCDR_CODES_GRADCOMPAS = 'Graduación Brújula';
    rsCDR_CODES_GRADCLINO  = ' Clinómetro graduación';
    rsCDR_CODES_CMBUNIT_0  = '400 - Gon';
    rsCDR_CODES_CMBUNIT_1  = '360 - Degrees';
    rsCDR_CODES_CMBUNIT_2  = '370 - Porcentajes';
    rsCDR_CODES_CMBUNIT_3  = '380 - Desniveles';
    rsCDR_CODES_CMBUNIT_4  = '800 - Lasermetro con clinometro integrado';
    rsCDR_CODES_FACT       = 'Longitud x';
    rsCDR_CODES_POSZERO    = 'Posición cero';
    rsCDR_CODES_CMBZERO_0  = 'Nadiral';
    rsCDR_CODES_CMBZERO_1  = 'Horizontal';
    rsCDR_CODES_CMBZERO_2  = 'Zenithal';
    rsCDR_CODES_ANGLIMIT   = 'Angulo límite:';
    rsCDR_CODES_PRECISION  = 'Incertitud de los instrumentos';
    // *********************************************
    // Resourcestring serie del marco
    rsDLG_PJMNGR_MOVETOSERIE      = 'Cambiar la serie ?';
    rsDLG_PJMNGR_ADDSERIE         = 'Añadir la serie';
    rsCDR_SERIE_CHOOSE_SECTEUR    = 'Seleccionar sector';
    rsCDR_SERIE_CHOOSE_TYPE_VISEE = 'Seleccione un tipo de destino';
    rsCDR_SERIE_CHOOSE_CODE       = 'Seleccionar código de instrumentos';
    rsCDR_SERIE_CHOOSE_EXPE       = 'Seleccionar la sesión topo';
    rsCDR_SERIE_LB_RESEAU         = 'Red';
    rsCDR_SERIE_ADDPHOTO          = 'Añadir Foto';
    rsCDR_SERIE_NUMERO            = 'Serie';
    rsCDR_SERIE_NAME              = 'Nombre';
    rsCDR_SERIE_DEPART            = 'Salida';
    rsCDR_SERIE_ARRIVEE           = 'Llegada';
    rsCDR_SERIE_CHANCE            = 'Suerte';
      rsCDR_SERIE_CHANCE0         = 'Ninguna';
      rsCDR_SERIE_CHANCE1         = 'Baja';
      rsCDR_SERIE_CHANCE2         = 'Buena';
      rsCDR_SERIE_CHANCE3         = 'Excelente';
      rsCDR_SERIE_OBSTACLE        = 'Obstáculo';
      rsCDR_SERIE_OBSTACLE0       = 'Ningun';
      rsCDR_SERIE_OBSTACLE1       = 'Pozo';
      rsCDR_SERIE_OBSTACLE2       = 'Chimenea';
      rsCDR_SERIE_OBSTACLE3       = 'Estrecho';
      rsCDR_SERIE_OBSTACLE4       = 'Lago';
      rsCDR_SERIE_OBSTACLE5       = 'Sifón';
      rsCDR_SERIE_OBSTACLE6       = 'Derrumbe';
      rsCDR_SERIE_OBSTACLE7       = 'Concreción';
      rsCDR_SERIE_OBSTACLE8       = 'Sedimentos';
      rsCDR_SERIE_OBSTACLE9       = 'Otro';
      // Específico de Francia lol
      rsCDR_SERIE_OBSTACLE10 = 'Gases tóxicos';
      rsCDR_SERIE_OBSTACLE11 = 'Ocas agresivas';
      rsCDR_SERIE_OBSTACLE12 = 'Animales peligrosos';
      rsCDR_SERIE_OBSTACLE13 = 'Otro tipo (censorado)';
    rsCDR_SERIE_COL_POINT      = 'Punto';
    rsCDR_SERIE_COL_TYPE       = 'Tipo';
    rsCDR_SERIE_COL_ID_TERRAIN = 'ID Terreno';
    rsCDR_SERIE_COL_SECTEUR    = 'Sector';
    rsCDR_SERIE_COL_CODE       = 'Código';
    rsCDR_SERIE_COL_EXPE       = 'Sesión';
    rsCDR_SERIE_COL_LEGNTH     = 'Longitud';
    rsCDR_SERIE_COL_AZIMUTH    = 'Acimuto';
    rsCDR_SERIE_COL_INCLIN     = 'Pend.';
    rsCDR_SERIE_COL_LG         = 'Izqu.';
    rsCDR_SERIE_COL_LD         = 'Dere.';
    rsCDR_SERIE_COL_HZ         = 'Up';
    rsCDR_SERIE_COL_HN         = 'Down';




    rsCDR_SERIE_LOCKED       = 'Cerrado';
    rsCDR_SERIE_INSERTLINE   = 'Insertar líneas';
    rsCDR_SERIE_NBLINES      = 'Números de líneas';
    rsCDR_SERIE_DELETELINE   = 'Lineas Borrado';
    rsCDR_SERIE_UNDOCOPY     = 'Duplicar abajo';
    rsCDR_SERIE_INC_UNDOCOPY = 'Duplicar abajo incrementado';
    rsCDR_SERIE_IMPLEMENT    = 'OK';
    rsCDR_SERIE_ENTREE_RATT  = 'Entrada';
    rsINPUT_COMMENTAIRE_TITRE = 'Comentarios de estación';
    rsINPUT_COMMENTAIRE_MSG   = 'Introduzca el texto';
    rsCDR_SERIE_PASTE         = 'Importar desde el portapapeles';
    rsCDR_SERIE_BTN_GRD_COPY  = 'Copiar la tabla';
    rsCDR_SERIE_AC_ADD_LINE   = 'Insertar lineas';
    rsCDR_SERIE_AC_DEL_LINE   = 'Borrar lineas';
    rsCDR_SERIE_AC_UNDOCOPY   = 'Duplicar hacia abajo';
    rsCDR_SERIE_COPY_DATA     = 'Copiar datos al portapapeles';
    rsCDR_SERIE_PASTE_DATA    = 'Pegar desde el portapapeles';
    rsCDR_SERIE_EXTRACT_LABELS = 'Extractar ID Terreno desdecomentario' + #10 +
                                 'y copiarlo en la columna ID Terreno';
    rsCDR_SERIE_BTN_LAST_ENTRANCE = 'Más reciente entrada %d - %s';
    // Los mensajes de error
    rsCDR_SERIE_MSG_ERR_ENTREE_NOT_FOUND = 'Entrada no encontrada';
    rsCDR_SERIE_MSG_ERR_RESEAU_NOT_FOUND = 'Red no encontrada';
    rsCDR_SERIE_MSG_ERR_CODE_NOT_FOUND   = 'Código instrumentos no encontrado';
    rsCDR_SERIE_MSG_ERR_EXPE_NOT_FOUND   = 'Sesión Topo no encontrada';
    rsCDR_SERIE_MSG_ERR_SERIE_NOT_FOUND  = 'Serie no encontrada';
    rsCDR_SERIE_MSG_ERR_LONG             = 'La longitud debe ser positiva y inferior a %.0f m';
    rsCDR_SERIE_MSG_ERR_LRUD             = 'La distancia no debe ser negativa';
    rsCDR_SERIE_NB_ANTENNES_MODIFIEES    = 'Se ha modificado %d medidas radiantes';
    rsCDR_SERIES_MSG_ERROR_LONGUEURS_ADMISES     = 'Longitud %.3fm incorrecta - Valores válidas: %.3f hasta %.3f';
    rsCDR_SERIES_MSG_ERROR_ATTRIBUTED_NO_SERIE   = 'Número de serie ya asignado';
    rsCDR_SERIES_MSG_ERROR_ORPHAN_SERIE          = 'La serie no está conectada a la red';
    rsCDR_SERIES_MSG_ERROR_CHECKING_SERIE        = 'Comprobación valores';
    rsCDR_SERIES_MSG_ERROR_AZIMUT_OUT_OF_RANGE   = 'Acimut %.3f incorrecto - Valores válidas: %.3f hasta %.3f';
    rsCDR_SERIES_MSG_ERROR_PENTE_OUT_OF_RANGE    = 'Pendiente %.3f incorrecto - Valores válidas: %.3f hasta %.3f';
    rsCDR_SERIES_MSG_ERROR_NONEXISTENT_SECTEUR   = 'Sector %d inexistente';
    rsCDR_SERIES_MSG_ERROR_NONEXISTENT_RESEAU    = 'Red  %d inexistente';
    rsCDR_SERIES_MSG_ERROR_NONEXISTENT_CODE      = 'Código %d inexistente - Verificaciones angulares inoperantes';
    rsCDR_SERIES_MSG_ERROR_NONEXISTENT_EXPE      = 'Sesión %d inexistente';
    rsCDR_SERIES_MSG_ERROR_INVALID_TYPE_VISEE     = 'Tipo %d incorrecto - Valores válidas: %d% hasta %d';
    rsCDR_SERIES_MSG_ERROR_INVALID_LONGUEUR       = 'Longitud incorrecta (%.3f m) Valores válidas: desde %.3f hasta %.3f';
    rsCDR_SERIES_MSG_ERROR_INVALID_LG             = 'LI incorrecta (%.3f m) Valores válidas: desde %.3f hasta %.3f';
    rsCDR_SERIES_MSG_ERROR_INVALID_LD             = 'LD incorrecta (%.3f m) Valores válidas: desde %.3f hasta %.3f';
    rsCDR_SERIES_MSG_ERROR_INVALID_HZ             = 'HU incorrecta (%.3f m) Valores válidas: desde %.3f hasta %.3f';
    rsCDR_SERIES_MSG_ERROR_INVALID_HN             = 'HN incorrecta (%.3f m) Valores válidas: desde %.3f hasta %.3f';
    // ************************************************ *******
    // Marco Resoursestring de Antenas
    rsCDR_ANTENNES_AC_ADDLINE    = 'Añadir línea';
    rsCDR_ANTENNES_AC_DELLINE    = 'Eliminar linea';
    rsCDR_ANTENNES_AC_SAVEGRD    = 'Confirmar cambios';
    rsCDR_ANTENNES_DEL_LINE      = 'Borrar línea %d';
    rsCDR_ANTENNES_FIND_RESEAU   = 'Red para la línea %d';
    rsCDR_ANTENNES_FIND_CODE     = 'Código para la línea %d';
    rsCDR_ANTENNES_FIND_EXPE     = 'Sesión de la línea %d';
    // ************************************************ *******
    // Resourcestring CdrNavigateurDB trama
    rsCDR_NAVIG_DB_DO_SORT       = 'Ordenar';
    rsCDR_NAVIG_DB_DO_ADD        = 'Añadir elemento';
    rsCDR_NAVIG_DB_DO_DELETE     = 'Destruir elemento';
    // *********************************
    // Resourcestring diálogo VTOPO
    rsVTOPO_EDPREFIX             = 'Prefijo de estaciones';
    rsVTOPO_LBIDPTDEP            = 'ID Punto de salida';
    rsVTOPO_LBREPORTER           = 'Operator';
    rsLBFICHIER                  = 'Archivo';
    rsLBMAINENTRANCE             = 'Entrada principal';
    rsLBIDMAINST                 = 'Estación de salida';
    rsLBSTPREFIX                 = 'Prefijo de estaciones';
    rsLBREPORTER                 = 'Operator';
    rsLBMAINENTCOORD             = 'Coordenadas de la entrada principal';
    rsLBCOLORDEFAULT             = 'Color por defecto';
    // ************************************************ *******
    // Resourcestring el visor OpenGL
    rsOGLVIEWERTITLE             = 'Visualisator de OpenGL [%s]';
    rsLBLBACKCOLOR               = 'Fondo';
    rsLBLCUBECOLOR               = 'Cubo';
    rsLBLREFCOLOR                = 'Referencial';
    rsLBLREFSIZE                 = 'Tamaño';
    rsOPENGLERROR                = 'Error OpenGL';
    rsOGLVQUIT                   = 'Dejando el visor OpenGL';
    // ************************************************ *******
    // Resourcestrings la herramienta de exportación gráfico

    rsDODISPDESELECTEDPARTS = 'Mostrar las piezas rechazadas por el MétaFiltre - Color';
    rsTAB_LAYERS     = 'Pañal';
    rsTAB_QUADR      = 'Guía';
    rsTAB_DESSIN     = 'Dibujo';
    rsTAB_TEXTES     = 'Textos';
    rsTYPEQDR        = 'Tipo de cuadrícula';
      rsQDNONE       = 'none';
      rsQDCROSS      = 'Cuadricula';
      rsQDQUADRILLES = 'X';
    rsPSDXF_TITLE = 'Exportaciones Gráficas: [%s]';
      rsGRAPHICS_PS  = 'Postscript PS';
      rsGRAPHICS_DXF = 'AutoCAD DXF';
      rsGRAPHICS_SVG = 'Scalable Vector Graphics SVG';
      rsGRAPHICS_WMF = 'Windows Metafile WMF';
    rsDLGDXF_TITLE   = 'DXF: %s';
    // ************************************************ *******
    // Palabras clave de MétaFiltre
    rsHLPMETAFILTRE         = 'METAFILTRE';
    rsMETAFILTRE_APPLY      = 'Aplicar';
      rsMETAFILTRE_NIL      = 'NADA';
      rsMETAFILTRE_ALL      = 'TODO';
      rsMETAFILTRE_ID       = 'ID';
      rsMETAFILTRE_LENGTH   = 'LONGITUD';
      rsMETAFILTRE_AZIMUTH  = 'ACIMUTO';
      rsMETAFILTRE_PENTE    = 'PENDIENTE';
      rsMETAFILTRE_DATE     = 'FECHA';
      rsMETAFILTRE_COLOR    = 'COLOR';
      rsMETAFILTRE_X        = 'COORD_X';
      rsMETAFILTRE_Y        = 'COORD_Y';
      rsMETAFILTRE_Z        = 'COORD_Z';
      rsMETAFILTRE_LARGEUR  = 'ANCHO'; // anchura ?
      rsMETAFILTRE_HAUTEUR  = 'ALTURA';
      rsMETAFILTRE_DATES    = 'FECHAS';
      rsMETAFILTRE_COLORS   = 'COLORES';
      rsMETAFILTRE_SERIE    = 'SERIE';
      rsMETAFILTRE_RESEAU   = 'RED';
      rsMETAFILTRE_CODE     = 'CODIGO';
      rsMETAFILTRE_EXPE     = 'SESION';
      rsMETAFILTRE_TYPEVISEE    = 'TIPO_MEDIDA';
      rsMETAFILTRE_SECTEUR      = 'SECTOR';
      rsMETAFILTRE_ENTREE_RATT  = 'ENTRADA_REF';
    rsMSG_METAFILTRE_ALL_REJECTED = 'El MétaFiltre rechazó todos los afectados';
    // ************************************************ *******
    // Estaciones de investigación
    rsMSG_FIND_STATION_TITRE      = 'Buscar estación';
    rsMSG_FIND_STATION_PROMPT     = 'Introduzca un ID literal o un par de series y de la estación (separador: punto decimal)';
    // ************************************************ *******
    // Resourcestring diálogo de MétaFiltre
    rsDLGMETAFILTRE_TBS1          = 'Fechas';
    rsDLGMETAFILTRE_TBS2          = 'Colores';
    rsDLGMETAFILTRE_PERSO         = 'Personalizado';
    // ************************************************ ******
    // Resourcestring el selector de colores
    rsSELECTCOLORTITLE            = 'Selección de un color';
    rsLBLUSEDCOLORS               = 'Colores precedientes';
    rsDLGCOULSAVEPAL              = 'Guardar paleta';
    rsDLGCOULRESTPAL              = 'Restaurar paleta';
    rsDLGCOULFILTERPAL            = 'Archivos de paleta (*.PAL)|*.pal|Todos (*.*)|*.*';
    rsDLGCOUDELPAL                = 'Reemplazar el archivo existente';
    rsPALETTENOTFOUNT             = 'Paleta encontrada';
    // ************************************************ ******
    // Resourcestring la utilidad de exportación gráfica
    rsDLGGRAPHIC_OUTPUTFMT        = 'Formato de salida';
    rsDLGGRAPHIC_LBFILENAME       = 'Nombre del archivo:';
    rsDLGGRAPHIC_LBOBS            = 'Comentarios';
    rsDLG_GRAPHIC_TABTITLE        = 'Exportación gráfica';
    rsDLGGRAPHIC_GBCROIX          = 'Cuadracula';
    rsDLGGRAPHIC_GBCHEM           = 'Centerlines y secciones';
    rsDLGGRAPHIC_GBWALLS          = 'Paredes';
    rsDLGGRAPHIC_SPACING          = 'Spacing';
    rsDLGGRAPHIC_TYPEGRID         = 'Tipo';
    rsDLGGRAPHIC_CMBGRD2          = 'X';
    rdDLGGRAPHIC_LAYER            = 'Pañal';
    rdDLGGRAPHIC_WALLFILL         = 'Relleno';
    rsDLGGRAPHIC_WFILL1           = 'Completo (un solo color)';
    rsDLGGRAPHIC_WFILL2           = 'Tipos de galerías';
    rsDLGGRAPHIC_WFILL3           = 'Colores especificados';
    rsDLGGRAPHIC_WFILL4           = 'Redes';
    rsDLGGRAPHIC_WFILL5           = 'Fecha de medidas';
    rsDLGGRAPHIC_CHKCHEM          = 'Exportación centerlines';
    rsDLGGRAPHIC_CHKSECT          = 'Exportación secciones transversales';
    // ************************************************ ******
    // Resourcestring enumera el selector
    rsSELECT_LISTE_ENTRANCE        = 'Marcar un registro';
    rsSELECT_LISTE_RESEAU          = 'Seleccionar una red';
    rsSELECT_LISTE_SECTEUR         = 'Seleccionar un sector';
    rsSELECT_LISTE_CODE            = 'Seleccionar un código de instrumentos';
    rsSELECT_LISTE_EXPE            = 'Seleccionar una sesión';
    rsSELECT_LISTE_SERIE           = 'Seleccionar una serie';
    // ************************************************ ******
    // Resourcestring el centro de impresión
    rsPRN_NOPRINTER                = 'No hay impresora instalada';
    rsPRN_TBPRINTER                = 'Impresora';
    rsPRN_TBDRAW                   = 'Dibujo';
    rsPRN_TITLE                    = 'Centro de impresión [%s]';
    rsPRN_CHKPOLY                  = 'Poligonales';
    rsPRN_CHKFILL                  = 'Relleno';
    rsPRN_CHKWALLS                 = 'Paredes';
    rsPRN_CHKSECTS                 = 'Secciones';
    rsPRN_CHKSTATIONS              = 'Estaciones';
    rsPRN_CHKSTATIONS_LBL          = 'ID Estaciones';
    rsPRN_CHKALTITUDE              = 'Altura';
    rsPRN_CHKCOTE                  = 'Cotas';
    rsPRN_CHKQUADRILLAGE           = 'Cuadracula';
    rsPRN_CHKENTREES               = 'Entradas';
    rsPRN_CHKANNOTATIONS           = 'Anotaciones';
    rsPRM_LBANGLEROT               = 'Angulo de rotación';
    rsPRN_TYPEQDR                  = 'Tipo de cuadrícula';
    rsPRN_QDNONE                   = 'Ninguna';
    rsPRN_QDCROSS                  = 'X';
    rsPRN_QDQUADRILLES             = 'Cuadracula';
    rsPRN_QDPOINTS                 = 'Puntos';
    rsPRN_SCALING                  = 'Escalera';
    rsPRN_LBSPACING                = 'Separación';
    rsLANDSCAPE                    = 'Landscape';
    rsPORTRAIT                     = 'Portrait';
    rsPRN_START_PRINTING           = 'Inicio de la impresión';
    rsPRN_PRINTING_DONE            = 'Impresión finalizada';
    rsDLGIMP_TAB1                  = 'Previa general';
    rsDLGIMP_TAB2                  = 'Impresora';
    rsDLGIMP_TAB3                  = 'Opciones de Giro';
    rsDLGIMP_TAB4                  = 'Redes';
    rsQDRSPACING                   = 'Separación';
    rsECHELLE                      = 'Escala: 1 /';
    rsLAYERS                       = 'Pañales de dibujo';
    rsPREVIEW                      = 'Previa';
    rsSTARTPRINTING                = 'Inicio impresión';
    rsREGLE                        = 'Regla';
    // ------------------------------------------------ -
    // Resourcestring el sistema de ayuda
    rsHLP_TITRE                         = 'Sistema de ayuda de GHTopo';
    rsHLP_BNEDIT                        = 'Editar el archivo con';
    rsHLP_DOCONTINUE                    = '¿Quiere Vd continuar?';
    rsHLP_TAB_RUBRIQUES                 = 'Secciones de ayuda';
    rsHLP_TAB_ABOUT                     = 'A proposito de GHTopo';
    // ------------------------------------------------ -
    // Diálogo resourcestring Serie / estación
    rsDLG_SERST_TITLE                   = 'Estaciones';
    rsDLG_SERST_SERIE                   = 'Serie';
    rsDLG_SERST_STATION                 = 'Estación';
    rsDLG_SERST_CLE                     = 'Código de campo';
    rsDLG_SERST_LBSEARCH                = 'Buscar';
    rsDLG_SERST_BYSERST                 = 'Per par de series / estación';
    rsDLG_SERST_BYREFTER                = 'En la referencia de campo';
    // ------------------------------------------------ -
    // Resourcestring anotaciones editor de diálogo
    rsDLG_ANN_TITLE                    = 'Editor de anotaciones';
    rsDLG_ANN_LBTEXTE                  = 'Anotación';
    rsDLG_ANN_LBMAX                    = 'Max';
    rsDLG_ANN_CHKTXTDISP               = 'Dibujar esta anotación';
    rsDLG_ANN_GRBPOSTEXT               = 'Posicionamiento del texto';
    rsDLG_ANN_GRBMETH0                 = 'Coordenadas absolutas';
    rsDLG_ANN_GRBMETH1                 = 'El aferrarse a la estación';
    rsDLG_ANN_LBPOSTEXTE               = 'Punto de base';
    rsDLG_ANN_LBSTATION                = 'Estación topo';
    rsDLG_ANN_LBOFFSET                 = 'Desplazamiento (m): X = %.3f m; Y =%.3f m';
    rsDLG_ANN_GRBATTRTXT               = 'Atributos de caracteres';
    rsDLG_ANN_GRBBASEPT                = 'Base punto de texto';
    // ------------------------------------------------ -
    // La búsqueda de canales literal ID
    rsDLG_FIND_PT_BY_ID_TITLE          = 'Buscar literal ID';
    rsDLG_FIND_PT_BY_ID_PROMPT         = 'Código de terreno';
    rsDLG_FIND_PROMPT                  = 'Resultados de la búsqueda';
    rsDLG_FIND_ENTRANCE_BY_TEXT        = 'Buscar una entrada';
    rsDLG_FIND_RESEAU_BY_TEXT          = 'Buscar una red';
    rsDLG_FIND_CODE_BY_TEXT            = 'Buscar código de instrumentos';
    rsDLG_FIND_EXPE_BY_TEXT            = 'Buscar sesión topo';
    rsDLG_FIND_SERIE_BY_TEXT           = 'Buscar una serie';
    rsDLG_FIND_SECTEUR_BY_TEXT         = 'Buscar un sector';
    // Número de acceso
    rsDLG_GOTO_PROMPT                  = 'Número';
    rsDLG_GOTO_ENTRANCE_BY_NUMERO      = 'Ir a una entrada';
    rsDLG_GOTO_RESEAU_BY_NUMERO        = 'Ir a una red ';
    rsDLG_GOTO_SECTEUR_BY_NUMERO       = 'Ir a un sector';
    rsDLG_GOTO_CODE_BY_NUMERO          = 'Ir a un código de instrumentos';
    rsDLG_GOTO_EXPE_BY_NUMERO          = 'Ir a una sesión topo';
    rsDLG_GOTO_SERIE_BY_NUMERO         = 'Ir a una serie';

    // Resourcestring herramienta Calculadora / convertidores
    rsDLG_CALC_TITLE                   = 'Calculadora y herramientas adicionales';
    rsDLG_CALC_TAB_CAL                 = 'Calculadora';
    rsDLG_CALC_EXPR                    = 'Introduzca una expresión';
    rsDLG_CALC_DOCALC                  = 'Calcular';
    rsDLG_CALC_CDR_CONVERT             = 'Convertidor de coordenadas';
    rsDLG_CALC_CDR_DECLIMAG            = 'Declinación magnética';
    rsDLG_CALC_BTN_CONVERT             = 'Convertir';
    rsDLG_CALC_BTN_CONVERT_TABLEAU     = 'Convertir Puntos';
    rsDLG_CALC_BTN_ECHANGE             = 'Permutar';
    rsDLG_CALC_BTN_OPEN_CSV            = 'Cargar archivo CSV';
    rsDLG_CALC_BTN_PASTE               = 'Pegar desde portapapeles';
    rsDLG_CALC_BTN_COPY_TABLEAU        = 'Copiar';
    rsDLG_CALC_BTN_EXPORT_CSV          = 'Exportar tabla hacia CSV';
     rsDLG_CALC_BTN_EXPORT_GIS         = 'Exportar hacia KML, GPX o OSM';
    rsDLG_CALC_BTN_QRCODE_PT           = 'Generar QRCode de este punto';
    rsDLG_CALC_BTN_QRCODE_SEL          = 'Generar QRCode de la selección';
    rsDLG_CALC_BTN_COPY_COORDS_ISOLEES = 'Copiar estas coordenadas';
    rsDLG_CALC_BTN_QRCODE_FROM_TEXT    = 'Generar QRCode desde el texto';
    rsDLG_CALC_BTN_QRCODE_TO_SVG       = 'Crear archivo SVG desde QRCode';
    rsDLG_CALC_LB_SYST_SOURCE          = 'Sistema origen';
    rsDLG_CALC_LB_SYST_CIBLE           = 'Sistema destino';
    rsDLG_CALC_HINT_GRD_CONVERSIONS    = 'Introduzca los valores en la cuadrícula o pegar desde el portapapeles';
    rsDLG_CALC_EXPRESSION              = 'Expresión';
    rsDLG_CALC_SYSCOORDS               = 'Sistemas de coordenadas';
    rsDLG_CALC_LSB_FUNCTIONS           = 'Lista de funciones';
    rsDLG_CALC_LSB_CALCULS             = 'Lista de cálculos';
    rsDLG_CALC_COORDS_UNITAIRE         = 'Coordenadas aisladas';
    rsDLG_CALC_COORDS_TABLEAU          = 'Tabla de conversión';
    rsDLG_CALC_TAB_QRCODE              = 'Generación de QRCodes';
    rsDLG_CALC_TAB_PASCAL_SCRIPT       = 'PascalScript';
    rsDLG_CALC_TAB_TEXTFILES           = 'Importar archivos de texto';
    rsDLG_CALC_TAB_COORDS_QRCODE_LISTE = 'Generar un QRCode de estas coordenadas';
    rsDLG_CALC_DECL_UNITAIRE           = 'Declinación magnética';
    rsDLG_CALC_DECL_TABLEAU            = 'Tabla de déclinciones';

// A traduire
// gestion du presse papiers pour l''import de données dans la grille Stations
  rsDLG_CLIPBRD_PTS_TITLE                    = 'Importación de puntos topo';

  // resourcestring du cadre Série
  rsCDR_SERIES_ADD_SERIE                     = 'Añadir une serie';
  rsCDR_SERIES_VALID_SERIE                   = 'Guardar modificaciones de este serie';
  rsCDR_SERIES_SORT_SERIES                   = 'Ordenar las series';
  rsCDR_SERIES_EXPORT_CSV                    = 'Exportar las cabecerasen un archivo CSV';
  rsCDR_SERIES_HELP                          = 'Ayuda';

  rsDLG_BDD_APPLY_MODIFS                     = 'Modificar la linea corriente';
  rsDLG_BDD_FIND                             = 'Buscar';
  rsDLG_BDD_COTO_BY_NUMERO                   = 'Ir al itém numero n';
  rsDLG_BDD_SORT                             = 'Ordenar';
  rsDLG_BDD_EXPORT_CSV                       = 'Exportar la lista en CSV';
  rsDLG_BDD_HELP_LISTES                      = 'Ayuda';

  // resourcestring du cadre Listes simples

  rsDLG_BDD_REMOVE_ENTREE                    = 'Suprimir une entrada';
  rsDLG_BDD_REMOVE_RESEAU                    = 'Suprimir un red';
  rsDLG_BDD_REMOVE_SECTEUR                   = 'Suprimir un sector';
  rsDLG_BDD_REMOVE_CODE                      = 'Suprimir un código de instrumentos';
  rsDLG_BDD_REMOVE_EXPE                      = 'Suprimir une sesión';
  rsDLG_BDD_REMOVE_SERIE                     = 'Suprimir une serie';

  rsDLG_BDD_ADD_ENTREE                       = 'Añadir une entrada';
  rsDLG_BDD_ADD_RESEAU                       = 'Añadir un red';
  rsDLG_BDD_ADD_SECTEUR                      = 'Añadir un sector';
  rsDLG_BDD_ADD_CODE                         = 'Añadir un código de instrumentos';
  rsDLG_BDD_ADD_EXPE                         = 'Añadir une sesión';
  rsDLG_BDD_ADD_SERIE                        = 'Añadir une serie';


  rsDLG_BDD_INSERT_ENTREE                    = 'Insertar une entrada';
  rsDLG_BDD_INSERT_RESEAU                    = 'Insertar un red';
  rsDLG_BDD_INSERT_SECTEUR                   = 'Insertar un sector';
  rsDLG_BDD_INSERT_CODE                      = 'Insertar un código de instrumentos';
  rsDLG_BDD_INSERT_EXPE                      = 'Insertar une sesión';
  rsDLG_BDD_INSERT_SERIE                     = 'Insertar une serie';

  // messages divers
  rsMSG_HAS_ALREADY_DATA                     = 'Tabla ya contiene datos. Continuar';
  rsDELETEITEM                               = 'Destruir elemento';
  rsONCLOSEPRJMNGR                           = 'Esto cerrará el corriente documento  - Guardar modificaciones';
  rsMSG_SEEALSO                              = 'Ver también ...';
  rsMSG_NDSNEEDED                            = 'Fichiero de nudos inexistente - Remedio: Recalcular la red';
  rsMSG_SAVECHANGES                          = 'Guardar modificaciones';
  rsMSG_ERASESTATION                         = 'Borrar la estación %d';
  rsMSG_FILENOTFOUND                         = 'Archivo %s no encontrado';
  rsMSG_READY                                = 'LISTO';
  rsMSG_NOFILEOPENED                         = 'No archivo abierto';
  rsDISPLAY_HELP_SYSTEM                      = 'Iniciando sistema de ayuda';
  rsHLPCENTER_TITLE                          = 'Sistema de ayuda de GHTopo';
  rsMATCHNOTFOUND                            = 'Coincidir que no se encuentra';
  rsNOCANCLOSE                               = 'Quitter en sauvegardant';
  rsWARN_FILEALREADYOPEN                     = 'Un document est déjà ouvert - Poursuivre ?';
  rsSAVECHANGES                              = 'Enregistrer les modificacións';
  rsNOCANCLOSEWND                            = 'Fenêtre permanente de GHTopo, ne peut être fermée';
  rsERASEEXISTNAMEDFILE                      = 'Sobrescribir archivo %s';
  rsSAVESLOST                                = 'Los cambios se perderán';
  rsCFGMTFLTR_UNABLE                         = 'Eoor en el archivo de filtros personalizados';
  rsERRORSTARTPRNCTR                         = 'Error iniciando el Centro de Impresion';
  rsNOPRINTERINSTALLED                       = 'No hay impresora installada';
  rsERRORLOADINGTOP                          = 'Error cargando el archivo TOP';
  rsIDSERIE_EXISTS                           = 'Index de serie %d ya atribuado (série #%d)';
  // Messages d''erreur
  rsMSG_VUE3D_FAIL                           = 'Fracaso iniciando visualizador 3D';
  rsMSG_VUE2D_FAIL                           = 'Fracaso iniciando visualizador en planta';
  rsMSG_STATS_DLG_FAIL                       = 'Fracaso iniciando el diálogo de estádisticas';
  rsMSG_PRINT_CENTER_FAIL                    = 'Fracaso iniciando el centro de impresion';
  rsMSG_PROJ_MANAGER_FAIL                    = 'Fracaso iniciando el frontal de gestión de la base';
  rsMSG_PROJ4S_FAIL                          = 'Fracaso iniciando el convertidor de coordenadas Proj4s';
  rsMSG_DECLIMAG_FAIL                        = 'Fracaso iniciando el calculador de déclinaciones magnéticas';
  rsMSG_TABLE_ENTITY_FAIL                    = 'Fracaso cargando tablas de entidades';
  rsMSG_CODE_NOT_FOUND                       = 'Código no encontrado';
  rsMSG_EXPE_NOT_FOUND                       = 'Sesión no encontrada';
  rsMSG_RESEAU_NOT_FOUND                     = 'Red no encontrado';
  rsMSG_SECTEUR_NOT_FOUND                    = 'Sector no encontrado';
  rsMSG_ANGLE_OUT_OF_RANGE                   = '%s debe de ser includo entre %.2f y %.2f';
  rsMSG_WARN_DELETE_ITEM                     = 'La supresión del objecto %d puede desorganizar la base. Perseguir ?';
  // Messages dans TToporobotStructure.LoadFichierTab()
  rsRD_TAB_MSG_ERR                           = 'Error tratando linea: ';
  rsRD_TAB_D_MULTI_OBS                       = 'Inicio de comentarios multilineas en linea %d';
  rsRD_TAB_F_MULTI_OBS                       = 'Fin de comentarios multilineas en linea %d';
  rsRD_TAB_STOP_9999                         = '*** Linea %d: Lectura deteneda por el código -9999 ***';
  rsRD_TAB_LN_OBS                            = '-- Linea %d es un comentario';
  rsRD_TAB_LASTSAVES                         = 'Ultimo guardo: %s %s';
  rsRD_TAB_CLASSEURS                         = '-7 [Classeurs] Esta funcionalidad no esta implementada';
  rsRD_TAB_ENTRANCE                          = 'Entrada creada #%d';
  rsRD_TAB_ENTR_NOGEOREF                     = '[Advertencia] (Ligne %d): Entrada %d (%s) no esta georeferenciada';
  rsRD_TAB_ENTR_BADLINK                      = '[Advertencia] (%d): Entrada %d (%s): Raccordement incorrect [Serie: %d - Estación: %d]';
  rsRD_TAB_IGNORED_SEC                       = '[Información] Sección ignorada';
  rsRD_TAB_BAD_TRIP                          = 'Sesión incorrecta';
  rsRD_TAB_BAD_CODE                          = 'Código incorrecto';
  rsRD_TAB_BAD_DATE                          = '[Advertencia] (%d) - Fecha incorrecta - Se convierte en la fecha actual';
  rsRD_TAB_BAD_IDXRES                        = '[Advertencia] (%d) - Index de red incorrecto (%d) para la série %d - Puesto a 0';
  rsRD_TAB_IDEXTR_MM                         = '[Advertencia] (%d) - Mismos ID de extremedades de la serie';
  rsRD_TAB_SELF_CLOSURE                      = '[Advertencia] (%d) - La serie %d se enchufa en el propio.';
  rsRD_TAB_NEG_LONG                          = '[Advertencia] (%d) - Longitud negativa (%.2f m), cambiada de signo';
  rsRD_ERROR_LN                              = '*** Error en linea #%d: %s';
  rsRD_CONTNT_LN                             = '-- Contenido de la linea: %s';
  rsRD_TAB_FIELD_VALUES                      = '-- Valores de los campos:';
  rsRD_TAB_NOFATAL_ERR                       = 'El archivo no contiene errores fatales';
  rsRD_TAB_WTFATAL_ERR                       = 'El archivo contiene %d errores';
  // *************************************************************
  // resourcestring du code de calcul
  rsDEFAULT                                  = 'Por defecto';
  rsFORFIXEDPOINTS                           = 'Item por entradas y puntos fijos';
  rsWARNINGENTRYADDED                        = 'El número de entradas dans -6 es diferente de el de la seccion -5 - Corregido.';
  rsCALCULNODES                              = 'CALCULACION COORDENADAS DE LOS NUDOS';
  rsLINEOFNB                                 = '---> Linea %d / %d';
  rsBINDMATRIX                               = 'MATRIZ DE INCIDENCIA [R]';
  rsNB_NON_NUL_TERMES                        = '%d términos distinto de cero en la matriz [R]';
  rsPR100_NON_NULS                           = 'i.e. %.3f%% de (%d x %d) = %d términos';
  rsFINDING_BRANCHES                         = 'Extractando ramos';
  rsADDENTRANCES                             = 'Añadiendo entradas';
  rsBUILDMATRICE                             = 'CONSTRUCCION DE LA MATRICE DE COMPENSACION: B = Rt.Wi.R';
  rsBUILDMATRICE_LAPLACIENNE                 = 'CONSTRUCCION DE LA MATRICE DE COMPENSACION: B = Rt.R';

  rsFIND_SUM_LIMITS                          = '-- Buscando limites de indices de sommación';
  rsFACTORISEMATRICE                         = 'FACTORIZACION DE LA MATRIZ DE COMPENSACION';
  rsFACTORISEMATRICE_LAPLACIENNE             = 'FACTORIZACION DE LA MATRIZ DE COMPENSACION';
  rsAXIS                                     = 'Eje:';
  rsDESCENTE                                 = '--> Descenso: V.V* = A';
  rsREMONTEE                                 = '--> Ascenso del sistema';
  rsTRIANGULARISATION                        = '--> Triangularización';
  rs2NDMEMBER                                = '- Secondo miembro';
  rsCOMPESMATRIX                             = '- Matriz de compensación';
  rsNODECOORD                                = '- Factorisación: Coordenadas de los nudos';
  rsCOORDONNEES_OK                           = '--> Coordonnées des %d noeuds OK';
  rsDEL_TEMP_MATRIX                          = '--> Destrucción des matrices temporaires';
  rsWRITE_NODES_COORDS                       = 'Escribiendo coordenadas de los nudos en: ';
  rsNODES_COORDS_FOR_DB                      = 'Coordonnées des noeuds pour:';
  rsCALCULCONTOURS                           = 'CALCUL CONTORNOS GALERIAS';
  rsCALCUL_ANTENNES                          = 'Calculando medidas radiantes';
  rsRECENSEM_JONC                            = '-- Buscando junccións';
  rsPURGE_TABLE_JONC                         = '-- Inicio tabla de las junccións';
  rsPURGE_TABLE_BRCH                         = '-- Inicio tabla de los ramos';
  rsNB_BRCHS                                 = '%d ramos';
  rsSTEP_CALC_01                             = '->Etapa %d / %d: Matriz de incidencia R';
  rsSTEP_CALC_02                             = '->Etapa %d / %d: Matriz de pondéración W';
  rsSTEP_CALC_03                             = '->Etapa %d / %d: Matriz de compensación B = Rt.W.R';
  rsSTEP_CALC_04                             = '->Etapa %d / %d: Coordenadas de los nudos';
  rsSTEP_CALC_05                             = '->Etapa %d / %d: Repartición des écarts';
  rsSTEP_CALC_06                             = '->Etapa %d / %d: Libéración de memoria y fin de tratamiento';
  rsFREE_TEMP_VARS                           = 'LIBERACION VARIABLES TEMPORALES';
  rsREPARTIR_ECARTS                          = 'REPARTICION DE ERRORES';
  rsSCAN_BRCHS                               = '-- Escaneando ramos';
  rsCHECK_VISEE_VALID_INTERVAL               = '%s debe de ser incluido entre %.2f %s y %.2f %s';
  rsCHECK_VISEE_VALID_TYPE_VISEE             = 'Tipo de medida debe de ser incluido entre %d y %d';

  // lecture des fichiers
  rsSEPARATOR_LINE                           = '-------------------------------';
  rsWARNINGS_READFILE                        = 'Advertenciad de lectura de:';
  rsCONV_TAB_MAC_UNIX                        = '-> Conversion de archivos TAB desde Mac o Unix';
  // -------------------------------------------------

  // -------------------------------------------------
  // Misc
  rsSELECTALL                                = 'Todo seleccionar';
  rsDESELECTALL                              = 'Todo deseleccionar';
  rsOBS                                      = 'Comentarios';
  rsFILTERS                                  = 'Filtros';
  rsCHKSELECTALL                             = 'Sel. todo';
  rsCHKDESELECTALL                           = 'Desel. todo';
  rsCHKREVERSE                               = 'Inv. sel.';
  rsMAIN_NETWORK                             = 'Red principal';
  // -------------------------------------------------
  // resourcestrings de dlgSelectDansListes
  rsSELECT_LISTE_GENERAL_ID                  = 'ID';
  rsSELECT_LISTE_NB_ELEMENTS                 = 'Lista de %d %s';
  rsSELECT_LISTE_OBSERV                      = 'Observaciones';
  rsSELECT_LISTE_ENTREES                     = 'Entradas';
  rsSELECT_LISTE_ENTREES_ID                  = 'ID';
  rsSELECT_LISTE_ENTREES_NOM                 = 'Entrada';
  rsSELECT_LISTE_ENTREES_REF                 = 'Ref.';
  rsSELECT_LISTE_ENTREES_X                   = 'X';
  rsSELECT_LISTE_ENTREES_Y                   = 'Y';
  rsSELECT_LISTE_ENTREES_Z                   = 'Z';
  rsSELECT_LISTE_RESEAUX                     = 'Redes';
  rsSELECT_LISTE_RESEAUX_ID                  = 'ID';
  rsSELECT_LISTE_RESEAUX_COLOR               = 'Color';
  rsSELECT_LISTE_RESEAUX_NOM                 = 'Red';
  rsSELECT_LISTE_SECTEURS                    = 'Sectores';
  rsSELECT_LISTE_SECTEURS_ID                 = 'ID';
  rsSELECT_LISTE_SECTEURS_COLOR              = 'Color';
  rsSELECT_LISTE_SECTEURS_NOM                = 'Sector';
  rsSELECT_LISTE_CODES                       = 'Códigos';
  rsSELECT_LISTE_CODES_ID                    = 'ID';
  rsSELECT_LISTE_CODES_AZIMUTS               = 'Acimutos';
  rsSELECT_LISTE_CODES_PENTES                = 'Pendientes';
  rsSELECT_LISTE_CODES_OBS                   = 'Comentarios';
  rsSELECT_LISTE_EXPES                       = 'Sesiones';
  rsSELECT_LISTE_EXPES_ID                    = 'ID';
  rsSELECT_LISTE_EXPES_COULEUR               = 'Color';
  rsSELECT_LISTE_EXPES_DATE                  = 'Fecha';
  rsSELECT_LISTE_EXPES_SPELEOMETRE           = 'Operator 1';
  rsSELECT_LISTE_EXPES_SPELEOGRAPHE          = 'Operator 2';
  rsSELECT_LISTE_EXPES_DECLINAISON           = 'Declinación';
  rsSELECT_LISTE_EXPES_OBS                   = 'Comentarios';
  rsSELECT_LISTE_SERIES                      = 'Series';
  rsSELECT_LISTE_SERIES_ID                   = 'ID';
  rsSELECT_LISTE_SERIES_DEPART               = 'Salida';
  rsSELECT_LISTE_SERIES_ARRIVEE              = 'Llegada';
  rsSELECT_LISTE_SERIES_NOM                  = 'Nombre';
  rsSELECT_LISTE_SERIES_RESEAU               = 'Red';
  rsSELECT_LISTE_SERIES_NBPOINTS             = 'Nb puntos';
  rsSELECT_LISTE_DATES                       = 'Fechas';
  rsSELECT_LISTE_UNE_DATE                    = 'Fecha';
  rsSELECT_LISTE_STATIONS                    = 'Puntos topo';
  rsSELECT_LISTE_STATIONS_ID                 = 'ID';
  rsSELECT_LISTE_STATIONS_NB                 = 'Nb estaciones';
  rsSELECT_LISTE_STATIONS_LABEL              = 'ID literal';
  rsSELECT_LISTE_STATIONS_X                  = 'X';
  rsSELECT_LISTE_STATIONS_Y                  = 'Y';
  rsSELECT_LISTE_STATIONS_Z                  = 'Z';
  // -------------------------------------------------
  // resourcestrings de dlgExportVersSIGExt
  rsEXPORT_SIG_TITRE                         = 'Exportar hacia SIG';
  rsEXPORT_SIG_SYST_COORDONNEES              = 'Sistema de coordenadas';
  rsEXPORT_SIG_SIG_CIBLE                     = 'Formado de salida';
  rsEXPORT_SIG_LB_FICHIER                    = 'Archivo';
  rsEXPORT_SIG_USE_COLORS                    = 'Usar colores de grupos';
  rsEXPORT_SIG_SILHOUETTE                    = 'Siluetas';
  rsEXPORT_SIG_BTN_GO                        = 'Generar';
  rsEXPORT_SIG_BTN_PREFIX                    = 'Prefijo';
  // -------------------------------------------------
  // resourcestrings de dlgStatistiquesExt
  rsSTATISTIQUES_TITRE                       = 'Estadisticas';
  rsSTATISTIQUES_TAB_TABLEAUX                = 'Tablas';
  rsSTATISTIQUES_TAB_SYNTHESE                = 'Sintésis';
  rsSTATISTIQUES_TAB_RESEAU_SECTEURS         = 'Redes y sectores';
  rsSTATISTIQUES_TAB_DIAGRAMMES              = 'Diagramas';
  rsSTATISTIQUES_TAB_CORDONNEES              = 'Coordenadas de las estaciones';
  rsSTATISTIQUES_LBL_TITRE_ROSE_DIAGRAM      = 'Histograma de direcciones';
  rsSTATISTIQUES_LBL_TITRE_DEPTH_DIAGRAM     = 'Histograma de alturas';
  rsSTATISTIQUES_LBL_LONG_MINI_VISEE         = 'Longitud mini';
  rsSTATISTIQUES_LBL_NB_PETALES              = 'Nb de pétalos';
  rsSTATISTIQUES_LBL_NB_BARRES               = 'Nb de barras';
  rsSTATISTIQUES_BTN_REDESSINER              = 'Redibujar';
  rsSTATISTIQUES_CMB_MODE_TABLE0             = 'Redes';
  rsSTATISTIQUES_CMB_MODE_TABLE1             = 'Sectores';
  rsSTATISTIQUES_CMB_MODE_TABLE2             = 'Códigos instrumentos';
  rsSTATISTIQUES_CMB_MODE_TABLE3             = 'Sesiones';
  rsSTATISTIQUES_CMB_MODE_TABLE4             = 'Fechas';
  rsSTATISTIQUES_NB_RESEAUX                  = '%d redes';
  rsSTATISTIQUES_NB_SECTEURS                 = '%d sectores';
  rsSTATISTIQUES_NB_CODES                    = '%d códigos';
  rsSTATISTIQUES_NB_SEANCES                  = '%d sesiones';
  rsSTATISTIQUES_NB_DATES                    = '%d fechas';
  rsSTATISTIQUES_TYPE_SHOT_FOSSILES          = 'Fosil';
  rsSTATISTIQUES_TYPE_SHOT_VADOSES           = 'Flujo libre';
  rsSTATISTIQUES_TYPE_SHOT_ENNOYABLES        = 'Inundables';
  rsSTATISTIQUES_TYPE_SHOT_SIPHONS           = 'Sifones';
  rsSTATISTIQUES_TYPE_SHOT_NATURELS          = 'Naturales';
  rsSTATISTIQUES_TYPE_SHOT_SPECIAUX          = 'Especiales';
  rsSTATISTIQUES_TYPE_SHOT_TUNNELS           = 'Tuneles';
  rsSTATISTIQUES_TYPE_SHOT_MINES             = 'Minas';
  rsSTATISTIQUES_COPY_TABLEAU                = 'Copiar la tabla';
  // resourcestrings de dlgUtilsDistoX
  rsUTILS_DISTOX_CAPTION                     = 'Utilitarios DistoX [%s]';
  // resourcestrings de dlgCoupeDeveloppee
  rsCOUPE_DEVEL_AC_OPEN_COUPE                = 'Abrir una corte desarrollada';
  rsCOUPE_DEVEL_AC_SAVE_COUPE                = 'Guardar la corte desarrollada';
  rsCOUPE_DEVEL_AC_EXPORT_SVG                = 'Exportar en SVG';
  rsCOUPE_DEVEL_AC_REVERSE_BRANCHE           = 'Rotar ramo';
  rsCOUPE_DEVEL_AC_ADD_SERIE                 = 'Añadir una serie';
  rsCOUPE_DEVEL_AC_REMOVE_SERIE              = 'Retirar la serie';
  rsCOUPE_DEVEL_AC_MAKE_COUPE                = 'Construir la corte desarrollada';
  rsCOUPE_DEVEL_AC_DO_REMOVE_SERIE           = 'Retirer la série';



  // resourcestrings de CadreListeSeriesFlatMode
  rsCDR_SERIES_FLAT_TAB_GOTO_LINE            = 'Ir hasta la linea';
  rsCDR_SERIES_FLAT_TAB_FIND                 = 'Buscar';
  rsCDR_SERIES_FLAT_TAB_LOAD_BUFFER          = 'Cargar el buffer';
  rsCDR_SERIES_FLAT_TAB_SAVE_BUFFER          = 'Guardar el buffer';
  rsCDR_SERIES_FLAT_TAB_COPY_TABLEAU         = 'Copiar la tabla';
  rsCDR_SERIES_FLAT_TAB_INSERT_LINE          = 'Insertar linea';
  rsCDR_SERIES_FLAT_TAB_REMOVE_LINE          = 'Suprimir linea';
  rsCDR_SERIES_FLAT_TAB_INSERT_SERIE_HERE    = 'Nueva serie desde este punto';
  rsCDR_SERIES_FLAT_TAB_CONTINUE_HERE        = 'Perseguir esta serie';
  rsCDR_SERIES_FLAT_TAB_PARSE_TABLEAU        = 'Analizar la tabla';
  rsCDR_SERIES_FLAT_TAB_SORT_TABLEAU         = 'Ordenar la tabla en orden ascendente de las series';
  rsCDR_SERIES_FLAT_EDIT_SERIE_BY_DIALOG     = 'Editar esta serie en un dialogo';
  rsCDR_SERIES_FLAT_TAB_UNDOCOPY             = 'Dublicar hacia abajo';
  rsCDR_SERIES_FLAT_MSG_ROW_UNDELETABLE      = 'La linea %d contiene una cabecera de serie y no puede ser suprimida';
  rsCDR_SERIES_FLAT_MSG_BUFFER_SAVED         = 'Buffer %s guardado en %s';
  rsCDR_SERIES_FLAT_MSG_REMOVE_LINE          = 'Suprimir %d lineas desde linea %d';
  rsCDR_SERIES_FLAT_MSG_REPLACE_BUFFER       = 'Replazar datos de la tabla';
  rsCDR_SERIES_FLAT_MSG_CONTINUE_THIS_ACTION = 'Efectuar este acción';
  rsCDR_SERIES_FLAT_NEW_SERIE                = 'Nueva serie';
  rsCDR_SERIES_FLAT_MSG_PARSING              = 'Analizando ...';
  rsCDR_SERIES_FLAT_MSG_PARSING_LINES        = 'Procesando %d lineas';
  rsCDR_SERIES_FLAT_MSG_DO_CREATE_SERIE_NTH  = 'Crear la serie %d';
  rsCDR_SERIES_FLAT_MSG_NB_LINES             = 'Número de lineas';
  rsWARN_SERIE_REVERSE                       = 'La serie %d llega en la red por la estación %d.%d';
  rsERR_SERIE_START_STATION_NOT_FOUND        = 'Estación de salida no encontrada';
  rsERR_SERIE_END_STATION_NOT_FOUND          = 'Estación de llegada no encontrada';
  rsWARN_SERIE_CONNECTED_FIXPT               = 'La serie %d llega sobre punto fijo o salida de una otra serie: %d.%d';
  rsERR_RESEAU_NOT_FOUND                     = 'Red no encontrada';
  rsERR_ENTRANCE_NOT_FOUND                   = 'Entrada no encontrada';


  // dlialogue Recherche de stación
  rsDLG_FIND_STATION_TITLE                   = 'Buscar estación topo';
  rsDLG_FIND_STATION_PROMPT                  = 'Etiqueta del punto topo';
  rsDLG_FIND_STATION_DO_MATCH                = 'Correspondencia exacta';

  // resourcestring du cadre Visualisateur 2D
  rsVUE2D_WINDOW_TITLE                       = 'Vista en planta';
  rsCDR_METAFILTRE_PROMPT                    = 'Filtros';
  rsCDR_METAFILTRE_CHK_ACTIF                 = 'Activo';
  rsCDR_VUE2D_TAB_VUE_INITIALE               = 'Vista inicial';
  rsCDR_VUE2D_TAB_VUE                        = 'Vista %0.2d';
  rsCDR_VUE2D_AC_ZOOM_ALL                    = 'Zoom sobre todo';
  rsCDR_VUE2D_AC_ZOOM_WINDOW                 = 'Zoom sobre ventana';
  rsCDR_VUE2D_AC_PAN_VUE                     = 'Pan vista';
  rsCDR_VUE2D_AC_DISTANCE_BT_STATIONS        = 'Distancia entre dos estaciones topo';
  rsCDR_VUE2D_MNU_FILTRES_STATION            = 'Filtros según la estación: %d.%d [%s]';
  // titres du sous-menu
  rsCDR_VUE2D_AC_METAFILTRE_SERIE            = 'Serie %d';
  rsCDR_VUE2D_AC_METAFILTRE_TOPO_DU_JOUR     = 'Medidas del %s';
  rsCDR_VUE2D_AC_METAFILTRE_RESEAU           = 'Red %d';
  rsCDR_VUE2D_AC_METAFILTRE_SECTEUR          = 'Sector %d';
  rsCDR_VUE2D_AC_METAFILTRE_CODE             = 'Código %d';
  rsCDR_VUE2D_AC_METAFILTRE_EXPE             = 'Sesión %d';
  rsCDR_VUE2D_AC_METAFILTRE_YEAR             = 'Año %d';
  rsCDR_VUE2D_AC_METAFILTRE_CURRENT_VIEW     = 'Filtros sobre la vista actual';
  rsCDR_VUE2D_AC_ISOVALEUR_Z                 = 'Curva de nivel para la altura %.0f m';
  // message Recalculer le réseau
  rsMSG_QUESTION_RECALCULER                  = 'Recalcular la red';

  // fenêtre Visualisateur 2D
  rsVUE2D_GRBX_ELEMENTS_DRAWN                = 'Elementos dibujados';
    rsVUE2D_CHK_DRW_STATIONS        = 'Estaciones';
    rsVUE2D_CHK_DRW_ENTRANCES       = 'Entradas';
    rsVUE2D_CHK_DRW_WALLS           = 'Paredes';
    rsVUE2D_CHK_DRW_CENTERLINES     = 'Poligonales';
    rsVUE2D_CHK_DRW_ALTITUDES       = 'Alturas';
    rsVUE2D_CHK_DRW_COTES           = 'Cotas';
    rsVUE2D_CHK_DRW_ANTENNES        = 'Medidas radiantes';
    rsVUE2D_CHK_DRW_IDStations      = 'ID';
    rsVUE2D_CHK_DRW_SECTIONS        = 'Secciones';
    rsVUE2D_CHK_DRW_FILL            = 'Relleno';
    rsVUE2D_CHK_DRW_QUADRILLAGE     = 'Cuadricula';
  rsVUE2D_GRBX_DEGRADE_ALTITUDES    = 'Gradiente de alturas';
  rsVUE2D_PRMS_DRW_BTN_APPLY        = 'Aplicar';

  // coupes développées
  rsEDITOR_COUPE_DEV_TITLE          = 'Alzado desarrollado';
  rsEDITOR_COUPE_DEV_CHK_RECALC     = 'Recalcular';


implementation
end.
