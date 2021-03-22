  //{$INCLUDE CadreDessinBGRARedessinEcran.inc}
  // Include contenant la procédure TCadreDessinBGRA.AfficherDebugVariablesInternes
  // 20/08/2015: La procédure TCadreDessinBGRA.AfficherDebugVariablesInternes(),
  //             qui est très volumineuse, est déportée dans le fichier d'inclusion
  //             CadreDessinAfficherDebugVariables.inc

// Ce fichier est une dépendance de cdrdessincanvasBGRA.pas
procedure TCadreDessinBGRA.AfficherDebugVariablesInternes();
CONST FMT_TYPES_ENUMERES = '%s = %s [%d]';
var
    i: Integer;
  procedure MiouMiou(const Miou: string); inline;
  begin
    AfficherMessage(Miou);
    lsbValeurs.Items.Add(Miou);
  end;
  procedure AfficherKV(const K: string; const V: string); overload;
  begin
    MiouMiou(format('%s = %s', [K, V]));
  end;
  procedure AfficherKV(const K: string; const V: double); overload;
  begin
    MiouMiou(format('%s = %.8f', [K, V]));
  end;
  procedure AfficherKV(const K: string; const V: integer); overload;
  var
    WU: String;
  begin
    WU := format('%s = %d', [K, V]);
    MiouMiou(WU);
  end;
  procedure AfficherKV(const K: string; const V: boolean); overload;
  var
    WU: String;
  begin
    WU := format('%s = %s', [K, BoolToStr(V, 'VRAI', 'FAUX')]);
    MiouMiou(WU);
  end;
  procedure AfficherKV(const K: string; const V: TPoint3Df); overload;
  begin
    MiouMiou(format('%s = (%.2f, %.2f, %.2f)', [K, V.X, V.Y, V.Z]));
  end;
  procedure AfficherKV(const K: string; const V: TPoint2Df); overload;
  begin
    MiouMiou(format('%s = (%.2f, %.2f)', [K, V.X, V.Y]));
  end;
  procedure AfficherKV(const K: string; const V: TModeEdition); overload;
  var
    EWE: String;
  begin
    EWE := ChooseString(Ord(V),  ['medCREATION', 'medMODIF']);
    MiouMiou(format(FMT_TYPES_ENUMERES, [K, EWE, Ord(V)]));
  end;

  procedure AfficherKV(const K: string; const V: TNatureObjetCourbe); overload;
  var
    EWE: String;
  begin
    EWE := ChooseString(Ord(V),  ['nocDEFAULT', 'nocPAROI', 'nocPAROIS_CACHEE', 'nocECOULEMENT', 'nocLIGNES_PENTE', 'nocCHENAL_VOUTE', 'nocMARCHE', 'nocPAROI_INCERTAINE', 'nocMUR_MACONNE']);
    MiouMiou(format(FMT_TYPES_ENUMERES, [K, EWE, Ord(V)]));
  end;
  procedure AfficherKV(const K: string; const V: TNatureObjetLigne); inline; overload;
  var
    EWE: String;
  begin
    EWE := ChooseString(Ord(V), ['nolDEFAULT', 'nolFLECHE', 'nolSUITE_RESEAU', 'nolFRACTURE', 'nolPENTE', 'nolPENDAGE']);
    MiouMiou(format(FMT_TYPES_ENUMERES, [K, EWE, Ord(V)]));
  end;
  procedure AfficherKV(const K: string; const V: TNatureObjetPolygone); inline; overload;
  var
    EWE: String;
  begin
    EWE := ChooseString(Ord(V), ['nopDEFAULT', 'nopLAC',
                                 'nopARGILE', 'nopSABLE', 'nopEBOULIS', 'nopGALETS', 'nopNEIGE',
                                 'nopSILHOUETTE', 'nopGROS_BLOC', 'nopGOUR', 'nopSIPHON',
                                 'nopVARVES', 'nopCHEMINS']);
    MiouMiou(format(FMT_TYPES_ENUMERES, [K, EWE, Ord(V)]));
  end;
  procedure AfficherKV(const K: string; const V: TNatureObjetSymbole); overload;
  var
    EWE: String;
  begin
    EWE := ChooseString(Ord(V), ['nosPHOTO', 'nosENTREE',
                                 'nosPOINT_TOPO', 'nosPOINT_FIXE', 'nosCORRESPONDANCE',
                                 'nosFISTULEUSE', 'nosCONCRETION_PAROI', 'nosEXCENTRIQUES',
                                 'nosSTALACTITES', 'nosCOLONNES', 'nosSTALAGMITES',
                                 'nosCRISTAUX', 'nosFRACTURES',
                                 'nosCUPULES', 'nosZEFF',
                                 'nosARRIVEE_EAU', 'nosPERTE',
                                 'nosDESOB',
                                 'nosDANGER',
                                 'nosGOUFFRE_SURFACE',
                                 'nosGROTTE_SURFACE',
                                 'nosPOINT_REMARQUABLE'
                                 ]);
    MiouMiou(format(FMT_TYPES_ENUMERES, [K, EWE, Ord(V)]));
  end;
  procedure AfficherKV(const K: string; const V: TNatureObjetTexte); overload;
  var
    EWE: String;
  begin
    EWE := ChooseString(Ord(V), ['notDEBUG', 'notTITRES', 'notSOUS_TITRES', 'notCOTATION', 'notTEXTE1', 'notTEXTE2']);
    MiouMiou(format(FMT_TYPES_ENUMERES, [K, EWE, Ord(V)]));
  end;

  procedure AfficherKV(const K: string; const V: TModeTravailCanvas); overload;
  var
    EWE: String;
  begin
    EWE := ChooseString(Ord(V), ['mtNONE',
                                 'mtDRAW_SCRAP',
                                 'mtDRAW_COURBE',
                                 'mtDRAW_FREEHAND_COURBE',
                                 'mtDRAW_POLYLINE',
                                 'mtDRAW_POLYGONE',
                                 'mtDRAW_SYMBOLE',
                                 'mtDRAW_TEXTE',
                                 'mtSELECT_SCRAP',
                                 'mtSELECT_LIGNE',
                                 'mtSELECT_POLYLIGNE',
                                 'mtSELECT_COURBE',
                                 'mtSELECT_POLYGONE',
                                 'mtSELECT_SYMBOLE',
                                 'mtSELECT_TEXTE',
                                 'mtMOVE_OBJET',
                                 'mtZOOM_PREMIER_COIN', 'mtZOOM_SECOND_COIN',
                                 'mtPAN_PREMIER_POINT', 'mtPAN_SECOND_POINT',
                                 'mtLIGNE_PREMIER_POINT', 'mtLIGNE_SECOND_POINT',
                                 'mtCOURBE_EDIT_SOMMET', 'mtCOURBE_MOVE_SOMMET',
                                 'mtPOLYGON_EDIT_SOMMET', 'mtPOLYGON_MOVE_SOMMET',
                                 'mtPOLYLIN_EDIT_SOMMET', 'mtPOLYLIN_MOVE_SOMMET',
                                 'mtSCRAP_EDIT_SOMMET', 'mtSCRAP_MOVE_SOMMET'
                                 ]);
    MiouMiou(format(FMT_TYPES_ENUMERES, [K, EWE, Ord(V)]));
  end;
  procedure AfficherKV(const K: string; const V: TModeSelectionEntites); overload;
  var
    EWE: String;
  begin
    EWE := ChooseString(Ord(V), ['mseNONE', 'mseSCRAPS', 'mseLIGNES', 'msePOLYLIGNES', 'mseCOURBES', 'msePOLYGONES', 'mseSYMBOLES', 'mseTEXTES', 'mseSYMBOLES_OR_TEXTES', 'mseALL']);
    MiouMiou(format(FMT_TYPES_ENUMERES, [K, EWE, Ord(V)]));
  end;
  procedure AfficherKV(const K: string; const V: TRetourTypeObjet); overload;
  var
    EWE: String;
  begin
    EWE := ChooseString(Ord(V), ['rtoNOT_FOUND', 'rtoLIGNE', 'rtoSCRAP', 'rtoCOURBE', 'rtoPOLYLIGNE', 'rtoPOLYGONE', 'rtoSYMBOLE', 'rtoTEXTE']);
    MiouMiou(format(FMT_TYPES_ENUMERES, [K, EWE, Ord(V)]));
  end;
begin
  lsbValeurs.Clear;
  MiouMiou('-- Général --');
    AfficherKV('FDoDraw', FDoDraw);
    //AfficherKV('FDoDisplayDebugTexts', FDoDisplayDebugTexts);
    //AfficherKV('FDoDisplayIDStations', FDoDisplayIDStations);
    AfficherKV('FModeTravailCanvas', FModeTravailCanvas);
  MiouMiou('-- Fonctions de rappel');
    AfficherKV('FProcGetInfoBasePoint', Assigned(FProcGetInfoBasePoint));
    AfficherKV('FProcGetSimpleline', Assigned(FProcGetSimpleline));
    AfficherKV('FProcGetScrap', Assigned(FProcGetScrap));
    AfficherKV('FProcGetCourbe', Assigned(FProcGetCourbe));
    AfficherKV('FProcGetPolygon', Assigned(FProcGetPolygon));
    AfficherKV('FProcGetPonctObject', Assigned(FProcGetTextObject));
    AfficherKV('FProcGetTextObject', Assigned(FProcGetPonctObject));
    AfficherKV('FProcActualiserOnglet', Assigned(FProcActualiserOnglet));
  MiouMiou('-- Types d''objets courants');
    AfficherKV('FCurrentNatureLigne', FCurrentNatureLigne);
    AfficherKV('FCurrentNatureCourbe', FCurrentNatureCourbe);
    AfficherKV('FCurrentNaturePolygone', FCurrentNaturePolygone);
    AfficherKV('FCurrentNatureSymbole', FCurrentNatureSymbole);
    AfficherKV('FCurrentNatureTexte', FCurrentNatureTexte);

  MiouMiou('-- Index courant des entités');
    AfficherKV('FCurrentSimpleLineIdx', FCurrentSimpleLineIdx);
    AfficherKV('FCurrentCurveIdx', FCurrentCurveIdx);
    AfficherKV('FCurrentScrapIdx', FCurrentScrapIdx);
    AfficherKV('FCurrentPolygonIdx', FCurrentPolygonIdx);
    AfficherKV('FCurrentSymboleIdx', FCurrentSymboleIdx);
    AfficherKV('FCurrentTextIdx', FCurrentTextIdx);
  MiouMiou('-- Limites du dessin --');
    AfficherKV('FRXMini', FRXMini);
    AfficherKV('FRXMaxi', FRXMaxi);
    AfficherKV('FRYMini', FRYMini);
    AfficherKV('FRYMaxi', FRYMaxi);
  MiouMiou('-- Gestion des zooms');
    AfficherKV('FZC1', FZC1);
    AfficherKV('FZC2', FZC2);
    AfficherKV('FRappHLVue', FRappHLVue);
    AfficherKV('FRappScrReal', FRappScrReal);
    AfficherKV('FInvRappScrReal', FInvRappScrReal);
    //AfficherKV('FNumeroDeZoom', FNumeroDeZoom);
    //for i:= 0 to FTableauZooms.Count - 1 do AfficherKV(Format('FTableauZooms[%d]', [i]), FTableauZooms.Strings[i]);
  MiouMiou('-- Elements en cours d''édition');
    AfficherKV('FIDGroupeEditing', Integer(FIDGroupeEditing));
    AfficherKV('FIDStyleCurveEditing', FIDStyleCurveEditing);
    AfficherKV('FIDStylePolygoneEditing', FIDStylePolygoneEditing);
  MiouMiou('-- Edition --');
    AfficherKV('FDoLockCurrentStation', FDoLockCurrentStation);
    AfficherKV('FAttenduSecondPoint', FAttenduSecondPoint);
    AfficherKV('FUnObjetEstSelectionne', FUnObjetEstSelectionne);
    AfficherKV('FVertexModified', FVertexModified);
    AfficherKV('FDoAddVertexCourbePolygone', FDoAddVertexCourbePolygone);
    AfficherKV('FTypeObjetTrouve', FTypeObjetTrouve);
    //AfficherKV('', );
  MiouMiou('-- Valeurs courantes');
    AfficherKV('FCurrentBasePoint', FCurrentBasePoint.IDTerrain);
    AfficherKV('FLastStationFound', FLastStationFound.IDTerrain);
end;
