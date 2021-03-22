//{$INCLUDE DocDessinExportToSVG.inc}

// Ce fichier est une dépendance de UnitDocDessin.pas

function TDocumentDessin.ExportToSVG(const MyFileName: TStringDirectoryFileName;
                                     const ExportScrapsOnly: boolean;
                                     const DoXHTML: boolean;
                                     const QTitre, QDesc: string): boolean;
const
  STYLE_DEFAULT_SILHOUETTE = 'DefaultSilhouette';
var
  MySVGCanvas: TSVGCanvas;
  // export d'un objet poncxtuel
  procedure ExporterObjectPonctuel(const OP: TSymbole);
  const
    R = 0.06;
  var
    BP: TBaseStation;
    GP: TGroupeEntites;
    PM: TPoint2Df;
  begin
    // TODO: Objets ponctuels
    GP := GetGroupeByIDGroupe(OP.IDGroupe);        // groupe
    if (GetBasePointByIndex(OP.IDBaseStation, BP)) then
    begin
      PM := MakeTPoint2Df(BP.PosStation.X + OP.Offset.X, BP.PosStation.Y + OP.Offset.Y);
      case OP.TypeObject of
        nosPHOTO:
          begin // Photo
            MySVGCanvas.DrawImage(OP.TagTexte, PM.X, PM.Y, OP.ScaleX, OP.ScaleY);
          end;
        nosPOINT_TOPO:
          begin  // Point topo
            MySVGCanvas.DrawSymbole('symbStation', BP.PosStation.X - R, BP.PosStation.Y - R, 2*R, 2*R, 0.00);
          end;
        nosCORRESPONDANCE:
          begin
            if (not GP.DecalageActif) then Exit;
          end;
        else
          pass;
      end;
    end;
  end;
  // dessin d'un texte
  procedure ExporterTexte(const T: TTextObject);
  var
    S : string;
    ST: TStyleTexte;
    BP: TBaseStation;
    GP: TGroupeEntites;

    PM: TPoint2Df;
  begin
    ST := GetStyleTexte(Ord(T.IDStyleTexte));                 // style
    GP := GetGroupeByIDGroupe(T.IDGroupe);        // groupe
    if (GetBasePointByIndex(T.IDBaseSt, BP)) then
    begin
      if (IsValidBaseStation(BP)) then
      begin
        // Rappel: Le décalage des groupes est du ressort de la balise <g>
        PM := MakeTPoint2Df(BP.PosStation.X + T.Offset.X,
                            BP.PosStation.Y + T.Offset.Y);
      end else begin // GetCoordsGCS() non utilisé: GetBasePointByIndex() indispensable ici

        PM := MakeTPoint2Df(T.Offset.X, T.Offset.Y);
      end;
      S :=InterpreterTexteAnnotation(T.Text, T.MaxLength, BP);
      MySVGCanvas.DrawTexte(ST.NameSVGStyle, T.Alignment, PM.X, PM.Y, 0.00, S);
    end;
  end;
  // dessin d'un polygone
  procedure ExporterPolygone(const QP: TPolygone);
  var
    QC: TArrayPoints2Df;
    ST: TStylePolygone;
    i, Nb: Integer;
  begin
    ST := GetStylePolygone(Ord(QP.IDStylePolygone));
    AfficherMessage(Format('ExporterPolygone: %d',[QP.IDGroupe]));
    if (ConvertirGHCD2StdPolygon(QP.Sommets, QC)) then
    begin
      Nb := length(QC);
      if (0 = NB) then exit;
      MySVGCanvas.BeginListeVertex();
        for i := 0 to Nb - 1 do MySVGCanvas.AddVertex(QC[i].X, QC[i].Y);
      MySVGCanvas.EndListeVertex();
      MySVGCanvas.DrawPolygon(ST.NameSVGStyle, false);
    end;
  end;
  // dessin d'une polyligne
  procedure ExporterPolyligne(const QP: TPolyLigne);
  var
    ST: TStyleCourbe;
    QC: TArrayPoints2Df;
    Nb, i: Integer;
  begin
    ST := GetStyleCourbe(Ord(QP.IDStylePolyLine));
    AfficherMessage(Format('ExporterPolyligne: %d',[QP.IDGroupe]));
    if (ConvertirGHCD2StdPolygon(QP.Sommets, QC)) then
    begin
      Nb := length(QC);
      if (0 = NB) then exit;
      MySVGCanvas.BeginListeVertex();
        for i := 0 to Nb - 1 do MySVGCanvas.AddVertex(QC[i].X, QC[i].Y);
      MySVGCanvas.EndListeVertex();
      MySVGCanvas.DrawPolylign(ST.NameSVGStyle, false);
    end;
  end;
  // dessin d'une ligne simple
  procedure ExporterSimpleLigne(const SL: TSimpleLigne);
  var
    BS1, BS2  : TBaseStation;
    PPP1, PPP2: TPoint2Df;
  begin
    if (GetBasePointByIndex(SL.IDBaseStExt1, BS1) AND
        GetBasePointByIndex(SL.IDBaseStExt2, BS2)) then
    begin
      PPP1 := MakeTPoint2Df(BS1.PosStation.X + SL.OffsetExtr1.X,
                            BS1.PosStation.Y + SL.OffsetExtr1.Y);
      PPP2 := MakeTPoint2Df(BS2.PosStation.X + SL.OffsetExtr2.X,
                            BS2.PosStation.Y + SL.OffsetExtr2.Y);
    end;
  end;

  // dessin d'une courbe
  procedure ExporterCourbe(const C: TCourbe);
  var
    ST: TStyleCourbe;
    i, NbArcs: Integer;
    A: TArcCourbe;
    BS1, BS2: TBaseStation;
    PP1, PP2, PP3, PP4: TPoint2Df;
    WU: String;
  begin
    ST := GetStyleCourbe(Ord(C.IDStyleCourbe));
    AfficherMessageErreur(Format('ExporterCourbe: %d',[C.IDGroupe]));
    MySVGCanvas.BeginListeBezierArcs();
      NbArcs := Length(C.Arcs);
      for i := 0 to NbArcs - 1 do
      begin
        A := C.Arcs[i];
         if (GetBasePointByIndex(A.IDStationP1, BS1) AND
             GetBasePointByIndex(A.IDStationP2, BS2)) then
         begin
           PP1 := MakeTPoint2Df(BS1.PosStation.X + A.OffsetP1.X,
                                BS1.PosStation.Y + A.OffsetP1.Y);
           PP2 := MakeTPoint2Df(BS1.PosStation.X + A.OffsetP1.X + A.TangP1.X,
                                BS1.PosStation.Y + A.OffsetP1.Y + A.TangP1.Y);
           PP3 := MakeTPoint2Df(BS2.PosStation.X + A.OffsetP2.X + A.TangP2.X,
                                BS2.PosStation.Y + A.OffsetP2.Y + A.TangP2.Y);
           PP4 := MakeTPoint2Df(BS2.PosStation.X + A.OffsetP2.X,
                                BS2.PosStation.Y + A.OffsetP2.Y);
           MySVGCanvas.AddBezierArc(PP1.X, PP1.Y, PP2.X, PP2.Y, PP3.X, PP3.Y, PP4.X, PP4.Y);
         end;
      end;
    MySVGCanvas.EndListeBezierArcs();
    MySVGCanvas.DrawBezierCurve(ST.NameSVGStyle, False);
    WU := MySVGCanvas.GetLastError();
  end;
  // dessin d'un scrap
  procedure ExporterScrap(const SC: TScrap; const QIdx: integer);
  var
    QC: TArrayPoints2Df;
    Nb, i: Integer;
  begin
    AfficherMessage(Format('ExporterScrap: %d',[QIdx]));
    if (ConvertirGHCD2StdPolygon(SC.Sommets, QC)) then
    begin
      Nb := length(QC);
      if (0 = NB) then exit;
      MySVGCanvas.BeginListeVertex();
        for i := 0 to Nb - 1 do MySVGCanvas.AddVertex(QC[i].X, QC[i].Y);
      MySVGCanvas.EndListeVertex();
      MySVGCanvas.DrawPolygon(Format('Scrap%d', [QIdx]), false);
    end;
  end;
  // balancer les feuilles de styles
  procedure ExporterFeuillesDeStyles();
  const

    MOTIF_ARGILE  = 'pat_clay';
    MOTIF_BLOC    = 'pat_bloc';
    MOTIF_SABLE   = 'pat_sand';
    MOTIF_GALETS  = 'pat_galets';
    MOTIF_EBOULIS = 'pat_eboulis';
    FORMAT_STYLE = '    .%s {stroke:%s; stroke-width:%.2f; fill:%s}';
  var
    i   : integer;
    Qc  : TColor;
    FSL : TStyleLigne;
    FSC : TStyleCourbe;
    FST : TStyleTexte;
    FSP : TStylePolygone;
    FSS : TStyleSymboles;
    Nb: Integer;
    MyScrap: TScrap;

  begin
    // motifs de remplissages
    MySVGCanvas.WriteCommentaire('** Styles de remplissages **');
    MySVGCanvas.WriteCommand('<defs>');
    // motifs
  //  <pattern x="10" y="10" width="20" height="20">
  // <rect x="5" y="5" width="10" height="10"/>
//</pattern>
    MySVGCanvas.WriteVerbatimLine(Format('  <pattern id="%s" >',
                                ['MyPattern']));
    MySVGCanvas.WriteVerbatimLine('  </pattern>');
    MySVGCanvas.WriteCommand('</defs>');
    //-----------------------------------------------------
    MySVGCanvas.WriteCommand('<defs>');
    MySVGCanvas.WriteCommand('<style type="text/css">');
    MySVGCanvas.WriteVerbatimLine('  <![CDATA[');
    // cadre du dessin
    MySVGCanvas.WriteStyleLinePolygoneTexte(SVG_NAME_CADRE_MASSICOT,
                                            clBlack, 255,  0.02, psSolid,
                                            clWhite, 255, bsClear,
                                            DEFAULT_FONT_NAME, 2.50, clBlack, 255, [], 'Format');
    MySVGCanvas.WriteStyleLinePolygoneTexte(SVG_NAME_CADRE_PERIMETRIQUE,
                                            clBlack, 255, 0.07, psSolid,
                                            clWhite, 255, bsClear,
                                            DEFAULT_FONT_NAME, 2.50, clBlack, 255, [], 'Cadre');

    // style des cartouches et règles
    MySVGCanvas.WriteStyleLinePolygoneTexte(SVG_NAME_CARTOUCHE_REGLE_CADRE,
                                            clBlack, 255,  0.02, psSolid,
                                            clWhite, 255, bsClear,
                                            DEFAULT_FONT_NAME, 2.50, clBlack, 255, [], 'Regle cadre');
    MySVGCanvas.WriteStyleLinePolygoneTexte(SVG_NAME_CARTOUCHE_REGLE_GRADU,
                                            clBlack, 255,  0.02, psSolid,
                                            clSilver, 255, bsSolid,
                                            DEFAULT_FONT_NAME, 2.50, clBlack, 255, [], 'Regle gradu');
    // style des marqueurs de stations
    MySVGCanvas.WriteVerbatimLine(Format(FORMAT_STYLE, [SVG_NAME_MARQUE_STATION_FILL       , RGB2HTMLColor(clBlack)  , 0.01, RGB2HTMLColor(clYellow)]));
    MySVGCanvas.WriteVerbatimLine(Format(FORMAT_STYLE, [SVG_NAME_MARQUE_STATION_CENTREFILL , RGB2HTMLColor(clBlack)  , 0.02, RGB2HTMLColor(clRed)]));
    // polygonales
    //MySVGCanvas.WriteVerbatimLine('     /* Styles de lignes de la polygonale*/');
    MySVGCanvas.WriteStyleLinePolygoneTexte(SVG_NAME_POLYGONALE_CENTERLINE,
                                            clGray, 255,  0.02, psSolid,
                                            clWhite, 255, bsClear,
                                            DEFAULT_FONT_NAME, 2.50, clBlack, 255, [], 'Centerline');
    MySVGCanvas.WriteStyleLinePolygoneTexte(SVG_NAME_POLYGONALE_SECTIONS  ,
                                            clSilver, 255,  0.01, psSolid,
                                            clWhite, 255, bsClear,
                                            DEFAULT_FONT_NAME, 2.50, clBlack, 255, [], 'Cross sections' );
    // styles de lignes
    Nb := GetNbStylesLigne();
    for i := 0 to Nb - 1 do
    begin
      FSL := GetStyleLigne(i);
      MySVGCanvas.WriteStyleLinePolygoneTexte(FSL.NameSVGStyle,
                                              FSL.LineColor, 255, FSL.PrintLineWidth, psSolid,
                                              clWhite, 255, bsClear,
                                              DEFAULT_FONT_NAME, 2.50, clBlack, 255, [], Format('LineStyle_%d', [i]));
    end;
    //MySVGCanvas.WriteVerbatimLine('     /* Styles de courbes */');
    Nb := GetNbStylesCourbe();
    for i := 0 to Nb - 1 do
    begin
      FSC := GetStyleCourbe(i);

      MySVGCanvas.WriteStyleLinePolygoneTexte(FSC.NameSVGStyle,
                                              FSC.LineColor, 255, FSC.PrintLineWidth, psSolid,
                                              clWhite, 255, bsClear,
                                              DEFAULT_FONT_NAME, 2.50, clBlack, 255, [], '');
    end;
    // styles de textes
    Nb := GetNbStylesTexte();
    for i := 0 to Nb - 1 do
    begin
      FST := GetStyleTexte(i);
      MySVGCanvas.WriteVerbatimLine(Format('    .%s {font-weight:normal; font-size: %.3fmm; font-family:''Arial''}',
                                         [FST.NameSVGStyle, FST.FontPrnHeight / 10.00]));
    end;
    // styles de polygones
    Nb := GetNbStylesPolygones();
    for i := 0 to Nb - 1 do
    begin
      FSP := GetStylePolygone(i);
      case FSP.IDStyle of
        0, 1, 7, 10:
          begin // couleurs pleines
            MySVGCanvas.WriteStyleLinePolygoneTexte(FSP.NameSVGStyle,
                                                    FSP.FillColor, 255, 0.0, psClear,
                                                    FSP.FillColor, 255, bsClear,
                                                    DEFAULT_FONT_NAME, 2.50, clBlack, 255, [], '');
            //MySVGCanvas.WriteVerbatimLine(Format('    .%s {fill: %s}', [ FSP.NameSVGStyle,RGB2HTMLColor(FSP.FillColor)]));
          end;
        2: begin // argile

          end;
      else
        MySVGCanvas.WriteVerbatimLine(Format('    .%s {fill: %s}',
                                         [ FSP.NameSVGStyle,
                                           RGB2HTMLColor(FSP.FillColor)
                                         ]));
      end;
    end;
    // styles de scraps
    Nb := GetNbScraps();
    if (Nb > 0) then
    begin
      for i := 0 to Nb-1 do
      begin
        MyScrap := GetScrap(i);
        MySVGCanvas.WriteStyleLinePolygoneTexte(Format('Scrap%d', [i]),
                                                MyScrap.Couleur, MyScrap.Opacite, 0.0, psClear,
                                                MyScrap.Couleur, MyScrap.Opacite, bsSolid,
                                                DEFAULT_FONT_NAME, 2.50, clBlack, 255, [], '');
      end;
    end;
    // et le style de scrap par défaut
    //MySVGCanvas.WriteStyleScrap(,
    //                                MyScrap.Couleur, MyScrap.Couleur,
    //                                MyScrap.Opacite);



    MySVGCanvas.WriteVerbatimLine('  ]]>');
    MySVGCanvas.WriteCommand('</style>');
    MySVGCanvas.WriteCommand('</defs>');
    //MySVGCanvas.WriteCommentaire('---------------------------');
  end;
  // balancer la table des symboles
  procedure ExporterTableSymboles();
  begin
    // TODO: Compléter la table des symboles
    MySVGCanvas.WriteCommentaire('Table des symboles');
    // Point topo
    MySVGCanvas.WriteVerbatimLine('  <symbol id="symbStation">');
    MySVGCanvas.WriteVerbatimLine(Format('    <circle class="%s" cx="0.00" cy="0.00" r="1.00"/>', [SVG_NAME_MARQUE_STATION_CENTREFILL]));
    MySVGCanvas.WriteVerbatimLine('  </symbol>');
    MySVGCanvas.WriteCommentaire('Fin de la table des symboles');
  end;
  // balancer le cadre de dessin
  procedure ExporterCadreDessin();
  const
    GRP_CADRE_NAME = 'CadreGeneral';
    MARGE_CADRE    = 2.00;
  begin
    MySVGCanvas.BeginGroupe(GRP_CADRE_NAME, 'Cadre perimetrique du dessin', 0.00, 0.00);
      //MySVGCanvas.SetPen(Crayon);
      MySVGCanvas.DrawRectangle(SVG_NAME_CADRE_PERIMETRIQUE,
                              FCoordsMini.X + MARGE_CADRE, FCoordsMini.Y + MARGE_CADRE,
                              FCoordsMaxi.X - MARGE_CADRE, FCoordsMaxi.Y - MARGE_CADRE);
      MySVGCanvas.DrawRectangle(SVG_NAME_CADRE_MASSICOT,
                              FCoordsMini.X + 0.5, FCoordsMini.Y + 0.5,
                              FCoordsMaxi.X - 0.5, FCoordsMaxi.Y - 0.5);
    MySVGCanvas.EndGroupe(GRP_CADRE_NAME);
  end;
  // balancer la polygonale
  procedure ExporterPolygonale();
  const
    GRP_POLYGONALS = 'Polygonales';
  var
    i  : integer;
    BS : TBaseStation;
  begin
    if (ExportScrapsOnly) then Exit; // en scraps seuls, ne rien faire
    MySVGCanvas.BeginGroupe(GRP_POLYGONALS, 'Squelette topo polygonal', 0.00, 0.00);
    // polygonales
    for i := 0 to FCenterLines.GetNbBasePoints() do
    begin
      BS := FCenterLines.GetBasePoint(i);
      if (BS.TypeStation = 1) then Continue;
      MySVGCanvas.DrawLine(SVG_NAME_POLYGONALE_CENTERLINE,
                         BS.PosExtr0.X, BS.PosExtr0.Y,
                         BS.PosStation.X, BS.PosStation.Y);
    end;
    // sections
    for i := 0 to FCenterLines.GetNbBasePoints() do
    begin
      BS := FCenterLines.GetBasePoint(i);
      if (BS.TypeStation = 7) then Continue;
      MySVGCanvas.DrawLine(SVG_NAME_POLYGONALE_SECTIONS,
                         BS.PosPG.X, BS.PosPG.Y,
                         BS.PosPD.X, BS.PosPD.Y);
    end;
    MySVGCanvas.EndGroupe(GRP_POLYGONALS);
  end;

  // balancer les groupes
  procedure ExporterGroupes();
  var
    NoGrp       : integer;
    NoObjet     : integer;
    QGroupe     : TGroupeEntites;
    QCourbe     : TCourbe;
    QTexte      : TTextObject;
    QPolygon    : TPolygone;
    QPonctObjet : TSymbole;
    QPolyligne: TPolyLigne;
    Nb: Integer;
    NbObj: Integer;
    QScrap: TScrap;
    QDecalage: TPoint3Df;
    QSimpleligne: TSimpleLigne;
  begin
    Nb := GetNbGroupes();
    for NoGrp := 0 to Nb - 1 do
    begin
      QGroupe := GetGroupe(NoGrp);
      // balise de début de groupe
      if (QGroupe.DecalageActif) then QDecalage := QGroupe.Decalage
                                 else QDecalage := MakeTPoint3Df(0.0, 0.0, 0.0);
      MySVGCanvas.BeginGroupe(Format('Groupe_%d',[QGroupe.IDGroupeEntites]), QGroupe.NomGroupe, QDecalage.X, QDecalage.Y);
      // scraps
      NbObj := GetNbScraps();
      if (NbObj > 0) then
      begin
        for NoObjet := 0 to NbObj - 1 do
        begin
          QScrap := GetScrap(NoObjet);
          if (QScrap.IDGroupe = QGroupe.IDGroupeEntites) then ExporterScrap(QScrap, NoObjet);
        end;
      end;
      // si on a choisi d'exporter les scraps seuls, on passe au groupe suivant
      if (not ExportScrapsOnly) then
      begin
        // polygones (en premier)
        NbObj := GetNbPolygones();
        if (NbObj > 0) then
        begin
          for NoObjet := 0 to NbObj - 1 do
          begin
            QPolygon := GetPolygone(NoObjet);
            if (QPolygon.IDGroupe = QGroupe.IDGroupeEntites) then ExporterPolygone(QPolygon);
          end;
        end;
        // courbes
        NbObj := GetNbCourbes();
        if (NbObj > 0) then
        begin
          for NoObjet := 0 to NbObj - 1 do
          begin
            QCourbe := GetCourbe(NoObjet);
            if (QCourbe.IDGroupe = QGroupe.IDGroupeEntites) then ExporterCourbe(QCourbe);
          end;
        end;
        //Polylignes
        NbObj := GetNbPolylignes();
        if (NbObj > 0) then
        begin
          for NoObjet := 0 to NbObj - 1 do
          begin
            QPolyligne := GetPolyligne(NoObjet);
            if (QPolyligne.IDGroupe = QGroupe.IDGroupeEntites) then ExporterPolyligne(QPolyligne);
          end;
        end;
        // Lignes simples
        NbObj := GetNbSimpleLignes();
        if (NbObj > 0) then
        begin
          for NoObjet := 0 to NbObj - 1 do
          begin
            QSimpleligne := GetSimpleLigne(NoObjet);
            if (QPolyligne.IDGroupe = QGroupe.IDGroupeEntites) then ExporterSimpleLigne(QSimpleligne);
          end;
        end;

        // objets ponctuels
        NbObj := GetNbSymboles();
        if (NbObj > 0) then
        begin
          for NoObjet := 0 to NbObj - 1 do
          begin
            QPonctObjet := GetSymbole(NoObjet);
            if (QPonctObjet.IDGroupe = QGroupe.IDGroupeEntites) then  ExporterObjectPonctuel(QPonctObjet);
          end;
        end;
        // textes
        NbObj := GetNbTextes();
        if (NbObj > 0) then
        begin
          for NoObjet := 0 to NbObj - 1 do
          begin
            QTexte := GetTexte(NoObjet);
            if (QTexte.IDGroupe = QGroupe.IDGroupeEntites) then ExporterTexte(QTexte);
          end;
        end;

      end; // if (not ExportScrapsOnly) then
      // balise de fin de groupe
      MySVGCanvas.EndGroupe(Format('Groupe_%d',[QGroupe.IDGroupeEntites]));
    end;
  end;
  procedure DessinCartouche();
  const
    MARGE_CADRE = 3.00;
    procedure DessinEchelle(const QX, QY, L, H: double);
    const
      SD = 10;
    var
      HL, HH: double;
      FX1, FY1, FX2, FY2: Double;
      i, EWE: Integer;
    begin
      AfficherMessage('-------- DessinEchelle');
      HH := H / 2;
      HL := L / SD;
      MySVGCanvas.BeginGroupe(GROUPE_ECHELLE, 'Echelle', 0.00, 0.00);
        MySVGCanvas.DrawRectangle(SVG_NAME_CARTOUCHE_REGLE_CADRE, QX, QY, QX + L, QY + H);
        for i := 0 to SD -1 do
        begin
          EWE := IIF(Odd(i), 0, 1);
          MySVGCanvas.DrawRectangle(SVG_NAME_CARTOUCHE_REGLE_GRADU,
                                    QX + i * HL, QY + EWE * HH,
                                    QX + (i+1) * HL, QY + (EWE + 1) * HH);

        end;
      MySVGCanvas.EndGroupe(GROUPE_ECHELLE);
    end;
  begin
    AfficherMessage('---- DessinCartouche');
    MySVGCanvas.BeginGroupe(GROUPE_CARTOUCHE, '', 0.00, 0.00);
    DessinEchelle(FCoordsMini.X + MARGE_CADRE, FCoordsMini.Y + MARGE_CADRE, 50.00, 02.00);
    MySVGCanvas.EndGroupe(GROUPE_CARTOUCHE);
  end;
begin
  Result := False;
  AfficherMessage(Format('%s.ExportToSVG: %s - %s - %s %s',[self.ClassName, MyFileName, BoolToStr(DoXHTML, 'XHTML', 'SVG'), QTitre, QDesc]));
  MySVGCanvas := TSVGCanvas.Create;
  try
    if (not MySVGCanvas.InitializeDocument(MyFileName, false, QTitre, QDesc, FCoordsMini.X, FCoordsMini.Y, FCoordsMaxi.X, FCoordsMaxi.Y, nil)) then exit;
    MySVGCanvas.BeginDrawingSection();
    //---------------------------------------------
    // le dessin ici
    //---------------------------------------------
      AfficherMessage('--> Feuilles de styles internes');
      ExporterFeuillesDeStyles();
      AfficherMessage('--> Table des symboles');
      ExporterTableSymboles();
      AfficherMessage('--> Cadre périmétrique');
      ExporterCadreDessin();
      AfficherMessage('--> Cartouche');
      DessinCartouche();
      AfficherMessage('--> Polygonales');
      ExporterPolygonale();
      AfficherMessage('--> Export Groupes');
      ExporterGroupes();
    //---------------------------------------------
    MySVGCanvas.EndDrawingSection();
    // finalisation
    MySVGCanvas.FinalizeDocument();
    Result := True;
  finally
    MySVGCanvas.Free;
  end;
end;

