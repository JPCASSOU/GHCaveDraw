//{$INCLUDE DocDessinSaveToFile.inc}
// Include contenant la fonction de sauvegarde du dessin
//-----------------------------------------------------------------------------
// GroupeExported: -1 => Tout le document est exporté,
//                 sinon, seul le groupe indiqué est exporté
//                 DoWriteCenterlinesSection: Exporter les centerlines ?
procedure TDocumentDessin.SaveToFile(const FichierGCD: TStringDirectoryFileName;
                                     const GroupeExported: TIDGroupeEntites;
                                     const CenterlinesInFichierSepareGCP: boolean);
var
  F : TextFile;
  CurrentGroupe : TGroupeEntites;
  InternalIdxCodeEPSG: Integer;
  MySystEPSG: TLabelSystemesCoordsEPSG;
  procedure WrtLn(const S: string); inline;
  begin
    WriteLn(F, S);
  end;
  function CanExportEntity(const IDGrpEntity, IDExpGroupe: TIDGroupeEntites): boolean; inline;
  begin
    if (IDExpGroupe = -1) then Result := True
                          else Result := (IDGrpEntity = IDExpGroupe);
  end;
  procedure WriteCommentsHeadSection(const HeadTitle: string; const NbElements: integer; const Autodoc: string);
  begin
    WrtLn('');
    WrtLn(Format('# Section: %s (%d elements)', [HeadTitle, NbElements]));
    WrtLn('#-------------------------------------------------------');
    WrtLn('# Parameters: ' + Autodoc);
    WrtLn('');
  end;
  procedure WrtSectionCenterlines();
  var
    i, Nb: integer;
    BP: TBaseStation;
  begin
    Nb := FCenterLines.GetNbBasePoints();
    if (CenterlinesInFichierSepareGCP) then  // envoie la commande d'inclusion de centerlines
    begin
      wrtln('#----------------------------------');
      wrtln(INSTRUCTION_INCLUDE_GCP + ' ' + FFichierCenterlines);
      wrtln('#----------------------------------');
    end
    else  // sinon on balance les centerlines
    begin
      WrtLn(BASEPOINTMARKER);
      WrtLn('  # Parameters: IDStation; Caption; GHTopo TypeStation; integer Colour; Start shot, End shot, Left and Right coordinates');
      AfficherMessage(Format('--> Saving %d basepoints', [FCenterLines.GetNbBasePoints]));
      for i := 0 to Nb - 1 do
      begin
        BP := FCenterLines.GetBasePoint(i);
        WrtLn(BP.toLineGCP());
      end;
      WrtLn(ENDBASEPOINTMARKER);
    end;
  end;
  procedure WrtSectionImages();
  var
    i, Nb: integer;
    QImg: TImageObject;
  begin
    Nb := self.GetNbImages();
    WriteCommentsHeadSection('Images', Nb, 'ID image, Position Corners X1Y1X2Y2, Opacity, FileName, Description');
    WrtLn('');
    if (Nb > 0) then
    begin
      AfficherMessage(Format('--> Saving %d images', [Nb]));
      WrtLn(IMAGESSECTION);
      for i := 0 to Nb - 1 do
      begin
        QImg := GetImage(i);
        WrtLn(Format('  ' + IMAGE + FMT_IMAGE,
                     [i,
                      FormatterNombreWithDotDecimal(QImg.PositionCoinsImage.X1, 2),
                      FormatterNombreWithDotDecimal(QImg.PositionCoinsImage.Y1, 2),
                      FormatterNombreWithDotDecimal(QImg.PositionCoinsImage.X2, 2),
                      FormatterNombreWithDotDecimal(QImg.PositionCoinsImage.Y2, 2),
                      QImg.Opacite,
                      QImg.SrcFilename,
                      QImg.Description
                     ]));
      end;
      WrtLn(ENDIMAGESSECTION);
    end;
  end;
  procedure WrtSectionSuperGroupes();
  var
    i, Nb: integer;
    SGRP: TSuperGroupe;
  begin
    Nb := GetNbSuperGroupes();
    WriteCommentsHeadSection('SuperGroupes', Nb, 'Color, NomGroupe, Displayed, Locked, Offset XYZ components (unimplemented)');
    AfficherMessage(Format('--> Saving %d supergroupes', [Nb]));
    WrtLn(SUPERGROUPESSECTION);
    for i := 0 to Nb - 1 do
    begin
      SGRP := GetSuperGroupe(i);
      WrtLn(SGRP.toLineGCD(i));
    end;
    WrtLn(ENDSUPERGROUPESSECTION);
    WrtLn('');
  end;
  procedure WrtSectionGroupes();
  var
    i, Nb: integer;
    GRP: TGroupeEntites;
  begin
    Nb := GetNbGroupes();
    WriteCommentsHeadSection('Groupes', Nb, 'IDGroupe, NomGroupe, Colour, Offset XYZ components, ID SuperGroup, LastModified, DecalageActif (1=Actif), ZOrder');
    AfficherMessage(Format('--> Saving %d groupes', [Nb]));
    WrtLn(GROUPESSECTION);
    for i:=0 to Nb - 1 do
    begin
      GRP := GetGroupe(i);
      WrtLn(GRP.toLineGCD(i));
    end;
    WrtLn(ENDGROUPESSECTION);
  end;
  procedure WrtSectionScraps();
  var
    i, j, Nb: integer;
    MyScrap: TScrap;
    V: TVertexPolygon;
  begin
    Nb := GetNbScraps();
    AfficherMessage(Format('--> Saving %d scraps', [Nb]));
    if (Nb = 0) then
    begin
      WrtLn(Format(NOENTITIES, [SCRAPSSECTION]));
      WrtLn('');
    end else
    begin
      WriteCommentsHeadSection('Scraps', Nb, '');
      WrtLn(SCRAPSSECTION);
      for i:=0 to Nb - 1 do
      begin
        MyScrap  := GetScrap(i);
        if (CanExportEntity(MyScrap.IDGroupe, GroupeExported)) then
        begin
          WrtLn('  # Last modification: ' + DateTimeToStr(Now()) );
          WrtLn('  # Scrap Parameters: IDScrap, IDGroupe, ColorR, ColorG, ColorB, Transparency (0..255), Name ');
          WrtLn('  # Vertex  Parameters: IDStation, Offset XYZ Components');
          WrtLn(MyScrap.toLineGCD_Begin(i));
          for j := 0 to MyScrap.getNbVertex() - 1 do
          begin
            V := MyScrap.getVertex(j);
            WrtLn(V.toLineGCD(j));
          end;
          WrtLn(MyScrap.toLineGCD_End());
        end; // if CanExportEntity()
      end;
      WrtLn(ENDSCRAPSSECTION);
    end;
  end;
  procedure WrtSectionCourbes();
  var
    i, j, Nb: integer;
    MyCourbe: TCourbe;
    A: TArcCourbe;
  begin
    Nb := GetNbCourbes();
    AfficherMessage(Format('--> Saving %d curve objects', [Nb]));
    if (Nb = 0) then
    begin
      WrtLn(Format(NOENTITIES, [CURVESSECTION]));
      WrtLn('');
    end else
    begin
      WriteCommentsHeadSection('Courbes', Nb, '');
      WrtLn(CURVESSECTION);
      for i:= 0 to Nb - 1 do
      begin
        MyCourbe  := GetCourbe(i);
        if (CanExportEntity(MyCourbe.IDGroupe, GroupeExported)) then
        begin
          WrtLn('  # Last modification: ' + DateTimeToStr(Now()) );
          WrtLn('  # Parameters for curve: ID Curve, IDGroupe, IDStyleCourbe, Closed (=1)');
          WrtLn('  # Parameters for each arc: IDStationP1, Offset_from_P1, "-1.0", Tangent_at_P1, IDStationP1, Offset_from_P2, Tangent_at_P2, "-2.0"');
          WrtLn(MyCourbe.toLineGCD_Begin(i));
          for j := 0 to MyCourbe.getNbArcs() - 1 do
          begin
            A := MyCourbe.getArc(j);
            WrtLn(A.toLineGCD(j));
          end;
          WrtLn(MyCourbe.toLineGCD_End());
        end; // if CanExportEntity()
      end;
      WrtLn(ENDCURVESSECTION);
    end;
  end;
  procedure WrtSectionSimpleLines();
  var
    i, Nb: integer;
    MySimpleLine: TSimpleLigne;
  begin
    Nb := GetNbSimpleLignes();
    AfficherMessage(Format('--> Saving %d line objects', [Nb]));
    if (Nb = 0) then
    begin
      WrtLn(Format(NOENTITIES, [LINESECTION]));
      WrtLn('');
    end else
    begin
      WriteCommentsHeadSection('Lines', Nb, 'ID, IDGroupe, Style Line, First Point: IDStation , Offset XYZ, Second Point: IDStation , Offset XYZ');
      WrtLn(LINESECTION);
      for i:=0 to Nb - 1 do
      begin
        MySimpleLine  := GetSimpleLigne(i);
        if (CanExportEntity(MySimpleLine.IDGroupe, GroupeExported)) then WrtLn(MySimpleLine.toLineGCD(i));//GenererLigneSimpleLigne(i, MySimpleLine));
      end;
      WrtLn(ENDLINESECTION);
    end;
  end;
  procedure WrtSectionPolylines();
  var
    i, j, Nb: integer;
    MyPolyLine: TPolyLigne;
    V: TVertexPolygon;
  begin
    Nb := GetNbPolylignes();
    AfficherMessage(Format('--> Saving %d polylines objects', [Nb]));
    if (Nb = 0) then
    begin
      WrtLn(Format(NOENTITIES, [POLYLINESSECTION]));
      WrtLn('');
    end else
    begin
      WriteCommentsHeadSection('Polylines', Nb, '');
      WrtLn(POLYLINESSECTION);
      for i:=0 to Nb - 1 do
      begin
        MyPolyLine  := GetPolyligne(i);
        if (CanExportEntity(MyPolyLine.IDGroupe, GroupeExported)) then
        begin
          WrtLn('  # Last modification: ' + DateTimeToStr(Now()) );
          WrtLn('  # Polylines Parameters: IDPolygon, IDGroupe, Style PolyLine, Closed (= 1)');
          WrtLn('  # Vertex  Parameters: IDStation, Offset XYZ Components');
          WrtLn(MyPolyLine.toLineGCD_Begin(i));
          for j := 0 to MyPolyLine.getNbVertex() - 1 do
          begin
            V := MyPolyLine.getVertex(j);
            WrtLn(V.toLineGCD(j));
          end;
          WrtLn(MyPolyLine.toLineGCD_End());
        end;
      end;
      WrtLn(ENDPOLYLINESSECTION);
    end;
  end;
  procedure WrtSectionPolygones();
  var
    i, j, Nb: integer;
    MyPolygon: TPolygone;
    V: TVertexPolygon;
  begin
    Nb := GetNbPolygones();
    AfficherMessage(Format('--> Saving %d polygon objects', [Nb]));
    if (Nb = 0) then
    begin
      WrtLn(Format(NOENTITIES, [POLYGONSECTION]));
      WrtLn('');
    end else
    begin
      WriteCommentsHeadSection('Polygons', Nb, '');
      WrtLn(POLYGONSECTION);
      for i:=0 to Nb - 1 do
      begin
        MyPolygon  := GetPolygone(i);
        if (CanExportEntity(MyPolygon.IDGroupe, GroupeExported)) then
        begin
          WrtLn(MyPolygon.toLineGCD_Begin(i));
          WrtLn('  # Last modification: ' + DateTimeToStr(Now()) );
          WrtLn('  # Polygon Parameters: IDPolygon, IDGroupe, Style Polygon');
          WrtLn('  # Vertex  Parameters: IDStation, Offset XYZ Components');
          for j := 0 to MyPolygon.getNbVertex() - 1 do
          begin
            V := MyPolygon.getVertex(j);
            WrtLn(V.toLineGCD(j));
          end;
          WrtLn(MyPolygon.toLineGCD_End());
        end;
      end;
      WrtLn(ENDPOLYGONSECTION);
    end;
  end;
  procedure WrtSectionTextes();
  var
    i, Nb: integer;
    MyTexte: TTextObject;
  begin
    Nb := GetNbTextes();
    AfficherMessage(Format('--> Saving %d text objects', [Nb]));
    if (Nb = 0) then
    begin
      WrtLn(Format(NOENTITIES, [TEXTSECTION]));
      WrtLn('');
    end else
    begin
      WriteCommentsHeadSection('Texts', Nb, 'ID, IDGroupe, Style Text, IDStation, Offset XYZ Components, Text Alignment, MaxLength, Text string');
      WrtLn(TEXTSECTION);
      for i:=0 to Nb - 1 do
      begin
        MyTexte  := GetTexte(i);
        if (CanExportEntity(MyTexte.IDGroupe, GroupeExported)) then  WrtLn(MyTexte.toLineGCD(i));
      end;
      WrtLn(ENDTEXTSECTION);
    end;
  end;
  procedure WrtSectionSymboles();
  var
    i, Nb: integer;
    MySymbole: TSymbole;
  begin
    Nb := GetNbSymboles();
    AfficherMessage(Format('--> Saving %d ponctual objects', [Nb]));
    if (Nb = 0) then
    begin
      WrtLn(Format(NOENTITIES, [PONCTOBJSECTION]));
      WrtLn('');
    end else
    begin
      WriteCommentsHeadSection('Symbols', Nb, 'ID, IDGroupe, TypeObject, Colour, IDStation, Offset XYZ Coordinates, Rotation, XY Scale, Legend or filename, Tag, Display photo');
      WrtLn(PONCTOBJSECTION);
      for i:=0 to Nb - 1 do
      begin
        MySymbole  := GetSymbole(i);
        if (CanExportEntity(MySymbole.IDGroupe, GroupeExported)) then WrtLn(MySymbole.toLineGCD(i));//GenererLigneSymbole(i, MySymbole));
      end;
      WrtLn(ENDPONCTOBJSECTION);
    end;
  end;
begin
  FCenterlinesIsIncludeFileGCP := CenterlinesInFichierSepareGCP;
  AfficherMessage(Format('%s.SaveToFile(%s)',[ClassName, FichierGCD]));
  AssignFile(F, FichierGCD);
  try
    ReWrite(F);

    WrtLn(Format('# File %s saved %s %s',[FichierGCD, DateToStr(Now), TimeToStr(Now)]));
    WrtLn(Format('# Producer: %s - %s', [rsGHCAVEDRAWEXENAME, GetGHCaveDrawVersionAndDateBuild()]));
    // écriture de l'en-tête dont les listes de groupes et les centerlines
    if (GroupeExported = -1) then
    begin
      WrtLn('');
      WrtLn('# Standard saving document');
      WrtLn('# =========================');
      WrtLn('# Survey Canvas Section - Canevas topographique');
      WrtLn('');
      InternalIdxCodeEPSG := FConversionSysteme.GetIndexSystemeByCodeEPSG(FCodeEPSG);
      if (InternalIdxCodeEPSG >= 0) then
      begin
        MySystEPSG := FConversionSysteme.GetCodeEPSGNomSysteme(InternalIdxCodeEPSG);
      end else
      begin
        MySystEPSG.CodeEPSG := DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG;
        MySystEPSG.Nom      := DEFAULT_SYSTEME_COORDONNEES_NOM;
      end;

      WrtLn('CodeEPSG' + #9 + IntToStr(MySystEPSG.CodeEPSG) + #9 + MySystEPSG.Nom);
      WrtLn('');
      WrtSectionCenterlines();   // écriture des centerlines
      WrtSectionImages();        // images
      WrtSectionSuperGroupes();  // super-groupes
      WrtSectionGroupes();       // groupes
    end
    else
    begin
      CurrentGroupe := GetGroupeByIDGroupe(GroupeExported);
      WrtLn('# Saving entities of groupe');
      //CurrentGroupe.
      WrtLn(Format('# %d: %s',[CurrentGroupe.IDGroupeEntites, CurrentGroupe.NomGroupe]));
      WrtLn('# =============================');
    end;
    // export des entités
    WrtSectionScraps();      // scraps
    WrtSectionCourbes();     // courbes
    WrtSectionSimpleLines(); // lignes simples
    WrtSectionPolylines();   // polylignes
    WrtSectionPolygones();   // polygones
    WrtSectionTextes();      // textes
    WrtSectionSymboles();    // symboles
  finally
    CloseFile(F);
  end;
end;

procedure TDocumentDessin.SaveEncryptedFile(const QFilename: TStringDirectoryFileName; const Password: string);
var
  fpIn  : Textfile;
  gzfpOut: gzFile;
  QTempDir, QTempFileGCD: String;
  MyLine: string;
  MyFormat: string;
  EWE: LongInt;
begin
  AfficherMessage(Format('%s.SaveEncryptedFile(%s)', [ClassName, QFilename]));
  MyFormat := '%s';
  QTempDir := GetGHCaveDrawDirectory() + 'Temp' + PathDelim;
  ForceDirectories(QTempDir);
  QTempFileGCD := QTempDir + 'temp_001.gcd';
  self.SaveToFile(QTempFileGCD, -1, false);
  AfficherMessage('Compress to ' + QFilename);
  AssignFile(fpIn, QTempFileGCD);
  try
    ReSet(fpIn);
    gzfpOut := gzopen(PChar(QFileName), 'w');
    if (gzfpOut <> nil) then
    begin
      try
        while(not Eof(fpIn)) do
        begin
          ReadLn(fpIn, MyLine);
          EWE := gzputs(gzfpOut, PChar(MyLine + #13#10));   //
        end;
      finally
        gzclose(gzfpOut);
      end;
    end;
  finally
    CloseFile(fpIn);
  end;
end;
//------------------------------------------------------------------------------
// END INCLUDE FILE
