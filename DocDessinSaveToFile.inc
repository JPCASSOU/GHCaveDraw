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
  i, j: integer;
  L : TSimpleLigne;
  C : TCourbe;
  PL: TPolyLigne;
  PG: TPolygone;
  O : TSymbole;
  V : TVertexPolygon;
  T : TTextObject;
  SGRP: TSuperGroupe;
  CurrentGroupe : TGroupeEntites;
  GRP           : TGroupeEntites;
  A : TArcCourbe;
  BP: TBaseStation;
  SL: TStyleCourbe;
  Nb: Integer;
  NbObj, n: Integer;
  WU: TIDBaseStation;
  S: TScrap;
  QAT: String;
  QImg: TImageObject;
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
    WrtLn(Format('# Section: %s (%d elements)', [HeadTitle, NbElements]));
    WrtLn('#-------------------------------------------------------');
    WrtLn('# Parameters: ' + Autodoc);
    WrtLn('');
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
      WrtLn('');
      WrtLn('# Survey Canvas Section');
      WrtLn('# Canevas topographique');
      WrtLn('');
      n := FConversionSysteme.GetIndexSystemeByCodeEPSG(FCodeEPSG);
      if (n >= 0) then
      begin
        MySystEPSG := FConversionSysteme.GetCodeEPSGNomSysteme(n);
      end else
      begin
        MySystEPSG.CodeEPSG := DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG;
        MySystEPSG.Nom      := DEFAULT_SYSTEME_COORDONNEES_NOM;
      end;

      WrtLn('CodeEPSG' + #9 + IntToStr(MySystEPSG.CodeEPSG) + #9 + MySystEPSG.Nom);
      WrtLn('');
      // écriture des centerlines
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
        for i:=0 to FCenterLines.GetNbBasePoints - 1 do
        begin
          BP := FCenterLines.GetBasePoint(i);
          WrtLn(GenererLigneBasePoint(i, BP));
        end;
        WrtLn(ENDBASEPOINTMARKER);
      end;
      // images
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
                        QImg.PositionCoinsImage.X1, QImg.PositionCoinsImage.Y1,
                        QImg.PositionCoinsImage.X2, QImg.PositionCoinsImage.Y2,
                        QImg.Opacite,
                        QImg.SrcFilename,
                        QImg.Description
                       ]));
        end;
        WrtLn(ENDIMAGESSECTION);
      end;
      // super-groupes
      Nb := GetNbSuperGroupes();
      WriteCommentsHeadSection('SuperGroupes', Nb, 'IDSuperGroupe, NomGroupe, Offset XYZ components, Displayed, Locked');
      AfficherMessage(Format('--> Saving %d supergroupes', [Nb]));
      WrtLn(SUPERGROUPESSECTION);
      for i := 0 to Nb - 1 do
      begin
        SGRP := GetSuperGroupe(i);
        WrtLn(format('  supergroupe' + #9 + '%d' + #9 +
                       '%s' + #9 + '%d' + #9 + '%d' + #9 +
                       FMT_COORDS + #9 + '%s',
                       [0, //SGRP.ID,
                        SGRP.NomSuperGroupe,
                        IIF(SGRP.Displayed, 1, 0),
                        0, //IIF(SGRP.Locked, 1, 0),
                        0.00, 0.00, 0.00, //SGRP.Decalage.X, SGRP.Decalage.Y, SGRP.Decalage.Z,
                        ArrayOfIdxGroupesToStr(SGRP.ListeGroupes)
                       ]));
      end;
      WrtLn(ENDSUPERGROUPESSECTION);
      WrtLn('');
      // groupes
      Nb := GetNbGroupes();
      WriteCommentsHeadSection('Groupes', Nb, 'IDGroupe, NomGroupe, Colour, Offset XYZ components, ID SuperGroup, LastModified, DecalageActif (1=Actif), ZOrder');
      AfficherMessage(Format('--> Saving %d groupes', [Nb]));
      WrtLn(GROUPESSECTION);
      for i:=0 to Nb - 1 do
      begin
        GRP := GetGroupe(i);
        WrtLn(GenererLigneGroupe(i, GRP));
      end;
      WrtLn(ENDGROUPESSECTION);
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
    NbObj := GetNbScraps();
    AfficherMessage(Format('--> Saving %d scraps', [NbObj]));
    if (NbObj = 0) then
    begin
      WrtLn(Format(NOENTITIES, [SCRAPSSECTION]));
      WrtLn('');
    end else
    begin
      WrtLn('');
      WriteCommentsHeadSection('Scraps', NbObj, '');
      WrtLn('');
      WrtLn(SCRAPSSECTION);
      for i:=0 to NbObj - 1 do
      begin
        S  := GetScrap(i);
        if (CanExportEntity(S.IDGroupe, GroupeExported)) then
        begin
          WrtLn('  # Last modification: ' + DateTimeToStr(Now()) );
          WrtLn('  # Scrap Parameters: IDScrap, IDGroupe, ColorR, ColorG, ColorB, Transparency (0..255), Name ');
          WrtLn('  # Vertex  Parameters: IDStation, Offset XYZ Components');
          WrtLn(GenererLigneEnteteScrap(i, S));
          for j := 0 to High(S.Sommets) do
          begin
            V := S.Sommets[j];
            WrtLn(GenererLigneVertexPolygon(j, V));
          end;
          WrtLn(GenererLigneEndScrap());
        end; // if CanExportEntity()
      end;
      WrtLn(ENDSCRAPSSECTION);
    end;
    //--------------------------------------------------------------------------

    NbObj := GetNbCourbes();
    AfficherMessage(Format('--> Saving %d curve objects', [NbObj]));
    if (NbObj = 0) then
    begin
      WrtLn(Format(NOENTITIES, [CURVESSECTION]));
      WrtLn('');
    end else
    begin
      WrtLn('');
      WriteCommentsHeadSection('Courbes', NbObj, '');
      WrtLn('');
      WrtLn(CURVESSECTION);
      for i:=0 to NbObj - 1 do
      begin
        C  := GetCourbe(i);
        if (CanExportEntity(C.IDGroupe, GroupeExported)) then
        begin
          WrtLn('  # Last modification: ' + DateTimeToStr(Now()) );
          WrtLn('  # Parameters for curve: ID Curve, IDGroupe, IDStyleCourbe, Closed (=1)');
          WrtLn('  # Parameters for each arc: IDStationP1, Offset_from_P1, "-1.0", Tangent_at_P1, IDStationP1, Offset_from_P2, Tangent_at_P2, "-2.0"');
          //DateTimeToStr(C.LastModified)
          WrtLn(GenererLigneEnteteCourbe(i, C));
          for j := 0 to High(C.Arcs) do
          begin
            A := C.Arcs[j];
            WrtLn(GenererLigneArcCourbe(j, A));
          end;
          WrtLn(GenererLigneEndCourbe());
        end; // if CanExportEntity()
      end;
      WrtLn(ENDCURVESSECTION);
    end;
    NbObj := GetNbSimpleLignes();
    AfficherMessage(Format('--> Saving %d line objects', [NbObj]));
    if (NbObj = 0) then
    begin
      WrtLn(Format(NOENTITIES, [LINESECTION]));
      WrtLn('');
    end else
    begin
      WriteCommentsHeadSection('Lines', NbObj, 'ID, IDGroupe, Style Line, First Point: IDStation , Offset XYZ, Second Point: IDStation , Offset XYZ');
      WrtLn(LINESECTION);
      for i:=0 to NbObj - 1 do
      begin
        L  := GetSimpleLigne(i);
        if (CanExportEntity(L.IDGroupe, GroupeExported)) then
        begin
          WrtLn(GenererLigneSimpleLigne(i, L));
        end;
      end;
      WrtLn(ENDLINESECTION);
    end;
    // polylignes  OK
    NbObj := GetNbPolylignes();
    AfficherMessage(Format('--> Saving %d polylines objects', [NbObj]));
    if (NbObj = 0) then
    begin
      WrtLn(Format(NOENTITIES, [POLYLINESSECTION]));
      WrtLn('');
    end else
    begin
      WriteCommentsHeadSection('Polylines', NbObj, '');
      WrtLn(POLYLINESSECTION);
      for i:=0 to NbObj - 1 do
      begin
        PL  := GetPolyligne(i);
        if (CanExportEntity(PL.IDGroupe, GroupeExported)) then
        begin
          WrtLn('  # Last modification: ' + DateTimeToStr(Now()) );
          WrtLn('  # Polylines Parameters: IDPolygon, IDGroupe, Style PolyLine, Closed (= 1)');
          WrtLn('  # Vertex  Parameters: IDStation, Offset XYZ Components');
          WrtLn(GenererLigneEntetePolyligne(i, PL));

          for j := 0 to High(PL.Sommets) do
          begin
            V := PL.Sommets[j];
            WrtLn(GenererLigneVertexPolygon(j, V));
          end;
          WrtLn(GenererLigneEndPolyligne());
        end;
      end;
      WrtLn(ENDPOLYLINESSECTION);
    end;
    // polygones
    NbObj := GetNbPolygones();
    AfficherMessage(Format('--> Saving %d polygon objects', [NbObj]));
    if (NbObj = 0) then
    begin
      WrtLn(Format(NOENTITIES, [POLYGONSECTION]));
      WrtLn('');
    end else
    begin
      WriteCommentsHeadSection('Polygons', NbObj, '');
      WrtLn(POLYGONSECTION);
      for i:=0 to NbObj - 1 do
      begin
        PG  := GetPolygone(i);
        if (CanExportEntity(PG.IDGroupe, GroupeExported)) then
        begin
          WrtLn(GenererLigneEntetePolygone(i, PG));
          WrtLn('  # Last modification: ' + DateTimeToStr(Now()) );
          WrtLn('  # Polygon Parameters: IDPolygon, IDGroupe, Style Polygon');
          WrtLn('  # Vertex  Parameters: IDStation, Offset XYZ Components');
          for j := 0 to High(PG.Sommets) do
          begin
            V := PG.Sommets[j];
            WrtLn(GenererLigneVertexPolygon(j, V));
          end;
          WrtLn(GenererLigneEndPolygone());
        end;
      end;
      WrtLn(ENDPOLYGONSECTION);
    end;
    // textes
    NbObj := GetNbTextes();
    AfficherMessage(Format('--> Saving %d text objects', [NbObj]));
    if (NbObj = 0) then
    begin
      WrtLn(Format(NOENTITIES, [TEXTSECTION]));
      WrtLn('');
    end else
    begin
      WriteCommentsHeadSection('Texts', NbObj, 'ID, IDGroupe, Style Text, IDStation, Offset XYZ Components, Text Alignment, MaxLength, Text string');
      WrtLn(TEXTSECTION);
      for i:=0 to NbObj - 1 do
      begin
        T  := GetTexte(i);
        if (CanExportEntity(T.IDGroupe, GroupeExported)) then
        begin
          WrtLn(GenererLigneTexte(i, T));
        end;
      end;
      WrtLn(ENDTEXTSECTION);
    end;
    NbObj := GetNbSymboles();
    AfficherMessage(Format('--> Saving %d ponctual objects', [NbObj]));
    if (NbObj = 0) then
    begin
      WrtLn(Format(NOENTITIES, [PONCTOBJSECTION]));
      WrtLn('');
    end else
    begin
      WriteCommentsHeadSection('Symbols', NbObj, 'ID, IDGroupe, TypeObject, Colour, IDStation, Offset XYZ Coordinates, Rotation, XY Scale, Legend or filename, Tag, Display photo');
      WrtLn(PONCTOBJSECTION);
      for i:=0 to NbObj - 1 do
      begin
        O  := GetSymbole(i);
        if (CanExportEntity(O.IDGroupe, GroupeExported)) then
        begin
          WrtLn(GenererLigneSymbole(i, O));
        end;
      end;
      WrtLn(ENDPONCTOBJSECTION);
    end;
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
