//{$INCLUDE DocDessinLoadFromFile.inc}
// Include contenant la fonction de chargement du dessin
// 25/08/2015: La procédure TDocumentDessin.LoadFromFile(const FichierGCD: string): boolean;
//             qui est volumineuse, est déportée dans le fichier d'inclusion
//             DocDessinLoadFromFile.inc

// Ce fichier est une dépendance de unitdocdessin.pas


function TDocumentDessin.LoadFromFile(const FichierGCD: TStringDirectoryFileName): boolean;
type TStatusFindInclude = (tincNO_INCLUDE, tincOK_TO_READ, tincFILE_NOT_FOUND, tincANY_ERROR);
var
  //DoCreateFirstSuperGroupe: Boolean;
  MySuperGroupe: TSuperGroupe;
  MyGroupe: TGroupeEntites;
  QFichierCorrect: Integer;

  function HasInstructionInclude(const QInstructionInclude: string;
                                 const QLigne: string;
                                 const QNumeroLigne: integer;
                                 out   IncludeFileName: TStringDirectoryFileName): TStatusFindInclude;
  var
    EWE: String;
  begin
    Result := tincNO_INCLUDE;
    try
      if (Pos(QInstructionInclude, QLigne) > 0) then
      begin
        EWE := QLigne;
        System.Delete(EWE, 1, length(QInstructionInclude));
        //FFichierCenterlines := Trim(EWE);
        IncludeFileName := FDossierDuDocument + PathDelim + Trim(EWE);
        AfficherMessageErreur(Format('*** Instruction %s trouvée en ligne %d - %s', [QInstructionInclude, QNumeroLigne, IncludeFileName]));
        AfficherMessageErreur('-- Lecture du fichier: ' + IncludeFileName);
        if (not FileExists(IncludeFileName)) then // test de l'existence du fichier
        begin
          AfficherMessageErreur(Format('%s: "%s"', [rsERR_MSG_FILE_NOT_FOUND, IncludeFileName]));
          Result := tincFILE_NOT_FOUND;
          Exit;
        end;
        Result := tincOK_TO_READ;
      end;

    except
      Result := tincANY_ERROR;
    end;
  end;
  // Procédure récursive de lecture
  // Retourne 0 si aucune erreur
  //         -1 si échec de lecture du fichier ppal
  //         -2 si fichier INCLUDE introuvable
  //         -3 si échec de lecture du fichier INCLUDE
  function LireUnFichier(const QFilename: string): integer;
  var
    fp      : TextFile;
    MyLine  : String;
    NumLigne: Int64;

    QFichierToInclude: TStringDirectoryFileName;
    Argv       : TGHStringArray;
    NbAc       : Integer;
    i, Nb      : Integer;
    QBasePoint: TBaseStation;

    SG         : TSuperGroupe;
    G          : TGroupeEntites;
    MyScrap    : TScrap;
    CV         : TCourbe;
    PLG        : TPolygone;
    PLL        : TPolyLigne;
    SL         : TSimpleLigne;
    T          : TTextObject;
    O          : TSymbole;
    MyIMG      : TImageObject;

    IdxMaxGroupe: TIDGroupeEntites;
    LS: TStringList;
    function LireLigne(): string;
    begin
      ReadLn(fp, Result);
      Result := Trim(Result);
      // Ne pas remplacer les espaces par des tab: certains paramètres texte peuvent être impactés
      (* Result := StringReplace(Result, ' ', #9, [rfIgnoreCase, rfReplaceAll]); //*)
      Inc(NumLigne);
      if (NumLigne mod 10000 = 0) then AfficherMessage(Format('Reading line %d', [NumLigne]));
    end;
  begin
    Result := -1;
    // lecture du fichier
    NumLigne := 0;
    AssignFile(fp, QFilename);
    LS  := TStringList.Create;
    try
      ReSet(fp);
      while (not EOF(fp)) do
      begin
        MyLine := LireLigne();
        if (MyLine = '') then Continue;
        if (Pos('#', MyLine) > 0) then Continue; // commentaires
        if (Pos('CodeEPSG', MyLine) = 1) then // code EPSG; le mot-clé doit être impérativement en début de ligne
        begin
          Argv := Split(MyLine, #9);
          FCodeEPSG := StrToIntDef(Trim(Argv[1]), DEFAULT_SYSTEME_COORDONNEES_CODE_EPSG);

          AfficherMessageErreur(Format('Code EPSG:%d', [FCodeEPSG]));
        end;
        // Lecture des centerlines
        case (HasInstructionInclude(INSTRUCTION_INCLUDE_GCP, MyLine, NumLigne, QFichierToInclude)) of
          tincNO_INCLUDE: // on lit la section basepoints
            begin
              if (MyLine = BASEPOINTMARKER) then
              begin
                AfficherMessage(Format('--> (%d) - ''%s'' section found',[NumLigne, BASEPOINTMARKER]));
                while (True) do
                begin
                  MyLine := LireLigne();
                  if MyLine ='' then Continue;
                  if (Pos('#', MyLine)>0) then Continue; // commentaires

                  if (MyLine = ENDBASEPOINTMARKER) then   // si le marqueur de fin de section 'Basepoints' est trouvé,
                  begin
                    TrierBasePointsParIDBaseStation();            // Trier la table des basepoints
                    Break;                                // puis sortir du while
                  end;
                  self.AddABasePointFromString(MyLine);
                end;
              end; //if (Line = BASEPOINTMARKER) then begin
            end;
          tincOK_TO_READ: // on inclut le fichier
            begin
              FCenterlinesIsIncludeFileGCP := True;
              FFichierCenterlines := ExtractFileName(QFichierToInclude);
              AfficherMessageErreur('*** Lecture du fichier include: ' + QFichierToInclude);
              Result := LireUnFichier(QFichierToInclude);
              AfficherMessageErreur(Format('--> Resultat: %d', [Result]));
            end;
          tincFILE_NOT_FOUND:
            begin
              Result := -2;
              AfficherMessageErreur(Format('*** [%d] %s: %s *** ', [Result, rsERR_MSG_FILE_NOT_FOUND, QFichierToInclude]));
              Exit;
            end;
          tincANY_ERROR:
            begin
              Result := -3;
              AfficherMessageErreur(Format('*** [%d] %s: %s *** ', [Result, rsERR_MSG_IMPORT_CENTERLINE, QFichierToInclude]));
              Exit;
            end;
        end;
        //*)
        // lecture images
        if (MyLine = IMAGESSECTION) then
        begin
          AfficherMessage(Format('--> (%d) - ''%s'' section found',[NumLigne, IMAGESSECTION]));
          while (True) do
          begin
            MyLine := LireLigne();
            if (MyLine = '') then Continue;
            if (Pos('#', MyLine)>0) then Continue; // commentaires
             //LirePolyLigne(MyLine);
            if (MyLine = ENDIMAGESSECTION) then
            begin
              AfficherMessage('--> '''+ ENDSUPERGROUPESSECTION +''' section found');
              Break;
            end;
            // check sommaire
            MyIMG := ExtractImageFromStr(MyLine);
            AddImage(MyIMG);
          end; // while (True) do begin
        end;
        // lecture super-groupes
        if (MyLine = SUPERGROUPESSECTION) then
        begin
          AfficherMessage(Format('--> (%d) - ''%s'' section found',[NumLigne, SUPERGROUPESSECTION]));
          while (True) do
          begin
            MyLine := LireLigne();
            if (MyLine = '') then Continue;
            if (Pos('#', MyLine)>0) then Continue; // commentaires

             //LirePolyLigne(MyLine);
            if (MyLine = ENDSUPERGROUPESSECTION) then
            begin
              AfficherMessage('--> '''+ ENDSUPERGROUPESSECTION +''' section found');
              Break;
            end;
            // check sommaire
            SG := ExtractSuperGroupeFromStr(MyLine);
            AddSuperGroupe(SG);
          end; // while (True) do begin
        end;
        // lecture groupes
        if (MyLine = GROUPESSECTION) then
        begin
          IdxMaxGroupe := -1;
          AfficherMessage(Format('--> (%d) - ''%s'' section found',[NumLigne, GROUPESSECTION]));
          while (True) do
          begin
            MyLine := LireLigne();
            if (MyLine = '') then Continue;
            if (Pos('#', MyLine) > 0) then Continue; // commentaires

            //LirePolyLigne(MyLine);
            if (MyLine = ENDGROUPESSECTION) then begin
             AfficherMessage('--> '''+ ENDGROUPESSECTION +''' section found');
             Break;
            end;
            G := ExtractGroupeFromStr(MyLine);
            if (G.IDGroupeEntites > IdxMaxGroupe) then IdxMaxGroupe := G.IDGroupeEntites;
            AddGroupe(G, false);
          end;
          AfficherMessage(Format('%d groupes ; IdxMax = %d', [GetNbGroupes(), IdxMaxGroupe]));
        end;
        // lecture scraps
        LS.Clear;
        if (MyLine = SCRAPSSECTION) then
        begin
          AfficherMessage(Format('--> (%d) - ''%s'' section found',[NumLigne, SCRAPSSECTION]));
          while (True) do
          begin
            MyLine := LireLigne();
            if (MyLine = '') then Continue;
            if (Pos('#', MyLine) > 0) then Continue; // commentaires
            if (Pos(SCRAP, MyLine ) > 0) then
            begin
             try
               Argv := Split(MyLine, #9);
               //# Scrap Parameters: IDScrap, IDGroupe, ColorR, ColorG, ColorB, Transparency (0..255), ZOrder, Name
               // scrap	1	14	255	126	120	128	0.00	Scrap1
               MyScrap.IDGroupe     := StrToIntDef(Argv[2], 0);
               MyScrap.Couleur      := RGBToColor(StrToIntDef(Argv[3], 255),
                                           StrToIntDef(Argv[4], 0),
                                           StrToIntDef(Argv[5], 0));
               MyScrap.Opacite  := StrToIntDef(Argv[6], 128);
               MyScrap.Nom      := Trim(Argv[7]);
               MyScrap.LastModified := Now();
               SetLength(MyScrap.Sommets, 0);
               // liste provisoire pour les arcs
               LS.Clear;
               while (True) do
               begin
                 MyLine := LireLigne();
                 if (MyLine = '') then Continue;
                 if (Pos('#', MyLine) > 0) then Continue; // commentaires
                 if (Pos(ENDSCRAP, MyLine) > 0) then Break;
                 LS.Add(MyLine);
               end;
               NbAc := LS.Count;
               SetLength(MyScrap.Sommets, NbAc);
               for i := 0 to High(MyScrap.Sommets) do MyScrap.Sommets[i] := ExtractVertexPolyFromStr(LS.Strings[i]);
               // OK ? On ajoute
               AddScrap(MyScrap, false);
             except;
               AfficherMessageErreur(Format('[%d] ** Error reading scrap', [NumLigne]));
             end;
            end;
            if (MyLine = ENDSCRAPSSECTION) then
            begin
             AfficherMessage('--> '''+ ENDSCRAPSSECTION +''' section found');
             Break;
            end;
          end;
        end; // if Line = POLYGONSECTION;

        // lecture courbes
        LS.Clear;
        if (MyLine = CURVESSECTION) then
        begin
          AfficherMessage(Format('--> (%d) - ''%s'' section found',[NumLigne, CURVESSECTION]));
          while (True) do
          begin
            Inc(i);
            MyLine := LireLigne();
            if MyLine ='' then Continue;
            if (Pos('#', MyLine)>0) then Continue; // commentaires
            if (Pos(ENTITYCURVE, MyLine ) > 0) then
            begin
             try
               Argv := Split(MyLine, #9);
               CV.IDGroupe      := StrToIntDef(Argv[2], 0);
               CV.IDStyleCourbe := TNatureObjetCourbe(StrToIntDef(Argv[3], 0));
               CV.Closed        := (1 = StrToIntDef(Argv[4], 0));
               CV.LastModified  := Now();

               SetLength(CV.Arcs, 0);
               // liste provisoire pour les arcs
               LS.Clear;
               while (True) do
               begin
                 MyLine := LireLigne();
                 if MyLine ='' then Continue;
                 if (Pos('#', MyLine)>0) then Continue; // commentaires
                 if (Pos(ENDENTITYCURVE, MyLine) > 0) then Break;
                 LS.Add(MyLine);
               end;
               NbAc := LS.Count;
               SetLength(CV.Arcs, NbAc);
               for i:=0 to High(CV.Arcs) do CV.Arcs[i] := ExtractArcPolyFromStr(LS.Strings[i]);
               // OK ? On ajoute
               //CV := CalcBoundingBoxCourbe(CV);
               CV.MarkToDelete  := false;
               AddCourbe(CV, false);
             except;
               AfficherMessageErreur(Format('[%d] ** Error reading curve', [NumLigne]));
             end;
            end;
            //LirePolyLigne(MyLine);
            if (MyLine = ENDCURVESSECTION) then begin
             AfficherMessage('--> '''+ ENDCURVESSECTION +''' section found');
             Break;
            end;
          end;
        end; // if Line = CURVESSECTION;
        // lecture polylignes
        LS.Clear;
        if (MyLine = POLYLINESSECTION) then
        begin
          AfficherMessage(Format('--> (%d) - ''%s'' section found',[NumLigne, POLYLINESSECTION]));
          while (True) do
          begin
            MyLine := LireLigne();
            if (MyLine = '') then Continue;
            if (Pos('#', MyLine) > 0) then Continue; // commentaires
            if (Pos(ENTITYPOLYLINE, MyLine ) > 0) then
            begin
              try
                Argv := Split(MyLine, #9);
                PLL.IDGroupe         := StrToIntDef(Argv[2], 0);
                PLL.IDStylePolyLine  := TNatureObjetCourbe(StrToIntDef(Argv[3], 0));
                PLL.Closed           := (1 = StrToInt(Argv[4]));//(1 = StrToIntDef(Argv[4], 0));
                PLL.LastModified     := Now(); //StrToDateTimeDef(Argv[4], Now());
                SetLength(PLL.Sommets, 0);
                // liste provisoire pour les arcs
                LS.Clear;
                while (True) do
                begin
                  MyLine := LireLigne();
                  if MyLine ='' then Continue;
                  if (Pos('#', MyLine) > 0) then Continue; // commentaires
                  if (Pos(ENDENTITYPOLYLINE, MyLine) > 0) then Break;
                  LS.Add(MyLine);
                end;
                NbAc := LS.Count;
                SetLength(PLL.Sommets, NbAc);
                for i:=0 to High(PLL.Sommets) do PLL.Sommets[i] := ExtractVertexPolyFromStr(LS.Strings[i]);
                PLL.MarkToDelete  := false;
                AddPolyLigne(PLL, false);
              except;
                AfficherMessageErreur(Format('[%d] ** Error reading polyline %d', [NumLigne, GetNbPolylignes()]));
              end;
            end;
            //LirePolyLigne(Line);
            if (MyLine = ENDPOLYLINESSECTION) then
            begin
              AfficherMessage('--> '''+ ENDPOLYLINESSECTION +''' section found');
              Break;
            end;
         end;
        end; // if Line = POLYLINESSECTION;
        // lecture polygones
        LS.Clear;
        if (MyLine = POLYGONSECTION) then
        begin
          AfficherMessage(Format('--> (%d) - ''%s'' section found',[NumLigne, POLYGONSECTION]));
          while (True) do
          begin
            MyLine := LireLigne();
            if (MyLine = '') then Continue;
            if (Pos('#', MyLine) > 0) then Continue; // commentaires
            if (Pos(ENTITYPOLYGON, MyLine ) > 0) then
            begin
             try
               Argv := Split(MyLine, #9);
               PLG.IDGroupe         := StrToIntDef(Argv[2], 0);
               PLG.IDStylePolygone  := TNatureObjetPolygone(StrToIntDef(Argv[3], 0));
               PLG.LastModified     := StrToDateTimeDef(Argv[4], Now());
               SetLength(PLG.Sommets, 0);
               // liste provisoire pour les arcs
               LS.Clear;
               while (True) do
               begin
                 MyLine := LireLigne();
                 if MyLine ='' then Continue;
                 if (Pos('#', MyLine) > 0) then Continue; // commentaires
                 if (Pos(ENDENTITYPOLYGON, MyLine) > 0) then Break;
                 LS.Add(MyLine);
               end;
               NbAc := LS.Count;
               SetLength(PLG.Sommets, NbAc);
               for i:=0 to High(PLG.Sommets) do PLG.Sommets[i] := ExtractVertexPolyFromStr(LS.Strings[i]);
               // OK ? On ajoute
               PLG.MarkToDelete := false;
               AddPolygone(PLG, false);
             except;
               AfficherMessageErreur(Format('[%d] ** Error reading polygon', [NumLigne]));
             end;
            end;
            if (MyLine = ENDPOLYGONSECTION) then
            begin
             AfficherMessage('--> '''+ ENDPOLYGONSECTION +''' section found');
             Break;
            end;
          end;
        end; // if Line = POLYGONSECTION;
        // lecture ligne
        if (MyLine = LINESECTION) then
        begin
          AfficherMessage(Format('--> (%d) - ''%s'' section found',[NumLigne, LINESECTION]));
          while (True) do
          begin
            MyLine := LireLigne();
            if MyLine ='' then Continue;
            if (Pos('#', MyLine)>0) then Continue; // commentaires
            if (MyLine = ENDLINESECTION) then begin
              AfficherMessage('--> '''+ ENDLINESECTION +''' section found');
              Break;
            end;
            SL := ExtractSimpleLineFromStr(NumLigne, MyLine);
            AddSimpleLigne(SL, false);
          end;
        end; // if MyLine = LINESECTION;
        if (MyLine = TEXTSECTION) then
        begin
          AfficherMessage(Format('--> (%d) - ''%s'' section found',[NumLigne, TEXTSECTION]));
          while (True) do
          begin
            MyLine := LireLigne();
            if MyLine ='' then Continue;
            if (Pos('#', MyLine)>0) then Continue; // commentaires
            if (MyLine = ENDTEXTSECTION) then begin
              AfficherMessage('--> '''+ ENDTEXTSECTION +''' section found');
              Break;
            end;
            T := ExtractObjTexteFromStr(MyLine);
            AddTexte(T, false);
          end;
        end; // if MyLine = TEXTSECTION;
        if (MyLine = PONCTOBJSECTION) then
        begin
          AfficherMessage(Format('--> (%d) - ''%s'' section found',[NumLigne, PONCTOBJSECTION]));
          while (True) do
          begin
            MyLine := LireLigne();
            if MyLine ='' then Continue;
            if (Pos('#', MyLine)>0) then Continue; // commentaires
            if (MyLine = ENDPONCTOBJSECTION) then begin
             AfficherMessage('--> '''+ PONCTOBJSECTION +''' section found');
             Break;
            end;
            O := ExtractSymbolFromStr(MyLine);
            AddSymbole(O, false);
          end;
        end; // if Line = PONCTOBJSECTION;
      end; // while général not eof

      //*****************************************
      // controle de la section basepoints
      // tri par index
      FCenterLines.TrierBasePointsParIDBaseStation();
      TrierGroupesByZOrder();
      // filtres
      FCenterLines.MetaFiltrer('');
      // mini et maxi
      AfficherMessage('-- SetMiniEtMaxi');
      SetMiniEtMaxi();
      // calcul des boites englobantes des groupes
      AfficherMessage('-- CalcBoundingBoxAllGroupes');
      CalcBoundingBoxAllGroupes;
      AfficherMessage('-- NbObjets');
      //******************************************
      AfficherMessageErreur(Format('%d images', [GetNbImages()]));
      AfficherMessage(Format('%d supergroupes', [GetNbSuperGroupes()]));
      AfficherMessage(Format('%d groupes', [GetNbGroupes()]));
      AfficherMessage(Format('%d scraps', [GetNbScraps()]));
      AfficherMessage(Format('%d courbes', [GetNbCourbes()]));
      AfficherMessage(Format('%d polylignes', [GetNbPolylignes()]));
      AfficherMessage(Format('%d polygones', [GetNbPolygones()]));
      AfficherMessage(Format('%d lignes', [GetNbSimpleLignes()]));
      AfficherMessage(Format('%d symboles', [GetNbSymboles()]));
      AfficherMessage(Format('%d labels', [GetNbTextes()]));
      //********************************************
      // calcul du nombre d'objets pour les groupes
      //CalcNbObjetsGroupes();
      AfficherMessage(FDocumentName + ' ouvert avec succes');

      Result := 0;
      LS.Clear;
    finally
      CloseFile(fp);
      LS.Free;
    end;
  end;
begin
  Result := False;
  // attribution du nom du document
  FDocumentName := FichierGCD;
  // dossier contenant le document
  FDossierDuDocument := ExtractFileDir(FichierGCD);
  // purge des listes
  FCenterLines.InitialiseCenterLines;
  FCenterlinesIsIncludeFileGCP := false;
  PurgerListesObjets();
  AfficherMessage(Format('%s.LoadFromFileExt(%s)',[ClassName, FichierGCD]));
  try
    QFichierCorrect := LireUnFichier(FichierGCD);
    case QFichierCorrect of
      -1:
      begin
        AfficherMessageErreur('--> Echec en lecture du fichier principal');
      end;
      -2:
      begin
        AfficherMessageErreur('--> Un des fichiers appelés par $INCLUDE est introuvable');
      end;
      -3:
      begin
        AfficherMessageErreur('--> Echec en lecture de fichiers à inclure');
      end;
      0: // Lecture OK
      begin
        if (self.GetNbSuperGroupes() = 0) then
          begin
            //MySuperGroupe.ID := 0;
            MySuperGroupe.NomSuperGroupe := 'SuperGroupe0';
            //MySuperGroupe.Decalage   := MakeTPoint3Df(0.0, 0.0, 0.0);
            MySuperGroupe.Displayed  := True;
            //MySuperGroupe.Locked     := False;
            AddSuperGroupe(MySuperGroupe);
          end;
          if (self.GetNbGroupes() = 0) then
          begin
            MyGroupe.IDGroupeEntites := 0;
            MyGroupe.NomGroupe       := 'Groupe0';
            MyGroupe.CouleurGroupe   := clBlue;
            MyGroupe.Decalage.Empty();
            MyGroupe.DateLastModif   := Now();
            MyGroupe.Filtres         := '';
            //MyGroupe.IDSuperGroupe   := 0;
            //MyGroupe.Locked          := false;
            MyGroupe.Visible         := true;
            AddGroupe(MyGroupe, false);
          end;
      end;
      else
      begin
        AfficherMessageErreur(Format('Erreur ouverture du fichier: %d', [QFichierCorrect]));
      end;
    end;
    Result := (QFichierCorrect = 0);
    AfficherMessageErreur(Format('Code retour: %d', [QFichierCorrect]));
  except

  end;
end;



function  TDocumentDessin.LoadEncryptedFile(const QFilename: TStringDirectoryFileName; const Password: string): boolean;
begin
  result := false;
  AfficherMessage(Format('%s.LoadEncryptedFile(%s)', [ClassName, QFilename]));
end;
