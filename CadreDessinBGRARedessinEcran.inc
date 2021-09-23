//{$INCLUDE CadreDessinBGRARedessinEcran.inc}
// Include contenant la procédure TCadreDessinBGRA.RedessinEcran
// 20/08/2015: La procédure TCadreDessinBGRA.RedessinEcran(const DoGenererImage: boolean),
//             qui est très volumineuse, est déportée dans le fichier d'inclusion
//             CadreDessinBGRARedessinEcran.inc
// 25/08/2015: Echelle de couleurs pour les pentes
// 30/09/2015: Ajout du mode 'croix' pour les quadrillages

// Ce fichier est une dépendance de cdrdessincanvasBGRA.pas


// redessin écran
function TCadreDessinBGRA.RedessinEcran(const DoGenererImage: boolean): integer;
var
  FDrwCurrentMode   : TModeSelectionEntites;
  FDrwCurrentIdxObj : integer;
  ii: integer;
  TmpBuffer: TGHCaveDrawDrawingContext;
  RF       : TRect;
  QEWE: TSymbole;
  QStyleCourbe: TStyleCourbe;
  NbGroupes: Integer;
  NumGroupe: Integer;
  MyGroupe: TGroupeEntites;
  NbCourbes: Integer;
  NbScraps: Integer;
  NbPolyLines: Integer;
  NbPolygones: Integer;
  NbSymboles: Integer;
  NbTextes: Integer;
  NbSimpleLignes: Integer;
  NbImages, QPointDePassage: Integer;
  MyImage: TImageObject;
  EWE: String;
  t: TDateTime;
  HHH, MMM, SSS, MMS: word;

  // dessin de la station courante
  procedure DessinerCurrentBaseStation();
  const
    RD = 8;
  begin
    TmpBuffer.DrawShape(FCurrentBasePoint.PosStation.X, FCurrentBasePoint.PosStation.Y, 0, RD, RD, clYellow, clRED);
  end;
  //----------------------------------------------------------------------------
  // dernière station topo trouvée (peut être différente de la station par défaut)
  procedure DrawLastStation(const BS: TBaseStation);
  const
    R = 8;
  var
    PM : TPoint2Df;
    PP : TPoint;
  begin
    AfficherMessage(Format('%s.DrawLastStation(%d)', [self.ClassName, BS.IDStation]));
    if (BS.IsStationTopo()) then
    begin
      try
        PM.X := BS.PosStation.X;
        PM.Y := BS.PosStation.Y;
        PP:= TmpBuffer.QGetCoordsPlan(PM);
        TmpBuffer.CanvasBGRA.Pen.Style   := psSolid;
        TmpBuffer.CanvasBGRA.Pen.Color   := clBlue;
        TmpBuffer.CanvasBGRA.Brush.Color := clAqua;
        TmpBuffer.CanvasBGRA.EllipseC(PP.X, PP.Y, R, R);
      except
        pass;
      end;
    end;
  end;
  // dessin des objets temporaires (directement sur le canevas)
  procedure DrawTmpObj_FreeHandPoints();
  var
    i, Nb: Integer;
    PM: TPoint2Df;
    PP: TPoint;
  begin
    Nb := FCourbeFreehand.GetNbPoints();
    if (Nb > 0) then
    begin
      Vue.Canvas.Pen.Color := clGray;
      for i := 0 to Nb - 1 do
      begin
        PM := FCourbeFreehand.GetPoint2d(i);
        PP := TmpBuffer.QGetCoordsPlan(PM);
        Vue.Canvas.EllipseC(PP.X, PP.Y, 2, 2);
      end;
    end;
  end;
  procedure DrawTmpObj_CourbeProvisoire();
  begin
    DessinerCourbeProvisoire(vue.Canvas);
  end;
  procedure DrawBoundingBoxGroupe(const GRP: TGroupeEntites);
  begin
    if (not GRP.Visible) then Exit;
    try
      if (GRP.BoundingBox.IsValid()) then
      begin
        TmpBuffer.DefineCrayon(psSolid, 0, clMaroon, 255);
        TmpBuffer.DefineBrosse(bsClear, clWhite, 255);
        TmpBuffer.DrawRectangle(GRP.BoundingBox.C1, GRP.BoundingBox.C2);
        TmpBuffer.RestoreBrosse();
        TmpBuffer.RestoreCrayon();
      end;
    except
    end;
  end;
  procedure thurlutte();
  begin
    AfficherMessage(Format('Groupe %d: %d - %s', [NumGroupe, MyGroupe.IDGroupeEntites, MyGroupe.NomGroupe]));
  end;

begin
  Result := 0;
  QPointDePassage := 0;
  SetMsgInLbMessage(errBUSY, 'Redessin en cours ...');
  // sémaphore 'FRedessinInProcess' est armé ? = On quitte (le dessin est en cours)
  if (FRedessinInProcess) then Exit;
  t := Now();
  FRedessinInProcess := True;     // Armement du sémaphore
  try
    TmpBuffer:= TGHCaveDrawDrawingContext.Create(Vue.Width, Vue.Height, BGRA(Red(FParamsVue2D.BackGroundColor),
                                                 Green(FParamsVue2D.BackGroundColor),
                                                 Blue(FParamsVue2D.BackGroundColor),
                                                 255));
    begin
      try
        if (Not FDoDraw) then Exit;
        // paramétrage
        TmpBuffer.SetDocuTopo(FDocumentDessin);
        TmpBuffer.SetParamDessin(FParamsVue2D);
        TmpBuffer.SetTextures(FTexturesPolygones);
        TmpBuffer.SetBounds(FRXMini, FRYMini, FRXMaxi, FRYMaxi);
        // début conventionnel
        TmpBuffer.BeginDrawing();
        AfficherMessage('Begin drawing');
        //************************
        // le dessin de la cavité
        NbGroupes       := FDocumentDessin.GetNbGroupes();
        NbScraps        := FDocumentDessin.GetNbScraps();
        NbCourbes       := FDocumentDessin.GetNbCourbes();
        NbPolyLines     := FDocumentDessin.GetNbPolylignes();
        NbPolygones     := FDocumentDessin.GetNbPolygones();
        NbSimpleLignes  := FDocumentDessin.GetNbSimpleLignes();
        NbSymboles      := FDocumentDessin.GetNbSymboles();
        NbTextes        := FDocumentDessin.GetNbTextes();
        // les images de fond
        if (tedIMAGES in FParamsVue2D.ElementsDrawn) then
        begin
          NbImages := FDocumentDessin.GetNbImages();
          if (NbImages > 0) then
          begin
            for ii := 0 to NbImages - 1 do TmpBuffer.DrawImage(FDocumentDessin.GetImage(ii));
          end;
        end;
        AfficherMessage('Begin drawing');
        for NumGroupe := 0 to NbGroupes - 1 do
        begin
          FDrwCurrentMode := mseNONE;
          MyGroupe := FDocumentDessin.GetGroupe(NumGroupe);
          // tracer le BBX du groupe
          // on teste si le groupe est partiellement visible dans la fenêtre
          // avant de le tracer (forte accélération du dessin)
          if (GroupeIsInView(MyGroupe) and (MyGroupe.Visible)) then
          begin
            // Bug du compilateur suspecté +++
            // Symptômes: Freeze au redessin
            // Reproductible: Oui
            // Systématique : Oui
            // Particularités:
            // 1. Si un appel de AfficherMessage() est effectué, le bug ne se produit pas, même avec un appel indirect via la fonction thurlutte()
            // 2. Si la fonction thurlutte() ne contient pas un appel à AfficherMessage(), le bug se produit
            // 3. Si un block de protection est installé, le bug ne se produit pas
            //    Toutefois, aucune erreur n'est interceptée (le programme passe par la ligne QPointDePassage := 9999;)
            //thurlutte(); // Fonction alternative au block de protection
            try
              // les scraps sont tracés dans tous les cas
              FDrwCurrentMode := mseSCRAPS;
              QPointDePassage := 1000;
              if (NbScraps > 0) then
              begin
                for ii:=0 to NbScraps - 1 do
                begin
                  FDrwCurrentIdxObj := ii;
                  if (MyGroupe.Visible) then TmpBuffer.DessinerScrap(FDocumentDessin.GetScrap(ii), ii, False, MyGroupe.IDGroupeEntites);
                end;
              end;
              FDrwCurrentMode := msePOLYGONES;
              QPointDePassage := 2000;
              if ((NbPolygones > 0) and (FEtendueXY < SEUIL_VISIBILITE_POLYGONES)) then
              begin
                for ii:=0 to NbPolygones - 1 do
                begin
                  FDrwCurrentIdxObj := ii;
                  if (MyGroupe.Visible) then TmpBuffer.DessinerPolygone(FDocumentDessin.GetPolygone(ii), ii, False, MyGroupe.IDGroupeEntites);
                end;
              end;
              // désactivation de la texture: les objets suivants n'ont pas de texture
              TmpBuffer.CanvasBGRA.Brush.Texture := nil;
              FDrwCurrentMode := mseCOURBES;
              QPointDePassage := 3000;
              if ((NbCourbes > 0) and (FEtendueXY < SEUIL_VISIBILITE_COURBES_POLYLIGNES)) then
              begin
                for ii:=0 to NbCourbes - 1 do
                begin
                  FDrwCurrentIdxObj := ii;
                  if (MyGroupe.Visible) then TmpBuffer.DessinerCourbe(FDocumentDessin.GetCourbe(ii), ii, MyGroupe.IDGroupeEntites);
                end;
              end;
              FDrwCurrentMode := msePOLYLIGNES;
              QPointDePassage := 4000;
              if ((NbPolyLines > 0) and (FEtendueXY < SEUIL_VISIBILITE_COURBES_POLYLIGNES))then
              begin
                for ii:=0 to NbPolyLines - 1 do
                begin
                  FDrwCurrentIdxObj := ii;
                  if (MyGroupe.Visible) then TmpBuffer.DessinerPolyLigne(FDocumentDessin.GetPolyligne(ii), ii, MyGroupe.IDGroupeEntites);
                end;
              end;
              FDrwCurrentMode := mseLIGNES;
              QPointDePassage := 5000;
              if ((NbSimpleLignes > 0) and (FEtendueXY < SEUIL_VISIBILITE_SIMPLES_LIGNES)) then
              begin
                for ii := 0 to NbSimpleLignes - 1 do
                begin
                  FDrwCurrentIdxObj := ii;
                  if (MyGroupe.Visible) then TmpBuffer.DessinerSimpleLigne(FDocumentDessin.GetSimpleLigne(ii), false, MyGroupe.IDGroupeEntites);
                end;
              end;
              QPointDePassage := 9999; // le programme passe ici !
            except // fin de la zone de sécurisation indispensable !!!
              Result := -QPointDePassage;
            end;
          end;
          // Symboles et textes: Pas la peine de tester les BBX (ces objets sont relativement rares)
          FDrwCurrentMode := mseSYMBOLES;
          if ((NbSymboles > 0) and (FEtendueXY < SEUIL_VISIBILITE_SYMBOLES)) then   // Symboles OK
          begin
            for ii := 0 to NbSymboles - 1 do
            begin
              FDrwCurrentIdxObj := ii;
              if (MyGroupe.Visible) then TmpBuffer.DessinerSymbole(FDocumentDessin.GetSymbole(ii), MyGroupe.IDGroupeEntites);
            end;
          end;
          FDrwCurrentMode := mseTEXTES;
          if ((tedTEXTES in FParamsVue2D.ElementsDrawn) and (NbTextes > 0) and (FEtendueXY < SEUIL_VISIBILITE_TEXTES)) then
          begin
            for ii := 0 to NbTextes - 1 do
            begin
              FDrwCurrentIdxObj := ii;
              if (MyGroupe.Visible) then TmpBuffer.DessinerTexte(FDocumentDessin.GetTexte(ii), False, MyGroupe.IDGroupeEntites);
            end;
          end;
        end; // for NoGroupe ...
        //************************
        if (tedQUADRILLES  in FParamsVue2D.ElementsDrawn) then TmpBuffer.DrawQuadrillage();
        if (tedCENTERLINES in FParamsVue2D.ElementsDrawn) then
        begin
          for ii:= 0 to FDocumentDessin.GetNbBaseStations() - 1 do TmpBuffer.DessinerBaseStation(FDocumentDessin.GetBaseStation(ii), HAUTEUR_LBL_BASESTATION_IN_PIXELS);
        end;
        DessinerCurrentBaseStation();            // dessiner station courante
        DrawLastStation(FLastStationFound);      // dessiner dernière station trouvée (peut être différente de la station courante)
        if (tedECHELLE_NORD in FParamsVue2D.ElementsDrawn) then TmpBuffer.DrawEchelleNord(Vue.Width, Vue.Height);
        TmpBuffer.EndDrawing()                   // fin conventionnelle
      except
        EWE := ChooseString(Ord(FDrwCurrentMode), [
                              'mseNONE',
                              'mseSCRAPS',
                              'mseCOURBES',
                              'msePOLYLIGNES',
                              'msePOLYGONES',
                              'mseLIGNES',
                              'mseSYMBOLES',
                              'mseTEXTES',
                              'mseIMAGES' ]);
        AfficherMessageErreur(Format('Mode: %s - Objet: %d', [EWE, FDrwCurrentIdxObj]));
      end;
    end;
    TmpBuffer.Draw(Vue.Canvas, 0, 0, True);
    if (DoGenererImage) then TmpBuffer.SaveToFile(FFichierImage);
    Result := QPointDePassage;
    AfficherMessageErreur(Format('%s.Redessin: %d', [ClassName, QPointDePassage]));
  finally
    FreeAndNil(TmpBuffer);//TmpBuffer.Free;
    FRedessinInProcess := False; // libération du sémaphore
    SetMsgInLbMessage(errNONE, GetResourceString(rsMSG_READY));
    // ne pas mettre cette ligne avant la libération du sémaphore
    Application.ProcessMessages;
    t := now() - t;
  end;
  try  // on dessine ici les objets temporaires
    DrawTmpObj_FreeHandPoints();   // dessin des points main levée
    DrawTmpObj_CourbeProvisoire();
  except
  end;
end;

//******************************************************************************

