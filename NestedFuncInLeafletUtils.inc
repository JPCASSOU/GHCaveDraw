// NestedFuncInLeafletUtils.inc:
// Contient les fonctions encapsulées dans les fonctions suivantes:
// -- TDocumentDessin.GenererCarteLeaflet()
const
  IDX_LAYER_OSM_MAPNIK    = 0;
  IDX_LAYER_OSM_TOPO      = 1;
  IDX_LAYER_GOOGLE_SAT    = 2;
  IDX_LAYER_ENTRANCES     = 3; VAR_LAYER_ENTRANCES     = 'LayerEntrances';
  IDX_LAYER_SCRAPS        = 4; VAR_LAYER_SCRAPS        = 'LayerScraps';
  IDX_LAYER_CENTERLINES   = 5; VAR_LAYER_CENTERLINES   = 'LayerCenterlines';
  IDX_LAYER_POI           = 6; VAR_LAYER_POI           = 'LayerPOIs';
  IDX_LAYER_STATIONS      = 7; VAR_LAYER_PointsTopo    = 'LayerPointsTopo';
var
 MyLeafletExport: TLeafletExport;
 Coin1, Coin2 , Centroide: TPoint3Df;
 QCentroideLonLat: TProjUV;
 DoExportSuperGroupes: boolean;
 //MyScrap        : TScrap;
 //MySymbole      : TSymbole;
 //MyBaseStation  : TBaseStation;

 function MiouMiou(const QX, QY: double): TProjUV;
 var
   PT: TProjUV;
 begin
   PT.setFrom(QX, QY);
   Result := FConversionSysteme.ConversionSyst1ToSyst2EPSG(FCodeEPSG, CODE_EPSG_WGS84, PT);
 end;
 procedure QDessinerScrap(const IdxScrap: int64;
                          const QScrap: TScrap;
                          const UseDefaultColor: boolean;
                          const TagString: string;
                          const TagNum: Int64);
 var
   ii, Nb: Integer;
   V: TVertexPolygon;
   BS: TBaseStation;
   P2: TProjUV;
   MyGroupe: TGroupeEntites;
   QAT: String;
 begin
   if (gisWITH_METADATA in WithExportGIS) then MyLeafletExport.WrtLinFmt(' // Scrap%d - %s', [IdxScrap, QScrap.Nom]);
   //MyLeafletExport.DefineStylePoly(0.05, MyScrap.Couleur, MyScrap.Couleur, 255, MyScrap.Opacite);
   if (UseDefaultColor) then MyLeafletExport.DefineStylePoly(1.00, DefaultColor, DefaultColor, 255, 255);

   MyGroupe := GetGroupeByIDGroupe(QScrap.IDGroupe);
   QAT := IIF (gisWITH_METADATA in WithExportGIS, MyGroupe.NomGroupe, 'Metadata unavailable');
   if (DoFillScraps) then MyLeafletExport.BeginPolygon(QAT, '')
                     else MyLeafletExport.BeginPolyLine(QAT, '');

   Nb := Length(QScrap.Sommets); // - 1;
   for ii := 0 to Nb - 1 do
   begin
     V := QScrap.getVertex(ii);
     if (GetBasePointByIndex(V.IDStation, BS)) then
     begin
       P2 := MiouMiou(BS.PosStation.X + V.Offset.X, BS.PosStation.Y + V.Offset.Y);
       MyLeafletExport.AddVertex(P2.U, P2.V, BS.PosStation.Z, ii < (Nb-1));
     end;
   end;
   if (DoFillScraps) then MyLeafletExport.EndPolygon()
                     else MyLeafletExport.EndPolyLine();
 end;
 procedure QDessinerSymbole(const IdxSymbole: int64; const QSymbole: TSymbole);
 var
   BS: TBaseStation;
   P2: TProjUV;
   EWE: string;
 begin
   if (GetBasePointByIndex(QSymbole.IDBaseStation, BS)) then
   begin
     P2 := MiouMiou(BS.PosStation.X + QSymbole.Offset.X,
                    BS.PosStation.Y + QSymbole.Offset.Y);
   {$IFDEF TIDBASEPOINT_AS_TEXT}
   EWE := BS.IDStation;
   {$ELSE}
   EWE := BS.getToporobotIDStationAsString();
   {$ENDIF TIDBASEPOINT_AS_TEXT}
     MyLeafletExport.AddPoint(P2.U, P2.V, BS.PosStation.Z, EWE, QSymbole.TagTexte);
   end;
 end;
 procedure QDessinerBaseStation(const BS: TBaseStation);
 var
   P2: TProjUV;
   EWE: string;
 begin
   P2 := MiouMiou(BS.PosStation.X, BS.PosStation.Y);
   {$IFDEF TIDBASEPOINT_AS_TEXT}
   EWE := BS.IDStation;
   {$ELSE}
   EWE := BS.GetToporobotIDStationAsString();
   {$ENDIF TIDBASEPOINT_AS_TEXT}
   MyLeafletExport.DrawPoint(P2.U, P2.V, BS.PosStation.Z,
                             0.2,
                             EWE,
                             Format('X = %.0f, Y = %.0f, Z = %.2f', [BS.PosStation.X, BS.PosStation.Y, BS.PosStation.Z])
                             );

 end;
 procedure QDessinerViseeCenterline(const BS: TBaseStation);
 var
   P0, P1: TProjUV;
 begin
   P0 := MiouMiou(BS.PosExtr0.X, BS.PosExtr0.Y);
   P1 := MiouMiou(BS.PosStation.X, BS.PosStation.Y);
   MyLeafletExport.DrawSegment(P0.U, P0.V, P1.U, P1.V);
 end;
//*********************************************************************************
procedure QCreateLayers(const WithSuperGroupes: boolean);
var
  i, Nb: Integer;
  MySuperGroupe: TSuperGroupe;
  VAR_LAYER_SuperGroupe: String;
begin
  MyLeafletExport.AddLayer(VAR_LAYER_ENTRANCES  , 'Entrées');
  MyLeafletExport.AddLayer(VAR_LAYER_SCRAPS     , 'Scraps');
  MyLeafletExport.AddLayer(VAR_LAYER_CENTERLINES, 'Centerlines');
  MyLeafletExport.AddLayer(VAR_LAYER_POI        , 'Points d''intérêt');
  MyLeafletExport.AddLayer(VAR_LAYER_PointsTopo , 'Stations topo');
  Nb := GetNbSuperGroupes();
  if (WithSuperGroupes AND (Nb > 0)) then
  begin
    for i := 0 to GetNbSuperGroupes() - 1 do
    begin
       MySuperGroupe := self.GetSuperGroupe(i);
       VAR_LAYER_SuperGroupe := Format('LayerSuperGroupe%d', [i]);
       MyLeafletExport.AddLayer(VAR_LAYER_SuperGroupe ,MySuperGroupe.NomSuperGroupe);
    end;
  end;
end;

procedure QExporterAllScraps();
var
  i, NbScraps: integer;
  MyScrap: TScrap;
begin
  NbScraps := GetNbScraps();
  MyLeafletExport.WriteLine(' // -----------------------');
  MyLeafletExport.WrtLinFmt(' // All the %d scraps', [NbScraps]);
  MyLeafletExport.setCurrentLayer(IDX_LAYER_SCRAPS);
  if (NbScraps = 0) then exit;
  for i := 0 to NbScraps - 1 do
  begin
    MyScrap := GetScrap(i);
    QDessinerScrap(i, MyScrap, True, '', 0);
    if ((0 = (i MOD 100)) AND assigned(FProcAfficherProgression)) then FProcAfficherProgression(Format('Scrap %d / %d: %s', [i, NbScraps, MyScrap.Nom]), 0, NbScraps - 1, i);
  end;
end;
procedure QExporterEntrances();
var
  i, NbSymboles: integer;
  MySymbole: TSymbole;
  VarCond: String;
begin
  // entrées
  MyLeafletExport.setCurrentLayer(IDX_LAYER_ENTRANCES);
  NbSymboles := GetNbSymboles();
  if (NbSymboles > 0) then // and (gisWITH_ENTRANCES in WithExportGIS)) then
  begin
    VarCond := MyLeafletExport.getLeafletVarNameDispEntrances();
    MyLeafletExport.BeginConditionalSection(True, VarCond);
      for i := 0 to NbSymboles - 1 do
      begin
        MySymbole := GetSymbole(i);
        if (nosGROTTE_SURFACE  = mySymbole.TypeObject) then QDessinerSymbole(i, MySymbole);
        if (nosGOUFFRE_SURFACE = mySymbole.TypeObject) then QDessinerSymbole(i, MySymbole);
      end;
    MyLeafletExport.EndConditionalSection(VarCond);
  end;
end;
procedure QExporterPOIs();
var
  i, NbSymboles: integer;
  MySymbole: TSymbole;
  VarCond: String;
begin
  MyLeafletExport.setCurrentLayer(IDX_LAYER_POI);
  NbSymboles := GetNbSymboles();
  if (NbSymboles > 0) then // and (gisWITH_POI in WithExportGIS)) then
  begin
    VarCond := MyLeafletExport.getLeafletVarNameDispSymbols();
    MyLeafletExport.BeginConditionalSection(True, VarCond);
      for i := 0 to NbSymboles - 1 do
      begin
        MySymbole := GetSymbole(i);
        if (nosPOINT_REMARQUABLE = mySymbole.TypeObject) then QDessinerSymbole(i, MySymbole);
      end;
    MyLeafletExport.EndConditionalSection(VarCond);
  end;
end;
procedure QExporterCenterlines();
var
  VarCond: String;
  MyBaseStation: TBaseStation;
  i: Integer;
begin
  MyLeafletExport.setCurrentLayer(IDX_LAYER_CENTERLINES);
  MyLeafletExport.DefineStylePoly(0.0, clRed, clRed, 255, 255);
  VarCond := MyLeafletExport.getLeafletVarNameDispCenterlines();
  MyLeafletExport.BeginConditionalSection(True, VarCond);
    for i := 0 to GetNbBaseStations() - 1 do
    begin
      MyBaseStation:= GetBaseStation(i);
      if (MyBaseStation.IDStation{$IFDEF TIDBASEPOINT_AS_TEXT} <> '') {$ELSE} > 0 {$ENDIF}) then
      QDessinerViseeCenterline(MyBaseStation);
    end;
  MyLeafletExport.EndConditionalSection(VarCond);
end;
procedure QExporterPointsTopo();
var
  i: Integer;
  MyBaseStation: TBaseStation;
  VarCond: String;
begin
  MyLeafletExport.setCurrentLayer(IDX_LAYER_STATIONS);
  MyLeafletExport.DefineStylePoly(0.0, clRed, clRed, 255, 255);
  VarCond := MyLeafletExport.getLeafletVarNameDispStations();
  MyLeafletExport.BeginConditionalSection(True, VarCond);
    for i := 0 to GetNbBaseStations() - 1 do
    begin
      MyBaseStation:= GetBaseStation(i);
      if (MyBaseStation.IDStation{$IFDEF TIDBASEPOINT_AS_TEXT} <> '') {$ELSE} > 0 {$ENDIF}) then
      QDessinerBaseStation(MyBaseStation);
    end;
  MyLeafletExport.EndConditionalSection(VarCond);
end;
//******************************************************************************
procedure QExporterScrapsBySupergroupes();
  var
    IDX_LAYER_SuperGroupe: Integer;
    idxSuperGroupe, NbSuperGroupes: integer;
    NbGrpsInSuperGroupe, j: integer;
    NbScraps, s: integer;
    MySuperGroupe: TSuperGroupe;
    MyScrap: TScrap;
    MyGroupe: TGroupeEntites;
  begin
    NbScraps := GetNbScraps();
    if (NbScraps = 0) then exit;
    NbSuperGroupes := GetNbSuperGroupes();
    MyLeafletExport.WriteLine(' // -----------------------');
    MyLeafletExport.WriteLine('');
    MyLeafletExport.WrtLinFmt(' // Scraps by the %d supergroupes', [NbSuperGroupes]);
    if (NbSuperGroupes = 0) then exit;
    for idxSuperGroupe := 0 to NbSuperGroupes - 1 do
    begin
      IDX_LAYER_SuperGroupe := IDX_LAYER_STATIONS + 1 + idxSuperGroupe;
      MySuperGroupe := self.GetSuperGroupe(idxSuperGroupe);
      NbGrpsInSuperGroupe := MySuperGroupe.ListeGroupes.GetNbElements();
      MyLeafletExport.setCurrentLayer(IDX_LAYER_SuperGroupe);
      MyLeafletExport.WrtLinFmt(' // -> scraps of supergroupe: %d - %s (%d groupes)', [idxSuperGroupe, MySuperGroupe.NomSuperGroupe, NbGrpsInSuperGroupe]);
      MyLeafletExport.WriteLine(' // -----------------------');
      MyLeafletExport.DefineStylePoly(0.00, MySuperGroupe.Couleur, MySuperGroupe.Couleur, 255, 128);
      if (NbGrpsInSuperGroupe > 0) then
      begin
        for j := 0 to NbGrpsInSuperGroupe - 1 do
        begin;
          MyGroupe := GetGroupeByIDGroupe(MySuperGroupe.ListeGroupes.GetElement(j));
          MyLeafletExport.WrtLinFmt(' //   scraps of groupe: %d - %s', [MyGroupe.IDGroupeEntites, MyGroupe.NomGroupe]);
          MyLeafletExport.WriteLine('');
          for s := 0 to NbScraps - 1 do
          begin
            MyScrap := GetScrap(s);
            if (MyGroupe.IDGroupeEntites = MyScrap.IDGroupe) then QDessinerScrap(s, MyScrap, False, '', 0);
          end;
        end;
      end;
      MyLeafletExport.WriteLine('');
    end;
    MyLeafletExport.WriteLine(' // end all scraps of supergroupes');
  end;
