unit cdrSectionTransversale;

{$INCLUDE CompilationParameters.inc}

interface

uses
  GHCD_Types,
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GeneralFunctions,
  Math,
  unitSectionsTransversales,
  unitCrossSectionDrwCtxt,
  UnitGraphismesProvisoires,
  CallDialogsGHCaveDraw,
  Graphics, Classes, SysUtils
    , BGRABitmap
    , BGRABitmapTypes
    , BGRACanvas
    , BGRAPhongTypes
    , BGRAGradients
    {$IFDEF MSWINDOWS}
    , BGRAWinBitmap
    {$ENDIF}
  , UnitTGHCaveDrawDrawingContext
  , FileUtil, curredit //, BGRAVirtualScreen
  , Forms, Controls, ExtCtrls, StdCtrls, ActnList, Buttons, Menus, Dialogs;


type TCrossSectionModeTravail = (mtNONE,
                           mtDRAW_COURBE,   // OK
                           mtDRAW_POLYGONE, // OK
                           mtDRAW_LINE,
                           mtDRAW_TEXTE,
                           mtSELECT_LIGNE,
                           mtSELECT_COURBE,   // OK
                           mtSELECT_POLYGONE, // OK
                           mtSELECT_TEXTE,
                           // dessin de lignes
                           mtLIGNE_PREMIER_POINT,
                           mtLIGNE_SECOND_POINT,
                           mtPICK_POS_TEXTE,
                           // déplacement de sommets
                           mtCOURBE_EDIT_SOMMET,
                           mtCOURBE_MOVE_SOMMET,
                           mtPOLYGON_EDIT_SOMMET,
                           mtPOLYGON_MOVE_SOMMET
                           );
type TCrossSectionModeSelection = (mseNONE, mseCOURBE, msePOLYGONE, mseLIGNE, mseTEXTE);
type TCrossSectionTypeListeObjets = (csloNONE, csloCOURBES, csloPOLYGONES, csloLIGNES, csloTEXTES);
type

  { TCadreSectionTransversale }

  TCadreSectionTransversale = class(TFrame)
    acCourbeChenalDeVoute: TAction;
    acCourbeEcoulements: TAction;
    acCourbeLignesPentes: TAction;
    acCourbeMiniRessaut: TAction;
    acCourbeNone: TAction;
    acCourbeParois: TAction;
    acCourbeParoisCachees: TAction;
    acCourbeRessauts: TAction;
    acCourbeSurplombs: TAction;
    acDeleteObject: TAction;
    acEditCourbe: TAction;
    acEditLigne: TAction;
    acEditObjet: TAction;
    acEditPolygone: TAction;
    acEditPolylign: TAction;
    acEditScrap: TAction;
    acEditSymbole: TAction;
    acEditTexte: TAction;
    acLigneFleche: TAction;
    acLigneNone: TAction;
    acMoveObject: TAction;
    acPanLeft: TAction;
    acPanRight: TAction;
    acPanUp: TAction;
    acPanDown: TAction;
    acNewCourbe: TAction;
    acPolygoneArgile: TAction;
    acPolygoneGrosBloc: TAction;
    acPolygoneLac: TAction;
    acPolygoneNone: TAction;
    acPolygoneSable: TAction;
    acReset: TAction;
    acNewPolygone: TAction;
    acSetModeTravailNone: TAction;
    acNewLigne: TAction;
    acNewTexte: TAction;
    acSelectCourbe: TAction;
    acSelectPolygone: TAction;
    acSelectSimpleLigne: TAction;
    acSelectTexte: TAction;
    acDeleteCourbe: TAction;
    acDeletePolygone: TAction;
    acDeleteTexte: TAction;
    acDeleteSimpleLigne: TAction;
    acNewNone: TAction;
    acTexteCotation: TAction;

    acCourbeArete: TAction;
    acPolygoneEboulis: TAction;
    acPolygoneArgileGalets: TAction;
    acPolygoneGalets: TAction;
    acPolygoneNeve: TAction;
    acPolygoneEau: TAction;
    acLigneFaille: TAction;
    acLignePendage: TAction;
    acTexteOrdinaire: TAction;
    acCourbeParoiIncertaine: TAction;
    acLigneDiverse: TAction;
    acPolygoneMasque: TAction;
    acCourbeMurMaconne: TAction;
    acCourbeParoiFracassee: TAction;
    acScrapEnglobant: TAction;
    acMoveUp: TAction;
    acMoveDown: TAction;
    acClippingByScrap: TAction;
    Action2: TAction;
    acValidateCourbePolygone: TAction;
    acZoomMoins: TAction;
    acZoomPlus: TAction;
    acZoomTout: TAction;
    AcnLstCadreCrossSections: TActionList;
    btnApplyModifsTexte: TButton;
    btnDeleteObject: TSpeedButton;
    btnDeleteObject1: TSpeedButton;
    btnScrapEnglobant: TSpeedButton;
    btnDeleteObject2: TSpeedButton;
    btnDeleteObject3: TSpeedButton;
    btnDeleteObject4: TSpeedButton;
    btnDeleteObject5: TSpeedButton;
    btnDrwObjMode1: TSpeedButton;
    btnDrwObjMode10: TSpeedButton;
    btnDrwObjMode11: TSpeedButton;
    btnDrwObjMode12: TSpeedButton;
    btnDrwObjMode2: TSpeedButton;
    btnDrwObjMode3: TSpeedButton;
    btnDrwObjMode4: TSpeedButton;
    btnDrwObjMode5: TSpeedButton;
    btnDrwObjMode6: TSpeedButton;
    btnDrwObjMode7: TSpeedButton;
    btnDrwObjMode8: TSpeedButton;
    btnDrwObjMode9: TSpeedButton;
    cmbStyleTexte: TComboBox;
    editPosX: TCurrencyEdit;
    editPosY: TCurrencyEdit;
    editTexte: TEdit;
    ImageList1: TImageList;
    lbIDTexte: TStaticText;
    lbModeTravail: TStaticText;
    lbMousePos: TStaticText;
    lbMessages: TStaticText;
    lsbObjects: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlAlignTexte: TPanel;
    pnlAttributsTexte: TPanel;
    pnlTypesObjets: TPanel;
    popupGeneral: TPopupMenu;
    popupCourbePoly: TPopupMenu;
    rbAli1: TRadioButton;
    rbAli2: TRadioButton;
    rbAli3: TRadioButton;
    rbAli4: TRadioButton;
    rbAli5: TRadioButton;
    rbAli6: TRadioButton;
    rbAli7: TRadioButton;
    rbAli8: TRadioButton;
    rbAli9: TRadioButton;
    sclAngleOrientation: TScrollBar;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    lbNatureObjets: TStaticText;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Vue: TPaintBox;
    pnlVue: TPanel;
    procedure acClippingByScrapExecute(Sender: TObject);
    procedure acCourbeAreteExecute(Sender: TObject);
    procedure acNewCourbeExecute(Sender: TObject);
    procedure acCourbeParoisExecute(Sender: TObject);
    procedure acCourbeParoiIncertaineExecute(Sender: TObject);
    procedure acDeleteCourbeExecute(Sender: TObject);
    procedure acDeletePolygoneExecute(Sender: TObject);
    procedure acDeleteSimpleLigneExecute(Sender: TObject);
    procedure acDeleteTexteExecute(Sender: TObject);
    procedure acLigneDiverseExecute(Sender: TObject);
    procedure acLigneFailleExecute(Sender: TObject);
    procedure acLignePendageExecute(Sender: TObject);
    procedure acNewNoneExecute(Sender: TObject);
    procedure acPanDownExecute(Sender: TObject);
    procedure acPanLeftExecute(Sender: TObject);
    procedure acPanRightExecute(Sender: TObject);
    procedure acPanUpExecute(Sender: TObject);
    procedure acPolygoneArgileExecute(Sender: TObject);
    procedure acPolygoneArgileGaletsExecute(Sender: TObject);
    procedure acPolygoneEauExecute(Sender: TObject);
    procedure acPolygoneEboulisExecute(Sender: TObject);
    procedure acNewPolygoneExecute(Sender: TObject);
    procedure acPolygoneGaletsExecute(Sender: TObject);
    procedure acPolygoneGrosBlocExecute(Sender: TObject);
    procedure acPolygoneMasqueExecute(Sender: TObject);
    procedure acPolygoneNeveExecute(Sender: TObject);
    procedure acPolygoneSableExecute(Sender: TObject);
    procedure acResetExecute(Sender: TObject);
    procedure acScrapEnglobantExecute(Sender: TObject);
    procedure acSelectCourbeExecute(Sender: TObject);
    procedure acSelectPolygoneExecute(Sender: TObject);
    procedure acSelectSimpleLigneExecute(Sender: TObject);
    procedure acSelectTexteExecute(Sender: TObject);
    procedure acNewLigneExecute(Sender: TObject);
    procedure acTexteCotationExecute(Sender: TObject);
    procedure acNewTexteExecute(Sender: TObject);
    procedure acTexteOrdinaireExecute(Sender: TObject);

    procedure acValidateCourbePolygoneExecute(Sender: TObject);
    procedure acZoomMoinsExecute(Sender: TObject);
    procedure acZoomPlusExecute(Sender: TObject);
    procedure acZoomToutExecute(Sender: TObject);
    procedure btnApplyModifsTexteClick(Sender: TObject);
    procedure btnDetourerClick(Sender: TObject);
    procedure lsbObjectsClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure sclAngleOrientationChange(Sender: TObject);
    procedure VueClick(Sender: TObject);
    procedure VueDblClick(Sender: TObject);
    procedure VueMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VuePaint(Sender: TObject);
  private
    { private declarations }
    FModeTravail: TCrossSectionModeTravail;
    FCrossSectionModeSelection: TCrossSectionModeSelection;
    FHerissonViseesRadiantes: THerissonViseesRadiantes;
    FMyCrossSection         : TCrossSection;
    FCrossSectionPolyProvisoire: TCrossSectionPolyProvisoire;
    FRedessinInProcess  : boolean;
    FDoDraw             : boolean;
    FBackGroundColor    : TColor;
    FScrapEnglobantColor: TColor;
    FAxisColor          : TColor;
     // limites du dessin
    FRXMini,
    FRXMaxi,
    FRYMini,
    FRYMaxi: double;
    FRappHLVue          : double;
    FRappScrReal        : double;
    FInvRappScrReal     : double;
    FMyPos              : TPoint2Df;
    FZC1, FZC2          : TPoint2Df;

    FNoVertexPolygoneProvisoire  : integer;
    FNoVertexCourbeProvisoire    : integer;
    // Drapeaux
    FVertexModified              : boolean;  // Drapeau Sommet de polygone modifié
    FAttenduSecondPoint          : boolean;  // Drapeau Attend un deuxmième point
    FUnObjetEstSelectionne       : boolean;  // Drapeau Un objet est sélectionné
    FDoAddVertexCourbePolygone   : boolean;  // Drapeau Ajouter un vertex ?
    FDoMovePoint                 : Boolean;
    FMovingPoint                 : Boolean;
    // Objets en cours d'édition
    FCurrentSimpleLigne          : TCrossSectionSimpleLigne;

    FCurrentCurveIdx             : integer;
    FCurrentPolygonIdx           : integer;
    FCurrentSimpleLigneIdx       : integer;
    FCurrentTexteIdx             : integer;


    // Textures. On les configure ici et on les passe en paramètre à un TCrossSectionDrawingContext
    // pour éviter des rechargements répétés
    FTexturesPolygones           : TTexturesPolygonObject;
    FTexturesAreOK               : boolean;

    // index de natures d'objets
    FCurrentNatureCourbe         : TNatureObjetCourbe;
    FCurrentNaturePolygone       : TNatureObjetPolygone;
    FCurrentNatureLigne          : TNatureObjetLigne;
    FCurrentNatureTexte          : TNatureObjetTexte;


    procedure AffecterActionAtButton(const WU: TSpeedButton; const QAction: TAction);
    procedure AfficherModeTravail(const MT: TCrossSectionModeTravail);
    procedure DessinerCourbeProvisoire(const Cnv: TCanvas);
    procedure DisplayAttributsCourbe(const QIdx: integer; const QVisible: boolean);
    procedure DisplayAttributsPolygone(const QIdx: integer; const QVisible: boolean);
    procedure DisplayAttributsSimpleLigne(const QIdx: integer; const QVisible: boolean);
    procedure DisplayAttributsTexte(const QIdx: integer; const QVisible: boolean);
    procedure DrawCourbe(const QBmp: TCrossSectionDrawingContext; const MyCourbe: TCrossSectionCourbe; const DoDrawSommets: boolean);
    procedure DrawPolygone(const QBmp: TCrossSectionDrawingContext; const MyPolygone: TCrossSectionPolygone; const DoDrawSommets: boolean; const DoUseStyles: boolean = true);
    procedure DrawSimpleLigne(const QBmp: TCrossSectionDrawingContext; const MySimpleLigne: TCrossSectionSimpleLigne; const DoDrawSommets: boolean);
    procedure DrawTexte(const QBmp: TCrossSectionDrawingContext; const MyTexte: TCrossSectionTexte; const DoDrawSommets: boolean);


    function GetAttribsTexteFromForm(): TCrossSectionTexte;
    function GetCoordsMonde(const PP: TPoint): TPoint2Df;
    function GetCoordsPlan(const PM: TPoint2Df): TPoint; overload;
    function GetCoordsPlan(const QX, QY: double): TPoint; overload;
    function GetRYMaxi(): double;
    procedure PreparerSubButtons(const MT: TCrossSectionModeTravail);
    procedure VolatileTraceVers(const Cnv: TCanvas; const XX, YY: Double; const Drawn: boolean);

    function QRechercheEtTraceCourbe(out QIdx: integer): boolean;

    procedure QAjouterUneCourbe();          // ajout d'une entité courbe
    procedure QAjouterUnPolygone();         // ajout d'une entité polygone

    procedure ListerLesObjets(const M: TCrossSectionTypeListeObjets; const QIdx: integer);

  public
    { public declarations }
    procedure SetModeTravail(const MT: TCrossSectionModeTravail; const QType: byte);
    function  CdrDrwInitialiser(const FS: THerissonViseesRadiantes): boolean;
    procedure SetSectionTransversale(const ST: TCrossSection);

    function  CdrDrwFinalize(): boolean;
    procedure SetViewLimits(const X1, Y1, X2, Y2: double);
    procedure Redessin();
    procedure DrawSectionTransversale(const QBmp: TCrossSectionDrawingContext);
    // pour tests
    function GetHerisson(): THerissonViseesRadiantes;
  end;

implementation
const M =  0.5;
{$R *.lfm}

{ TCadreSectionTransversale }

function TCadreSectionTransversale.CdrDrwInitialiser(const FS: THerissonViseesRadiantes): boolean;
var
  InitialDir: String;
  procedure ParametrerBitmap(var BT: TBGRABitmap; const FichierBMP: string);
  begin
    BT.LoadFromFile(InitialDir + FichierBMP);
    BT.ApplyGlobalOpacity(128);
  end;
  procedure S666(const Acn: TAction; const S: string);
  begin
    ACN.Caption := GetResourceString(S);
    ACN.Hint    := Acn.Caption;
  end;
begin
  result := false;
  AfficherMessage(Format('%s.CdrDrwInitialize()', [ClassName]));
  FMyCrossSection := nil;
  // double tampon
  //self.DoubleBuffered := true;
  self.pnlVue.DoubleBuffered := true;
  // captions
  S666(acZoomTout   , rsCDR_DRW_ACN_VUE_ZOOM_ALL);
  S666(acZoomPlus   , rsCDR_DRW_ACN_VUE_ZOOM_PLUS);
  S666(acZoomMoins  , rsCDR_DRW_ACN_VUE_ZOOM_MOINS);
  S666(acPanLeft    , rsCDR_DRW_ACN_VUE_PAN_LEFT);
  S666(acPanRight   , rsCDR_DRW_ACN_VUE_PAN_RIGHT);
  S666(acPanUp      , rsCDR_DRW_ACN_VUE_PAN_UP);
  S666(acPanDown    , rsCDR_DRW_ACN_VUE_PAN_DOWN);
  // actions de boutons
  S666(acReset      , rsBTN_DRW_HINT_SETMODE_0);
  //S666(acEditObjet               , rsBTN_DRW_HINT_EDIT_OBJ);
  //S666(acDeleteObject            , rsBTN_DRW_HINT_DELETE_OBJ);

  //S666(acNewCourbe     ,



  // action de création d'objets

  S666(acNewCourbe               , rsBTN_DRW_HINT_NEW_COURBE);
  S666(acNewLigne                , rsBTN_DRW_HINT_NEW_LIGNE);
  S666(acNewPolygone             , rsBTN_DRW_HINT_NEW_POLYGON);
  S666(acNewTexte                , rsBTN_DRW_HINT_NEW_TEXTE);
  //*)
  AfficherMessage('A004');
  // actions pour types d'objets
  // -- courbes
  (*
  S666(acCourbeNone              , rsBTN_COURBE_NONE);
  S666(acCourbeParois            , rsBTN_COURBE_PAROI);
  S666(acCourbeParoisCachees     , rsBTN_COURBE_PAROI_CACHEE);
  S666(acCourbeEcoulements       , rsBTN_COURBE_ECOULEMENT);
  S666(acCourbeLignesPentes      , rsBTN_COURBE_LIGNES_PENTE);
  S666(acCourbeRessauts          , rsBTN_COURBE_RESSAUT);
  S666(acCourbeSurplombs         , rsBTN_COURBE_SURPLOMB);
  S666(acCourbeChenalDeVoute     , rsBTN_COURBE_CHENAL);
  S666(acCourbeMiniRessaut       , rsBTN_COURBE_MINI_RESSAUT);
  S666(acCourbeParoiIncertaine   , rsBTN_COURBE_PAROI_INCERTAINE);
  S666(acCourbeMurMaconne        , rsBTN_COURBE_MUR_MACONNE);
  S666(acCourbeParoiFracassee    , rsBTN_COURBE_PAROI_FRACASSEE);
  //*)
  // -- lignes
  S666(acLignePendage            , rsBTN_LIGNE_PENTE);
  S666(acLigneFaille             , rsBTN_LIGNE_FRACTURES);
  //*)
  //S777(acLigneSuiteReseau        , rsBTN_LIGNE_SUITE);
  //S777(acLignePentes             , rsBTN_LIGNE_PENTE);
  // -- polygones

  //S666(acPolygoneNone            , rsBTN_POLYGONE_NONE);
  S666(acPolygoneEau             , rsBTN_POLYGONE_LAC);
  S666(acPolygoneArgile          , rsBTN_POLYGONE_ARGILE);
  S666(acPolygoneSable           , rsBTN_POLYGONE_SABLE);
  S666(acPolygoneEboulis         , rsBTN_POLYGONE_BLOCS);
  S666(acPolygoneGalets          , rsBTN_POLYGONE_GALETS);
  S666(acPolygoneNeve            , rsBTN_POLYGONE_NEIGE);
  //S777(acPolygoneSilhouettes     , rsBTN_POLYGONE_SILHOUETTES);
  S666(acPolygoneGrosBloc        , rsBTN_POLYGONE_GROS_BLOC);
  //S666(acPolygoneSiphon          , rsBTN_POLYGONE_SIPHON);
  S666(acPolygoneArgileGalets    , rsBTN_POLYGONE_VARVES);
  //S777(acPolygoneChemins         , rsBTN_POLYGONE_CHEMINS);
  //*)

  // Drapeaux
  FVertexModified              := False;  // Drapeau Sommet de polygone modifié
  FAttenduSecondPoint          := False;  // Drapeau Attend un deuxmième point
  FUnObjetEstSelectionne       := False;  // Drapeau Un objet est sélectionné
  FDoAddVertexCourbePolygone   := False;  // Drapeau Ajouter un vertex ?
  FDoMovePoint                 := false;
  FMovingPoint                 := false;
  FNoVertexCourbeProvisoire    := -1;
  FNoVertexPolygoneProvisoire  := -1;

  FCurrentCurveIdx             := -1;
  FCurrentPolygonIdx           := -1;
  FCurrentSimpleLigneIdx       := -1;
  FCurrentTexteIdx             := -1;
  // styles d'objets

  FCurrentNatureCourbe         := nocDEFAULT;
  FCurrentNaturePolygone       := nopDEFAULT;
  FCurrentNatureLigne          := nolDEFAULT;
  FCurrentNatureTexte          := notTEXTE1;

  //try
    FRedessinInProcess   := false;
    FDoDraw              := false;
    FHerissonViseesRadiantes := FS;
    FBackGroundColor     := $E0E0E0; //** TODO: A personnaliser //clCream;
    FScrapEnglobantColor := clWhite;
    FAxisColor           := clYellow;
    result := true;

  //except
  //end;
  // Textures
  FTexturesAreOK := false;
  // bitmaps pour les remplissages
  FTexturesPolygones.bmpClay       := TBGRABitmap.Create;
  FTexturesPolygones.bmpSand       := TBGRABitmap.Create;
  FTexturesPolygones.bmpEboulis    := TBGRABitmap.Create;
  FTexturesPolygones.bmpGalets     := TBGRABitmap.Create;
  FTexturesPolygones.bmpSnow       := TBGRABitmap.Create;
  FTexturesPolygones.bmpClayGalets := TBGRABitmap.Create;
  try
    // textures  /!\ RAPPEL: Les fonctions CreateXxxTexture créent l'objet
    //                       Ne pas utiliser .Create
    FTexturesPolygones.bmpTextureFlotte  := CreateWaterTexture(100, 100);
    FTexturesPolygones.bmpTextureEboulis := CreateStoneTexture(100, 100);

    InitialDir := GetIconesTexturesDirectory();
    ParametrerBitmap(FTexturesPolygones.bmpClay      , 'clay.bmp');
    ParametrerBitmap(FTexturesPolygones.bmpSand      , 'sand.bmp');
    ParametrerBitmap(FTexturesPolygones.bmpEboulis   , 'eboulis.bmp');
    ParametrerBitmap(FTexturesPolygones.bmpGalets    , 'galets.bmp');
    ParametrerBitmap(FTexturesPolygones.bmpSnow      , 'snow.bmp');
    ParametrerBitmap(FTexturesPolygones.bmpClayGalets, 'clay_galets.bmp');
    AfficherMessage('Loading textures OK');
    FTexturesAreOK := true;
  except
    AfficherMessage('*** Failed to Loading textures');
  end;
  SetModeTravail(mtNONE, 0);
  FCrossSectionPolyProvisoire := TCrossSectionPolyProvisoire.Create;
  FCrossSectionPolyProvisoire.ClearVertex();
  Result := True;
end;
function TCadreSectionTransversale.CdrDrwFinalize  : boolean;
begin
  FDoDraw := False;
  AfficherMessage(Format('%s.CdrDrwFinalize()', [ClassName]));
  Result := False;
  try
    Result := true;
    FCrossSectionPolyProvisoire.ClearVertex();
  finally
    FreeAndNil(FCrossSectionPolyProvisoire);//FCrossSectionPolyProvisoire.Free;
    (*
    FTexturesPolygones.bmpClay.free;
    FTexturesPolygones.bmpSand.free;
    FTexturesPolygones.bmpEboulis.free;
    FTexturesPolygones.bmpGalets.Free;
    FTexturesPolygones.bmpSnow.free;
    FTexturesPolygones.bmpClayGalets.free;
    // libère textures
    FTexturesPolygones.bmpTextureFlotte.Free;
    FTexturesPolygones.bmpTextureEboulis.Free;
    //*)
  end;
end;

procedure TCadreSectionTransversale.DrawPolygone(const QBmp: TCrossSectionDrawingContext;
                                                 const MyPolygone: TCrossSectionPolygone;
                                                 const DoDrawSommets: boolean;
                                                 const DoUseStyles: boolean = true);
const
  QR = 6;
var
  s, j: Integer;
  AC: TPoint2Df;
begin
  QBmp.TracePolygone(MyPolygone, DoDrawSommets, DoUseStyles);
  if (DoDrawSommets) then
  begin
    QBmp.DefineCrayon(psSolid, 2, clRed, 255);
    QBmp.DefineBrosse(bsClear, clAqua, 128);
    s := High(MyPolygone.Vertexes);
    for j := 0 to s do
    begin
      AC := MyPolygone.Vertexes[j];
      QBmp.TraceCercle(AC.X, AC.Y, QR);
    end;
    QBmp.RestoreCrayon();
    QBmp.RestoreBrosse();
  end;
end;

procedure TCadreSectionTransversale.DrawCourbe(const QBmp: TCrossSectionDrawingContext;
                                               const MyCourbe: TCrossSectionCourbe;
                                               const DoDrawSommets: boolean);
const
  QR = 6;
var
  s, j: Integer;
  AC: TCrossSectionArcCourbe;
  PC1, PC2: TPoint2Df;
  EWE: String;
begin
  QBmp.TraceCourbeBezier(MyCourbe, True);
  if (DoDrawSommets) then
  begin
    QBmp.DefineCrayon(psSolid, 0, clRed, 255);
    QBmp.DefineBrosse(bsSolid, clAqua, 128);
    s := High(MyCourbe.Arcs);
    for j := 0 to s do
    begin
      AC := MyCourbe.Arcs[j];
      QBmp.TraceVers(AC.CoordsP1.X, AC.CoordsP1.Y, false);
      PC1.X := AC.CoordsP1.X + AC.TangP1.X;
      PC1.Y := AC.CoordsP1.Y + AC.TangP1.Y;
      QBmp.TraceVers(PC1.X, PC1.Y, true);
      PC2.X := AC.CoordsP2.X + AC.TangP2.X;
      PC2.Y := AC.CoordsP2.Y + AC.TangP2.Y;
      QBmp.TraceVers(PC2.X, PC2.Y, true);
      QBmp.TraceVers(AC.CoordsP2.X, AC.CoordsP2.Y, true);
      QBmp.TraceCercle(AC.CoordsP1.X, AC.CoordsP1.Y, QR);
      QBmp.TraceCercle(PC1.X, PC1.Y, QR);
      QBmp.TraceCercle(PC2.X, PC2.Y, QR);
      QBmp.TraceCercle(AC.CoordsP2.X, AC.CoordsP2.Y, QR);
    end;
    QBmp.RestoreCrayon();
    QBmp.RestoreBrosse();
  end;
end;

procedure TCadreSectionTransversale.DrawSimpleLigne(const QBmp: TCrossSectionDrawingContext; const MySimpleLigne: TCrossSectionSimpleLigne; const DoDrawSommets: boolean);
begin
  QBmp.TraceSimpleLigne(MySimpleLigne);
  if (DoDrawSommets) then
  begin
    QBmp.DefineCrayon(psSolid, 4, clRed, 255);
      QBmp.TraceVers(MySimpleLigne.Extr1.X, MySimpleLigne.Extr1.Y, False);
      QBmp.TraceVers(MySimpleLigne.Extr2.X, MySimpleLigne.Extr2.Y, True);
      QBmp.TraceCercle(MySimpleLigne.Extr1.X, MySimpleLigne.Extr1.Y, 2);
      QBmp.TraceCercle(MySimpleLigne.Extr2.X, MySimpleLigne.Extr2.Y, 2);
    QBmp.RestoreCrayon();
  end;
end;
procedure TCadreSectionTransversale.DrawTexte(const QBmp: TCrossSectionDrawingContext; const MyTexte: TCrossSectionTexte; const DoDrawSommets: boolean);
begin
  QBmp.TraceTexte(MyTexte);
  if (DoDrawSommets) then
  begin
    QBmp.DefineBrosse(bsSolid, clRed, 255);
    QBmp.TraceCercle(MyTexte.PosX, MyTexte.PosY, 8);
    QBmp.RestoreBrosse();
  end;
end;

procedure TCadreSectionTransversale.DrawSectionTransversale(const QBmp: TCrossSectionDrawingContext);
var
  n, i, s, j: Integer;
  T: TCrossSectionTexte;
  C: TCrossSectionCourbe;
  AC: TCrossSectionArcCourbe;
  P: TCrossSectionPolygone;
  VX: TPoint2Df;
  SL: TCrossSectionSimpleLigne;
  procedure TracerScrapEnglobant();
  var
    MyScrapEnglobant: TCrossSectionPolygone;
  begin

    MyScrapEnglobant := FMyCrossSection.getScrapEnglobant();
    if (0 = length(MyScrapEnglobant.Vertexes)) then Exit;
    QBmp.DefineBrosse(bsSolid, FScrapEnglobantColor, 255);      // clAqua est provisoire
    QBmp.DefineCrayon(psClear, 0, clWhite, 255);
      DrawPolygone(QBmp, MyScrapEnglobant, false, false);


    QBmp.RestoreCrayon();
    QBmp.RestoreBrosse();
  end;

begin
  AfficherMessage(format('%s.DrawSection()', [ClassName]));
  if (not Assigned(FMyCrossSection)) then
  begin
    AfficherMessage('-- Classe FMyCrossSection non créée');
    Exit;
  end;
  // le scrap englobant, tracé avant tout le reste
  TracerScrapEnglobant();

  // les polygones
  n := FMyCrossSection.getNbPolygones();
  AfficherMessage(Format('--- %d polygones', [n]));
  if (n > 0) then
  begin
    QBmp.DefineBrosse(bsSolid, clBlue, 128);
    for i := 0 to n - 1 do
    begin
      P := FMyCrossSection.getPolygone(i);
      DrawPolygone(QBmp, P, false);
      if ((FCurrentPolygonIdx >= 0) and (i = FCurrentPolygonIdx)) then DrawPolygone(QBmp, P, True);
    end;
  end;
  QBmp.DefineBrosse(bsSolid, FBackGroundColor, 255); // restauration de la brosse par défaut
  QBmp.CanvasBGRA.Brush.Texture := nil;              // et désactivation des textures
  // les lignes
  n := FMyCrossSection.getNbSimplesLignes();
  AfficherMessage(Format('--- %d simples lignes', [n]));
  if (n > 0) then
  begin
    QBmp.DefineCrayon(psSolid, 0, clRed, 255);

    for i := 0 to n - 1 do
    begin
      SL := FMyCrossSection.getSimpleLigne(i);
      DrawSimpleLigne(QBmp, SL, false);
      if ((FCurrentSimpleLigneIdx >= 0) and (i = FCurrentSimpleLigneIdx)) then DrawSimpleLigne(QBmp, SL, True);
    end;
  end;
  // les courbes
  n := FMyCrossSection.getNbCourbes();
  AfficherMessage(Format('--- %d courbes', [n]));
  if (n > 0) then
  begin
    QBmp.DefineCrayon(psSolid, 2, clMaroon, 255);
    for i := 0 to n - 1 do
    begin
      C := FMyCrossSection.getCourbe(i);
      DrawCourbe(QBmp, C, false);
      if ((FCurrentCurveIdx >= 0) and (i = FCurrentCurveIdx)) then DrawCourbe(QBmp, C, true);
    end;
  end;
  // les textes
  n := FMyCrossSection.getNbTextes();
  AfficherMessage(Format('--- %d textes', [n]));
  if (n > 0) then
  begin
    QBmp.DefineBrosse(bsSolid, FBackGroundColor, 255);
    for i := 0 to n - 1 do
    begin
      T := FMyCrossSection.getTexte(i);
      DrawTexte(QBmp, T, false);
      if ((FCurrentTexteIdx >= 0) and (i = FCurrentTexteIdx)) then DrawTexte(QBmp, T, True);
    end;
  end;
end;

function TCadreSectionTransversale.GetHerisson: THerissonViseesRadiantes; deprecated 'Fonction de test unitaire';
begin
  result := FHerissonViseesRadiantes;
end;



procedure TCadreSectionTransversale.acCourbeAreteExecute(Sender: TObject);
begin
  SetModeTravail(mtDRAW_COURBE, Ord(nocPAROIS_CACHEE));
end;

procedure TCadreSectionTransversale.acClippingByScrapExecute(Sender: TObject);
begin
  FMyCrossSection.ClippingByScrapEnglobant();
  Vue.Invalidate;
end;

procedure TCadreSectionTransversale.acNewCourbeExecute(Sender: TObject);
begin
  PreparerSubButtons(mtDRAW_COURBE);
  //SetModeTravail(mtDRAW_COURBE);
end;

procedure TCadreSectionTransversale.acCourbeParoisExecute(Sender: TObject);
begin
  SetModeTravail(mtDRAW_COURBE, Ord(nocPAROI));

end;

procedure TCadreSectionTransversale.acCourbeParoiIncertaineExecute(Sender: TObject);
begin
  SetModeTravail(mtDRAW_COURBE, Ord(nocPAROI_INCERTAINE));
end;

procedure TCadreSectionTransversale.acDeleteCourbeExecute(Sender: TObject);
begin
  if (FCurrentCurveIdx < 0) then Exit;
  if (not QuestionOuiNon(Format('Supprimer l''objet %d', [FCurrentCurveIdx]))) then Exit;
  FMyCrossSection.RemoveCourbe(FCurrentCurveIdx);
  Vue.Invalidate;
end;

procedure TCadreSectionTransversale.acDeletePolygoneExecute(Sender: TObject);
begin
  if (FCurrentPolygonIdx < 0) then Exit;
  if (not QuestionOuiNon(Format('Supprimer l''objet %d', [FCurrentPolygonIdx]))) then Exit;
  FMyCrossSection.RemovePolygone(FCurrentPolygonIdx);
  Vue.Invalidate;
end;

procedure TCadreSectionTransversale.acDeleteSimpleLigneExecute(Sender: TObject);
begin
  if (FCurrentSimpleLigneIdx < 0) then Exit;
  if (not QuestionOuiNon(Format('Supprimer l''objet %d', [FCurrentSimpleLigneIdx]))) then Exit;
  FMyCrossSection.RemoveSimpleLigne(FCurrentSimpleLigneIdx);
  Vue.Invalidate;
end;

procedure TCadreSectionTransversale.acDeleteTexteExecute(Sender: TObject);
begin
  if (FCurrentTexteIdx < 0) then Exit;
  if (not QuestionOuiNon(Format('Supprimer l''objet %d', [FCurrentTexteIdx]))) then Exit;
  FMyCrossSection.RemoveTexte(FCurrentTexteIdx);
  Vue.Invalidate;
end;

procedure TCadreSectionTransversale.acLigneDiverseExecute(Sender: TObject);
begin
  SetModeTravail(mtLIGNE_PREMIER_POINT, 3);
end;

procedure TCadreSectionTransversale.acLigneFailleExecute(Sender: TObject);
begin
  SetModeTravail(mtLIGNE_PREMIER_POINT, Ord(nolFRACTURE));
end;

procedure TCadreSectionTransversale.acLignePendageExecute(Sender: TObject);
begin
  SetModeTravail(mtLIGNE_PREMIER_POINT, Ord(nolPENDAGE));
end;

procedure TCadreSectionTransversale.PreparerSubButtons(const MT: TCrossSectionModeTravail);
begin
  AffecterActionAtButton(btnDrwObjMode1  , acNewNone);
  AffecterActionAtButton(btnDrwObjMode2  , acNewNone);
  AffecterActionAtButton(btnDrwObjMode3  , acNewNone);
  AffecterActionAtButton(btnDrwObjMode4  , acNewNone);
  AffecterActionAtButton(btnDrwObjMode5  , acNewNone);
  AffecterActionAtButton(btnDrwObjMode6  , acNewNone);

  AffecterActionAtButton(btnDrwObjMode7  , acNewNone);
  AffecterActionAtButton(btnDrwObjMode8  , acNewNone);
  AffecterActionAtButton(btnDrwObjMode9  , acNewNone);
  AffecterActionAtButton(btnDrwObjMode10 , acNewNone);
  //AffecterActionAtButton(btnDrwObjMode11 , acNewNone);
  //AffecterActionAtButton(btnDrwObjMode12 , acNewNone);

  case MT of
    mtNONE: ; // ne rien faire
    mtDRAW_COURBE:
      begin
        AffecterActionAtButton(btnDrwObjMode1, acCourbeParois);
        AffecterActionAtButton(btnDrwObjMode2, acCourbeParoiIncertaine);
        AffecterActionAtButton(btnDrwObjMode3, acCourbeArete);

      end;
    mtDRAW_LINE:
      begin
        AffecterActionAtButton(btnDrwObjMode1, acLigneFaille);
        AffecterActionAtButton(btnDrwObjMode2, acLignePendage);
        AffecterActionAtButton(btnDrwObjMode3, acLigneDiverse);
      end;
    mtDRAW_POLYGONE:
      begin
        AffecterActionAtButton(btnDrwObjMode1, acPolygoneGrosBloc);
        AffecterActionAtButton(btnDrwObjMode2, acPolygoneEboulis);
        AffecterActionAtButton(btnDrwObjMode3, acPolygoneGalets);
        AffecterActionAtButton(btnDrwObjMode4, acPolygoneArgileGalets);
        AffecterActionAtButton(btnDrwObjMode5, acPolygoneArgile);
        AffecterActionAtButton(btnDrwObjMode6, acPolygoneSable);
        AffecterActionAtButton(btnDrwObjMode7, acPolygoneNeve);
        AffecterActionAtButton(btnDrwObjMode8, acPolygoneEau);
        AffecterActionAtButton(btnDrwObjMode9, acPolygoneMasque);


      end;

    mtDRAW_TEXTE:
      begin
        AffecterActionAtButton(btnDrwObjMode1, acTexteOrdinaire);
        AffecterActionAtButton(btnDrwObjMode2, acTexteCotation);
      end;
  end;
end;
procedure TCadreSectionTransversale.acNewNoneExecute(Sender: TObject);
begin
  SetModeTravail(mtNONE, 0);
  PreparerSubButtons(mtNONE);
end;

procedure TCadreSectionTransversale.acPanDownExecute(Sender: TObject);
begin
  SetViewLimits(FRXMini, FRYMini + M, FRXMaxi, FRYMaxi + M);
end;

procedure TCadreSectionTransversale.acPanLeftExecute(Sender: TObject);
begin
  SetViewLimits(FRXMini + M, FRYMini, FRXMaxi + M, FRYMaxi);
end;

procedure TCadreSectionTransversale.acPanRightExecute(Sender: TObject);
begin
  SetViewLimits(FRXMini - M, FRYMini, FRXMaxi - M, FRYMaxi);
end;

procedure TCadreSectionTransversale.acPanUpExecute(Sender: TObject);
begin
  SetViewLimits(FRXMini, FRYMini - M, FRXMaxi, FRYMaxi - M);
end;

procedure TCadreSectionTransversale.acPolygoneArgileExecute(Sender: TObject);
begin
  SetModeTravail(mtDRAW_POLYGONE, ord(nopARGILE));
end;

procedure TCadreSectionTransversale.acPolygoneArgileGaletsExecute(Sender: TObject);
begin
  SetModeTravail(mtDRAW_POLYGONE, ord(nopARGILES_GALETS));
end;

procedure TCadreSectionTransversale.acPolygoneEauExecute(Sender: TObject);
begin
  SetModeTravail(mtDRAW_POLYGONE, ord(nopLAC));
end;

procedure TCadreSectionTransversale.acPolygoneEboulisExecute(Sender: TObject);
begin
  SetModeTravail(mtDRAW_POLYGONE, ord(nopEBOULIS));
end;

procedure TCadreSectionTransversale.acNewPolygoneExecute(Sender: TObject);
begin
  PreparerSubButtons(mtDRAW_POLYGONE);
end;

procedure TCadreSectionTransversale.acPolygoneGaletsExecute(Sender: TObject);
begin
  SetModeTravail(mtDRAW_POLYGONE, ord(nopGALETS));
end;

procedure TCadreSectionTransversale.acPolygoneGrosBlocExecute(Sender: TObject);
begin
  SetModeTravail(mtDRAW_POLYGONE, ord(nopGROS_BLOC));
end;

procedure TCadreSectionTransversale.acPolygoneMasqueExecute(Sender: TObject);
begin
  SetModeTravail(mtDRAW_POLYGONE, ord(nopMASQUES));
end;

procedure TCadreSectionTransversale.acPolygoneNeveExecute(Sender: TObject);
begin
  SetModeTravail(mtDRAW_POLYGONE, ord(nopNEIGE));
end;

procedure TCadreSectionTransversale.acPolygoneSableExecute(Sender: TObject);
begin
   SetModeTravail(mtDRAW_POLYGONE, ord(nopSABLE));
end;

procedure TCadreSectionTransversale.acResetExecute(Sender: TObject);
begin
  SetModeTravail(mtNONE, 0);
  ListerLesObjets(csloNONE, 0);
end;

procedure TCadreSectionTransversale.acScrapEnglobantExecute(Sender: TObject);
begin
  FMyCrossSection.MakeScrapEnglobant();
  Vue.Invalidate;
end;

procedure TCadreSectionTransversale.acSelectCourbeExecute(Sender: TObject);
begin
  SetModeTravail(mtSELECT_COURBE, Ord(FCurrentNatureCourbe));
  AffecterActionAtButton(btnDeleteObject, acDeleteCourbe);
  ListerLesObjets(csloCOURBES, 0);

end;

procedure TCadreSectionTransversale.acSelectPolygoneExecute(Sender: TObject);
begin
  SetModeTravail(mtSELECT_POLYGONE, Ord(FCurrentNaturePolygone));
  AffecterActionAtButton(btnDeleteObject, acDeletePolygone);
  ListerLesObjets(csloPOLYGONES, 0);
end;

procedure TCadreSectionTransversale.acSelectSimpleLigneExecute(Sender: TObject);
begin
  SetModeTravail(mtSELECT_LIGNE, Ord(FCurrentNatureLigne));
  AffecterActionAtButton(btnDeleteObject, acDeleteSimpleLigne);
  ListerLesObjets(csloLIGNES, 0);
end;

procedure TCadreSectionTransversale.acSelectTexteExecute(Sender: TObject);
begin
  SetModeTravail(mtSELECT_TEXTE, Ord(FCurrentNatureTexte));
  AffecterActionAtButton(btnDeleteObject, acDeleteTexte);
  ListerLesObjets(csloTEXTES, 0);
end;

procedure TCadreSectionTransversale.acNewLigneExecute(Sender: TObject);
begin
  PreparerSubButtons(mtDRAW_LINE);
  //SetModeTravail(mtLIGNE_PREMIER_POINT);
end;

procedure TCadreSectionTransversale.acTexteCotationExecute(Sender: TObject);
begin
  SetModeTravail(mtPICK_POS_TEXTE, ord(notCOTATION));

end;

procedure TCadreSectionTransversale.acNewTexteExecute(Sender: TObject);
begin
   PreparerSubButtons(mtDRAW_TEXTE);
  //SetModeTravail(mtDRAW_TEXTE);
end;

procedure TCadreSectionTransversale.acTexteOrdinaireExecute(Sender: TObject);
begin
  SetModeTravail(mtPICK_POS_TEXTE, ord(notTEXTE1));
end;



procedure TCadreSectionTransversale.acValidateCourbePolygoneExecute(Sender: TObject);
begin
  AfficherMessage('Pret pour enregistrer');
  case FModeTravail of
   mtDRAW_COURBE:
     begin
       QAjouterUneCourbe;
       SetModeTravail(mtDRAW_COURBE, Ord(FCurrentNatureCourbe));
       ListerLesObjets(csloCOURBES, FMyCrossSection.getNbCourbes() - 1);
     end;
   mtDRAW_POLYGONE:
     begin
       QAjouterUnPolygone;
       SetModeTravail(mtDRAW_POLYGONE, Ord(FCurrentNaturePolygone));
       ListerLesObjets(csloPOLYGONES, FMyCrossSection.getNbPolygones() - 1);
     end;
  end;

end;

procedure TCadreSectionTransversale.acZoomMoinsExecute(Sender: TObject);
begin
  SetViewLimits(FRXMini - M, FRYMini - M, FRXMaxi + M, FRYMaxi + M);
end;

procedure TCadreSectionTransversale.acZoomPlusExecute(Sender: TObject);
begin
  SetViewLimits(FRXMini + M, FRYMini + M, FRXMaxi - M, FRYMaxi - M);
end;

procedure TCadreSectionTransversale.acZoomToutExecute(Sender: TObject);
begin
  SetViewLimits(-10, -10, 10, 10);
end;


//******************************************************************************
// conversion de coordonnées
function TCadreSectionTransversale.GetCoordsPlan (const PM: TPoint2Df): TPoint;
begin
  Result.X:=Round((PM.X - FRXMini) * FRappScrReal);
  Result.Y:=Round((FRYMaxi-PM.Y) * FRappScrReal);
end;
function TCadreSectionTransversale.GetCoordsMonde(const PP: TPoint): TPoint2Df;
begin
  Result.X:=  FInvRappScrReal * PP.X + FRXMini;
  Result.Y:= -FInvRappScrReal * PP.Y + FRYMaxi;

end;

function TCadreSectionTransversale.GetCoordsPlan(const QX, QY: double): TPoint;
begin
  Result := GetCoordsPlan(MakeTPoint2Df(QX, QY));
end;

//******************************************************************************
// zooms et limites de vue
procedure TCadreSectionTransversale.SetViewLimits(const X1, Y1, X2, Y2: double);
const Epsilon=1e-2;
var
  qX1, qX2, qY1, qY2: Double;
  d1, d2: double;
begin
  AfficherMessage(Format('%s.SetViewLimits(%.2f, %.2f, %.2f, %.2f)', [ClassName, X1, Y1, X2, Y2]));
  qX1 := Math.Min(X1, X2);
  qY1 := Math.Min(Y1, Y2);
  qX2 := Math.Max(X1, X2);
  qY2 := Math.Max(Y1, Y2);

  d1 := qX2 - qX1;
  d2 := qY2 - qY1;
  // si zone trop étroite, abandonner
  if ((Abs(d1) < Epsilon) or (Abs(d2) < Epsilon)) then Exit;

  FRXMini:=qX1;
  FRXMaxi:=qX2;
  FRYMini:=qY1;
  FRYMaxi:=qY2;
  // Redéfinition de la hauteur maxi
  FRYMaxi:=GetRYMaxi;
  // redessine
  FDoDraw := True;
  Vue.Invalidate;
end;


procedure TCadreSectionTransversale.VueMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  procedure QDrawLigne(const P1, P2: TPoint); inline;
  begin
    Vue.Canvas.Line(P1.X,P1.Y, P2.X, P2.Y);
  end;
  procedure QDrawRectangle(const P1, P2: TPoint); inline;
  begin
    Vue.Canvas.Rectangle(P1.X, P1.Y, P2.X, P2.Y);
  end;
  procedure DrawShapeElastique(const QX, QY: integer; const SType: byte);
  begin
(*    ZP1 := GetCoordsPlan(FZC1);

    case SType of
      1: QDrawLigne(ZP1, ZP2);
      2: QDrawRectangle(ZP1, ZP2);
    end;
    ZP2 := MakeTPoint(QX, QY);
    case SType of
      1: QDrawLigne(FZP1, FZP2);
      2: QDrawRectangle(FZP1, FZP2);
    end;
//*)
  end;
begin
  try
    //if (not FDoDraw) then Exit;

    FMyPos := GetCoordsMonde(MakeTPoint(X, Y));
    lbMousePos.Caption := Format('X = %.2f m, Y = %.2f m',[FMyPos.X, FMyPos.Y]);

    case FModeTravail of
      mtLIGNE_SECOND_POINT:
      begin
        if (Not FAttenduSecondPoint) then Exit;
        DrawShapeElastique(X, Y, 1);
      end;
    else;
    end;
  except
  end;
end;

// attrape la hauteur de vue
function TCadreSectionTransversale.GetRYMaxi: double;
// Cette fonction calcule également d'autres paramètres
begin
  // calcul du rapport Hauteur/largeur de vue
  FRappHLVue   := Vue.Height / Vue.Width;
  // calcul du rapport Ecran/Réel
  FRappScrReal := Vue.Width / (FRXMaxi-FRXMini);
  FInvRappScrReal:=1/FRappScrReal;
  // calcul de la hauteur de visualisation
  Result:=FRYMini + (FRXMaxi-FRXMini) * FRappHLVue;
end;


procedure TCadreSectionTransversale.ListerLesObjets(const M: TCrossSectionTypeListeObjets; const QIdx: integer);
var
  Nb, i: Integer;
  QC   : TCrossSectionCourbe;
  QP   : TCrossSectionPolygone;
  QL   : TCrossSectionSimpleLigne;
  QT   : TCrossSectionTexte;
begin
  // on masque tous les panels d'attributs d'objets
  DisplayAttributsCourbe(QIdx, false);
  DisplayAttributsPolygone(QIdx, false);
  DisplayAttributsSimpleLigne(QIdx, false);
  DisplayAttributsTexte(QIdx, false);
  nb := 0;
  lsbObjects.Clear;
  case M of
    csloNONE      : ;
    csloCOURBES   : Nb := FMyCrossSection.getNbCourbes();
    csloPOLYGONES : Nb := FMyCrossSection.getNbPolygones();
    csloLIGNES    : Nb := FMyCrossSection.getNbSimplesLignes();
    csloTEXTES    : Nb := FMyCrossSection.getNbTextes();
  end;
  if (Nb = 0) then Exit;
  case M of
    csloNONE:  //ne rien faire
      begin
        lsbObjects.Clear;
      end;
    csloCOURBES:
      begin
        for i := 0 to Nb -1 do
        begin
          QC := FMyCrossSection.getCourbe(i);
          lsbObjects.Items.Add(Format('Courbe%d: %d arcs', [i, 1 + High(QC.Arcs)]));
        end;
      end;
    csloPOLYGONES:
      begin
        for i := 0 to Nb -1 do
        begin
          QP := FMyCrossSection.getPolygone(i);
          lsbObjects.Items.Add(Format('Polygone%d: %d sommets', [i, 1 + High(QP.Vertexes)]));
        end;
      end;
    csloLIGNES:
      begin
        for i := 0 to Nb -1 do
        begin
          QL := FMyCrossSection.getSimpleLigne(i);
          lsbObjects.Items.Add(Format('Ligne%d: (%.3f, %.3f) - (%.3f, %.3f)',
                                      [i, QL.Extr1.X, QL.Extr1.Y, QL.Extr2.X, QL.Extr2.Y]));
        end;
      end;
    csloTEXTES:
      begin
        for i := 0 to Nb - 1 do
        begin
          QT := FMyCrossSection.getTexte(i);
          lsbObjects.Items.Add(Format('Texte%d: (%.3f, %.3f) - %s', [i, QT.PosX, QT.PosY, QT.Text]));
        end;
        // afficher les attributs de texte
        DisplayAttributsTexte(QIdx, true);
      end;
  end;
  lsbObjects.ItemIndex := QIdx;
end;
procedure TCadreSectionTransversale.DisplayAttributsCourbe(const QIdx: integer; const QVisible: boolean);
begin
  if (not QVisible) then Exit;

end;
procedure TCadreSectionTransversale.DisplayAttributsPolygone(const QIdx: integer; const QVisible: boolean);
begin
  if (not QVisible) then Exit;

end;
procedure TCadreSectionTransversale.DisplayAttributsSimpleLigne(const QIdx: integer; const QVisible: boolean);
begin
  if (not QVisible) then Exit;

end;
procedure TCadreSectionTransversale.DisplayAttributsTexte(const QIdx: integer; const QVisible: boolean);
var
  T: TCrossSectionTexte;
  procedure SetBtnAlignement();
  begin
    rbAli1.Checked := False;
    rbAli2.Checked := False;
    rbAli3.Checked := False;
    rbAli4.Checked := False;
    rbAli5.Checked := False;
    rbAli6.Checked := False;
    rbAli7.Checked := False;
    rbAli8.Checked := False;
    rbAli9.Checked := False;
    case T.Alignment of
      1: rbAli1.Checked := True;
      2: rbAli2.Checked := True;
      3: rbAli3.Checked := True;
      4: rbAli4.Checked := True;
      5: rbAli5.Checked := True;
      6: rbAli6.Checked := True;
      7: rbAli7.Checked := True;
      8: rbAli8.Checked := True;
      9: rbAli9.Checked := True;
    else
      rbAli1.Checked := True;
    end;
  end;
begin
  pnlAttributsTexte.Visible := QVisible;
  if (not pnlAttributsTexte.Visible) then Exit;
  T := FMyCrossSection.getTexte(QIdx);
  lbIDTexte.Caption := format('%d', [QIdx]);
  SetBtnAlignement();
  editTexte.Text := T.Text;
  editPosX.Value := T.PosX;
  editPosY.Value := T.PosY;
  case T.IDStyleTexte of
    notTEXTE1   : cmbStyleTexte.ItemIndex := 0;
    notCOTATION : cmbStyleTexte.ItemIndex := 1;
  else
    cmbStyleTexte.ItemIndex := 0;
  end;
end;

function TCadreSectionTransversale.GetAttribsTexteFromForm(): TCrossSectionTexte;
  function GetAlignement(): byte;
  begin
    Result := 0;
    if (rbAli1.Checked) then result := 1;
    if (rbAli2.Checked) then result := 2;
    if (rbAli3.Checked) then result := 3;
    if (rbAli4.Checked) then result := 4;
    if (rbAli5.Checked) then result := 5;
    if (rbAli6.Checked) then result := 6;
    if (rbAli7.Checked) then result := 7;
    if (rbAli8.Checked) then result := 8;
    if (rbAli9.Checked) then result := 9;
  end;
begin
  Result.Text := editTexte.Text;
  Result.PosX := editPosX.Value;
  Result.PosY := editPosY.Value;
  Result.Alignment := GetAlignement();
  case cmbStyleTexte.ItemIndex of
    0: Result.IDStyleTexte := notTEXTE1;
    1: Result.IDStyleTexte := notCOTATION;
  end;
end;


procedure TCadreSectionTransversale.lsbObjectsClick(Sender: TObject);
begin
  AfficherModeTravail(FModeTravail);
  if (lsbObjects.Items.Count = 0) then Exit;
  case FModeTravail of
    mtSELECT_LIGNE   : FCurrentSimpleLigneIdx  := lsbObjects.ItemIndex;
    mtSELECT_COURBE  : FCurrentCurveIdx        := lsbObjects.ItemIndex;
    mtSELECT_POLYGONE: FCurrentPolygonIdx      := lsbObjects.ItemIndex;
    mtSELECT_TEXTE   : FCurrentTexteIdx        := lsbObjects.ItemIndex;
  else
    ;
  end;
  case FModeTravail of
    mtSELECT_LIGNE   : DisplayAttributsSimpleLigne(lsbObjects.ItemIndex, true);
    mtSELECT_COURBE  : DisplayAttributsCourbe(lsbObjects.ItemIndex, true);
    mtSELECT_POLYGONE: DisplayAttributsPolygone(lsbObjects.ItemIndex, true);
    mtSELECT_TEXTE   : DisplayAttributsTexte(lsbObjects.ItemIndex, true);
  else
    ;
  end;

end;

procedure TCadreSectionTransversale.MenuItem1Click(Sender: TObject);
begin

end;

procedure TCadreSectionTransversale.sclAngleOrientationChange(Sender: TObject);
begin
  FHerissonViseesRadiantes.setAngleOrientationSection(sclAngleOrientation.Position);
  FHerissonViseesRadiantes.CalcProjsPZAndSetMiniMaxi();
  vue.Invalidate;
end;

procedure TCadreSectionTransversale.AffecterActionAtButton(const WU: TSpeedButton; const QAction: TAction); inline;
begin
 WU.Glyph  := nil;
 WU.Action := QAction;
 AfficherMessageErreur(QAction.Caption);
end;

procedure TCadreSectionTransversale.btnApplyModifsTexteClick(Sender: TObject);
var
  EWE: TCrossSectionTexte;
begin
  EWE := GetAttribsTexteFromForm();
  FMyCrossSection.putTexte(lsbObjects.ItemIndex, EWE);
  Vue.Invalidate;
end;

procedure TCadreSectionTransversale.btnDetourerClick(Sender: TObject);
begin

end;

procedure TCadreSectionTransversale.QAjouterUneCourbe;
var
  C: TCrossSectionCourbe;
begin
  if (FCrossSectionPolyProvisoire.GenererCourbe(True, C)) then
  begin
    C.IDStyleCourbe := FCurrentNatureCourbe;
    FMyCrossSection.addCourbe(C);
  end;
  Vue.Invalidate; // RedessinEcran(false);
end;

procedure TCadreSectionTransversale.QAjouterUnPolygone;
var
  P: TCrossSectionPolygone;
begin
  if (FCrossSectionPolyProvisoire.GenererPolygone(P)) then
  begin
    P.IDStylePolygone := FCurrentNaturePolygone; //    C.IDStyleCourbe := FCurrentNatureCourbe; //FDocumentDessin.GetLineStyle(FCurrentLineStyle);
    FMyCrossSection.addPolygone(P);
  end;
  Vue.Invalidate; // RedessinEcran(false);
end;

function TCadreSectionTransversale.QRechercheEtTraceCourbe(out QIdx: integer): boolean;
var
  CV  : TCourbe;
  BBX : TBoundingBox;
  QPM : TPoint2Df;
  q: Int64;
begin
  Result := False;
  (*
  QIdx
  n :=
  q := FDocumentDessin.FindObject(FMyPos.X, FMyPos.Y, mseCOURBES);
  if (q >= 0) then
  begin
    SetCurrentCourbeIdx(q);
    CV := FDocumentDessin.GetCourbe(q);
    FProcGetCourbe(CV, q);
    BBX := CV.BoundingBox; // on sauvegarde la BoundingBox
    QPM := MakeTPoint2Df(CV.BoundingBox.C1.X, CV.BoundingBox.C1.Y);
    QPM := MakeTPoint2Df(CV.BoundingBox.C2.X, CV.BoundingBox.C2.Y);
    try
       FCourbePolygoneProvisoire.ClearVertex;
       // récupération du groupe et du style
       FIDGroupeEditing  := CV.IDGroupe;
       FIDStyleCurveEditing:= CV.IDStyleCourbe;
       FCourbePolygoneProvisoire.LoadCourbe(CV, false);  // chargement courbe
       FCourbePolygoneProvisoire.GenerateCourbe(False, -1, -1, CV);
       CV.BoundingBox := BBX; // restauration de la BoundingBox
       DrawVolatileCourbe(vue.Canvas, CV, q, True, True);
       // affecter le bouton de strip
       AffecterActionABouton(btnEditObjectSpecial, acReverseCourbe);
       QIdx := Q;
       Result := True;
     except
     end;
  end;
  //*)
end;


//******************************************************************************
procedure TCadreSectionTransversale.Redessin;
var
  TmpBuffer : TCrossSectionDrawingContext;
  procedure DrawViseesRayonnantes();
  const DC = 0.25;
  var
    n, i: Integer;
    EWE: TViseeRayonnante;
    WU: TColor;
    PP, PZ: double;
  begin
    n := FHerissonViseesRadiantes.getNbViseesRayonnantes();
    if (n > 0) then
    begin
      TmpBuffer.DefineCrayon(psSolid, 0, clSilver, 255);
      for i := 0 to n -1 do
      begin
        EWE := FHerissonViseesRadiantes.getViseeRayonnante(i);
        FHerissonViseesRadiantes.CalcProjPZVisee(EWE.Longueur, EWE.Azimut, EWE.Pente, PP, PZ);
        TmpBuffer.TraceVers(0.00, 0.00, false);
        TmpBuffer.TraceVers(PP, PZ, True);
        if (EWE.AsSection) then
        begin
          TmpBuffer.TraceCercle(PP, PZ, 4);
        end;
      end;
    end;
    TmpBuffer.RestoreCrayon();
    TmpBuffer.DefineCrayon(psSolid, 0, clMaroon, 255);
    TmpBuffer.DefineBrosse(bsSolid, clRed, 192);
    TmpBuffer.TraceRectangle(MakeTPoint2Df(-DC, -DC), MakeTPoint2Df(DC, DC));
    TmpBuffer.RestoreBrosse();
    TmpBuffer.RestoreCrayon();
  end;

begin
  if (not FDoDraw) then Exit;
  // lbMessages.Caption := IIF(FRedessinInProcess, 'REDESS', 'DONE');
  //if (FRedessinInProcess) then exit;
  // Armement du sémaphore
  FRedessinInProcess := True;
  //if (not FDoDraw) then Exit;
  AfficherMessage(Format('%s.Redessin()', [ClassName]));
  try

    TmpBuffer := TCrossSectionDrawingContext.Create(Vue.Width, Vue.Height, BGRA(Red(FBackGroundColor), Green(FBackGroundColor), Blue(FBackGroundColor), 255));
    try
      TmpBuffer.SetCrossSection(FMyCrossSection);
      TmpBuffer.SetTexturesPolygones(FTexturesPolygones, FTexturesAreOK);
      TmpBuffer.SetBounds(FRXMini, FRYMini, FRXMaxi, FRYMaxi);

      // début conventionnel
      TmpBuffer.BeginDrawing();
      //************************


      // dessin de la section
      DrawSectionTransversale(TmpBuffer);
      // le dessin du réticule
      TmpBuffer.DefineCrayon(psSolid, 0, FAxisColor, 192);
      TmpBuffer.TraceVers(FRXMini, 0.00, false);
      TmpBuffer.TraceVers(FRXMaxi, 0.00, true);
      TmpBuffer.TraceVers(0.00, FRYMini, false);
      TmpBuffer.TraceVers(0.00, FRYMaxi, true);
      DrawViseesRayonnantes();
      // fin conventionnelle
      TmpBuffer.EndDrawing();
    except

    end;
    TmpBuffer.Draw(Vue.Canvas, 0, 0, True);
  finally
    FreeAndNil(TmpBuffer);//TmpBuffer.Free;
    FRedessinInProcess := False; // libération du sémaphore
    //lbMessages.Caption := IIF(FRedessinInProcess, 'REDESS', 'DONE');
    // ne pas mettre cette ligne avant la libération du sémaphore
    Application.ProcessMessages;
  end;
  // on dessine ici les objets temporaires
  try
//    DrawTmpObj_FreeHandPoints;   // dessin des points main levée
//    DrawTmpObj_CourbeProvisoire;
  except
  end;
end;

procedure TCadreSectionTransversale.SetModeTravail(const MT: TCrossSectionModeTravail; const QType: byte);
  procedure QSetPen(const QPM: TPenMode; const QPS: TPenStyle; const QPW: integer; const QPC: TColor);
    begin
      Vue.Canvas.Pen.Mode := QPM;
      Vue.Canvas.Pen.Style:= QPS;
      Vue.Canvas.Pen.Color:= QPC;
      Vue.Canvas.Pen.Width:= QPW;
    end;
    procedure Rearmer();
    begin
      FUnObjetEstSelectionne     := False;
      FVertexModified            := False;
      FAttenduSecondPoint        := False;
      FDoAddVertexCourbePolygone := False;
      FDoMovePoint               := false;
      FMovingPoint               := false;
      QSetPen(pmCopy, psSolid, 0, clGray);

      // reset éléments sélectionnés
      FCurrentCurveIdx           := -1;
      FCurrentPolygonIdx         := -1;
      FCurrentSimpleLigneIdx     := -1;
      FCurrentTexteIdx           := -1;
    end;
var
  EWE: String;
  WU: Integer;
begin
  FModeTravail := MT;
  case FModeTravail of
    mtNONE:
      begin
        FCrossSectionModeSelection := mseNONE;
        Rearmer;

        lbMessages.Caption         := '';
        PreparerSubButtons(mtNONE);
        FCurrentNatureCourbe   := nocDEFAULT;
        FCurrentNaturePolygone := nopDEFAULT;
        FCurrentNatureLigne    := nolDEFAULT;
        FCurrentNatureTexte    := notTEXTE1;
        EWE := '---';  WU  := 0;
      end;
    mtDRAW_COURBE:
      begin
        FCrossSectionPolyProvisoire.ClearVertex();
        FCurrentNatureCourbe := TNatureObjetCourbe(QType);
        EWE := 'Courbe';  WU  := Ord(FCurrentNatureCourbe);
      end;
    mtDRAW_POLYGONE:
      begin
        FCrossSectionPolyProvisoire.ClearVertex();
        FCurrentNaturePolygone := TNatureObjetPolygone(QType);
        EWE := 'Polygone';  WU  := Ord(FCurrentNaturePolygone);
      end;
    mtLIGNE_PREMIER_POINT:
      begin
        FAttenduSecondPoint := True;
        QSetPen(pmCopy, psSolid, 0, clGray);
        lbMessages.Caption  := 'Ligne: Premier coin' ;
        FCurrentNatureLigne := TNatureObjetLigne(QType);
        EWE := 'Ligne';  WU  := Ord(FCurrentNatureLigne);
      end;
    mtLIGNE_SECOND_POINT:
      begin
        QSetPen(pmNotXor, psSolid, 0, clGray);
        lbMessages.Caption  := 'Ligne: Deuxième coin';
        FCurrentNatureLigne := TNatureObjetLigne(QType);
        EWE := 'Ligne';  WU  := Ord(FCurrentNatureLigne);
      end;
    mtDRAW_TEXTE:
      begin
        lbMessages.Caption  := 'Texte: Position';
        FCurrentNatureTexte := TNatureObjetTexte(QType);
        EWE := 'Texte';  WU  := Ord(FCurrentNatureTexte);
      end;

  end;
  lbNatureObjets.caption := Format('%s: %d', [EWE, WU]);
  AfficherModeTravail(MT);
end;
procedure TCadreSectionTransversale.AfficherModeTravail(const MT: TCrossSectionModeTravail);
begin
  lbModeTravail.Caption     := ChooseString(Ord(MT), [
                                             'mtNONE',
                                             'mtDRAW_COURBE',
                                             'mtDRAW_POLYGONE',
                                             'mtDRAW_LINE',
                                             'mtDRAW_TEXTE',
                                             'mtSELECT_LIGNE',
                                             'mtSELECT_COURBE',
                                             'mtSELECT_POLYGONE',
                                             'mtSELECT_TEXTE',
                                             'mtLIGNE_PREMIER_POINT',
                                             'mtLIGNE_SECOND_POINT',
                                             'mtPICK_POS_TEXTE',
                                             'mtCOURBE_EDIT_SOMMET',
                                             'mtCOURBE_MOVE_SOMMET',
                                             'mtPOLYGON_EDIT_SOMMET',
                                             'mtPOLYGON_MOVE_SOMMET'
                                            ]);
end;

procedure TCadreSectionTransversale.SetSectionTransversale(const ST: TCrossSection);
begin
  FMyCrossSection := ST;
end;

procedure TCadreSectionTransversale.VuePaint(Sender: TObject);
begin
  Redessin();
end;

// dessin de la courbe provisoire
procedure TCadreSectionTransversale.DessinerCourbeProvisoire(const Cnv: TCanvas);
var
  i: integer;
  dx, dy: double;
  WU: Boolean;
  V: TCrossSectionVertexCourbe;
begin
  WU := (FModeTravail = mtDRAW_COURBE) OR
        (FModeTravail = mtDRAW_POLYGONE);
  if (WU AND FDoAddVertexCourbePolygone) then
  begin
    V := FCrossSectionPolyProvisoire.GetVertex(0);

    Cnv.Pen.Color   := clGray;
    Cnv.Brush.Color := clBlue;
    Cnv.Brush.Style := bsSolid;
    VolatileTraceVers(Cnv, V.Position.X, V.Position.Y, False);
    for i:= 1 to FCrossSectionPolyProvisoire.GetNbVertex() - 1 do
    begin
      V := FCrossSectionPolyProvisoire.GetVertex(i);
      VolatileTraceVers(Cnv, V.Position.X, V.Position.Y, True);
    end;
  end;
end;

procedure TCadreSectionTransversale.VolatileTraceVers(const Cnv: TCanvas; const XX, YY: Double; const Drawn: boolean);
var
  PM: TPoint2Df;
  PP: TPoint;
begin
  PM := MakeTPoint2Df(XX, YY);
  PP := GetCoordsPlan(PM);
  if (Drawn) then Cnv.LineTo(PP.X, PP.Y)
             else Cnv.MoveTo(PP.X, PP.Y);
end;

procedure TCadreSectionTransversale.VueClick(Sender: TObject);
begin

end;

procedure TCadreSectionTransversale.VueDblClick(Sender: TObject);
begin
  if (not FDoDraw) then Exit;

  // réinit crayons
  Vue.Canvas.Pen.Mode   := pmCopy;
  Vue.Canvas.Pen.Style  := psSolid;
  case FModeTravail of
    mtDRAW_COURBE,
    mtDRAW_POLYGONE:
      begin
        if (FDoAddVertexCourbePolygone) then acValidateCourbePolygone.Execute;
      end;
    mtDRAW_TEXTE:
      ;
    mtSELECT_COURBE:
      begin
        //QRechercheEtTraceCourbe(q);
      end;
    mtSELECT_LIGNE:
      begin
        //QRechercheEtTraceLigne(q);
      end;
    mtSELECT_POLYGONE:
      begin
        //QRechercheEtTracePolygone(q);
      end;
    mtSELECT_TEXTE:
      begin
      end;
  end;
end;

procedure TCadreSectionTransversale.VueMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure QSetPenMode(const QM: TPenMode; const QC: TColor);
    begin
      Vue.Canvas.Pen.Mode:= QM;
      Vue.Canvas.Pen.Color:= QC;
    end;
var
  WU: String;

  MyPolygone: TCrossSectionPolygone;
  FCurrVtxCurveProv, FCurrVtxPolyProv: TCrossSectionVertexCourbe;
  QCourbe: TCrossSectionCourbe;
begin
  // réassignation du pop-up par défaut
  self.PopupMenu := PopUpGeneral;
  case FModeTravail of
    mtNONE:
      begin
        ;
      end;
    // ligne par pointage de deux points
    mtLIGNE_PREMIER_POINT:
       begin
         FZC1 := FMyPos;
         FCurrentSimpleLigne.Extr1 := FZC1;
         SetModeTravail(mtLIGNE_SECOND_POINT, Ord(FCurrentNatureLigne));
       end;
    mtLIGNE_SECOND_POINT:
       begin
         FZC2 := FMyPos;
         FCurrentSimpleLigne.Extr2 := FZC2;
         FMyCrossSection.addSimpleLigne(FCurrentSimpleLigne);
         SetModeTravail(mtLIGNE_PREMIER_POINT, Ord(FCurrentNatureLigne));     //SetModeTravail(mtNONE);
         Vue.Invalidate; // RedessinEcran(false);
       end;

    mtPICK_POS_TEXTE:
       begin
         FZC1 := FMyPos;
         WU := '';
         if (InputQuery('Texte', 'Texte', WU)) then
         begin
           FMyCrossSection.addTexteByValues(FCurrentNatureTexte, 0, FZC1.X, FZC1.Y, WU);
         end;
         SetModeTravail(mtNONE, Ord(FCurrentNatureTexte));     //SetModeTravail(mtNONE);
         Vue.Invalidate; // RedessinEcran(false);
       end;

    // courbes
    mtSELECT_COURBE: //mtCHOIX_COURBE:
      begin
        FCurrentCurveIdx := FMyCrossSection.FindIdxNearCourbeToXY(FMyPos);
        if (FCurrentCurveIdx >= 0) then
        begin
          ListerLesObjets(csloCOURBES, FCurrentCurveIdx);
          FCrossSectionPolyProvisoire.LoadCourbe(FMyCrossSection.getCourbe(FCurrentCurveIdx), false);
          SetModeTravail(mtCOURBE_EDIT_SOMMET, Ord(FCurrentNatureCourbe));
          vue.Invalidate;
        end;
      end;
    mtCOURBE_EDIT_SOMMET:
      begin
        FNoVertexCourbeProvisoire := FCrossSectionPolyProvisoire.GetIDVertexByXY(FMyPos.X, FMyPos.Y);
        if (FNoVertexCourbeProvisoire > -1) then
        begin
          FZC1     := FMyPos;
          SetModeTravail(mtCOURBE_MOVE_SOMMET, Ord(FCurrentNatureCourbe));
        end;
      end;
    mtCOURBE_MOVE_SOMMET:
      begin
        FZC2 := FMyPos;
        FCurrVtxCurveProv := FCrossSectionPolyProvisoire.GetVertex(FNoVertexCourbeProvisoire);
        FCurrVtxCurveProv.Position.X += (FZC2.X - FZC1.X);
        FCurrVtxCurveProv.Position.Y += (FZC2.Y - FZC1.Y);
        FCrossSectionPolyProvisoire.PutVertex(FNoVertexCourbeProvisoire, FCurrVtxCurveProv);
        if (FCrossSectionPolyProvisoire.GenererCourbe(True, QCourbe)) then FMyCrossSection.PutCourbe(FCurrentCurveIdx, QCourbe);
        Vue.Invalidate; // RedessinEcran(false);
        SetModeTravail(mtNONE, 0);            // indispensable pour réarmer le contexte
      end;
    // polygones
    mtSELECT_POLYGONE:
      begin
        FCurrentPolygonIdx := FMyCrossSection.FindIdxNearPolygoneToXY(FMyPos);
        if (FCurrentPolygonIdx >= 0) then
        begin
          ListerLesObjets(csloPOLYGONES, FCurrentPolygonIdx);
          FCrossSectionPolyProvisoire.LoadPolygone(FMyCrossSection.getPolygone(FCurrentPolygonIdx));
          SetModeTravail(mtPOLYGON_EDIT_SOMMET, Ord(FCurrentNaturePolygone));
          Vue.Invalidate; // Une coupe comporte peu de graphismes, on peut se permettre un regen complet de la vue
        end;
      end;
    mtPOLYGON_EDIT_SOMMET:
      begin
        FNoVertexPolygoneProvisoire := FCrossSectionPolyProvisoire.GetIDVertexByXY(FMyPos.X, FMyPos.Y);
        if (FNoVertexPolygoneProvisoire > -1) then
        begin
          FZC1              := FMyPos;
          SetModeTravail(mtPOLYGON_MOVE_SOMMET, ord(FCurrentNaturePolygone));
        end;
      end;
    mtPOLYGON_MOVE_SOMMET:
      begin
        FZC2 := FMyPos;
        FCurrVtxPolyProv := FCrossSectionPolyProvisoire.GetVertex(FNoVertexPolygoneProvisoire);
        FCurrVtxPolyProv.Position.X += (FZC2.X - FZC1.X);
        FCurrVtxPolyProv.Position.Y += (FZC2.Y - FZC1.Y);
        FCrossSectionPolyProvisoire.PutVertex(FNoVertexPolygoneProvisoire, FCurrVtxPolyProv);

        if (FCrossSectionPolyProvisoire.GenererPolygone(MyPolygone)) then
        begin
          FMyCrossSection.putPolygone(FCurrentPolygonIdx, MyPolygone);
        end;

        Vue.Invalidate; // redessin écran
        SetModeTravail(mtNONE, 0);            // indispensable pour réarmer le contexte
      end;
    //--------------------------------
    mtSELECT_LIGNE: //mtCHOIX_LIGNE:
      begin
        FCurrentSimpleLigneIdx := FMyCrossSection.FindIdxNearSimpleLigneToXY(FMyPos);
        if (FCurrentSimpleLigneIdx >= 0) then
        begin
          Vue.Invalidate;
        end;
      end;
    mtSELECT_TEXTE: //mtCHOIX_TEXTE:
      begin
        FCurrentTexteIdx := FMyCrossSection.FindIdxNearTexteToXY(FMyPos);
        if (FCurrentTexteIdx >= 0) then
        begin
          Vue.Invalidate;
        end;
      end;
    //-----------------------------------
    // dessin des objets utilisant l'objet TCourbePolygoneProvisoire
    mtDRAW_COURBE,
    mtDRAW_POLYGONE:
      begin
         // réassignation du pop-up
         //self.PopupMenu := mnuCourbe;
         if (Not FDoAddVertexCourbePolygone) then
         begin
           FZC1 := FMyPos;
           FCrossSectionPolyProvisoire.ClearVertex;
           // premier vertex

           FCrossSectionPolyProvisoire.AddVertexByXY(FZC1.X, FZC1.Y);
           // on passe en mode ajout de points
           FDoAddVertexCourbePolygone := True;
         end
         else  // C'est le premier point
         begin
           // réassignation du pop-up
           self.PopupMenu := popupCourbePoly;
           FZC1 := FMyPos;
           FCrossSectionPolyProvisoire.AddVertexByXY(FZC1.X, FZC1.Y);
           // dessin de la courbe
           DessinerCourbeProvisoire(Vue.Canvas);
         end;
      end;
  end;
end;

//******************************************************************************

end.

