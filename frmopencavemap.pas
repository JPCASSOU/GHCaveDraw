unit frmOpenCaveMap;
{$INCLUDE CompilationParameters.inc}
{$IFDEF TIDBASEPOINT_AS_TEXT}
   {$ERROR: La BDD doit être réorganisée dans ce mode}
{$ENDIF TIDBASEPOINT_AS_TEXT}


interface
uses
  GHCD_Types
  {$IFDEF LANGUAGE_FRENCH}
  ,UnitMessages_fr,
  {$ENDIF}
  GeneralFunctions,
  unitOpenCaveMap,
  math,
  CallDialogsGHCaveDraw,
  md5,
  UnitDocDessin,
  UnitCenterlinesBasePoints,
  ZConnection, ZDataset, //mysql50conn, sqldb,
  LCLType,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList, Menus, Buttons, ExtCtrls, ComCtrls, Types;

type

  { TdlgOpenCaveMap }

  TdlgOpenCaveMap = class(TForm)
    acConnecter: TAction;
    acUpdateTableSimplesLignes: TAction;
    acUpdateTableTextes: TAction;
    acUpdateTableSymboles: TAction;
    acUpdateTablePolygones: TAction;
    acUpdateTablePolylines: TAction;
    acUpdateTableCourbes: TAction;
    acUpdateTableScraps: TAction;
    acUpdateTableGroupes: TAction;
    acUpdateTableBasePoints: TAction;
    ActionList1: TActionList;
    BitBtn2: TBitBtn;
    Button1: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button15: TButton;
    Button2: TButton;
    Button3: TButton;
    chkViderToutesTables: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    editNumCavite: TCurrencyEdit;
    editPassword: TEdit;
    editLogin: TEdit;
    hcColonnes: THeaderControl;
    Label1: TLabel;
    Label2: TLabel;
    lbUserMail: TStaticText;
    lbUserDateInscription: TStaticText;
    lsbCavitesInDataBase: TListBox;
    lbConnexionOK: TStaticText;
    editCommentairesModifs: TMemo;
    Panel1: TPanel;
    pnlBDDActions: TPanel;
    pnlUtilisateur: TPanel;
    lbUserName: TStaticText;
    lbUserLogin: TStaticText;
    lbUserID: TStaticText;
    lbDBMessages: TStaticText;
    procedure acConnecterExecute(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure hcColonnesSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbCavitesInDataBaseClick(Sender: TObject);
    procedure lsbCavitesInDataBaseDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure Panel1Click(Sender: TObject);
  private
    { private declarations }
    FMyDocDessin: TDocumentDessin;

    //FTestMyDocDessin: TDocumentDessin; // pour tests
    FTransactionEnCours: boolean;
    FConnexionOpenCavemap: TConnexionOpenCaveMap;
    procedure ListerLesCavitesInBase(const Qidx: integer);
  public
    { public declarations }
    function Initialiser(const FD: TDocumentDessin): boolean;
    function GetDocDessinExtraitFromBDD(): TDocumentDessin;

  end;

var
  dlgOpenCaveMap: TdlgOpenCaveMap;

implementation
uses
  DGCDummyUnit;
const
  CAPTION_CONNEXION_BDD   = 'Connexion BDD';
  CAPTION_DECONNEXION_BDD = 'Déconnexion BDD';

{$R *.lfm}

{ TdlgOpenCaveMap }

procedure TdlgOpenCaveMap.ListerLesCavitesInBase(const Qidx: integer);
var
  QIdCavite: LongInt;
  LS: TStringList;
  i, n: Integer;
  EWE: TCaviteInBDD;
  QUser1, QUser2: TUtilisateurOCM;
begin
  if (not FConnexionOpenCavemap.Connected) then Exit;
  FConnexionOpenCavemap.ExtractListeCavitesFromBDD();

  lsbCavitesInDataBase.Clear;
  n := FConnexionOpenCavemap.GetNbCavitesInListe();
  if (n = 0) then Exit;
  for i := 0 to n - 1 do
  begin
    EWE := FConnexionOpenCavemap.GetCaviteFromLstBDD(i);
    FConnexionOpenCavemap.ExtractUserDirectlyFromBDD(EWE.IDUserCreateur, QUser1);
    FConnexionOpenCavemap.ExtractUserDirectlyFromBDD(EWE.IDLastUser, QUser2);
    lsbCavitesInDataBase.Items.add(Format('%d: EPSG:%d %s - %d gr, %d sc, %d co, %d pg, %d sy, %d tx - Cree par %d: %s - Modif par %d: %s - ',
                                          [EWE.IDCavite, EWE.CodeEPSG, EWE.NomCavite, EWE.NbGroupes, EWE.NbScraps, EWE.NbCourbes, EWE.NbPolygones, EWE.NbSymboles, EWE.NbTextes,
                                           QUser1.IDUtilisateur, QUser1.NomUtilisateur,
                                           QUser2.IDUtilisateur, QUser2.NomUtilisateur
                                          ]));
  end;
  if      (QIdx = -1) then lsbCavitesInDataBase.ItemIndex := 0
  else if (QIdx = -2) then lsbCavitesInDataBase.ItemIndex := lsbCavitesInDataBase.Count - 1
  else                     lsbCavitesInDataBase.ItemIndex := QIdx;
end;



(*


function TdlgOpenCaveMap.BDDExtractPolygoneOfCavite(const QIDCavite: integer; const QIdxPolygone: integer; out MyPolygone: TPolygone): boolean;
var
  WU: String;
  Nb: Integer;
  QIdPolygone: Int64;
  QNbVertex, QIdxVertex: LongInt;
  MyVertex : TVertexPolygon;
begin
  Result := false;
  AfficherMessageErreur(Format('%s.BDDExtractPolygoneOfCavite: %d', [ClassName, QIDCavite, QIdxPolygone]));
  WU := format('SELECT * FROM %s WHERE (%s div %d = %d) and (%s mod %d = %d) ORDER BY %s ASC;',
               [TABLE_POLYGONES,
                TBL_POLYGONES_COL_IDPoly, NB_MAX_OBJ_PAR_CAVITE, QIDCavite,
                TBL_POLYGONES_COL_IDPoly, NB_MAX_OBJ_PAR_CAVITE, QIdxPolygone,
                TBL_POLYGONES_COL_IDPoly]);
  AfficherMessageErreur(WU);
  ZQuery1.SQL.Clear;
  ZQuery1.SQL.Text := WU;
  Nb := 0;
  try
    ZQuery1.Open;
    while (not ZQuery1.EOF) do
    begin
      // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
      QIdPolygone                   := StrToInt64Def(ZQuery1.Fields[0].AsString, 0);
      MyPolygone.IDGroupe           := StrToInt64Def(ZQuery1.Fields[1].AsString, 0) mod MULT_IDX_CAVITE;
      MyPolygone.IDStylePolygone    := TNatureObjetPolygone(StrToIntDef(ZQuery1.Fields[2].AsString, 0));                            // IDGroupe
      QNbVertex                     := StrToIntDef(ZQuery1.Fields[3].AsString, 0);
      AfficherMessageErreur(Format('Polygone %d; Groupe: %d; Style: %d; NbVertex = %d',
                                   [QIdPolygone,
                                    MyPolygone.IDGroupe, MyPolygone.IDStylePolygone,
                                    QNbVertex]));
      ZQuery1.Next;
      Inc(Nb);
    end;
  finally
    ZQuery1.Close;
  end;
  if (Nb = 0) then Exit;
  WU := format('SELECT * FROM %s WHERE (%s = %d) ORDER BY %s ASC;',
               [TABLE_VERTEX_POLYGONES,
                TBL_VERTEX_SCRAPS_POLY_COL_IDPoly, QIdPolygone,
                'IdxVertex']);

  AfficherMessageErreur(WU);
  ZQuery1.SQL.Clear;
  ZQuery1.SQL.Text := WU;
  Nb := 0;
  try
    ZQuery1.Open;
    while (not ZQuery1.EOF) do
    begin
      // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
      QIdPolygone := StrToInt64Def(ZQuery1.Fields[0].AsString, 0);
      QIdxVertex  := StrToIntDef(ZQuery1.Fields[1].AsString, 0);
      MyVertex.IDStation := StrToInt64Def(ZQuery1.Fields[2].AsString, 0);
      MyVertex.Offset.X  := ConvertirEnNombreReel(ZQuery1.Fields[3].AsString, 0.00);
      MyVertex.Offset.Y  := ConvertirEnNombreReel(ZQuery1.Fields[4].AsString, 0.00);

      AfficherMessageErreur(Format('-- Poly %d; Vertex: %d; Basepoint: %d; Offset = (%.3f, %.3f)',
                                   [QIdPolygone, QIdxVertex,
                                    MyVertex.IDStation, MyVertex.Offset.X, MyVertex.Offset.Y
                                   ]));
      ZQuery1.Next;
      Inc(Nb);
    end;
  finally
    ZQuery1.Close;
  end;
end;

//*)



//******************************************************************************


procedure TdlgOpenCaveMap.acConnecterExecute(Sender: TObject);
var
  U: TUtilisateurOCM;
begin

  lsbCavitesInDataBase.Clear;
  try
    if (FConnexionOpenCavemap.Connected) then FConnexionOpenCavemap.CloseConnexion()
                                         else FConnexionOpenCavemap.OpenConnexion();

  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
    end;
  end;
  if (FConnexionOpenCavemap.Connected) then
  begin
    // checker l'utilisateur
    if (FConnexionOpenCavemap.CheckUser(Trim(editLogin.Text), Trim(editPassword.Text))) then
    begin
      U := FConnexionOpenCavemap.GetCurrentUser();
      lbUserID.Caption    := Format('%d', [U.IDUtilisateur]);
      lbUserName.Caption  := U.NomUtilisateur;
      lbUserLogin.Caption := U.Login;
      lbUserDateInscription.Caption := DateTimeToStr(U.DateInscription);
      lbUserMail.Caption  := U.Email;
      acConnecter.Caption := CAPTION_DECONNEXION_BDD;
      lbConnexionOK.Color := clGreen;
      ListerLesCavitesInBase(0);
      pnlBDDActions.Enabled := true;
    end
    else
    begin
      ShowMessage('Login ou password invalide');
      FConnexionOpenCavemap.CloseConnexion();
    end;
  end
  else
  begin
    pnlBDDActions.Enabled := false;
    FConnexionOpenCavemap.CloseConnexion();
    acConnecter.Caption := CAPTION_CONNEXION_BDD;
    lbConnexionOK.Color  := clRed;
    ShowMessage('Déconnecté du serveur');
    lsbCavitesInDataBase.Clear;
  end;
end;





procedure TdlgOpenCaveMap.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
    FConnexionOpenCavemap.Finalise();
  finally
    FConnexionOpenCavemap.Free;
    //FTestMyDocDessin.Free;
  end;
end;

procedure TdlgOpenCaveMap.FormCreate(Sender: TObject);
begin

end;

procedure TdlgOpenCaveMap.FormShow(Sender: TObject);
begin
  //FTestMyDocDessin := TDocumentDessin.Create;
  //FTestMyDocDessin.Initialize();
  FConnexionOpenCavemap := TConnexionOpenCaveMap.Create();

  try
    //FConnexionOpenCavemap.Initialise(FMyDocDessin);
    FConnexionOpenCavemap.Initialise(FMyDocDessin, lbDBMessages);
  except
  end;
end;

function TdlgOpenCaveMap.GetDocDessinExtraitFromBDD: TDocumentDessin;
begin
  Result := FMyDocDessin;
end;

procedure TdlgOpenCaveMap.hcColonnesSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbCavitesInDataBase.Invalidate;
end;

procedure TdlgOpenCaveMap.Button11Click(Sender: TObject);
var
  WU: Boolean;
  EWE: LongInt;
  CodeEPSG: TCodeEPSG;
  Cav: TCaviteInBDD;
begin
  if (not QuestionOuiNon('Ceci remplacera la cavité déjà en base - Continuer')) then Exit;
  EWE := 1 + lsbCavitesInDataBase.ItemIndex;//editNumCavite.AsInteger;
  pnlBDDActions.Enabled := false;
  FConnexionOpenCavemap.ExtractCaviteFromLstBDD(EWE, Cav);
  try
    WU := chkViderToutesTables.Checked;
    FConnexionOpenCavemap.DispMsgInLbl('Ecrasement des données existantes ...');
    FConnexionOpenCavemap.DetruireCaviteInDB(EWE);
    CodeEPSG := Cav.CodeEPSG;//FMyDocDessin.GetCodeEPSG();
    FConnexionOpenCavemap.ExportTableBasePointsToDatabase(EWE, WU, CodeEPSG);
    FConnexionOpenCavemap.ExportTableGroupesToDatabase(EWE, WU);
    FConnexionOpenCavemap.ExportTableScrapsToDatabase(EWE, WU);
    FConnexionOpenCavemap.ExportTableCourbesToDatabase(EWE, WU);
    FConnexionOpenCavemap.ExportTablePolygonesToDatabase(EWE, WU);
    FConnexionOpenCavemap.ExportTablePolylinesToDatabase(EWE, WU);
    FConnexionOpenCavemap.ExportTableSimplesLignesToDatabase(EWE, WU);
    FConnexionOpenCavemap.ExportTableSymbolesToDatabase(EWE, WU);
    FConnexionOpenCavemap.ExportTableTextesToDatabase(EWE, WU);

    FConnexionOpenCavemap.AddGroupeModifications(Cav.IDCavite, 'Updated ' + Cav.NomCavite);
    FConnexionOpenCavemap.DispMsgInLbl('Exportation terminée');

    ListerLesCavitesInBase(0);
  finally
    pnlBDDActions.Enabled := true;
  end;
end;

procedure TdlgOpenCaveMap.Button12Click(Sender: TObject);
var
  EWE: LongInt;
  MyCavite: TCaviteInBDD;
begin
  if (not QuestionOuiNon('Ceci effacera le dessin en cours - Continuer')) then Exit;
  pnlBDDActions.enabled := false;
  try   EWE := editNumCavite.AsInteger;

    if (not FConnexionOpenCavemap.ExtractCaviteFromLstBDD(EWE, MyCavite)) then
    begin
      ShowMessage('Cavité non trouvée');
      pnlBDDActions.Enabled := True;
      Exit;
    end;
    FMyDocDessin.ResetAll();

    ShowMessageFmt('Cavité: %s - EPSG:%d', [MyCavite.NomCavite, MyCavite.CodeEPSG]);
    FMyDocDessin.SetCodeEPSG(MyCavite.CodeEPSG);
    FMyDocDessin.SetDocumentName(StringReplace(MyCavite.NomCavite, ' ', '_', [rfIgnoreCase, rfReplaceAll]));
    //Exit;
    if (FConnexionOpenCavemap.BDDExtractBasePointsOfCavite(MyCavite.IDCavite, MyCavite.CodeEPSG)) then
    begin
      FConnexionOpenCavemap.BDDExtractAllScrapsOfCavite(MyCavite.IDCavite);
      FConnexionOpenCavemap.BDDExtractAllCourbesOfCavite(MyCavite.IDCavite);
      FConnexionOpenCavemap.BDDExtractAllPolygonesOfCavite(MyCavite.IDCavite);
      FConnexionOpenCavemap.BDDExtractAllPolylinesOfCavite(MyCavite.IDCavite);
      FConnexionOpenCavemap.BDDExtractAllSimpleLinesOfCavite(MyCavite.IDCavite);
      FConnexionOpenCavemap.BDDExtractAllSymbolesOfCavite(MyCavite.IDCavite);
      FConnexionOpenCavemap.BDDExtractAllTextesOfCavite(MyCavite.IDCavite);
      FConnexionOpenCavemap.BDDExtractAllGroupesOfCavite(MyCavite.IDCavite);
    end;
    FMyDocDessin.CalcBoundingBoxAllGroupes();
    FMyDocDessin.SetMiniEtMaxi();
    ListerLesCavitesInBase(0);
  finally
    pnlBDDActions.enabled := true;
  end;
end;

procedure TdlgOpenCaveMap.Button13Click(Sender: TObject);
var
  C: TCaviteInBDD;
  n: Integer;
begin
  C.NomCavite := FMyDocDessin.GetDocumentName();
  C.IDCavite  := -1;
  C.CodeEPSG  := FMyDocDessin.GetCodeEPSG();
  if (FConnexionOpenCavemap.AddCaviteInBDD(C)) then
  begin
    ListerLesCavitesInBase(-2);
    n := FConnexionOpenCavemap.GetNbCavitesInListe();
    C := FConnexionOpenCavemap.GetCaviteFromLstBDD(n - 1);
    FConnexionOpenCavemap.AddGroupeModifications(C.IDCavite, editCommentairesModifs.Text);
  end;
end;
procedure TdlgOpenCaveMap.Button15Click(Sender: TObject);
var
  EWE: LongInt;
  C: TCaviteInBDD;
  WU: String;
begin
  pnlBDDActions.enabled := false;
  try
    EWE := editNumCavite.AsInteger;
    if (not QuestionOuiNon('Ceci détruira les données de la cavité dans la base - Continuer')) then Exit;
    FConnexionOpenCavemap.DetruireCaviteInDB(EWE);
    C := FConnexionOpenCavemap.GetCaviteFromLstBDD(EWE);
    WU := Format('Effacement des données de la cavité %d: %s', [C.IDCavite, C.NomCavite]);
    FConnexionOpenCavemap.AddGroupeModifications(EWE, WU);
    ListerLesCavitesInBase(EWE);

  finally
    pnlBDDActions.enabled := true;
  end;
end;

procedure TdlgOpenCaveMap.Button2Click(Sender: TObject);
begin
  if (not QuestionOuiNon('Ceci videra les tables des objets graphiques - Continuer')) then Exit;
  FConnexionOpenCavemap.ViderTablesObjetsGraphiques(false);
  ListerLesCavitesInBase(0);
end;

procedure TdlgOpenCaveMap.Button3Click(Sender: TObject);
begin
  try
    edit2.Text := FConnexionOpenCavemap.GetScalarStrSQLResult(edit1.Text);

  except
    showmessage('La requête doit retourner un scalaire (texte ou nombre)');
  end;
end;

// Lors d'une MAJ de la base, on écrase la version précédente
// TODO: Voir avec Update si on peut modifier une colonne de version
function TdlgOpenCaveMap.Initialiser(const FD: TDocumentDessin): boolean;
begin
  FTransactionEnCours := false;
  // headers
  //hcColonnes.Sections.Clear;
  //hcColonnes.Sections.Add;


  Result := false;
  try
    FMyDocDessin := FD;
    acConnecter.Caption := CAPTION_CONNEXION_BDD;
    result := true;
  except
  end;
end;

procedure TdlgOpenCaveMap.lsbCavitesInDataBaseClick(Sender: TObject);
begin
  if (lsbCavitesInDataBase.Count > 0) then editNumCavite.AsInteger := lsbCavitesInDataBase.ItemIndex + 1;
end;

procedure TdlgOpenCaveMap.lsbCavitesInDataBaseDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
  mg  = 1;
var
  EWE: TCaviteInBDD;
  procedure DessineFiletColonne(const TB: integer);
  begin
    lsbCavitesInDataBase.Canvas.Pen.Color   := clSilver; // pour les filets
    lsbCavitesInDataBase.Canvas.MoveTo(TB, ARect.Top);
    lsbCavitesInDataBase.Canvas.LineTo(TB, ARect.Bottom);
  end;
  procedure DessineRectCouleur(const TB: integer; const W: integer;const Coul: TColor; const bg: TColor);
  var
    QR: TRect;
  begin
    lsbCavitesInDataBase.Canvas.Pen.Style   := psSolid;
    lsbCavitesInDataBase.Canvas.Brush.Style := bsSolid;
    lsbCavitesInDataBase.Canvas.Brush.Color := Coul;
    lsbCavitesInDataBase.Canvas.Pen.Color   := clBlack;
    QR.Left   := ARect.Left   + TB + 3;
    QR.Top    := ARect.Top    + 1;
    QR.Bottom := ARect.Bottom - 1;
    QR.Right  := QR.Left + W - 4;
    lsbCavitesInDataBase.Canvas.Rectangle(QR);
    lsbCavitesInDataBase.Canvas.Brush.Color := bg;
  end;
  procedure DessineItem(const bg,tc: TColor);
  VAR
    HS: THeaderSection;
    QR: TRect;
  begin
    lsbCavitesInDataBase.Canvas.Brush.Color := bg;
    lsbCavitesInDataBase.Canvas.Font.Color  := tc;

    lsbCavitesInDataBase.Canvas.FillRect(ARect);
    HS := hcColonnes.Sections.Items[0];  // ID cavité
    lsbCavitesInDataBase.Canvas.TextOut(HS.Left + 4, ARect.Top,  Format('%d',[EWE.IDCavite]));
    DessineFiletColonne(HS.Left - Q4);

    HS := hcColonnes.Sections.Items[1];  // EPSG
    lsbCavitesInDataBase.Canvas.TextOut(HS.Left + 4, ARect.Top,  Format('%d',[EWE.CodeEPSG]));
    DessineFiletColonne(HS.Left - Q4);
    HS := hcColonnes.Sections.Items[2];  // nom cavité
    lsbCavitesInDataBase.Canvas.TextOut(HS.Left + 4, ARect.Top,  EWE.NomCavite);
    DessineFiletColonne(HS.Left - Q4);
    HS := hcColonnes.Sections.Items[3];  // Création
    //lsbCavitesInDataBase.Canvas.TextOut(HS.Left + 4, ARect.Top,  EWE.NomCavite);
    DessineFiletColonne(HS.Left - Q4);
    HS := hcColonnes.Sections.Items[4];  // MAJ
    //lsbCavitesInDataBase.Canvas.TextOut(HS.Left + 4, ARect.Top,  EWE.NomCavite);
    DessineFiletColonne(HS.Left - Q4);
    HS := hcColonnes.Sections.Items[5];  // nb groupes
    lsbCavitesInDataBase.Canvas.TextOut(HS.Left + 4, ARect.Top,  Format('%d',[EWE.NbGroupes]));
    DessineFiletColonne(HS.Left - Q4);
    HS := hcColonnes.Sections.Items[6];  // nb scraps
    lsbCavitesInDataBase.Canvas.TextOut(HS.Left + 4, ARect.Top,  Format('%d',[EWE.NbScraps]));
    DessineFiletColonne(HS.Left - Q4);
  end;
begin
  if (FConnexionOpenCavemap.GetNbCavitesInListe() = 0) then exit;
  try
    EWE := FConnexionOpenCavemap.GetCaviteFromLstBDD(Index);
    DessineItem(clWhite, clBlack);
    if (odSelected in state) then
    begin
      lsbCavitesInDataBase.Canvas.brush.color := clBlue;
      DessineItem(clBlue, clWhite);
    end;
  except
    AfficherMessageErreur('Warn: Liste cavités vide');
  end;
end;

procedure TdlgOpenCaveMap.Panel1Click(Sender: TObject);
begin

end;

end.


// La factorisation de ce code, utilisé en deux endroits du projet, est très difficile.
procedure TCadreListeGroupes.lsbGroupesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);

var

  MyGRP: TGroupeEntites;
  WU: TColor;
  procedure DessineCoche(const TB: integer; const IsChecked: boolean; const bg: TColor);
  var
    QR: TRect;
    H: Integer;
  begin
    lsbGroupes.Canvas.Pen.Style   := psSolid;
    lsbGroupes.Canvas.Brush.Style := bsSolid;
    lsbGroupes.Canvas.Pen.Color   := clBlack;

    QR.Left   := ARect.Left   + TB + 2;
    QR.Top    := ARect.Top    + 1;
    QR.Bottom := ARect.Bottom - 1;
    H := QR.Bottom - QR.Top;
    QR.Right  := QR.Left + H;
    lsbGroupes.Canvas.Brush.Color := clWhite;
    if (IsChecked) then lsbGroupes.Canvas.Brush.Color := clRed;
    lsbGroupes.Canvas.Rectangle(QR);
    lsbGroupes.Canvas.Brush.Color := bg;
  end;



begin

end;


