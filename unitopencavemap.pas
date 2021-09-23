unit unitOpenCaveMap;
{$INCLUDE CompilationParameters.inc}
{$IFDEF TIDBASEPOINT_AS_TEXT}
   {$WARNING: La BDD doit être réorganisée dans ce mode}
{$ENDIF TIDBASEPOINT_AS_TEXT}

interface
uses
  GHCD_Types
  {$IFDEF LANGUAGE_FRENCH}
  , UnitMessages_fr
  {$ENDIF}
  , GeneralFunctions
  , UnitListesSimplesWithGeneriques
  , UnitDocDessin
  , UnitCenterlinesBasePoints
  , ConvertisseurJPC
  , Classes, SysUtils, Graphics, math
  , ZConnection, ZDataset, md5
  , StdCtrls
  ;



type

{ TConnexionOpenCaveMap }

 TConnexionOpenCaveMap = class
  private
    FProcDisplayAvancement: TProcDisplayAvancement;
    FMyDocDessin          : TDocumentDessin;
    FTransactionInProcess : boolean;
    FSqlConnexion         : TZConnection;
    FMainSqlQuery         : TZQuery;
    FSecSqlQuery          : TZQuery;
    FIsoleeSqlQuery       : TZQuery;
    // liste des cavités
    FListeCavitesInBDD: TListeSimple<TCaviteInBDD>;
    // code EPSG
    FCodeEPSG: integer;
    // utilisateur courant
    FCurrentUser: TUtilisateurOCM;

    // widget contenant les messages d'information
    FLabelMsg: TStaticText;

    procedure BeginTransaction();
    procedure CancelTransaction();
    procedure CommitTransaction();



    function SendSQLCommand(const Cmd: string): boolean;
    procedure ViderTable(const NomTable: string);
    function  ReprojeterCoordsToUCS(const QLon, QLat: double; out QX, QY: double): boolean;
    function  DeprojeterCoordsToWGS(const QX, QY: double; out QLon, QLat: double): boolean;

    function GetNbElementsInTable(const NomTable: string): Int64;
    function GetNbElementsInTableForCavite(const NomTable: string; const FieldIDObj: string;  const IdxCavite: Int64): Int64;

  public
    function Initialise(const FD: TDocumentDessin; const LB: TStaticText): boolean;
    procedure SetProcDisplayAvancement(const P: TProcDisplayAvancement);
    function Connected: boolean;
    procedure Finalise();
    function OpenConnexion(): boolean;
    function CheckUser(const Login, Pwd: string): boolean;
    procedure CloseConnexion();

    procedure ViderTablesObjetsGraphiques(const PreserveCenterlines: boolean);

    function GetScalarStrSQLResult(const Q: string): string;
    function GetScalarInt32SQLResult(const Q: string): integer;
    function GetScalarDoubleSQLResult(const Q: string): double;
    function GetScalarInt64SQLResult(const Q: string): int64;


    function BDDExtractBasePointsOfCavite(const QIDCavite: integer; const QCodeEPSG: integer): boolean;
    function BDDExtractAllGroupesOfCavite(const QIDCavite: integer): boolean;
    function BDDExtractAllScrapsOfCavite(const QIDCavite: integer): boolean;
    function BDDExtractAllCourbesOfCavite(const QIDCavite: integer): boolean;
    function BDDExtractAllPolygonesOfCavite(const QIDCavite: integer): boolean;
    function BDDExtractAllPolylinesOfCavite(const QIDCavite: integer): boolean;
    function BDDExtractAllSimpleLinesOfCavite(const QIDCavite: integer): boolean;
    function BDDExtractAllSymbolesOfCavite(const QIDCavite: integer): boolean;
    function BDDExtractAllTextesOfCavite(const QIDCavite: integer): boolean;
    function DetruireCaviteInDB(const NumCavite: integer): Integer;
    procedure ExportTableBasePointsToDatabase(const NumCavite: integer; const DoViderTable: boolean; const CodeEPSG: integer);
    procedure ExportTableGroupesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
    procedure ExportTableScrapsToDatabase(const NumCavite: integer; const DoViderTable: boolean);
    procedure ExportTableCourbesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
    procedure ExportTablePolygonesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
    procedure ExportTablePolylinesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
    procedure ExportTableSimplesLignesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
    procedure ExportTableSymbolesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
    procedure ExportTableTextesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
    procedure ExportVertexToSQL(const NumCavite: integer; const TableVertex: string; const V: TVertexPolygon; const QIdxPoly: Int64; const QIdxVtx: integer);
    function  GetNbCavitesInListe(): integer;
    procedure ExtractListeCavitesFromBDD();
    function  GetCaviteFromLstBDD(const Idx: integer): TCaviteInBDD;
    procedure AddCaviteFromBDD(const C: TCaviteInBDD);
    function  ExtractCaviteFromLstBDD(const QIdxCavite: Int64; out QCavite: TCaviteInBDD): boolean;
    // extraction d'enregistrements isolés
    function  ExtractUserDirectlyFromBDD(const QIdx: integer; out QUser: TUtilisateurOCM): boolean;
    function  ExtractCaviteDirectlyFromBDD(const QIdxCavite: Int64; out QCavite: TCaviteInBDD): boolean;



    function  GetCurrentUser(): TUtilisateurOCM;

    function  AddCaviteInBDD(const C: TCaviteInBDD): boolean;
    function  AddGroupeModifications(const IdxCavite: Int64; const Commentaires: string): boolean;
    // nombre d'objets par cavité
    function GetNbCourbesForCavite(const IdxCavite: Int64): integer;
    function GetNbGroupesForCavite(const IdxCavite: Int64): integer;
    function GetNbPolygonesForCavite(const IdxCavite: Int64): integer;
    function GetNbPolylignesForCavite(const IdxCavite: Int64): integer;
    function GetNbScrapsForCavite(const IdxCavite: Int64): integer;
    function GetNbSimplesLignesForCavite(const IdxCavite: Int64): integer;
    function GetNbSymbolesForCavite(const IdxCavite: Int64): integer;
    function GetNbTextesForCavite(const IdxCavite: Int64): integer;

    procedure DispMsgInLbl(const Msg: string);
end;

implementation
uses
  DGCDummyUnit
  , Forms, Dialogs;
{$DEFINE DISP_TO_CONSOLE}
{$UNDEF DISP_TO_CONSOLE}

const
  EPSG_WGS84     = 4326;

const
  TABLE_UTILISATEURS      = 'utilisateurs';
  TABLE_CAVITES           = 'cavites';
  TABLE_BASEPOINTS        = 'basepoints';
  TABLE_GROUPES           = 'groupes';
  TABLE_SCRAPS            = 'scraps';
  TABLE_VERTEX_SCRAPS     = 'vertexscraps';
  TABLE_COURBES           = 'courbes';
  TABLE_ARCS_COURBES      = 'arcscourbes';
  TABLE_POLYGONES         = 'polygones';
  TABLE_VERTEX_POLYGONES  = 'vertexpolygones';
  TABLE_POLYLIGNES        = 'polylignes';
  TABLE_VERTEX_POLYLIGNES = 'vertexpolylignes';
  TABLE_SIMPLES_LIGNES    = 'simpleslignes';
  TABLE_TEXTES            = 'textes';
  TABLE_SYMBOLES          = 'symboles';
  TABLE_GROUPES_MODIFICATIONS = 'groupesmodifications';


  TBL_VERTEX_SCRAPS_POLY_COL_IDPoly   = 'IDPoly';
  TBL_POLYGONES_COL_IDPoly            = 'IDPoly';
  TBL_COURBES_COL_IDCourbe            = 'IDCourbe';
  TBL_ARCS_COL_IDCourbe               = 'IDCourbe';
  TBL_SLIGNES_COL_IDSimpleLigne       = 'IDSimpleLigne';
  TBL_SYMBOLES_COL_IDSymbole          = 'IDSymbole';
  TBL_TEXTES_COL_IDTexte              = 'IDTexte';


{ TConnexionOpenCaveMap }

function CalcIdxBasepointForCavite(const QIdxCavite: integer; const QIDBaseStation: Int64): Int64;
begin
  Result := Sign(QIDBaseStation) * (QIdxCavite * MULT_IDX_CAVITE + abs(QIDBaseStation mod MULT_IDX_CAVITE));
end;
function CalcIdxGroupeForCavite(const QIdxCavite: integer; const QIDGroupe: TIDGroupeEntites): Int64;
begin
  Result := QIdxCavite * NB_MAX_OBJ_PAR_CAVITE + (QIDGroupe mod NB_MAX_OBJ_PAR_CAVITE);
end;
function CalcIdxObjetForCavite(const QIdxCavite: integer; const QIDObjet: integer): Int64;
begin
  Result := QIdxCavite * NB_MAX_OBJ_PAR_CAVITE + QIDObjet;
end;



function TConnexionOpenCaveMap.ReprojeterCoordsToUCS(const QLon, QLat: double; out QX, QY: double): boolean;
var
  P1, P2: TProjUV;
  FC: TConversionSysteme;
begin
  FC := FMyDocDessin.GetConvertisseurCoordonnees();
  P1.setFrom(QLat, QLon);
  P2   := FC.ConversionSyst1ToSyst2EPSG(4326, FCodeEPSG, P1);
  QX := P2.U;
  QY := P2.V;
  Result := True;
end;
function TConnexionOpenCaveMap.DeprojeterCoordsToWGS(const QX, QY: double; out QLon, QLat: double): boolean;
var
  P1, P2: TProjUV;
  FC: TConversionSysteme;
begin
  FC := FMyDocDessin.GetConvertisseurCoordonnees();
  P1.setFrom(QX, QY);
  P2   := FC.ConversionSyst1ToSyst2EPSG(FCodeEPSG, 4326, P1);
  QLon := P2.V;
  QLat := P2.U;
  Result := True;
end;



function TConnexionOpenCaveMap.Initialise(const FD: TDocumentDessin; const LB: TStaticText): boolean;
var
  FC: TConversionSysteme;
begin
  Result := false;
  FC := FMyDocDessin.GetConvertisseurCoordonnees();
  FProcDisplayAvancement := nil;
  FLabelMsg := LB;
  FMyDocDessin := FD;
  // liste des cavités
  FListeCavitesInBDD := TListeSimple<TCaviteInBDD>.Create;
  FListeCavitesInBDD.ClearListe();

  // connexion
  FSqlConnexion    := TZConnection.Create(Application);
  FMainSqlQuery    := TZQuery.Create(Application);
  FSecSqlQuery     := TZQuery.Create(Application);
  FIsoleeSqlQuery  := TZQuery.Create(Application);

  try
    AfficherMessage(Format('%s.Initialise()', [ClassName]));
    FTransactionInProcess  := false;
    {$IFDEF MSWINDOWS}
      FSqlConnexion.LibraryLocation := GetGHCaveDrawDirectory() + 'libmysql.dll';
    {$ENDIF}
    {$IFDEF LINUX}
      FSqlConnexion.LibraryLocation := GetGHCaveDrawDirectory() + 'libmysql.so';
    {$ENDIF}
    FSqlConnexion.Protocol := 'mysql-5';
    FSqlConnexion.HostName := '127.0.0.1';
    FSqlConnexion.Database := 'opencavemap';
    FSqlConnexion.Password := '';
    FSqlConnexion.User     := 'root';

    FMainSqlQuery.Connection   := FSqlConnexion;
    FSecSqlQuery.Connection    := FSqlConnexion;
    FIsoleeSqlQuery.Connection := FSqlConnexion;

    // fermer toutes les connexions
    CloseConnexion();
    Result := true;
  except
  end;
end;




function TConnexionOpenCaveMap.CheckUser(const Login, Pwd: string): boolean;
var
  MD5Pwd, WU: String;
  Nb: Integer;
begin
  Result := false;
  if (not FSqlConnexion.Connected) then Exit;
  MD5Pwd := MD5Print(MD5String(Pwd));
  WU := format('SELECT * FROM %s WHERE (Login LIKE "%s") and (HashPassword="%s");', [TABLE_UTILISATEURS, Login, MD5Pwd]);
  //select * from utilisateurs where LOGIN like "jpcassoU";
  Nb := 0;
  FMainSqlQuery.SQL.Text := WU;
  try
    FMainSqlQuery.Open;
    // La requête comporte tous les critères. On recherche uniquement s'il y a une et une seule ligne satisfaisant les critères
    // L'item trouvé va dans FCurrentUser
    while (not FMainSqlQuery.EOF) do
    begin
      FMainSqlQuery.Next;
      FCurrentUser.IDUtilisateur   := StrToInt64Def(FMainSqlQuery.Fields[0].AsString, 0);        // IDUtilisateur
      FCurrentUser.NomUtilisateur  := Trim(FMainSqlQuery.Fields[1].AsString);                    // NomUtilisateur
      FCurrentUser.Login           := Trim(FMainSqlQuery.Fields[2].AsString);                    // Login
      // Hash du pwd: inutilisé                                                                  // HashPassword
      FCurrentUser.DateInscription := DateSqlToTDateTime(FMainSqlQuery.Fields[4].AsString);      // DateInscription
      FCurrentUser.Email           := Trim(FMainSqlQuery.Fields[5].AsString);                    // Email
      Inc(Nb);
    end;
    Result := (Nb = 1);
  finally
    FMainSqlQuery.Close;
  end;
end;

function TConnexionOpenCaveMap.AddCaviteInBDD(const C: TCaviteInBDD): boolean;
var
  WU: String;
begin
  Result := false;
  try
    WU := Format('INSERT INTO %s (NomCavite, CodeEPSG, IDUserCreateur, DateAjout) ' +
               'VALUES ("%s", %d, %d, ''%s'');',
               [TABLE_CAVITES, C.NomCavite, C.CodeEPSG, FCurrentUser.IDUtilisateur, DateTimeToDateSql(Now())]);
    SendSQLCommand(WU);
    Result := true;
  finally
  end;
end;

function TConnexionOpenCaveMap.AddGroupeModifications(const IdxCavite: Int64; const Commentaires: string): boolean;
var
  WU: String;
begin
  Result := false;
  try
    WU := Format('INSERT INTO %s (IDCavite, IDUser, DateModif, Commentaires) ' +
                 'VALUES (%d, %d, ''%s'', "%s");',
               [TABLE_GROUPES_MODIFICATIONS,
                IdxCavite,FCurrentUser.IDUtilisateur, DateTimeToDateSql(Now()), Commentaires]);
    SendSQLCommand(WU);
    Result := true;
  finally
  end;
end;



function TConnexionOpenCaveMap.OpenConnexion(): boolean;
begin
  AfficherMessage(Format('%s.OpenConnexion()', [ClassName]));
  DispMsgInLbl(Format('%s.OpenConnexion()', [ClassName]));
  FSqlConnexion.Disconnect; // par sécurité
  FSqlConnexion.Connect;
  FTransactionInProcess := false;
  Result := FSqlConnexion.Connected;
  DispMsgInLbl(IIF(Result, 'Connexion établie', 'Echec de connexion'));

end;


procedure TConnexionOpenCaveMap.CloseConnexion();
begin
  try
    AfficherMessage(Format('%s.CloseConnexion()', [ClassName]));
    FSqlConnexion.Disconnect;
    DispMsgInLbl(Format('%s.CloseConnexion()', [ClassName]));
    DispMsgInLbl('Fin de connexion');
  except
  end;
end;
//******************************************************************************
function TConnexionOpenCaveMap.SendSQLCommand(const Cmd: string): boolean;
begin
  result := false;
  //try
    //AfficherMessage('MySQL Command: ' + Cmd);
    FMainSqlQuery.SQL.Text := Cmd;
    FMainSqlQuery.ExecSQL;
    Result := True;
  //except
  //  AfficherMessageErreur(Cmd);
  //end;
  application.ProcessMessages;
end;

procedure TConnexionOpenCaveMap.SetProcDisplayAvancement(const P: TProcDisplayAvancement);
begin
  FProcDisplayAvancement := P;
end;

procedure TConnexionOpenCaveMap.BeginTransaction();
begin
  SendSQLCommand('START TRANSACTION');
  FTransactionInProcess := True;
end;
procedure TConnexionOpenCaveMap.CancelTransaction();
begin
  SendSQLCommand('ROLLBACK;');
  FTransactionInProcess := false;
end;


procedure TConnexionOpenCaveMap.CommitTransaction();
begin
  if (not FTransactionInProcess) then ShowMessage('Transaction non ouverte');
  SendSQLCommand('COMMIT;');
  FTransactionInProcess := false;
end;

procedure TConnexionOpenCaveMap.Finalise();
begin
  AfficherMessage(Format('%s.Finalise()', [ClassName]));
  try
    FSqlConnexion.Disconnect;
    FListeCavitesInBDD.ClearListe();
  finally
    FreeAndNil(FSqlConnexion);//FSqlConnexion.Free;
    FreeAndNil(FMainSqlQuery);//FMainSqlQuery.Free;
    FreeAndNil(FSecSqlQuery);//FSecSqlQuery.Free;
    FreeAndNil(FIsoleeSqlQuery);//FIsoleeSqlQuery.free;
    FreeAndNil(FListeCavitesInBDD);//FListeCavitesInBDD.Free;
  end;
end;

function TConnexionOpenCaveMap.GetCaviteFromLstBDD(const Idx: integer): TCaviteInBDD;
begin
  Result := FListeCavitesInBDD.GetElement(Idx);
end;

function TConnexionOpenCaveMap.GetCurrentUser(): TUtilisateurOCM;
begin
  Result := FCurrentUser;
end;

function TConnexionOpenCaveMap.GetNbCavitesInListe(): integer;
begin
  Result := FListeCavitesInBDD.GetNbElements();
end;

function TConnexionOpenCaveMap.GetNbElementsInTable(const NomTable: string): Int64;
var
  Q: String;
begin
  Q := Format('SELECT COUNT(*) FROM %s;', [NomTable]);
  Result := GetScalarInt32SQLResult(Q);
end;
function TConnexionOpenCaveMap.GetNbElementsInTableForCavite(const NomTable: string; const FieldIDObj: string; const IdxCavite: Int64): Int64;
var
  Q: String;
begin
  Q := Format('SELECT COUNT(*) FROM %s WHERE (%s div %d = %d);', [NomTable, FieldIDObj, NB_MAX_OBJ_PAR_CAVITE, IdxCavite]);
  Result := GetScalarInt64SQLResult(Q);
end;

procedure TConnexionOpenCaveMap.DispMsgInLbl(const Msg: string);
begin
  if (Assigned(FLabelMsg)) then FLabelMsg.Caption := Msg;
  Application.ProcessMessages;
end;

function TConnexionOpenCaveMap.GetNbGroupesForCavite(const IdxCavite: Int64): integer;
begin
  result := GetNbElementsInTableForCavite(TABLE_GROUPES, 'IDGroupe', IdxCavite);
end;
function TConnexionOpenCaveMap.GetNbScrapsForCavite(const IdxCavite: Int64): integer;
begin
  result := GetNbElementsInTableForCavite(TABLE_SCRAPS, 'IDScrap', IdxCavite);
end;
function TConnexionOpenCaveMap.GetNbCourbesForCavite(const IdxCavite: Int64): integer;
begin
  result := GetNbElementsInTableForCavite(TABLE_COURBES, 'IDCourbe', IdxCavite);
end;
function TConnexionOpenCaveMap.GetNbPolylignesForCavite(const IdxCavite: Int64): integer;
begin
  result := GetNbElementsInTableForCavite(TABLE_POLYLIGNES, 'IDPoly', IdxCavite);
end;
function TConnexionOpenCaveMap.GetNbPolygonesForCavite(const IdxCavite: Int64): integer;
begin
  result := GetNbElementsInTableForCavite(TABLE_POLYGONES, 'IDPoly', IdxCavite);
end;
function TConnexionOpenCaveMap.GetNbSimplesLignesForCavite(const IdxCavite: Int64): integer;
begin
  result := GetNbElementsInTableForCavite(TABLE_SIMPLES_LIGNES, 'IDSimpleLigne', IdxCavite);
end;
function TConnexionOpenCaveMap.GetNbSymbolesForCavite(const IdxCavite: Int64): integer;
begin
  result := GetNbElementsInTableForCavite(TABLE_SYMBOLES, 'IDSymbole', IdxCavite);
end;
function TConnexionOpenCaveMap.GetNbTextesForCavite(const IdxCavite: Int64): integer;
begin
  result := GetNbElementsInTableForCavite(TABLE_TEXTES, 'IDTexte', IdxCavite);
end;

function TConnexionOpenCaveMap.GetScalarDoubleSQLResult(const Q: string): double;
begin
  Result := ConvertirEnNombreReel(GetScalarStrSQLResult(Q), 0.00);
end;

function TConnexionOpenCaveMap.GetScalarInt32SQLResult(const Q: string): integer;
begin
  Result := StrToIntDef(GetScalarStrSQLResult(Q), 0);
end;

function TConnexionOpenCaveMap.GetScalarInt64SQLResult(const Q: string): int64;
begin
  Result := StrToInt64Def(GetScalarStrSQLResult(Q), 0);
end;

function TConnexionOpenCaveMap.GetScalarStrSQLResult(const Q: string): string;
begin
  Result := '';
  try
    FIsoleeSqlQuery.SQL.Clear;
    FIsoleeSqlQuery.SQL.Text := Q;
    FIsoleeSqlQuery.Open;
    while not FIsoleeSqlQuery.EOF do
    begin
      Result  := Trim(FIsoleeSqlQuery.Fields[0].AsString);
      FIsoleeSqlQuery.Next;
    end;
  finally
    FIsoleeSqlQuery.Close;
  end;
end;







function TConnexionOpenCaveMap.Connected: boolean;
begin
  result := FSqlConnexion.Connected;
end;


//******************************************************************************
procedure TConnexionOpenCaveMap.ExtractListeCavitesFromBDD();
var
  QCavite: TCaviteInBDD;
  Q: String;
begin
  AfficherMessage(Format('%s.ListerCavitesInBase()', [ClassName]));
  if (not FSqlConnexion.Connected) then Exit;
  Q := Format('SELECT * FROM %s;', [TABLE_CAVITES]);
  //AfficherMessage(Q);
  FListeCavitesInBDD.ClearListe();
  try
    FMainSqlQuery.SQL.Clear;
    FMainSqlQuery.SQL.Text := Q;
    FMainSqlQuery.Open;
    while not FMainSqlQuery.EOF do
    begin
      QCavite.IDCavite           := StrToIntDef(FMainSqlQuery.Fields[0].AsString, 0);
      QCavite.NomCavite          := Trim(FMainSqlQuery.Fields[1].AsString);
      QCavite.CodeEPSG           := StrToIntDef(FMainSqlQuery.Fields[2].AsString, 0);

      QCavite.IDCavite           := StrToIntDef(FMainSqlQuery.Fields[0].AsString, 0);
      QCavite.NomCavite          := Trim(FMainSqlQuery.Fields[1].AsString);
      QCavite.CodeEPSG           := StrToIntDef(FMainSqlQuery.Fields[2].AsString, 0);
      QCavite.IDUserCreateur     := StrToIntDef(FMainSqlQuery.Fields[3].AsString, 0);
      QCavite.DateAjout          := DateSqlToTDateTime(FMainSqlQuery.Fields[4].AsString);
      QCavite.IDLastUser         := StrToIntDef(FMainSqlQuery.Fields[5].AsString, 0);
      QCavite.DateLastModif      := DateSqlToTDateTime(FMainSqlQuery.Fields[6].AsString);
      QCavite.IsEditing          := (1 = StrToIntDef(FMainSqlQuery.Fields[7].AsString, 0));

      QCavite.NbGroupes          := GetNbGroupesForCavite(QCavite.IDCavite);
      QCavite.NbScraps           := GetNbScrapsForCavite(QCavite.IDCavite);
      QCavite.NbCourbes          := GetNbCourbesForCavite(QCavite.IDCavite);
      QCavite.NbPolygones        := GetNbPolygonesForCavite(QCavite.IDCavite);
      QCavite.NbPolylignes       := GetNbPolylignesForCavite(QCavite.IDCavite);
      QCavite.NbSimplesLignes    := GetNbSimplesLignesForCavite(QCavite.IDCavite);
      QCavite.NbSymboles         := GetNbSymbolesForCavite(QCavite.IDCavite);
      QCavite.NbTextes           := GetNbTextesForCavite(QCavite.IDCavite);



      QCavite.NbGroupes          := GetNbGroupesForCavite(QCavite.IDCavite);
      QCavite.NbScraps           := GetNbScrapsForCavite(QCavite.IDCavite);
      QCavite.NbCourbes          := GetNbCourbesForCavite(QCavite.IDCavite);
      QCavite.NbPolygones        := GetNbPolygonesForCavite(QCavite.IDCavite);
      QCavite.NbPolylignes       := GetNbPolylignesForCavite(QCavite.IDCavite);
      QCavite.NbSimplesLignes    := GetNbSimplesLignesForCavite(QCavite.IDCavite);
      QCavite.NbSymboles         := GetNbSymbolesForCavite(QCavite.IDCavite);
      QCavite.NbTextes           := GetNbTextesForCavite(QCavite.IDCavite);
      //*)
      AddCaviteFromBDD(QCavite);
      FMainSqlQuery.Next;
    end;
  finally
    FMainSqlQuery.Close;
  end;
end;

//******************************************************************************
function TConnexionOpenCaveMap.BDDExtractBasePointsOfCavite(const QIDCavite: integer; const QCodeEPSG: integer): boolean;
const
  TBL_BASEPOINTS_COL_IDBasePoint = 'IDBasePoint';
var
  //FCenterlines: TCenterLines;
  BP          : TBaseStation;
  WU          : String;
  Nb, i: Integer;
begin
  Result := false;
  FCodeEPSG := QCodeEPSG;
  AfficherMessageErreur(Format('%s.ExtractBasePointsOfCavite: %d - EPSG:%d', [ClassName, QIDCavite, FCodeEPSG]));
  DispMsgInLbl('Extraction des centerlines');
  FMyDocDessin.PurgerListesObjets();
  //FCenterlines := TCenterLines.Create();
  try
    //FMyDocDessin. FCenterlines.InitialiseCenterLines();
    WU := format('SELECT * FROM %s WHERE (Abs(%s div %d) = %d) ORDER BY %s ASC;', [TABLE_BASEPOINTS, TBL_BASEPOINTS_COL_IDBasePoint, MULT_IDX_CAVITE, QIDCavite, TBL_BASEPOINTS_COL_IDBasePoint]);
    FMainSqlQuery.SQL.Clear;
    FMainSqlQuery.SQL.Text := WU;
    try
      FMainSqlQuery.Open;
      while (not FMainSqlQuery.EOF) do
      begin
        // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
        BP.IDStation      := StrToInt64Def(FMainSqlQuery.Fields[0].AsString, 0) mod MULT_IDX_CAVITE;        // IDBasePoint
                                                                                                            // IDCavite: champ ignoré
        BP.Couleur        := StrToIntDef(FMainSqlQuery.Fields[2].AsString, clBlue);                         // Couleur
        ReprojeterCoordsToUCS(ConvertirEnNombreReel(FMainSqlQuery.Fields[3].AsString, 0.00),
                              ConvertirEnNombreReel(FMainSqlQuery.Fields[4].AsString, 0.00),
                              BP.PosExtr0.X, BP.PosExtr0.Y);
        BP.PosExtr0.Z     := ConvertirEnNombreReel(FMainSqlQuery.Fields[5].AsString, 0.00);                         // PosExtr0_Alt
        ReprojeterCoordsToUCS(ConvertirEnNombreReel(FMainSqlQuery.Fields[6].AsString, 0.00),
                              ConvertirEnNombreReel(FMainSqlQuery.Fields[7].AsString, 0.00),
                              BP.PosStation.X, BP.PosStation.Y);
        BP.PosStation.Z   := ConvertirEnNombreReel(FMainSqlQuery.Fields[8].AsString, 0.00);                         // PosStation_Alt

        ReprojeterCoordsToUCS(ConvertirEnNombreReel(FMainSqlQuery.Fields[9].AsString, 0.00),
                              ConvertirEnNombreReel(FMainSqlQuery.Fields[10].AsString, 0.00),
                              BP.PosPG.X, BP.PosPG.Y);
        BP.PosPG.Z        := ConvertirEnNombreReel(FMainSqlQuery.Fields[11].AsString, BP.PosStation.Z);             // Pos_PG_Alt
        ReprojeterCoordsToUCS(ConvertirEnNombreReel(FMainSqlQuery.Fields[12].AsString, 0.00),
                              ConvertirEnNombreReel(FMainSqlQuery.Fields[13].AsString, 0.00),
                              BP.PosPD.X, BP.PosPD.Y);
        BP.PosPD.Z        := ConvertirEnNombreReel(FMainSqlQuery.Fields[14].AsString, BP.PosStation.Z);             // Pos_PD_Alt
        BP.IDTerrain      := Trim(FMainSqlQuery.Fields[15].AsString);                                       // IDTerrain
        FMyDocDessin.AddABasePoint(BP);
        FMainSqlQuery.Next;
      end;
    finally
      FMainSqlQuery.Close;
    end;
    //FCenterlines.FinaliseCenterLines();
  finally
    //FCenterlines.Free;
  end;
  // contrôle
  Nb := FMyDocDessin.GetNbBaseStations();
  AfficherMessage(Format('%d basepoints', [Nb]));
  {$IFDEF DISP_TO_CONSOLE}
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    BP := FMyDocDessin.GetBaseStation(i);
    AfficherMessage(Format('Point;%d; %.3f; %.3f; %.3f;%s', [BP.IDStation,  BP.PosStation.X, BP.PosStation.Y, BP.PosStation.Z, BP.IDTerrain]));
  end;
  {$ENDIF}
  //*)
  Result := true;
end;
// TODO: Les groupes doivent être extraits en dernier en raison du calcul des BoundingBox
function TConnexionOpenCaveMap.BDDExtractAllGroupesOfCavite(const QIDCavite: integer): boolean;
const
  TBL_BASEPOINTS_COL_IDGroupe = 'IDGroupe';
var
  GRP: TGroupeEntites;
  WU: String;
  Nb, i: Integer;
begin
  Result := false;
  AfficherMessage(Format('%s.BDDExtractAllGroupesOfCavite: %d', [ClassName, QIDCavite]));
  DispMsgInLbl('Extraction des groupes');
  WU := format('SELECT * FROM %s WHERE (Abs(%s div %d) = %d) ORDER BY %s ASC;', [TABLE_GROUPES, TBL_BASEPOINTS_COL_IDGroupe, NB_MAX_OBJ_PAR_CAVITE, QIDCavite, TBL_BASEPOINTS_COL_IDGroupe]);
  AfficherMessage(WU);
  FMainSqlQuery.SQL.Clear;
  FMainSqlQuery.SQL.Text := WU;
  //try
    FMainSqlQuery.Open;
    while (not FMainSqlQuery.EOF) do
    begin
      // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
      AfficherMessage(FMainSqlQuery.Fields[0].AsString);
      GRP.IDGroupeEntites    := StrToInt64Def(FMainSqlQuery.Fields[0].AsString, 0) MOD (QIDCavite * NB_MAX_OBJ_PAR_CAVITE);        // IDGroupe
      //GRP.IDSuperGroupe      := StrToInt64Def(FMainSqlQuery.Fields[1].AsString, 0);                            // IDSuperGroupe
      GRP.CouleurGroupe      := StrToIntDef(FMainSqlQuery.Fields[2].AsString, clBlue);                         // CouleurGroupe
      GRP.ZOrder             := ConvertirEnNombreReel(FMainSqlQuery.Fields[3].AsString, 0.00);                         // ZOrder
      GRP.Decalage.X         := ConvertirEnNombreReel(FMainSqlQuery.Fields[4].AsString, 0.00);                         // DecalageX
      GRP.Decalage.Y         := ConvertirEnNombreReel(FMainSqlQuery.Fields[5].AsString, 0.00);                         // DecalageY
      GRP.Decalage.Z         := 0.00;
      GRP.DecalageActif      := (ConvertirEnNombreReel(FMainSqlQuery.Fields[6].AsString, 0) = 1);
      GRP.Visible            := (ConvertirEnNombreReel(FMainSqlQuery.Fields[7].AsString, 0) = 1);
      //GRP.Locked             := (ConvertirEnNombreReel(FMainSqlQuery.Fields[8].AsString, 0) = 1);
      GRP.DateLastModif      := Now();
      GRP.NomGroupe          := Trim(FMainSqlQuery.Fields[10].AsString);                                                 // NomGroupe;
      GRP.Filtres            := Trim(FMainSqlQuery.Fields[11].AsString);                                                 // Filtres;
      // AfficherMessage(Format('Groupe; %d; %d; %.3f; %.3f; %.3f; %s; %s', [GRP.IDGroupeEntites, GRP.IDSuperGroupe,
      //                                                                          GRP.Decalage.X, GRP.Decalage.Y, GRP.ZOrder,
      //                                                                          GRP.NomGroupe, GRP.Filtres]));
      FMyDocDessin.AddGroupe(GRP, false);
      FMainSqlQuery.Next;
    end;
  //finally
    FMainSqlQuery.Close;
  //end;
  // contrôle
  Nb := FMyDocDessin.GetNbGroupes();
  AfficherMessage(Format('%d groupes', [Nb]));
  {.$IFDEF DISP_TO_CONSOLE}

  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    GRP := FMyDocDessin.GetGroupe(i);
    AfficherMessage(Format('Groupe; %d; %d; %.3f; %.3f; %.3f; %s; %s', [GRP.IDGroupeEntites, 0, //GRP.IDSuperGroupe,
                                                                        GRP.Decalage.X, GRP.Decalage.Y, GRP.ZOrder,
                                                                        GRP.NomGroupe, GRP.Filtres]));
  end;
  {.$ENDIF}
end;

function TConnexionOpenCaveMap.BDDExtractAllScrapsOfCavite(const QIDCavite: integer): boolean;
const
  TBL_SCRAPS_COL_IDScrap = 'IDScrap';
var
  WU: String;
  Nb, NbV, j, i: Integer;
  QIdScrap: Int64;
  QNbVertex, QIdxVertex: LongInt;
  MyVertex : TVertexPolygon;
  MyScrap: TScrap;
begin
  Result := false;
  AfficherMessage(Format('%s.BDDExtractAllScrapsOfCavite: %d', [ClassName, QIDCavite]));
  DispMsgInLbl('Extraction des scraps');
  WU := format('SELECT * FROM %s WHERE (%s div %d = %d) ORDER BY %s ASC;',
               [TABLE_SCRAPS,
                TBL_SCRAPS_COL_IDScrap, NB_MAX_OBJ_PAR_CAVITE, QIDCavite,
                TBL_SCRAPS_COL_IDScrap]);
  AfficherMessage(WU);
  FMainSqlQuery.SQL.Clear;
  FMainSqlQuery.SQL.Text := WU;
  Nb := 0;
  //try
    FMainSqlQuery.Open;
    while (not FMainSqlQuery.EOF) do
    begin
      // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
      QIdScrap                   := StrToInt64Def(FMainSqlQuery.Fields[0].AsString, 0);
      MyScrap.IDGroupe           := StrToInt64Def(FMainSqlQuery.Fields[1].AsString, 0) mod NB_MAX_OBJ_PAR_CAVITE;
      MyScrap.Couleur            := StrToIntDef(FMainSqlQuery.Fields[2].AsString, clSilver);
      MyScrap.Opacite            := StrToIntDef(FMainSqlQuery.Fields[3].AsString, 50);                            // IDGroupe
      QNbVertex                  := StrToIntDef(FMainSqlQuery.Fields[4].AsString, 0);
      //AfficherMessage(Format('Scrap %d; Groupe: %d; Couleur: %d; Opacite: %d; NbVertex = %d',
      //                             [QIdScrap,
      //                              MyScrap.IDGroupe, MyScrap.Couleur, MyScrap.Opacite,
      //                              QNbVertex]));
      SetLength(MyScrap.Sommets, QNbVertex);
      //************************************************************************
      // Sous-requête pour les vertex
        WU := format('SELECT * FROM %s WHERE (%s = %d) ORDER BY %s ASC;',
                     [TABLE_VERTEX_SCRAPS,
                      TBL_VERTEX_SCRAPS_POLY_COL_IDPoly, QIdScrap,
                     'IdxVertex']);

        AfficherMessage(WU);
        FSecSqlQuery.SQL.Clear;
        FSecSqlQuery.SQL.Text := WU;
        NbV := 0;
        //try
          FSecSqlQuery.Open;
          while (not FSecSqlQuery.EOF) do
          begin
            // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
            QIdScrap    := StrToInt64Def(FSecSqlQuery.Fields[0].AsString, 0);
            QIdxVertex  := StrToIntDef(FSecSqlQuery.Fields[1].AsString, 0);
            MyVertex.IDStation := StrToInt64Def(FSecSqlQuery.Fields[2].AsString, 0) mod MULT_IDX_CAVITE;
            MyVertex.Offset.X  := ConvertirEnNombreReel(FSecSqlQuery.Fields[3].AsString, 0.00);
            MyVertex.Offset.Y  := ConvertirEnNombreReel(FSecSqlQuery.Fields[4].AsString, 0.00);
            MyScrap.putVertex(NbV, MyVertex); //Sommets[NbV] := MyVertex;
            FSecSqlQuery.Next;
            Inc(NbV);
            //AfficherMessage(inttostr(nbV));
          end;
        //finally
          FSecSqlQuery.Close;
        //end;
      // TODO Ajouter le polygone ici
      //************************************************************************
      FMyDocDessin.AddScrap(MyScrap, True);
      FMainSqlQuery.Next;
      Inc(Nb);
    end;
    AfficherMessage('Scrap done');
  //finally
    FMainSqlQuery.Close;
  //end;
  // contrôle
  Nb := FMyDocDessin.GetNbScraps();
  AfficherMessage(Format('%d scraps', [Nb]));
  {$IFDEF DISP_TO_CONSOLE}
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    MyScrap := FMyDocDessin.GetScrap(i);
    AfficherMessage(Format('Scrap %d; Groupe: %d; Couleur: %d; Opacite: %d; NbVertex = %d',
                                   [i,
                                    MyScrap.IDGroupe, MyScrap.Couleur, MyScrap.Opacite,
                                    QNbVertex]));
    for j := 0 to High(MyScrap.Sommets) do
    begin
      MyVertex := MyScrap.Sommets[j];
      AfficherMessage(Format('-- Vertex: %d; Basepoint: %d; Offset = (%.3f, %.3f)',
                             [j, MyVertex.IDStation, MyVertex.Offset.X, MyVertex.Offset.Y]));
    end;
  end;
  {$ENDIF}
end;



procedure TConnexionOpenCaveMap.AddCaviteFromBDD(const C: TCaviteInBDD);
begin
  FListeCavitesInBDD.AddElement(C);
end;

function TConnexionOpenCaveMap.BDDExtractAllCourbesOfCavite(const QIDCavite: integer): boolean;
var
  MyCourbe: TCourbe;
  MyArc   : TArcCourbe;
  WU: String;
  QNbArcs, QIdxArc: LongInt;
  Nb, NbV, i, j: Integer;
  QIdCourbe: Int64;
begin
  Result := false;
  AfficherMessage(Format('%s.BDDExtractAllPolygonesOfCavite: %d', [ClassName, QIDCavite]));
  DispMsgInLbl('Extraction des courbes');


  WU := format('SELECT * FROM %s WHERE (%s div %d = %d) ORDER BY %s ASC;',
               [TABLE_COURBES,
                TBL_COURBES_COL_IDCourbe, NB_MAX_OBJ_PAR_CAVITE, QIDCavite,
                TBL_COURBES_COL_IDCourbe]);
  AfficherMessage(WU);
  FMainSqlQuery.SQL.Clear;
  FMainSqlQuery.SQL.Text := WU;
  Nb := 0;
  try
    FMainSqlQuery.Open;
    while (not FMainSqlQuery.EOF) do
    begin

      // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
      QIdCourbe                 := StrToInt64Def(FMainSqlQuery.Fields[0].AsString, 0);
      MyCourbe.IDGroupe         := StrToInt64Def(FMainSqlQuery.Fields[1].AsString, 0) mod NB_MAX_OBJ_PAR_CAVITE;
      MyCourbe.IDStyleCourbe    := TNatureObjetCourbe(StrToIntDef(FMainSqlQuery.Fields[2].AsString, 0));                            // IDGroupe
      QNbArcs                 := StrToIntDef(FMainSqlQuery.Fields[3].AsString, 0);
      //AfficherMessage(Format('Courbe %d; Groupe: %d; Style: %d; NbVertex = %d',
      //                             [QIdCourbe,
      //                              MyCourbe.IDGroupe, MyCourbe.IDStyleCourbe,
      //                              QNbArcs]));
      SetLength(MyCourbe.Arcs, QNbArcs);
      //************************************************************************
      // Sous-requête pour les vertex
        WU := format('SELECT * FROM %s WHERE (%s = %d) ORDER BY %s ASC;',
                     [TABLE_ARCS_COURBES,
                      TBL_ARCS_COL_IDCOURBE, QIdCourbe,
                     'IdxArc']);

        //AfficherMessage(WU);
        FSecSqlQuery.SQL.Clear;
        FSecSqlQuery.SQL.Text := WU;
        NbV := 0;
        try
          FSecSqlQuery.Open;
          while (not FSecSqlQuery.EOF) do
          begin
            // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
            QIdCourbe         := StrToInt64Def(FSecSqlQuery.Fields[0].AsString, 0);
            QIdxArc           := StrToIntDef(FSecSqlQuery.Fields[1].AsString, 0);

            MyArc.IDStationP1 := StrToInt64Def(FSecSqlQuery.Fields[2].AsString, 0) mod MULT_IDX_CAVITE;
            MyArc.IDStationP2 := StrToInt64Def(FSecSqlQuery.Fields[3].AsString, 0) mod MULT_IDX_CAVITE;
            MyArc.OffsetP1.X  := ConvertirEnNombreReel(FSecSqlQuery.Fields[4].AsString, 0.00);
            MyArc.OffsetP1.Y  := ConvertirEnNombreReel(FSecSqlQuery.Fields[5].AsString, 0.00);
            MyArc.TangP1.X    := ConvertirEnNombreReel(FSecSqlQuery.Fields[6].AsString, 0.00);
            MyArc.TangP1.Y    := ConvertirEnNombreReel(FSecSqlQuery.Fields[7].AsString, 0.00);
            MyArc.OffsetP2.X  := ConvertirEnNombreReel(FSecSqlQuery.Fields[8].AsString, 0.00);
            MyArc.OffsetP2.Y  := ConvertirEnNombreReel(FSecSqlQuery.Fields[9].AsString, 0.00);
            MyArc.TangP2.X    := ConvertirEnNombreReel(FSecSqlQuery.Fields[10].AsString, 0.00);
            MyArc.TangP2.Y    := ConvertirEnNombreReel(FSecSqlQuery.Fields[11].AsString, 0.00);
            //MyVertex.Offset.X  := ;
            //MyVertex.Offset.Y  := ConvertirEnNombreReel(FSecSqlQuery.Fields[4].AsString, 0.00);
            (*
            AfficherMessage(Format('-- Courbe %d; Arc: %d; Basepoint1: %d; BasePoint2: %d; OffsetP1 = (%.3f, %.3f); TangP1 = (%.3f, %.3f); OffsetP2 = (%.3f, %.3f); TangP2 = (%.3f, %.3f)',
                                         [QIdCourbe, QIdxArc,
                                          MyArc.IDStationP1, MyArc.IDStationP1,
                                          MyArc.OffsetP1.X, MyArc.OffsetP1.Y, MyArc.TangP1.X, MyArc.TangP1.Y,
                                          MyArc.OffsetP2.X, MyArc.OffsetP2.Y, MyArc.TangP2.X, MyArc.TangP2.Y
                                         ]));
            //*)
            MyCourbe.Arcs[NbV] := MyArc;

            FSecSqlQuery.Next;
            Inc(NbV);
          end;
        finally
          FSecSqlQuery.Close;
        end;
      //*)
      // TODO Ajouter le polygone ici
      //************************************************************************
      FMyDocDessin.AddCourbe(MyCourbe, True);
      FMainSqlQuery.Next;
      Inc(Nb);
    end;
    AfficherMessage(Format('%d courbes', [Nb]));
  finally
    FMainSqlQuery.Close;
  end;

  // contrôle
  Nb := FMyDocDessin.GetNbCourbes();
  AfficherMessage(Format('%d courbes', [Nb]));
  {$IFDEF DISP_TO_CONSOLE}

  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    MyCourbe := FMyDocDessin.GetCourbe(i);
    QNbArcs  := 1 + High(MyCourbe.Arcs);
    AfficherMessage(Format('Courbe %d; Groupe: %d; Style: %d; NbVertex = %d',
                                 [i, MyCourbe.IDGroupe, MyCourbe.IDStyleCourbe, QNbArcs]));
    for j := 0 to High(MyCourbe.Arcs) do
    begin
      MyArc := MyCourbe.Arcs[j];
      AfficherMessage(Format('-- Courbe %d; Arc: %d; Basepoint1: %d; BasePoint2: %d; OffsetP1 = (%.3f, %.3f); TangP1 = (%.3f, %.3f); OffsetP2 = (%.3f, %.3f); TangP2 = (%.3f, %.3f)',
                             [i, j, MyArc.IDStationP1, MyArc.IDStationP1,
                              MyArc.OffsetP1.X, MyArc.OffsetP1.Y, MyArc.TangP1.X, MyArc.TangP1.Y,
                              MyArc.OffsetP2.X, MyArc.OffsetP2.Y, MyArc.TangP2.X, MyArc.TangP2.Y
                             ]));
    end;
  end;
  {$ENDIF}
end;

function TConnexionOpenCaveMap.BDDExtractAllPolygonesOfCavite(const QIDCavite: integer): boolean;
var
  WU: String;
  Nb, NbV, i, j: Integer;
  QIdPolygone: Int64;
  QNbVertex, QIdxVertex: LongInt;
  MyVertex : TVertexPolygon;
  MyPolygone: TPolygone;
begin
  Result := false;
  AfficherMessage(Format('%s.BDDExtractAllPolygonesOfCavite: %d', [ClassName, QIDCavite]));
  DispMsgInLbl('Extraction des polygones');
  WU := format('SELECT * FROM %s WHERE (%s div %d = %d) ORDER BY %s ASC;',
               [TABLE_POLYGONES,
                TBL_POLYGONES_COL_IDPoly, NB_MAX_OBJ_PAR_CAVITE, QIDCavite,
                TBL_POLYGONES_COL_IDPoly]);
  AfficherMessage(WU);
  FMainSqlQuery.SQL.Clear;
  FMainSqlQuery.SQL.Text := WU;
  Nb := 0;
  try
    FMainSqlQuery.Open;
    while (not FMainSqlQuery.EOF) do
    begin
      // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
      QIdPolygone                   := StrToInt64Def(FMainSqlQuery.Fields[0].AsString, 0);
      MyPolygone.IDGroupe           := StrToInt64Def(FMainSqlQuery.Fields[1].AsString, 0) mod NB_MAX_OBJ_PAR_CAVITE;
      MyPolygone.IDStylePolygone    := TNatureObjetPolygone(StrToIntDef(FMainSqlQuery.Fields[2].AsString, 0));                            // IDGroupe
      QNbVertex                     := StrToIntDef(FMainSqlQuery.Fields[3].AsString, 0);
      //AfficherMessage(Format('Polygone %d; Groupe: %d; Style: %d; NbVertex = %d',
      //                             [QIdPolygone,
      //                              MyPolygone.IDGroupe, MyPolygone.IDStylePolygone,
      //                              QNbVertex]));
      SetLength(MyPolygone.Sommets, QNbVertex);
      //************************************************************************
      // Sous-requête pour les vertex
        WU := format('SELECT * FROM %s WHERE (%s = %d) ORDER BY %s ASC;',
                     [TABLE_VERTEX_POLYGONES,
                      TBL_VERTEX_SCRAPS_POLY_COL_IDPoly, QIdPolygone,
                     'IdxVertex']);

        //AfficherMessage(WU);
        FSecSqlQuery.SQL.Clear;
        FSecSqlQuery.SQL.Text := WU;
        NbV := 0;
        try
          FSecSqlQuery.Open;
          while (not FSecSqlQuery.EOF) do
          begin
            // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
            QIdPolygone := StrToInt64Def(FSecSqlQuery.Fields[0].AsString, 0);
            QIdxVertex  := StrToIntDef(FSecSqlQuery.Fields[1].AsString, 0);
            MyVertex.IDStation := StrToInt64Def(FSecSqlQuery.Fields[2].AsString, 0) mod MULT_IDX_CAVITE;
            MyVertex.Offset.X  := ConvertirEnNombreReel(FSecSqlQuery.Fields[3].AsString, 0.00);
            MyVertex.Offset.Y  := ConvertirEnNombreReel(FSecSqlQuery.Fields[4].AsString, 0.00);

            //AfficherMessage(Format('-- Poly %d; Vertex: %d; Basepoint: %d; Offset = (%.3f, %.3f)',
            //                             [QIdPolygone, QIdxVertex,
            //                              MyVertex.IDStation, MyVertex.Offset.X, MyVertex.Offset.Y
            //                             ]));
            MyPolygone.putVertex(NbV, MyVertex);  //MyPolygone.Sommets[NbV] := MyVertex;
            FSecSqlQuery.Next;
            Inc(NbV);
          end;
        finally
          FSecSqlQuery.Close;
        end;
      // TODO Ajouter le polygone ici
      //************************************************************************
      FMyDocDessin.AddPolygone(MyPolygone, True);
      FMainSqlQuery.Next;
      Inc(Nb);
    end;
    AfficherMessage(Format('%d polygones', [Nb]));
  finally
    FMainSqlQuery.Close;
  end;

  // contrôle
  Nb := FMyDocDessin.GetNbPolygones();
  AfficherMessage(Format('%d polygones', [Nb]));
  {$IFDEF DISP_TO_CONSOLE}

  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    MyPolygone := FMyDocDessin.GetPolygone(i);
    AfficherMessage(Format('Polygone %d; Groupe: %d; Style: %d; NbVertex = %d',
                                 [i,
                                  MyPolygone.IDGroupe, MyPolygone.IDStylePolygone,
                                  QNbVertex]));
    for j := 0 to High(MyPolygone.Sommets) do
    begin
      MyVertex := MyPolygone.Sommets[j];
      AfficherMessage(Format('-- Vertex: %d; Basepoint: %d; Offset = (%.3f, %.3f)',
                             [j, MyVertex.IDStation, MyVertex.Offset.X, MyVertex.Offset.Y]));
    end;
  end;
  {$ENDIF}
end;

function TConnexionOpenCaveMap.BDDExtractAllPolylinesOfCavite(const QIDCavite: integer): boolean;
var
  WU: String;
  Nb, NbV, i, j: Integer;
  QIdPolyligne: Int64;
  QNbVertex, QIdxVertex: LongInt;
  MyVertex : TVertexPolygon;
  MyPolyligne: TPolyLigne;
begin
  Result := false;
  AfficherMessage(Format('%s.BDDExtractAllPolylignesOfCavite: %d', [ClassName, QIDCavite]));
  DispMsgInLbl('Extraction des polylignes');
  WU := format('SELECT * FROM %s WHERE (%s div %d = %d) ORDER BY %s ASC;',
               [TABLE_POLYLIGNES,
                TBL_POLYGONES_COL_IDPoly, NB_MAX_OBJ_PAR_CAVITE, QIDCavite,
                TBL_POLYGONES_COL_IDPoly]);
  AfficherMessage(WU);
  FMainSqlQuery.SQL.Clear;
  FMainSqlQuery.SQL.Text := WU;
  Nb := 0;
  try
    FMainSqlQuery.Open;
    while (not FMainSqlQuery.EOF) do
    begin
      // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
      QIdPolyligne                  := StrToInt64Def(FMainSqlQuery.Fields[0].AsString, 0);
      MyPolyligne.IDGroupe          := StrToInt64Def(FMainSqlQuery.Fields[1].AsString, 0) mod NB_MAX_OBJ_PAR_CAVITE;
      MyPolyligne.IDStylePolyLine   := TNatureObjetCourbe(StrToIntDef(FMainSqlQuery.Fields[2].AsString, 0));                            // IDGroupe
      QNbVertex                     := StrToIntDef(FMainSqlQuery.Fields[3].AsString, 0);
      MyPolyligne.Closed            := (1 = StrToIntDef(FMainSqlQuery.Fields[4].AsString, 0));
      //AfficherMessage(Format('Polygone %d; Groupe: %d; Style: %d; NbVertex = %d',
      //                             [QIdPolyligne,
      //                              MyPolyligne.IDGroupe, MyPolyligne.IDStylePolyLine,
      //                              QNbVertex]));
      SetLength(MyPolyligne.Sommets, QNbVertex);
      //************************************************************************
      // Sous-requête pour les vertex
        WU := format('SELECT * FROM %s WHERE (%s = %d) ORDER BY %s ASC;',
                     [TABLE_VERTEX_POLYLIGNES,
                      TBL_VERTEX_SCRAPS_POLY_COL_IDPoly, QIdPolyligne,
                     'IdxVertex']);

        //AfficherMessage(WU);
        FSecSqlQuery.SQL.Clear;
        FSecSqlQuery.SQL.Text := WU;
        NbV := 0;
        try
          FSecSqlQuery.Open;
          while (not FSecSqlQuery.EOF) do
          begin
            // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
            QIdPolyligne := StrToInt64Def(FSecSqlQuery.Fields[0].AsString, 0);
            QIdxVertex  := StrToIntDef(FSecSqlQuery.Fields[1].AsString, 0);
            MyVertex.IDStation := StrToInt64Def(FSecSqlQuery.Fields[2].AsString, 0) mod MULT_IDX_CAVITE;
            MyVertex.Offset.X  := ConvertirEnNombreReel(FSecSqlQuery.Fields[3].AsString, 0.00);
            MyVertex.Offset.Y  := ConvertirEnNombreReel(FSecSqlQuery.Fields[4].AsString, 0.00);

            AfficherMessage(Format('-- Poly %d; Vertex: %d; Basepoint: %d; Offset = (%.3f, %.3f)',
                                         [QIdPolyligne, QIdxVertex,
                                          MyVertex.IDStation, MyVertex.Offset.X, MyVertex.Offset.Y
                                         ]));
            MyPolyligne.putVertex(NbV, MyVertex);            //MyPolyligne.Sommets[NbV] := MyVertex;
            FSecSqlQuery.Next;
            Inc(NbV);
          end;
        finally
          FSecSqlQuery.Close;
        end;
      // TODO Ajouter le polygone ici
      //************************************************************************
      FMyDocDessin.AddPolyLigne(MyPolyligne, true);
      FMainSqlQuery.Next;
      Inc(Nb);
    end;
    AfficherMessage(Format('%d polylignes', [Nb]));
  finally
    FMainSqlQuery.Close;
  end;

  // contrôle
  Nb := FMyDocDessin.GetNbPolylignes();
  AfficherMessage(Format('%d polylignes', [Nb]));
  {$IFDEF DISP_TO_CONSOLE}
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    MyPolyligne := FMyDocDessin.GetPolyligne(i);
    AfficherMessage(Format('Polyligne %d; Groupe: %d; Style: %d; NbVertex = %d',
                          [i,
                           MyPolyligne.IDGroupe, MyPolyligne.IDStylePolyLine,
                           QNbVertex]));

    for j := 0 to High(MyPolyligne.Sommets) do
    begin
      MyVertex := MyPolyligne.Sommets[j];
      AfficherMessage(Format('-- Vertex: %d; Basepoint: %d; Offset = (%.3f, %.3f)',
                             [j, MyVertex.IDStation, MyVertex.Offset.X, MyVertex.Offset.Y]));
    end;
  end;
  {$ENDIF}
end;
//TConnexionOpenCaveMap
function TConnexionOpenCaveMap.BDDExtractAllSimpleLinesOfCavite(const QIDCavite: integer): boolean;
var
  WU: String;
  Nb, NbV, i, j: Integer;
  QIdSimpleLigne: Int64;
  MySimpleLigne: TSimpleLigne;
begin
  Result := false;
  AfficherMessage(Format('%s.BDDExtractAllSimpleLinesOfCavite: %d', [ClassName, QIDCavite]));
  DispMsgInLbl('Extraction des lignes simples');
  WU := format('SELECT * FROM %s WHERE (%s div %d = %d) ORDER BY %s ASC;',
               [TABLE_SIMPLES_LIGNES,
                TBL_SLIGNES_COL_IDSimpleLigne, NB_MAX_OBJ_PAR_CAVITE, QIDCavite,
                TBL_SLIGNES_COL_IDSimpleLigne]);
  AfficherMessage(WU);
  FMainSqlQuery.SQL.Clear;
  FMainSqlQuery.SQL.Text := WU;
  Nb := 0;
  //try
    FMainSqlQuery.Open;
    while (not FMainSqlQuery.EOF) do
    begin
      // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
      QIdSimpleLigne                  := StrToInt64Def(FMainSqlQuery.Fields[0].AsString, 0);                              // IDSimpleLigne
      MySimpleLigne.IDGroupe          := StrToInt64Def(FMainSqlQuery.Fields[1].AsString, 0) mod NB_MAX_OBJ_PAR_CAVITE;    // IDGroupe
      MySimpleLigne.IDStyleLigne      := TNatureObjetLigne(StrToIntDef(FMainSqlQuery.Fields[2].AsString, 0));             // TypeLigne
      MySimpleLigne.IDBaseStExt1      := StrToInt64Def(FMainSqlQuery.Fields[3].AsString, 0) mod MULT_IDX_CAVITE;          // IDBaseStExt1
      MySimpleLigne.IDBaseStExt2      := StrToInt64Def(FMainSqlQuery.Fields[4].AsString, 0) mod MULT_IDX_CAVITE;          // IDBaseStExt2
      MySimpleLigne.OffsetExtr1.X     := ConvertirEnNombreReel(FMainSqlQuery.Fields[5].AsString, 0.00);                           // OffExtrLin1_X
      MySimpleLigne.OffsetExtr1.Y     := ConvertirEnNombreReel(FMainSqlQuery.Fields[6].AsString, 0.00);                           // OffExtrLin1_Y
      MySimpleLigne.OffsetExtr2.X     := ConvertirEnNombreReel(FMainSqlQuery.Fields[7].AsString, 0.00);                           // OffExtrLin2_X
      MySimpleLigne.OffsetExtr2.Y     := ConvertirEnNombreReel(FMainSqlQuery.Fields[8].AsString, 0.00);                           // OffExtrLin2_Y
      MySimpleLigne.ExtrLin1          := TExtremiteLigne(StrToIntDef(FMainSqlQuery.Fields[ 9].AsString, 0));              // ExtrLin1
      MySimpleLigne.ExtrLin2          := TExtremiteLigne(StrToIntDef(FMainSqlQuery.Fields[10].AsString, 0));              // ExtrLin2
      //************************************************************************

      // TODO Ajouter le polygone ici
      //************************************************************************
      FMyDocDessin.AddSimpleLigne(MySimpleLigne, true);
      FMainSqlQuery.Next;
      Inc(Nb);
    end;
  //finally
    FMainSqlQuery.Close;
  //end;

  // contrôle
  Nb := FMyDocDessin.GetNbSimpleLignes();
  AfficherMessage(Format('%d simples lignes', [Nb]));
  {$IFDEF DISP_TO_CONSOLE}
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    MySimpleLigne := FMyDocDessin.GetSimpleLigne(i);
    AfficherMessage(Format('Ligne %d; Style: %d; Groupe: %d; PT1: %d, PT2: %d, OffP1: %.2f, %.2f; OffP2: %.2f, %.2f; %d, %d',
                          [i, MySimpleLigne.IDStyleLigne, MySimpleLigne.IDGroupe,
                           MySimpleLigne.IDBaseStExt1, MySimpleLigne.IDBaseStExt2,
                           MySimpleLigne.OffsetExtr1.X, MySimpleLigne.OffsetExtr1.Y,
                           MySimpleLigne.OffsetExtr2.X, MySimpleLigne.OffsetExtr2.Y,
                           MySimpleLigne.ExtrLin1, MySimpleLigne.ExtrLin2
                           ]));
  end;
  {$ENDIF}
end;

function TConnexionOpenCaveMap.BDDExtractAllSymbolesOfCavite(const QIDCavite: integer): boolean;
var
  WU: String;
  Nb, NbV, i, j: Integer;
  MySymbole: TSymbole;
  QIdSymbole: Int64;
begin
  Result := false;
  AfficherMessage(Format('%s.BDDExtractAllSymbolesOfCavite: %d', [ClassName, QIDCavite]));
  DispMsgInLbl('Extraction des symboles');
  WU := format('SELECT * FROM %s WHERE (%s div %d = %d) ORDER BY %s ASC;',
               [TABLE_SYMBOLES,
                TBL_SYMBOLES_COL_IDSymbole, NB_MAX_OBJ_PAR_CAVITE, QIDCavite,
                TBL_SYMBOLES_COL_IDSymbole]);
  AfficherMessage(WU);
  FMainSqlQuery.SQL.Clear;
  FMainSqlQuery.SQL.Text := WU;
  Nb := 0;
  //try
    FMainSqlQuery.Open;
    while (not FMainSqlQuery.EOF) do
    begin
      // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
      QIdSymbole                  := StrToInt64Def(FMainSqlQuery.Fields[0].AsString, 0);                              // IDSymbole
      MySymbole.IDGroupe          := StrToInt64Def(FMainSqlQuery.Fields[1].AsString, 0) mod NB_MAX_OBJ_PAR_CAVITE;          // IDGroupe
      MySymbole.TypeObject        := TNatureObjetSymbole(StrToIntDef(FMainSqlQuery.Fields[2].AsString, 0));           // TypeSymbole
      MySymbole.IDBaseStation     := StrToInt64Def(FMainSqlQuery.Fields[3].AsString, 0) mod MULT_IDX_CAVITE;          // IDBasePoint
      MySymbole.Offset.X          := ConvertirEnNombreReel(FMainSqlQuery.Fields[4].AsString, 0.00);                           // Offset_X
      MySymbole.Offset.Y          := ConvertirEnNombreReel(FMainSqlQuery.Fields[5].AsString, 0.00);                           // Offset_Y
      MySymbole.AngleRot          := ConvertirEnNombreReel(FMainSqlQuery.Fields[6].AsString, 0.00);                           // AngleRot
      MySymbole.ScaleX            := ConvertirEnNombreReel(FMainSqlQuery.Fields[7].AsString, 0.00);                           // Scale_X
      MySymbole.ScaleY            := ConvertirEnNombreReel(FMainSqlQuery.Fields[8].AsString, 0.00);                           // Scale_Y
      MySymbole.UnTag             := StrToIntDef(FMainSqlQuery.Fields[ 9].AsString, 0);                               // UnTag
      MySymbole.TagTexte          := Trim(FMainSqlQuery.Fields[10].AsString);                                         // TagTexte
      //************************************************************************

      // TODO Ajouter le symbole ici
      //************************************************************************
      FMyDocDessin.AddSymbole(MySymbole, true);
      FMainSqlQuery.Next;
      Inc(Nb);
    end;

  //finally
    FMainSqlQuery.Close;
  //end;


  // contrôle
  Nb := FMyDocDessin.GetNbSymboles();
  AfficherMessage(Format('%d symboles', [Nb]));
  {$IFDEF DISP_TO_CONSOLE}
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    MySymbole := FMyDocDessin.GetSymbole(i);
    AfficherMessage(Format('Symbole %d; TypeStyle: %d; Groupe: %d; PT1: %d, OffP1: %.2f, %.2f; Angle: %.2f; Scale:  %.2f, %.2f; %d, %s',
                          [i, MySymbole.TypeObject, MySymbole.IDGroupe,
                           MySymbole.IDBaseStation, MySymbole.Offset.X, MySymbole.Offset.Y,
                           MySymbole.AngleRot, MySymbole.ScaleX, MySymbole.ScaleY,
                           MySymbole.UnTag, MySymbole.TagTexte
                           ]));
  end;
  {$ENDIF}
end;

function TConnexionOpenCaveMap.BDDExtractAllTextesOfCavite(const QIDCavite: integer): boolean;
var
  WU: String;
  Nb, i: Integer;
  MyTexte : TTextObject;
  QIdTexte: Int64;
begin
  Result := false;
  AfficherMessage(Format('%s.BDDExtractAllTextesOfCavite: %d', [ClassName, QIDCavite]));
  DispMsgInLbl('Extraction des textes');
  WU := format('SELECT * FROM %s WHERE (%s div %d = %d) ORDER BY %s ASC;',
               [TABLE_TEXTES,
                TBL_TEXTES_COL_IDTexte, NB_MAX_OBJ_PAR_CAVITE, QIDCavite,
                TBL_TEXTES_COL_IDTexte]);
  AfficherMessage(WU);
  FMainSqlQuery.SQL.Clear;
  FMainSqlQuery.SQL.Text := WU;
  Nb := 0;
  //try
    FMainSqlQuery.Open;
    while (not FMainSqlQuery.EOF) do
    begin
      // Les fonctions AsLargeInt et AsDouble sont connues mais pas utilisées
      QIdTexte                  := StrToInt64Def(FMainSqlQuery.Fields[0].AsString, 0);                              // IDTexte
      MyTexte.IDGroupe          := StrToInt64Def(FMainSqlQuery.Fields[1].AsString, 0) mod NB_MAX_OBJ_PAR_CAVITE;    // IDGroupe
      MyTexte.IDStyleTexte      := TNatureObjetTexte(StrToIntDef(FMainSqlQuery.Fields[2].AsString, 0));             // TypeTexte
      MyTexte.IDBaseStation     := StrToInt64Def(FMainSqlQuery.Fields[3].AsString, 0) mod MULT_IDX_CAVITE;          // IDBasePoint
      MyTexte.Offset.X          := ConvertirEnNombreReel(FMainSqlQuery.Fields[4].AsString, 0.00);                           // Offset_X
      MyTexte.Offset.Y          := ConvertirEnNombreReel(FMainSqlQuery.Fields[5].AsString, 0.00);                           // Offset_Y
      MyTexte.Alignment         := StrToIntDef(FMainSqlQuery.Fields[6].AsString, 0);                                // Alignment
      MyTexte.MaxLength         := StrToIntDef(FMainSqlQuery.Fields[7].AsString, 0);                                // MaxLength
      MyTexte.Text              := Trim(FMainSqlQuery.Fields[8].AsString);                                         // Texte
      //************************************************************************

      // TODO Ajouter le polygone ici
      //************************************************************************
      FMyDocDessin.AddTexte(MyTexte, true);
      FMainSqlQuery.Next;
      Inc(Nb);
    end;
  //finally
    FMainSqlQuery.Close;
  //end;


  // contrôle
  Nb := FMyDocDessin.GetNbTextes();
  AfficherMessage(Format('%d textes', [Nb]));
  {$IFDEF DISP_TO_CONSOLE}
  if (Nb = 0) then Exit;
  for i := 0 to Nb - 1 do
  begin
    MyTexte := FMyDocDessin.GetTexte(i);
    AfficherMessage(Format('Texte %d; TypeStyle: %d; Groupe: %d; PT1: %d, OffP1: %.2f, %.2f; Alignement: %d; MaxLength: %d, "%s"',
                          [i, MyTexte.IDStyleTexte, MyTexte.IDGroupe,
                           MyTexte.IDBaseSt, MyTexte.Offset.X, MyTexte.Offset.Y,
                           MyTexte.Alignment, MyTexte.MaxLength,
                           MyTexte.Text
                           ]));
  end;
  {$ENDIF}
end;
//******************************************************************************
function TConnexionOpenCaveMap.DetruireCaviteInDB(const NumCavite: integer): Integer;
const
  DELETE_WHERE_IDOBJET_EQUAL_NB = 'DELETE FROM %s WHERE (%s div %d = %d);';
var
  WU: String;
  procedure S666(const TBL, R: string);
  begin
    try
      AfficherMessage('--- > Table: ' + TBL);
      SendSQLCommand(R);
    except
      AfficherMessage('** Pas de données dans la table ' + TBL);
    end;
  end;
begin
  WU := '';
  result := 0;
  AfficherMessage(Format('%s.DetruireCaviteInDB: %d', [ClassName, NumCavite]));
  try
  // détruire les enregistrements précédents
    Result := -1; WU := TABLE_BASEPOINTS;
    S666(TABLE_BASEPOINTS        , Format('DELETE FROM %s WHERE (ABS(%s div %d) = %d);', [TABLE_BASEPOINTS, 'IDBasePoint', MULT_IDX_CAVITE, NumCavite]));
    Result := Result - 1; WU := TABLE_GROUPES;
    S666(TABLE_GROUPES           , Format(DELETE_WHERE_IDOBJET_EQUAL_NB, [TABLE_GROUPES, 'IDGroupe', NB_MAX_OBJ_PAR_CAVITE, NumCavite]));
    Result := Result - 1; WU := TABLE_SCRAPS;
    S666(TABLE_SCRAPS            , Format(DELETE_WHERE_IDOBJET_EQUAL_NB, [TABLE_SCRAPS, 'IDScrap', NB_MAX_OBJ_PAR_CAVITE, NumCavite]));
    Result := Result - 1; WU := TABLE_VERTEX_SCRAPS;
    S666(TABLE_VERTEX_SCRAPS     , Format(DELETE_WHERE_IDOBJET_EQUAL_NB, [TABLE_VERTEX_SCRAPS, 'IDPoly', NB_MAX_OBJ_PAR_CAVITE, NumCavite]));
    Result := Result - 1; WU := TABLE_COURBES;
    S666(TABLE_COURBES           , Format(DELETE_WHERE_IDOBJET_EQUAL_NB, [TABLE_COURBES, 'IDCourbe', NB_MAX_OBJ_PAR_CAVITE, NumCavite]));
    Result := Result - 1; WU := TABLE_ARCS_COURBES;
    S666(TABLE_ARCS_COURBES      , Format(DELETE_WHERE_IDOBJET_EQUAL_NB, [TABLE_ARCS_COURBES, 'IDCourbe', NB_MAX_OBJ_PAR_CAVITE, NumCavite]));
    Result := Result - 1; WU := TABLE_POLYGONES;
    S666(TABLE_POLYGONES         , Format(DELETE_WHERE_IDOBJET_EQUAL_NB, [TABLE_POLYGONES, TBL_POLYGONES_COL_IDPoly, NB_MAX_OBJ_PAR_CAVITE, NumCavite]));
    Result := Result - 1; WU := TABLE_VERTEX_POLYGONES;
    S666(TABLE_VERTEX_POLYGONES  , Format(DELETE_WHERE_IDOBJET_EQUAL_NB, [TABLE_VERTEX_POLYGONES, 'IDPoly', NB_MAX_OBJ_PAR_CAVITE, NumCavite]));
    Result := Result - 1; WU := TABLE_POLYLIGNES;
    S666(TABLE_POLYLIGNES        , Format(DELETE_WHERE_IDOBJET_EQUAL_NB, [TABLE_POLYLIGNES, TBL_POLYGONES_COL_IDPoly, NB_MAX_OBJ_PAR_CAVITE, NumCavite]));
    Result := Result - 1; WU := TABLE_VERTEX_POLYLIGNES;
    S666(TABLE_VERTEX_POLYLIGNES , Format(DELETE_WHERE_IDOBJET_EQUAL_NB, [TABLE_VERTEX_POLYLIGNES, 'IDPoly', NB_MAX_OBJ_PAR_CAVITE, NumCavite]));
    Result := Result - 1; WU := TABLE_SIMPLES_LIGNES;
    S666(TABLE_SIMPLES_LIGNES    , Format(DELETE_WHERE_IDOBJET_EQUAL_NB, [TABLE_SIMPLES_LIGNES, 'IDSimpleLigne', NB_MAX_OBJ_PAR_CAVITE, NumCavite]));
    Result := Result - 1; WU := TABLE_SYMBOLES;
    S666(TABLE_SYMBOLES          , Format(DELETE_WHERE_IDOBJET_EQUAL_NB, [TABLE_SYMBOLES, 'IDSymbole', NB_MAX_OBJ_PAR_CAVITE, NumCavite]));
    Result := Result - 1; WU := TABLE_TEXTES;
    S666(TABLE_TEXTES            , Format(DELETE_WHERE_IDOBJET_EQUAL_NB, [TABLE_TEXTES, 'IDTexte', NB_MAX_OBJ_PAR_CAVITE, NumCavite]));
    Result := 0;
  except
    AfficherMessage(Format('Echec en destruction de la table (%d) %s', [Result, WU]));
  end;
end;

procedure TConnexionOpenCaveMap.ViderTable(const NomTable: string);
begin
  SendSQLCommand('TRUNCATE TABLE '+ NomTable + ';');
end;

procedure TConnexionOpenCaveMap.ViderTablesObjetsGraphiques(const PreserveCenterlines: boolean);
begin
  AfficherMessage(Format('%s.ViderTablesObjetsGraphiques()', [ClassName]));
  if (not PreserveCenterlines) then ViderTable(TABLE_BASEPOINTS);
  ViderTable(TABLE_GROUPES);
  ViderTable(TABLE_SCRAPS);
  ViderTable(TABLE_VERTEX_SCRAPS);
  ViderTable(TABLE_COURBES);
  ViderTable(TABLE_ARCS_COURBES);
  ViderTable(TABLE_POLYGONES);
  ViderTable(TABLE_VERTEX_POLYGONES);
  ViderTable(TABLE_POLYLIGNES);
  ViderTable(TABLE_VERTEX_POLYLIGNES);
  ViderTable(TABLE_SIMPLES_LIGNES);
  ViderTable(TABLE_SYMBOLES);
  ViderTable(TABLE_TEXTES);
end;

//******************************************************************************

procedure TConnexionOpenCaveMap.ExportTableBasePointsToDatabase(const NumCavite: integer; const DoViderTable: boolean; const CodeEPSG: integer);
var
  i, Nb, NbViseesAntennesExportees: Integer;
  QBP: TBaseStation;
  QAT, fatche: String;
  procedure S666(const BP: TBaseStation);
  var
    WU: String;
    QQ: Int64;
    BP_PosExtr0_Lon, BP_PosExtr0_Lat: double;
    BP_PosStation_Lon, BP_PosStation_Lat: double;
    BP_PosPG_Lon, BP_PosPG_Lat: double;
    BP_PosPD_Lon, BP_PosPD_Lat: double;
  begin
    if (BP.IDStation > 0) then
      QQ := CalcIdxBasepointForCavite(NumCavite, BP.IDStation)
    else
      QQ := CalcIdxBasepointForCavite(NumCavite, -NbViseesAntennesExportees);
    try
      DeprojeterCoordsToWGS(BP.PosExtr0.X, BP.PosExtr0.Y, BP_PosExtr0_Lon, BP_PosExtr0_Lat);
      DeprojeterCoordsToWGS(BP.PosStation.X, BP.PosStation.Y, BP_PosStation_Lon, BP_PosStation_Lat);
      DeprojeterCoordsToWGS(BP.PosPG.X, BP.PosPG.Y, BP_PosPG_Lon, BP_PosPG_Lat);
      DeprojeterCoordsToWGS(BP.PosPD.X, BP.PosPD.Y, BP_PosPD_Lon, BP_PosPD_Lat);

      WU := Format('INSERT INTO %s (IDBasePoint, IDCavite, Couleur, ' +
                   'PosExtr0_Lon, PosExtr0_Lat, PosExtr0_Alt, ' +
                   'PosStation_Lon, PosStation_Lat, PosStation_Alt, ' +
                   'Pos_PG_Lon, Pos_PG_Lat, Pos_PG_Alt, ' +
                   'Pos_PD_Lon, Pos_PD_Lat, Pos_PD_Alt, ' +
                   'IDTerrain) ' +
                   'VALUES (%d, %d, %d, ' +
                           '%s, %s, %s, ' +
                           '%s, %s, %s, ' +
                           '%s, %s, %s, ' +
                           '%s, %s, %s, ' +
                           '"%s");',
               [TABLE_BASEPOINTS,
                QQ, NumCavite, 0,
                BP_PosExtr0_Lon, BP_PosExtr0_Lat, BP.PosExtr0.Z,
                BP_PosStation_Lon, BP_PosStation_Lat, BP.PosStation.Z,
                FormatterNombreWithDotDecimal(BP_PosPG_Lon, 6),
                FormatterNombreWithDotDecimal(BP_PosPG_Lat, 6),
                FormatterNombreWithDotDecimal(BP.PosPG.Z  , 3),
                FormatterNombreWithDotDecimal(BP_PosPD_Lon, 6),
                FormatterNombreWithDotDecimal(BP_PosPD_Lat, 6),
                FormatterNombreWithDotDecimal(BP.PosPD.Z  , 3),
                mysqli_real_escape_string(BP.IDTerrain)
                ]);
      SendSQLCommand(WU);
    except
    end;
  end;
begin
  Nb := FMyDocDessin.GetNbBaseStations();
  AfficherMessage(Format('%s.ExportTableBasePointsToDatabase: %d - EPSG:%d', [ClassName, Nb, CodeEPSG]));
  DispMsgInLbl(Format('Export des centerlines: %d basepoints', [Nb]));
  FCodeEPSG := CodeEPSG;
  //try
    if (DoViderTable) then ViderTable(TABLE_BASEPOINTS);
    NbViseesAntennesExportees := 1;
    BeginTransaction();
      for i := 0 to Nb - 1 do
      begin
        QBP := FMyDocDessin.GetBaseStation(i);
        S666(QBP);
        NbViseesAntennesExportees += 1;
        if (NbViseesAntennesExportees mod 1000 = 0) then
          DispMsgInLbl(Format('Export des centerlines: %d / %d', [NbViseesAntennesExportees, Nb]));
      end;
    CommitTransaction();
  //except
  //end;
  Application.ProcessMessages;
end;

procedure TConnexionOpenCaveMap.ExportTableGroupesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
var
  i, Nb: Integer;
  QBP: TGroupeEntites;
  QAT: String;
  procedure QExportGroupe(const BP: TGroupeEntites);
  var
    WU: String;
  begin
    try
      WU := Format('REPLACE INTO %s (IDGroupe, IDSuperGroupe, CouleurGroupe, DecalageX, DecalageY, ZOrder, NomGroupe, Filtres) ' +
                       'VALUES (%d, %d, %d, %s, %s, %s, "%s", "%s");',
               [TABLE_GROUPES,
                CalcIdxGroupeForCavite(NumCavite, BP.IDGroupeEntites),
                0, //BP.IDSuperGroupe,
                BP.CouleurGroupe,
                FormatterNombreWithDotDecimal(BP.Decalage.X, 6),
                FormatterNombreWithDotDecimal(BP.Decalage.Y, 6),
                FormatterNombreWithDotDecimal(BP.ZOrder    , 3),
                mysqli_real_escape_string(BP.NomGroupe),
                mysqli_real_escape_string(BP.Filtres)]);
      AfficherMessage(WU);
      SendSQLCommand(WU);
    except
      AfficherMessage('**** ' + WU);
    end;
  end;
begin
  Nb := FMyDocDessin.GetNbGroupes();
  AfficherMessage(Format('%s.ExportTableGroupesToDatabase: %d', [ClassName, Nb]));
  DispMsgInLbl(format('Export des %d groupes', [Nb]));
  //try
    if(DoViderTable) then ViderTable(TABLE_GROUPES);
    BeginTransaction();
      for i := 0 to Nb - 1 do
      begin
        QBP := FMyDocDessin.GetGroupe(i);
        affichermessage(format('-- Groupe: %d - %s', [QBP.IDGroupeEntites, QBP.NomGroupe]));
        QExportGroupe(QBP);
      end;
    CommitTransaction();
  //except
  //end;
  Application.ProcessMessages;
end;


procedure TConnexionOpenCaveMap.ExportTableScrapsToDatabase(const NumCavite: integer; const DoViderTable: boolean);
var
  Nb, i: Integer;
  MyScrap: TScrap;
  QAT: String;
  procedure QExporterScrap(const BP: TScrap; const QIdx: integer);
  var
    n, a: Integer;
    WU: String;
    QIdxScrap: Int64;
  begin
    //try
      QIdxScrap := CalcIdxObjetForCavite(NumCavite, QIdx);
      WU := Format('INSERT INTO %s (IDScrap, IDGroupe, Couleur, Opacite, NbVertex) ' +
                   'VALUES (%d, %d, %d, %d, %d);',
               [TABLE_SCRAPS,
                QIdxScrap,
                CalcIdxGroupeForCavite(NumCavite, BP.IDGroupe),
                BP.Couleur,
                BP.Opacite,
                1 + High(BP.Sommets)
               ]);
      if (SendSQLCommand(WU)) then
      begin
        n := High(BP.Sommets);
        AfficherMessage(Format('Export des %d sommets du scrap %d', [n, QIdx]));
        for a := 0 to n do ExportVertexToSQL(NumCavite, TABLE_VERTEX_SCRAPS, BP.getVertex(a), QIdxScrap, a);
      end;

    //except
    //end;
  end;
begin
  Nb := FMyDocDessin.GetNbScraps();
  AfficherMessage(Format('%s.ExportTableScrapsToDatabase: %d', [ClassName, Nb]));
  DispMsgInLbl(format('Export des %d scraps', [Nb]));
  try
    if (Nb = 0) then Exit;
    if (DoViderTable) then
    begin
      ViderTable(TABLE_SCRAPS);
      ViderTable(TABLE_VERTEX_SCRAPS);
    end;
    BeginTransaction();
      for i := 0 to Nb -1 do
      begin
        MyScrap := FMyDocDessin.GetScrap(i);
        QExporterScrap(MyScrap, i);
      end;
    CommitTransaction();
  except
  end;
  Application.ProcessMessages;
end;
procedure TConnexionOpenCaveMap.ExportTableCourbesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
var
  MyCourbe: TCourbe;
  Nb, i: Integer;
  QAT: String;
  procedure QExporterArc(const A: TArcCourbe; const QIdxCourbe: Int64; const QIdxArc: integer);
  var
    WU: String;
  begin
    WU := Format('INSERT INTO %s (IDCourbe, IdxArc, IDStationP1, IDStationP2, ' +
                 'OffsetP1_X, OffsetP1_Y, TangP1_X, TangP1_Y, ' +
                 'OffsetP2_X, OffsetP2_Y, TangP2_X, TangP2_Y) ' +
                 'VALUES (%d, %d, %d, %d, ' +
                 '%.6f, %.6f, %.6f, %.6f, ' +
                 '%.6f, %.6f, %.6f, %.6f);',
             [TABLE_ARCS_COURBES,
              QIdxCourbe, QIdxArc,
              CalcIdxBasepointForCavite(NumCavite, A.IDStationP1),
              CalcIdxBasepointForCavite(NumCavite, A.IDStationP2),
              A.OffsetP1.X, A.OffsetP1.Y, A.TangP1.X, A.TangP1.Y,
              A.OffsetP2.X, A.OffsetP2.Y, A.TangP2.X, A.TangP2.Y]);
    FMainSqlQuery.SQL.Text := WU;
    FMainSqlQuery.ExecSQL;
  end;

  procedure QExporterCourbe(const C: TCourbe; const QIdx: integer);
  var
    WU: String;
    QIdxCourbe: Int64;
    a, n: Integer;
  begin
    try
      QIdxCourbe := CalcIdxObjetForCavite(NumCavite,  QIdx);
      WU := Format('INSERT INTO %s (IDCourbe, IDGroupe, TypeCourbe, NbArcs) VALUES (%d, %d, %d, %d);',
               [TABLE_COURBES,
                QIdxCourbe,
                CalcIdxGroupeForCavite(NumCavite, C.IDGroupe),
                C.IDStyleCourbe,
                1 + High(C.Arcs)]);
      if (SendSQLCommand(WU)) then
      begin
        n := High(C.Arcs);
        for a := 0 to n do QExporterArc(C.Arcs[a], QIdxCourbe, a);
      end;
    except
    end;
  end;

begin
  Nb := FMyDocDessin.GetNbCourbes();
  AfficherMessage(Format('%s.ExportTableCourbesToDatabase: %d', [ClassName, Nb]));
  if (Nb = 0) then Exit;
  DispMsgInLbl(format('Export des %d courbes', [Nb]));
  try

    if (DoViderTable) then
    begin
      ViderTable(TABLE_COURBES);
      ViderTable(TABLE_ARCS_COURBES);
    end;
    BeginTransaction();
      for i := 0 to Nb -1 do
      begin
        MyCourbe := FMyDocDessin.GetCourbe(i);
        QExporterCourbe(MyCourbe, i);
      end;
    CommitTransaction();
  except
  end;
  Application.ProcessMessages;
end;



procedure TConnexionOpenCaveMap.ExportTablePolygonesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
var
  Nb, i: Integer;
  MyPolygone: TPolygone;
  QAT: String;
  procedure QExporterPolygone(const P: TPolygone; const QIdx: integer);
  var
    WU: String;
    a, n: Integer;
    QIdxPolygone: Int64;
  begin
    //try
      QIdxPolygone := CalcIdxObjetForCavite(NumCavite, QIdx);
      WU := Format('INSERT INTO %s (IDPoly, IDGroupe, TypePoly, NbVertex) VALUES (%d, %d, %d, %d);',
               [TABLE_POLYGONES,
                QIdxPolygone,
                CalcIdxGroupeForCavite(NumCavite, P.IDGroupe),
                P.IDStylePolygone,
                1 + High(P.Sommets)]);
      // vertex
      if (SendSQLCommand(WU)) then
      begin
        n := High(P.Sommets);
        AfficherMessage(Format('Export des %d points du polygone %d', [n, QIdx]));

        for a := 0 to n do ExportVertexToSQL(NumCavite, TABLE_VERTEX_POLYGONES, P.getVertex(a), QIdxPolygone, a);
      end;
    //except
    //end;
  end;
begin
  Nb := FMyDocDessin.GetNbPolygones();
  AfficherMessage(Format('%s.ExportTablePolygonesToDatabase: %d', [ClassName, Nb]));
  if (Nb = 0) then Exit;
  DispMsgInLbl(format('Export des %d polygones', [Nb]));
  try

    if (DoViderTable) then
    begin
      ViderTable(TABLE_POLYGONES);
      ViderTable(TABLE_VERTEX_POLYGONES);
    end;
    BeginTransaction();
      for i := 0 to Nb -1 do
      begin
        MyPolygone := FMyDocDessin.GetPolygone(i);
        QExporterPolygone(MyPolygone, i);
      end;
    CommitTransaction();
  except

  end;
  Application.ProcessMessages;
end;
procedure TConnexionOpenCaveMap.ExportTablePolylinesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
var
  Nb, i: Integer;
  MyPolyLigne: TPolyLigne;
  QAT: String;
  procedure QExporterPolyligne(const P: TPolyLigne; const QIdx: integer);
  var
    WU: String;
    QIdxPolyligne: Int64;
    a, n: Integer;
  begin
    //try
      QIdxPolyligne := CalcIdxObjetForCavite(NumCavite, QIdx);
      WU := Format('INSERT INTO %s (IDPoly, IDGroupe, TypePoly, NbVertex, PolyClosed) VALUES (%d, %d, %d, %d, %d);',
               [TABLE_POLYLIGNES,
                QIdxPolyligne,
                CalcIdxGroupeForCavite(NumCavite, P.IDGroupe),
                P.IDStylePolyLine,
                1 + High(P.Sommets),
                IIF(P.Closed, 1, 0)]);
      if (SendSQLCommand(WU)) then
      begin
        n := High(P.Sommets);
        for a := 0 to n do ExportVertexToSQL(NumCavite, TABLE_VERTEX_POLYLIGNES, P.getVertex(a), QIdxPolyligne, a);
      end;
    //except
    //end;
  end;
begin
  Nb := FMyDocDessin.GetNbPolylignes();
  AfficherMessage(Format('%s.ExportTablePolylinesToDatabase: %d', [ClassName, Nb]));
  if (Nb = 0) then Exit;
  DispMsgInLbl(format('Export des %d polylignes', [Nb]));
  try
    if (DoViderTable) then
    begin
      ViderTable(TABLE_POLYLIGNES);
      ViderTable(TABLE_VERTEX_POLYLIGNES);
    end;
    BeginTransaction();
      for i := 0 to Nb -1 do
      begin
        MyPolyLigne := FMyDocDessin.GetPolyligne(i);
        QExporterPolyligne(MyPolyLigne, i);
      end;
    CommitTransaction();
  except
  end;
  Application.ProcessMessages;
end;


procedure TConnexionOpenCaveMap.ExportTableSimplesLignesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
var
  Nb, i: Integer;
  MySimpleLigne: TSimpleLigne;
  QAT: String;
  procedure QExporterSimpleLigne(const BP: TSimpleLigne; const QIdx: integer);
  var
    n, a       : Integer;
    QIdxSimpleLigne: Int64;
    WU: String;
  begin
    try
      QIdxSimpleLigne := CalcIdxObjetForCavite(NumCavite, QIdx);
      WU := Format('INSERT INTO %s (IDSimpleLigne, IDGroupe, TypeLigne, IDBaseStExt1, IDBaseStExt2, OffExtrLin1_X, OffExtrLin1_Y, OffExtrLin2_X, OffExtrLin2_Y) ' +
                   'VALUES (%d, %d, %d, %d, %d, %.6f, %.6f, %.6f, %.6f);',
               [TABLE_SIMPLES_LIGNES,
                QIdxSimpleLigne,
                CalcIdxGroupeForCavite(NumCavite, BP.IDGroupe),
                BP.IDStyleLigne,
                CalcIdxBasepointForCavite(NumCavite, BP.IDBaseStExt1),
                CalcIdxBasepointForCavite(NumCavite, BP.IDBaseStExt2),
                BP.OffsetExtr1.X, BP.OffsetExtr1.Y,
                BP.OffsetExtr2.X, BP.OffsetExtr2.Y
               ]);
      SendSQLCommand(WU);
    except;

    end;
  end;
begin
  try
    Nb := FMyDocDessin.GetNbSimpleLignes();
    AfficherMessage(Format('%s.ExportTableSimplesLignessToDatabase: %d', [ClassName, Nb]));
    if (Nb = 0) then Exit;
    DispMsgInLbl(format('Export des %d lignes simples', [Nb]));
    if (DoViderTable) then ViderTable(TABLE_SIMPLES_LIGNES);
    BeginTransaction();
      for i := 0 to Nb -1 do
      begin
        MySimpleLigne := FMyDocDessin.GetSimpleLigne(i);
        QExporterSimpleLigne(MySimpleLigne, i);
      end;
    CommitTransaction();
  except

  end;
  Application.ProcessMessages;
end;


procedure TConnexionOpenCaveMap.ExportTableSymbolesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
var
  Nb, i: Integer;
  MySymbole: TSymbole;
  QAT: String;
  procedure QExporterSymbole(const BP: TSymbole; const QIdx: integer);
  var
    n, a       : Integer;
    QIdxSymbole: Int64;
    WU: String;
  begin
    try
      QIdxSymbole := CalcIdxObjetForCavite(NumCavite, QIdx);
      WU := Format('INSERT INTO %s (IDSymbole, IDGroupe, TypeSymbole, IDBasePoint, Offset_X, Offset_Y, AngleRot, Scale_X, Scale_Y, UnTag, TagTexte) ' +
                   'VALUES (%d, %d, %d, %d, %s, %s, %s, %s, %s, %d, "%s");',
               [TABLE_SYMBOLES,
                QIdxSymbole,
                CalcIdxGroupeForCavite(NumCavite , BP.IDGroupe),
                BP.TypeObject,
                CalcIdxBasepointForCavite(NumCavite, BP.IDBaseStation),
                FormatterNombreWithDotDecimal(BP.Offset.X,   6),
                FormatterNombreWithDotDecimal(BP.Offset.Y,   6),
                FormatterNombreWithDotDecimal(BP.AngleRot,   3),
                FormatterNombreWithDotDecimal(BP.ScaleX  ,   3),
                FormatterNombreWithDotDecimal(BP.ScaleY  ,   3),
                BP.UnTag,
                mysqli_real_escape_string(BP.TagTexte)
               ]);
      SendSQLCommand(WU);
    except
    end;
  end;
begin
  try
    Nb := FMyDocDessin.GetNbSymboles();
    AfficherMessage(Format('%s.ExportTableSymbolesToDatabase: %d', [ClassName, Nb]));
    if (Nb = 0) then Exit;
    DispMsgInLbl(format('Export des %d symboles', [Nb]));
    if (DoViderTable) then ViderTable(TABLE_SYMBOLES);
    BeginTransaction();
      for i := 0 to Nb -1 do
      begin
        MySymbole := FMyDocDessin.GetSymbole(i);
        QExporterSymbole(MySymbole, i);
      end;
    CommitTransaction();
  except
  end;
  Application.ProcessMessages;
end;
procedure TConnexionOpenCaveMap.ExportTableTextesToDatabase(const NumCavite: integer; const DoViderTable: boolean);
var
  Nb, i: Integer;
  MyTexte: TTextObject;
  QAT: String;
  procedure QExporterTexte(const BP: TTextObject; const QIdx: integer);
  var
    n, a       : Integer;
    QIdxTexte  : Int64;
    WU: String;
  begin
    QIdxTexte   := CalcIdxObjetForCavite(NumCavite, QIdx);
    WU := Format('INSERT INTO %s (IDTexte, IDGroupe, TypeTexte, IDBasePoint, Offset_X, Offset_Y, Alignment, MaxLength, Texte) ' +
                 'VALUES (%d, %d, %d, %d, %.6f, %.6f, %d, %d, "%s");',
             [TABLE_TEXTES,
              QIdxTexte,
              CalcIdxGroupeForCavite(NumCavite, BP.IDGroupe),
              BP.IDStyleTexte,
              CalcIdxBasepointForCavite(NumCavite, BP.IDBaseStation),
              BP.Offset.X, BP.Offset.Y,
              BP.Alignment, BP.MaxLength,
              mysqli_real_escape_string(BP.Text)
             ]);
    SendSQLCommand(WU);
  end;
begin
  try
    Nb := FMyDocDessin.GetNbTextes();
    AfficherMessage(Format('%s.ExportTableTextesToDatabase: %d', [ClassName, Nb]));
    if (Nb = 0) then Exit;
    DispMsgInLbl(format('Export des %d objets texte', [Nb]));
    if (DoViderTable) then ViderTable(TABLE_TEXTES);
    BeginTransaction();
      for i := 0 to Nb -1 do
      begin
        MyTexte := FMyDocDessin.GetTexte(i);
        QExporterTexte(MyTexte, i);
      end;
    CommitTransaction();
  except
  end;
end;
// TConnexionOpenCaveMap

procedure TConnexionOpenCaveMap.ExportVertexToSQL(const NumCavite: integer; const TableVertex: string; const V: TVertexPolygon;  const QIdxPoly: Int64; const QIdxVtx: integer);
var
  WU: String;
begin
  WU := Format('INSERT INTO %s (IDPoly, IdxVertex, IDBasePoint, Offset_X, Offset_Y) ' +
              'VALUES (%d, %d, %d, %.6f, %.6f);',
           [TableVertex,
            QIdxPoly,
            QIdxVtx,
            CalcIdxBasepointForCavite(NumCavite, V.IDStation),
            V.Offset.X, V.Offset.Y]);
  FMainSqlQuery.SQL.Text := WU;
  FMainSqlQuery.ExecSQL;
end;
// extraction d'enregistrements isolés
function TConnexionOpenCaveMap.ExtractCaviteDirectlyFromBDD(const QIdxCavite: Int64; out QCavite: TCaviteInBDD): boolean;
var
  n: Integer;
  Q: String;
begin
  Result := false;
  AfficherMessage(Format('%s.ExtractCaviteDirectlyFromBDD()', [ClassName]));
  if (not FSqlConnexion.Connected) then Exit;
  Q := Format('SELECT * FROM %s WHERE (IDCavite=%d);', [TABLE_CAVITES]);
  AfficherMessage(Q);
  FListeCavitesInBDD.ClearListe();
  try
    FSecSqlQuery.SQL.Clear;
    FSecSqlQuery.SQL.Text := Q;
    FSecSqlQuery.Open;
    n := 0;
    while not FSecSqlQuery.EOF do
    begin
      QCavite.IDCavite           := StrToIntDef(FSecSqlQuery.Fields[0].AsString, 0);
      QCavite.NomCavite          := Trim(FSecSqlQuery.Fields[1].AsString);
      QCavite.CodeEPSG           := StrToIntDef(FSecSqlQuery.Fields[2].AsString, 0);
      QCavite.IDUserCreateur     := StrToIntDef(FSecSqlQuery.Fields[3].AsString, 0);
      QCavite.DateAjout          := DateSqlToTDateTime(FSecSqlQuery.Fields[4].AsString);
      QCavite.IDLastUser         := StrToIntDef(FSecSqlQuery.Fields[5].AsString, 0);
      QCavite.DateLastModif      := DateSqlToTDateTime(FSecSqlQuery.Fields[6].AsString);
      QCavite.IsEditing          := (1 = StrToIntDef(FSecSqlQuery.Fields[7].AsString, 0));
      QCavite.NbGroupes          := GetNbGroupesForCavite(QCavite.IDCavite);
      QCavite.NbScraps           := GetNbScrapsForCavite(QCavite.IDCavite);
      QCavite.NbCourbes          := GetNbCourbesForCavite(QCavite.IDCavite);
      QCavite.NbPolygones        := GetNbPolygonesForCavite(QCavite.IDCavite);
      QCavite.NbPolylignes       := GetNbPolylignesForCavite(QCavite.IDCavite);
      QCavite.NbSimplesLignes    := GetNbSimplesLignesForCavite(QCavite.IDCavite);
      QCavite.NbSymboles         := GetNbSymbolesForCavite(QCavite.IDCavite);
      QCavite.NbTextes           := GetNbTextesForCavite(QCavite.IDCavite);
      //*)
      FSecSqlQuery.Next;
    end;
    Result := (n = 1);
  finally
    FSecSqlQuery.Close;
  end;
end;

function TConnexionOpenCaveMap.ExtractUserDirectlyFromBDD(const QIdx: integer; out QUser: TUtilisateurOCM): boolean;
var
  Nb: Integer;
  WU: String;
begin
  Result := false;
  if (not FSqlConnexion.Connected) then Exit;
  WU := format('SELECT * FROM %s WHERE (IDUtilisateur = %d);', [TABLE_UTILISATEURS, QIdx]);
  //select * from utilisateurs where LOGIN like "jpcassoU";
  Nb := 0;
  FSecSqlQuery.SQL.Text := WU;
  try
    FSecSqlQuery.Open;
    // La requête comporte tous les critères. On recherche uniquement s'il y a une et une seule ligne satisfaisant les critères
    // L'item trouvé va dans FCurrentUser
    while (not FSecSqlQuery.EOF) do
    begin
      FSecSqlQuery.Next;
      QUser.IDUtilisateur   := StrToInt64Def(FSecSqlQuery.Fields[0].AsString, 0);        // IDUtilisateur
      QUser.NomUtilisateur  := Trim(FSecSqlQuery.Fields[1].AsString);                    // NomUtilisateur
      QUser.Login           := Trim(FSecSqlQuery.Fields[2].AsString);                    // Login
      // Hash du pwd: inutilisé                                                                  // HashPassword
      QUser.DateInscription := DateSqlToTDateTime(FSecSqlQuery.Fields[4].AsString);      // DateInscription
      QUser.Email           := Trim(FSecSqlQuery.Fields[5].AsString);                    // Email
      Inc(Nb);
    end;
    Result := (Nb = 1);
  finally
    FSecSqlQuery.Close;
  end;
end;
//******************************************************************************

function TConnexionOpenCaveMap.ExtractCaviteFromLstBDD(const QIdxCavite: Int64; out QCavite: TCaviteInBDD): boolean;
var
  n, i: Integer;
begin
  Result := false;
  n := GetNbCavitesInListe();
  if (n = 0) then Exit;
  for i := 0 to n - 1 do
  begin
    QCavite := GetCaviteFromLstBDD(i);
    if (QCavite.IDCavite = QIdxCavite) then
    begin
      Result := true;
      Exit;
    end;
  end;

end;



end.

