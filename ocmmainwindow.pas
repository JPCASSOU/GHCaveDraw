unit OCMMainWindow;
{$INCLUDE CompilationParameters.inc}
interface
uses
  GHCD_Types
  {$IFDEF LANGUAGE_FRENCH}
  , UnitMessages_fr
  {$ENDIF}
  , GeneralFunctions
  , db, ZConnection, ZDataset, ZSqlMonitor
  //, UnitDocDessin
  , Classes, SysUtils, FileUtil, curredit, rxdbgrid, DBDateTimePicker, TADbSource, Forms, Controls, Graphics, Dialogs, ActnList, ExtCtrls, StdCtrls, Buttons, ComCtrls, DBGrids, DbCtrls;

type

  { TOCMFrontal }

  TOCMFrontal = class(TForm)
    acConnecter: TAction;
    acConnecter1: TAction;
    ActionList1: TActionList;
    acUpdateTableBasePoints: TAction;
    acUpdateTableBasePoints1: TAction;
    acUpdateTableCourbes: TAction;
    acUpdateTableCourbes1: TAction;
    acUpdateTableGroupes: TAction;
    acUpdateTableGroupes1: TAction;
    acUpdateTablePolygones: TAction;
    acUpdateTablePolygones1: TAction;
    acUpdateTablePolylines: TAction;
    acUpdateTablePolylines1: TAction;
    acUpdateTableScraps: TAction;
    acUpdateTableScraps1: TAction;
    acUpdateTableSimplesLignes: TAction;
    acUpdateTableSimplesLignes1: TAction;
    acUpdateTableSymboles: TAction;
    acUpdateTableSymboles1: TAction;
    acUpdateTableTextes: TAction;
    acUpdateTableTextes1: TAction;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    Button3: TButton;
    DataSrcUsers: TDataSource;
    DataSrcCavites: TDataSource;
    DBDateTimePicker1: TDBDateTimePicker;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Edit1: TEdit;
    Edit2: TEdit;
    editLogin: TEdit;
    editNumCavite: TCurrencyEdit;
    editPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbConnexionOK: TStaticText;
    lbUserDateInscription: TStaticText;
    lbUserID: TStaticText;
    lbUserLogin: TStaticText;
    lbUserMail: TStaticText;
    lbUserName: TStaticText;
    lsbMessages: TListBox;
    PageControl1: TPageControl;
    pnlUtilisateur: TPanel;
    RxDBGrid1: TRxDBGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ZConnexion1: TZConnection;
    ZQuery1: TZQuery;
    ZTableUsers: TZTable;
    ZTableCavites: TZTable;
    procedure acConnecterExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure RxDBGrid1CellClick(Column: TColumn);
  private
    { private declarations }
    //FTestMyDocDessin: TDocumentDessin; // pour tests
    FTransactionEnCours: boolean;

    //FConnexionOpenCavemap: TConnexionOpenCaveMap;
    procedure ListerLesCavitesInBase(const Qidx: integer);
  public
    { public declarations }
    function Initialiser(): boolean;
  end;

var
  OCMFrontal: TOCMFrontal;

implementation


{$R *.lfm}

{ TOCMFrontal }
const
  CAPTION_CONNEXION_BDD   = 'Connexion BDD';
  CAPTION_DECONNEXION_BDD = 'Déconnexion BDD';

procedure TOCMFrontal.acConnecterExecute(Sender: TObject);
var
  U: TUtilisateurOCM;
begin
  (*
  lsbCavitesInDataBase.Clear;
  if (FConnexionOpenCavemap.Connected) then FConnexionOpenCavemap.CloseConnexion()
                                       else FConnexionOpenCavemap.OpenConnexion();
  //if (MySQL50Connection1.Connected) then
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
    end
    else
    begin
      ShowMessage('Login ou password invalide');
      FConnexionOpenCavemap.CloseConnexion();
    end;
  end
  else
  begin
    FConnexionOpenCavemap.CloseConnexion();
    acConnecter.Caption := CAPTION_CONNEXION_BDD;
    lbConnexionOK.Color  := clRed;
    ShowMessage('Déconnecté du serveur');
    lsbCavitesInDataBase.Clear;
  end;
  //*)
end;

procedure TOCMFrontal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
    //FConnexionOpenCavemap.Finalise();
  finally
    //FConnexionOpenCavemap.Free;
    //FTestMyDocDessin.Free;
  end;
end;

procedure TOCMFrontal.FormShow(Sender: TObject);
begin
  //FConnexionOpenCavemap := TConnexionOpenCaveMap.Create();
  try
    //FConnexionOpenCavemap.Initialise(FMyDocDessin);
    //FMyDocDessin := nil;
    //FConnexionOpenCavemap.Initialise(nil);

    ZConnexion1.LibraryLocation := GetGHCaveDrawDirectory() + 'libmysql.dll';
    ZConnexion1.Protocol := 'mysql-5';
    ZConnexion1.HostName := '127.0.0.1';
    ZConnexion1.Database := 'opencavemap';
    ZConnexion1.Password := '';
    ZConnexion1.User     := 'root';
  except
  end;
end;

procedure TOCMFrontal.ListerLesCavitesInBase(const Qidx: integer);
var
  QIdCavite: LongInt;
  LS: TStringList;
  i, n: Integer;
  EWE: TCaviteInBDD;
  QUser1, QUser2: TUtilisateurOCM;
begin
  (*
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
  //*)
end;

procedure TOCMFrontal.RxDBGrid1CellClick(Column: TColumn);
begin

end;

function TOCMFrontal.Initialiser(): boolean;
begin
  (*
  FTransactionEnCours := false;
  Result := false;
  try
    //FMyDocDessin := FD;
    acConnecter.Caption := CAPTION_CONNEXION_BDD;
    result := true;
  except
  end;
  //*)
end;

end.

