unit CdrListeGroupes;
// 29/07/2015: Mise en surbrillance du groupe courant
// 17/10/2015: Refonte des boutons + ajout pop up pour la liste

{$INCLUDE CompilationParameters.inc}

interface

uses
  GHCD_Types,
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
   Graphics, UnitDocDessin, GeneralFunctions,
   CallDialogsGHCaveDraw, cdrdessincanvasBGRA, CadreListboxGroupes, LCLType,
   Classes, SysUtils, FileUtil, Forms, Controls,
   StdCtrls, ExtCtrls,
   ComCtrls,
   ActnList, Menus;
type

  { TCadreListeGroupesForMainWindow }

  TCadreListeGroupesForMainWindow = class(TFrame)
    acNewGroupe: TAction;
    acDefCurrentGroupe: TAction;
    acGroupeVisible: TAction;
    acGroupeDecale: TAction;
    acMasquerTousGroupes: TAction;
    acBasculerVisibiliteGroupes: TAction;
    acSupprimerObjetsGroupe: TAction;
    acRendreVisiblesGroupesSelected: TAction;
    acSetCurrentGroupe: TAction;
    acLocaliserGroupe: TAction;
    acLstGroupes: TActionList;
    acReorganiserCourbesGroupe: TAction;
    btnAddGroupe: TButton;
    btnInvertChkGroupes: TButton;
    btnUnvisibleAllGroupes: TButton;
    Button2: TButton;
    CdrListBoxGroupes1: TCdrListBoxGroupes;
    chkDecalageActif: TCheckBox;
    chkVisible: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbColorGroupe: TStaticText;
    lbIDGroupe: TStaticText;
    lbNbObjets: TStaticText;
    lbNomGroupe: TStaticText;
    lbZOrder: TStaticText;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlBtns: TPanel;
    popupListeGroupes: TPopupMenu;
    procedure acBasculerVisibiliteGroupesExecute(Sender: TObject);
    procedure acDefCurrentGroupeExecute(Sender: TObject);
    procedure acGroupeDecaleExecute(Sender: TObject);
    procedure acGroupeVisibleExecute(Sender: TObject);
    procedure acLocaliserGroupeExecute(Sender: TObject);
    procedure acMasquerTousGroupesExecute(Sender: TObject);
    procedure acNewGroupeExecute(Sender: TObject);
    procedure acRendreVisiblesGroupesSelectedExecute(Sender: TObject);
    procedure acReorganiserCourbesGroupeExecute(Sender: TObject);
    procedure acSetCurrentGroupeExecute(Sender: TObject);
    procedure acSupprimerObjetsGroupeExecute(Sender: TObject);
    procedure btnDeleteObjetsGroupeClick(Sender: TObject);
    procedure chkDecalageActifChange(Sender: TObject);
    procedure chkDecalageActifClick(Sender: TObject);
    procedure chkLockedChange(Sender: TObject);
    procedure chkLockedClick(Sender: TObject);
    procedure chkVisibleChange(Sender: TObject);
    procedure chkVisibleClick(Sender: TObject);

  private
    { private declarations }
    FProcLocaliserGroupe: TProcUseGroupe;
    FGroupesModifiables: boolean;
    FActiveGroupeIdx: integer;
    FDocDessin: TDocumentDessin;
    FCdrDessin: TCadreDessinBGRA;
    procedure CreerNouveauGroupe();

    procedure InvertFlagGroupe(const QIdxGroupe: integer;
                               const QFlag: TFlagGroupe);
    procedure InvertVisibiliteGroupes();

    procedure MarquerVisibiliteTousGroupes(const B: boolean);
    procedure ModifyGroupe(const Idx: TIDGroupeEntites);
    procedure PutGroupeInForm(const Idx: TIDGroupeEntites);
    procedure SetCurrentGroupe();
  public
    { public declarations }
    function  Initialiser(const CtxtDessin: TCadreDessinBGRA;
                          const IsGroupesModifiables: boolean;
                          const ProcUseGroupe: TProcUseGroupe): boolean;
    procedure ListerLesGroupes(const IdxGr: integer);
    procedure SetGroupesModifiables(const B: boolean);
    procedure SetActiveGroupeIdx(const I: integer);
  end;

implementation
uses DGCDummyUnit;

{$R *.lfm}

{ TCadreListeGroupesForMainWindow }


procedure TCadreListeGroupesForMainWindow.SetCurrentGroupe();
begin
  if (FDocDessin.GetNbGroupes() > 0) then
  begin
    FActiveGroupeIdx := CdrListBoxGroupes1.GetSelectedIndex();
    FCdrDessin.SetCurrentGroupeByIdx(FActiveGroupeIdx);
    FCdrDessin.RedessinEcran(false);
  end;
end;

procedure TCadreListeGroupesForMainWindow.SetGroupesModifiables(const B: boolean);
begin
  FGroupesModifiables := B;
end;

procedure TCadreListeGroupesForMainWindow.acDefCurrentGroupeExecute(Sender: TObject);
begin
  SetCurrentGroupe();
end;



procedure TCadreListeGroupesForMainWindow.acGroupeDecaleExecute(Sender: TObject);
begin
  InvertFlagGroupe(CdrListBoxGroupes1.GetSelectedIndex(), fgDO_OFFSET);
end;


procedure TCadreListeGroupesForMainWindow.acGroupeVisibleExecute(Sender: TObject);
begin
  InvertFlagGroupe(CdrListBoxGroupes1.GetSelectedIndex(), fgVISIBLE);
end;

procedure TCadreListeGroupesForMainWindow.acLocaliserGroupeExecute(Sender: TObject);
var
  Grp: TGroupeEntites;
begin
  Grp := FDocDessin.GetGroupe(CdrListBoxGroupes1.GetSelectedIndex());
  if (Assigned(FProcLocaliserGroupe)) then FProcLocaliserGroupe(Grp);
end;

procedure TCadreListeGroupesForMainWindow.acBasculerVisibiliteGroupesExecute(Sender: TObject);
begin
  InvertVisibiliteGroupes();
end;

procedure TCadreListeGroupesForMainWindow.acMasquerTousGroupesExecute(Sender: TObject);
begin
  MarquerVisibiliteTousGroupes(false);
end;

procedure TCadreListeGroupesForMainWindow.acNewGroupeExecute(Sender: TObject);
begin
  CreerNouveauGroupe();
end;

procedure TCadreListeGroupesForMainWindow.acRendreVisiblesGroupesSelectedExecute(
  Sender: TObject);
begin

end;

procedure TCadreListeGroupesForMainWindow.acReorganiserCourbesGroupeExecute(Sender: TObject);
var
  MyGroupe: TGroupeEntites;
begin
  MyGroupe := FDocDessin.GetGroupe(CdrListBoxGroupes1.GetSelectedIndex());
  FDocDessin.ReorienterCourbesOfGroupe(MyGroupe);
end;

procedure TCadreListeGroupesForMainWindow.acSetCurrentGroupeExecute(Sender: TObject);
begin

end;

procedure TCadreListeGroupesForMainWindow.acSupprimerObjetsGroupeExecute(Sender: TObject);
begin

end;

procedure TCadreListeGroupesForMainWindow.CreerNouveauGroupe();
var
  GP: TGroupeEntites;
  QDoSetThisGroupeAsCurrent: boolean;
begin
  // le groupe sera initialisé dans EditerGroupe
  if (EditerGroupe(GP, medCREATION, '', QDoSetThisGroupeAsCurrent)) then
  begin
    GP.IDGroupeEntites := FDocDessin.GetMaxIdxGroupe(True);
    GP.ZOrder          := -5000; // remonte le nouveau groupe en tête de liste
    GP.Visible       := True;
    FDocDessin.AddGroupe(GP, True);
    FDocDessin.TrierGroupesByZOrder();
    ListerLesGroupes(0);
    FCdrDessin.ActualiserCmbListeDesGroupes();
    if (QDoSetThisGroupeAsCurrent) then SetCurrentGroupe();
  end;
end;

procedure TCadreListeGroupesForMainWindow.MarquerVisibiliteTousGroupes(const B: boolean);
begin
  FDocDessin.MarquerVisibiliteOfAllGroupes(B);
  ListerLesGroupes(CdrListBoxGroupes1.GetSelectedIndex());
  FCdrDessin.Vue.Invalidate; //CadreDessin1.RedessinEcran(false);
end;

procedure TCadreListeGroupesForMainWindow.InvertVisibiliteGroupes();
begin
  FDocDessin.BasculerVisibiliteOfAllGroupes();
  ListerLesGroupes(CdrListBoxGroupes1.GetSelectedIndex());
  FCdrDessin.Vue.Invalidate; //CadreDessin1.RedessinEcran(false);
end;


procedure TCadreListeGroupesForMainWindow.chkDecalageActifChange(Sender: TObject);
begin

end;
procedure TCadreListeGroupesForMainWindow.InvertFlagGroupe(const QIdxGroupe: integer;
                                                           const QFlag: TFlagGroupe);
var
  Grp: TGroupeEntites;
begin
  Grp := FDocDessin.GetGroupe(QIdxGroupe);
  case QFlag of
    fgVISIBLE   : Grp.Visible       := Not Grp.Visible;
    fgDO_OFFSET : Grp.DecalageActif := Not grp.DecalageActif;
  end;
  FDocDessin.PutGroupe(QIdxGroupe, Grp);
  ListerLesGroupes(CdrListBoxGroupes1.GetSelectedIndex());
  FCdrDessin.Vue.Invalidate; //CadreDessin1.RedessinEcran(false);
end;

procedure TCadreListeGroupesForMainWindow.btnDeleteObjetsGroupeClick(Sender: TObject);
var
  IdxGr: Integer;
  MyGroupe: TGroupeEntites;
  WU: String;
begin
  IdxGr := CdrListBoxGroupes1.GetSelectedIndex();
  if (IdxGr >= 0)then
  begin
    MyGroupe := FDocDessin.GetGroupe(IdxGr);
    WU := Format('Supprimer les objets du groupe %d - %s', [MyGroupe.IDGroupeEntites, MyGroupe.NomGroupe]);
    if (QuestionOuiNon(WU)) then
    begin
      FDocDessin.SupprimerObjetsGroupe(MyGroupe);
      FCdrDessin.RedessinEcran(false);
    end;
  end;
end;



procedure TCadreListeGroupesForMainWindow.chkDecalageActifClick(Sender: TObject);
var
  n: Integer;
  EWE: TGroupeEntites;
begin
  n := CdrListBoxGroupes1.GetSelectedIndex();
  if (n < 0) then Exit;
  EWE := FDocDessin.GetGroupe(n);
  EWE.DecalageActif := chkDecalageActif.Checked;
  FDocDessin.PutGroupe(n, EWE);
  ListerLesGroupes(n);
end;

procedure TCadreListeGroupesForMainWindow.chkLockedChange(Sender: TObject);
begin

end;




procedure TCadreListeGroupesForMainWindow.chkLockedClick(Sender: TObject);
var
  n: Integer;
  EWE: TGroupeEntites;
begin
  n := CdrListBoxGroupes1.GetSelectedIndex();
  if (n < 0) then Exit;
  EWE := FDocDessin.GetGroupe(n);
  FDocDessin.PutGroupe(n, EWE);
  ListerLesGroupes(n);
end;

procedure TCadreListeGroupesForMainWindow.chkVisibleChange(Sender: TObject);
var
  n: Integer;
  EWE: TGroupeEntites;
begin
  n := CdrListBoxGroupes1.GetSelectedIndex();
  if (n < 0) then Exit;
  EWE := FDocDessin.GetGroupe(n);
  EWE.Visible := chkVisible.Checked;
  FDocDessin.PutGroupe(n, EWE);
  ListerLesGroupes(n);
end;

procedure TCadreListeGroupesForMainWindow.chkVisibleClick(Sender: TObject);
begin

end;




procedure TCadreListeGroupesForMainWindow.SetActiveGroupeIdx(const I: integer);
begin
  FActiveGroupeIdx := I;
end;

procedure TCadreListeGroupesForMainWindow.ListerLesGroupes(const IdxGr: integer);
var
  i, n: Integer;
  EWE: TGroupeEntites;
  WU: Integer;
begin
  acBasculerVisibiliteGroupes.Caption     := GetResourceString(rsCDR_LST_GRP_BASC_VISI_GROUPES);
  acDefCurrentGroupe.Caption              := GetResourceString(rsCDR_LST_GRP_DEF_GROUPE_COURANT);
  acNewGroupe.Caption                     := GetResourceString(rsCDR_LST_GRP_NOUVEAU_GROUPE);
  acGroupeDecale.Caption                  := GetResourceString(rsCDR_LST_GRP_DECALAGE_GROUPE);
  acGroupeVisible.Caption                 := GetResourceString(rsCDR_LST_GRP_VISIBILITE_GROUPE);
  acMasquerTousGroupes.Caption            := GetResourceString(rsCDR_LST_GRP_MASQUER_TOUS_GROUPES);
  acRendreVisiblesGroupesSelected.Caption := GetResourceString(rsCDR_LST_GRP_SELECT_GRP_VISIBLE);
  acSupprimerObjetsGroupe.Caption         := GetResourceString(rsCDR_LST_GRP_DELETE_OBJ_GROUPE);
  CdrListBoxGroupes1.ListerLesGroupes(IdxGr);
  acNewGroupe.Visible := FGroupesModifiables;
  acDefCurrentGroupe.Visible := FGroupesModifiables;
  FCdrDessin.Vue.Invalidate;
end;



procedure TCadreListeGroupesForMainWindow.PutGroupeInForm(const Idx: TIDGroupeEntites);
var
  EWE : TGroupeEntites;
begin
  if (Idx < 0) then Exit;
  EWE  := FDocDessin.GetGroupe(Idx);
  lbIDGroupe.Caption       := Format('%d', [EWE.IDGroupeEntites]);
  lbNomGroupe.Caption      := EWE.NomGroupe;
  lbColorGroupe.Color      := EWE.CouleurGroupe;
  chkVisible.Checked       := EWE.Visible;
  chkDecalageActif.Checked := EWE.DecalageActif;
  lbZOrder.Caption         := Format('%.2f', [EWE.ZOrder]);
  lbNbObjets.Caption       := Format('%d', [EWE.NombreObjets]);
end;

procedure TCadreListeGroupesForMainWindow.ModifyGroupe(const Idx: TIDGroupeEntites);
var
  QDoSetThisGroupeAsCurrent: boolean;
  GP: TGroupeEntites;
begin
  if (Idx >=0) then
  begin
    GP := FDocDessin.GetGroupe(Idx);
    if (EditerGroupe(GP, medMODIF, '', QDoSetThisGroupeAsCurrent)) then
    begin
      FDocDessin.PutGroupe(Idx, GP);
      FDocDessin.TrierGroupesByZOrder();
      FCdrDessin.ActualiserCmbListeDesGroupes();
      if (QDoSetThisGroupeAsCurrent) then SetCurrentGroupe();
    end;
  end;
  ListerLesGroupes(Idx);
end;




function TCadreListeGroupesForMainWindow.Initialiser(const CtxtDessin: TCadreDessinBGRA;
                                                     const IsGroupesModifiables: boolean;
                                                     const ProcUseGroupe: TProcUseGroupe): boolean;
  procedure S777(const WU: TAction; const CaptionHint: string);
   begin
     WU.Caption  := GetResourceString(CaptionHint);
     WU.Hint     := GetResourceString(CaptionHint);
   end;
begin
  Result := false;
  try
    FGroupesModifiables := IsGroupesModifiables;
    FCdrDessin := CtxtDessin;
    FDocDessin := FCdrDessin.GetDocDessin;

    // hauteur de la liste
    pnlBtns.Visible     := FGroupesModifiables;

    FProcLocaliserGroupe:= ProcUseGroupe;
    // captions des actions
    S777(acNewGroupe                , rsCDR_LST_GRP_NOUVEAU_GROUPE);
    S777(acBasculerVisibiliteGroupes, rsCDR_LST_GRP_BASC_VISI_GROUPES);
    S777(acDefCurrentGroupe         , rsCDR_LST_GRP_DEF_GROUPE_COURANT);
    S777(acMasquerTousGroupes       , rsCDR_LST_GRP_MASQUER_TOUS_GROUPES);
    S777(acLocaliserGroupe          , rsCDR_LST_GRP_LOCALISER_GROUPE);
    Result := CdrListBoxGroupes1.Initialiser(FDocDessin, 0, false,
                                             PutGroupeInForm,
                                             ModifyGroupe);
  except
  end;
  //ListerLesGroupes(0);

end;

end.

