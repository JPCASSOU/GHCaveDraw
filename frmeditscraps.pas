unit frmEditScraps;

{$mode delphi}

interface

uses
  GHCD_Types,
  GeneralFunctions,
  UnitDocDessin,
  CadreListboxGroupes,
  Classes, SysUtils, FileUtil, curredit, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ExtCtrls;

type

{ TdlgEditScrap }

 TdlgEditScrap = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnColorScrap: TColorButton;
    btnColorScrapFromGroupe: TButton;
    CdrListBoxGroupes1: TCdrListBoxGroupes;
    editOpacite: TCurrencyEdit;
    editNomScrap: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbColorScrap: TLabel;
    lbColorScrap1: TLabel;
    Panel1: TPanel;
    sclOpacite: TScrollBar;
    lbNumGroupe: TStaticText;
    procedure btnColorScrapFromGroupeClick(Sender: TObject);
    procedure sclOpaciteChange(Sender: TObject);
  private
    { private declarations }
    FMyDocDessin : TDocumentDessin;
    FScrap       : TScrap;
    procedure ExtractGroupe(const Idx: TIDGroupeEntites);
  public
    { public declarations }
    function Initialise(const FD:TDocumentDessin; const SC: TScrap): boolean;
    function GetScrap(): TScrap;
  end;

var
  dlgEditScrap: TdlgEditScrap;

implementation
uses DGCDummyUnit;

{$R *.lfm}

{ TdlgEditScrap }

function TdlgEditScrap.GetScrap(): TScrap;
begin
  FScrap.Nom         := Trim(editNomScrap.Text);
  FScrap.Couleur     := btnColorScrap.ButtonColor;
  FScrap.Opacite     := editOpacite.AsInteger; //sclOpacite.Position;
  FScrap.IDGroupe    := FMyDocDessin.GetIndexGroupeByInternalIdx(CdrListBoxGroupes1.GetSelectedIndex());
  Result             := FScrap;
end;




procedure TdlgEditScrap.sclOpaciteChange(Sender: TObject);
begin
  editOpacite.AsInteger := sclOpacite.Position;
end;

procedure TdlgEditScrap.btnColorScrapFromGroupeClick(Sender: TObject);
var
  QIDGroupe: TIDGroupeEntites;
  Grp: TGroupeEntites;
begin
  QIDGroupe    := FMyDocDessin.GetIndexGroupeByInternalIdx(CdrListBoxGroupes1.GetSelectedIndex());
  if (QIDGroupe = -1) then exit;
  Grp := FMyDocDessin.GetGroupeByIDGroupe(QIDGroupe);
  btnColorScrap.ButtonColor := Grp.CouleurGroupe;
end;

procedure TdlgEditScrap.ExtractGroupe(const Idx: TIDGroupeEntites);
var
  EWE: TIDGroupeEntites;
begin
  EWE := FMyDocDessin.GetIndexGroupeByInternalIdx(Idx);
  lbNumGroupe.Caption        := Format('%d', [EWE]);
end;

function TdlgEditScrap.Initialise(const FD:TDocumentDessin; const SC: TScrap): boolean;
var
  i, Nb, QIdx: Integer;
  GRP: TGroupeEntites;
  WU: String;
begin
  FMyDocDessin := FD;
  FScrap := SC;
  QIdx := FMyDocDessin.GetInternalIdxGroupe(FScrap.IDGroupe);
  CdrListBoxGroupes1.Initialiser(FMyDocDessin, QIdx, false, ExtractGroupe, nil);
  btnColorScrap.ButtonColor  := FScrap.Couleur;
  btnColorScrapFromGroupe.Caption:= 'Celle du groupe';
  editNomScrap.Text          := FScrap.Nom;
  sclOpacite.Position        := FScrap.Opacite;
  editOpacite.AsInteger      := sclOpacite.Position;
  lbNumGroupe.Caption        := Format('%d', [FScrap.IDGroupe]);
end;


end.

