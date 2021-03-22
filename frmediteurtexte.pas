unit frmEditeurTexte;

{$mode delphi}

interface

uses
  GHCD_Types, CadreGHCDEditor,
  Classes, SysUtils, FileUtil, SynHighlighterAny, SynEdit, SynMemo, Forms,
  Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls;

type

  { TdlgEditeurTexte }

  TdlgEditeurTexte = class(TForm)
    BitBtn1: TBitBtn;
    CdrGHCDTextEditor1: TCdrGHCDTextEditor;
    Panel1: TPanel;
  private
    { private declarations }
    FModeSelectionEntites: TModeSelectionEntites;
  public
    { public declarations }
    function  Initialiser(): boolean;
    procedure LoadFile(const QFileName: string);
    procedure SetText(const LS: TStringList);
    procedure SetModeSelectionEntites(const QM: TModeSelectionEntites);
  end;

var
  dlgEditeurTexte: TdlgEditeurTexte;

implementation

{$R *.lfm}

{ TdlgEditeurTexte }



function TdlgEditeurTexte.Initialiser(): boolean;
begin
  Result := CdrGHCDTextEditor1.Initialiser(False);
end;

procedure TdlgEditeurTexte.LoadFile(const QFileName: string);
begin
  CdrGHCDTextEditor1.LoadFromFile(QFileName);
end;

procedure TdlgEditeurTexte.SetModeSelectionEntites(const QM: TModeSelectionEntites);
begin
  FModeSelectionEntites := QM;
end;

procedure TdlgEditeurTexte.SetText(const LS: TStringList);
begin
  CdrGHCDTextEditor1.SetLines(LS);
end;

end.
