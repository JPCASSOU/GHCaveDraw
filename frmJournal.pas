unit frmJournal;

{$INCLUDE CompilationParameters.inc}

interface

uses
 {$IFDEF LANGUAGE_FRENCH}
   UnitMessages_fr,
 {$ENDIF}
  GeneralFunctions,
  Classes, SysUtils, FileUtil, SynEdit,
  Clipbrd,
  Forms, Controls, Graphics, Dialogs, StdCtrls, PairSplitter;

type

  { TdlgProcessing }

  TdlgProcessing = class(TForm)
    btnCopyJournal: TButton;
    btnCopyConsole: TButton;
    btnViderJournal: TButton;
    btnViderConsole: TButton;
    lbTitreConsoleErreur: TLabel;
    lbTitreLsbJournal: TLabel;
    lsbMessages: TListBox;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    memoMsgsErreur: TSynEdit;
    procedure btnCopyConsoleClick(Sender: TObject);
    procedure btnCopyJournalClick(Sender: TObject);
    procedure btnViderConsoleClick(Sender: TObject);
    procedure btnViderJournalClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure memoMsgsErreurChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  dlgProcessing: TdlgProcessing;

implementation

{$R *.lfm}

{ TdlgProcessing }

procedure TdlgProcessing.btnCopyJournalClick(Sender: TObject);
var
  PP: TClipboard;
  i, Nb: Integer;
  EWE: String;
begin
  PP := TClipboard.Create;
  EWE := '';
  try
    try
      PP.Clear;
      Nb := lsbMessages.Items.Count;
      if (Nb = 0) then Exit;
      for i := 0 to Nb - 1 do EWE += lsbMessages.Items[i] + #13#10;
      PP.AsText := EWE;
    except
    end;
  finally
    FreeAndNil(PP);
  end;
end;

procedure TdlgProcessing.btnViderConsoleClick(Sender: TObject);
begin
  memoMsgsErreur.Clear;
end;

procedure TdlgProcessing.btnViderJournalClick(Sender: TObject);
begin
  lsbMessages.Clear;
end;

procedure TdlgProcessing.btnCopyConsoleClick(Sender: TObject);
var
  PP: TClipboard;
begin
  PP := TClipboard.Create;
  try
    PP.Clear;
    PP.AsText := memoMsgsErreur.Text;
  finally
    FreeAndNil(PP);
  end;
end;

procedure TdlgProcessing.FormCreate(Sender: TObject);
begin
  self.Caption                    := GetResourceString(rsDLG_CONSOLE_TITLE);
  lbTitreLsbJournal.Caption       := GetResourceString(rsDLG_CONSOLE_LB_JOURNAL);
  btnCopyJournal.Caption          := GetResourceString(rsDLG_CONSOLE_BTN_COPY);
  btnViderJournal.Caption         := GetResourceString(rsDLG_CONSOLE_BTN_VOID);

  lbTitreConsoleErreur.Caption    := GetResourceString(rsDLG_CONSOLE_LB_CONSOLE_ERREUR);
  btnCopyConsole.Caption          := GetResourceString(rsDLG_CONSOLE_BTN_COPY);
  btnViderConsole.Caption         := GetResourceString(rsDLG_CONSOLE_BTN_VOID);
end;

procedure TdlgProcessing.memoMsgsErreurChange(Sender: TObject);
begin

end;

end.

