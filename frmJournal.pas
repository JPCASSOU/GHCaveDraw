unit frmJournal;

{$INCLUDE CompilationParameters.inc}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit,
  Clipbrd,
  Forms, Controls, Graphics, Dialogs, StdCtrls, PairSplitter;

type

  { TdlgProcessing }

  TdlgProcessing = class(TForm)
    btnCopyMsg: TButton;
    Label1: TLabel;
    lsbMessages: TListBox;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    memoMsgsErreur: TSynEdit;
    procedure btnCopyMsgClick(Sender: TObject);
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

procedure TdlgProcessing.btnCopyMsgClick(Sender: TObject);
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
    PP.Free;
  end;
end;

procedure TdlgProcessing.memoMsgsErreurChange(Sender: TObject);
begin

end;

end.

