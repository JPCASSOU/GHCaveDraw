unit CadreEditeurTexte;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, ActnList, SynHighlighterPas, SynEdit;

type

  { TCdrTextEditor }

  TCdrTextEditor = class(TFrame)
    acnLstTextEditor: TActionList;
    acOpen: TAction;
    acSave: TAction;
    acCopy: TAction;
    acPaste: TAction;
    imgLstTextEditor: TImageList;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    TextEditor: TSynEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    SynPasSyn1: TSynPasSyn;
    procedure acCopyExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
  private

  public
    function Initialiser(): boolean;
    procedure AddLine(const S: string);
    procedure ClearText();
  end;

implementation

{$R *.lfm}

{ TCdrTextEditor }

procedure TCdrTextEditor.acCopyExecute(Sender: TObject);
begin
  TextEditor.CopyToClipboard;
end;

procedure TCdrTextEditor.acOpenExecute(Sender: TObject);
begin
  ;
end;

procedure TCdrTextEditor.acPasteExecute(Sender: TObject);
begin
  TextEditor.PasteFromClipboard;
end;

procedure TCdrTextEditor.acSaveExecute(Sender: TObject);
begin
  ;
end;

function TCdrTextEditor.Initialiser(): boolean;
  procedure S666(const AC: TAction; const ACCaption: string);
  begin
    AC.Hint    := ACCaption;
    AC.Caption := ACCaption;
  end;
begin
  result := false;
  S666(acOpen, 'Ouvrir');
  S666(acSave, 'Sauvegarder sous');
  S666(acCopy, 'Copier');
  S666(acPaste,'Coller');
  TextEditor.Highlighter := SynPasSyn1;
  ClearText();
end;

procedure TCdrTextEditor.AddLine(const S: string);
begin
  TextEditor.Lines.Add(S);
end;

procedure TCdrTextEditor.ClearText();
begin
  TextEditor.Clear;
end;

end.

