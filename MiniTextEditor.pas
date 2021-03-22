unit MiniTextEditor;
// Petit éditeur de texte incorporé
// 24/08/2012
{$INCLUDE CompilationParameters.inc}
interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  Common,
  CallDialogsStdVersion, Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, StdCtrls;

type

  { TdlgEditeur }

  TdlgEditeur = class(TForm)
    acSaveAs: TAction;
    acQuit: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuFile: TMenuItem;
    EditorText: TSynEdit;
    procedure acQuitExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function InitialiseEditor(const FileTexte: string; const EditMode: Boolean): boolean;
  end;

var
  dlgEditeur: TdlgEditeur;

implementation

{$R *.lfm}

{ TdlgEditeur }

procedure TdlgEditeur.FormShow(Sender: TObject);
begin
  mnuFile.Caption  := GetResourceString(rsMNU_FILE);
  acSaveAs.Caption := GetResourceString(rsSAVEAS);
  acQuit.Caption   := GetResourceString(rsQUIT_EDITOR);
end;

procedure TdlgEditeur.acQuitExecute(Sender: TObject);
begin
  self.Close;
end;

procedure TdlgEditeur.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;
  CanClose := GHTopoQuestionOuiNon(EnlevePerluete(rsQUIT_EDITOR));
end;

function TdlgEditeur.InitialiseEditor(const FileTexte: string;
  const EditMode: Boolean): boolean;
begin
  Result := False;
  try
    if (FileTexte = '') then
      EditorText.Lines.Clear
    else
      EditorText.Lines.LoadFromFile(FileTexte);
    Result := True;
  finally
  end;
end;

end.

