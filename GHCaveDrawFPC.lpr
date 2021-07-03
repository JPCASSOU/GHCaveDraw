program GHCaveDrawFPC;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads,
    {$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainFrmEditor
  {$IFDEF MSWINDOWS}
  , frmJournal //, CadreGHCDEditor, CadreEditeurTexte, frmDisplaySuperGroupes, CadreListboxGroupes, UnitExportGeoJSON, unit2
  {$ENDIF MSWINDOWS}
  ;
{$R *.res}

begin
  RequireDerivedFormResource := false;
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  {$IFDEF MSWINDOWS}
  Application.CreateForm(TdlgProcessing, dlgProcessing);
  {$ENDIF}
  {$IFDEF MAC_OS_X_VERSION_MIN_REQUIRED}
  Application.CreateForm(TdlgProcessing, dlgProcessing);
  {$ENDIF}
  Application.Run;
end.

