program GHCaveDrawViewer;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads,
    {$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainFrmViewer
  {$IFDEF MSWINDOWS}
  , frmJournal
  //, printer4lazarus
  {$ENDIF MSWINDOWS}
  ;
{$R *.res}

begin
  RequireDerivedFormResource := false;
  Application.Scaled:=True;
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

