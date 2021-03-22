unit frmCentreImpression;
{$INCLUDE CompilationParameters.inc}

interface

uses
  Classes, SysUtils
  {$IFDEF LANGUAGE_FRENCH}
    , UnitMessages_fr
  {$ENDIF}
  , FileUtil, Forms, Controls, Graphics, Dialogs;

type
  TdlgPrintingCenter = class(TForm)
  private

  public

  end;

var
  dlgPrintingCenter: TdlgPrintingCenter;

implementation

{$R *.lfm}

end.

