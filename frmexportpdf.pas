unit frmExportPDF;

{$mode delphi}

interface

uses

  Classes, SysUtils, FileUtil, PReport, Forms, Controls, Graphics, Dialogs
  , GHCD_Types
  , UnitDocDessin
  ;

type

  { TdlgExportPDF }

  TdlgExportPDF = class(TForm)
    PReport1: TPReport;
    PRPage1: TPRPage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FDocDessin: TDocumentDessin;
  public
    { public declarations }
    procedure SetDocDessin(const DC: TDocumentDessin);
  end;

var
  dlgExportPDF: TdlgExportPDF;

implementation

{$R *.lfm}

{ TdlgExportPDF }

procedure TdlgExportPDF.FormShow(Sender: TObject);
begin
  PReport1.Author  := 'JP CASSOU';
  PReport1.Creator := ApplicationName;
end;

procedure TdlgExportPDF.FormCreate(Sender: TObject);
begin

end;

procedure TdlgExportPDF.SetDocDessin(const DC: TDocumentDessin);
begin
  FDocDessin := DC;
end;


end.

