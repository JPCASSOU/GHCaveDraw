unit frmhierarchieobjets;

{$mode delphi}

interface

uses
  GHCD_Types,
  UnitDocDessin, CadreHierarchieObjets,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TdlgHierarchieDesObjets }

  TdlgHierarchieDesObjets = class(TForm)
    CdrHierarchieObjets1: TCdrHierarchieObjets;
    Panel1: TPanel;
  private
    FDocumentDessin: TDocumentDessin;
  public
    function Initialiser(const FD: TDocumentDessin): boolean;
  end;

var
  dlgHierarchieDesObjets: TdlgHierarchieDesObjets;

implementation

{$R *.lfm}

{ TdlgHierarchieDesObjets }

function TdlgHierarchieDesObjets.Initialiser(const FD: TDocumentDessin): boolean;
begin
  result := CdrHierarchieObjets1.Initialiser(FD);

end;

end.

