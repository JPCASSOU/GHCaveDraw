unit frmListeGroupes;

{$mode delphi}

interface

uses
  GHCD_Types,
  GeneralFunctions,
  LCLType,
  UnitDocDessin,
  CadreListboxGroupes,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, Buttons, ExtCtrls, Types;

type

{ TdlgListeGroupes }

 TdlgListeGroupes = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CdrListBoxGroupes1: TCdrListBoxGroupes;
    Panel1: TPanel;
  private
    { private declarations }

  public
    { public declarations }
    function Initialiser(const FD: TDocumentDessin; const GroupeIdx: integer): boolean;
    function GetSelectedIndex(): integer;
  end;

var
  dlgListeGroupes: TdlgListeGroupes;

implementation
uses
  DGCDummyUnit;

{$R *.lfm}
{ TdlgListeGroupes }

function TdlgListeGroupes.Initialiser(const FD: TDocumentDessin;
                                      const GroupeIdx: integer): boolean;
begin
  CdrListBoxGroupes1.Initialiser(FD, GroupeIdx, false, nil, nil);
end;

function TdlgListeGroupes.GetSelectedIndex(): integer;
begin
  Result := CdrListBoxGroupes1.GetSelectedIndex();
end;



end.

