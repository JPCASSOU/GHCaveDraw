unit UnitExportTherion;

{$INCLUDE CompilationParameters.inc}



interface

uses
  SysUtils
  , Classes
  {$IFDEF LANGUAGE_FRENCH}
  , UnitMessages_fr
  {$ENDIF}
  , GHCD_Types
  , GeneralFunctions
  , UnitDocDessin
  ;

type

{ TDossierTherion }

 TDossierTherion = class
  strict private

  private
    FDocTopo       : TDocumentDessin;
    FDossierTherion: TStringDirectoryFileName;
    FProcNotify    : TProcedureWithStringOfObject;
    function MakeTHConfig(): boolean;
  public
    function Initialiser(const DT: TDocumentDessin; const DossierTherion: TStringDirectoryFileName; const ProcNotify: TProcedureWithStringOfObject): boolean;
    procedure Finaliser();
end;

implementation

{ TDossierTherion }


function TDossierTherion.Initialiser(const DT: TDocumentDessin; const DossierTherion: TStringDirectoryFileName; const ProcNotify: TProcedureWithStringOfObject): boolean;
var
  FichierCenterlines: TStringDirectoryFileName;
  EWE: String;
begin
  EWE := '%s introuvable' + #13#10 +
         'A reconstruire avec GHTopo' + #13#10 +
         'Les autres fichiers seront construits normalement';
  result       := false;
  FDocTopo     := DT;
  FProcNotify  := ProcNotify;
  FDossierTherion := DossierTherion;
  AfficherMessage(Format('%s.Initialiser: %s', [ClassName, FDossierTherion]));
  FichierCenterlines := FDossierTherion + PathDelim + 'centerlines.th';
  if (not ForceDirectories(FDossierTherion)) then Exit;
  if (not MakeTHConfig()) then Exit;
  // vérification du fichier centerlines, qui doit être reconstruit par GHTopo
  // TODO: Ecrire un petit utilitaire console qui reconstruit un fichier centerlines.th depuis un xtb
  if ((not FileExists(FichierCenterlines)) AND Assigned(FProcNotify)) then FProcNotify(Format(EWE, [FichierCenterlines]));
  result := True;
end;

procedure TDossierTherion.Finaliser();
begin
  AfficherMessage(Format('%s.Finaliser: %s', [ClassName, FDossierTherion]));
end;
//******************************************************************************
function TDossierTherion.MakeTHConfig(): boolean;
var
  LS: TStringList;
begin
  Result := false;
  LS := TStringList.Create;
  try
    LS.Clear;
    LS.Add(Format('encoding %s', ['utf-8']));
    LS.Add(Format('source %s', ['centerlines.th']));
    ls.Add('export model -fmt loch');
    LS.SaveToFile(FDossierTherion + PathDelim + 'thconfig');
    LS.Clear;
    Result := True;
  finally
    FreeAndNil(LS);
  end;
end;

end.

