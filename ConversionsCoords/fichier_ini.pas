unit fichier_ini;

interface

type

  TFichierIni = class
  private
    fAutoSave : boolean;
    fNom, fChemin : string;
    procedure SetAutoSave (Value : boolean);
    procedure Lecture;
    procedure Ecriture;
  public
    property AutoSave : boolean read FAutoSave write SetAutoSave;
    property Chemin : string read fChemin;
    constructor Create;
  end;

var
  FichierIni : TFichierIni;

implementation

uses
{$IFDEF MSWINDOWS}
  SHFolder,
{$ENDIF}
  SysUtils, Forms, FileUtil;

const
  Taille=256;

constructor TFichierIni.Create;
var
  NomCourt : string;
{$IFDEF MSWINDOWS}
  resultat : array[0..taille] of char;
{$ENDIF}
begin
  NomCourt:=ChangeFileExt(ExtractFileName(Application.ExeName),'');
  NomCourt:=lowerCase(NomCourt);
  fAutoSave:=true;
{$IFDEF MSWINDOWS}
  SHGetFolderPath(0,CSIDL_APPDATA,0,taille,resultat);
  fChemin:=StrPas(resultat)+'\'+NomCourt+'\';
{$ENDIF}
{$IFDEF LINUX}
  fChemin:=GetEnvironmentVariable('HOME')+'/.'+NomCourt+'/';
{$ENDIF}
{$IFDEF MAC_OS_X_VERSION_MIN_REQUIRED}
  fChemin:=GetEnvironmentVariable('HOME')+'/.'+NomCourt+'/';
{$ENDIF}
  if not DirectoryExists(fChemin) then
    CreateDir(fChemin);
  fNom:=ChangeFileExt(ExtractFileName(Application.ExeName),'.ini');
  fNom:=lowerCase(fNom);
end;

procedure TFichierIni.SetAutoSave (Value : boolean);
begin
  fAutoSave:=Value;
  Ecriture;
end;

procedure TFichierIni.Lecture;
var
  F : TextFile;
  S : string;
begin
  if FileExists(fChemin+fNom) then
  begin
    AssignFile(F,fChemin+fNom);
    try
      reset(F);
      while not(Eof(F)) do
      begin
        readln(F,S);
        if Pos('AutoSave',S)<>0 then
          fAutoSave:=(S='AutoSave = OUI');
      end;
    finally
      closeFile(F);
    end;
  end;
end;

procedure TFichierIni.Ecriture;
var
  F : TextFile;
begin
  AssignFile(F,fChemin+fNom);
  try
    rewrite(F);
    try
      if fAutoSave then
        writeln(F,'AutoSave = OUI')
      else
        writeln(F,'AutoSave = NON');
    finally
      closeFile(F);
    end;
  except
  end;
end;


initialization
  FichierIni:=TFichierIni.Create;
  FichierIni.Lecture;
finalization
  FichierIni.Ecriture;
  FichierIni.Free;
end.
