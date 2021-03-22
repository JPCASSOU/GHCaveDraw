unit UnitWrapperDeclimag;
// Wrapper pour le calcul des déclinaisons magnétiques
// via les algos de IGRF
// et la librairie de calcul en langage C.
// ------------------------------------------
// Nécessite LibDeclimagIGRF.dll
// 14/11/2013: Validé à raison du jeu d'essai (déviation max de 0.05 degdec)
//             - Déclinaisons: OK à 0.05 degdec près
//             - Variations annuelles: OK
// 25/11/2013: Cette unité est muette (désactivation des AfficherMessage)
// 25/11/2013: Anglicisation des messages pour indiquer qu'il s'agit d'une librairie libre tierce

{$INCLUDE CompilationParameters.inc}

interface

uses
  windows,
  Classes, SysUtils, Dialogs, FileUtil;

type

{ TCalculDeclinaisonMagnetique }

 TCalculDeclinaisonMagnetique = class
  private

  public
    function  DemarrerDLL: boolean;
    procedure StopperDLL;
    function  Initialiser: boolean;
    procedure Finaliser;
    procedure DisplayHello;
    function  CalcHypot(const X, Y: double): double;
    function CalculerDeclimag(const Lon, Lat, Alt: double; const JJ, MM, AAAA: integer): double; overload;
    function CalculerDeclimag(const Lon, Lat, Alt: double; const D: TDateTime): double; overload;
    function CheckFichierCOF: boolean;

end;

implementation
var
  hDLL: THandle;
  // pointeurs vers les fonctions de la DLL
  pProcDeclimagInitialise: function: integer; stdcall;
  pProcDeclimagFinalise  : procedure; stdcall;
  pProcHello             : procedure; stdcall;

  pProcDLLHypot          : function(X, Y: double): double; stdcall;
  pProcCalcDeclimag      : function(Lon, Lat, Alt: double; JJ, MM, AAAA: integer): double; stdcall;

const
  LIB_DECLIMAG = 'LibDeclimagIGRF.dll';

{ TCalculDeclinaisonMagnetique }

function TCalculDeclinaisonMagnetique.DemarrerDLL: boolean;
const WU = 'Unable to start function: %s()';
begin
  Result := False;
  // Fichiers indispensables: WMM.COF
  Result := CheckFichierCOF();
  try
    // recherche de la DLL
    hDLL:=LoadLibrary(LIB_DECLIMAG);
    if (hDLL = 0) then begin
      ShowMessage(LIB_DECLIMAG + ': Library not ready');
      Exit;
    end;
    // routine de démarrage
    @pProcDeclimagInitialise := GetProcAddress(hDLL, 'DLLCalcDeclimagInitialise');
    if (@pProcDeclimagInitialise = nil) then
    begin
      ShowMessageFmt(WU, ['DLLCalcDeclimagInitialise']);
      FreeLibrary(hDLL);
      Exit;
    end;
    // routine de libération
    @pProcDeclimagFinalise :=  GetProcAddress(hDLL, 'DLLCalcDeclimagFinalise');
    if (@pProcDeclimagFinalise = nil) then
    begin
      ShowMessageFmt(WU, ['DLLCalcDeclimagFinalise']);
      FreeLibrary(hDLL);
      Exit;
    end;
    // recherche des routines de la DLL
    @pProcHello:=GetProcAddress(hDll,'HelloWorld');
    if (@pProcHello = nil) then
    begin
      ShowMessageFmt(WU, ['HelloWorld']);
      FreeLibrary(hDLL);
      Exit;
    end;
    @pProcDLLHypot:=GetProcAddress(hDll,'DLLHypotenuse');
    if (@pProcDLLHypot = nil) then
    begin
      ShowMessageFmt(WU, ['DLLHypotenuse']);
      FreeLibrary(hDLL);
      Exit;
    end;
    @pProcCalcDeclimag := GetProcAddress(hDLL, 'DLLCalcDeclimag');
    if (@pProcCalcDeclimag = nil) then
    begin
      ShowMessageFmt(WU, ['DLLCalcDeclimag']);
      FreeLibrary(hDLL);
      Exit;
    end;
    Result := True;
  except
  end;
end;

procedure TCalculDeclinaisonMagnetique.StopperDLL;
begin
  FreeLibrary(hDLL);
end;

function TCalculDeclinaisonMagnetique.Initialiser: boolean;
begin
  Result := (pProcDeclimagInitialise = 0);
  //ShowMessage(BoolToStr(Result, 'Init grille OK', 'Grille HS'));
end;

procedure TCalculDeclinaisonMagnetique.Finaliser;
begin
  pProcDeclimagFinalise;
end;

procedure TCalculDeclinaisonMagnetique.DisplayHello;
begin
  pProcHello;
end;

function TCalculDeclinaisonMagnetique.CalcHypot(const X, Y: double): double;
begin
  Result := pProcDLLHypot(X, Y);
end;

// Calcul de la déclinaison
function TCalculDeclinaisonMagnetique.CalculerDeclimag(const Lon, Lat, Alt: double; const JJ, MM, AAAA: integer): double; overload;
begin
  Result := pProcCalcDeclimag(Lon, Lat, Alt, JJ, MM, AAAA);
end;


// Les angles sont en degrés et les altitudes en mètres
function TCalculDeclinaisonMagnetique.CalculerDeclimag(const Lon, Lat, Alt: double; const D: TDateTime): double; overload;
var
  YYYY: word;
  MM: word;
  JJ: word;
begin
  DecodeDate(D, YYYY, MM, JJ);
  //Result := pProcCalcDeclimag(Lon, Lat, Alt, DD, MM, YYYY);
  Result := pProcCalcDeclimag(Lon, Lat, Alt, Integer(JJ), Integer(MM), Integer(YYYY));
end;

// vérifie et recrée éventuellement le fichier COF
function TCalculDeclinaisonMagnetique.CheckFichierCOF: boolean;
const
  FICHIER_COF = 'WMM.COF';
var
  fp: TextFile;
begin
  Result := False;
  // pour éviter une opération d'écriture sur disque si le fichier existe déjà
  if (FileExists(FICHIER_COF)) then // En Lazarus 1.8, tous les string sont en Unicode.
  begin
    Result := True;
    //ShowMessage(Format('%s already exists', [FICHIER_COF]));
    Exit;
  end;
  ShowMessage(Format('%s not found - Trying to regenerate file',[FICHIER_COF]));
  AssignFile(fp, FICHIER_COF);
  try
    Rewrite(fp);
    WriteLn(fp, '    2010.0            WMM-2010        11/20/2009');
    WriteLn(fp, '  1  0  -29496.6       0.0       11.6        0.0');
    WriteLn(fp, '  1  1   -1586.3    4944.4       16.5      -25.9');
    WriteLn(fp, '  2  0   -2396.6       0.0      -12.1        0.0');
    WriteLn(fp, '  2  1    3026.1   -2707.7       -4.4      -22.5');
    WriteLn(fp, '  2  2    1668.6    -576.1        1.9      -11.8');
    WriteLn(fp, '  3  0    1340.1       0.0        0.4        0.0');
    WriteLn(fp, '  3  1   -2326.2    -160.2       -4.1        7.3');
    WriteLn(fp, '  3  2    1231.9     251.9       -2.9       -3.9');
    WriteLn(fp, '  3  3     634.0    -536.6       -7.7       -2.6');
    WriteLn(fp, '  4  0     912.6       0.0       -1.8        0.0');
    WriteLn(fp, '  4  1     808.9     286.4        2.3        1.1');
    WriteLn(fp, '  4  2     166.7    -211.2       -8.7        2.7');
    WriteLn(fp, '  4  3    -357.1     164.3        4.6        3.9');
    WriteLn(fp, '  4  4      89.4    -309.1       -2.1       -0.8');
    WriteLn(fp, '  5  0    -230.9       0.0       -1.0        0.0');
    WriteLn(fp, '  5  1     357.2      44.6        0.6        0.4');
    WriteLn(fp, '  5  2     200.3     188.9       -1.8        1.8');
    WriteLn(fp, '  5  3    -141.1    -118.2       -1.0        1.2');
    WriteLn(fp, '  5  4    -163.0       0.0        0.9        4.0');
    WriteLn(fp, '  5  5      -7.8     100.9        1.0       -0.6');
    WriteLn(fp, '  6  0      72.8       0.0       -0.2        0.0');
    WriteLn(fp, '  6  1      68.6     -20.8       -0.2       -0.2');
    WriteLn(fp, '  6  2      76.0      44.1       -0.1       -2.1');
    WriteLn(fp, '  6  3    -141.4      61.5        2.0       -0.4');
    WriteLn(fp, '  6  4     -22.8     -66.3       -1.7       -0.6');
    WriteLn(fp, '  6  5      13.2       3.1       -0.3        0.5');
    WriteLn(fp, '  6  6     -77.9      55.0        1.7        0.9');
    WriteLn(fp, '  7  0      80.5       0.0        0.1        0.0');
    WriteLn(fp, '  7  1     -75.1     -57.9       -0.1        0.7');
    WriteLn(fp, '  7  2      -4.7     -21.1       -0.6        0.3');
    WriteLn(fp, '  7  3      45.3       6.5        1.3       -0.1');
    WriteLn(fp, '  7  4      13.9      24.9        0.4       -0.1');
    WriteLn(fp, '  7  5      10.4       7.0        0.3       -0.8');
    WriteLn(fp, '  7  6       1.7     -27.7       -0.7       -0.3');
    WriteLn(fp, '  7  7       4.9      -3.3        0.6        0.3');
    WriteLn(fp, '  8  0      24.4       0.0       -0.1        0.0');
    WriteLn(fp, '  8  1       8.1      11.0        0.1       -0.1');
    WriteLn(fp, '  8  2     -14.5     -20.0       -0.6        0.2');
    WriteLn(fp, '  8  3      -5.6      11.9        0.2        0.4');
    WriteLn(fp, '  8  4     -19.3     -17.4       -0.2        0.4');
    WriteLn(fp, '  8  5      11.5      16.7        0.3        0.1');
    WriteLn(fp, '  8  6      10.9       7.0        0.3       -0.1');
    WriteLn(fp, '  8  7     -14.1     -10.8       -0.6        0.4');
    WriteLn(fp, '  8  8      -3.7       1.7        0.2        0.3');
    WriteLn(fp, '  9  0       5.4       0.0        0.0        0.0');
    WriteLn(fp, '  9  1       9.4     -20.5       -0.1        0.0');
    WriteLn(fp, '  9  2       3.4      11.5        0.0       -0.2');
    WriteLn(fp, '  9  3      -5.2      12.8        0.3        0.0');
    WriteLn(fp, '  9  4       3.1      -7.2       -0.4       -0.1');
    WriteLn(fp, '  9  5     -12.4      -7.4       -0.3        0.1');
    WriteLn(fp, '  9  6      -0.7       8.0        0.1        0.0');
    WriteLn(fp, '  9  7       8.4       2.1       -0.1       -0.2');
    WriteLn(fp, '  9  8      -8.5      -6.1       -0.4        0.3');
    WriteLn(fp, '  9  9     -10.1       7.0       -0.2        0.2');
    WriteLn(fp, ' 10  0      -2.0       0.0        0.0        0.0');
    WriteLn(fp, ' 10  1      -6.3       2.8        0.0        0.1');
    WriteLn(fp, ' 10  2       0.9      -0.1       -0.1       -0.1');
    WriteLn(fp, ' 10  3      -1.1       4.7        0.2        0.0');
    WriteLn(fp, ' 10  4      -0.2       4.4        0.0       -0.1');
    WriteLn(fp, ' 10  5       2.5      -7.2       -0.1       -0.1');
    WriteLn(fp, ' 10  6      -0.3      -1.0       -0.2        0.0');
    WriteLn(fp, ' 10  7       2.2      -3.9        0.0       -0.1');
    WriteLn(fp, ' 10  8       3.1      -2.0       -0.1       -0.2');
    WriteLn(fp, ' 10  9      -1.0      -2.0       -0.2        0.0');
    WriteLn(fp, ' 10 10      -2.8      -8.3       -0.2       -0.1');
    WriteLn(fp, ' 11  0       3.0       0.0        0.0        0.0');
    WriteLn(fp, ' 11  1      -1.5       0.2        0.0        0.0');
    WriteLn(fp, ' 11  2      -2.1       1.7        0.0        0.1');
    WriteLn(fp, ' 11  3       1.7      -0.6        0.1        0.0');
    WriteLn(fp, ' 11  4      -0.5      -1.8        0.0        0.1');
    WriteLn(fp, ' 11  5       0.5       0.9        0.0        0.0');
    WriteLn(fp, ' 11  6      -0.8      -0.4        0.0        0.1');
    WriteLn(fp, ' 11  7       0.4      -2.5        0.0        0.0');
    WriteLn(fp, ' 11  8       1.8      -1.3        0.0       -0.1');
    WriteLn(fp, ' 11  9       0.1      -2.1        0.0       -0.1');
    WriteLn(fp, ' 11 10       0.7      -1.9       -0.1        0.0');
    WriteLn(fp, ' 11 11       3.8      -1.8        0.0       -0.1');
    WriteLn(fp, ' 12  0      -2.2       0.0        0.0        0.0');
    WriteLn(fp, ' 12  1      -0.2      -0.9        0.0        0.0');
    WriteLn(fp, ' 12  2       0.3       0.3        0.1        0.0');
    WriteLn(fp, ' 12  3       1.0       2.1        0.1        0.0');
    WriteLn(fp, ' 12  4      -0.6      -2.5       -0.1        0.0');
    WriteLn(fp, ' 12  5       0.9       0.5        0.0        0.0');
    WriteLn(fp, ' 12  6      -0.1       0.6        0.0        0.1');
    WriteLn(fp, ' 12  7       0.5       0.0        0.0        0.0');
    WriteLn(fp, ' 12  8      -0.4       0.1        0.0        0.0');
    WriteLn(fp, ' 12  9      -0.4       0.3        0.0        0.0');
    WriteLn(fp, ' 12 10       0.2      -0.9        0.0        0.0');
    WriteLn(fp, ' 12 11      -0.8      -0.2       -0.1        0.0');
    WriteLn(fp, ' 12 12       0.0       0.9        0.1        0.0');
    WriteLn(fp, '999999999999999999999999999999999999999999999999');
    WriteLn(fp, '999999999999999999999999999999999999999999999999');
    Result := True;
  finally
    CloseFile(fp);
  end;
end;




end.


