unit OOoTools;

{
This unit is part of a toolbox to pilot OpenOffice.org from Delphi using COM Automation.
Copyright (C) 2004-2007  Bernard Marcelly
This unit is free software; you can redistribute it and/or modify it under the terms of
the GNU Lesser General Public License as published by the Free Software Foundation;
either version 2.1 of the License, or (at your option) any later version.
This library is distributed in the hope that it will be useful,but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Lesser General Public License for more details.
You should have received a copy of the GNU Lesser General Public License along with this
library; if not, write to the Free Software Foundation, Inc.,
59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{
Ce fichier a été modifié pour fonctionner avec Lazarus 0.9.24
Les paramétres string ont été transformé en WideString

This file has been modified to work with Lazarus 0.9.24
String parameters modified to WideString

Michel Tournay, le 22/01/2008
}

interface

uses SysUtils, Graphics, Variants;

type
  EOOoError = class(Exception);
type TOOoColor = longword;

var
  OpenOffice, StarDesktop: variant;
  OOoIntrospection, OOoReflection: variant;
{$IFDEF MSWINDOWS}
procedure ConnectOpenOffice;
procedure DisconnectOpenOffice(closeOpenOffice: boolean = False);
{$ENDIF}
function IsOpenOfficeConnected: boolean;
function CreateUnoService(serviceName: WideString): variant;
function CreateUnoStruct(structName: WideString; indexMax: integer = -1): variant;
function CreateProperties(propertyList: array of variant): variant;
function MakePropertyValue(PropName: ansistring; PropValue: variant): variant;
function HasUnoInterfaces(thisObject: variant; interfaceList: array of string): boolean;
function isNullEmpty(thisVariant: variant): boolean;
function dummyArray: variant;

procedure execDispatch(Command: WideString; params: variant);
procedure copyToClipboard;
procedure pasteFromClipboard;
function runScript(scriptName: string; argsList: array of variant; language: string = 'Basic'; location: WideString = 'user'): variant;
procedure runBasicMacro(macroName: string; argsList: string = ''; docName: string = '');
procedure BasicXray(var myObject: variant);
function convertToURL(winAddr: WideString): string;
function convertFromURL(URLaddr: WideString): string;
function OOoRGB(const redV, greenV, blueV: byte): TOOoColor;
function OOoRed(const colorOOo: TOOoColor): byte;
function OOoGreen(const colorOOo: TOOoColor): byte;
function OOoBlue(const colorOOo: TOOoColor): byte;
function PasColor2OOoColor(const OC: TColor): TOOoColor;



implementation

uses
  Classes,
  Controls,
  Forms,
  {$IFDEF MSWINDOWS}
  ComObj,
  {$ENDIF}
  OOoMessages;

const
  URLprefix: array [1..7] of string =
    ('file:///', 'ftp://', 'news:', 'http://', 'mailto:', 'macro:', 'private:');

var
  disp: variant;

{ -------------------------------------------------- }


{ vérifie si le Variant contient bien quelque chose
  Tests whether the Variant contains something                  }
function isNullEmpty(thisVariant: variant): boolean;
begin
  Result := VarIsEmpty(thisVariant) or VarIsNull(thisVariant) or VarIsClear(thisVariant);
end;




{ équivalent de la fonction OOoBasic
  equivalent to OOoBasic function                         }
function CreateUnoService(serviceName: WideString): variant;
begin
  Result := OpenOffice.createInstance(serviceName);
  if isNullEmpty(Result) then
    raise EOOoError.Create(Format(OOo_serviceKO, [serviceName]));
end;




{ utile si la connexion OLE peut avoir été supprimée entre deux tâches
  useful if the OLE connection may have been deleted between two jobs   }
function IsOpenOfficeConnected: boolean;
var
  DeskTopbis: variant;
begin
  IsOpenOfficeConnected := False;
  if isNullEmpty(OpenOffice) then
    exit;
  { vérifier si OpenOffice peut renvoyer un service quelconque
    check if OpenOffice can return any service                   }
  try
    DeskTopbis := OpenOffice.createInstance('com.sun.star.frame.Desktop');
    IsOpenOfficeConnected := True;
  except
    { pour que ConnectOpenOffice puisse reconnecter
      allow ConnectOpenOffice to connect again           }
    OpenOffice := Null;
  end;
end;


{ initialiser l'interface vers OpenOffice
  initiate COM interface towards OpenOffice           }
{$IFDEF MSWINDOWS}
procedure ConnectOpenOffice;
begin
  if IsOpenOfficeConnected then
    exit;
  Screen.Cursor := crHourglass;
  Application.ProcessMessages;
  try
    OpenOffice := CreateOleObject('com.sun.star.ServiceManager');
    if isNullEmpty(OpenOffice) then
      raise EOOoError.Create(OOo_connectKO);
    StarDesktop := CreateUnoService('com.sun.star.frame.Desktop');
    disp := CreateUnoService('com.sun.star.frame.DispatchHelper');
    OOoIntrospection := CreateUnoService('com.sun.star.beans.Introspection');
    OOoReflection := CreateUnoService('com.sun.star.reflection.CoreReflection');
  finally
    Screen.Cursor := crDefault;
  end;
end;


{ supprimer l'interface COM vers OpenOffice
  close COM interface towards OpenOffice               }
procedure DisconnectOpenOffice(closeOpenOffice: boolean = False);
begin
  if closeOpenOffice then
    StarDesktop.terminate;
  OpenOffice := Unassigned;
  StarDesktop := Unassigned;
  disp := Unassigned;
  OOoIntrospection := Unassigned;
  OOoReflection := Unassigned;
end;
{$ENDIF}


{ équivalent de la fonction OOoBasic
  equivalent to OOoBasic function                    }
function CreateUnoStruct(structName: WideString; indexMax: integer = -1): variant;
var
  d: integer;
begin
  try
    if indexMax < 0 then
      Result := OpenOffice.Bridge_GetStruct(structName)
    else begin
      Result := VarArrayCreate([0, indexMax], varVariant);
      for d := 0 to indexMax do
        Result[d] := OpenOffice.Bridge_GetStruct(structName);
    end;
  except
    Result := Null;
  end;
  if isNullEmpty(Result) then
    raise EOOoError.Create(Format(OOo_structureKO, [structName]));
end;


{ équivalent de la fonction de Danny Brewer
  equivalent to Danny Brewer's function                 }
function MakePropertyValue(PropName: ansistring; PropValue: variant): variant;
begin
  Result := OpenOffice.Bridge_GetStruct('com.sun.star.beans.PropertyValue');
  Result.Name := PropName;
  Result.Value := PropValue;
end;


{ fonction plus puisssante que MakePropertyValue
  function more powerful than MakePropertyValue          }
function CreateProperties(propertyList: array of variant): variant;
var
  x, y, xMax: integer;
begin
  xMax := High(propertyList);
  if (not odd(xMax)) or (xMax < 1) then
    raise EOOoError.Create(OOo_nbrArgsKO);

  Result := VarArrayCreate([0, xMax shr 1], varVariant);
  x := 0;
  y := 0;
  repeat
    Result[y] := OpenOffice.Bridge_GetStruct('com.sun.star.beans.PropertyValue');
    case VarType(propertyList[x]) of { check that the argument is a String }
      varOleStr, varStrArg, varString: Result[y].Name := propertyList[x];
      else raise EOOoError.Create(Format(OOo_notString, [x]));
    end;
    Result[y].Value := propertyList[x + 1];
    Inc(y);
    Inc(x, 2);
  until x > xMax;
end;


{ crée un tableau vide, pour une liste vide
creates an empty array for an empty list               }
function dummyArray: variant;
begin
  Result := VarArrayCreate([0, -1], varVariant);
end;



{ équivalent de la fonction OOoBasic
  equivalent to OOoBasic function                         }
function HasUnoInterfaces(thisObject: variant; interfaceList: array of string): boolean;
var
  objInterf: TStringList;
  insp, info1, info2, info3: variant;
  x, x2: integer;
  oneInterf: string;
begin
  Result := False;
  objInterf := TStringList.Create;
  try
    insp := OOoIntrospection.inspect(thisObject);
    info1 := insp.getMethods(-1);
    for x := 0 to VarArrayHighBound(info1, 1) do begin
      info2 := info1[x];
      info3 := info2.DeclaringClass;
      oneInterf := info3.Name;
      if (oneInterf <> '') and (objInterf.IndexOf(oneInterf) < 0) then
        objInterf.Add(oneInterf);
    end;
    for x := 0 to High(interfaceList) do begin
      x2 := objInterf.IndexOf(interfaceList[x]);
      if x2 < 0 then
        exit;
      if objInterf.Strings[x2] <> interfaceList[x] then
        exit; // vérifier la casse  // check case
    end;
    Result := True;
  except
    raise EOOoError.Create(OOo_inspectionKO);
  end;
end;


{ appel du dispatch OpenOffice, comme l'enregistreur de macro
  calling OpenOffice dispatch, like the macro recorder              }
procedure execDispatch(Command: WideString; params: variant);
begin
  disp.executeDispatch(StarDesktop.CurrentFrame, Command, '', 0, params);
end;


procedure copyToClipboard;
begin
  execDispatch('.uno:Copy', dummyArray);
end;


procedure pasteFromClipboard;
begin
  execDispatch('.uno:Paste', dummyArray);
end;


function runScript(scriptName: string; argsList: array of variant; language: string = 'Basic'; location: WideString = 'user'): variant;
var
  mspf, scriptPro, xScript, args: variant;
  x, xMax: integer;
begin
  if (language = 'Basic') and (location = 'user') then
    location := 'application';
  mspf := CreateUnoService('com.sun.star.script.provider.MasterScriptProviderFactory');
  scriptPro := mspf.createScriptProvider('');
  xScript := scriptPro.getScript('vnd.sun.star.script:' + scriptName + '?language=' + language + '&location=' + location);
  xMax := High(argsList);
  args := VarArrayCreate([0, xMax], varVariant);
  for x := 0 to xMax do
    args[x] := argsList[x];
  Result := xScript.invoke(args, dummyArray, dummyArray);
end;


procedure runBasicMacro(macroName: string; argsList: string = ''; docName: string = '');
begin
  execDispatch('macro://' + docName + '/' + macroName + '(' + argsList + ')', dummyArray);
end;


procedure BasicXray(var myObject: variant);
begin
  runScript('XrayTool._Main.Xray', [myObject]);
end;


{ ----------- convert to / from URL -----------------
  fonctions similaires à celles de OOoBasic
  functions similar to those of OOoBasic           }

function convertToURL(winAddr: WideString): string;
var
  sv: variant;
  x: integer;
  sLow, UTF8Addr, prefix: string;
begin
  sLow := AnsiLowerCase(winAddr);
  prefix := '';
  for x := 1 to High(URLprefix) do
    if Pos(URLprefix[x], sLow) = 1 then begin
      winAddr := Copy(winAddr, Length(URLprefix[x]) + 1, 2000);
      if x > 1 then
        prefix := URLprefix[x];  // prefix file:/// is useless
      Break;
    end;
  if (Length(prefix) = 0) and (Pos('@', sLow) > 0) then
    Result := 'mailto:' + winAddr
  else begin
    sv := CreateUnoService('com.sun.star.ucb.FileContentProvider');
    UTF8Addr := sv.getFileURLFromSystemPath('', winAddr);
    if Length(UTF8Addr) = 0 then
      raise EOOoError.Create(OOo_convertToURLKO);
    Result := prefix + UTF8Addr;
  end;
end;


function convertFromURL(URLaddr: WideString): string;
var
  sv: variant;
  x: integer;
  sLow, winAddr, prefix: string;
begin
  sLow := AnsiLowerCase(URLaddr);
  prefix := '';
  for x := 1 to High(URLprefix) do
    if Pos(URLprefix[x], sLow) = 1 then begin
      if x > 1 then begin
        URLaddr := Copy(URLaddr, Length(URLprefix[x]) + 1, 2000);
        prefix := URLprefix[x];
      end;
      Break;
    end;
  sv := CreateUnoService('com.sun.star.ucb.FileContentProvider');
  winAddr := sv.getSystemPathFromFileURL(URLaddr);
  if Length(prefix) <> 0 then // backslash only with file:///
    winAddr := StringReplace(winAddr, '\', '/', [rfReplaceAll]);
  if Length(winAddr) = 0 then
    raise EOOoError.Create(OOo_convertFromURLKO);
  Result := prefix + winAddr;
end;



{  -------------  color functions  ---------------
  fonctions identiques à celles de OOoBasic
  functions identical to those of OOoBasic               }

function OooRGB(const redV, greenV, blueV: byte): TOOoColor;
begin
  Result := (redV shl 16) + (greenV shl 8) + blueV;
end;


function OOoBlue(const colorOOo: TOOoColor): byte;
begin
  Result := colorOOo and 255;
end;


function OOoGreen(const colorOOo: TOOoColor): byte;
begin
  Result := (colorOOo shr 8) and 255;
end;


function OOoRed(const colorOOo: TOOoColor): byte;
begin
  Result := (colorOOo shr 16) and 255;
end;

function PasColor2OOoColor(const OC: TColor): TOOoColor;
begin
  Result := OooRGB(Red(OC), Green(OC), Blue(OC));
end;

end.