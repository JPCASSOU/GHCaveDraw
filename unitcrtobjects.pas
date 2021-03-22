unit unitCRTObjects;

{$mode objfpc}{$H+}
// Le
interface

uses
  Classes, SysUtils, crt;

type TCheckItem = record
  Checked: boolean;
  Caption: string;
end;


type

{ TCRTWindow }

 { TCRTModalWindow }

 TCRTModalWindow = class
  private
    FX, FY, FL, FH : integer;
    FBG, FFG       : byte;
    FMotifCadre    : string;
    FShadow        : boolean;
    FTitle         : string;
    FModalResult   : integer;
    procedure IntraLoop();
  public
    function WinInit(const X, Y, L, H: integer; const BG, FG: byte; const MotifCadre: string; const Shadow: boolean; const Title: string): integer;
    procedure Refresh();
    property  ModalResult: integer read FModalResult;

end;

//*****************************************************************************
type

{ TCRTListBox }

 // Contrairement au menu, la liste défilante TCRTListBox doit être implémentée en POO
 // - Un grand nombre d'éléments peut être passé
 // - Un TCRTListBox se peuple typiquement via un parcours de liste avec la fonction AddItem()
 // - Dans certains cas, on peut avoir besoin de la chaîne de l'item elle-même
 // - Retourne l'index de l'élément sélectionné, ou -1 si abandon

 TCRTListBox = class
  private
    FX, FY, FL, FH : integer;
    FShadow        : boolean;
    FNbItemsVisibles: integer;
    FBG, FFG       : byte;
    FMotifCadre    : string;
    FTitle         : string;
    FListeItems    : TStringList;
    FCurrentFirstLine: integer;
    FCurrIdx       : integer;
    FSelectedIndex : integer;
    procedure DrawItem(const Idx: integer; const Selected: boolean);
    procedure Lister(const L1: integer);

    procedure IntraLoop();
    procedure setCurrentFirstLine(const n: integer);
  public
    function Initialise(const X, Y, L, H: integer; const BG, FG: byte; const MotifCadre: string; const Shadow: boolean; const Title: string): boolean;
    procedure ShowModal();
    procedure Finalise();
    procedure AddItem(const S: string);
    function  GetSelectedIndex(): integer;
    function  GetSelectedItem(): string;

end;
//**************************************************************************
implementation
uses
  CRTUtils;

// Ici, l'approche objet est la plus appropriée
{ TCRTListBox }
// Extraction OK,
// Parcours ligne par ligne Up, Down OK
// TODO: Parcours page par page
procedure TCRTListBox.DrawItem(const Idx: integer; const Selected: boolean);
  begin
    if (Selected) then
    begin
      CRT.TextBackground(FBG);
      CRT.TextColor(FFG)
    end else begin
      CRT.TextBackground(FFG);
      CRT.TextColor(FBG)
    end;
    LocatePrint(FX + 1 , 1 + FY + Idx, StringOfChar(' ', FL - 2));
    LocatePrint(FX + 2 , 1 + FY + Idx, FListeItems.Strings[Idx + FCurrentFirstLine]);
  end;

procedure TCRTListBox.Lister(const L1: integer);
var
  i, n: Integer;
begin
  n := FNbItemsVisibles;
  if (FListeItems.Count < FNbItemsVisibles) then n := FListeItems.Count - 1;

  for i := 0 to n  do DrawItem(i, false);
  DrawItem(FCurrIdx, True);
end;
procedure TCRTListBox.setCurrentFirstLine(const n: integer);
begin
  FCurrentFirstLine := n;
  if (FCurrentFirstLine < 0) then FCurrentFirstLine := 0;
end;

procedure TCRTListBox.IntraLoop;
const
  QSTEP = 1;
var
  ch: Char;
begin
  repeat
    ch:=ReadKey;
    case ch of
     #0 : begin
            ch:=ReadKey; {Read ScanCode}
            case ch of
              'H' : // Up
              begin
                DrawItem(FCurrIdx, false);
                FCurrIdx -= QSTEP;
                if (FCurrIdx < 0) then
                begin
                  FCurrIdx := 0;
                  setCurrentFirstLine(FCurrentFirstLine - QSTEP);
                  Lister(FCurrentFirstLine);
                end;
                DrawItem(FCurrIdx, true);
              end;
              'P' : // Down
              begin
                DrawItem(FCurrIdx, false);
                FCurrIdx += QSTEP;
                if (FCurrIdx > FListeItems.Count -1) then FCurrIdx := FListeItems.Count - 1;
                if (FCurrentFirstLine + FCurrIdx > FListeItems.Count - QSTEP) then
                begin
                  FCurrIdx := FNbItemsVisibles;
                end;
                if (FCurrIdx  > FNbItemsVisibles) then
                begin
                  FCurrIdx := FNbItemsVisibles;
                  setCurrentFirstLine(FCurrentFirstLine + QSTEP);
                  Lister(FCurrentFirstLine);
                end;
                DrawItem(FCurrIdx, true);
              end;
              //'K' : LocatePrint(1,24, 'Left');
              //'M' : LocatePrint(1,24, 'Right');
            end;
            //LocatePrint(1, 24, Format('Idx: %d - FCurrentFirstLine: %d - FSelectedIndex: %d   ', [FCurrIdx, FCurrentFirstLine, FSelectedIndex]));
          end;
    #13 : FSelectedIndex := FCurrentFirstLine + FCurrIdx;
    #27 : FSelectedIndex := -1;
    end;
  until (ch = #27) or (ch = #13);
end;

function TCRTListBox.Initialise(const X, Y, L, H: integer; const BG, FG: byte; const MotifCadre: string; const Shadow: boolean; const Title: string): boolean;
begin
  result := false;
  try
    FX := X; FY := Y; FL := L; FH := H; FShadow := Shadow;
    FNbItemsVisibles := FH - 3;
    FBG := BG; FFG := FG;
    FMotifCadre := MotifCadre;
    FTitle := Title;
    FListeItems := TStringList.Create;
    FListeItems.Clear;
    FCurrentFirstLine := 0;
    FCurrIdx := 0;
    FSelectedIndex := -1;
  except
  end;
end;

procedure TCRTListBox.ShowModal;
begin
  CRTDrawFenetre(FX, FY, FL, FH, FBG, FFG, FMotifCadre, FShadow, FTitle);
  Lister(0);
  IntraLoop();
end;

procedure TCRTListBox.Finalise;
begin
  try
    FListeItems.Clear;
  finally
    FListeItems.Free;
  end;
end;

procedure TCRTListBox.AddItem(const S: string);
begin
  FListeItems.Add(S);
end;

function TCRTListBox.GetSelectedIndex(): integer;
begin
  Result := FSelectedIndex;
end;

function TCRTListBox.GetSelectedItem: string;
begin
  Result := FListeItems.Strings[FSelectedIndex];
end;

{ TCRTWindow }

procedure TCRTModalWindow.IntraLoop;
var
  ch: Char;
begin
  repeat
    ch:=ReadKey;
    case ch of
     #0 : begin
            ch:=ReadKey; {Read ScanCode}
            //case ch of
              //'H' :
              //'P' :
              //'K' : LocatePrint(1,24, 'Left');
              //'M' : LocatePrint(1,24, 'Right');
            //end;
          end;
     #13: FModalResult := 666;
     #27: FModalResult := 404;
    else
      ;
    end;
  until (ch = #27) or (ch = #13);
end;

procedure TCRTModalWindow.Refresh;
begin
  CRTDrawFenetre(FX, FY, FL, FH, FBG, FFG, FMotifCadre, FShadow, FTitle);
end;

function TCRTModalWindow.WinInit(const X, Y, L, H: integer; const BG, FG: byte; const MotifCadre: string; const Shadow: boolean; const Title: string): integer;
begin
  Result := -1;
  try
    FModalResult := 0;
    FX := X; FY := Y; FL := L; FH := H; FShadow := Shadow;
    FBG := BG; FFG := FG;
    FMotifCadre := MotifCadre;
    FTitle := Title;
    Refresh();
    IntraLoop();

  except
  end;
end;

end.

