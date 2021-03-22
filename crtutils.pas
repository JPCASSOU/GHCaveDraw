unit CRTUtils;

{$mode objfpc}{$H+}

interface

uses
  crt,
  math,
  Classes, SysUtils,
  unitCRTObjects;

const
  MOTIF_CADRE_MENUS  = #201#200#187#188#205#186;
  MOTIF_CADRE_WNDS   = '      ';

const
  COULEUR_CADRE   = CRT.Cyan;
  COULEUR_TITRE   = CRT.White;
  COULEUR_FOND    = CRT.Blue;
  COULEUR_TEXTE   = CRT.Red;


procedure CLS();
procedure SetColorBGFG(const B, T: byte);
procedure CRTDrawFenetre(const X, Y, L, H: integer; const BG, FG: byte; const MotifCadre: string; const Shadow: boolean; const Title: string);
function  CRTMenu(const X, Y, L: integer;  const BG, FG: byte; const Titre: string; const Items: array of string): integer;
function  InKey(): char;
procedure LocatePrint(const X, Y: integer; const T: string);
function  CRTEditLine(const X, Y, L: integer; const BG, FG: byte; var Text: string): boolean;
function  CRTMessageDlg(const S: array of string): boolean;


implementation

procedure CLS();
begin
  CRT.ClrScr;
  CRT.cursoroff;
end;

procedure SetColorBGFG(const B, T: byte);
begin
  CRT.TextBackground(B);
  CRT.TextColor(T);
end;

function InKey(): char;
begin
  Result := CRT.ReadKey;
end;

procedure LocatePrint(const X, Y: integer; const T: string);
begin
  CRT.GotoXY(X, Y);
  Write(T);
end;

procedure CRTDrawFenetre(const X, Y, L, H: integer; const BG, FG: byte; const MotifCadre: string; const Shadow: boolean; const Title: string);
var
  i, QY: Integer;
  QCGH, QCGB, QCDH, QCDB, BV, BH: Char;
begin
  //CRT.HighVideo;
  QCGH := MotifCadre[1];
  QCGB := MotifCadre[2];
  QCDH := MotifCadre[3];
  QCDB := MotifCadre[4];
  BH   := MotifCadre[5];
  BV   := MotifCadre[6];
  SetColorBGFG(COULEUR_CADRE, COULEUR_TITRE);
  // dessin du cadre
  LocatePrint(X, Y, QCGH);
  LocatePrint(X+1, Y, StringOfChar(BH, L-2));
  LocatePrint(X+L-1, Y, QCDH);
  for i := Y + 1 to Y + H - 1 do
  begin
    LocatePrint(X, i, BV);
    SetColorBGFG(BG, FG);
    LocatePrint(X+1, i, StringOfChar(' ', L-2));
    SetColorBGFG(COULEUR_CADRE, COULEUR_TITRE);
    LocatePrint(X + L - 1, i, BV);
    if (Shadow) then
    begin
      SetColorBGFG(0, 7);
      LocatePrint(X + L, i, ' ');
      SetColorBGFG(COULEUR_CADRE, COULEUR_TITRE);
    end;
  end;
  QY := Y + H - 1;
  SetColorBGFG(COULEUR_CADRE, COULEUR_TITRE);
  LocatePrint(X,  QY, QCGB);
  LocatePrint(X+1, QY, StringOfChar(BH, L-2));
  LocatePrint(X+L-1, QY, QCDB);
  LocatePrint(X + 2, Y, Title);
  if (Shadow) then
  begin
    SetColorBGFG(0, 7);
    LocatePrint(X+1, QY + 1, StringOfChar(' ', L));
  end;
  SetColorBGFG(BG, FG);
end;


// Classiquement, un menu comporte peu d'éléments qu'on peut passer via un tableau.
// On attend d'un menu un numéro d'item sélectionné, ou -1 si abandon
// L'approche procédurale via une simple fonction est la plus adaptée
function CRTMenu(const X, Y, L: integer;  const BG, FG: byte; const Titre: string; const Items: array of string): integer;
  procedure DrawItem(const Idx: integer; const Selected: boolean);
  begin
    if (Selected) then
    begin
      CRT.TextBackground(BG);
      CRT.TextColor(FG)
    end else begin
      CRT.TextBackground(FG);
      CRT.TextColor(BG)
    end;
    LocatePrint(X + 1 , 1 + Y + Idx, StringOfChar(' ', L - 2));
    LocatePrint(X + 2 , 1 + Y + Idx, Items[Idx]);
  end;
var
  n, i: Integer;
  ch: Char;
  CurrIdx: integer;
begin
  Result := -1;
  n := 1 + High(Items);
  CRTDrawFenetre(X, Y, L, n + 2, FG, BG, MOTIF_CADRE_MENUS, true, Titre);
  for i := 0 to n - 1 do DrawItem(i, false);
  CurrIdx := 0;
  DrawItem(CurrIdx, True);

  repeat
    ch:=ReadKey;
    case ch of
     #0 : begin
            ch:=ReadKey; {Read ScanCode}
            case ch of
              'H' : // Up
              begin
                DrawItem(CurrIdx, false);
                CurrIdx -= 1;
                if (CurrIdx < 0) then CurrIdx := High(Items);
                DrawItem(CurrIdx, true);
              end;
              'P' : // Down
              begin
                DrawItem(CurrIdx, false);
                CurrIdx += 1;
                if (CurrIdx  > High(Items)) then CurrIdx := 0;
                DrawItem(CurrIdx, true);
              end;
              //'K' : LocatePrint(1,24, 'Left');
              //'M' : LocatePrint(1,24, 'Right');
            end;
          end;
    #13 : Result := CurrIdx;
    #27 : WriteLn('ESC');
    end;
  until (ch = #27) or (ch = #13);
  //if (w = #13) then Result := CurrIdx;
end;

function CRTEditLine(const X, Y, L: integer; const BG, FG: byte; var Text: string): boolean;
var
  P: Integer;
  ch: Char;
  procedure RefreshTexte(const QP: integer);
  begin
    LocatePrint(X, Y, StringOfChar('_', L));
    LocatePrint(X, Y, Text);
    CRT.GotoXY(X + QP, Y);
  end;
begin
  Result := false;
  CRT.cursoron;
  CRT.TextBackground(BG);
  CRT.TextColor(FG);
  RefreshTexte(0);

  P := 0;
  repeat
    ch:=ReadKey;
    case ch of
     #0 : begin
            ch:=ReadKey; {Read ScanCode}
            case ch of
              'H' : ;// Up
              'P' : ;// Down
              'K' : //Left')
              begin
                dec(P);
                if (P < 0) then P := 0;
                CRT.GotoXY(X + P, Y);
              end;
              'M' : //'Right');
              begin
                Inc(P);
                CRT.GotoXY(X + P, Y);
              end;
            else
              ;
            end;
          end;
    #8  : begin // backspace
            System.Delete(Text, P,1);
            Dec(P);
            if (P < 0) then P := 0;
            RefreshTexte(P);
          end;
    (*
    #127 : begin // del
            System.Delete(Text, P,1);
            RefreshTexte(P);

          end;
    //*)
    ' ',
    ',', ';', ':','!',
    'a' .. 'z',
    'A' .. 'Z',
    '0' .. '9',
    '+', '-', '.':
        begin
          System.Insert(ch, Text, P + 1);
          Inc(P);
          RefreshTexte(P);
        end;
    #13 : Result := True;
    #27 : WriteLn('ESC');
    end;
  until (ch = #27) or (ch = #13);

end;

function CRTMessageDlg(const S: array of string): boolean;
var
  n, i, MaxL, QX, QY: Integer;
  ch: Char;
begin
  Result := false;
  n := length(S);
  if (n = 0) then Exit;
  MaxL := 0;
  for i := 0 to n-1 do MaxL := Max(MaxL, length(S[i]));
  QX := 10; QY := 11;
  CRTDrawFenetre(QX, QY, 4 + MaxL, 2 + n, 4, 15, MOTIF_CADRE_WNDS, True, 'Information');
  for i := 0 to n - 1 do LocatePrint(QX + 2, QY + 1 + i, S[i]);
  repeat
    ch := ReadKey;
  until (ch = #27) or (ch = #13);

end;

end.

