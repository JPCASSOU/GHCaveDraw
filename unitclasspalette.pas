unit UnitClassPalette;

{$mode delphi}

interface

uses
  GHCD_Types,
  GeneralFunctions,
  Classes, SysUtils, Graphics;
type TMacintoshColor = record
  R, G, B: word;
end;
type TArray256Colors = array[0 .. 255] of TColor;

function Acad2RGB(const n : integer) : tColor; deprecated; // palette par défaut d'Autocad
function RGB2Acad(const C: TColor) : Byte; deprecated;

type TPalette256 = class
  private
    FColorArray : TArray256Colors;
    procedure ViderTableCouleurs();
  public
    procedure GenerateWeb216Palette(); deprecated;
    procedure GenerateACADPalette(); deprecated;
    procedure GenerateGrayScalePalette(); deprecated;
    //procedure GenerateTOPOROBOTPalette();
    function  GetColorByIndex(const Idx: Integer):TColor;
    procedure Finaliser();
end;

implementation
uses DGCDummyUnit;
// Conversions couleurs PC<>Mac
(*
function MacColorToPCColor(const MC: TMacintoshColor): TColor; overload;
begin
  Result := MacColorToPCColor(MC.R, MC.G, MC.B);
end;
function MacColorToPCColor(const mR, mG, mB: word):TColor; overload;
begin
  Result := RGBToColor(mR shr 8, mG shr 8, mB shr 8);
end;
function PCColorToMacColor(const PCC: TColor): TMacintoshColor;
begin
  Result.R := Red(PCC) * 256;
  Result.G := Green(PCC) * 256;
  Result.B := Blue(PCC) * 256;
end;
//*)

function Acad2RGB(const n : integer) : tColor; // palette par défaut d'Autocad
var r,g,b,
    c,d,u : integer;
    C1 : Tcolor;  // RGB(r, g ,b)
    const StandCol : array[0..9] of tcolor =
        (clBlack ,clRed,$0001E1F3{clYellow},$0000C800{clLime},$00FDE302{clAqua},
         clBlue,clFuchsia,clBlack,clGray,clLtGray);
    const FullPalete : array[0..23] of tcolor =
        ($0000FF,$0040FF,$0080FF,$00C0FF,
         $00FFFF,$00FFC0,$00FF80,$00FF40,
         $00FF00,$40FF00,$80FF00,$C0FF00,
         $FFFF00,$FFC000,$FF8000,$FF4000,
         $FF0000,$FF0040,$FF0080,$FF00C0,
         $FF00FF,$C000FF,$8000FF,$4000FF);//..$0000FF (retour au début)
begin
  c := n mod 256; // au cas ou ?
  if (c < 10) then exit(StandCol[c]);

  d := ((c-10) div 10);// 0..23
  // Couleur de base à corriger
  C1 := FullPalete[d mod 24];
  // Correction:--------------------------------
  d := c div 10; // dizaines
  u := c-d*10; // unités
  // séparation des couleurs RGB
  b := (C1 and $FF0000) shr 16;
  g := (C1 and $00FF00) shr 8;
  r := (C1 and $0000FF);
  //Plus clair pour les impairs
  if ((u div 2)*2<>u) then
  begin
    b := b + ((255-b) div 2);
    g := g + ((255-g) div 2);
    r := r + ((255-r) div 2);
  end;
  // Plus foncé si u grand
  b := b*4 div (u+4);
  g := g*4 div (u+4);
  r := r*4 div (u+4);
  // Couleur corrigée:---------------------------
  C1 := RGBToColor(r,g,b);
  result := C1;
end;
function RGB2Acad(const C: TColor): Byte; deprecated;
var dRMin, dGmin, dBmin  :integer;
    CouleurR, CouleurG, CouleurB: integer;
    ACcolor,  dColor, res       : integer;
    ACcolorR, ACcolorG, ACcolorB: integer;
begin
    Result:=0;
    dRmin := 99999999;
    dGmin := 99999999;
    dBmin := 99999999;
    CouleurR := Red(C);
    CouleurG := Green(C);
    CouleurB := Blue(C);
    for res := 1 to 255 do
    begin
      ACcolor := Acad2RGB(res);
      ACcolorR := Red(ACcolor);
      ACcolorG := Red(ACcolor);
      ACcolorB := Blue(ACcolor);
      dColor := abs(ACcolorR - CouleurR)+
                abs(ACcolorG - CouleurG)+
                abs(ACcolorB - CouleurB);
      if (dColor < dRmin) then
      begin
          dRmin := dColor;
          result := res;
      end;
    end;
end;

// dégradé de couleurs
function GetColorDegrade(const z, zmin, zmax: Double; const Coul1, Coul2: TColor): TColor;
var
  D: Double;
  H: Double;
  DR, DG, DB : integer; // et non byte !!!
begin
  if (Z < ZMin) then Exit(Coul1);
  if (Z > zmax) then Exit(Coul2);
  D := zmax - zmin;
  if (Abs(D) < 1e-8) then Exit(Coul1);
  H := (z - zmin) / D;
  DR := Red(Coul2)   - Red(Coul1);
  DG := Green(Coul2) - Green(Coul1);
  DB := Blue(Coul2)  - Blue(Coul1);
  Result := RGBToColor(Red(Coul1)   + Trunc(DR * H),
                       Green(Coul1) + Trunc(DG * H),
                       Blue(Coul1)  + Trunc(DB * H));
end;

function TPalette256.GetColorByIndex(const Idx: Integer): TColor;
begin
  Result := clSilver;
  if (IsInRange(Idx, 0, High(self.FColorArray))) then Result := self.FColorArray[Idx];
end;

procedure TPalette256.Finaliser();
begin
  ;;
end;

procedure TPalette256.ViderTableCouleurs();
var
  i: Integer;
begin
  for i:= 0 to High(self.FColorArray) - 1 do FColorArray[i] := clSilver;
end;

// palettes non utilisées par GHTopo
procedure TPalette256.GenerateGrayScalePalette();
var
  i: byte;
begin
 ViderTableCouleurs();
 for i:=0 to High(FColorArray) do FColorArray[i] := RGBToColor(i,i,i);
end;

procedure TPalette256.GenerateACADPalette();
var
  i: integer;
begin
  ViderTableCouleurs();
  for i:=0 to 255 do FColorArray[i] := Acad2RGB(i);
end;
procedure TPalette256.GenerateWeb216Palette();
var
  i, j, k  : integer;
  r,g,b    : byte;
  IdxColor : integer;
begin
  ViderTableCouleurs();
  for i := 1 to 6 do
    for j := 1 to 6 do
      for k := 1 to 6 do
        begin
          IdxColor := (i-1) * 36 + (j-1) * 6 + (k-1);
          r := 255 - (i-1) * 51;
          g := 255 - (j-1) * 51;
          b := 255 - (k-1) * 51;
          FColorArray[IdxColor] := RGBToColor(r,g,b);
        end;
end;
end.
////////////////////////////////////////////////////////////////////////////////
(*
procedure TPalette256.GenerateTOPOROBOTPalette();
const
  M4369  = 4369;
  M13107             = 13107;
  TWO_TIMES_M13107   = 2 * M13107;
  THREE_TIMES_M13107 = 3 * M13107;
  FOUR_TIMES_M13107  = 4 * M13107;
  FIVE_TIMES_M13107  = 5 * M13107;
  M48059 = 48059;
  M56797 = 56797;
  M61166 = 61166;
var
  i,j,k,q,r: integer;
  procedure Cuicui(const qr, qg, qb: byte);
  var
    WU, t: Integer;
  begin
    q := q + 1; FColorArray[q] := MacColorToPCColor(qr * M61166, qg * M61166, qb * M61166);
    q := q + 1; FColorArray[q] := MacColorToPCColor(qr * M48059, qg * M48059, qb * M48059);
    r := 0;
    for t := 1 to 7 do
    begin
      q := q + 1;
      r := r + IIF((t mod 2 <>0), M4369, 2 * M4369);
      WU := M48059 - r;
      FColorArray[q]:=MacColorToPCColor(qr * WU, qg * WU, qb * WU);
    end;
  end;
begin
  AfficherMessage(Format('%s.GenerateTOPOROBOTPalette',[ClassName]));
  ViderTableCouleurs();
  FColorArray[0] := clWhite;
  FColorArray[1] := clBlack;
  FColorArray[2] := MacColorToPCColor(30583, 30583 , 30583);
  FColorArray[3] := MacColorToPCColor(21845, 21845 , 21845);
  FColorArray[4] := MacColorToPCColor(65535, 65535 , 0);
  FColorArray[5] := MacColorToPCColor(65535, 26214 , 0); {5}
  FColorArray[6] := MacColorToPCColor(56797, 0     , 0); {6}
  FColorArray[7] := MacColorToPCColor(65535, 0     , 39321); {7}
  FColorArray[8] := MacColorToPCColor(26214, 0     , 39321); {8}
  FColorArray[9] := MacColorToPCColor(0    , 0     , 56797); {9}
  FColorArray[10]:= MacColorToPCColor(0    , 39321 , 65535); {10}
  FColorArray[11]:= MacColorToPCColor(0    , M61166, 0); {11}
  FColorArray[12]:= MacColorToPCColor(0    , 26214 , 0); {12}
  for i:=1 to 2 do
    FColorArray[12+i]:=MacColorToPCColor(M13107*(i+1), M13107 * i, M13107 * (i-1));
  FColorArray[15] := MacColorToPCColor(M48059, M48059, M48059);
  for i:=16 to 19 do  FColorArray[i] := MacColorToPCColor(FIVE_TIMES_M13107, FIVE_TIMES_M13107, M13107 * (20-i));
  // palette calculée
  q := 19;
  for i:=4 downto 2 do
    for j:=5 downto 0 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(FIVE_TIMES_M13107, i*M13107, j*M13107); end;
  q := 36;
  for i := 5 downto 0 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(FIVE_TIMES_M13107, M13107, i*M13107); end;
  for i := 5 downto 4 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(FIVE_TIMES_M13107, 0, i*M13107); end;
  for i := 2 downto 0 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(FIVE_TIMES_M13107, 0, i*M13107); end;
  for j:=5 downto 0 do
    for k:=5 downto 0 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(FOUR_TIMES_M13107, j*M13107, k*M13107); end;
  for j:=5 downto 3 do
    for k:=5 downto 0 do begin q := q + 1; FColorArray[q]:=MacColorToPCColor(THREE_TIMES_M13107,j*M13107, k*M13107); end;
  for k := 5 downto 2 do begin q := q + 1; FColorArray[q]:=MacColorToPCColor(THREE_TIMES_M13107,TWO_TIMES_M13107, k*M13107); end;
  q := q + 1;
  FColorArray[q] := MacColorToPCColor(THREE_TIMES_M13107,2*M13107, 0);
  for j:=1 downto 0 do
    for k:=5 downto 0 do begin q := q + 1; FColorArray[q]:=MacColorToPCColor(THREE_TIMES_M13107,j*M13107, k*M13107); end;
  for j:=5 downto 1 do
    for k:=5 downto 0 do begin q := q + 1; FColorArray[q]:=MacColorToPCColor(TWO_TIMES_M13107,j*M13107, k*M13107); end;
  q:=q-1;
  for k:=5 downto 4  do begin  q := q + 1; FColorArray[q]:=MacColorToPCColor(TWO_TIMES_M13107,0, k*M13107); end;
  for k:=2 downto 0  do begin  q := q + 1; FColorArray[q]:=MacColorToPCColor(TWO_TIMES_M13107,0, k*M13107); end;
  for j:=5 downto 0 do
    for k:=5 downto 0 do begin q := q + 1; FColorArray[q]:=MacColorToPCColor(M13107,j*M13107, k*M13107); end;
  for j:=5 downto 4 do
    for k:=5 downto 0 do begin q := q + 1; FColorArray[q]:=MacColorToPCColor(0,j*M13107, k*M13107); end;
  for k:=4 downto 0 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(0, THREE_TIMES_M13107, k*M13107); end;
  for k:=5 downto 1 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(0, TWO_TIMES_M13107, k*M13107); end;
  for k:=5 downto 0 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(0, M13107, k*M13107); end;
  for k:=5 downto 1 do begin q := q + 1; FColorArray[q] := MacColorToPCColor(0, 0, k*M13107); end;
  Cuicui(1, 0, 0);
  Cuicui(0, 1, 0);
  Cuicui(0, 0, 1);
  r := M61166; FColorArray[q + 1] := MacColorToPCColor(r,r,r);
  r := M56797; FColorArray[q + 2] := MacColorToPCColor(r,r,r);
  r := 43690 ; FColorArray[q + 3] := MacColorToPCColor(r,r,r);
  r := 34952 ; FColorArray[q + 4] := MacColorToPCColor(r,r,r);
  for i:=1 to 3 do begin
    r:=r div 2;
    FColorArray[q+4+i]:=MacColorToPCColor(r,r,r);
    //AfficherMessageErreur(inttostr(q+4+i)); OK
  end;
end;
//*)
