unit PostScriptCanvasUnit;

interface
uses
  GHCD_Types,
  SysUtils,
  Classes,
  Graphics;



// Fontes pour PostScript
type TFontPSProperties = record
  Name  : string;
  Size  : integer;
  Height: integer;
  Color : TColor;
  Style : TFontStyles;
end;
type TPenPSProperties = record
  Name    : string;
  Color   : TColor;
  fWidth  : double;
  nWidth  : integer;
end;
type TBrushPSProperties = record
  Color   : TColor;
  Alpha   : integer;
end;

type

{ TPostScriptCanvas }

TPostScriptCanvas = class
  private
    FpPSFILE: TextFile;
    FNbWrittenLines: integer;
    FScale: double;

    FFont : TFontPSProperties;
    FPen  : TPenPSProperties;
    FBrush: TBrushPSProperties;
    FXMin,
    FXMax,
    FYMin,
    FYMax : Double;
    procedure DefineColor(const C: TColor);
    procedure WriteLine(const S: string);
  public
    function  InitializeDocument(const FileName, Commentaire: string): boolean;
    procedure FinalizeDocument;
    // écriture d'une ligne

    // limites du dessin
    procedure SetDrawingBounds(const X1, Y1, X2, Y2: Double);
    // définition des couleurs et fontes
    procedure SetPen(const Value: TPenPSProperties);
    procedure SetDefaultPen(const Value: TPenPSProperties);
    procedure SetDefaultBrush(const Value: TBrushPSProperties);

    function  GetPen: TPenPSProperties;
    procedure SetBrush(const Value: TBrushPSProperties);
    function  GetBrush: TBrushPSProperties;
    procedure SetFont(const Value: TFontPSProperties);

    //****************************
    // définition de commentaire
    procedure WriteCommentaire(const s: string);
    procedure WriteCommand(const s: string);
    // définition de couches
    procedure BeginLayer(const LayerName: string);
    procedure EndLayer(const LayerName: string);
    // mise à l'échelle
    procedure SetScale(const E: double);
    // routines de dessin
    procedure MoveTo(const X,Y: Double);
    procedure LineTo(const X,Y: Double);
    procedure DrawPoint(const X,Y: Double);
    procedure DrawLine(const X1, Y1, X2, Y2: Double);
    procedure DrawCircle(const XC, YC, R: double);
    procedure DrawPolylign(const Points: array of TPoint2Df);
    procedure DrawPolygon(const Points: array of TPoint2Df);
    procedure DrawBorderedPolygon(const Points: array of TPoint2Df);
    procedure TextOut(const X,Y: Double; const Text: string);
    //*******************
end;

implementation


procedure DisplayMsg(const Str: string);
begin
  ;//WriteLn(Str);
end;

function GetFloatRValue(const C: TColor): double;
begin
  Result := Red(C) / 256;
end;
function GetFloatGValue(const C: TColor): double;
begin
  Result := Green(C) / 256;
end;
function GetFloatBValue(const C: TColor): double;
begin
  Result := Blue(C) / 256;
end;

procedure TPostScriptCanvas.WriteLine(const S: string);
begin
  WriteLn(FpPSFILE, s);
end;
//**** Définition des pinceaux et couleurs
procedure TPostScriptCanvas.DefineColor(const C: TColor);
begin
  WriteLine(Format('%f %f %f srgb',[GetFloatRValue(C),
                                    GetFloatGValue(C),
                                    GetFloatBValue(C)]));
end;
function TPostScriptCanvas.GetPen: TPenPSProperties;
begin
  Result:=FPen;
end;
procedure TPostScriptCanvas.SetPen(const Value: TPenPSProperties);
begin
  FPen:=Value;
end;
procedure TPostScriptCanvas.SetBrush(const Value: TBrushPSProperties);
begin
  FBrush:=Value;
end;
function  TPostScriptCanvas.GetBrush: TBrushPSProperties;
begin
  Result:=FBrush;
end;

procedure TPostScriptCanvas.SetFont(const Value: TFontPSProperties);
begin
  WriteLine('/Helvetica findfont');
  WriteLine(Format('%d scalefont setfont',[Value.Size]));

end;
procedure TPostScriptCanvas.SetDefaultPen(const Value: TPenPSProperties);
begin
  WriteLine(Format('%f %f %f srgb',[GetFloatRValue(FPen.Color),
                                    GetFloatGValue(FPen.Color),
                                    GetFloatBValue(FPen.Color)]));
  WriteLine(Format('%f setlinewidth',[FPen.fWidth]));
end;
procedure TPostScriptCanvas.SetDefaultBrush(const Value: TBrushPSProperties);
begin
;
end;

//*******************
procedure TPostScriptCanvas.SetDrawingBounds(const X1, Y1, X2, Y2: Double);
begin
  FXMin:=X1;
  FYMin:=Y1;
  FXMax:=X2;
  FYMax:=Y2;
end;
procedure TPostScriptCanvas.WriteCommand(const s: string);
begin
  WriteLine(Trim(S));
end;
//****************************
procedure TPostScriptCanvas.WriteCommentaire(const s: string);
begin
  WriteLine(Format('%% %s %%',[s]));
end;
//****************************
function TPostScriptCanvas.InitializeDocument(const FileName, Commentaire: string): boolean;
  procedure DefinePSMacro(const Alias, Instrs, Comments: string);
  begin
    WriteLine(Format('/%s {%s} def %% %s',[Alias, Instrs, Comments]));
  end;
begin
  Result:=False;
  FNbWrittenLines:=0;
  assignFile(FpPSFILE, FileName);
  try
    ReWrite(FpPSFILE);
    //-------------------------------
    // écriture de l'en tête ici
    // ------------------------------
    WriteLine('%!PS-Adobe-3.0');
    WriteLine('%% PostScript File generated by GHTopo %%');
    WriteLine(Format('%%%% File    : %s %%%%',[FileName]));
    WriteLine(Format('%%%% Date    : %s %%%%',[DateToStr(Now)]));
    WriteLine(Format('%%%% Comments: %s %%%%',[Commentaire]));

    WriteLine(Format('%%%%%s%%%%',[StringOfChar('=',80)]));
    // écriture des alias
    WriteLine('% Alias list');
    DefinePSMacro('m', 'moveto', '');
    DefinePSMacro('l', 'lineto', '');
    DefinePSMacro('srgb', 'setrgbcolor', '');

    // le dessin
    WriteLine('');
    WriteLine('%% Drawing %%');
    WriteLine('');
    Result:=True;
  except
    DisplayMsg('Error initializing text file');
    CloseFile(FpPSFILE);
  end;
end;
procedure TPostScriptCanvas.FinalizeDocument;
begin
  try
    writeLine('showpage');
    WriteLine('%EOF');
  finally
    CloseFile(FpPSFILE);
  end;
end;
// définition de couches
procedure TPostScriptCanvas.BeginLayer(const LayerName: string);
begin
  WriteLine(Format('%% Begin Layer: %s %%',[LayerName]));
  WriteLine('%AI5_BeginLayer');

end;
procedure TPostScriptCanvas.EndLayer(const LayerName: string);
begin
  WriteLine('%AI5_EndLayer--');
  WriteLine(Format('%% End Layer: %s %%',[LayerName]));
end;
//********* routines de dessin
procedure TPostScriptCanvas.MoveTo(const X,Y: Double);
begin
  WriteLine('newpath');
  WriteLine(Format('  %f %f m',[X,Y]));
end;
// mise à l'échelle
procedure TPostScriptCanvas.SetScale(const E: double);
begin
  FScale := E;
end;

procedure TPostScriptCanvas.LineTo(const X,Y: Double);  (* peu utilisé *)
begin
  WriteLine(Format('  %f %f l',[X,Y]));
end;

procedure TPostScriptCanvas.DrawPoint(const X,Y: Double);
begin
;
end;
procedure TPostScriptCanvas.DrawCircle(const XC, YC, R: double);
begin
  WriteLine('newpath');
  WriteLine(Format('%f %f %f 0 360 arc',[XC, YC, R]));
  DefineColor(FPen.Color);
  WriteLine(Format('%f setlinewidth',[FPen.fWidth]));
  WriteLine('stroke');
end;

procedure TPostScriptCanvas.DrawLine(const X1, Y1, X2, Y2: Double);
begin
  WriteLine('newpath');
  WriteLine(Format('  %f %f m',[X1,Y1]));
  WriteLine(Format('  %f %f l',[X2,Y2]));
  DefineColor(FPen.Color);
  WriteLine(Format('%f setlinewidth',[FPen.fWidth]));
  WriteLine('stroke');
end;

procedure TPostScriptCanvas.DrawPolylign(const Points: array of TPoint2Df);
var
  i: integer;
begin
  if (High(Points)<1) then Exit;
  WriteLine('newpath');
  WriteLine(Format('  %f %f m',[Points[0].X, Points[0].Y]));
  for i:=1 to High(Points) do WriteLine(Format('  %f %f l',[Points[i].X, Points[i].Y]));
  DefineColor(FPen.Color);
  WriteLine(Format('%f setlinewidth',[FPen.fWidth]));
  WriteLine('stroke');
end;

procedure TPostScriptCanvas.DrawPolygon(const Points: array of TPoint2Df);
var
  i: integer;
begin
  if (High(Points)<1) then Exit;
  //WriteLine('newpath');
  WriteLine(Format('  %f %f m',[Points[0].X, Points[0].Y]));
  for i:=1 to High(Points) do WriteLine(Format('  %f %f l',[Points[i].X, Points[i].Y]));
  DefineColor(FBrush.Color);

  WriteLine('fill');
end;


procedure TPostScriptCanvas.DrawBorderedPolygon(const Points: array of TPoint2Df);
var
  i: integer;
begin
  if (High(Points)<1) then Exit;
  WriteLine('newpath');
  WriteLine(Format('  %f %f m',[Points[0].X, Points[0].Y]));
  for i:=1 to High(Points) do
    WriteLine(Format('  %f %f l',[Points[i].X, Points[i].Y]));
  WriteLine('gsave');
  DefineColor(FBrush.Color);
  WriteLine('fill');
  WriteLine('grestore');
  DefineColor(FPen.Color);
  WriteLine(Format('%f setlinewidth',[FPen.fWidth]));
  WriteLine('closepath');
  WriteLine('stroke');
end;

procedure TPostScriptCanvas.TextOut(const X,Y: Double; const Text: string);
begin
   WriteLine(Format('%f %f moveto (%s) show',[X,Y,Text]));
end;

end.

