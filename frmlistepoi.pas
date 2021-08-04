unit frmListePOI;
{$INCLUDE CompilationParameters.inc}

interface

uses
  Classes, SysUtils
  {$IFDEF LANGUAGE_FRENCH}
  , UnitMessages_fr
  {$ENDIF}
  , GHCD_Types
  , GeneralFunctions
  , UnitDocDessin
  , UnitListesSimplesWithGeneriques
  , Clipbrd
  , Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, Buttons, Types, LCLType, ExtCtrls;
type TPointOfInterest = record
  {$IFDEF TIDBASEPOINT_AS_TEXT}
  IDStation     : TIDBaseStation;
  {$ELSE}
  Serie         : integer;
  Point         : integer;
  {$ENDIF TIDBASEPOINT_AS_TEXT}
  Position      : TPoint3Df;
  IDTerrain     : string;
  Description   : string;
end;

type

  { TdlgListePOI }

  TdlgListePOI = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    hcColonnes: THeaderControl;
    lsbPOI: TListBox;
    lbNbPOIs: TStaticText;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure hcColonnesSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbPOIDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  private
    FDocDessin: TDocumentDessin;
    FListedesPOI: TListeSimple<TPointOfInterest>;
    procedure ListerPOI();
  public
    function Initialiser(const FD: TDocumentDessin): boolean;
    procedure Finaliser();

  end;

var
  dlgListePOI: TdlgListePOI;

implementation

{$R *.lfm}

{ TdlgListePOI }

procedure TdlgListePOI.lsbPOIDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
  mg  = 1;
var
  MyPOI: TPointOfInterest;
  procedure DessineRectCouleur(const TB: integer; const W: integer;const Coul: TColor; const bg: TColor);
  var
    QR: TRect;
  begin
    lsbPOI.Canvas.Pen.Style   := psSolid;
    lsbPOI.Canvas.Brush.Style := bsSolid;
    lsbPOI.Canvas.Brush.Color := Coul;
    lsbPOI.Canvas.Pen.Color   := clBlack;
    QR.Left   := ARect.Left   + TB + 3;
    QR.Top    := ARect.Top    + 1;
    QR.Bottom := ARect.Bottom - 1;
    QR.Right  := QR.Left + W - 4;
    lsbPOI.Canvas.Rectangle(QR);
    lsbPOI.Canvas.Brush.Color := bg;
  end;
  procedure DessineFiletColonne(const TB: integer);
  begin
    lsbPOI.Canvas.Pen.Color   := clSilver; // pour les filets
    lsbPOI.Canvas.MoveTo(TB, ARect.Top);
    lsbPOI.Canvas.LineTo(TB, ARect.Bottom);
  end;
  procedure DessineItem(const bg,tc: TColor);
  VAR
    HS: THeaderSection;
    QR: TRect;
  begin
   lsbPOI.Canvas.Brush.Color := bg;
   lsbPOI.Canvas.Font.Color  := tc;

   lsbPOI.Canvas.FillRect(ARect);
   HS := hcColonnes.Sections.Items[0];  // Station
   {$IFDEF TIDBASEPOINT_AS_TEXT}
   lsbPOI.Canvas.TextOut(HS.Left + 4, ARect.Top, Format(FORMAT_BASEPOINT, [MyPOI.IDStation]));
   {$ELSE}
   lsbPOI.Canvas.TextOut(HS.Left + 4, ARect.Top, Format('%d.%d',[MyPOI.Serie, MyPOI.Point]));
   {$endif TIDBASEPOINT_AS_TEXT}
   HS := hcColonnes.Sections.Items[1];  // IDTerrain
   DessineFiletColonne(HS.Left - Q4);
   lsbPOI.Canvas.TextOut(HS.Left + 4, ARect.Top, MyPOI.IDTerrain);
   HS := hcColonnes.Sections.Items[2];  // X
   DessineFiletColonne(HS.Left - Q4);
   lsbPOI.Canvas.TextOut(HS.Left + 4, ARect.Top, Format('%.2n', [MyPOI.Position.X]));  //FormatterNombreOOo(MyPOI.X, 0, false));
   HS := hcColonnes.Sections.Items[3];  // Y
   DessineFiletColonne(HS.Left - Q4);
   lsbPOI.Canvas.TextOut(HS.Left + 4, ARect.Top, Format('%.2n', [MyPOI.Position.Y]));  //FormatterNombreOOo(MyPOI.Y, 0, false));
   HS := hcColonnes.Sections.Items[4];  // Z
   DessineFiletColonne(HS.Left - Q4);
   lsbPOI.Canvas.TextOut(HS.Left + 4, ARect.Top, Format('%.2n', [MyPOI.Position.Z]));  //FormatterNombreOOo(MyPOI.Z, 0, false));
   HS := hcColonnes.Sections.Items[5];  // Descro
   DessineFiletColonne(HS.Left - Q4);
   lsbPOI.Canvas.TextOut(HS.Left + 4, ARect.Top,  MyPOI.Description);
  end;
begin
  if (0 = FListedesPOI.GetNbElements()) then Exit;
  MyPOI := FListedesPOI.GetElement(Index);
  DessineItem(clWhite, clBlack);
  if (odSelected in state) then
  begin
    lsbPOI.Canvas.brush.color := clBlue;
    DessineItem(clBlue, clWhite);
  end;
end;

procedure TdlgListePOI.hcColonnesSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbPOI.Invalidate;
end;

procedure TdlgListePOI.Button1Click(Sender: TObject);
const TAB = #9;
var
  //MyClipboard: TClipboard;
  i, Nb: LongInt;
  MyPOI: TPointOfInterest;
  EWE: String;
begin
  Nb := FListedesPOI.GetNbElements();
  if (0 = Nb) then Exit;
  //MyClipboard := TClipboard.Create(ctClipboard);
  try
    Clipboard.Clear;
    EWE := '';
    for i := 0 to Nb - 1 do
    begin
      MyPOI := FListedesPOI.GetElement(i);
     {$IFDEF TIDBASEPOINT_AS_TEXT}
     EWE   += Format(FORMAT_BASEPOINT, [MyPOI.IDStation]) + TAB +
     {$ELSE}
     EWE   += Format('%d.%d', [MyPOI.Serie, MyPOI.Point]) + TAB +
     {$endif TIDBASEPOINT_AS_TEXT}
               MyPOI.IDTerrain + TAB +
               FormatterNombreOOo(MyPOI.Position.X, 3, false) + TAB +
               FormatterNombreOOo(MyPOI.Position.Y, 3, false) + TAB +
               FormatterNombreOOo(MyPOI.Position.Z, 3, false) + TAB +
               MyPOI.Description +
               #13#10;
    end;
    Clipboard.AsText := EWE;
  finally
    //FreeAndNil(Clipboard);
  end;
end;

procedure TdlgListePOI.ListerPOI();
var
  i, Nb: LongInt;
  MyPOI     : TPointOfInterest;
begin
  lsbPOI.Clear;
  Nb := FListedesPOI.GetNbElements();
  if (0 = Nb) then Exit;
  for i := 0 to Nb - 1 do
  begin
    MyPOI := FListedesPOI.GetElement(i);
    {$IFDEF TIDBASEPOINT_AS_TEXT}
    lsbPOI.Items.add(format(FORMAT_BASEPOINT + ' - %s', [MyPOI.IDStation, MyPOI.Description]));
    {$ELSE}
    lsbPOI.Items.add(format('%d.%d - %s', [MyPOI.Serie, MyPOI.Point, MyPOI.Description]));
    {$endif TIDBASEPOINT_AS_TEXT}
  end;
  lsbPOI.ItemIndex := 0;
  lbNbPOIs.Caption := Format('%d POIs', [Nb]);
end;

function TdlgListePOI.Initialiser(const FD: TDocumentDessin): boolean;
var
  BP        : TBaseStation;
  i, Nb     : Integer;
  MySymbole : TSymbole;
  MyPOI     : TPointOfInterest;
  EWE: TToporobotIDStation;
begin
  result := false;
  self.Caption := 'Points Of Interest';
  FDocDessin := FD;
  FListedesPOI := TListeSimple<TPointOfInterest>.Create;
  try
    FListedesPOI.ClearListe();
    Nb := FDocDessin.GetNbSymboles();
    if (Nb > 0) then
    begin
      for i := 0 to Nb - 1 do
      begin
        MySymbole := FDocDessin.GetSymbole(i);
        if (nosPOINT_REMARQUABLE = MySymbole.TypeObject) then
        begin
          if (FDocDessin.GetBasePointByIndex(MySymbole.IDBaseStation, BP)) then
          begin
            {$IFDEF TIDBASEPOINT_AS_TEXT}
            MyPOI.IDStation := MySymbole.IDBaseStation;
            MyPOI.IDTerrain := '';
            {$ELSE}
            EWE := BP.GetToporobotIDStation();
            MyPOI.Serie     := EWE.aSerie;
            MyPOI.Point     := EWE.aStation;
            MyPOI.IDTerrain := EWE.aIDTerrain;
            {$ENDIF TIDBASEPOINT_AS_TEXT}
            MyPOI.Description := MySymbole.TagTexte;
            MyPOI.Position := BP.PosStation;
            FListedesPOI.AddElement(MyPOI);
          end;
        end;
      end;
    end;
    ListerPOI();
    Result := True;
  except

  end;
end;

procedure TdlgListePOI.Finaliser();
begin
  try
    FListedesPOI.ClearListe();
  finally
    FreeAndNil(FListedesPOI);
  end;
end;
end.
