unit frmSandbox;

{$mode delphi}

interface

uses
  GHCD_Types,
  GeneralFunctions,
  UnitDocDessin,
  CallDialogsGHCaveDraw,
  math,  UComplex,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, curredit, SynEdit, Types, LCLType, PopupNotifier;
type

  { TdlgBacASable }

  TdlgBacASable = class(TForm)
    btnDegradeColorForZCourant: TColorButton;
    btnColorZMaxi: TColorButton;
    btnColorForZCourant: TColorButton;
    btnColorZMini: TColorButton;
    btnSelectAcadColor: TButton;
    Button1: TButton;
    btnClearConsole: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    btnHierarchieObjets: TButton;
    Button6: TButton;
    ColorButton1: TColorButton;
    editAcadColorIndex: TCurrencyEdit;
    editNbIntervalles: TCurrencyEdit;
    editZMaxi: TCurrencyEdit;
    editZCurrent: TCurrencyEdit;
    editZMini: TCurrencyEdit;
    HeaderLstEchelleAltitudes: THeaderControl;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbColorOfAcadIdx: TStaticText;
    editConsole: TSynEdit;
    lsbColorScale: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnClearConsoleClick(Sender: TObject);
    procedure btnHierarchieObjetsClick(Sender: TObject);
    procedure btnSelectAcadColorClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure HeaderLstEchelleAltitudesSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbColorScaleDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure PopupNotifier1Close(Sender: TObject; var CloseAction: TCloseAction);
  strict private
    FEchelleCouleursZ: TColorScaleByAltitudes;
    procedure ListerEchelleCouleurs();
  private
    FDocumentDessin : TDocumentDessin;
    procedure DispMsg(const Msg: string);
  public
    function Initialiser(const FD: TDocumentDessin): boolean;

  end;

var
  dlgBacASable: TdlgBacASable;

implementation

{$R *.lfm}

{ TdlgBacASable }

procedure TdlgBacASable.btnSelectAcadColorClick(Sender: TObject);
var
  WU: Integer;
begin
  WU := SelectCouleurAutocad(editAcadColorIndex.AsInteger);
  editAcadColorIndex.AsInteger := WU;
  lbColorOfAcadIdx.Color       := Acad2RGB(WU);
end;

procedure TdlgBacASable.Button1Click(Sender: TObject);
var
  EWE: TColorScaleItem;
  i: Integer;
begin
  FEchelleCouleursZ.MakeRegularScale(editNbIntervalles.AsInteger,
                                     editZMini.Value, editZMaxi.Value,
                                     btnColorZMini.ButtonColor, btnColorZMaxi.ButtonColor);
  DispMsg(format('%d plages - Z = [%.2f; %.2f]',
                 [FEchelleCouleursZ.getNbElements(),
                  FEchelleCouleursZ.ZMini, FEchelleCouleursZ.ZMaxi
                  ]));
  for i := 0 to FEchelleCouleursZ.getNbElements() - 1 do
  begin
    EWE := FEchelleCouleursZ.getScaleItem(i);
    DispMsg(format('%d - %.2f, %X, %d', [i, EWE.ZMax, EWE.Couleur, EWE.AcadColorIdx]));
  end;
  ListerEchelleCouleurs();
end;

procedure TdlgBacASable.Button3Click(Sender: TObject);
var
  EWE: TColorScaleItem;
begin
  EWE := FEchelleCouleursZ.getScaleItem(lsbColorScale.ItemIndex);
  FEchelleCouleursZ.setColorScaleItemWithoutZMax(lsbColorScale.ItemIndex, ColorButton1.ButtonColor);
  ListerEchelleCouleurs();
end;

procedure TdlgBacASable.Button4Click(Sender: TObject);
begin
  btnColorForZCourant.ButtonColor        := FEchelleCouleursZ.getColorForZ(editZCurrent.Value);
  btnDegradeColorForZCourant.ButtonColor := FEchelleCouleursZ.getDegradeColorForZ(editZCurrent.Value);

end;

procedure TdlgBacASable.Button5Click(Sender: TObject);
begin
  DisplayToast(self, 'Mon toast');
end;

procedure TdlgBacASable.Button6Click(Sender: TObject);
begin

end;

procedure TdlgBacASable.HeaderLstEchelleAltitudesSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbColorScale.Invalidate;
end;

procedure TdlgBacASable.lsbColorScaleDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
  mg  = 1;
var
  EWE: TColorScaleItem;
  procedure DessineRectCouleur(const TB: integer; const W: integer;const Coul: TColor; const bg: TColor);
  var
    QR: TRect;
  begin
    lsbColorScale.Canvas.Pen.Style   := psSolid;
    lsbColorScale.Canvas.Brush.Style := bsSolid;
    lsbColorScale.Canvas.Brush.Color := Coul;
    lsbColorScale.Canvas.Pen.Color   := clBlack;
    QR.Left   := ARect.Left   + TB + 3;
    QR.Top    := ARect.Top    + 1;
    QR.Bottom := ARect.Bottom - 1;
    QR.Right  := QR.Left + W - 4;
    lsbColorScale.Canvas.Rectangle(QR);
    lsbColorScale.Canvas.Brush.Color := bg;
  end;
  procedure DessineFiletColonne(const TB: integer);
  begin
    lsbColorScale.Canvas.Pen.Color   := clSilver; // pour les filets
    lsbColorScale.Canvas.MoveTo(TB, ARect.Top);
    lsbColorScale.Canvas.LineTo(TB, ARect.Bottom);
  end;
  procedure DessineItem(const bg,tc: TColor);
  VAR
    HS: THeaderSection;
    QR: TRect;
  begin
    lsbColorScale.Canvas.Brush.Color := bg;
    lsbColorScale.Canvas.Font.Color  := tc;
    lsbColorScale.Canvas.FillRect(ARect);
    HS := HeaderLstEchelleAltitudes.Sections.Items[0];
    lsbColorScale.Canvas.TextOut(HS.Left + 4, ARect.Top,  Format('%d',[Index]));
    HS := HeaderLstEchelleAltitudes.Sections.Items[1];
    DessineFiletColonne(HS.Left - Q4);
    DessineRectCouleur(HS.Left - Q4, HS.Width, EWE.Couleur, bg);
    HS := HeaderLstEchelleAltitudes.Sections.Items[2];  // AutoCAD index couleur
    DessineFiletColonne(HS.Left - Q4);
    lsbColorScale.Canvas.TextOut(HS.Left + 4, ARect.Top,  Format('%d',[EWE.AcadColorIdx]));
    DessineFiletColonne(HS.Left - Q4);
    HS := HeaderLstEchelleAltitudes.Sections.Items[3];  // ZMax
    lsbColorScale.Canvas.TextOut(HS.Left + 4, ARect.Top,  Format('%.2f',[EWE.ZMax]));
  end;
begin
  try
    EWE := FEchelleCouleursZ.getScaleItem(Index);
    if (odSelected in state) then DessineItem(clBlue     , clWhite)
                             else DessineItem(clWhite    , clBlack);
  except
    pass;
  end;

end;

procedure TdlgBacASable.PopupNotifier1Close(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TdlgBacASable.ListerEchelleCouleurs();
var
  i, n: Integer;
begin
  lsbColorScale.Clear;
  n := FEchelleCouleursZ.getNbElements();
  for i := 0 to n-1 do lsbColorScale.Items.add(inttostr(i));
  lsbColorScale.ItemIndex := 0;
  lsbColorScale.Invalidate;
end;

procedure TdlgBacASable.btnClearConsoleClick(Sender: TObject);
begin
  editConsole.Clear;
end;

procedure TdlgBacASable.btnHierarchieObjetsClick(Sender: TObject);
begin
  DisplayHierarchieObjets(FDocumentDessin);
end;

procedure TdlgBacASable.DispMsg(const Msg: string);
begin
  editConsole.Lines.Add(Msg);
end;

function TdlgBacASable.Initialiser(const FD: TDocumentDessin): boolean;
var
  CoinBasGauche, CoinHautDroit: TPoint3Df;
begin
  result := false;
  FDocumentDessin := FD;
  FEchelleCouleursZ.Empty();
  editConsole.Clear;

  CoinBasGauche := FDocumentDessin.GetCoordsMini();
  CoinHautDroit := FDocumentDessin.GetCoordsMaxi();
  editZMini.Value := CoinBasGauche.Z;
  editZMaxi.Value := CoinHautDroit.Z;
  ListerEchelleCouleurs();
  result := true;
end;


end.

