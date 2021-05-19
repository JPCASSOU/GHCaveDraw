unit CadreListboxGroupes;

{$INCLUDE CompilationParameters.inc}

interface

uses
  GHCD_Types,
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GeneralFunctions,
  UnitDocDessin,
  Classes, SysUtils, Forms, Dialogs, Controls, StdCtrls, ComCtrls, LCLType, Graphics;

type   TCdrListBoxGroupes = class(TFrame)
    editFindingGroupe: TEdit;
    hcColonnes: THeaderControl;
    Label1: TLabel;
    lbNbGroupes: TLabel;
    lsbGroupes: TListBox;
    procedure editFindingGroupeChange(Sender: TObject);
    procedure hcColonnesSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbGroupesDblClick(Sender: TObject);
    procedure lsbGroupesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbGroupesSelectionChange(Sender: TObject; User: boolean);
  private
    FDocDessin: TDocumentDessin;
    FProcOnChangeTransmitIndex  : TProcTransmitIdxGroupe;
    FProcOnDblClickTransmitIndex: TProcTransmitIdxGroupe;

  public
    function  Initialiser(const FD: TDocumentDessin;
                          const QIdx: integer;
                          const QMultiSelect: boolean;
                          const ProcOnChange  : TProcTransmitIdxGroupe;
                          const ProcOnDblClick: TProcTransmitIdxGroupe): boolean;
    procedure Finaliser();
    function  GetSelectedIndex(): integer;
    function  IsSelectedItem(const Idx: integer): boolean;
    procedure ListerLesGroupes(const IdxGr: integer);
  end;

implementation
uses
  DGCDummyUnit;
{$R *.lfm}

{ TCdrListBoxGroupes }
function TCdrListBoxGroupes.Initialiser(const FD: TDocumentDessin;
                                        const QIdx: integer;
                                        const QMultiSelect: boolean;
                                        const ProcOnChange  : TProcTransmitIdxGroupe;
                                        const ProcOnDblClick: TProcTransmitIdxGroupe): boolean;
begin
  result := false;
  try
    FDocDessin             := FD;
    FProcOnChangeTransmitIndex       := ProcOnChange;
    FProcOnDblClickTransmitIndex     := ProcOnDblClick;
    lsbGroupes.ItemHeight  := 20;
    lsbGroupes.MultiSelect := QMultiSelect;
    ListerLesGroupes(QIdx);
    result := true;
  except
    pass;
  end;
end;

procedure TCdrListBoxGroupes.Finaliser();
begin
  pass;
end;

function TCdrListBoxGroupes.GetSelectedIndex(): integer;
begin
  Result := lsbGroupes.ItemIndex;
end;

procedure TCdrListBoxGroupes.ListerLesGroupes(const IdxGr: integer);
var
  n, WU, i: Integer;
  EWE: TGroupeEntites;
begin
  lsbGroupes.Visible := false;
  lsbGroupes.Clear;
  n := FDocDessin.GetNbGroupes();
  WU := IIF(IdxGr < 0, n-1, IdxGr);
  for i := 0 to n-1 do
  begin
    EWE := FDocDessin.GetGroupe(i);
    lsbGroupes.Items.Add(Format('%.3d (%.0f) - %s', [EWE.IDGroupeEntites, EWE.ZOrder, EWE.NomGroupe]));
  end;
  lsbGroupes.TopIndex  := IIF(IdxGr = 0, 0, WU - 1);
  lsbGroupes.ItemIndex := WU;
  lsbGroupes.Visible   := true;
  lbNbGroupes.Caption  := Format('%d groupes', [WU]);
end;

function TCdrListBoxGroupes.IsSelectedItem(const Idx: integer): boolean;
begin
  result := lsbGroupes.Selected[Idx];
end;

//********************************************************************************************************************************

procedure TCdrListBoxGroupes.hcColonnesSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbGroupes.Invalidate;
end;

procedure TCdrListBoxGroupes.lsbGroupesDblClick(Sender: TObject);
begin
  if (assigned(FProcOnDblClickTransmitIndex)) then FProcOnDblClickTransmitIndex(lsbGroupes.ItemIndex);
end;

procedure TCdrListBoxGroupes.lsbGroupesDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
  mg  = 1;
var
  MyGRP: TGroupeEntites;
  procedure DessineCoche(const TB: integer; const IsChecked: boolean; const bg: TColor);
  var
    QR: TRect;
    H : Integer;
  begin
    lsbGroupes.Canvas.Pen.Style   := psSolid;
    lsbGroupes.Canvas.Brush.Style := bsSolid;
    lsbGroupes.Canvas.Pen.Color   := clBlack;

    QR.Left   := ARect.Left   + TB + 2;
    QR.Top    := ARect.Top    + 1;
    QR.Bottom := ARect.Bottom - 1;
    H := QR.Bottom - QR.Top;
    QR.Right  := QR.Left + H;
    lsbGroupes.Canvas.Brush.Color := clWhite;
    if (IsChecked) then lsbGroupes.Canvas.Brush.Color := clRed;
    lsbGroupes.Canvas.Rectangle(QR);
    lsbGroupes.Canvas.Brush.Color := bg;
  end;
  procedure DessineRectCouleur(const TB: integer; const W: integer;const Coul: TColor; const bg: TColor);
  var
    QR: TRect;
  begin
    lsbGroupes.Canvas.Pen.Style   := psSolid;
    lsbGroupes.Canvas.Brush.Style := bsSolid;
    lsbGroupes.Canvas.Brush.Color := Coul;
    lsbGroupes.Canvas.Pen.Color   := clBlack;
    QR.Left   := ARect.Left   + TB + 3;
    QR.Top    := ARect.Top    + 1;
    QR.Bottom := ARect.Bottom - 1;
    QR.Right  := QR.Left + W - 4;
    lsbGroupes.Canvas.Rectangle(QR);
    lsbGroupes.Canvas.Brush.Color := bg;
  end;
  procedure DessineFiletColonne(const TB: integer);
  begin
    lsbGroupes.Canvas.Pen.Color   := clSilver; // pour les filets
    lsbGroupes.Canvas.MoveTo(TB, ARect.Top);
    lsbGroupes.Canvas.LineTo(TB, ARect.Bottom);
  end;
  procedure DessineItem(const bg,tc: TColor);
  VAR
    HS: THeaderSection;
    QR: TRect;
  begin
    lsbGroupes.Canvas.Brush.Color := bg;
    lsbGroupes.Canvas.Font.Color  := tc;
    lsbGroupes.Canvas.FillRect(ARect);
    HS := hcColonnes.Sections.Items[0];  // affiché
    // mettre en évidence les groupes vides
    if (MyGRP.NombreObjets = 0) then DessineRectCouleur(HS.Left - Q4, HS.Width, clFuchsia, bg);
    HS := hcColonnes.Sections.Items[1];  // affiché
    DessineCoche(HS.Left, MyGRP.Visible, bg);
    //HS := hcColonnes.Sections.Items[2];  // verrouillé
    //DessineCoche(HS.Left, MyGRP.Locked, bg);
    //DessineFiletColonne(HS.Left - Q4);
    HS := hcColonnes.Sections.Items[2];  // couleur
    lsbGroupes.Canvas.Brush.Color := MyGRP.CouleurGroupe;
    DessineRectCouleur(HS.Left - Q4, HS.Width, MyGRP.CouleurGroupe, bg);
    DessineFiletColonne(HS.Left - Q4);
    HS := hcColonnes.Sections.Items[3];  // num  groupe
    lsbGroupes.Canvas.TextOut(HS.Left + 4, ARect.Top,  Format('%d',[MyGRP.IDGroupeEntites]));
    DessineFiletColonne(HS.Left - Q4);
    HS := hcColonnes.Sections.Items[4];  // ZOrder
    lsbGroupes.Canvas.TextOut(HS.Left + 4, ARect.Top,  Format('%.0f',[MyGRP.ZOrder]));
    DessineFiletColonne(HS.Left - Q4);
    HS := hcColonnes.Sections.Items[5];  // Nom entrée
    DessineFiletColonne(HS.Left - Q4);
    lsbGroupes.Canvas.TextOut(HS.Left + 4, ARect.Top, MyGRP.NomGroupe);
  end;
begin
  try
    MyGRP := FDocDessin.GetGroupe(Index);
    if (odSelected in state) then DessineItem(clBlue     , clWhite)
                             else DessineItem(clWhite    , clBlack);
  except
    pass;
  end;
end;

procedure TCdrListBoxGroupes.lsbGroupesSelectionChange(Sender: TObject; User: boolean);
begin
  if (assigned(FProcOnChangeTransmitIndex)) then FProcOnChangeTransmitIndex(lsbGroupes.ItemIndex);
end;

procedure TCdrListBoxGroupes.editFindingGroupeChange(Sender: TObject);
var
  WU, P, n: Integer;
  EWE: TCaption;
  // recherche idx groupe d'après texte à rechercher
  function QFindIdxGroupeFoundByText(const S: string): integer;
  var
    i, Nb: Integer;
    fatxe: string;
    SP   : String;
    MyGroupe: TGroupeEntites;
  begin
    Result := -1;
    SP := LowerCase(Trim(S));
    Nb := FDocDessin.GetNbGroupes();
    if (Nb = 0) then Exit;
    for i := 0 to Nb -1 do
    begin
      MyGroupe := FDocDessin.GetGroupe(i);
      fatxe := LowerCase(Format('%d - %s', [MyGroupe.IDGroupeEntites, MyGroupe.NomGroupe]));
      if (Pos(SP, fatxe) > 0) then exit(i);
    end;
  end;
begin
  editFindingGroupe.Color := clWhite;
  // un signe égal en tête > on recherche d'après le numéro de groupe
  EWE := trim(editFindingGroupe.Text);
  P := pos('=', EWE);
  if (P = 1) then // pour une recherche sur no de groupe, le signe = doit être placé en tête.
  begin
    System.Delete(EWE, 1, 1); // on vire le égal
    n := StrToIntDef(EWE, -1);
    WU := FDocDessin.GetInternalIdxGroupe(n);
    if (WU > 0) then lsbGroupes.ItemIndex := WU  else editFindingGroupe.Color := clRed;
  end
  else // recherche sur la descro du groupe
  begin
    WU := QFindIdxGroupeFoundByText(EWE);
    if (WU > 0) then lsbGroupes.ItemIndex := WU  else editFindingGroupe.Color := clRed;
  end;
end;

end.

