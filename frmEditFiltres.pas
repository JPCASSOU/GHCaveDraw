unit frmEditFiltres;

{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  ToporobotClasses2012,
  Common, Math,
  CallDialogsStdVersion, UnitObjetSerie,
  Clipbrd,
  Classes, SysUtils, FileUtil, SynPluginSyncroEdit, SynEdit, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, ExtCtrls, ComCtrls, Types, LCLType, PairSplitter, SynGutterBase, SynEditMarks, curredit;

type

  { TdlgEditFiltres }

  TdlgEditFiltres = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnAddFiltre: TButton;
    btnAddLastFiltre: TButton;
    btnCopierLesFiltres: TButton;
    btnCopyExpression: TButton;
    btnFilterColor: TColorButton;
    btnModifier: TButton;
    btnSupprimer: TButton;
    btnViderLesFiltres: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    cmbFiltres: TComboBox;
    CurrencyEdit3: TCurrencyEdit;
    editFilterDescription: TEdit;
    editFilterExpression: TSynEdit;
    editFilterName: TEdit;
    editNumItem: TCurrencyEdit;
    editPtStart: TCurrencyEdit;
    editPtEnd: TCurrencyEdit;
    hcColsTitres: THeaderControl;
    lbPtFrom: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbPtTo: TLabel;
    lbDescription: TLabel;
    lbExpression: TLabel;
    lbNbFiltresPerso: TLabel;
    lbNomFiltre: TLabel;
    lsbFiltresPerso: TListBox;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlHelperForFiltre: TPanel;
    procedure btnAddFiltreClick(Sender: TObject);
    procedure btnAddLastFiltreClick(Sender: TObject);
    procedure btnCopierLesFiltresClick(Sender: TObject);
    procedure btnCopyExpressionClick(Sender: TObject);
    procedure btnModifierClick(Sender: TObject);
    procedure btnSupprimerClick(Sender: TObject);
    procedure btnViderLesFiltresClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure cmbFiltresChange(Sender: TObject);
    procedure editFilterExpressionChange(Sender: TObject);
    procedure editFilterExpressionGutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
    procedure hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure lsbFiltresPersoDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsbFiltresPersoSelectionChange(Sender: TObject; User: boolean);
  private
    FDocTopo: TToporobotStructure2012;
    FTemporaryFilter: TFiltrePersonnalise;
    FEditorExprCurrentLine: integer;
    function  GetFiltreFromForm(): TFiltrePersonnalise;
    procedure ListerLesFiltres(const Index: integer);
    procedure PreparerEtAfficherHelperFiltre(const Line: integer; const TxtFiltre: string);
    function  PreparerFiltre(): string;
    procedure PutFiltreInForm(const FF: TFiltrePersonnalise);
  public
    function Initialiser(const FD: TToporobotStructure2012; const F: string; const DoUseFiltre: boolean): boolean;
    function GetFiltre(): string;
  end;

var
  dlgEditFiltres: TdlgEditFiltres;

implementation

{$R *.lfm}
const
  TAB = #9;
  QFMT = '%d' + TAB + '%d' + TAB + '%d' + TAB + '%s' + TAB + '%s' + TAB + '%s' + #13#10;

procedure TdlgEditFiltres.btnAddFiltreClick(Sender: TObject);
var
  FF: TFiltrePersonnalise;
begin
  FF := GetFiltreFromForm();
  AfficherMessageErreur('Ajout de ' + FF.Expression);
  FDocTopo.AddFiltrePerso(FF);
  ListerLesFiltres(-1);
end;

procedure TdlgEditFiltres.btnAddLastFiltreClick(Sender: TObject);
begin
  FDocTopo.AddFiltrePerso(FTemporaryFilter);
  ListerLesFiltres(-1);
end;



procedure TdlgEditFiltres.btnCopierLesFiltresClick(Sender: TObject);
var
  EWE: String;
  MyClipBoard: TClipboard;
  i, Nb: Integer;
  FF: TFiltrePersonnalise;
begin
  Nb := FDocTopo.GetNbFiltresPersos();
  if (Nb = 0) then Exit;
  MyClipBoard := TClipboard.Create(ctClipboard);
  try
    MyClipBoard.Clear;
    // contenu
    EWE := 'R' + TAB + 'G' + TAB + 'B' + TAB + 'Nom du filtre' + TAB + 'Description' + TAB + 'Expression' + #13#10;
    for i := 0 to Nb - 1 do
    begin
      FF := FDocTopo.GetFiltrePerso(i);
      EWE += Format(QFMT, [Red(FF.CouleurFiltre), Green(FF.CouleurFiltre), Blue(FF.CouleurFiltre),
                           FF.NomFiltre, FF.Description, FF.Expression]);
    end;
    MyClipBoard.AsText := EWE;
  finally
    MyClipBoard.Free;
  end;
end;

procedure TdlgEditFiltres.btnCopyExpressionClick(Sender: TObject);
var
  MyClipBoard: TClipboard;
  FF: TFiltrePersonnalise;
begin
  MyClipBoard := TClipboard.Create(ctClipboard);
  try
    MyClipBoard.Clear;
    // contenu
    FF := GetFiltreFromForm();
    MyClipBoard.AsText := Format(QFMT, [Red(FF.CouleurFiltre), Green(FF.CouleurFiltre), Blue(FF.CouleurFiltre),
                                        FF.NomFiltre, FF.Description, FF.Expression]);
  finally
    MyClipBoard.Free;
  end;
end;

procedure TdlgEditFiltres.btnModifierClick(Sender: TObject);
var
  FF: TFiltrePersonnalise;
begin
  if (not lsbFiltresPerso.ItemIndex >= 0) then exit;
  FF := GetFiltreFromForm();
  FDocTopo.PutFiltrePerso(lsbFiltresPerso.ItemIndex, FF);
  ListerLesFiltres(lsbFiltresPerso.ItemIndex);
end;

procedure TdlgEditFiltres.btnSupprimerClick(Sender: TObject);
var
  n: Integer;
begin
  n := lsbFiltresPerso.ItemIndex;
  if ((n >= 0) and (GHTopoQuestionOuiNon('Supprimer filtre'))) then
  begin
    FDocTopo.RemoveFiltrePerso(n);
    ListerLesFiltres(0);
  end;
end;

procedure TdlgEditFiltres.btnViderLesFiltresClick(Sender: TObject);
begin
  if (GHTopoQuestionOuiNon('Supprimer tous les filtres')) then
  begin
    FDocTopo.ViderListeFiltres();
    ListerLesFiltres(0);
  end;
end;

procedure TdlgEditFiltres.Button1Click(Sender: TObject);
var
  QNumSerie: TNumeroSerie;
  QSR: TObjSerie;
  QIdxSerie, n: integer;
  QCC: TCode;
  QEX: TExpe;
begin
  case cmbFiltres.ItemIndex of
    kFLT_SERIE:
      begin
        n := 1;
        if (SelectionDansListe(FDocTopo, mslSERIE, True, n)) then
        begin
          QSR := FDocTopo.GetSerie(n);
          editNumItem.AsInteger := QSR.GetNumeroDeSerie();
          editPtStart.AsInteger := 1;
          editPtEnd.AsInteger   := QSR.GetNbVisees() - 1;
        end;
      end;
    kFLT_RESEAU:
      begin
        n := 0;
        if (SelectionDansListe(FDocTopo, mslRESEAUX, True, n)) then editNumItem.AsInteger := n;
      end;
    kFLT_SECTEUR:
      begin
        n := 0;
        if (SelectionDansListe(FDocTopo, mslSECTEURS, True, n)) then editNumItem.AsInteger := n;
      end;
    kFLT_ENTRANCE_RATT:
      begin
        n := 0;
        if (SelectionDansListe(FDocTopo, mslENTRANCES, True, n)) then editNumItem.AsInteger := n;
      end;
    kFLT_CODE:
      begin
        n := 1;
        if (SelectionDansListe(FDocTopo, mslCODE, True, n)) then
        begin
          QCC := FDocTopo.GetCode(n);
          editNumItem.AsInteger := QCC.IDCode;
        end;
      end;
    kFLT_EXPE:
      begin
        n := 1;
        if (SelectionDansListe(FDocTopo, mslEXPE, True, n)) then
        begin
          QEX := FDocTopo.GetExpe(n);
          editNumItem.AsInteger := QEX.IDExpe;
        end;
      end;
    kFLT_COULEUR:
      begin
        editNumItem.AsInteger := SelectionCouleurToporobot(1);
      end;
  otherwise
    pass;
  end;


end;

procedure TdlgEditFiltres.Button2Click(Sender: TObject);
var
  Q1, Q2: Integer;
  EWE: String;
  procedure fatche(const F: string); inline;
  begin
    editFilterExpression.Lines[FEditorExprCurrentLine - 1] := Format('%s=%d', [F, editNumItem.AsInteger]);
  end;
begin
  case cmbFiltres.ItemIndex of
    kFLT_SERIE:
    begin
      Q1 := Min(editPtStart.AsInteger, editPtEnd.AsInteger);
      Q2 := Max(editPtStart.AsInteger, editPtEnd.AsInteger);
      EWE := Format('%s=%d:%d-%d', [rsMETAFILTRE_SERIE, editNumItem.AsInteger, Q1, Q2]);
      editFilterExpression.Lines[FEditorExprCurrentLine - 1] := EWE;
    end;
    kFLT_CODE    : fatche(rsMETAFILTRE_CODE);
    kFLT_EXPE    : fatche(rsMETAFILTRE_EXPE);
    kFLT_RESEAU  : fatche(rsMETAFILTRE_RESEAU);
    kFLT_SECTEUR : fatche(rsMETAFILTRE_SECTEUR);
    kFLT_COULEUR : fatche(rsMETAFILTRE_COLOR);
  otherwise
    pass;
  end;
  pnlHelperForFiltre.Visible := false;
end;

procedure TdlgEditFiltres.Button3Click(Sender: TObject);
begin
  pnlHelperForFiltre.Visible := false;
end;

procedure TdlgEditFiltres.cmbFiltresChange(Sender: TObject);
  procedure miou(const B: boolean);
  begin
    if (not pnlHelperForFiltre.Visible) then exit;
    lbPtFrom.Visible    := B;
    lbPtTo.Visible      := B;
    editPtStart.Visible := B;
    editPtEnd.Visible   := B;
  end;
begin
  miou(cmbFiltres.ItemIndex = kFLT_SERIE);

end;

procedure TdlgEditFiltres.editFilterExpressionChange(Sender: TObject);
begin

end;

procedure TdlgEditFiltres.PreparerEtAfficherHelperFiltre(const Line: integer; const TxtFiltre: string);
begin
  pnlHelperForFiltre.Visible := not pnlHelperForFiltre.Visible;
  if (not pnlHelperForFiltre.Visible) then Exit;
  FEditorExprCurrentLine := Line;
  pnlHelperForFiltre.top  := editFilterExpression.Top  + (FEditorExprCurrentLine - 1) * editFilterExpression.LineHeight;
end;

procedure TdlgEditFiltres.editFilterExpressionGutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
begin
  PreparerEtAfficherHelperFiltre(Line, editFilterExpression.Lines[Line]);
end;

procedure TdlgEditFiltres.hcColsTitresSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  lsbFiltresPerso.Invalidate;
end;

procedure TdlgEditFiltres.lsbFiltresPersoDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  Q4  = 4; // compensation du décalage entre le header et la liste
  mg  = 1;
var
  FF: TFiltrePersonnalise;
  procedure DessineCoche(const IsChecked: boolean);
  var
    QR: TRect;
    H: Integer;
  begin
    with lsbFiltresPerso.Canvas do
    begin
      Pen.Style    := psSolid;
      Brush.Style  := bsSolid;
      Pen.Color    := clBlack;
      QR.Left      := ARect.Left   + 1;
      QR.Top       := ARect.Top    + 1;
      QR.Bottom    := ARect.Bottom - 1;
      H            := QR.Bottom - QR.Top;
      QR.Right     := QR.Left + H;
      Brush.Color  := IIF(IsChecked, clBlue, clWhite);
      Rectangle(QR);
    end;
  end;
  procedure DessineFiletColonne(const TB: integer); inline;
  begin
    lsbFiltresPerso.Canvas.Line(TB, ARect.Top, TB, ARect.Bottom);
  end;
  // bg = couleur de fond; tc= couleur du texte
  procedure DessineItem(const bg,tc: TColor);
  VAR
    //TB: integer;
    QR: TRect;
    HS: THeaderSection;
  begin
    with lsbFiltresPerso do
    begin
      Canvas.FillRect(ARect);
      Canvas.Brush.Color := bg;
      Canvas.Font.Color  := tc;
      Canvas.Pen.Color   := clSilver; // pour les filets
      HS := hcColsTitres.Sections.Items[0];  // ID filtre
      canvas.TextOut(HS.Left + 4, ARect.Top+1,  Format('%d',[Index]));
      HS := hcColsTitres.Sections.Items[1];  // Couleur filtre
      DessineFiletColonne(HS.Left - Q4);
      canvas.Pen.Color   := clBlack;
      canvas.Brush.Color := FF.CouleurFiltre;
      QR := Rect(HS.Left, ARect.Top + mg, HS.Right - 8, ARect.Bottom - mg);
      canvas.Rectangle(QR);
      Canvas.Brush.Color   :=bg;
      canvas.Pen.Color    :=clSilver;
      HS := hcColsTitres.Sections.Items[2];  // Nom filtre
      QR := Rect(HS.Left, ARect.Top + mg, HS.Right - 8, ARect.Bottom - mg);
      DessineFiletColonne(HS.Left - Q4);
      canvas.TextRect(QR, HS.Left + 4, ARect.Top+1, FF.NomFiltre);
      HS := hcColsTitres.Sections.Items[3];
      QR := Rect(HS.Left, ARect.Top + mg, HS.Right - 8, ARect.Bottom - mg);

      DessineFiletColonne(HS.Left - Q4);
      canvas.TextRect(QR, HS.Left + 4, ARect.Top+1, FF.Description);
      HS := hcColsTitres.Sections.Items[4];
      DessineFiletColonne(HS.Left - Q4);
      canvas.TextOut(HS.Left + 4, ARect.Top+1, FF.Expression);
    end;
  end;
begin
  FF := FDocTopo.GetFiltrePerso(Index);
  try
    if (odSelected in State) then DessineItem(clBlue , clWhite)
                             else DessineItem(clwhite, clBlack);
  except
    AfficherMessageErreur('Warn: Liste filtres vide');
  end;

end;

procedure TdlgEditFiltres.lsbFiltresPersoSelectionChange(Sender: TObject; User: boolean);
var
  FF: TFiltrePersonnalise;
begin
  if (0 = FDocTopo.GetNbFiltresPersos()) then Exit;
  FF := FDocTopo.GetFiltrePerso(lsbFiltresPerso.ItemIndex);
  PutFiltreInForm(FF);
end;

function TdlgEditFiltres.GetFiltreFromForm(): TFiltrePersonnalise;
begin
  Result.NomFiltre     := Trim(editFilterName.text);
  Result.Description   := Trim(editFilterDescription.Text);
  Result.Expression    := PreparerFiltre();
  Result.CouleurFiltre := btnFilterColor.ButtonColor;
end;

procedure TdlgEditFiltres.PutFiltreInForm(const FF: TFiltrePersonnalise);
var
  i: Integer;
  LS: TGHStringArray;
  EWE, WU: String;
begin
  editFilterName.Text        := FF.NomFiltre;
  editFilterDescription.Text := FF.Description;

  btnFilterColor.ButtonColor := FF.CouleurFiltre;
  // découpage des filtres multiples
  editFilterExpression.Lines.Clear;
  EWE := Trim(FF.Expression);
  i := 0;
  repeat
    WU := ExtractFirstElementOfStr(EWE, ';');
    editFilterExpression.Lines.Add(UpperCase(WU));
    Inc(i);
  until (pos(';', EWE) = 0) or (i > 500); // sécurité anti-boucle infinie
end;


{ TdlgEditFiltres }
procedure TdlgEditFiltres.ListerLesFiltres(const Index: integer);
var
  i, Nb, WU: Integer;
  FF: TFiltrePersonnalise;
begin
  lsbFiltresPerso.Clear;
  pnlHelperForFiltre.Visible := false;

  Nb := FDocTopo.GetNbFiltresPersos();
  lbNbFiltresPerso.Caption := Format('%d filtres', [Nb]);
  if (0 = Nb) then Exit;
  for i := 0 to Nb - 1 do
  begin
    FF := FDocTopo.GetFiltrePerso(i);
    lsbFiltresPerso.Items.Add(FF.NomFiltre);
  end;
  WU := IIF(-1 = Index, lsbFiltresPerso.Count - 1, Index);
  lsbFiltresPerso.ItemIndex := WU;
end;

function TdlgEditFiltres.PreparerFiltre(): string;
var
  i, nb: Integer;
  EWE: String;
begin
  Result := '';
  nb := editFilterExpression.Lines.Count;
  if (nb = 0) then exit;
  for i := 0 to nb-1 do
  begin
    EWE := UpperCase(Trim(editFilterExpression.Lines[i]));
    Result := Result + EWE + ';';
  end;
end;
function TdlgEditFiltres.Initialiser(const FD: TToporobotStructure2012; const F: string; const DoUseFiltre: boolean): boolean;
var
  i: Integer;
begin
  FDocTopo := FD;
  FEditorExprCurrentLine := 0;
  FTemporaryFilter.NomFiltre       := 'Nouveau filtre';
  FTemporaryFilter.CouleurFiltre   := clAqua;
  FTemporaryFilter.Description     := 'Utilise le filtre issu des visualisateurs';
  FTemporaryFilter.Expression      := F;
  // combobox du helper de filtres
  cmbFiltres.clear;
  for i := low(ARRAY_OF_FILTERS) to high(ARRAY_OF_FILTERS) do cmbFiltres.Items.Add(ARRAY_OF_FILTERS[i]);
  cmbFiltres.ItemIndex := 0;


  ListerLesFiltres(-1);
end;

function TdlgEditFiltres.GetFiltre(): string;
begin
  Result := PreparerFiltre();
end;

end.

