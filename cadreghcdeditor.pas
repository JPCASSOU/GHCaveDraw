unit CadreGHCDEditor;
// 30/08/2020: Pointage temporel

{$INCLUDE CompilationParameters.inc}

interface

uses
  GHCD_Types,
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GeneralFunctions,
  Classes, SysUtils, Forms, Controls, LCLType, ActnList, ExtCtrls, Buttons,
  SynEdit,
  SynHighlighterAny,
  SynCompletion;

type

  { TCdrGHCDTextEditor }

  TCdrGHCDTextEditor = class(TFrame)
    acCopy: TAction;
    acnLstTextEditor: TActionList;
    acOpen: TAction;
    acPaste: TAction;
    acSave: TAction;
    imgLstTextEditor: TImageList;
    Panel1: TPanel;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SynEdit1: TSynEdit;
    SynKwdsGHCaveDraw: TSynAnySyn;
    procedure acCopyExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
  private
    FAutoCompletion         : TSynCompletion;
    function InitColorationSyntaxique(const KC: boolean): boolean;

  public
    function  Initialiser(const KC: boolean): boolean;
    procedure Finaliser();
    procedure SetLines(const LS: TStringList);
    procedure LoadFromFile(const F: TStringDirectoryFileName);
    procedure SaveToFile(const F: TStringDirectoryFileName);
    function  GetNbLines(): integer;
    function  GetALine(const Idx: integer): string;

  end;

implementation

{$R *.lfm}

{ TCdrGHCDTextEditor }

function TCdrGHCDTextEditor.Initialiser(const KC: boolean): boolean;
  procedure S666(const AC: TAction; const ACCaption: string);
  begin
    AC.Hint    := ACCaption;
    AC.Caption := ACCaption;
  end;
begin
  result := false;
  S666(acOpen, 'Ouvrir');
  S666(acSave, 'Sauvegarder sous');
  S666(acCopy, 'Copier');
  S666(acPaste,'Coller');
  //try
    FAutoCompletion         := TSynCompletion.Create(self);
    FAutoCompletion.Editor  := SynEdit1;
    FAutoCompletion.ShortCut:= VK_F2;
    InitColorationSyntaxique(KC);
    SynEdit1.Clear;
    result := True;
  //except
  //end;
end;

procedure TCdrGHCDTextEditor.Finaliser();
begin
  try

  finally
    //FreeAndNil(FAutoCompletion);
  end;
end;


procedure TCdrGHCDTextEditor.SetLines(const LS: TStringList);
var
  i, n: Integer;
begin
  SynEdit1.Clear;
  n := LS.Count;
  if (n = 0) then exit;
  for i := 0 to n - 1 do SynEdit1.Lines.Add(LS[i]);
end;

procedure TCdrGHCDTextEditor.LoadFromFile(const F: TStringDirectoryFileName);
begin
  SynEdit1.Lines.LoadFromFile(F);
end;

procedure TCdrGHCDTextEditor.SaveToFile(const F: TStringDirectoryFileName);
begin
  SynEdit1.Lines.SaveToFile(F);
end;

function TCdrGHCDTextEditor.GetNbLines(): integer;
begin
  Result := SynEdit1.Lines.Count;
end;

function TCdrGHCDTextEditor.GetALine(const Idx: integer): string;
begin
  Result := SynEdit1.Lines[Idx];
end;


procedure TCdrGHCDTextEditor.acOpenExecute(Sender: TObject);
begin
  pass;
end;

procedure TCdrGHCDTextEditor.acPasteExecute(Sender: TObject);
begin
  pass;
end;

procedure TCdrGHCDTextEditor.acCopyExecute(Sender: TObject);
begin
  pass;
end;

procedure TCdrGHCDTextEditor.acSaveExecute(Sender: TObject);
begin
  pass;
end;

function TCdrGHCDTextEditor.InitColorationSyntaxique(const KC: boolean): boolean;
var
  n, i: Integer;
  procedure F666(const WU: string);
  begin
    SynKwdsGHCaveDraw.KeyWords.Add(uppercase(WU)); // keywords must be uppercased
  end;
begin
  SynKwdsGHCaveDraw.KeyWords.Clear;
  if (KC) then
  begin
    F666(CROSS_PROFILES_SECTION);
      F666(CROSS_SECTION);
        F666(CROSS_SECTION_SCRAP_ENGLOBANT);
          F666(CROSS_SECTION_VERTEX_SCRAP_ENGLOBANT);
        F666(END_CROSS_SECTION_SCRAP_ENGLOBANT);
        F666(CROSS_SECTION_COURBES);
          F666(CROSS_SECTION_A_COURBE);
            F666(CROSS_SECTION_ARC_COURBE);
          F666(END_CROSS_SECTION_A_COURBE);
        F666(END_CROSS_SECTION_COURBES);
        F666(CROSS_SECTION_POLYGONES);
          F666(CROSS_SECTION_A_POLYGONE);
            F666(CROSS_SECTION_POLY_VERTEX);
          F666(END_CROSS_SECTION_A_POLYGONE);
        F666(END_CROSS_SECTION_POLYGONES);
        F666(CROSS_SECTION_LINES);
          F666(CROSS_SECTION_SIMPLELINE);
        F666(END_CROSS_SECTION_LINES);
        F666(CROSS_SECTION_TEXTES);
          F666(CROSS_SECTION_SIMPLETEXT);
        F666(END_CROSS_SECTION_TEXTES);
      F666(END_CROSS_SECTION);
    F666(END_CROSS_PROFILES_SECTION);
  end else
  begin
    // mots clés pour les styles d'objets
    F666(STYLESSECTION);
      F666(STYLECURVES);
        F666(STYLECURVE);
      F666(ENDSTYLECURVES);
      F666(STYLELINES);
        F666(STYLELINE);
      F666(ENDSTYLELINES);
      F666(STYLEFILLS);
        F666(STYLEFILL);
      F666(ENDSTYLEFILLS);
      F666(STYLEPOLYGONS);
        F666(STYLEPOLYGON);
      F666(ENDSTYLEPOLYGONS);
      F666(STYLETEXTS);
        F666(STYLETEXT);
      F666(ENDSTYLETEXTS);
      F666(STYLEPONCTOBJs);
        F666(STYLEPONCTOBJ);
      F666(ENDSTYLEPONCTOBJS);

    F666(ENDSTYLESSECTION);
    // mots clés pour points de base
    F666(BASEPOINTMARKER);
    F666(ENDBASEPOINTMARKER);
    // supergroupes
    F666(SUPERGROUPESSECTION);
      F666(SUPERGROUPE_ITEM);
    F666(ENDSUPERGROUPESSECTION);
    // groupes
    F666(GROUPESSECTION);
      F666(GROUPE_ITEM);
    F666(ENDGROUPESSECTION);
    // images
    F666(IMAGESSECTION);
      F666(IMAGE);
      F666(ENDIMAGE);
    F666(ENDIMAGESSECTION);
    // scraps
    F666(SCRAPSSECTION);
      F666(SCRAP);
        F666(VERTEX);
      F666(ENDSCRAP);
    F666(ENDSCRAPSSECTION);
    // courbes
    F666(CURVESSECTION);
      F666(ENTITYCURVE);
        F666(CURVE_ARC);
      F666(ENDENTITYCURVE);
    F666(ENDCURVESSECTION);
    // lignes simples
    F666(LINESECTION);
    F666(ENDLINESECTION);
    // polylignes
    F666(POLYLINESSECTION);
      F666(ENTITYPOLYLINE);
      F666(ENDENTITYPOLYLINE);
    F666(ENDPOLYLINESSECTION);
    // polygones
    F666(POLYGONSECTION);
      F666(ENTITYPOLYGON);
      F666(ENDENTITYPOLYGON);
    F666(ENDPOLYGONSECTION);
    // textes
    F666(TEXTSECTION);
      F666(TEXT_ITEM);
    F666(ENDTEXTSECTION);
    // symboles
    F666(PONCTOBJSECTION);
      F666(PONCT_OBECT_ITEM);
    F666(ENDPONCTOBJSECTION);
  end;
  SynKwdsGHCaveDraw.Markup  := false;
  SynKwdsGHCaveDraw.Enabled := True;

  // autocompletion
  n := SynKwdsGHCaveDraw.KeyWords.count;
  FAutoCompletion.ItemList.Clear;
  for i := 0 to n - 1 do FAutoCompletion.ItemList.Add(LowerCase(SynKwdsGHCaveDraw.KeyWords.Strings[i]));
  // contrôle
  (*
  n := FAutoCompletion.ItemList.Count;
  AfficherMessageErreur(Format('%d keywords', [n]));
  AfficherMessageErreur(StringOfChar('=', 132));
  for i := 0 to n - 1 do AfficherMessageErreur(FAutoCompletion.ItemList[i]);
  AfficherMessageErreur(StringOfChar('=', 132));
  //*)
end;

end.

