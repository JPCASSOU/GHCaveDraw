unit dlgHelpSystem;
// Système d'aide. Fonctionnalités de modification non implémentées (peu utilisées).
// 12/12/2013: Regroupement des fenêtres "A propos" et "Système d'aide"
// 14/04/2014: Ajout du QRCode
{$INCLUDE CompilationParameters.inc}

interface

uses
  Classes, SysUtils, FileUtil,
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    windows,
  {$ENDIF}
  GeneralFunctions
  , UnitDocDessin
  , GHCD_Types
  , dateutils
  , winpeimagereader {need this for reading exe info}
  , elfreader {needed for reading ELF executables}
  , machoreader {needed for reading MACH-O executables}
  , UnitKMLExport
  , UnitLeafletExport
  ,CadreEditeurTexte
  , BGRABitmap, BGRABitmapTypes, BGRAGradients, curredit, SynHighlighterPas,
  SynEdit, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ComCtrls,
  ExtCtrls, ubarcodes, types;
const
  URL_GETTING_GHTOPO = 'ghtopo.blog4ever.com';
type
  { TfrmHelpSystem }
  TfrmHelpSystem = class(TForm)
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    Button7: TButton;
    CdrTextEditor1: TCdrTextEditor;
    CdrTextEditor2: TCdrTextEditor;
    cmbResampleFilter: TComboBox;
    cmbPerlinFunc: TComboBox;
    CurrencyEdit1: TCurrencyEdit;
    editNatureTexture: TEdit;
    editSizeTextureX: TCurrencyEdit;
    editTileOverlap: TCurrencyEdit;
    editColorG: TCurrencyEdit;
    editColorR: TCurrencyEdit;
    editColorA: TCurrencyEdit;
    editHorizontalPeriod: TCurrencyEdit;
    editVerticalPeriod: TCurrencyEdit;
    editExponent: TCurrencyEdit;
    editLightColorB: TCurrencyEdit;
    editLightColorG: TCurrencyEdit;
    editLightColorR: TCurrencyEdit;
    editLightColorA: TCurrencyEdit;
    editColorB: TCurrencyEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    editChaineASplitter: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbCodeEPSG: TLabel;
    lbURLGHTopo: TEdit;
    lbTypeInterfaceUI: TLabel;
    lsbChaineDecomposee: TListBox;
    memoNotesVersion: TMemo;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    QRCode: TBarcodeQR;
    BitBtn1: TBitBtn;
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    lbAuthor: TLabel;
    lbGHCaveDrawName: TLabel;
    lbLicence: TLabel;
    lbPlateforme: TLabel;
    lbVersion: TLabel;
    lsbResolutionEcrans: TListBox;
    PageControl1: TPageControl;
    sclHorizontalPeriod: TScrollBar;
    sclColorA: TScrollBar;
    sclColorB: TScrollBar;
    sclColorG: TScrollBar;
    sclColorR: TScrollBar;
    sclVerticalPeriod: TScrollBar;
    sclExponent: TScrollBar;
    sclLightColorB: TScrollBar;
    sclLightColorG: TScrollBar;
    sclLightColorR: TScrollBar;
    sclLightColorA: TScrollBar;
    tabShtTextures  : TTabSheet;
    tabshtStats     : TTabSheet;
    tabshtSandBox   : TTabSheet;
    tabShtAbout     : TTabSheet;
    procedure Button4Click(Sender: TObject);

    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure cmbPerlinFuncChange(Sender: TObject);
    procedure cmbResampleFilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Label19Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure sclColorAChange(Sender: TObject);
    procedure sclColorBChange(Sender: TObject);
    procedure sclColorGChange(Sender: TObject);
    procedure sclColorRChange(Sender: TObject);
    procedure sclExponentChange(Sender: TObject);
    procedure sclHorizontalPeriodChange(Sender: TObject);
    procedure sclLightColorAChange(Sender: TObject);
    procedure sclLightColorBChange(Sender: TObject);
    procedure sclLightColorGChange(Sender: TObject);
    procedure sclLightColorRChange(Sender: TObject);
    procedure sclVerticalPeriodChange(Sender: TObject);
  private
    FDocuDessin: TDocumentDessin;
    FMaTextureDeTest: TBGRABitmap;
    { private declarations }
    function CreateAnyTexture(tx, ty: integer; const DoEcho: boolean): TBGRABitmap;
    function CreateCustomTexture(AWidth, AHeight: integer; HorizontalPeriod: Single=1; VerticalPeriod: Single=1; Exponent: Double=1; ResampleFilter: TResampleFilter=rfCosine): TBGRABitmap;

    procedure ShowAPropos;
    procedure AfficherStatsDessin;
  public
    { public declarations }
    function Initialiser(const DC: TDocumentDessin; const HelpFile: string; const QTopics: string; const ReadOnly: boolean ): boolean;
  end;

var
  frmHelpSystem: TfrmHelpSystem;

implementation
{$R *.lfm}
function TfrmHelpSystem.CreateCustomTexture(AWidth, AHeight: integer;
                                            HorizontalPeriod: Single = 1;
                                            VerticalPeriod: Single = 1;
                                            Exponent: Double = 1;
                                            ResampleFilter: TResampleFilter = rfCosine): TBGRABitmap;
 var
   colorOscillation: integer;
   p: PBGRAPixel;
   i: Integer;
   S1, S2: TBGRAPixel;
 begin
   result := CreateCyclicPerlinNoiseMap(AWidth,AHeight,
                                        HorizontalPeriod,
                                        VerticalPeriod,
                                        Exponent,
                                        ResampleFilter);


   S1 := BGRA(sclLightColorB.Position,
              sclLightColorG.Position,
              sclLightColorR.Position,
              sclLightColorA.Position);
   S2 := BGRA(sclColorB.Position,
              sclColorG.Position,
              sclColorR.Position,
              sclColorA.Position);

   p := result.Data;
   for i := 0 to result.NbPixels-1 do
   begin
     colorOscillation := round(((sin(p^.red*Pi/32)+1)/2)*256);
     p^ := Interp256(S1, S2, colorOscillation);
     inc(p);
   end;
 end;
function TfrmHelpSystem.CreateAnyTexture(tx, ty: integer; const DoEcho: boolean): TBGRABitmap;
const
  blurSize     = 5;
var
  WU           : TRect;
  temp         : TBGRABitmap;
  phong        : TPhongShading;
  QTileOverlap : LongInt;
begin
   QTileOverlap := editTileOverlap.AsInteger;
   case cmbPerlinFunc.ItemIndex of
     0: result := CreateCyclicPerlinNoiseMap(tx, ty,
                                        sclHorizontalPeriod.Position / 100.0,
                                        sclVerticalPeriod.Position / 100.0,
                                        sclExponent.Position / 100.0,
                                        TResampleFilter(cmbResampleFilter.ItemIndex)
                                        );
     1: result := CreatePerlinNoiseMap(tx, ty,
                                        sclHorizontalPeriod.Position / 100.0,
                                        sclVerticalPeriod.Position / 100.0,
                                        sclExponent.Position / 100.0,
                                        TResampleFilter(cmbResampleFilter.ItemIndex)
                                        );
     2: result := CreateCustomTexture(tx, ty,sclHorizontalPeriod.Position / 100.0,
                                        sclVerticalPeriod.Position / 100.0,
                                        sclExponent.Position / 100.0,
                                        TResampleFilter(cmbResampleFilter.ItemIndex)
                                        );
   end;

   // GetPart crée également l'objet Temp
   if (cmbPerlinFunc.ItemIndex in [0, 1]) then
   begin
     if (DoEcho) then
     begin
       editHorizontalPeriod.Value := sclHorizontalPeriod.Position / 100.0;
       editVerticalPeriod.Value   := sclVerticalPeriod.Position / 100.0;
       editExponent.Value         := sclExponent.Position / 100.0;

       editLightColorB.AsInteger := sclLightColorB.Position;
       editLightColorG.AsInteger := sclLightColorG.Position;
       editLightColorR.AsInteger := sclLightColorR.Position;
       editLightColorA.AsInteger := sclLightColorA.Position;
       editColorB.AsInteger      := sclColorB.Position;
       editColorG.AsInteger      := sclColorG.Position;
       editColorR.AsInteger      := sclColorR.Position;
       editColorA.AsInteger      := sclColorA.Position;
     end;
     WU := MakeTRect(-QTileOverlap, -QTileOverlap,
                      tx + QTileOverlap, ty + QTileOverlap);
     temp:= result.GetPart(WU) as TBGRABitmap;
     BGRAReplace(temp,temp.FilterBlurRadial(blurSize,rbFast));
     try
       phong := TPhongShading.Create;
       phong.LightSourceDistanceFactor := 0;
       phong.LightDestFactor           := 0;
       phong.LightSourceIntensity      := 150;
       phong.LightPositionZ            := 80;
       phong.LightColor                := BGRA(sclLightColorB.Position,
                                               sclLightColorG.Position,
                                               sclLightColorR.Position,
                                               sclLightColorA.Position);
       phong.NegativeDiffusionFactor   := 0.3;
       phong.SpecularIndex             := 20;
       phong.AmbientFactor             := 0.4;
       phong.Draw(result,temp,20,-blurSize,-blurSize,
                           BGRA(sclColorB.Position,
                                sclColorG.Position,
                                sclColorR.Position,
                                sclColorA.Position));




     finally
       FreeAndNil(phong);
       FreeAndNil(temp);
     end;

   end; //  if (cmbPerlinFunc.ItemIndex in [0, 1]) then
end;


{ TfrmHelpSystem }
procedure TfrmHelpSystem.Button4Click(Sender: TObject);
var
  EWE: TGHStringArray;
  i: Integer;
begin
  lsbChaineDecomposee.Items.Add('Guillemets conserves');
  EWE := DecoupeChaineEspacesGuillemets(editChaineASplitter.Text, false);
  lsbChaineDecomposee.Clear;
  for i := 0 to High(EWE) do lsbChaineDecomposee.Items.Add(EWE[i]);
  lsbChaineDecomposee.Items.Add('Guillemets supprimes');
  EWE := DecoupeChaineEspacesGuillemets(editChaineASplitter.Text, true);
  for i := 0 to High(EWE) do lsbChaineDecomposee.Items.Add(EWE[i]);
end;


procedure TfrmHelpSystem.Button6Click(Sender: TObject);
  procedure MiouMiou(const Miou: string);
  begin
    CdrTextEditor1.AddLine(Miou);
  end;
var
  EWE: String;
begin
  {
    function CreateTextureSand(): TBGRABitmap;
    const
      blurSize     = 5;
      tile_overlap = 4;
    var
      QTileOverlap : LongInt;
      i: integer
      S1, S2: TBGRAPixel;
      colorOscillation: integer;
      p: PBGRAPixel;
    begin
      QTileOverlap := 4;
      result := CreateCustomTexture(128, 128, 4, 1.33, 1.42, 0.60000, rfCosine);
      // GetPart crée également l'objet Temp
      // JESUS = HITLER -- FUCK THE CHRIST
      // Marble or wooden like texture
      result := CreateCyclicPerlinNoiseMap(128, 128, 4, 1.33, 1.42, 0.60000, rfCosine);
      S1 := BGRA(105, 233, 240, 255);
      S2 := BGRA(28, 139, 166, 255);
      p := result.Data;
      for i := 0 to result.NbPixels-1 do
      begin
        colorOscillation := round(((sin(p^.red*Pi/32)+1)/2)*256);
        p^ := Interp256(S1, S2, colorOscillation);
        inc(p);
      end;
    end;
  //}

  CdrTextEditor1.ClearText();
  MiouMiou(Format('function CreateTexture%s(): TBGRABitmap;', [trim(editNatureTexture.Text)]));
  MiouMiou('const');
  MiouMiou(Format('  blurSize     = %d;', [5]));
  MiouMiou(Format('  tile_overlap = %d;', [4]));
  MiouMiou('var');
  MiouMiou('  QTileOverlap : LongInt;');
  if (cmbPerlinFunc.ItemIndex in [0, 1]) then
  begin
    MiouMiou('  WU      : TRect;');
    MiouMiou('  temp    : TBGRABitmap;');
    MiouMiou('  phong   : TPhongShading;');
  end;
  if (2 = cmbPerlinFunc.ItemIndex) then
  begin
    MiouMiou('  i: integer');
    MiouMiou('  S1, S2: TBGRAPixel;');
    MiouMiou('  colorOscillation: integer;');
    MiouMiou('  p: PBGRAPixel;');
  end;

  MiouMiou('begin');
    MiouMiou(Format('  QTileOverlap := %d;', [editTileOverlap.AsInteger]));
    case cmbPerlinFunc.ItemIndex of
      0: EWE := 'CreateCyclicPerlinNoiseMap';
      1: EWE := 'CreatePerlinNoiseMap';
      2: EWE := 'CreateCustomTexture';
    end;

    MiouMiou('  // GetPart crée également l''objet Temp   ');
    case (cmbPerlinFunc.ItemIndex) of
      0, 1:
      begin
        MiouMiou(Format('(*001*)  result := %s(%d, %d, %d, %.2f, %.2f, %.5f, %s);',
                    [EWE,
                     editSizeTextureX.AsInteger,
                     editSizeTextureX.AsInteger,
                     editTileOverlap.AsInteger,
                     sclHorizontalPeriod.Position / 100.0,
                     sclVerticalPeriod.Position / 100.0,
                     sclExponent.Position / 100.0,
                     cmbResampleFilter.Items[cmbResampleFilter.ItemIndex]
                     ]));
        MiouMiou('  // Bumped procedural texture');
        MiouMiou('  WU := MakeTRect(-tile_overlap,-tile_overlap,tx+tile_overlap,ty+tile_overlap);');
        MiouMiou('  temp:= result.GetPart(WU) as TBGRABitmap;');


        MiouMiou('  BGRAReplace(temp,temp.FilterBlurRadial(blurSize,rbFast));');
        MiouMiou('  try');
        MiouMiou('    phong := TPhongShading.Create;');
        MiouMiou(Format('    phong.LightSourceDistanceFactor := %d;', [0]));
        MiouMiou(Format('    phong.LightDestFactor           := %d;', [0]));
        MiouMiou(Format('    phong.LightSourceIntensity      := %d;', [150]));
        MiouMiou(Format('    phong.LightPositionZ            := %d;', [80]));
        MiouMiou(Format('    phong.LightColor := BGRA(%d, %d, %d, %d);', [sclLightColorB.Position, sclLightColorG.Position, sclLightColorR.Position, sclLightColorA.Position]));
        MiouMiou('');
        MiouMiou(Format('    phong.NegativeDiffusionFactor   := %.2f; ', [0.3]));
        MiouMiou(Format('    phong.SpecularIndex             := %d;'  , [20]));
        MiouMiou(Format('    phong.AmbientFactor            := %.3f;', [0.4]));
        MiouMiou(Format('    phong.Draw(Result, Temp, %d, -blurSize,-blurSize, BGRA(%d, %d, %d, %d));', [20, sclColorB.Position, sclColorG.Position, sclColorR.Position,  sclColorA.Position]));
        MiouMiou('  finally');
        MiouMiou('    FreeAndNil(phong);');
        MiouMiou('    FreeAndNil(temp);');
        MiouMiou('  end;');

      end;
      2:
      begin
        MiouMiou('  // Marble or wooden like texture');

        MiouMiou(Format('  result := %s(%d, %d, %d, %.2f, %.2f, %.5f, %s);',
                    [EWE,
                     editSizeTextureX.AsInteger,
                     editSizeTextureX.AsInteger,
                     editTileOverlap.AsInteger,
                     sclHorizontalPeriod.Position / 100.0,
                     sclVerticalPeriod.Position / 100.0,
                     sclExponent.Position / 100.0,
                     cmbResampleFilter.Items[cmbResampleFilter.ItemIndex]
                     ]));
                     //*)
       MiouMiou(Format('  S1 := BGRA(%d, %d, %d, %d);', [sclLightColorB.Position,
                                                         sclLightColorG.Position,
                                                         sclLightColorR.Position,
                                                         sclLightColorA.Position
                                                        ]));
       MiouMiou(Format('  S2 := BGRA(%d, %d, %d, %d);', [
                                                         sclColorB.Position,
                                                         sclColorG.Position,
                                                         sclColorR.Position,
                                                         sclColorA.Position
                                                        ]));
       MiouMiou('  p := result.Data;');
       MiouMiou('  for i := 0 to result.NbPixels-1 do');
       MiouMiou('  begin');
       MiouMiou('    colorOscillation := round(((sin(p^.red*Pi/32)+1)/2)*256);');
       MiouMiou('    p^ := Interp256(S1, S2, colorOscillation);');
       MiouMiou('    inc(p);');
       MiouMiou('  end;');
      end;
    end;
  MiouMiou('end;');




  try
    FMaTextureDeTest.SaveToFile(GetGHCaveDrawDirectory() + 'MaTexture.png');

  except
    ShowMessage('Echec création du PNG de la texture');
  end;



(*

  function TfrmHelpSystem.CreateAnyTexture(tx, ty: integer; const DoEcho: boolean): TBGRABitmap;
const
  blurSize     = 5;
  tile_overlap = 4;
var
  WU: TRect;

  ;
begin
   result := CreateCyclicPerlinNoiseMap(tx, ty,
                                        ,
                                        ,
                                        ,
                                        TResampleFilter(cmbResampleFilter.ItemIndex)
                                        );

   // GetPart crée également l'objet Temp
   WU := MakeTRect(-tile_overlap,-tile_overlap,tx+tile_overlap,ty+tile_overlap);
   temp:= result.GetPart(WU) as TBGRABitmap;
   BGRAReplace(temp,temp.FilterBlurRadial(blurSize,rbFast));
   try
     phong := TPhongShading.Create;
     phong.LightSourceDistanceFactor := 0;
     phong.LightDestFactor           := 0;
     phong.LightSourceIntensity      := 150;
     phong.LightPositionZ            := 80;
     phong.LightColor                := BGRA(sclLightColorB.Position,
                                             sclLightColorG.Position,
                                             sclLightColorR.Position,
                                             sclLightColorA.Position);
     phong.NegativeDiffusionFactor   := 0.3;
     phong.SpecularIndex             := 20;
     phong.AmbientFactor             := 0.4;
     phong.Draw(result,temp,20,-blurSize,-blurSize,
                         BGRA(sclColorB.Position,
                              sclColorG.Position,
                              sclColorR.Position,
                              sclColorA.Position));




   finally
     FreeAndNil(phong);
     FreeAndNil(temp);
   end;
end;

  //*)
end;

procedure TfrmHelpSystem.Button7Click(Sender: TObject);
const
  FOLDER_ENTRANCES   = 'Entrances';
  FOLDER_SCRAPS      = 'Scraps';
  FOLDER_CENTERLINES = 'Centerlines';
  STYLE_SCRAP      = 'StyleScrap0';
  STYLE_CENTERLINE = 'StyleCenterline0';
var
  MyKML : TKMLExport;
begin
  MyKML := TKMLExport.Create;
  try
    if (MyKML.Initialiser(GetGHCaveDrawDirectory() + 'TestTKMLExport.KML')) then
    begin
      MyKML.WriteCommentaire('Styles');
      MyKML.DefineStylePoly(STYLE_SCRAP     , 1.0, clBlue, clRed, 192, 128);
      MyKML.DefineStylePoly(STYLE_CENTERLINE, 0.5, clRed, clRed, 192, 128);

      MyKML.WriteCommentaire('Entités');
      MyKML.BeginFolder(4, FOLDER_ENTRANCES);
        MyKML.AddPoint(43.08, -0.07, 551, 'Toto', '<B>LLANFAIRPWLLGWYNGYLL</B><BR>titi');
        MyKML.AddPoint(43.01, -0.06, 666, 'Mimi', '<B>OMEX</B><BR>miaou');
      MyKML.EndFolder(4, FOLDER_ENTRANCES);
      MyKML.BeginFolder(4, FOLDER_SCRAPS);
        MyKML.BeginPolygon('Scrap0', STYLE_SCRAP);
          MyKML.AddVertex(43.055, -0.045, 1200);
          MyKML.AddVertex(43.042, -0.041, 1200);
          MyKML.AddVertex(43.085, -0.049, 1200);
          MyKML.AddVertex(43.095, -0.065, 1200);
        MyKML.EndPolygon();
      MyKML.EndFolder(4, FOLDER_SCRAPS);
      MyKML.BeginFolder(4, FOLDER_CENTERLINES);
        MyKML.BeginPolyline('Centerline0', STYLE_CENTERLINE);
          MyKML.AddVertex(43.0, -0.05, 1200);
          MyKML.AddVertex(43.042, -0.056, 1200);
          MyKML.AddVertex(43.025, -0.059, 1200);
          MyKML.AddVertex(43.033, -0.063, 1200);
        MyKML.EndPolyline();
      MyKML.EndFolder(4, FOLDER_CENTERLINES);
      MyKML.Finaliser();
    end;
  finally
    FreeAndNil(MyKML);
  end;
end;

procedure TfrmHelpSystem.cmbPerlinFuncChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;

procedure TfrmHelpSystem.cmbResampleFilterChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;

procedure TfrmHelpSystem.FormCreate(Sender: TObject);
begin

end;


procedure TfrmHelpSystem.FormShow(Sender: TObject);
begin
  self.Caption                  := GetResourceString(rsHLP_TITRE);
  tabShtAbout.Caption           := GetResourceString(rsHLP_TAB_ABOUT);
  tabshtStats.Caption           := GetResourceString(rsHLP_TAB_STATS);
  PageControl1.TabIndex         := 0;
  ShowAPropos;
  AfficherStatsDessin;
end;

procedure TfrmHelpSystem.Image1Click(Sender: TObject);
begin

end;

procedure TfrmHelpSystem.Label19Click(Sender: TObject);
begin

end;

procedure TfrmHelpSystem.PageControl1Change(Sender: TObject);
begin

end;

procedure TfrmHelpSystem.PaintBox1Paint(Sender: TObject);
const MG = 20;
var
  QTmpBuffer : TBGRABitmap;
begin
  try
    QTmpBuffer:= TBGRABitmap.Create(PaintBox1.Width, PaintBox1.Height, BGRA(192, 192, 192, 255));

    QTmpBuffer.CanvasBGRA.Pen.Color := clBlue;
    QTmpBuffer.CanvasBGRA.Pen.Width := 2;
    QTmpBuffer.CanvasBGRA.Brush.Texture := FMaTextureDeTest;

    QTmpBuffer.CanvasBGRA.Rectangle(MG,MG, PaintBox1.Width - MG, PaintBox1.Height - MG);





    QTmpBuffer.Draw(PaintBox1.Canvas, 0, 0, True);

  finally
    FreeAndNil(QTmpBuffer);//TmpBuffer.Free;
    // ne pas mettre cette ligne avant la libération du sémaphore
    Application.ProcessMessages;
  end;
end;

procedure TfrmHelpSystem.Panel3Click(Sender: TObject);
begin

end;

procedure TfrmHelpSystem.sclColorAChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;

procedure TfrmHelpSystem.sclColorBChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;

procedure TfrmHelpSystem.sclColorGChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;

procedure TfrmHelpSystem.sclColorRChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;



procedure TfrmHelpSystem.sclExponentChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;

procedure TfrmHelpSystem.sclHorizontalPeriodChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;

procedure TfrmHelpSystem.sclLightColorAChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;

end;

procedure TfrmHelpSystem.sclLightColorBChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;

procedure TfrmHelpSystem.sclLightColorGChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;



procedure TfrmHelpSystem.sclLightColorRChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;

procedure TfrmHelpSystem.sclVerticalPeriodChange(Sender: TObject);
begin
  FMaTextureDeTest := CreateAnyTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger, True);
  PaintBox1.Invalidate;
end;

procedure TfrmHelpSystem.ShowAPropos;
const
  FMT_ECRANS_RESOL = 'Monitor %d: %dx%d - %s';
var
  DateCompilation: TDateTime;
  i, W, H: Integer;
  M: TMonitor;
  {$IFDEF MSWINDOWS}
  OSVersion: TOSVersionInfo;
  {$ENDIF}
  YYYY, MM, DD, HH, MN, SS, MS: word;
  WU: String;
  //FileVerInfo: TFileVersionInfo;
begin
  WU                   := Format('Get it: %s', [URL_GETTING_GHTOPO]);
  DateCompilation      := FileDateToDateTime(FileAge(ParamStr(0)));
  DecodeDateTime(DateCompilation, YYYY, MM, DD, HH, MN, SS, MS);
  lbVersion.Caption    := GetGHCaveDrawVersionAndDateBuild();
  lbAuthor.Caption     := Format(GetResourceString(rsGHCD_AUTHOR), [YYYY]);
  lbLicence.Caption    := GetResourceString(rsGHCD_LICENSE);
  lbURLGHTopo.Text     := WU;
  {$IFDEF TABLET_VERSION}
    lbTypeInterfaceUI.Caption := GetResourceString(rsTYPE_INTERFACE_TABLETTE);
  {$ELSE}
    lbTypeInterfaceUI.Caption := GetResourceString(rsTYPE_INTERFACE_DESKTOP);
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    GetVersionExA(OSVersion);
    lbPlateforme.Caption := Format('%s', ['Microsoft Windows']);
  {$ENDIF}
  {$IFDEF LINUX}
    //TODO: Obtenir la version de Linux
    lbPlateforme.Caption := 'Linux';
  {$ENDIF}
  {$IFDEF MACOSX}
    lbPlateforme.Caption := 'MacOSX';
  {$ENDIF}

  lsbResolutionEcrans.Clear;
  for i:= 0 to Screen.MonitorCount - 1 do
  begin
    M := Screen.Monitors[i];
    W := M.Width;
    H := M.Height;
    lsbResolutionEcrans.Items.Add(GetResourceString(Format(FMT_ECRANS_RESOL, [M.MonitorNum,W, H, BoolToStr(M.Primary, 'Primary', 'Auxiliary')])));
  end;
  lsbResolutionEcrans.ItemIndex := 0;
  Edit1.Text := Trim(GetGHCaveDrawDirectory());
  // code QR
  WU := URL_GETTING_GHTOPO;
  QRCode.Text := WU;
  // notes de version
  memoNotesVersion.Lines.Clear;
  memoNotesVersion.Lines.Clear;
  memoNotesVersion.Lines.Add('Target CPU: ' + {$I %FPCTARGETCPU%});
  memoNotesVersion.Lines.Add('Target OS: ' + {$I %FPCTARGETOS%});
  memoNotesVersion.Lines.Add('FPC Compiler: ' + {$I %FPCVERSION%});
  WU := 'Compilation: ' + {$I %DATE%} + ' ' + {$I %TIME%};
  memoNotesVersion.Lines.Add(WU);
end;


procedure TfrmHelpSystem.AfficherStatsDessin;
var
  NbGroupes, NbScraps: Integer;
  NbCourbes, NbPolylignes, NbPolygones: Integer;
  NbSimpleLines: Integer;
  NbSymboles, NbTextes: Integer;
  cnMini, cnMaxi: TPoint3Df;
  i: Integer;
  MyGroupe: TGroupeEntites;
  NbSimplesLignes: Integer;
  NbViseesCheminement: integer;
  NbViseesAntennes: integer;
  SuperficieVides: Double;
begin
  NbViseesCheminement  := 0;
  NbViseesAntennes     := 0;
  CdrTextEditor2.ClearText();
  try
    CdrTextEditor2.AddLine(FDocuDessin.GetDocumentName);
    FDocuDessin.GetNbVisees(NbViseesCheminement, NbViseesAntennes);

    NbGroupes    := FDocuDessin.GetNbGroupes();
    NbScraps     := FDocuDessin.GetNbScraps();
    NbCourbes    := FDocuDessin.GetNbCourbes();
    NbPolylignes := FDocuDessin.GetNbPolylignes();
    NbPolygones  := FDocuDessin.GetNbPolygones();
    NbSimpleLines:= FDocuDessin.GetNbSimpleLignes();
    NbSymboles   := FDocuDessin.GetNbSymboles();
    NbTextes     := FDocuDessin.GetNbTextes();
    CdrTextEditor2.AddLine('');
    cnMini  := FDocuDessin.GetCoordsMini;
    cnMaxi  := FDocuDessin.GetCoordsMaxi;
    CdrTextEditor2.AddLine(Format('%d visées de cheminement', [NbViseesCheminement]));
    CdrTextEditor2.AddLine(Format('%d visées rayonnantes', [NbViseesAntennes]));
    CdrTextEditor2.AddLine('');

    CdrTextEditor2.AddLine(Format('Coords mini: (%.2f, %.2f)', [cnMini.X, cnMini.Y]));
    CdrTextEditor2.AddLine(Format('Coords maxi: (%.2f, %.2f)', [cnMaxi.X, cnMaxi.Y]));
    CdrTextEditor2.AddLine(Format('Etendue    : (%.2f, %.2f)', [cnMaxi.X - cnMini.X, cnMaxi.Y - cnMini.Y]));
    CdrTextEditor2.AddLine('');
    CdrTextEditor2.AddLine(Format('%d groupes'         , [NbGroupes]));
    CdrTextEditor2.AddLine(Format('%d scraps'          , [NbScraps]));
    CdrTextEditor2.AddLine(Format('%d courbes'         , [NbCourbes]));
    CdrTextEditor2.AddLine(Format('%d polylignes'      , [NbPolylignes]));
    CdrTextEditor2.AddLine(Format('%d polygones'       , [NbPolygones]));
    CdrTextEditor2.AddLine(Format('%d lignes simples'  , [NbSimpleLines]));
    CdrTextEditor2.AddLine(Format('%d symboles'        , [NbSymboles]));
    CdrTextEditor2.AddLine(Format('%d textes'          , [NbTextes]));

    // stats groupe par groupe
    CdrTextEditor2.AddLine('');
    CdrTextEditor2.AddLine('Statistiques par groupes');

    CdrTextEditor2.AddLine('No; Idx; Z-Order; Nom; Nb scraps; Nb courbes; Nb polylignes; Nb polygones; Nb lignes; Nb symboles; Nb textes;Superficie vides');

    for i := 0 to NbGroupes - 1 do
    begin
      MyGroupe       := FDocuDessin.GetGroupe(i);
      NbScraps       := FDocuDessin.GetNbScrapsPourUnGroupe(MyGroupe.IDGroupeEntites);
      NbCourbes      := FDocuDessin.GetNbCourbesPourUnGroupe(MyGroupe.IDGroupeEntites);
      NbPolylignes   := FDocuDessin.GetNbPolylignesPourUnGroupe(MyGroupe.IDGroupeEntites);
      NbPolygones    := FDocuDessin.GetNbPolygonesPourUnGroupe(MyGroupe.IDGroupeEntites);
      NbSimplesLignes:= FDocuDessin.GetNbSimplesLignesPourUnGroupe(MyGroupe.IDGroupeEntites);
      NbSymboles     := FDocuDessin.GetNbSymbolesPourUnGroupe(MyGroupe.IDGroupeEntites);
      NbTextes       := FDocuDessin.GetNbTextesPourUnGroupe(MyGroupe.IDGroupeEntites);
      // calcul de la superficie
      SuperficieVides := FDocuDessin.CalcSuperficieVides(MyGroupe);
      CdrTextEditor2.AddLine(Format('%d; %d; %.2f; %s; %d; %d; %d; %d; %d; %d; %d; %.0f', [
                      i,
                      MyGroupe.IDGroupeEntites,
                      MyGroupe.ZOrder,
                      AnsiToUtf8(MyGroupe.NomGroupe),
                      NbScraps,
                      NbCourbes,
                      NbPolylignes,
                      NbPolygones,
                      NbSimplesLignes,
                      NbSymboles,
                      NbTextes,
                      SuperficieVides
                     ]));
    end;
    lbCodeEPSG.Caption := Format('EPSG:%d', [FDocuDessin.GetCodeEPSG()]);
  except
    CdrTextEditor2.AddLine('Document vide');
  end;
end;

function TfrmHelpSystem.Initialiser(const DC: TDocumentDessin; const HelpFile: string; const QTopics: string;  const ReadOnly: boolean ): boolean;
var
  BP: TBGRABitmap;
begin
  FDocuDessin := DC;
  lbGHCaveDrawName.Caption := GetResourceString(rsGHCAVEDRAWEXENAME + ' ' + IIF(ReadOnly, 'Viewer', 'Editor'));
  Result := True;
  tabShtAbout.Enabled    := true;
  tabshtSandBox.Enabled  := true;
  tabshtStats.Enabled    := true;
  tabShtTextures.Enabled := true;

  // créer une texture
  editSizeTextureX.AsInteger  := 128;
  editTileOverlap.AsInteger   := 4;
  FMaTextureDeTest := CreateWaterTexture(editSizeTextureX.AsInteger, editSizeTextureX.AsInteger);
  // combo resample
  cmbResampleFilter.Clear;
  cmbResampleFilter.Items.Add('rfBox');
  cmbResampleFilter.Items.Add('rfLinear');
  cmbResampleFilter.Items.Add('rfHalfCosine');
  cmbResampleFilter.Items.Add('rfCosine');
  cmbResampleFilter.Items.Add('rfBicubic');
  cmbResampleFilter.Items.Add('rfMitchell');
  cmbResampleFilter.Items.Add('rfSpline');
  cmbResampleFilter.Items.Add('rfLanczos2');
  cmbResampleFilter.Items.Add('rfLanczos3');
  cmbResampleFilter.Items.Add('rfLanczos4');
  cmbResampleFilter.Items.Add('rfBestQuality');
  cmbResampleFilter.DropDownCount := cmbResampleFilter.Items.Count;
  cmbResampleFilter.ItemIndex := 3; //rfcosine
  // éditeurs de texte
  CdrTextEditor1.Initialiser();  // éditeur de code pour les textures
  CdrTextEditor2.Initialiser();  // éditeur pour les stats
end;
end.

