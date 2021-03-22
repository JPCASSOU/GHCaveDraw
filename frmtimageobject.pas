unit frmTImageObject;

{$INCLUDE CompilationParameters.inc}

interface

uses
  GHCD_Types
  , GeneralFunctions
  , UnitDocDessin
  , Classes
  //, windows,
  , SysUtils, FileUtil
  , BGRABitmap
  , BGRABitmapTypes
  , BGRACanvas
  , BGRAPhongTypes
  , BGRAGradients, curredit
  , Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, EditBtn, ExtDlgs
  ;

type

  { TdlgTImageObject }

  TdlgTImageObject = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    btnFindPT1: TButton;
    btnFindPT2: TButton;
    btnOuvrirImg: TButton;
    Button2: TButton;
    chkPickPT1: TCheckBox;
    chkPickPT2: TCheckBox;
    editDescroImage: TEdit;
    editPT1IDTerrain: TEdit;
    editPT2IDTerrain: TEdit;
    editX1Img: TCurrencyEdit;
    editX2Img: TCurrencyEdit;
    editXReelPT1: TCurrencyEdit;
    editXReelPT2: TCurrencyEdit;
    editY1Img: TCurrencyEdit;
    editY2Img: TCurrencyEdit;
    editYReelPT1: TCurrencyEdit;
    editYReelPT2: TCurrencyEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbFileNameImage: TLabel;
    lbFoundStationP1: TStaticText;
    lbFoundStationP2: TStaticText;
    lbHintTodo: TStaticText;
    lbMousePosX1: TStaticText;
    lbMousePosX2: TStaticText;
    lbMousePosY1: TStaticText;
    lbMousePosY2: TStaticText;
    lbSizeImage: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pnlCalibration: TPanel;
    pnlImage: TPanel;
    pbxImage: TPaintBox;
    sclImgOpacity: TScrollBar;
    lbCoordSouris: TStaticText;
    ScrollBox1: TScrollBox;
    lbCalibrationOK: TStaticText;

    procedure BitBtn3Click(Sender: TObject);
    procedure btnOuvrirImgClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnFindPT2Click(Sender: TObject);
    procedure btnFindPT1Click(Sender: TObject);
    procedure chkPickPT1Change(Sender: TObject);
    procedure chkPickPT2Change(Sender: TObject);
    procedure pbxImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbxImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbxImagePaint(Sender: TObject);
    procedure sclImgOpacityChange(Sender: TObject);
  private
    { private declarations }
    FDoCreateImage: boolean;
    FDossierContenantImage: string;
    FMyDocDessin     : TDocumentDessin;
    FCalibrationPT1  : TPoint;
    FCalibrationPT2  : TPoint;
    FCalibrationIsOK : boolean;

    FImageObject: TImageObject;
    FMyTmpBuff  : TBGRABitmap;
    FMyImageContent: TBGRABitmap;
    FImageLoaded: boolean;
    // coordonnées réelles des coins de l'image
    FC1, FC2: TPoint2Df;
    function CalculerLimitesReellesImage(): boolean;
    function DoDrawPtCalibration(const P: TPoint): boolean;
    function LoadImageFile(const QFilename: string): boolean;
    procedure Redessin;
    procedure RenseignerChamps();
    function  GetCoordsGCS(const QX, QY: integer): TPoint2Df;
  public
    { public declarations }
    procedure Initialiser(const FD: TDocumentDessin;
                          const QInitialDir: string;
                          const DoCreateImage: boolean);
    procedure Finaliser();
    // à utiliser exclusivement pour la modification d'une image
    procedure setImageObject(const Img: TImageObject);
    function  getImageObject: TImageObject;
    procedure setPositionPtRefImage(const P0: TPoint2Df; const CoteCarre: double); overload;
    procedure setPositionPtRefImage(const P1, P2: TPoint2Df); overload;
  end;

var
  dlgTImageObject: TdlgTImageObject;

implementation

{$R *.lfm}

{ TdlgTImageObject }


procedure TdlgTImageObject.pbxImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (chkPickPT1.Checked) then
  begin
    FCalibrationPT1 := MakeTPoint(X, Y);
    pbxImage.Invalidate;
    lbMousePosX1.Caption := Format('%d', [FCalibrationPT1.X]);
    lbMousePosY1.Caption := Format('%d', [FCalibrationPT1.Y]);
    //lbHintTodo.Caption   := 'Entrer les coordonnées du premier point';
    chkPickPT1.Checked:= false; // et on désactive le mode Pick de P1

  end;
  if (chkPickPT2.Checked) then
  begin
    FCalibrationPT2 := MakeTPoint(X, Y);
    pbxImage.Invalidate;
    //lbHintTodo.Caption   := 'Entrer les coordonnées du second point';
    chkPickPT2.Checked:= false; // et on désactive le mode Pick de P2
    lbMousePosX2.Caption := Format('%d', [FCalibrationPT2.X]);
    lbMousePosY2.Caption := Format('%d', [FCalibrationPT2.Y]);
  end;
end;


procedure TdlgTImageObject.pbxImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  PP: TPoint2Df;
begin
  PP := GetCoordsGCS(X, Y);
  if (FImageLoaded) then lbCoordSouris.Caption := format('%d, %d - %.2f, %.2f', [X, Y, PP.X, PP.Y]);
end;

procedure TdlgTImageObject.pbxImagePaint(Sender: TObject);
begin
  Redessin();
end;

procedure TdlgTImageObject.Redessin();
  procedure DrawPtCalibration(const P: TPoint; const C: TColor);
  begin
    FMyTmpBuff.CanvasBGRA.Pen.Color:= clBlack;
    FMyTmpBuff.CanvasBGRA.Brush.Opacity := 255;
    FMyTmpBuff.CanvasBGRA.Brush.Color   := C;
    FMyTmpBuff.CanvasBGRA.EllipseC(P.X, P.Y, 4, 4);
  end;
var
  R: TRECT;
begin
  if (not FImageLoaded) then Exit;
  try
    // copier le contenu de l'image sauvegardée dans FMyImageContent
    R := MakeTRect(0, 0, FMyImageContent.Width, FMyImageContent.Height);
    FMyTmpBuff.CanvasBGRA.CopyRect(0, 0, FMyImageContent, R);
    // rectangle de masquage
    FMyTmpBuff.CanvasBGRA.Brush.Color   := clWhite;
    FMyTmpBuff.CanvasBGRA.Brush.Opacity := sclImgOpacity.Position;
    FMyTmpBuff.CanvasBGRA.Rectangle(0, 0, FMyTmpBuff.Width, FMyTmpBuff.Height, true);
    // points de calibration
    if (DoDrawPtCalibration(FCalibrationPT1)) then DrawPtCalibration(FCalibrationPT1, clRed);
    if (DoDrawPtCalibration(FCalibrationPT2)) then DrawPtCalibration(FCalibrationPT2, clBlue);
    FMyTmpBuff.Draw(pbxImage.Canvas, 0, 0, True);
  except
    ;
  end;
end;

procedure TdlgTImageObject.RenseignerChamps;
begin
  editDescroImage.Text := FImageObject.Description;
  (*
  editXReelPT1.Value := FImageObject.PositionCoinsImage.X1;
  editYReelPT1.Value := FImageObject.PositionCoinsImage.Y1;
  editXReelPT2.Value := FImageObject.PositionCoinsImage.X2;
  editYReelPT2.Value := FImageObject.PositionCoinsImage.Y2;
  //*)

  editX1Img.Value := FImageObject.PositionCoinsImage.X1;
  editY1Img.Value := FImageObject.PositionCoinsImage.Y1;
  editX2Img.Value := FImageObject.PositionCoinsImage.X2;
  editY2Img.Value := FImageObject.PositionCoinsImage.Y2;
  sclImgOpacity.Position := FImageObject.Opacite;
end;

procedure TdlgTImageObject.sclImgOpacityChange(Sender: TObject);
begin
  FImageObject.Opacite := sclImgOpacity.Position;
  pbxImage.Invalidate; //Redessin();
end;




function TdlgTImageObject.DoDrawPtCalibration(const P: TPoint): boolean;
begin
  result := (P.X > -1) and (P.Y > -1);
end;

procedure TdlgTImageObject.Initialiser(const FD: TDocumentDessin;
                                       const QInitialDir: string;
                                       const DoCreateImage: boolean);
begin
  pnlCalibration.Enabled := false;
  FMyDocDessin     := FD;
  FDoCreateImage   := DoCreateImage;
  FDossierContenantImage := QInitialDir;
  FCalibrationIsOK := false;
  FCalibrationPT1  := MakeTPoint(-1, -1);
  FCalibrationPT2  := MakeTPoint(-1, -1);
  FImageLoaded     := false;
  FMyImageContent  := TBGRABitmap.Create;
  FMyTmpBuff       := TBGRABitmap.Create;
  ScrollBox1.DoubleBuffered := True;
  lbCalibrationOK.Color := IIF(FCalibrationIsOK, clGreen, clRed);
end;



procedure TdlgTImageObject.Finaliser;
begin
  FMyTmpBuff.Free;
  FMyImageContent.Free;
end;

function TdlgTImageObject.GetCoordsGCS(const QX, QY: integer): TPoint2Df;
var
  dx, dy: Double;
begin
  Result := MakeTPoint2Df(-1.00, -1.00);
  if (FCalibrationIsOK) then
  begin
    dx := FC2.X - FC1.X;
    dy := FC2.Y - FC1.Y;
    Result.X := FC1.X + dx * QX / pbxImage.Width;
    Result.Y := FC1.Y + dy * QY / pbxImage.Height;
  end;
end;

function TdlgTImageObject.LoadImageFile(const QFilename: string): boolean;
begin
  Result := false;
  FImageObject.SrcFilename := ExtractFileName(QFileName);
  lbFileNameImage.Caption := FImageObject.SrcFilename;
  try
    FMyImageContent.LoadFromFile(QFilename);
    pbxImage.Width  := FMyImageContent.Width;
    pbxImage.Height := FMyImageContent.Height;
    FMyTmpBuff.SetSize(FMyImageContent.Width, FMyImageContent.Height);
    lbSizeImage.Caption := Format('%dx%d %d', [FMyImageContent.Width, FMyImageContent.Height, -1]);
    editXReelPT2.Value := editXReelPT1.Value;
    editYReelPT2.Value := editYReelPT1.Value;
    pbxImage.Invalidate;
    Result := True;
  except
  end;
end;



procedure TdlgTImageObject.btnOuvrirImgClick(Sender: TObject);
var
  PD: TOpenPictureDialog;
  r: Extended;
begin
  FImageLoaded := false;
  //FImageObject.SrcFilename := GetGHCaveDrawDirectory() + 'exemple_image.png';
  PD := TOpenPictureDialog.Create(Application);
  try
    PD.InitialDir := FDossierContenantImage;
    if (PD.Execute) then
    begin
      FImageLoaded := LoadImageFile(PD.FileName);
      pnlCalibration.Enabled := FImageLoaded;
    end;
  finally
    PD.Free;
  end;
end;



procedure TdlgTImageObject.Button2Click(Sender: TObject);
begin
  FCalibrationIsOK := CalculerLimitesReellesImage();
  lbCalibrationOK.Color := IIF(FCalibrationIsOK, clGreen, clRed);
end;



procedure TdlgTImageObject.BitBtn3Click(Sender: TObject);
begin

end;

procedure TdlgTImageObject.btnFindPT1Click(Sender: TObject);
var
  BP: TBaseStation;
begin
  if (FMyDocDessin.FindBasePoint(Trim(editPT1IDTerrain.Text), BP)) then
  begin
    editXReelPT1.Value := BP.PosStation.X;
    editYReelPT1.Value := BP.PosStation.Y;
    lbFoundStationP1.Caption := BP.IDTerrain;
    ShowMessage('station 1 trouvée');
  end
  else
  begin
    editXReelPT1.Value := 0.00;
    editYReelPT1.Value := 0.00;
    lbFoundStationP1.Caption := 'Not found';
  end;
end;

procedure TdlgTImageObject.chkPickPT1Change(Sender: TObject);
begin
  chkPickPT2.Checked := Not chkPickPT1.Checked;
end;

procedure TdlgTImageObject.chkPickPT2Change(Sender: TObject);
begin
  chkPickPT1.Checked := Not chkPickPT2.Checked;
  //if (chkPickPT2.Checked) then lbHintTodo.Caption := 'Click sur l''image pour le second  point';
end;

procedure TdlgTImageObject.btnFindPT2Click(Sender: TObject);
var
  EWE: TPoint2Df;
  BP: TBaseStation;
begin
  if (FMyDocDessin.FindBasePoint(Trim(editPT2IDTerrain.Text), BP)) then
  begin
    editXReelPT2.Value := BP.PosStation.X;
    editYReelPT2.Value := BP.PosStation.Y;
    lbFoundStationP2.Caption := BP.IDTerrain;
    ShowMessage('station 2 trouvée');
  end
  else
  begin
    editXReelPT2.Value := 0.00;
    editYReelPT2.Value := 0.00;
    lbFoundStationP2.Caption := 'Not found';
  end;
end;



procedure TdlgTImageObject.setImageObject(const Img: TImageObject);
begin
  FImageObject := Img;
  // renseigner les champs
  RenseignerChamps();
end;



function TdlgTImageObject.getImageObject: TImageObject;
begin
  Result := FImageObject;
  // les données issues des zones de texte sont récupérées à cet instant
  // (pour éviter la gestion en temps réel)
  Result.Description := editDescroImage.Text;
end;

procedure TdlgTImageObject.setPositionPtRefImage(const P0: TPoint2Df; const CoteCarre: double);
begin
  editXReelPT1.Value := P0.X;
  editYReelPT1.Value := P0.Y;
  editXReelPT2.Value := P0.X + CoteCarre;
  editYReelPT2.Value := P0.Y + CoteCarre;
end;
procedure TdlgTImageObject.setPositionPtRefImage(const P1, P2: TPoint2Df);
begin
  editXReelPT1.Value := P1.X;
  editYReelPT1.Value := P1.Y;
  editXReelPT2.Value := P2.X;
  editYReelPT2.Value := P2.Y;
end;

function TdlgTImageObject.CalculerLimitesReellesImage(): boolean;
var
  dx, dy: Extended;
  ix, iy: Integer;
  rx, ry: Extended;
  P1, P2: TPoint2Df;
begin
  AfficherMessage(Format('%s.CalculerLimitesReellesImage:', [self.ClassName]));
  Result := false;
  try
    P1 := MakeTPoint2Df(editXReelPT1.Value , editYReelPT1.Value);
    P2 := MakeTPoint2Df(editXReelPT2.Value , editYReelPT2.Value);
    AfficherMessage(Format('-- %d, %d -> %d, %d', [FCalibrationPT1.X, FCalibrationPT1.Y,
                                                   FCalibrationPT2.X, FCalibrationPT2.X]));

    dx := P2.X - P1.X;
    dy := P2.Y - P1.Y;
    ix := FCalibrationPT2.X - FCalibrationPT1.x;
    iy := FCalibrationPT2.y - FCalibrationPT1.y;
    if ((ix * iy) = 0) then
    begin
      ShowMessage('Calibration incorrecte ou non effectuée');
      exit;
    end;
    rx := dx / ix;
    ry := dy / iy;

    FC1 := MakeTPoint2Df(P1.X - FCalibrationPT1.X * rx,
                        P1.Y + (FMyTmpBuff.Height - FCalibrationPT1.Y) * ry);
    FC2 := MakeTPoint2Df(P1.X + (FMyTmpBuff.Width  - FCalibrationPT1.X) * rx,
                        P1.Y - FCalibrationPT1.Y * ry);

    // position réelle des coins de l'image
    FImageObject.PositionCoinsImage := MakeTRect2Df(FC1.X, FC1.Y, FC2.X, FC2.Y);
    // image activée par défaut
    FImageObject.Displayed := True;
    // opacité
    FImageObject.Opacite    := sclImgOpacity.Position;
    // acquittement visuel
    RenseignerChamps();

    Result := True;
  except
  end;
end;

end.

