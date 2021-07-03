unit UnitExportDXF;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics,
  FPDxfWriteBridge;
const ACAD_LINESTYLE = 'CONTINUOUS';
type TPolygoneOrPolyline = (tpPOLYGON, tpPOLYLINE);
type

{ TGeoJSONExport }

 { TDXFExport }

 TDXFExport = class
 private
   FProcProduceBlocks   : TProduceBlock;
   FProcProduceEntities : TProduceEntity;

   FMyFPDxfWriteBridge: TFPDxfWriteBridge;
   FFileName          : string;
   FDocTitle          : string;
   FLineColor         : TColor;

   FExtMiniX          : double;
   FExtMiniY          : double;

   FExtMaxiX          : double;
   FExtMaxiY          : double;


 public
   property    DXFBridge: TFPDxfWriteBridge read FMyFPDxfWriteBridge;
   constructor Create(AOwner: TComponent);
   destructor  Destroy;
   function    Initialiser(const QFilename: string;
                           const QDocTitle: string;
                           const QProduceBlocks: TProduceBlock;
                           const QProduceEntities: TProduceEntity;
                           const X1, Y1, X2, Y2: double): boolean;
   procedure AddLayer(const QLayerName: string; const QAcadColorIndex: integer; const QLineType: string = ACAD_LINESTYLE);
   procedure Finaliser();
end;
implementation
uses
  DGCDummyUnit;

{ TDXFExport }
constructor TDXFExport.Create(AOwner: TComponent);
begin
  inherited Create;

  FMyFPDxfWriteBridge := TFPDxfWriteBridge.Create(AOwner);
  FMyFPDxfWriteBridge.OnProduceBlock  := nil;
  FMyFPDxfWriteBridge.OnProduceEntity := nil;
end;

destructor TDXFExport.Destroy;
begin
  FMyFPDxfWriteBridge.Destroy;
  inherited;
end;

function TDXFExport.Initialiser(const QFilename: string;
                                const QDocTitle: string;
                                const QProduceBlocks: TProduceBlock;
                                const QProduceEntities: TProduceEntity;
                                const X1, Y1, X2, Y2: double): boolean;
begin
  result := false;//....
  FFileName := QFilename;
  try

    FExtMiniX := X1;
    FExtMiniY := Y1;
    FExtMaxiX := X2;
    FExtMaxiY := Y2;
    FMyFPDxfWriteBridge.setDrawingExtend(FExtMiniX, FExtMiniY, FExtMaxiX , FExtMaxiY);
    FMyFPDxfWriteBridge.OnProduceEntity := QProduceBlocks;
    FMyFPDxfWriteBridge.OnProduceBlock  := QProduceEntities;
    result := true;
  except
    ;;
  end;
end;



procedure TDXFExport.AddLayer(const QLayerName: string; const QAcadColorIndex: integer; const QLineType: string = ACAD_LINESTYLE);
begin
  FMyFPDxfWriteBridge.AddLayer(QLayerName, QLineType, QAcadColorIndex);
end;


procedure TDXFExport.Finaliser();
begin
  FMyFPDxfWriteBridge.SaveToFile(FFileName);
end;


end.
