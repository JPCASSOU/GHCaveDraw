unit UnitExportGeoJSON;
// OK
// Plateforme de tests à utiliser: https://geojson.io/#map=16/44.7013/-0.3812

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics;
type TPolygoneOrPolyline = (tpPOLYGON, tpPOLYLINE);
type

{ TGeoJSONExport }

 TGeoJSONExport = class
 strict private
   FP                 : TextFile;
   procedure BeginPolygonPolyline(const P: TPolygoneOrPolyline; const Name: string; const TagString: string);
   procedure EndPolygonPolyline(const P: TPolygoneOrPolyline; const IsLast: boolean);
 private
   FDocTitle          : string;
   FLineColor         : TColor;
   FLineOpacity       : byte;
   FLineWidth         : byte;

   FFillColor         : TColor;
   FFillOpacity       : byte;


   FCentroideLat      : double;
   FCentroideLon      : Double;
   // styles courants
   FCurrentLineWidth    : double;
   FCurrentLineColor    : TColor;
   FCurrentFillColor    : TColor;
   FCurrentLineOpacity  : double;
   FCurrentFillOpacity  : double;
   FListeLayers         : TStringList;

   FCurrentLayerName       : string;
   FCurrentLayerDescription: string;

   FCurrentTagString    : string;
   FCurrentObjectName   : string;
 public
   procedure SetLineProperties(const QLineColor: TColor; const QLineOpacity: byte = 255;  const QLineWidth: byte = 0);
   procedure SetFillProperties(const QFillColor: TColor; const QFillOpacity: byte = 255);
   function  Initialiser(const QFilename: string;
                         const QDocTitle: string;
                         const QCentroideLon, QCentroideLat: Double;
                         const QDefaultColor: TColor;
                         const QOpacity: byte): boolean;
   procedure WriteHeader();
   procedure WriteFooter();
   procedure Finaliser();
   procedure WriteLine(const S: string);
   procedure AddLayer(const L: string; const Desc: string);
   procedure setCurrentLayer(const Idx: integer);
   procedure DefineStylePoly(const LineWidth: double; const LineColor, FillColor: TColor; const LineOpacity, FillOpacity: byte);

   procedure AddPoint(const Lat, Lon, Alt: double; const Name: string; const Description: string; const IsLast: boolean);
   procedure BeginPolygon(const Name: string; const TagString: string);
   procedure AddVertex(const Lat, Lon, Alt: double; const IsLast: boolean);
   procedure EndPolygon(const IsLast: boolean);
   procedure BeginPolyLine(const Name: string; const TagString: string);
   procedure EndPolyLine(const IsLast: boolean);
   procedure DrawSegment(const Lat1, Lon1, Lat2, Lon2: double);

end;

implementation

// pour se rendre indépendant des unités de GHTopo
function FormatterNombreReel(const Value: double; const NbDecs: integer = 2): string;
var
  EWE: String;
begin
  EWE := Format('%%.%df', [NbDecs]);
  Result := Format(EWE, [Value]);
  Result := StringReplace(Result, DefaultFormatSettings.DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
end;
function QColorToHTMLColor(const Couleur: TColor): string;
var
  R, V, B: byte;
begin
  R := Red(Couleur);
  V := Green(Couleur);
  B := Blue(Couleur);
  Result := Format('#%.2X%.2X%.2X', [R, V, B]);
end;

{ TGeoJSONExport }
procedure TGeoJSONExport.WriteLine(const S: string);
begin
  Writeln(FP, S);
end;

procedure TGeoJSONExport.BeginPolygonPolyline(const P: TPolygoneOrPolyline; const Name: string; const TagString: string);
var
  EWE: String;
begin
  case P of
    tpPOLYGON : EWE := 'Polygon';
    tpPOLYLINE: EWE := 'LineString';
  end;
  WriteLine(Format('  { "%s": "%s",', ['type', 'Feature']));
  WriteLine(Format('    "%s": {', ['properties']));
 // WriteLine(Format('      "%s": "%s",', ['name', Name]));
  WriteLine(Format('      "%s": "%s",'  , ['stroke'      , QColorToHTMLColor(FLineColor)]));
  WriteLine(Format('      "%s": "%s",'  , ['fill'        , QColorToHTMLColor(FFillColor)]));
  WriteLine(Format('      "%s": %d,', ['stroke-width', FLineWidth]));
  WriteLine(Format('      "%s": %.2f', ['opacity', FFillOpacity / 256.0]));
  WriteLine(       '    },');
  WriteLine(Format('    "%s": {', ['geometry']));
  WriteLine(Format('      "%s": "%s",', ['type', EWE]));
  WriteLine(Format('      "%s":[', ['coordinates']));
  WriteLine(       '        ['); // support du multi-polygones
end;
procedure TGeoJSONExport.EndPolygonPolyline(const P: TPolygoneOrPolyline; const IsLast: boolean);
begin
  WriteLine(       '        ]');  // support du multi-polygones
  WriteLine(       '      ]');    //  "%s":[', ['coordinates']));
  WriteLine(       '    }');      // "%s": {', ['geometry']));
  WriteLine(format('  }%s', [BoolToStr(IsLast, '', ',')]));
end;

procedure TGeoJSONExport.SetLineProperties(const QLineColor: TColor; const QLineOpacity: byte; const QLineWidth: byte);
begin
  FLineColor         := QLineColor;
  FLineOpacity       := QLineOpacity;
  FLineWidth         := QLineWidth;
end;

procedure TGeoJSONExport.SetFillProperties(const QFillColor: TColor; const QFillOpacity: byte);
begin
  FFillColor         := QFillColor;
  FFillOpacity       := QFillOpacity;
end;

function TGeoJSONExport.Initialiser(const QFilename: string;
                                    const QDocTitle: string;
                                    const QCentroideLon, QCentroideLat: Double;
                                    const QDefaultColor: TColor;
                                    const QOpacity: byte): boolean;
begin
  Result := false;
  FLineColor     := QDefaultColor;
  FLineOpacity   := QOpacity;
  FLineWidth     := 1;

  FFillColor     := QDefaultColor;
  FFillOpacity   := QOpacity;

  AssignFile(FP, QFilename);
  try
    ReWrite(fp);
    result := true;
  except
    Exit(false);
  end;

end;
procedure TGeoJSONExport.Finaliser();
begin
  try
    CloseFile(FP);
  finally
    FreeAndNil(FListeLayers);
  end;
end;

procedure TGeoJSONExport.WriteHeader();
begin
  WriteLine(Format('{ "%s": "%s",', ['type', 'FeatureCollection']));
  WriteLine(Format('  "%s": {', ['style']));
  WriteLine(Format('    "%s": "%s",'  , ['stroke'      , QColorToHTMLColor(FLineColor)]));
  WriteLine(Format('    "%s": "%s",'  , ['fill'        , QColorToHTMLColor(FFillOpacity)]));
  WriteLine(Format('    "%s": "%.0f",', ['stroke-width', 0.00]));
  WriteLine(Format('    "%s": "%.2f"' , ['opacity', FFillOpacity / 256.0]));
  WriteLine(       '  },');
  WriteLine(Format('  "%s":[', ['features']))
end;

procedure TGeoJSONExport.WriteFooter();
begin
  WriteLine(      '   ]'); //"%s":[' ['features']))
  WriteLine(      '}');  //"%s": "%s",' ['type', 'FeatureCollection']))
end;

procedure TGeoJSONExport.AddLayer(const L: string; const Desc: string);
begin

end;

procedure TGeoJSONExport.setCurrentLayer(const Idx: integer);
begin

end;

procedure TGeoJSONExport.DefineStylePoly(const LineWidth: double; const LineColor, FillColor: TColor; const LineOpacity, FillOpacity: byte);
begin

end;

procedure TGeoJSONExport.AddPoint(const Lat, Lon, Alt: double; const Name: string; const Description: string; const IsLast: boolean);
begin
  WriteLine(Format('   {"%s": "%s",', ['type', 'Feature']));
  WriteLine(Format('    "%s": {', ['geometry']));
  WriteLine(Format('      "%s": "%s",', ['type', 'Point']));
  WriteLine(Format('      "%s":[%s, %s]', ['coordinates']));
  WriteLine(Format('   }%s', [BoolToStr(IsLast, '' ,',')]));

end;



procedure TGeoJSONExport.AddVertex(const Lat, Lon, Alt: double; const IsLast: boolean);
begin
  WriteLine(format('          [%s, %s]%s', [FormatterNombreReel(Lon, 8), FormatterNombreReel(Lat, 8), BoolToStr(IsLast, '',',')]));
end;
procedure TGeoJSONExport.BeginPolygon(const Name: string; const TagString: string);
begin
  BeginPolygonPolyline(tpPOLYGON, Name, TagString);


end;

procedure TGeoJSONExport.EndPolygon(const IsLast: boolean);
begin
  EndPolygonPolyline(tpPOLYGON, IsLast);
end;

procedure TGeoJSONExport.BeginPolyLine(const Name: string; const TagString: string);
begin
  BeginPolygonPolyline(tpPOLYLINE, Name, TagString);
end;


procedure TGeoJSONExport.EndPolyLine(const IsLast: boolean);
begin
  EndPolygonPolyline(tpPOLYLINE, IsLast);
end;



procedure TGeoJSONExport.DrawSegment(const Lat1, Lon1, Lat2, Lon2: double);
begin

end;

end.

