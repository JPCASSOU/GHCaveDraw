unit UnitKMLExport;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics;

type

{ TKMLExport }

 TKMLExport = class
  private
    FKMLFilename: string;
    FP : TextFile;

    procedure WriteLine(const S: string);
    function  FormatterVersionEncodageXML(const version: string; const encodage: string): string;
    procedure BaliseOuvrante(const Indent: integer; const S: string);
    procedure BaliseFermante(const Indent: integer; const S: string);
    procedure SetAttribut(const Indent: integer; const Key, Value: string);
    procedure BeginStyle(const Indent: integer; const StyleName: string);
    procedure EndStyle(const Indent: integer; const StyleName: string);
    procedure DefineLineStyle(const Indent: integer; const LWidth: double; const LColor: TColor; const Opacity: byte = 255);
    procedure DefineFillStyle(const Indent: integer; const LColor: TColor; const Opacity: byte=255);

    procedure BeginCDATA(const Indent: integer);
    procedure WriteCDATAString(const Indent: integer; const S: string);
    procedure EndCDATA(const Indent: integer);
  public
    function  Initialiser(const Filename: string): boolean;
    procedure Finaliser();
    procedure WriteHeader();
    procedure WriteFooter();
    procedure WriteCommentaire(const S: string);
    procedure BeginFolder(const Indent: integer; const S: string);
    procedure EndFolder(const Indent: integer; const S: string);
    procedure DefineStylePoly(const StyleName: string; const LineWidth: double; const LineColor, FillColor: TColor; const LineOpacity, FillOpacity: byte);

    procedure BeginPolyline(const Name: string; const StyleURL: string);
    procedure EndPolyline();
    procedure BeginPolygon(const Name: string; const StyleURL: string);
    procedure EndPolygon();
    procedure AddPoint(const Lat, Lon, Alt: double; const Name: string; const Description: string);
    procedure AddVertex(const Lat, Lon, Alt: double);
end;

implementation
const
  KML_OPENGIS_WEBSITE     = 'http://www.opengis.net/kml/2.2';
  KML_GOOGLE_KML_WEBSITE  = 'http://www.google.com/kml/ext/2.2';
  W3C_W3_WEBSITE          = 'http://www.w3.org/2005/Atom';
  W3C_XML_SCHEMA_WEBSITE  = 'http://www.w3.org/2001/XMLSchema-instance';
  GPX_TOPOGRAPHIX_WEBSITE = 'http://www.topografix.com/GPX/1/0';
const
  INDENT_PLACEMARK = 6;
const
  KML_KEYWORD_DOCUMENT          = 'Document';
  KML_KEYWORD_FOLDER            = 'Folder';
  KML_KEYWORD_PLACEMARK         = 'Placemark';
  KML_KEYWORD_StyleUrl          = 'styleUrl';
  KML_KEYWORD_Name              = 'name';
  KML_KEYWORD_Style             = 'Style';
  KML_KEYWORD_LineStyle         = 'LineStyle';
  KML_KEYWORD_PolyStyle         = 'PolyStyle';
  KML_KEYWORD_Point             = 'Point';
  KML_KEYWORD_Polygon           = 'Polygon';
  KML_KEYWORD_Polyline          = 'LineString';
  KML_KEYWORD_Extrude           = 'extrude';
  KML_KEYWORD_Tesselate         = 'tesselate';
  KML_KEYWORD_altitudeMode      = 'altitudeMode';
  KML_KEYWORD_outerBoundaryIs   = 'outerBoundaryIs';
  KML_KEYWORD_LinearRing        = 'LinearRing';
  KML_KEYWORD_Description   = 'description';
  KML_KEYWORD_Coordinates   = 'coordinates';

  
{ TKMLExport }

function TKMLExport.Initialiser(const Filename: string): boolean;
begin
  Result := false;
  FKMLFilename := Filename;
  AssignFile(FP, FKMLFilename);
  try
    ReWrite(fp);
    Result := True;
  except

  end;
end;

procedure TKMLExport.Finaliser();
begin
  try

  finally
    CloseFile(fp);
  end;
end;

procedure TKMLExport.WriteHeader();
begin
  WriteLn(fp, FormatterVersionEncodageXML('1.0', 'UTF-8'));
  WriteLn(fp, Format('<kml xmlns="%s" xmlns:gx="%s" xmlns:kml="%s" xmlns:atom="%s">',
                [KML_OPENGIS_WEBSITE,
                 KML_GOOGLE_KML_WEBSITE,
                 KML_OPENGIS_WEBSITE,
                 W3C_W3_WEBSITE
                ]));
  BaliseOuvrante(1, KML_KEYWORD_DOCUMENT);
end;

procedure TKMLExport.WriteFooter();
begin
  BaliseFermante(1, KML_KEYWORD_DOCUMENT);
  BaliseFermante(0, 'kml');
end;

procedure TKMLExport.WriteCommentaire(const S: string);
begin
  WriteLine('<!-- ' + S + '-->');
end;

procedure TKMLExport.DefineStylePoly(const StyleName: string; const LineWidth: double; const LineColor, FillColor: TColor; const LineOpacity, FillOpacity: byte);
begin
  BeginStyle(2, StyleName);
    DefineLineStyle(4, LineWidth, LineColor, LineOpacity);
    DefineFillStyle(4, FillColor, FillOpacity);
  EndStyle(2, StyleName);
end;

procedure TKMLExport.BeginCDATA(const Indent: integer);
begin
  WriteLine(Format('%s<![CDATA[', [StringOfChar(' ', Indent)]));
end;

procedure TKMLExport.WriteCDATAString(const Indent: integer; const S: string);
begin
  WriteLine(Format('%s%s', [StringOfChar(' ', Indent + 4), S]));
end;

procedure TKMLExport.EndCDATA(const Indent: integer);
begin
  WriteLine(Format('%s]]>', [StringOfChar(' ', Indent)]));
end;

procedure TKMLExport.AddPoint(const Lat, Lon, Alt: double; const Name: string; const Description: string);
begin
  BaliseOuvrante(INDENT_PLACEMARK, KML_KEYWORD_PLACEMARK);
    SetAttribut(INDENT_PLACEMARK + 2, KML_KEYWORD_Name, Name);
    BaliseOuvrante(INDENT_PLACEMARK + 2, KML_KEYWORD_Description);
      BeginCDATA(INDENT_PLACEMARK + 4);
        WriteCDATAString(INDENT_PLACEMARK + 4, Trim(Description));
        WriteCDATAString(INDENT_PLACEMARK + 4, Format('<HR>Lat: %.8f, Lon: %.8f, Alt: %.3f', [Lat, Lon, Alt]));
      EndCDATA(INDENT_PLACEMARK + 4);
    BaliseFermante(INDENT_PLACEMARK + 2, KML_KEYWORD_Description);
    BaliseOuvrante(INDENT_PLACEMARK + 2, KML_KEYWORD_Point);
      SetAttribut(INDENT_PLACEMARK + 4, KML_KEYWORD_Coordinates, Format('%.8f, %.8f, %.3f', [Lon, Lat, Alt]));
    BaliseFermante(INDENT_PLACEMARK + 2,  KML_KEYWORD_Point);
  BaliseFermante(INDENT_PLACEMARK, KML_KEYWORD_PLACEMARK);
end;

procedure TKMLExport.BeginPolygon(const Name: string; const StyleURL: string);
begin
  WriteCommentaire('Polygone: ' + Name);
  BaliseOuvrante(INDENT_PLACEMARK, KML_KEYWORD_PLACEMARK);
  SetAttribut(INDENT_PLACEMARK + 2, KML_KEYWORD_Name, Name);
  SetAttribut(INDENT_PLACEMARK + 2, KML_KEYWORD_StyleUrl, '#' + StyleURL);
  BaliseOuvrante(INDENT_PLACEMARK + 2, KML_KEYWORD_Polygon);
  SetAttribut(INDENT_PLACEMARK + 4, KML_KEYWORD_Tesselate, '1');
  BaliseOuvrante(INDENT_PLACEMARK + 4, KML_KEYWORD_outerBoundaryIs);
  BaliseOuvrante(INDENT_PLACEMARK + 6, KML_KEYWORD_LinearRing);
  BaliseOuvrante(INDENT_PLACEMARK + 8, KML_KEYWORD_Coordinates);
end;

procedure TKMLExport.AddVertex(const Lat, Lon, Alt: double);
begin
  WriteLine(Format('%s%.8f, %.8f, %.3f', [StringOfChar(' ', INDENT_PLACEMARK + 8), Lon, Lat, Alt + 0.01]));
end;

procedure TKMLExport.EndPolygon();
begin
  BaliseFermante(INDENT_PLACEMARK + 8, KML_KEYWORD_Coordinates);
  BaliseFermante(INDENT_PLACEMARK + 6, KML_KEYWORD_LinearRing);
  BaliseFermante(INDENT_PLACEMARK + 4, KML_KEYWORD_outerBoundaryIs);
  BaliseFermante(INDENT_PLACEMARK + 2, KML_KEYWORD_Polygon);
  BaliseFermante(INDENT_PLACEMARK, KML_KEYWORD_PLACEMARK);
end;

procedure TKMLExport.BeginPolyline(const Name: string; const StyleURL: string);
begin
  WriteCommentaire('Polyligne: ' + Name);
  BaliseOuvrante(INDENT_PLACEMARK, KML_KEYWORD_PLACEMARK);
  SetAttribut(INDENT_PLACEMARK + 2, KML_KEYWORD_Name, Name);
  SetAttribut(INDENT_PLACEMARK + 2, KML_KEYWORD_StyleUrl, '#' + StyleURL);
  BaliseOuvrante(INDENT_PLACEMARK + 2, KML_KEYWORD_Polyline);
  SetAttribut(INDENT_PLACEMARK + 4, KML_KEYWORD_Extrude, '1');
  SetAttribut(INDENT_PLACEMARK + 4, KML_KEYWORD_Tesselate, '1');
  SetAttribut(INDENT_PLACEMARK + 4, KML_KEYWORD_altitudeMode, 'absolute');
  BaliseOuvrante(INDENT_PLACEMARK + 8, KML_KEYWORD_Coordinates);


end;

procedure TKMLExport.EndPolyline();
begin
  BaliseFermante(INDENT_PLACEMARK + 8, KML_KEYWORD_Coordinates);
  BaliseFermante(INDENT_PLACEMARK + 2, KML_KEYWORD_Polyline);
  BaliseFermante(INDENT_PLACEMARK, KML_KEYWORD_PLACEMARK);
end;

procedure TKMLExport.WriteLine(const S: string);
begin
  WriteLn(fp, S);
end;

//******************************************************************************
function TKMLExport.FormatterVersionEncodageXML(const version: string; const encodage: string): string; inline;
begin
  Result := Format('<?xml version="%s" encoding="%s"?>', [version, encodage]);
end;

procedure TKMLExport.BaliseOuvrante(const Indent: integer; const S: string);
begin
  WriteLine(Format('%s<%s>', [StringOfChar(' ', Indent), S]));
end;

procedure TKMLExport.BaliseFermante(const Indent: integer; const S: string);
begin
  WriteLine(Format('%s</%s>', [StringOfChar(' ', Indent), S]));
end;
procedure TKMLExport.BeginFolder(const Indent: integer; const S: string);
begin
  BaliseOuvrante(Indent, KML_KEYWORD_FOLDER);
  SetAttribut(Indent + 2, 'name', S);
  SetAttribut(Indent + 2, 'open', '1');

end;

procedure TKMLExport.EndFolder(const Indent: integer; const S: string);
begin
  BaliseFermante(Indent, KML_KEYWORD_FOLDER);
end;

procedure TKMLExport.SetAttribut(const Indent: integer; const Key, Value: string);
begin
  WriteLine(Format('%s<%s>%s</%s>', [StringOfChar(' ', Indent), Key, Value, Key]));
end;

procedure TKMLExport.BeginStyle(const Indent: integer; const StyleName: string);
begin
  WriteLine(Format('%s<%s id="%s">', [StringOfChar(' ', Indent), KML_KEYWORD_Style, StyleName]));
end;

procedure TKMLExport.EndStyle(const Indent: integer; const StyleName: string);
begin
  WriteLine(Format('%s</%s>', [StringOfChar(' ', Indent), KML_KEYWORD_Style, StyleName]));
end;

procedure TKMLExport.DefineLineStyle(const Indent: integer; const LWidth: double; const LColor: TColor; const Opacity: byte = 255);
var
  EWE: String;
begin
  EWE := Format('%.2X%.2X%.2X%.2X', [Opacity, Blue(LColor), Green(LColor), Red(LColor)]);
  BaliseOuvrante(Indent, KML_KEYWORD_LineStyle);
    SetAttribut(Indent + 2, 'color', EWE);
    SetAttribut(Indent + 2, 'width', Format('%.2f', [LWidth]));
  BaliseFermante(Indent, KML_KEYWORD_LineStyle);
end;
procedure TKMLExport.DefineFillStyle(const Indent: integer; const LColor: TColor; const Opacity: byte = 255);
var
  EWE: String;
begin
  EWE := Format('%.2X%.2X%.2X%.2X', [Opacity, Blue(LColor), Green(LColor), Red(LColor)]);
  BaliseOuvrante(Indent, KML_KEYWORD_PolyStyle);
    SetAttribut(Indent + 2, 'color', EWE);
  BaliseFermante(Indent, KML_KEYWORD_PolyStyle);
end;





end.

