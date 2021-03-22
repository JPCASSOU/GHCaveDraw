{$DEFINE FOR_GHCAVEDRAW}
//{$UNDEF FOR_GHCAVEDRAW}
{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
Filename:	oodrawwriter.pas
Use:		Write OpenOffice Draw (odg) Data from Canvas procedures
First Author: 	Dr. Marcus Sackrow
Date:		July 2009
}

(* NOTES IMPORTANTES:

- Unités  : Centièmmes de millimètres
- Origine : Coin haut gauche de la feuille physique (et non des marges)
- Orientation: X croissants de gauche à droite
               y croissants de haut en bas
- Pour les chemins, mettre impérativement 'M0 0 ' en tête

10/02/2014: Styles OK sauf textes
13/02/2014: Réécriture complète sans utilisation du DOM ni du TCanvas
14/02/2014: Ajout du wrapper DessinTriangle()
            Scan sous XMLValidator des fichiers XML générés : aucune erreur.
            FIXE: Styles de page OK
17/02/2014: Fixation de bugs dans la génération des courbes (utilise les transformations de ViewBox)
Graphismes implémentés
Date        Lignes   Polylignes  Beziers   Polygone   Ellipses  Cadre_texte Symboles  Images
13/02/2014     OK       OK          OK         OK         OK         OK         N/D      OK
25/02/2015: Ajout des fontes usuelles

//*)
unit oodrawwriter;

{$mode delphi}{$H+}

interface

uses
  GHCD_Types,
  Classes, SysUtils,
  {$IFDEF FOR_GHCAVEDRAW}  GeneralFunctions, {$ENDIF}
  Zip, ZipUtils,
  Graphics, png, PNGcomn,
  FileUtil,
  dateutils,
  Math;
type TScalaire = double;
type TODGPoint2Df = record
  X: TScalaire;
  Y: TScalaire;
end;
type TODGArrayPoints2Df = array of TODGPoint2Df;


type TODGRect2Df = record
  Left   : TScalaire;
  Top    : TScalaire;
  Right  : TScalaire;
  Bottom : TScalaire;
end;
type TODGBezierArc = record
  P1 : TODGPoint2Df;
  PC1: TODGPoint2Df;
  PC2: TODGPoint2Df;
  P2 : TODGPoint2Df;
end;
(*
type TStyleGraphicProperties = record                                // Paramètres usuels
  draw_stroke                  : string;  // "solid"                 // OUI
  svg_stroke_width             : double;  // ="0.071cm"              // OUI
  svg_stroke_color             : TColor;  // ="#800000"              // OUI
  draw_marker_start_width      : double;  // ="0.2cm"
  draw_marker_start_center     : boolean; // = "false"
  draw_marker_end_width        : double;  // ="0.2cm"
  draw_marker_end_center       : boolean; // = "false"
  svg_stroke_opacity           : double;  // ="100%"                 // OUI
  draw_stroke_linejoin         : string;  // ="round"
  draw_fill                    : string;  // ="solid"                // OUI
  draw_fill_color              : TColor;  // ="#00ffff"              // OUI
  draw_secondary_fill_color    : TColor;  // ="#99ccff"
  draw_gradient_step_count     : double;  // ="0"
  draw_fill_hatch_solid        : boolean; // = "false"
  draw_opacity                 : double;  // ="100%"                 // OUI
  draw_fill_image_width        : double;  // ="0cm"
  draw_fill_image_height       : double;  // ="0cm"
  style_repeat                 : string;  // ="repeat"
  draw_fill_image_ref_point_x  : double;  // ="0%"
  draw_fill_image_ref_point_y  : double;  // ="0%"
  draw_fill_image_ref_point    : string;  // ="center"
  draw_tile_repeat_offset      : double;  // ="0% vertical"
  draw_textarea_horizontal_align : string;  // ="justify"
  draw_textarea_vertical_align   : string;  // ="top"
  draw_auto_grow_height          : boolean;    // = "true"
  draw_auto_grow_width           : boolean;    // = "false"
  draw_fit_to_size               : boolean;    // = "false"
  draw_fit_to_contour            : boolean;    // = "false"
  // fo
  fo_max_height                  : double; // ="0cm"
  fo_max_width                   : double; // ="0cm"
  fo_min_height                  : double; // ="0cm"
  fo_min_width                   : double; // ="0cm"
  fo_padding_top                 : double; // ="0.16cm"
  fo_padding_bottom              : double; // ="0.16cm"
  fo_padding_left                : double; // ="0.285cm"
  fo_padding_right               : double; // ="0.285cm"
  fo_wrap_option                 : string;  // ="no_wrap"
  // draw
  draw_shadow                    : string;  // ="hidden"
  draw_shadow_offset_x           : double; // ="0.2cm"
  draw_shadow_offset_y           : double; // ="0.2cm"
  draw_shadow_color              : TColor; // ="#808080"
  draw_shadow_opacity            : double; //="100%"
  // text
  text_animation                 : string;  // ="none"
  text_animation_direction       : string;  // ="left"
  text_animation_start_inside    : boolean;    // = "false"
  text_animation_stop_inside     : boolean;    // = "false"
  text_animation_repeat          : double; // ="0"
  text_animation_delay           : string;  // ="P0D"
  text_animation_steps           : double; // ="0cm">
end;
//*)
type TPointeurFichier = type TextFile;

type

{ Todg }

 Todg = class //(Tcanvas)
                 private
                   TEMPDIR:string;      // for the files before put to zip (init at create)
                   FPointerToFileContent: TPointeurFichier;
                   // Nb d'objets
                   FNbCourbesBeziers   : integer;
                   FNbPolygones        : integer;
                   FNbTextes           : integer;
                   FNbPhotos           : integer;
                   // Position du curseur
                   FPenPos : TODGPoint2Df;
                   // Largeur et hauteur
                   FWidth : TScalaire;
                   FHeight: TScalaire;
                   // listes de styles; on les compose directement dans une simple chaîne
                   FListeStylesLignesEtPolygones: TStringList;
                   FListeStylesTextes           : TStringList;
                   FListeDesPhotos              : TStringList;
                   //noms de styles
                   FCurrentStyleName            : string;
                   // nombre d'entités
                   FNbPolybeziers   : integer;
                   procedure CheckSize(ax,ay: TScalaire); //set width and height
                   function GenererImageBidon(const FichierPNG: string; const IsLandscape: boolean): boolean;
                   function GetAPhoto(const Idx: integer): string;
                   function GetNbPhotos: integer;
                   procedure SetPixel(X,Y: Integer; Value: TColor); //for property Pixels
                   // ajouts d'attributs à un item ou tête de section
                   procedure AddAttribute(var AllAttributes: string; const Cle, Valeur: string; const DoCRLF: boolean);
                   //***********************************************************

                   function GetAStyleLigne(const Idx: integer): string;
                   function GetAStyleTexte(const Idx: integer): string;
                public
                   //class function
                   constructor Create;
                   destructor Destroy; override;
                   procedure Clear;

                   procedure GenererODG(const filename:string);
                   // Pixels
                   property  OOoDrwPixels[X, Y: Integer]: TColor write SetPixel;

                   // properties
                   property width : TScalaire read FWidth;
                   property height: TScalaire read FHeight;
                   // **********************************************************
                   // NOUVELLE VERSION SANS DOM
                   function  InitialiserFichierContent: boolean;
                   procedure OuvrirNouvellePage(const PageName, StylePage, StyleMasterPage: string);
                   procedure DebutGroupe(const NomGroupe: string);
                   procedure FinGroupe(const NomGroupe: string);
                   procedure FermerPage(const PageName: string);
                   procedure CloturerFichierContent;


                   // styles (nouvelle version)
                   function GetCurrentStylename: string;
                   function GetNbStylesLignes: integer;
                   function GetNbStylesTextes: integer;
                   procedure AddUsuallyStyleGraphic(const QNamestyle: string;
                                                    const QDrawStroke: string;
                                                    const QSVGStrokeWidth: double;
                                                    const QSVGStrokeColor: TColor;
                                                    const QSVGStrokeOpacity: integer;
                                                    const QDrawFill: string;
                                                    const QDrawFillColor: TColor;
                                                    const QDrawOpacity: integer);
                   procedure AddUsuallyStyleText(const QNamestyle: string;
                                                    const QBackColor   : TColor;
                                                    const QFontName: string;
                                                    const QFontColor: TColor;
                                                    const QFontStyle: TFontStyles;
                                                    const QFontSize: double;
                                                    const QAlignement: integer);
                   procedure RegenererFichierMIME(const FichierMIME: string);
                   procedure RegenererFichierStyles(const FichierXML: string);
                   procedure RegenererFichierManifest(const FichierXML: string);
                   procedure RegenererFichierSettings(const FichierXML: string);
                   procedure RegenererFichierMeta(const FichierXML: string);
                   //----------------------
                   // dessin des objets
                   procedure DessinMoveTo(X1, Y1: TScalaire); overload;
                   procedure DessinMoveTo(p: TODGPoint2Df); overload;

                   procedure DessinLigne(const QStyle: string; const p1, p2: TODGPoint2Df); overload;
                   procedure DessinLigne(const QStyle: string; const Points: TODGRect2Df); overload;
                   procedure DessinLigneTo(const QStyle: string; const X1, Y1: TScalaire); overload;
                   procedure DessinLigne(const QStyle: string; const x1, y1, x2, y2: TScalaire); overload;
                   // dessin de polyugones et polylignes
                   procedure DessinRectangle(const QStyle: string; const x1, y1, x2, y2: TScalaire; const QFilled: boolean);
                   procedure DessinTriangle(const QStyle: string; const x1, y1, x2, y2, x3, y3: TScalaire; const QFilled: boolean);

                   procedure DessinPolygoneSVG(const QStyle: string; var Sommets: array of TODGPoint2Df; const QClosed, QFilled: boolean);

                   procedure DessinPolyBezier(const QStyle: string; var BezierArcs: array of TODGBezierArc; const QClosed, QFilled: boolean);

                   procedure DessinEllipse(const QStyle: string; const x1, y1, x2, y2: TScalaire); overload;
                   procedure DessinEllipse(const QStyle: string; const ARect: TODGRect2Df); overload;
                   procedure DessinEllipseC(const QStyle: string; const x, y: TScalaire; const rx, ry: TScalaire);
                   procedure DessinTexte(const QStyle: string; const X, Y: TScalaire; const QAlignment: byte; const QText: String);
                   procedure InsertImage(const QLayer: string; const QStyle: string; const X, Y, L, H: TScalaire; const FichierImage: String);

                   // couleur html
                   function QGetCouleurHTML(const C: TColor): string; inline;


                 end;

implementation
{$IFNDEF FOR_GHCAVEDRAW}
  uses odgtest; // pour AfficherMessage
{$ENDIF}
CONST OPEN_DOCUMENT_VERSION = '1.2';
CONST DOSSIER_IMAGES    = 'Pictures/';
CONST FILE_MIME_TYPE    = 'mimetype';
CONST XML_FILE_MANIFEST = 'manifest.xml';
CONST XML_FILE_CONTENT  = 'content.xml';
CONST XML_FILE_STYLES   = 'styles.xml';
CONST XML_FILE_SETTINGS = 'settings.xml';
const XML_FILE_META     = 'meta.xml';
const XML_FILE_CONFIGURATIONS2_ACCELERATOR_CURRENT = 'current.xml';
//CONST XML_FILE_CONTENT_NEW  = 'content666.xml';

const STYLE_LAYOUT_PAGE_PRINCIPALE  = 'StylePagePrincipale';
const STYLE_DRAWING_PAGE_PRINCIPALE = 'StyleDessinPagePrincipale';



const BIGMAX = 2000000000;
//const NOM_LAYER_DEFAUT = 'layout';
//const FMT_CMM = '%d'; // valeurs en mm
//const FMT_2V_CMM = ' %d, %d';
//const FMT_4V_CMM = '%d %d %d %d';
const FMT_CMM = '%.0f'; // valeurs en mm
const FMT_2V_CMM = ' %.0f, %.0f';
const FMT_4V_CMM = '%.0f %.0f %.0f %.0f';

//******************************************************************************
const
  //                         'application/vnd.oasis.opendocument.graphics', which differs from the mediatype of the root document
  //                         'application/vnd.oasis.opendocument.graphics'!
  SECTION_OOO_MIME_DESC    = 'application/vnd.oasis.opendocument.graphics';
  SECTION_OOO_MANIFEST     = 'manifest:manifest';

  SECTION_OOO_TYPEDOCUMENT    = 'office:document-content';
  SECTION_OOO_AUTO_STYLES     = 'office:automatic-styles';
  SECTION_OOO_FONT_FACE_DECLS = 'office:font-face-decls';
  SECTION_OOO_BODY            = 'office:body';
  SECTION_OOO_DRAWING         = 'office:drawing';
  SECTION_OOO_PAGE            = 'draw:page';

const
  CONFIG_ITEM    = 'config:config-item';
  CONFIG_NAME    = 'config:name';
  CONFIG_TYPE    = 'config:type';
  USER_CONFIG_STANDARD = '$(user)/config/standard';

// écriture sécurisée en UTF8 (remplace WriteLn()
procedure WriteLnUTF8(var fp: TPointeurFichier; const S: string);
begin
   Write(fp,  AnsiToUtf8(S + #10));
end;

// conversion sécurisée de fichiers en UTF8
procedure ConvertirFichierEnUTF8(const Fichier: TStringDirectoryFileName);
var
  fp: TextFile;
  LS: TStringList;
  EWE: String;
  i: Integer;
begin
  exit;
  AfficherMessage('ConvertirFichierEnUTF8: ' + Fichier);
  LS := TStringList.Create;
  try
    LS.LoadFromFile(Fichier);
    if (LS.Count = 0) then Exit;
    AssignFile(fp, Fichier);
    try
      ReWrite(fp);
      for i := 0 to LS.Count - 1 do
      begin
        EWE := ls.Strings[i];
        Write(fp, EWE + #10);
        if (Pos('<?xml', EWE) > 0) then Write(fp, '<!-- Le Christ doit être égorgé ( leurre pour forcer en UTF8 ) -->' + #10);
      end;
    finally
      Closefile(fp);
    end;
  finally
    FreeAndNil(LS);//LS.Free;
  end;
end;

// destruction sécurisée des fichiers XML
procedure EcraserFichiersXML(const FichierXML: string);
begin
  AfficherMessage('Deleting ' + FichierXML);
  if (FileExists(FichierXML)) then DeleteFile(PChar(FichierXML));
end;

procedure WriteXMLHeader(var fp: TPointeurFichier; const Version, Encoding: string);
begin
  WriteLnUTF8(fp, Format('<?xml version="%s" encoding="%s"?>', [Version, Encoding]));
end;
procedure BeginSection(var fp: TPointeurFichier; const Niveau: integer; const SectionName: string); overload;
begin
  WriteLnUTF8(fp, StringOfChar(' ', Niveau * 2) + Format('<%s>', [SectionName]));
end;
procedure BeginSection(var fp: TPointeurFichier; const Niveau: integer; const SectionName, Parameters: string); overload;
begin
  WriteLnUTF8(fp, StringOfChar(' ', Niveau * 2) + Format('<%s %s>', [SectionName, Parameters]));
end;
procedure EndSection(var fp: TPointeurFichier; const Niveau: integer; const SectionName: string);
begin
  WriteLnUTF8(fp, StringOfChar(' ', Niveau * 2) + Format('</%s>', [SectionName]));
end;
procedure WriteItem(var fp: TPointeurFichier; const Niveau: integer; const NomItem, Attributs: string); inline;
begin
  WriteLnUTF8(fp, StringOfChar(' ', Niveau * 2) + Format('<%s %s/>', [NomItem, Attributs]));
end;
// section contenant une seule valeur de la forme:
// <config:config-item config:name="IsPrintBooklet" config:type="boolean">false<config:config-item>
procedure WriteCondensedSectionBool(var fp: TPointeurFichier; const Niveau: integer; const QCle: string; const Valeur: Boolean);
var
  toto: string;
begin
  if (Valeur) then toto := 'true' else toto := 'false';
  WriteLnUTF8(fp, StringOfChar(' ', Niveau * 2) + Format('<%s %s="%s" %s="%s">%s</%s>', [CONFIG_ITEM, CONFIG_NAME, QCle, CONFIG_TYPE, 'boolean', toto, CONFIG_ITEM]));
end;
procedure WriteCondensedSectionInt(var fp: TPointeurFichier; const Niveau: integer; const QCle: string; const Valeur: integer);
begin
  WriteLnUTF8(fp, StringOfChar(' ', Niveau * 2) + Format('<%s %s="%s" %s="%s">%d</%s>', [CONFIG_ITEM, CONFIG_NAME, QCle, CONFIG_TYPE, 'int', Valeur, CONFIG_ITEM]));
end;
procedure WriteCondensedSectionShort(var fp: TPointeurFichier; const Niveau: integer; const QCle: string; const Valeur: integer);
begin
  WriteLnUTF8(fp, StringOfChar(' ', Niveau * 2) + Format('<%s %s="%s" %s="%s">%d</%s>', [CONFIG_ITEM, CONFIG_NAME, QCle, CONFIG_TYPE, 'short', Valeur, CONFIG_ITEM]));
end;
procedure WriteCondensedSectionString(var fp: TPointeurFichier; const Niveau: integer; const QCle: string; const Valeur: string);
begin
  WriteLnUTF8(fp, StringOfChar(' ', Niveau * 2) + Format('<%s %s="%s" %s="%s">%s</%s>', [CONFIG_ITEM, CONFIG_NAME, QCle, CONFIG_TYPE, 'string', Valeur, CONFIG_ITEM]));
end;
procedure WriteCondensedSection(var fp: TPointeurFichier; const Niveau: integer; const QCle: string; const Valeur: string); overload;
begin
  WriteLnUTF8(fp, StringOfChar(' ', Niveau * 2) + Format('<%s>%s</%s>', [QCle, Valeur, QCle]));
end;
procedure WriteCondensedSection(var fp: TPointeurFichier; const Niveau: integer; const QCle: string; const QAttrs: string; const Valeur: string); overload;
begin
  WriteLnUTF8(fp, StringOfChar(' ', Niveau * 2) + Format('<%s %s>%s</%s>', [QCle, QAttrs, Valeur, QCle]));
end;
//###########################################################################
// Constructor
constructor Todg.create;
var dir: array [0..256] of Char;
Begin
  // Creates
  inherited create;
  // liste de styles
  FListeStylesLignesEtPolygones := TStringList.Create;
  FListeStylesTextes            := TStringList.Create;
  FListeDesPhotos               := TStringList.Create;

  FListeStylesLignesEtPolygones.Clear;
  FListeStylesTextes.Clear;
  FListeDesPhotos.Clear;
  // init some variables
  FPenPos.x:=0;
  FPenPos.y:=0;
  FWidth:=0;
  FHeight:=0;
  // Nb objets
  FNbPolybeziers := 0;
  FNbPolygones := 0;

  FNbCourbesBeziers   := 0;
  FNbPolygones        := 0;
  FNbTextes           := 0;
  FNbPhotos           := 0;
  // Styles
  FCurrentStyleName := 'standard';
  TEMPDIR := GetGHCaveDrawDirectory();
end;


//###########################################################################
// Destructor

destructor Todg.destroy;
var i:integer;
Begin
  try
    FListeStylesLignesEtPolygones.Clear;
    FListeStylesTextes.Clear;
    FListeDesPhotos.Clear;
  finally
    FreeAndNil(FListeStylesLignesEtPolygones);//FListeStylesLignesEtPolygones.Free;
    FreeAndNil(FListeStylesTextes);//FListeStylesTextes.Free;
    FreeAndNil(FListeDesPhotos);//FListeDesPhotos.Clear;
  end;
  inherited;
end;
//###########################################################################
// Clear

Procedure Todg.Clear;
var i:integer;
Begin
  FPenPos.x:=0;
  FPenPos.y:=0;
  FWidth:=0;
  FHeight:=0;
end;


// #####################################################################
// Set a Pixel (makes a strange work-around ;), but it works :P)

procedure Todg.SetPixel(X,Y: Integer; Value: TColor);
Begin
  //Brush.Color:=Value;
  //Pen.Style:=psClear;
  //OOoDrwRectangle(x,y,x+1,y+1);
end;

procedure Todg.AddAttribute(var AllAttributes: string; const Cle, Valeur: string; const DoCRLF: boolean);
var
  QDoCrLf: String;
begin
  if (DoCRLF) then QDoCrLf := #13#10 else QDoCrLf :='';
  AllAttributes := AllAttributes + Format(' %s="%s"%s', [Cle, Valeur, QDoCrLf]);
end;


function Todg.GetAStyleLigne(const Idx: integer): string;
begin
  Result := FListeStylesLignesEtPolygones.Strings[Idx];
end;

function Todg.GetAStyleTexte(const Idx: integer): string;
begin
  Result := FListeStylesTextes.Strings[Idx];
end;
function Todg.GetAPhoto(const Idx: integer): string;
begin
  Result := FListeDesPhotos.Strings[Idx];
end;


function Todg.GetNbStylesLignes: integer;
begin
  Result := FListeStylesLignesEtPolygones.Count;
end;

function Todg.GetNbStylesTextes: integer;
begin
  Result := FListeStylesTextes.Count;
end;
function Todg.GetNbPhotos: integer;
begin
  Result := FListeDesPhotos.Count;
end;

(* fontes usuelles
<office:font-face-decls>
<style:font-face style:name="Arial" svg:font-family="Arial" style:font-family-generic="roman" style:font-pitch="variable"/>
<style:font-face style:name="Times New Roman" svg:font-family="&apos;Times New Roman&apos;" style:font-family-generic="roman" style:font-pitch="variable"/>
<style:font-face style:name="Mangal" svg:font-family="Mangal" style:font-family-generic="system" style:font-pitch="variable"/>
<style:font-face style:name="Microsoft YaHei" svg:font-family="&apos;Microsoft YaHei&apos;" style:font-family-generic="system" style:font-pitch="variable"/>
<style:font-face style:name="Segoe UI" svg:font-family="&apos;Segoe UI&apos;" style:font-family-generic="system" style:font-pitch="variable"/>
<style:font-face style:name="Tahoma" svg:font-family="Tahoma" style:font-family-generic="system" style:font-pitch="variable"/>
</office:font-face-decls>
//****************************************)


procedure Todg.AddUsuallyStyleGraphic(const QNamestyle: string;
                                      const QDrawStroke: string;
                                      const QSVGStrokeWidth: double;
                                      const QSVGStrokeColor: TColor;
                                      const QSVGStrokeOpacity: integer;
                                      const QDrawFill: string;
                                      const QDrawFillColor: TColor;
                                      const QDrawOpacity: integer);
var
  QNameSubSection: String;
  QNameSubSubSection: String;
  EWE: String;
  MonStyle: String;
  procedure QBeginSection(const SectionName, Parameters: string);  inline;
  begin
    MonStyle += Format(' <%s %s>', [SectionName, Parameters]);
  end;
  procedure QEndSection(const SectionName: string); inline;
  begin
    MonStyle += Format('</%s>', [SectionName]);
  end;
  procedure QWriteItem(const NomItem, Attributs: string); inline;
  begin
    MonStyle += Format(' <%s %s/>', [NomItem, Attributs]);
  end;
begin
  // <style:graphic-properties draw:stroke-dash="Dash_20_1" svg:stroke-width="0.1cm" svg:stroke-color="#800000" draw:marker-start-width="0.35cm" draw:marker-end-width="0.35cm"
  // draw:fill="solid"
  // draw:fill-color="#00ff00"
  // draw:fill-gradient-name="Gradient_20_8" draw:fill-hatch-name="Hatching_20_1" draw:fill-image-name="Bitmape_20_1" draw:opacity="50%" draw:fill-image-width="0cm" draw:fill-image-height="0cm" draw:textarea-vertical-align="middle" draw:shadow-opacity="50%"/>

  //<style:style  style:name="00_Formes1" style:family="graphic"> <style:graphic-properties  draw:stroke="solid" svg:stroke-width="0.500cm" svg:stroke-color="#FF00FF" svg:stroke-opacity="100%" draw:marker-start-width="0.2cm" draw:marker-start-center="false" draw:marker-end-width="0.2cm" draw:marker-end-center="false"
  // draw:fill="solid" draw:fill-color="#00FF00" draw:fill-opacity="50%" draw:textarea-horizontal-align="justify" fo:padding-top="0.125cm" fo:padding-bottom="0.125cm" fo:padding-left="0.25cm" fo:padding-right="0.25cm" draw:shadow="hidden" draw:shadow-offset-x="0.2cm" draw:shadow-offset-y="0.2cm" draw:shadow-color="#808080"></style:graphic-properties></style:style>

  AfficherMessage(Format('%s.AddUsuallyStyleGraphic: %s', [ClassName, QNamestyle]));
  MonStyle := '';
  QNameSubSection := 'style:style';
  EWE := '';
  AddAttribute(EWE, 'style:name', QNamestyle, False);
  AddAttribute(EWE, 'style:family', 'graphic', False);
  QBeginSection(QNameSubSection, EWE);
    EWE := '';
    AddAttribute(EWE, 'draw:stroke', QDrawStroke, False);
    AddAttribute(EWE, 'svg:stroke-width', Format('%.3fcm',[QSVGStrokeWidth]), False);
    AddAttribute(EWE, 'svg:stroke-color', QGetCouleurHTML(QSVGStrokeColor), False);
    AddAttribute(EWE, 'svg:stroke-opacity', Format('%d%%', [QSVGStrokeOpacity]), False);
    AddAttribute(EWE, 'draw:marker-start-width', '0.2cm', False);
    AddAttribute(EWE, 'draw:marker-start-center', 'false', False);
    AddAttribute(EWE, 'draw:marker-end-width', '0.2cm', False);
    AddAttribute(EWE, 'draw:marker-end-center', 'false', False);
    AddAttribute(EWE, 'draw:fill', QDrawFill, False);
    AddAttribute(EWE, 'draw:fill-color', QGetCouleurHTML(QDrawFillColor), False);
    AddAttribute(EWE, 'draw:fill-gradient-name', 'Gradient_20_8', False);
    //AddAttribute(EWE, 'draw:fill-opacity',  Format('%d%%', [QDrawOpacity]), False);
    AddAttribute(EWE, 'draw:textarea-horizontal-align', 'justify', False);
    AddAttribute(EWE, 'fo:padding-top', '0.125cm', False);
    AddAttribute(EWE, 'fo:padding-bottom', '0.125cm', False);
    AddAttribute(EWE, 'fo:padding-left', '0.25cm', False);
    AddAttribute(EWE, 'fo:padding-right', '0.25cm', False);
    AddAttribute(EWE, 'draw:shadow', 'hidden', False);
    AddAttribute(EWE, 'draw:shadow-offset-x', '0.2cm', False);
    AddAttribute(EWE, 'draw:shadow-offset-y', '0.2cm', False);
    AddAttribute(EWE, 'draw:shadow-color', '#808080', False);
    QNameSubSubSection := 'style:graphic-properties';
    QBeginSection(QNameSubSubSection, EWE);
    QEndSection(QNameSubSubSection);
  QEndSection(QNameSubSection);
  FListeStylesLignesEtPolygones.Add(MonStyle);
end;

procedure Todg.AddUsuallyStyleText(const QNamestyle: string;
                                   const QBackColor: TColor;
                                   const QFontName: string;
                                   const QFontColor: TColor;
                                   const QFontStyle: TFontStyles;
                                   const QFontSize: double;
                                   const QAlignement: integer);
// L'alignement est défini de la même manière que dans GHCaveDraw
// 7    8    9
// 1    2    3
// 4    5    6
//      0 = défaut
const
  DRAW_TEXTAREA_VERTICAL_ALIGN   = 'draw:textarea-vertical-align';
  DRAW_TEXTAREA_HORIZONTAL_ALIGN = 'draw:textarea-horizontal-align';
  DRAW_AUTO_GROW_HEIGHT          = 'draw:auto-grow-height';
  DRAW_AUTO_GROW_WIDTH           = 'draw:auto-grow-width';

var
  MyStyle : string;
  QNameSubSection: String;
  QNameSubSubSection: String;
  EWE: String;
  procedure QBeginSection(const SectionName, Parameters: string); inline;
  begin
    MyStyle += Format(' <%s %s>', [SectionName, Parameters]);
  end;
  procedure QEndSection(const SectionName: string); inline;
  begin
    MyStyle += Format('</%s>', [SectionName]);
  end;
  procedure QWriteItem(const Niveau: integer; const NomItem, Attributs: string); inline;
  begin
    MyStyle += Format(' <%s %s/>', [NomItem, Attributs]);
  end;
begin
  AfficherMessage(Format('%s.AddUsuallyStyleTexte: %s', [ClassName, QNamestyle]));
  MyStyle := '';
  QNameSubSection := 'style:style';
  EWE := '';
  AddAttribute(EWE, 'style:name',  QNamestyle, False);    // nom interne
  AddAttribute(EWE, 'style:display-name', QNamestyle, False);    // nom affiché
  AddAttribute(EWE, 'style:family', 'graphic', False);
  AddAttribute(EWE, 'style:parent-style-name', 'standard', False);
  QBeginSection(QNameSubSection, EWE);
    // cadre du texte = rectangle qui le contient
    EWE := '';
    AddAttribute(EWE, 'draw:stroke', 'none', False);
    //AddAttribute(EWE, 'draw:stroke-dash', 'Dash_20_1', False);
    AddAttribute(EWE, 'svg:stroke-color', QGetCouleurHTML(QBackColor), False);
    AddAttribute(EWE, 'draw:fill', 'none', False);
    AddAttribute(EWE, 'draw:fill-color', QGetCouleurHTML(QBackColor), False);
    // alignement du texte selon méthode GHCaveDraw
    // si alignement hors de ces limites, ne rien faire
    if (QAlignement in [1 .. 9]) then
    begin
      AddAttribute(EWE, DRAW_AUTO_GROW_HEIGHT   , 'false', False);
      AddAttribute(EWE, DRAW_AUTO_GROW_WIDTH    , 'false', False);
      case QAlignement of
        7, 8, 9: AddAttribute(EWE, DRAW_TEXTAREA_VERTICAL_ALIGN, 'top', false);
        4, 5, 6: AddAttribute(EWE, DRAW_TEXTAREA_VERTICAL_ALIGN, 'center', false);
        1, 2, 3: AddAttribute(EWE, DRAW_TEXTAREA_VERTICAL_ALIGN, 'bottom', false);
      end;
      case QAlignement of
        1, 4, 7: AddAttribute(EWE, DRAW_TEXTAREA_HORIZONTAL_ALIGN, 'left', false);
        2, 5, 8: AddAttribute(EWE, DRAW_TEXTAREA_HORIZONTAL_ALIGN, 'center', false);
        3, 6, 9: AddAttribute(EWE, DRAW_TEXTAREA_HORIZONTAL_ALIGN, 'right', false);
      end;
    end
    else
    begin
      AddAttribute(EWE, DRAW_TEXTAREA_HORIZONTAL_ALIGN , 'left', False);
      AddAttribute(EWE, DRAW_AUTO_GROW_HEIGHT          , 'true', False);
      AddAttribute(EWE, DRAW_AUTO_GROW_WIDTH           , 'true', False);
    end;

    AddAttribute(EWE, 'fo:min-height', '0cm', False);
    AddAttribute(EWE, 'fo:min-width', '0cm', False);
    QNameSubSubSection := 'style:graphic-properties';
    QBeginSection(QNameSubSubSection, EWE);
    QEndSection(QNameSubSubSection);
    // le texte lui-même
    EWE := '';
    AddAttribute(EWE, 'fo:color', QGetCouleurHTML(QFontColor), False);
    AddAttribute(EWE, 'fo:font-family', QFontName, False);
    AddAttribute(EWE, 'style:font-family-generic ', 'swiss', False);
    AddAttribute(EWE, 'style:font-pitch', 'variable', false);
    AddAttribute(EWE, 'fo:font-size', Format('%.0fpt', [QFontSize]), false);   // fo:font-size="24pt"
    if (fsBold      in QFontStyle) then
    begin
      AddAttribute(EWE, 'fo:font-weight', 'bold', False);
      AddAttribute(EWE, 'style:font-style-name', 'Gras', False);
    end;
    if (fsUnderline in QFontStyle) then
    begin
      AddAttribute(EWE, 'style:text-underline-style', 'solid', False); //style:text-underline-style
      AddAttribute(EWE, 'style:text-underline-type' , 'single', false);
      AddAttribute(EWE, 'style:text-underline-width', 'auto'  , false);
      AddAttribute(EWE, 'style:text-underline-color', 'font-color', false);
    end;
  QNameSubSubSection := 'style:text-properties';
    QBeginSection(QNameSubSubSection, EWE);
    QEndSection(QNameSubSubSection);
  QEndSection(QNameSubSection);
  FListeStylesTextes.Add(MyStyle);
  //*)
end;


procedure Todg.RegenererFichierSettings(const FichierXML: string);
var
  fp: TPointeurFichier;
  NameMainSection: String;
  EWE: String;
  NameSectionLevel1: String;
  NameSectionLevel2: String;
  NameSectionLevel3: String;
  NameSectionLevel4: String;
begin
  AfficherMessage(Format('%s.RegenererFichierStyles: %s', [ClassName, FichierXML]));
  AssignFile(fp, FichierXML);
  try
    Rewrite(fp);
    WriteXMLHeader(fp, '1.0', 'UTF-8');
    NameMainSection := 'office:document-settings';
    EWE := '';
    AddAttribute(EWE, 'xmlns:office'        , 'urn:oasis:names:tc:opendocument:xmlns:office:1.0', True);
    AddAttribute(EWE, 'xmlns:xlink'         , 'http://www.w3.org/1999/xlink', True);
    AddAttribute(EWE, 'xmlns:presentation'  , 'urn:oasis:names:tc:opendocument:xmlns:presentation:1.0', True);
    AddAttribute(EWE, 'xmlns:config'        , 'urn:oasis:names:tc:opendocument:xmlns:config:1.0', True);
    AddAttribute(EWE, 'xmlns:ooo'           , 'http://openoffice.org/2004/office', True);
    AddAttribute(EWE, 'xmlns:smil'          , 'urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0', True);
    AddAttribute(EWE, 'xmlns:anim'          , 'urn:oasis:names:tc:opendocument:xmlns:animation:1.0', True);
    AddAttribute(EWE, 'xmlns:officeooo'     , 'http://openoffice.org/2009/office', True);
    AddAttribute(EWE, 'xmlns:drawooo'       , 'http://openoffice.org/2010/draw', True);
    AddAttribute(EWE, 'office:version'    , OPEN_DOCUMENT_VERSION, False);
    BeginSection(fp, 0, NameMainSection, EWE);
      NameSectionLevel1 := 'office:settings';
      BeginSection(fp, 1, NameSectionLevel1);
        EWE := '';
        AddAttribute(EWE, 'config:name', 'ooo:view-settings', false);
        NameSectionLevel2:= 'config:config-item-set';
        BeginSection(fp, 2, NameSectionLevel2, EWE);
          WriteCondensedSectionBool(fp, 3, 'GridIsFront', False);
          WriteCondensedSectionInt(fp, 3, 'VisibleAreaTop'     , -648);
          WriteCondensedSectionInt(fp, 3, 'VisibleAreaLeft'    , -8531);
          WriteCondensedSectionInt(fp, 3, 'VisibleAreaHeight'  , 43629);
          WriteCondensedSectionInt(fp, 3, 'VisibleAreaWidth'   , 77161);
          EWE := '';
          AddAttribute(EWE, 'config:name', 'Views', False);
          NameSectionLevel3 := 'config:config-item-map-indexed';
          BeginSection(fp, 3, NameSectionLevel3, EWE);
            NameSectionLevel4 := 'config:config-item-map-entry';
            BeginSection(fp, 4, NameSectionLevel4);
              WriteCondensedSectionString(fp, 5, 'ViewId', 'view1');
              WriteCondensedSectionBool(fp, 5, 'GridIsVisible', false);
              WriteCondensedSectionBool(fp, 5, 'GridIsFront', false);
              WriteCondensedSectionBool(fp, 5, 'IsSnapToGrid', true);
              WriteCondensedSectionBool(fp, 5, 'IsSnapToPageMargins', true);
              WriteCondensedSectionBool(fp, 5, 'IsSnapToSnapLines', false);
              WriteCondensedSectionBool(fp, 5, 'IsSnapToObjectFrame', false);
              WriteCondensedSectionBool(fp, 5, 'IsSnapToObjectPoints', false);
              WriteCondensedSectionBool(fp, 5, 'IsPlusHandlesAlwaysVisible', false);
              WriteCondensedSectionBool(fp, 5, 'IsFrameDragSingles', true);
              WriteCondensedSectionInt(fp, 5, 'EliminatePolyPointLimitAngle', 1500);
              WriteCondensedSectionBool(fp, 5, 'IsEliminatePolyPoints', false);
              WriteCondensedSectionBool(fp, 5, 'NoAttribs', false);
              WriteCondensedSectionBool(fp, 5, 'NoColors', true);
              WriteCondensedSectionBool(fp, 5, 'RulerIsVisible', true);
              WriteCondensedSectionShort(fp, 5, 'PageKind', 0);
              WriteCondensedSectionShort(fp, 5, 'SelectedPage', 0);
              WriteCondensedSectionBool(fp, 5, 'IsLayerMode', true);
              WriteCondensedSectionBool(fp, 5, 'IsDoubleClickTextEdit', true);
              WriteCondensedSectionBool(fp, 5, 'IsClickChangeRotation', false);
              WriteCondensedSectionShort(fp, 5, 'SlidesPerRow', 4);
              WriteCondensedSectionInt(fp, 5, 'EditModeStandard', 0);
              WriteCondensedSectionInt(fp, 5, 'EditModeNotes', 0);
              WriteCondensedSectionInt(fp, 5, 'EditModeHandout', 1);
              WriteCondensedSectionInt(fp, 5, 'VisibleAreaTop', -648);
              WriteCondensedSectionInt(fp, 5, 'VisibleAreaLeft', -8531);
              WriteCondensedSectionInt(fp, 5, 'VisibleAreaWidth', 77162);
              WriteCondensedSectionInt(fp, 5, 'VisibleAreaHeight', 43630);
              WriteCondensedSectionInt(fp, 5, 'GridCoarseWidth', 1000);
              WriteCondensedSectionInt(fp, 5, 'GridCoarseHeight', 1000);
              WriteCondensedSectionInt(fp, 5, 'GridFineWidth', 100);
              WriteCondensedSectionInt(fp, 5, 'GridFineHeight', 100);
              WriteCondensedSectionInt(fp, 5, 'GridSnapWidthXNumerator', 100);
              WriteCondensedSectionInt(fp, 5, 'GridSnapWidthXDenominator', 1);
              WriteCondensedSectionInt(fp, 5, 'GridSnapWidthYNumerator', 100);
              WriteCondensedSectionInt(fp, 5, 'GridSnapWidthYDenominator', 1);
              WriteCondensedSectionBool(fp, 5, 'IsAngleSnapEnabled', false);
              WriteCondensedSectionInt(fp, 5, 'SnapAngle', 1500);
              WriteCondensedSectionBool(fp, 5, 'ZoomOnPage', true);
            EndSection(fp, 4, NameSectionLevel4);
          EndSection(fp, 3, NameSectionLevel3);
        EndSection(fp, 2, NameSectionLevel2);
        NameSectionLevel2 := 'config:config-item-set';
        EWE := '';
        AddAttribute(EWE, 'config:name', 'ooo:configuration-settings', false);
        BeginSection(fp, 2, NameSectionLevel2, EWE);
          WriteCondensedSectionBool(fp, 3, 'ApplyUserData', true);
          WriteCondensedSectionString(fp, 3, 'BitmapTableURL', USER_CONFIG_STANDARD + '.sob');
          WriteCondensedSectionShort(fp, 3, 'CharacterCompressionType', 0);
          WriteCondensedSectionString(fp, 3, 'ColorTableURL', USER_CONFIG_STANDARD + '.soc');
          WriteCondensedSectionString(fp, 3, 'DashTableURL', USER_CONFIG_STANDARD + '.sod');
          WriteCondensedSectionInt(fp, 3, 'DefaultTabStop', 1250);
          WriteCondensedSectionString(fp, 3, 'GradientTableURL', USER_CONFIG_STANDARD + '.sog');
          WriteCondensedSectionString(fp, 3, 'HatchTableURL', USER_CONFIG_STANDARD + '.soh');
          WriteCondensedSectionBool(fp, 3, 'IsKernAsianPunctuation', false);
          WriteCondensedSectionBool(fp, 3, 'IsPrintBooklet', false);
          WriteCondensedSectionBool(fp, 3, 'IsPrintBookletBack', true);
          WriteCondensedSectionBool(fp, 3, 'IsPrintBookletFront', true);
          WriteCondensedSectionBool(fp, 3, 'IsPrintDate', false);
          WriteCondensedSectionBool(fp, 3, 'IsPrintFitPage', false);
          WriteCondensedSectionBool(fp, 3, 'IsPrintHiddenPages', true);
          WriteCondensedSectionBool(fp, 3, 'IsPrintTime', false);
          WriteCondensedSectionString(fp, 3, 'LineEndTableURL', USER_CONFIG_STANDARD + '.soe');
          WriteCondensedSectionBool(fp, 3, 'LoadReadonly', false);
          WriteCondensedSectionShort(fp, 3, 'MeasureUnit', 3);
          WriteCondensedSectionInt(fp, 3, 'PageNumberFormat', 4);
          WriteCondensedSectionBool(fp, 3, 'ParagraphSummation', false);
          WriteCondensedSectionInt(fp, 3, 'PrintQuality', 0);
          WriteCondensedSectionString(fp, 3, 'PrinterIndependentLayout', 'low-resolution');
          WriteCondensedSectionBool(fp, 3, 'SaveVersionOnClose', false);
          WriteCondensedSectionInt(fp, 3, 'ScaleDenominator', 1);
          WriteCondensedSectionInt(fp, 3, 'ScaleNumerator', 1);
          WriteCondensedSectionBool(fp, 3, 'UpdateFromTemplate', true);
        EndSection(fp, 2, NameSectionLevel2);
      EndSection(fp, 1, NameSectionLevel1);
    EndSection(fp, 0, NameMainSection);
  finally
    closefile(fp);
  end;
end;

procedure Todg.RegenererFichierMeta(const FichierXML: string);
var
  fp: TPointeurFichier;
  NameMainSection: String;
  EWE: String;
  NameSectionLevel1: String;
  NameSectionLevel2: String;
  NameSectionLevel3: String;
  NameSectionLevel4: String;
  YYYY, MM, DD, HH, MN, SS, MS: word;
begin
  AfficherMessage(Format('%s.RegenererFichierMeta: %s', [ClassName, FichierXML]));
  AssignFile(fp, FichierXML);
  try
    Rewrite(fp);
    //*****************************
    // sections de header
    //*****************************
    WriteXMLHeader(fp, '1.0', 'UTF-8');
    NameMainSection := 'office:document-meta';
    EWE := '';
    //xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
    AddAttribute(EWE, 'xmlns:office'       , 'urn:oasis:names:tc:opendocument:xmlns:office:1.0', True);
    AddAttribute(EWE, 'xmlns:xlink'        , 'http://www.w3.org/1999/xlink', True);
    AddAttribute(EWE, 'xmlns:dc'           , 'http://purl.org/dc/elements/1.1/', True);
    AddAttribute(EWE, 'xmlns:meta'         , 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0', True);
    AddAttribute(EWE, 'xmlns:presentation' , 'urn:oasis:names:tc:opendocument:xmlns:presentation:1.0', True);
    AddAttribute(EWE, 'xmlns:ooo'          , 'http://openoffice.org/2004/office', True);
    AddAttribute(EWE, 'xmlns:smil'         , 'urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0', True);
    AddAttribute(EWE, 'xmlns:anim'         , 'urn:oasis:names:tc:opendocument:xmlns:animation:1.0', True);
    AddAttribute(EWE, 'xmlns:grddl'        , 'http://www.w3.org/2003/g/data-view#', True);
    AddAttribute(EWE, 'xmlns:officeooo'    , 'http://openoffice.org/2009/office', True);
    AddAttribute(EWE, 'xmlns:drawooo'      , 'http://openoffice.org/2010/draw', True);
    AddAttribute(EWE, 'office:version'     , OPEN_DOCUMENT_VERSION, False);
    BeginSection(fp, 0, NameMainSection, EWE);
      // section META
      NameSectionLevel1 := 'office:meta';
      BeginSection(fp, 1, NameSectionLevel1);
        DecodeDateTime(Now(), YYYY, MM, DD, HH, MN, SS, MS);
        WriteCondensedSection(fp, 2, 'meta:creation-date', Format('%.4d-%0.2d-%.2dT%.2d:%.2d:%.2d.00', [YYYY, MM, DD, HH, MN, SS]));
        //WriteCondensedSection(fp, 2, 'meta:editing-duration' , 'POD');
        WriteCondensedSection(fp, 2, 'meta:editing-cycles'   ,'1');
        WriteCondensedSection(fp, 2, 'meta:generator'        , 'GHTopo');
        WriteCondensedSection(fp, 2, 'dc:date'           , Format('%.4d-%0.2d-%.2dT%.2d:%.2d:%.2d.00', [YYYY, MM, DD, HH, MN, SS]));
        WriteCondensedSection(fp, 2, 'dc:language'       , 'fr-FR');
        WriteCondensedSection(fp, 2, 'dc:creator'        , 'JP CASSOU');
        //meta:document-statistic meta:object-count="5"/>    // TODO: Nb d'objets à inclure
      EndSection(fp, 1, NameSectionLevel1);
    EndSection(fp, 0, NameMainSection);
  finally
    CloseFile(fp)
  end;
end;

function Todg.InitialiserFichierContent: boolean;
var
  FichierXML: string;
  EWE: String;
  Nb: Integer;
  i: Integer;
begin
  Result := False;
  FichierXML := TEMPDIR + XML_FILE_CONTENT;
  EcraserFichiersXML(FichierXML);
  AfficherMessage(Format('%s.GenererFichierContent: %s', [ClassName, FichierXML]));
  AssignFile(FPointerToFileContent, FichierXML);
  try
    ReWrite(FPointerToFileContent);
    //*****************************
    // sections de header
    //*****************************
    WriteXMLHeader(FPointerToFileContent, '1.0', 'UTF-8');
    // racine des styles
    EWE := '';
    //xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
    AddAttribute(EWE, 'office:version', OPEN_DOCUMENT_VERSION, false);
    AddAttribute(EWE, 'xmlns:fo'     , 'urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0', True);
    AddAttribute(EWE, 'xmlns:svg'    , 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0', True);
    AddAttribute(EWE, 'xmlns:draw'   , 'urn:oasis:names:tc:opendocument:xmlns:drawing:1.0', True);
    AddAttribute(EWE, 'xmlns:text'   , 'urn:oasis:names:tc:opendocument:xmlns:text:1.0', True);
    AddAttribute(EWE, 'xmlns:style'  , 'urn:oasis:names:tc:opendocument:xmlns:style:1.0', True);
    AddAttribute(EWE, 'xmlns:xlink'  , 'http://www.w3.org/1999/xlink', True);
    AddAttribute(EWE, 'xmlns:office' , 'urn:oasis:names:tc:opendocument:xmlns:office:1.0', False);
    BeginSection(FPointerToFileContent, 0, SECTION_OOO_TYPEDOCUMENT, EWE);
    (*

<office:automatic-styles>
< ="dp1" =""/>
</office:automatic-styles>
//*)
    WriteLnUTF8(FPointerToFileContent, '<office:scripts/>'); // ne pas toucher à cette zone
    //fontes
    BeginSection(FPointerToFileContent, 0, SECTION_OOO_FONT_FACE_DECLS);
      EWE := '';
      AddAttribute(EWE, 'style:name'                , 'Arial', False);
      AddAttribute(EWE, 'svg:font-family'           , 'Arial', False);
      AddAttribute(EWE, 'style:font-family-generic' , 'roman', False);
      AddAttribute(EWE, 'style:font-pitch'          , 'variable', False);
      WriteItem(FPointerToFileContent, 1, 'style:font-face', EWE);
      //*********
    EndSection(FPointerToFileContent, 0, SECTION_OOO_FONT_FACE_DECLS);
    // styles par défaut
    BeginSection(FPointerToFileContent, 0, SECTION_OOO_AUTO_STYLES);
      EWE := '';
      AddAttribute(EWE, 'style:name', 'dp1', false);
      AddAttribute(EWE, 'style:family', 'drawing-page', false);
      WriteItem(FPointerToFileContent, 1, 'style:style', EWE);
    EndSection(FPointerToFileContent, 0, SECTION_OOO_AUTO_STYLES);

      BeginSection(FPointerToFileContent, 1, SECTION_OOO_BODY);
        BeginSection(FPointerToFileContent, 2, SECTION_OOO_DRAWING);

    AfficherMessage('-- Initialisation OK');
    Result := True;
  except

  end;
end;
procedure TOdg.OuvrirNouvellePage(const PageName, StylePage, StyleMasterPage: string);
var
  EWE: String;
begin
//  <draw:page ="page1" ="dp1" ="Standard">
  AfficherMessage(Format('%s.NouvellePage: %s - %s, %s', [ClassName, PageName, StylePage, StyleMasterPage]));
  EWE := '';
  AddAttribute(EWE, 'draw:name'             , PageName  , false);
  AddAttribute(EWE, 'draw:style-name'       , StylePage , false);
  AddAttribute(EWE, 'draw:master-page-name' , PageName , false);

  BeginSection(FPointerToFileContent, 3, SECTION_OOO_PAGE, EWE);

end;
procedure TOdg.FermerPage(const PageName: string);
begin
  AfficherMessage(Format('%s.NouvellePage', [ClassName]));
  try
    EndSection(FPointerToFileContent, 3, SECTION_OOO_PAGE);
  except
  end;
end;
procedure TOdg.DebutGroupe(const NomGroupe: string);
var
  EWE: String;
begin
  // <draw:g draw:name="QPolybezier1">
  EWE := '';
  AddAttribute(EWE, 'draw:name', NomGroupe, False);
  BeginSection(FPointerToFileContent, 4, 'draw:g', EWE);
end;
procedure TOdg.FinGroupe(const NomGroupe: string);
begin
  EndSection(FPointerToFileContent, 4, 'draw:g');
end;

procedure TOdg.CloturerFichierContent;
begin
  AfficherMessage(Format('%s.CloturerFichierContent', [ClassName]));
  try
        EndSection(FPointerToFileContent, 2, SECTION_OOO_DRAWING);
      EndSection(FPointerToFileContent, 1, SECTION_OOO_BODY);
    EndSection(FPointerToFileContent, 0, SECTION_OOO_TYPEDOCUMENT);
  finally
    Closefile(FPointerToFileContent);
  end;
end;
procedure Todg.RegenererFichierManifest(const FichierXML: string);
var
  fp: TPointeurFichier;
  EWE: String;
  procedure AjouterManifestFichier(const F666: string);
  begin
    EWE := '';
    AddAttribute(EWE, 'manifest:full-path', F666, false);
    AddAttribute(EWE, 'manifest:media-type', 'text/xml', false);
    writeitem(fp, 1, 'manifest:file-entry', EWE);
  end;
begin
  AfficherMessage(Format('%s.RegenererFichierManifest: %s', [ClassName, FichierXML]));
  AssignFile(fp, FichierXML);
  try
    Rewrite(fp);
    WriteXMLHeader(fp,'1.0', 'UTF-8');
    EWE := '';
    AddAttribute(EWE, 'xmlns:manifest', 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0', false);
    AddAttribute(EWE, 'manifest:version', OPEN_DOCUMENT_VERSION, false);
    BeginSection(fp, 0, SECTION_OOO_MANIFEST, EWE);
      EWE := '';
      AddAttribute(EWE, 'manifest:version'    , OPEN_DOCUMENT_VERSION, false);
      AddAttribute(EWE, 'manifest:full-path'  , '/', false);
      AddAttribute(EWE, 'manifest:media-type' , SECTION_OOO_MIME_DESC, false);
      writeitem(fp, 1, 'manifest:file-entry'  , EWE);
      AjouterManifestFichier('content.xml');
      AjouterManifestFichier('styles.xml');
      AjouterManifestFichier('meta.xml');
      AjouterManifestFichier('settings.xml');


      (*
      EWE := '';
      AddAttribute(EWE, 'manifest:full-path', 'content.xml', false);
      AddAttribute(EWE, 'manifest:media-type', 'text/xml', false);
      writeitem(fp, 1, 'manifest:file-entry', EWE);
      //*)
    EndSection(fp, 0, SECTION_OOO_MANIFEST);
  finally
    CloseFile(fp);
  end;
end;
procedure Todg.RegenererFichierMIME(const FichierMIME: string);
var
  fp: TPointeurFichier;
begin
  AssignFile(fp, FichierMIME);
  try
    rewrite(fp);
    WriteLnUTF8(fp, SECTION_OOO_MIME_DESC);
  finally
    closefile(fp);
  end;
end;

procedure Todg.RegenererFichierStyles(const FichierXML: string);
var
  fp: TPointeurFichier;
  NameSection: String;
  EWE: String;
  NameSubSection: String;
  NameMainSection: String;
  NameSubSubSection: String;
  ns: Integer;
  QMyStyle: String;
  procedure QAddLayer(const QLayer: string);
  begin
    EWE := '';
    AddAttribute(EWE, 'draw:name', QLayer, False);
    WriteItem(fp, 3, 'draw:layer', EWE);
  end;
begin
  AfficherMessage(Format('%s.RegenererFichierStyles: %s', [ClassName, FichierXML]));
  AssignFile(fp, FichierXML);
  try
    Rewrite(fp);
    //*****************************
    // sections de header
    //*****************************
    WriteXMLHeader(fp,'1.0', 'UTF-8');
    // racine des styles
    AfficherMessage('--- Racine des styles 000');
    NameMainSection := 'office:document-styles';
    EWE := '';
    AddAttribute(EWE, 'xmlns:office', 'urn:oasis:names:tc:opendocument:xmlns:office:1.0', True);
    AddAttribute(EWE, 'xmlns:style' , 'urn:oasis:names:tc:opendocument:xmlns:style:1.0', True);
    AddAttribute(EWE, 'xmlns:text'  , 'urn:oasis:names:tc:opendocument:xmlns:text:1.0', True);
    AddAttribute(EWE, 'xmlns:table' , 'urn:oasis:names:tc:opendocument:xmlns:table:1.0', True);
    AddAttribute(EWE, 'xmlns:draw'  , 'urn:oasis:names:tc:opendocument:xmlns:drawing:1.0', True);
    AddAttribute(EWE, 'xmlns:fo'    , 'urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0', True);
    AddAttribute(EWE, 'xmlns:xlink' , 'http://www.w3.org/1999/xlink', True);
    AddAttribute(EWE, 'xmlns:dc'    , 'http://purl.org/dc/elements/1.1/', True);
    AddAttribute(EWE, 'xmlns:meta'  , 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0', True);
    AddAttribute(EWE, 'xmlns:number', 'urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0', True);
    AddAttribute(EWE, 'xmlns:presentation', 'urn:oasis:names:tc:opendocument:xmlns:presentation:1.0', True);
    AddAttribute(EWE, 'xmlns:svg'         , 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0', True);
    AddAttribute(EWE, 'xmlns:chart'       , 'urn:oasis:names:tc:opendocument:xmlns:chart:1.0', True);
    AddAttribute(EWE, 'xmlns:dr3d'        , 'urn:oasis:names:tc:opendocument:xmlns:dr3d:1.', True);
    AddAttribute(EWE, 'xmlns:math'        , 'http://www.w3.org/1998/Math/MathML', True);
    AddAttribute(EWE, 'xmlns:form'        , 'urn:oasis:names:tc:opendocument:xmlns:form:1.0', True);
    AddAttribute(EWE, 'xmlns:script'      , 'urn:oasis:names:tc:opendocument:xmlns:script:1.0', True);
    AddAttribute(EWE, 'xmlns:ooo'         , 'http://openoffice.org/2004/office', True);
    AddAttribute(EWE, 'xmlns:ooow'        , 'http://openoffice.org/2004/writer', True);
    AddAttribute(EWE, 'xmlns:oooc'        , 'http://openoffice.org/2004/calc', True);
    AddAttribute(EWE, 'xmlns:dom'         , 'http://www.w3.org/2001/xml-events', True);
    AddAttribute(EWE, 'xmlns:smil'        , 'urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0', True);
    AddAttribute(EWE, 'xmlns:anim'        , 'urn:oasis:names:tc:opendocument:xmlns:animation:1.0', True);
    AddAttribute(EWE, 'xmlns:rpt'         , 'http://openoffice.org/2005/report', True);
    AddAttribute(EWE, 'xmlns:of'          , 'urn:oasis:names:tc:opendocument:xmlns:of:' + OPEN_DOCUMENT_VERSION, True);
    AddAttribute(EWE, 'xmlns:xhtml'       , 'http://www.w3.org/1999/xhtml', True);
    AddAttribute(EWE, 'xmlns:grddl'       , 'http://www.w3.org/2003/g/data-view#', True);
    AddAttribute(EWE, 'xmlns:officeooo'   , 'http://openoffice.org/2009/office', True);
    AddAttribute(EWE, 'xmlns:tableooo'    , 'http://openoffice.org/2009/table', True);
    AddAttribute(EWE, 'xmlns:drawooo'     , 'http://openoffice.org/2010/draw', True);
    AddAttribute(EWE, 'office:version'    , OPEN_DOCUMENT_VERSION, False);
    //*)
    BeginSection(fp, 0, NameMainSection, EWE);
    //*****************************
    // section des styles
    //*****************************
      NameSection := 'office:styles';
      BeginSection(fp, 1, NameSection , '');
        // styles de lignes
        WriteLnUTF8(fp, '  <!-- Line and polygon styles -->');
        for ns := 0 to GetNbStylesLignes - 1 do
        begin
          QMyStyle := GetAStyleLigne(ns);
          WriteLnUTF8(fp, '      ' + QMyStyle);
        end;
        // styles de textes
        WriteLnUTF8(fp, '  <!-- Text styles -->');
        for ns := 0 to GetNbStylesTextes - 1 do
        begin
          QMyStyle := GetAStyleTexte(ns);
          WriteLnUTF8(fp, '      ' + QMyStyle);
        end;

      EndSection(fp, 1, NameSection);
      //************************************
      // sections de footpage
      //************************************
      // Automatic styles
      // 1: <office:automatic-styles>
      // 2:   <style:page-layout style:name="PM0">
      // 3       <style:page-layout-properties fo:margin-top="1cm" fo:margin-bottom="1cm" fo:margin-left="1cm" fo:margin-right="1cm" fo:page-width="84.1cm" fo:page-height="59.4cm" style:print-orientation="landscape"/>
      // 2     </style:page-layout>
      // 2:   <style:style style:name="Mdp1" style:family="drawing-page">
      // 3:     <style:drawing-page-properties draw:background-size="border" draw:fill="none"/>
      // 2:   </style:style>
      // 1: </office:automatic-styles>
      WriteLnUTF8(fp, '  <!-- Automatic styles -->');
      NameSection := 'office:automatic-styles';
      BeginSection(fp, 1, NameSection , '');
        NameSubSection := 'style:page-layout';
        EWE := '';
        AddAttribute(EWE, 'style:name', STYLE_LAYOUT_PAGE_PRINCIPALE, False);
        BeginSection(fp, 2, NameSubSection, EWE);
          EWE := '';
          AddAttribute(EWE, 'fo:margin-top', '1cm', False);
          AddAttribute(EWE, 'fo:margin-left', '1cm', False);
          AddAttribute(EWE, 'fo:margin-bottom', '1cm', False);
          AddAttribute(EWE, 'fo:margin-right', '1cm', False);
          AddAttribute(EWE, 'fo:page-width', '84.1cm', False);
          AddAttribute(EWE, 'fo:page-height', '59.4cm', False);
          AddAttribute(EWE, 'style:print-orientation', 'landscape', False);
          WriteItem(fp, 3, 'style:page-layout-properties', EWE);
        EndSection(fp, 2, NameSubSection);
        NameSubSection := 'style:style';
        EWE := '';
        AddAttribute(EWE, 'style:name'   , STYLE_DRAWING_PAGE_PRINCIPALE, False);
        AddAttribute(EWE, 'style:family' , 'drawing-page', False);
        BeginSection(fp, 2, NameSubSection, EWE);
          EWE := '';
          AddAttribute(EWE, 'draw:background-size' , 'border', False);
          AddAttribute(EWE, 'draw:fill'            , 'none', False);
          WriteItem(fp, 3, 'style:drawing-page-properties', EWE);
        EndSection(fp, 2, NameSubSection);
      EndSection(fp, 1, NameSection);
      // Master styles
      WriteLnUTF8(fp, '  <!-- Master styles [OK] -->');
      NameSection := 'office:master-styles';
      BeginSection(fp, 1, NameSection , '');
        NameSubSection := 'draw:layer-set';
        BeginSection(fp, 2, NameSubSection , '');
          QAddLayer('layout');
          QAddLayer('backgroundobjects');
          QAddLayer('measurelines');
          QAddLayer('backgroundobjects');
          QAddLayer('controls');
          // couches additionnelles
          QAddLayer('centerlines');       // cheminements
          QAddLayer('photos');            // photos

        EndSection(fp, 2, NameSubSection);
        EWE := '';
        AddAttribute(EWE, 'style:name'             , 'Standard', false);
        AddAttribute(EWE, 'style:page-layout-name' , STYLE_LAYOUT_PAGE_PRINCIPALE, false);
        AddAttribute(EWE, 'draw:style-name'          , STYLE_DRAWING_PAGE_PRINCIPALE, false);
        WriteItem(fp, 1, 'style:master-page', EWE);
      EndSection(fp, 1, NameSection);
    EndSection(fp, 0, NameMainSection);
    AfficherMessage('-- Fichier de styles OK');
  finally
    closefile(fp);
  end;
end;



procedure Todg.DessinEllipse(const QStyle: string; const x1,y1,x2,y2: TScalaire); overload;
var
  EWE: String;
Begin
  checksize(x1,y1);
  checksize(x2,y2);
  EWE := '';
  AddAttribute(EWE, 'draw:style-name', QStyle, false);
  AddAttribute(EWE, 'svg:x', Format(FMT_CMM, [x1]), false);
  AddAttribute(EWE, 'svg:y', Format(FMT_CMM, [y1]), false);
  AddAttribute(EWE, 'svg:width',  Format(FMT_CMM, [x2-x1]), false);
  AddAttribute(EWE, 'svg:height', Format(FMT_CMM, [y2-y1]), false);
  WriteItem(FPointerToFileContent, 5, 'draw:ellipse', EWE);
  DessinMoveTo(x2,y2);
end;

procedure Todg.DessinEllipse(const QStyle: string; const ARect:TODGRect2Df); overload;
Begin
  DessinEllipse(QStyle, ARect.Left,ARect.Top,ARect.Right,ARect.Bottom);
end;

procedure Todg.DessinEllipseC(const QStyle: string; const x,y: TScalaire; const rx,ry: TScalaire);
begin
  DessinEllipse (QStyle, x-rx,y-ry,x+rx,y+ry);
end;

// dessin de rectangle
// Polyligne                : QClosed = False; QFilled = False;
// Polygone fermé non rempli: QClosed = True ; QFilled = False;
// Polygone fermé et rempli : QClosed = True ; QFilled = True;

procedure Todg.DessinRectangle(const QStyle: string; const x1, y1, x2, y2: TScalaire; const QFilled: boolean);
var
  Coins: array[0..3] of TODGPoint2Df;
Begin

  Coins[0].X := x1;  Coins[0].Y := y1;
  Coins[1].X := x2;  Coins[1].Y := y1;
  Coins[2].X := x2;  Coins[2].Y := y2;
  Coins[3].X := x1;  Coins[3].Y := y2;
  //Coins[4] := Coins[0];
  DessinPolygoneSVG(QStyle, Coins, True, QFilled);
end;

procedure Todg.DessinTriangle(const QStyle: string; const x1, y1, x2, y2, x3, y3: TScalaire; const QFilled: boolean);
var
  Vertex: array[0..2] of TODGPoint2Df;
Begin
  Vertex[0].X := x1;  Vertex[0].Y := y1;
  Vertex[1].X := x2;  Vertex[1].Y := y2;
  Vertex[2].X := x3;  Vertex[2].Y := y3;
  DessinPolygoneSVG(QStyle, Vertex, True, QFilled);
end;



// dessin de polygones et polylignes
// Polyligne                : QClosed = False; QFilled = False;
// Polygone fermé non rempli: QClosed = True ; QFilled = False;
// Polygone fermé et rempli : QClosed = True ; QFilled = True;
procedure Todg.DessinPolygoneSVG(const QStyle: string; var Sommets: array of TODGPoint2Df; const QClosed, QFilled: boolean);
var
  ViewBoxX, ViewBoxY, ViewBoxL, ViewBoxH: TScalaire;
  BB_X1, BB_Y1, BB_X2, BB_Y2: TScalaire;

  EWE: TODGPoint2Df;
  coords: String;
  i: Integer;
  sname: String;
  WU: String;
  // translater les arcs pour les mettre dans la ViewBox
  procedure TranslaterSommets;
  var
   BBCx1, BBCy1, BBCx2, BBCy2 : TScalaire;
   A: Integer;
   MonPoint: TODGPoint2Df;
   procedure Miou(const qx, qy: TScalaire);
   begin
     BBCx1 := min(qx, BBCx1);
     BBCx2 := max(qx, BBCx2);
     BBCy1 := min(qy, BBCy1);
     BBCy2 := max(qy, BBCy2);
   end;
  begin
   // Etape 1: Calcul des min/max
   BBCx1 := +BIGMAX;
   BBCy1 := +BIGMAX;
   BBCx2 := -BIGMAX;
   BBCy2 := -BIGMAX;
   for A := 0 to High(Sommets) do
   begin
     MonPoint := Sommets[A];
     Miou(MonPoint.X, MonPoint.y);
   end;
   // Etape 2: Paramétrage des viexbox
   ViewBoxX := BBCx1;
   ViewBoxY := BBCy1;
   ViewBoxL := BBCx2 - BBCx1;
   ViewBoxH := BBCy2 - BBCy1;
   // Etape 3: Translation
   for A := 0 to High(Sommets) do
   begin
     MonPoint := Sommets[A];
     MonPoint.X    := MonPoint.X - ViewBoxX;
     MonPoint.Y    := MonPoint.Y - ViewBoxY;
     Sommets[A] := MonPoint;
   end;
  end;
begin
  // Pour dessiner un Polygone, il est nécessaire d'utiliser ViewBox
  // Etapes:
  // 1 - Calculer les paramètres de la viewbox
  // -- Rechercher les maxi et mini des points de controle
  // --> on obtient ViewBoxX, ViewBoxY, ViewBoxL, ViewBoxH
  // 2 - Translater les points avec tx = -ViewBoxX et ty = -ViewBoxY
  // 3 - Tracer la courbe avec svg:x="<ViewBoxX>",
  //                           svg:y="<ViewBoxY>",
  //                           svg:width="<ViewBoxL>",
  //                           svg:height="<ViewBoxH>",
  //                           svg:viewBox="0, 0, <ViewBoxL>, <ViewBoxH>",
  // calcul des translations
  TranslaterSommets;
  // systématiquement faire un M 0 0' avant le tracé d'un stroke
   if (QFilled) then coords := 'M 0 0 ZM '
                else coords := 'M 0 0 M ';
  //coords:='M 0 0 M';
  for i := 0 to High(Sommets) do
  begin
    EWE := Sommets[i];
    coords += Format(FMT_2V_CMM, [EWE.X, EWE.Y]);
  end;
  // polygone fermé ?
  if (QClosed) then coords += 'z';

  // make the entries
  WU := '';
  //AddAttribute(WU, 'draw:name'      , Format('Polygone%d', [FNbPolygones]), false);
  AddAttribute(WU, 'draw:style-name', QStyle, false);
  AddAttribute(WU, 'svg:x'          , Format(FMT_CMM, [ViewBoxX]), false);
  AddAttribute(WU, 'svg:y'          , Format(FMT_CMM, [ViewBoxY]), false);
  AddAttribute(WU, 'svg:width'      , Format(FMT_CMM, [ViewBoxL]), false);
  AddAttribute(WU, 'svg:height'     , Format(FMT_CMM, [ViewBoxH]), false);
  AddAttribute(WU, 'svg:viewBox'    , Format(FMT_4V_CMM, [0.0, 0.0, ViewBoxL, ViewBoxH]), false);
  AddAttribute(WU, 'svg:d', coords, false);
  //WriteItem(FPointerToFileContent, 5, 'draw:path', WU);
  BeginSection(FPointerToFileContent, 5, 'draw:path', WU);
    WriteLnUTF8(FPointerToFileContent, StringOfChar(' ', 6 * 2) + '<text:p/>');
  EndSection(FPointerToFileContent, 5, 'draw:path');
  // move to last point for further painting with lineto
  DessinMoveTo(EWE.x, EWE.y);
end;

procedure Todg.DessinPolyBezier(const QStyle: string; var BezierArcs: array of TODGBezierArc; const QClosed, QFilled: boolean);
const
   FM1 = '%.0f, %.0f ';
 var
   ViewBoxX, ViewBoxY, ViewBoxL, ViewBoxH: TScalaire;
   BB_X1, BB_Y1, BB_X2, BB_Y2: TScalaire;
   i:integer;
   coords: String;
   sname: String;
   EWE: TODGBezierArc;
   WU: String;
   // translater les arcs pour les mettre dans la ViewBox
   procedure TranslaterArcs;
   var
     BBCx1, BBCy1, BBCx2, BBCy2 : TScalaire;
     MonArc: TODGBezierArc;
     A: Integer;
     procedure Miou(const qx, qy: TScalaire);
     begin
       BBCx1 := min(qx, BBCx1);
       BBCx2 := max(qx, BBCx2);
       BBCy1 := min(qy, BBCy1);
       BBCy2 := max(qy, BBCy2);
     end;
   begin
     // Etape 1: Calcul des min/max
     BBCx1 := +BIGMAX;
     BBCy1 := +BIGMAX;
     BBCx2 := -BIGMAX;
     BBCy2 := -BIGMAX;
     for A := 0 to High(BezierArcs) do
     begin
       MonArc := BezierArcs[A];
       Miou(MonArc.P1.X, MonArc.P1.y);
       Miou(MonArc.PC1.X, MonArc.PC1.y);
       Miou(MonArc.PC2.X, MonArc.PC2.y);
       Miou(MonArc.P2.X, MonArc.P2.y);
     end;
     // Etape 2: Paramétrage des viexbox
     ViewBoxX := BBCx1;
     ViewBoxY := BBCy1;
     ViewBoxL := BBCx2 - BBCx1;
     ViewBoxH := BBCy2 - BBCy1;
     // Etape 3: Translation
     for A := 0 to High(BezierArcs) do
     begin
       MonArc := BezierArcs[A];
       MonArc.P1.X    := MonArc.P1.X - ViewBoxX;
       MonArc.P1.Y    := MonArc.P1.Y - ViewBoxY;
       MonArc.PC1.X   := MonArc.PC1.X - ViewBoxX;
       MonArc.PC1.Y   := MonArc.PC1.Y - ViewBoxY;
       MonArc.PC2.X   := MonArc.PC2.X - ViewBoxX;
       MonArc.PC2.Y   := MonArc.PC2.Y - ViewBoxY;
       MonArc.P2.X    := MonArc.P2.X - ViewBoxX;
       MonArc.P2.Y    := MonArc.P2.Y - ViewBoxY;
       BezierArcs[A] := MonArc;
     end;
   end;
   procedure DrawArc2(var QCoords: string; const BA: TODGBezierArc; const Idx: integer);
   begin
     if (Idx = 0) then QCoords += Format('M ' + FM1, [BA.P1.X, BA.P1.Y]);
     QCoords += Format('C ' + FM1, [BA.PC1.X, BA.PC1.Y]);
     QCoords += Format(FM1, [BA.PC2.X, BA.PC2.Y]);
     QCoords += Format(FMT_2V_CMM, [BA.P2.X, BA.P2.Y]);
   end;
Begin
   Inc(FNbCourbesBeziers);
   // Pour dessiner un Polybezier, il est nécessaire d'utiliser ViewBox
   // Etapes:
   // 1 - Calculer les paramètres de la viewbox
   // -- Rechercher les maxi et mini des points de controle
   // --> on obtient ViewBoxX, ViewBoxY, ViewBoxL, ViewBoxH
   // 2 - Translater les points avec tx = -ViewBoxX et ty = -ViewBoxY
   // 3 - Tracer la courbe avec svg:x="<ViewBoxX>",
   //                           svg:y="<ViewBoxY>",
   //                           svg:width="<ViewBoxL>",
   //                           svg:height="<ViewBoxH>",
   //                           svg:viewBox="0, 0, <ViewBoxL>, <ViewBoxH>",
   // calcul des translations
   TranslaterArcs;
   // composition du texte à passer en paramètre de path=""
   if (QFilled) then coords := 'M 0 0 Z '    // tout polygone (ou courbe) rempli est encadré de deux Z en début et en fin de chemin
                else coords := 'M 0 0 ';     // Le Z de tête indique que l'objet est rempli
                                             // Le Z de queue indique qu'il est fermé
                                             // L'objet n'est rempli que s'il y a un Z de queue
   for i := 0 to High(BezierArcs) do
   begin
     EWE := BezierArcs[i];
     DrawArc2(coords, EWE, i);
   end;
   // polygone fermé ?
   if (QClosed) then coords += 'z';
   // début de la chaine de paramètres
   WU := '';
   AddAttribute(WU, 'draw:style-name', QStyle, false);
   AddAttribute(WU, 'draw:name', Format('Polybezier%d', [FNbCourbesBeziers]),false);
   AddAttribute(WU, 'svg:x'          , Format(FMT_CMM, [ViewBoxX]), false);
   AddAttribute(WU, 'svg:y'          , Format(FMT_CMM, [ViewBoxY]), false);
   AddAttribute(WU, 'svg:width'      , Format(FMT_CMM, [ViewBoxL]), false);
   AddAttribute(WU, 'svg:height'     , Format(FMT_CMM, [ViewBoxH]), false);
   AddAttribute(WU, 'svg:viewBox'    , Format(FMT_4V_CMM, [0.0, 0.0, ViewBoxL, ViewBoxH]), false);
   AddAttribute(WU, 'svg:d', coords, false);
   BeginSection(FPointerToFileContent, 5, 'draw:path', WU);
     WriteLnUTF8(FPointerToFileContent, StringOfChar(' ', 6 * 2) + '<text:p/>');
   EndSection(FPointerToFileContent, 5, 'draw:path');
   // move to last point for further painting with lineto
   DessinMoveTo(EWE.P2.x, EWE.P2.y);
end;
procedure Todg.DessinMoveTo(X1,Y1: TScalaire);  overload;
Begin
  FPenPos.x:= x1;
  FPenPos.y:= y1;
end;

procedure Todg.DessinMoveTo(p:TODGPoint2Df);  overload;
Begin
  FPenPos:=p;
end;
procedure Todg.DessinLigne(const QStyle: string; const x1,y1,x2,y2: TScalaire); overload;
var
  EWE: String;
begin
  // if bigger than width and height -> extend them
  checksize(x1,y1);
  checksize(x2,y2);
  // set the parameter
  EWE := '';
  AddAttribute(EWE, 'draw:style-name', QStyle, False);
  AddAttribute(EWE, 'svg:x1'         , Format(FMT_CMM, [x1]), False);
  AddAttribute(EWE, 'svg:y1'         , Format(FMT_CMM, [y1]), False);
  AddAttribute(EWE, 'svg:x2'         , Format(FMT_CMM, [x2]), False);
  AddAttribute(EWE, 'svg:y2'         , Format(FMT_CMM, [y2]), False);
  WriteItem(FPointerToFileContent, 6, 'draw:line', EWE);
  DessinMoveTo(x2,y2);
end;

procedure Todg.DessinLigne(const QStyle: string; const p1,p2: TODGPoint2Df); overload;
Begin
  DessinLigne(QStyle, p1.x,p1.y,p2.x,p2.y);
end;

procedure Todg.DessinLigne(const QStyle: string; const Points: TODGRect2Df); overload;
Begin
  DessinLigne(QStyle, Points.Left,Points.Top,Points.Right,Points.Bottom);
end;

procedure Todg.DessinLigneTo(const QStyle: string; const X1,Y1: TScalaire);
Begin
  DessinLigne(QStyle, FPenPos.x,FPenPos.y, x1, y1);
end;
// QAlignement: 7 8 9
//              4 5 6
//              1 2 3
procedure TOdg.DessinTexte(const QStyle: string; const X,Y: TScalaire; const QAlignment: byte; const QText:String);
var
  x2,y2:TScalaire;
  EWE: String;
begin
  checksize(x,y);
  y2:=y+10;
  x2:=x+10;
  checksize(x2,y2);
  EWE := '';
  AddAttribute(EWE, 'draw:style-name', QStyle, false);
  AddAttribute(EWE, 'svg:x'          , Format(FMT_CMM, [x]), false);
  AddAttribute(EWE, 'svg:y'          , Format(FMT_CMM, [y]), false);
  AddAttribute(EWE, 'svg:width'      , Format(FMT_CMM, [x2-x]), false);
  AddAttribute(EWE, 'svg:height'     , Format(FMT_CMM, [y2-y]), false);
  BeginSection(FPointerToFileContent, 5, 'draw:frame', EWE);
    BeginSection(FPointerToFileContent, 6, 'draw:text-box', EWE);
      EWE := '';
      AddAttribute(EWE, 'text:style-name', QStyle, False);
      WriteCondensedSection(FPointerToFileContent, 7, 'text:p', EWE, QText);
    EndSection(FPointerToFileContent, 6, 'draw:text-box');
  EndSection(FPointerToFileContent, 5, 'draw:frame');
  DessinMoveTo(x2,y2);
end;

function Todg.GetCurrentStylename: string;
begin
  Result := FCurrentStyleName;
end;

function Todg.QGetCouleurHTML(const C: TColor): string; inline;
begin
  Result := Format('#%.2X%.2X%.2X', [Red(C), Green(C), Blue(C)]);
end;

procedure Todg.InsertImage(const QLayer: string; const QStyle: string; const X,Y, L, H: TScalaire; const FichierImage:String);
var
  x2, y2: TScalaire;
  EWE: String;
  ImgWidth: TScalaire;
  ImgHeight: TScalaire;
begin
  AfficherMessage(Format('%s.InsertImage: %s, %s, %.0f, %.0f - %s',[ClassName, QLayer, QStyle, X, Y, FichierImage]));
  if (not FileExists(FichierImage)) then
  begin
    AfficherMessage(' --- Fichier image introuvable');
    Exit;
  end;
  // ajout à la liste des photos
  FListeDesPhotos.Add(FichierImage);
  // L = -1 -> taille automatique des photos
  ImgWidth  := ifthen((L < 0), 16180, L);
  ImgHeight := ifthen((H < 0), 10000, H);
  checksize(x,y);
  x2:=x+L;
  y2:=y+H;
  checksize(x2,y2);
  EWE := '';
  AddAttribute(EWE, 'draw:layer'       , QLayer, false);
  AddAttribute(EWE, 'draw:style-name'  , QStyle, false);
  AddAttribute(EWE, 'svg:x', Format('%.0f', [X]), false);
  AddAttribute(EWE, 'svg:y', Format('%.0f', [Y]), false);
  AddAttribute(EWE, 'svg:width', Format('%.0f', [ImgWidth]), false);
  AddAttribute(EWE, 'svg:height', Format('%.0f', [ImgHeight]), false);
  BeginSection(FPointerToFileContent, 5, 'draw:frame', EWE);
    EWE := '';
    AddAttribute(EWE, 'xlink:href', DOSSIER_IMAGES + FichierImage, false);
    AddAttribute(EWE, 'xlink:type', 'simple', false);
    AddAttribute(EWE, 'xlink:show', 'embed', false);
    AddAttribute(EWE, 'xlink:actuate', 'onLoad', false);

    BeginSection(FPointerToFileContent, 6, 'draw:image', EWE);
      WriteLnUTF8(FPointerToFileContent, StringOfChar(' ', 7 * 2) + '<text:p/>');
    EndSection(FPointerToFileContent, 6, 'draw:image');
  EndSection(FPointerToFileContent, 5, 'draw:frame');
//*)
end;




// #####################################################################
//write odgfile

function Todg.GenererImageBidon(const FichierPNG: string; const IsLandscape: boolean): boolean;
var
  MyPNG: TPortableNetworkGraphic;
  L13: Integer;
  L23: Integer;
begin
  Result := false;
  MyPNG  := TPortableNetworkGraphic.Create;
  try
    try
      if (IsLandscape) then begin
        MyPng.Width := 256;
        myPng.Height:= 181;
      end else begin
        MyPng.Width := 181;
        myPng.Height:= 256;
      end;
      with MyPNG.Canvas do
      begin
        L13 := MyPNG.Width div 3;
        L23 := 2 * L13;
        Brush.Style:= bsSolid;
        Brush.Color:= clBlue;
        Rectangle(0,0, L13, MyPNG.Height);
        Brush.Style:= bsSolid;
        Brush.Color:= clGreen;
        Rectangle(L13, 0, L23, MyPNG.Height);
        Brush.Style:= bsSolid;
        Brush.Color:= clRed;
        Rectangle(L23, 0, MyPNG.Width, MyPNG.Height);
      end;
      MyPNG.SaveToFile(FichierPNG);
      Result := True
    except
    end;
  finally
    FreeAndNil(MyPNG);//MyPNG.Free;
  end;
end;

procedure Todg.GenererODG(const filename:string);
const
  THUMBNAIL_FILE = 'thumbnail.png';
var azip:zipfile;
  fzip:zip_fileinfo;
  i:integer;
  YYYY, MM, DD, HH, MN, SS, MS: Word;
  MyTof: String;
  // subroutine for save a file to the ZIP
  procedure savetozip(fromf,tof:string);
  var results:integer;
     buffer:pointer;
     F:file;
  Begin
   zipOpenNewFileInZip(azip, Pchar(tof), @fzip, NIL,0 ,NIL,0,'file',0,9);
   assignfile(F,fromf);
   reset(F,1);
   getmem(buffer,1024);   // get mem
   results:=0;
   while not EOF(F) do    // copy file
   Begin
     blockread(f,buffer^,1024,results);
     zipWriteInFileInZip(azip, buffer, results);
   end;
   freemem(buffer,1024);   // free mem
   closefile(F);           // close file
   zipCloseFileInZip(azip);
  end;
  procedure RegenererCurrentAccelerator(const FTC : string);
  var
    F666: TPointeurFichier;
  begin
    // '/Configurations2/accelerator/
    AssignFile(f666,FTC);
    try
      rewrite(f666);
    finally
      CloseFile(F666);
    end;
  end;
Begin
  AfficherMessage('-- Ecrasement des anciens fichiers XML');
  // destruction éventuelle des fichiers hors contenu
  EcraserFichiersXML(TEMPDIR + FILE_MIME_TYPE);
  EcraserFichiersXML(TEMPDIR + XML_FILE_CONFIGURATIONS2_ACCELERATOR_CURRENT);

  EcraserFichiersXML(TEMPDIR + XML_FILE_MANIFEST);
  EcraserFichiersXML(TEMPDIR + XML_FILE_STYLES);
  EcraserFichiersXML(TEMPDIR + XML_FILE_SETTINGS);
  EcraserFichiersXML(TEMPDIR + XML_FILE_META);
  EcraserFichiersXML(TEMPDIR + 'thumbnail.png');

  AfficherMessage('-- Reconstruction des fichiers XML');
  RegenererFichierMIME(TEMPDIR + FILE_MIME_TYPE);
  RegenererFichierManifest(TEMPDIR + XML_FILE_MANIFEST);
  RegenererFichierStyles(TEMPDIR + XML_FILE_STYLES);
  RegenererFichierSettings(TEMPDIR + XML_FILE_SETTINGS);
  RegenererFichierMeta(TEMPDIR + XML_FILE_META);
  RegenererCurrentAccelerator(TEMPDIR + XML_FILE_CONFIGURATIONS2_ACCELERATOR_CURRENT);
  // Conversion en UTF8
  ConvertirFichierEnUTF8(TEMPDIR + FILE_MIME_TYPE);
  ConvertirFichierEnUTF8(TEMPDIR + XML_FILE_STYLES);
  ConvertirFichierEnUTF8(TEMPDIR + XML_FILE_MANIFEST);
  ConvertirFichierEnUTF8(TEMPDIR + XML_FILE_CONFIGURATIONS2_ACCELERATOR_CURRENT);
  ConvertirFichierEnUTF8(TEMPDIR + XML_FILE_SETTINGS);
  ConvertirFichierEnUTF8(TEMPDIR + XML_FILE_META);
  ConvertirFichierEnUTF8(TEMPDIR + XML_FILE_CONTENT);
  DecodeDateTime(Now(), YYYY, MM, DD, HH, MN, SS, MS);
  // i doenst care about the dates in the file :P
  fzip.dosDate := 0;
  fzip.tmz_date.tm_year  := YYYY;
  fzip.tmz_date.tm_mon   := MM;
  fzip.tmz_date.tm_mday  := DD;
  fzip.tmz_date.tm_hour  := HH;
  fzip.tmz_date.tm_min   := MN;
  fzip.tmz_date.tm_sec   := SS;


  // ??? how to set to what? seems no problem if 0
  fzip.external_fa:=0;
  fzip.internal_fa:=0;



  //Open the odg file
  azip:=zipopen(PChar(filename),0);
  savetozip(TEMPDIR + FILE_MIME_TYPE, FILE_MIME_TYPE);                       // fichier MIME
  savetozip(TEMPDIR + XML_FILE_STYLES, XML_FILE_STYLES);                     // Styles write to the odg, remove tempfile
  savetozip(TEMPDIR + XML_FILE_MANIFEST, 'META-INF/' + XML_FILE_MANIFEST);   // Manifest write to the odg, remove tempfile
  //savetozip(Thumbnails);
  savetozip(TEMPDIR + XML_FILE_CONFIGURATIONS2_ACCELERATOR_CURRENT,          // current.xml write to the odg, remove tempfile
                      'Configurations2/accelerator/' + XML_FILE_CONFIGURATIONS2_ACCELERATOR_CURRENT);
  savetozip(TEMPDIR + XML_FILE_SETTINGS, XML_FILE_SETTINGS);                 // Settings write to odg
  savetozip(TEMPDIR + XML_FILE_META, XML_FILE_META);                         // Meta write to odg
  savetozip(TEMPDIR + XML_FILE_CONTENT, XML_FILE_CONTENT);
  if (GenererImageBidon(TEMPDIR + THUMBNAIL_FILE, True)) then savetozip(TEMPDIR + THUMBNAIL_FILE, 'Thumbnails/' + THUMBNAIL_FILE);
  // images
  if (GetNbPhotos > 0) then
  begin
    for i := 0 to GetNbPhotos - 1 do
    begin
      MyTof := GetAPhoto(i);
      AfficherMessage('-- Add ' + TEMPDIR + DOSSIER_IMAGES + MyTof);
      savetozip(TEMPDIR + MyTof, DOSSIER_IMAGES + MyTof);
    end;
  end;
  zipclose(azip,'end');
end;

// set width and height to the max used values
procedure Todg.CheckSize(ax,ay: TScalaire);
Begin
  Fwidth := max(Fwidth,  ax);
  FHeight:= max(FHeight, ay);
end;

end.

