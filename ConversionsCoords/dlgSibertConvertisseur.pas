unit dlgSibertConvertisseur;
// Inutilisé dans GHTopo
{$mode delphi}{$H+}


// TODO: Mettre les noms de projections en format EPSG

(*

Ellipsoide="Clarke80" "Clarke 1880" 6378249.2 0.0824832568
Ellipsoide="Hayford 09" "Hayford 1909" 6378388 0.08199189
Ellipsoide="Plessis" "Plessis 1840" 6376523 0.0804334508
Datum="NTF" "Nouvelle Triangulation Française" "Clarke80" -168 -60 320 "C:\Users\cassou\Desktop\Logiciels_JPC\source_convertisseur\gr3df97a.txt"
Datum="ED 50" "European Datum" "Hayford 09" -84 -97 -117
Datum="ATG" "Ancienne triangulation française" "Plessis" 1118 23 66
Projection "Lambert Conique Sécant"="LT 93" "Lambert-93" "WGS84" 700000 6600000 3d 46d30' 44d 49d
Projection "Lambert Conique Tangent"="LT 1" "Lambert I" "NTF" 0.99987734 600000 1200000 2d20'14.025'' 55g
Projection "Lambert Conique Tangent"="LT 2" "Lambert II" "NTF" 0.99987742 600000 200000 2d20'14.025'' 52g
Projection "Lambert Conique Tangent"="LT 3" "Lambert III" "NTF" 0.9998775 600000 3200000 2d20'14.025'' 49g
Projection "Lambert Conique Tangent"="LT 2+" "Lambert II étendu" "NTF" 0.99987742 600000 2200000 2d20'14.025'' 52g
Projection "Lambert Conique Tangent"="LT 4" "Lambert IV" "NTF" 0.99994471 234.358 185861.369 2d20'14.025'' 46.85g
Projection "Lambert Conique Sécant"="LT GC" "Lambert Grand Champ" "NTF" 600000 600000 2d20'14.025'' 47d 45d 49d
Projection "Bonne"="Michelin" "Bonne Michelin" "ATG" 0 5570439.711 2d20'14.025'' 48d51'36''
Projection "Lambert Conique Sécant"="CC42" "Conique Conforme Zone 1" "WGS84" 1700000 1200000 3d 42d 41.25d 42.75d
Projection "Lambert Conique Sécant"="CC43" "Conique Conforme Zone 2" "WGS84" 1700000 2200000 3d 43d 42.25d 43.75d
Projection "Lambert Conique Sécant"="CC44" "Conique Conforme Zone 3" "WGS84" 1700000 3200000 3d 44d 43.25d 44.75d
Projection "Lambert Conique Sécant"="CC45" "Conique Conforme Zone 4" "WGS84" 1700000 4200000 3d 45d 44.25d 45.75d
Projection "Lambert Conique Sécant"="CC46" "Conique Conforme Zone 5" "WGS84" 1700000 5200000 3d 46d 45.25d 46.75d
Projection "Lambert Conique Sécant"="CC47" "Conique Conforme Zone 6" "WGS84" 1700000 6200000 3d 47d 46.25d 47.75d
Projection "Lambert Conique Sécant"="CC48" "Conique Conforme Zone 7" "WGS84" 1700000 7200000 3d 48d 47.25d 48.75d
Projection "Lambert Conique Sécant"="CC49" "Conique Conforme Zone 8" "WGS84" 1700000 8200000 3d 49d 48.25d 49.75d
Projection "Lambert Conique Sécant"="CC50" "Conique Conforme Zone 9" "WGS84" 1700000 9200000 3d 50d 49.25d 50.75d
Projection "Mercator sur sphère"="Mercator" "Mercator" "WGS84" 0 0 0
Multiple "Transverse Mercator Multiple"="UTM" "Universal Transverse Mercator" "WGS84" 0.9996 500000 0 10000000 -177d 6d
Multiple "Transverse Mercator Multiple"="UTM ED50" "Universal Transverse Mercator" "ED 50" 0.9996 500000 0 10000000 -177d 6d



//*)

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls; //, dlgProjection;

type

  { TdlgConvertisseurSibert }

  TdlgConvertisseurSibert = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckSourceParis: TCheckBox;
    CheckDestParis: TCheckBox;
    ComboSource: TComboBox;
    ComboDestination: TComboBox;
    EditXDest: TEdit;
    EditYDest: TEdit;
    EditConvergence: TEdit;
    EditAlteration: TEdit;
    EditXSource: TEdit;
    EditYSource: TEdit;
    EditDescription2: TEdit;
    EditFuseauSource: TEdit;
    EditDescription1: TEdit;
    EditFuseauDest: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    RadioUnite: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboDestinationChange(Sender: TObject);
    procedure ComboSourceChange(Sender: TObject);
    procedure ComboSourceExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    //fSortie : boolean;
    //fSortieSender : TObject;
    procedure ChargementDonnees;
  public
    { public declarations }
  end;

var
  dlgConvertisseurSibert: TdlgConvertisseurSibert;

implementation
uses
  donnee,
  outils;
  //ESB_fichier_ini;

{$R *.lfm}

{ TdlgConvertisseurSibert }

procedure TdlgConvertisseurSibert.Button1Click(Sender: TObject);
begin
  //with TDlgProjections do;
end;

procedure TdlgConvertisseurSibert.Button2Click(Sender: TObject);
var
  SX, SY : string;
  UniteAngle : TUniteAngle;
  NumSource, NumDest : integer;
  Facteur : real;
begin
  case RadioUnite.ItemIndex of
    0 : UniteAngle:=uaDegre;
    1 : UniteAngle:=uaDM;
    2 : UniteAngle:=uaDMS;
    3 : UniteAngle:=uaGrade;
  else
    Raise EConvertError.Create('Unité d''angle inconnue');
  end;
  //if ComboSource.ItemIndex < Donnees.LesDatum.Count then
  if (ComboSource.ItemIndex < Donnees.GetNbDatum) then
  begin
    if CheckSourceParis.Checked then
      NumSource:=1
    else
      NumSource:=0;
  end
  else begin
    try
      NumSource:=StrToInt(EditFuseauSource.Text);
    except
      if EditFuseauSource.Enabled then
      begin
        EditFuseauSource.SetFocus;
        Raise EConvertError.Create('Le numéro de fuseau de départ est incorrecte, exemple 12');
      end
      else
        NumSource:=0;
    end;
  end;
  //if ComboDestination.ItemIndex<Donnees.LesDatum.Count then
  if (ComboDestination.ItemIndex < Donnees.GetNbDatum) then
    if CheckDestParis.Checked then
      NumDest:=1
    else
      NumDest:=0;
  // la fonction de conversion est ici
  Donnees.Conversion(ComboSource.Text, ComboDestination.Text,
                     EditXSource.Text, EditYSource.Text,
                     NumSource, NumDest,
                     SX, SY, UniteAngle);
  //----------------------------------------
  EditXDest.Text:=SX;
  EditYDest.Text:=SY;
  Donnees.Convergence(ComboSource.Text, ComboDestination.Text,
                     EditXSource.Text, EditYSource.Text,
                     NumSource,
                     SX, UniteAngle);
  EditConvergence.Text:=SX;
  Donnees.Alteration(ComboSource.Text, ComboDestination.Text,
                     EditXSource.Text, EditYSource.Text,
                     NumSource,
                     Facteur);
  EditAlteration.Text:=FloatToStr(Facteur);
  if ComboDestination.ItemIndex>=Donnees.LesDatum.Count+Donnees.LesProjection.Count then
    EditFuseauDest.Text:=IntToStr(NumDest);

end;

procedure TdlgConvertisseurSibert.ComboDestinationChange(Sender: TObject);
begin
  EditDescription2.Text:=Donnees.LaDescription(ComboDestination.Items[ComboDestination.ItemIndex]);
  if ComboDestination.ItemIndex<Donnees.LesDatum.Count then
  begin
    Label12.Caption:='&Latitude';
    Label13.Caption:='Lon&gitude';
    CheckDestParis.Enabled:=true;
  end
  else begin
    Label12.Caption:='Easting (m)';
    Label13.Caption:='Northing (m)';
    CheckDestParis.Enabled:=false;
  end;
  EditFuseauDest.Enabled:=
    (ComboDestination.ItemIndex >= Donnees.LesDatum.Count+Donnees.LesProjection.Count);
end;

procedure TdlgConvertisseurSibert.ComboSourceChange(Sender: TObject);
begin
  EditDescription1.Text:=Donnees.LaDescription(ComboSource.Items[ComboSource.ItemIndex]);
  if ComboSource.ItemIndex<Donnees.LesDatum.Count then
  begin
    Label3.Caption:='&Latitude';
    Label3.Caption:='Lon&gitude';
    CheckSourceParis.Enabled:=true;
  end
  else begin
    Label3.Caption:='&Easting (m)';
    Label3.Caption:='&Northing (m)';
    CheckSourceParis.Enabled:=false;
  end;
  EditFuseauSource.Enabled:=
    (ComboSource.ItemIndex>=Donnees.LesDatum.Count+Donnees.LesProjection.Count);
end;

procedure TdlgConvertisseurSibert.ComboSourceExit(Sender: TObject);
begin

end;

procedure TdlgConvertisseurSibert.FormCreate(Sender: TObject);
var
 Flux : TStream;
 Chemin : string;

begin
 //fSortie:=false;
 defaultFormatSettings.DecimalSeparator:='.';
 Donnees:=TDonnees.Create;
 //if FichierIni.AutoSave then
 //begin
 try
   Chemin := ExtractFilePath(ParamStr(0)) +'00_donnees.txt';

   Flux:=TFileStream.Create(Chemin,fmOpenRead);
   try
     Donnees.LoadFromStream(Flux);
   finally
     Flux.Free;
   end;
 except // si on n'a pas réussi à charger donnees.txt,
        // on essaie de charger france.txt
   try
     Chemin:= ExtractFilePath(ParamStr(0)) +'00_france.txt';
     Flux:=TFileStream.Create(Chemin,fmOpenRead);
     try
       Donnees.LoadFromStream(Flux);
     finally
       Flux.Free;
     end;
   except
   end;
 end;
 //end;
 ChargementDonnees();


end;

procedure TdlgConvertisseurSibert.FormDestroy(Sender: TObject);
begin
  Donnees.Free;
end;

procedure TdlgConvertisseurSibert.ChargementDonnees;
var
  ListeProposition : TStringList;
  Separations : integer;
begin
  ListeProposition:=TStringList.Create;
  try
    ListeProposition.Clear;
    Donnees.ListeSystemes(ListeProposition,Separations);
    ComboSource.Items.Assign(ListeProposition);
    ComboSource.ItemIndex:=0;
    ComboSource.Tag:=Separations;
    ComboDestination.Items.Assign(ListeProposition);
    ComboDestination.ItemIndex:=0;
    ComboDestination.Tag:=Separations;
    ComboSourceChange(self);
    ComboDestinationChange(self);

  finally
    ListeProposition.Free;
  end;

end;

end.

