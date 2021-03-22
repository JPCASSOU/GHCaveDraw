unit donnee;
{$mode delphi}{$H+}
// /!\ NE PAS TOUCHER A CES FICHIERS SOUS AUCUN PRETEXTE !!!

interface

uses
  Classes, Contnrs,
  outils;

type

  { TDonnees }

  TDonnees = class
    function GetNbProjection: integer;
    function GetNomDeProjection(const Idx: integer): string;
    function ListerLesSystemes: integer;
    function GetNbDeSystemes: integer;
    function GetNbDatum: integer;

    function GetNbEllipsoide: integer;
    function GetNbMultiple: integer;
  private
    fNomJeuDeDonnees : string;
    fLesEllipsoide : TObjectList;
    fLesDatum : TObjectList;
    fLesProjection : TObjectList;
    fLesMultiple : TObjectList;

    // liste interne des systèmes
    FListeDesNomsDeSystemes: array of string;
    FNbreDeSystemes: integer;



  public
    property NomJeuDonnees: string read fNomJeuDeDonnees write fNomJeuDeDonnees;
    property LesEllipsoide : TObjectList read fLesEllipsoide;
    property LesDatum : TObjectList read fLesDatum;
    property LesProjection : TObjectList read fLesProjection;
    property LesMultiple : TObjectList read fLesMultiple;
    property NbEllipsoide : integer read GetNbEllipsoide;
    property NbDatum : integer read GetNbDatum;
    property NbProjection : integer read GetNbProjection;
    property NbMultiple : integer read GetNbMultiple;
    constructor Create;
    destructor Destroy; override;
    procedure AjoutLigne (Ligne : string);
    procedure LoadLigne (Stream : TStream);
    procedure LoadFromStream (Stream : TStream);
    procedure SaveToStream (Stream : TStream);
    // indique l'élément associé à LeQuel
    function UnEllipsoide(LeQuel : string) : TObject;
    function UnDatum(LeQuel : string) : TObject;
    function UneProjection(LeQuel : string) : TObject;
    function NumeroEllipsoide(LeQuel : string) : integer;
    function NumeroDatum(LeQuel : string) : integer;
    function NumeroProjection(LeQuel : string) : integer;
    function NumeroMultiple(LeQuel : string) : integer;
    // indique si un nom est déjà utilisé
    function Inconnu(Candidat : string) : boolean;
    // retire l'élément demandé, vérifie les dépendances, renvoie False si il y en a
    function RetireEllipsoide(LeQuel : string) : boolean;
    function RetireDatum(LeQuel : string) : boolean;
    // extrait la liste des datums et projections
    procedure ListeSystemes(var LaListe : TStringList; var Separations : integer);
    // Separations indique où se trouvent les limites entre datums,
    // projections simples et projections multiples
    // la 1ère limite est sur les 8 bits de poids faibles
    // la seconde sur les 8 bits suivants

    // retourne la description associée
    function LaDescription(LeQuel : string) : string;

    // réalise une conversion depuis le système Source vers les coordonnées géocentriques (en WGS84) (paramètres chaines)
    function VersGeocentrique(NomSource, XSource, YSource : string;
                IndiceSource : integer; var XGeo, YGeo, ZGeo : real) : boolean;
    // réalise une conversion depuis les coordonnées géocentriques (en WGS84) vers le système Dest (paramètres chaines)
    function DepuisGeocentrique(XGeo, YGeo, ZGeo : real; NomDest : string;
                            var IndiceDest : integer; var XDest, YDest : string;
                                           UniteAngle : TUniteAngle) : boolean;
    // réalise une conversion depuis le système Source vers les coordonnées géocentriques (en WGS84) (paramètres numériques)
    function VersGeocentriqueNum(NomSource : string; XSource, YSource : real;
                IndiceSource : integer; var XGeo, YGeo, ZGeo : real) : boolean;
    // réalise une conversion depuis les coordonnées géocentriques (en WGS84) vers le système Dest (paramètres numériques)
    function DepuisGeocentriqueNum(XGeo, YGeo, ZGeo : real; NomDest : string;
                            var IndiceDest : integer; var XDest, YDest : real) : boolean;


    // réalise une conversion entre les systèmes Source et Dest (paramètres chaines)
    function Conversion(NomSource, NomDest, XSource, YSource : string;
                     IndiceSource : integer; var IndiceDest : integer;
                         var XDest, YDest : string; UniteAngle : TUniteAngle) : boolean;
    // réalise une conversion entre les systèmes Source et Dest (paramètres numériques)
    function ConversionNum(NomSource, NomDest : string; XSource, YSource : real;
                     IndiceSource : integer; var IndiceDest : integer;
                     var XDest, YDest : real) : boolean;
    function Convergence(NomSource, NomDest, XSource, YSource : string;
                         IndiceSource : integer;
                         var Declinaison : string; UniteAngle : TUniteAngle) : boolean;
    function Alteration(NomSource, NomDest, XSource, YSource : string;
                             IndiceSource : integer;
                             var Facteur : real) : boolean;

  end;

var
  Donnees : TDonnees;

const
  aWGS84 : real = 6378137;
  eWGS84 : real = 0.08181919106;
//  bWGS84 : real = aWGS84*sqrt(1-sqr(eWGS84));
  bWGS84 : real = 6335439;


implementation

uses
  SysUtils,
  ellipsoide, datum, projection, multiple, createur, relevement;

constructor TDonnees.Create;
begin
  fLesEllipsoide:=TObjectList.Create;
  fLesDatum:=TObjectList.Create;
  fLesProjection:=TObjectList.Create;
  fLesMultiple:=TObjectList.Create;
  AjoutLigne(Format('Ellipsoide="GRS 80" "GRS 80" %g %g',[aWGS84,eWGS84]));
  AjoutLigne('Datum="WGS84" "World Geodesic System" "GRS 80" 0 0 0');
end;

destructor TDonnees.Destroy;
begin
  LesMultiple.Free;
  LesProjection.Free;
  LesDatum.Free;
  LesEllipsoide.Free;
end;

procedure TDonnees.AjoutLigne (Ligne : string);
var
  Flux : TMemoryStream;
begin
  Flux:=TMemoryStream.Create;
  Flux.Write(Ligne[1],length(Ligne));
  Flux.Position:=0;
  LoadLigne(Flux);
  Flux.Free;
end;

procedure TDonnees.LoadLigne (Stream : TStream);
var
  S : string;
  UnEllipsoide : TEllipsoide;
  UnDatum : TDatum;
  UnProjection : TProjection;
  UnMultiple : TMultiple;
  Inconnu : boolean;
  ParamProj : TStringList;

  procedure VerifieNom(Nom : string);
  // vérifie que le nom n'existe pas déjà, sinon génère une exception
  var
    I : integer;
  begin
    for I:=0 to LesEllipsoide.Count-1 do
      if (LesEllipsoide[I] as TEllipsoide).Nom=Nom then
        Raise EConvertError.CreateFmt('%s est déjà utilisé',[Nom]);
    for I:=0 to LesDatum.Count-1 do
      if (LesDatum[I] as TDatum).Nom=Nom then
        Raise EConvertError.CreateFmt('%s est déjà utilisé',[Nom]);
    for I:=0 to LesProjection.Count-1 do
      if (LesProjection[I] as TProjection).Nom=Nom then
         Raise EConvertError.CreateFmt('%s est déjà utilisé',[Nom]);
    for I:=0 to LesMultiple.Count-1 do
      if (LesMultiple[I] as TMultiple).Nom=Nom then
        Raise EConvertError.CreateFmt('%s est déjà utilisé',[Nom]);
  end;

begin
  ParamProj:=TStringList.Create;
  inconnu:=true;
  S:=DebutLigne(Stream);
  if S='Ellipsoide' then
  begin
    inconnu:=false;
    UnEllipsoide:=TEllipsoide.Create;
    try
      UnEllipsoide.LoadFromStream(Stream);
      VerifieNom(UnEllipsoide.Nom);
      LesEllipsoide.Add(UnEllipsoide);
    except
      UnEllipsoide.Free;
    end;
  end;
  if S='Datum' then
  begin
    inconnu:=false;
    UnDatum:=TDatum.Create;
    try
      UnDatum.LoadFromStream(Stream, self);
      VerifieNom(UnDatum.Nom);
      LesDatum.Add(UnDatum);
    except
      UnDatum.Free;
    end;
  end;
  if pos('Projection',S)=1 then
  begin
    UnProjection:=nil;
    try
      inconnu:=false;
      DetailParam(S,ParamProj);
      if ParamProj.Count<>2 then
      begin
        FinLigne(Stream);
        raise EConvertError.CreateFmt('Définission de projection ''%s'' incorrecte',[S]);
      end;
      if ParamProj[0]<>'Projection' then
      begin
        FinLigne(Stream);
        Raise EConvertError.CreateFmt('Donnée ''%s'' de nature inconnue',[S]);
      end;
      UnProjection:=CreateProjection(ParamProj[1]);
      if UnProjection=nil then
      begin
        FinLigne(Stream);
        Raise EConvertError.CreateFmt('Projection %s inconnue',[S]);
      end;
      UnProjection.LoadFromStream(Stream);
      VerifieNom(UnProjection.Nom);
      LesProjection.Add(UnProjection);
    except
      UnProjection.Free;
    end;
  end;
  if pos('Multiple',S)=1 then
  begin
    UnMultiple:=nil;
    try
      inconnu:=false;
      DetailParam(S,ParamProj);
      if ParamProj.Count<>2 then
      begin
        FinLigne(Stream);
        raise EConvertError.CreateFmt('Définission de Multiple ''%s'' incorrecte',[S]);
      end;
      if ParamProj[0]<>'Multiple' then
      begin
        FinLigne(Stream);
        Raise EConvertError.CreateFmt('Donnée ''%s'' de nature inconnue',[S]);
      end;
      UnMultiple:=CreateMultiple(ParamProj[1]);
      if UnMultiple=nil then
      begin
        FinLigne(Stream);
        Raise EConvertError.CreateFmt('Multiple %s inconnue',[S]);
      end;
      UnMultiple.LoadFromStream(Stream);
      VerifieNom(UnMultiple.Nom);
      LesMultiple.Add(UnMultiple);
    except
      UnMultiple.Free;
    end;
  end;
  if inconnu then
    try
      FinLigne(Stream);
      Raise EConvertError.CreateFmt('Donnée %s de nature inconnue',[S]);
    except
    end;
  ParamProj.Free;
end;

procedure TDonnees.LoadFromStream (Stream : TStream);
begin
  assert( defaultFormatSettings.DecimalSeparator='.');
  Stream.Position:=0;
  while Stream.Position<Stream.Size do
    LoadLigne(Stream);
end;

procedure TDonnees.SaveToStream (Stream : TStream);
var
  I : integer;
begin
  assert( defaultFormatSettings.DecimalSeparator='.');
  // on ne sauve pas le premier ellipsoïde qui est GRS80
  for I:=1 to LesEllipsoide.Count-1 do
    (LesEllipsoide[I] as TEllipsoide).SaveToStream(Stream);
  // on ne sauve pas le premier datum qui WGS84
  for I:=1 to LesDatum.Count-1 do
    (LesDatum[I] as TDatum).SaveToStream(Stream);
  for I:=0 to LesProjection.Count-1 do
    (LesProjection[I] as TProjection).SaveToStream(Stream);
  for I:=0 to LesMultiple.Count-1 do
    (LesMultiple[I] as TMultiple).SaveToStream(Stream);
end;

procedure TDonnees.ListeSystemes(var LaListe : TStringList; var Separations : integer);
// extrait la liste des datums et projections
// Separations indique où se trouvent les limites entre datums,
// projections simples et projections multiples
// la 1ère limite est sur les 8 bits de poids faibles
// la seconde sur les 8 bits suivants
var
  I : integer;
begin
  LaListe.Clear;
  for I:=0 to LesDatum.Count-1 do
    LaListe.Add((LesDatum[I] as TDatum).Nom);
  Separations:=LaListe.Count-1;
  for I:=0 to LesProjection.Count-1 do
    LaListe.Add((LesProjection[I] as TProjection).Nom);
  inc(Separations,(LaListe.Count-1) shl 8);
  for I:=0 to LesMultiple.Count-1 do
    LaListe.Add((LesMultiple[I] as TMultiple).Nom);
end;

// réalise une conversion depuis le système Source vers les coordonnées géocentriques (en WGS84) (paramètres chaines)
function TDonnees.VersGeocentrique(NomSource, XSource, YSource : string;
            IndiceSource : integer; var XGeo, YGeo, ZGeo : real) : boolean;
// réalise une conversion depuis le système Source vers les coordonnées géocentriques (en WGS84) (paramètres chaines)
// NomSource est l'identifiants du système source
// X et Y sont les coordonnées
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource indique le numéro de fuseau
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource est inutilisé
// s'il s'agit d'un Datum, X indique la Latitude et Y la Longitude
// IndiceSource=0 pour une origine à Greenwich, =1 pour Paris
var
  I : integer;
  Lat, Long : real;
begin
  I:=NumeroDatum(NomSource);
  if I<>-1 then
  begin // c'est un datum, i.e. XSource et YSource sont des angles
    try
      Lat:=StrToAngle(XSource);
    except
      raise EConvertError.Create('Latitude de départ incorrecte, exemple 2d24''17.036''''');
    end;
    try
      Long:=StrToAngle(YSource);
    except
      raise EConvertError.Create('Longitude de départ incorrecte, exemple 2d24''17.036''''');
    end;
  end
  else
  begin // c'est une projection i.e. XSource et YSource sont des mètres
    try
      Lat:=StrToFloat(XSource);
    except
      raise EConvertError.Create('Easting de départ incorrecte, exemple 231500');
    end;
    try
      Long:=StrToFloat(YSource);
    except
      raise EConvertError.Create('Northing de départ incorrecte, exemple 231500');
    end;
  end;
  result:=VersGeocentriqueNum(NomSource, Lat, Long, IndiceSource, XGeo, YGeo, ZGeo);
end;

function TDonnees.DepuisGeocentrique(XGeo, YGeo, ZGeo : real; NomDest : string;
                        var IndiceDest : integer; var XDest, YDest : string;
                                       UniteAngle : TUniteAngle) : boolean;
// réalise une conversion depuis les coordonnées géocentriques (en WGS84) vers le système Dest (paramètres chaines)
// NomDest est l'identifiant du système destination
// X et Y sont les coordonnées
// s'il s'agit d'une projection, ce sont des mètres
// IndiceDestination indique le numéro de fuseau
// s'il s'agit d'une projection, ce sont des mètres
// IndiceDestination est indéfini et inutilisé
// s'il s'agit d'un Datum, X indique la Latitude et Y la Longitude
// IndiceSource=0 pour une origine à Greenwich, =1 pour Paris
// UniteAngle indique l'unité d'angle à utiliser pour la destination
var
  I : integer;
  Lat, Long : real;
begin
  result:=DepuisGeocentriqueNum(XGeo, YGeo, ZGeo, NomDest, IndiceDest, Lat, Long);
  I:=NumeroDatum(NomDest);
  if I<>-1 then
  begin // c'est un datum
    XDest:=AngleToStr(Lat,UniteAngle);
    YDest:=AngleToStr(Long,UniteAngle);
  end
  else
  begin // c'est une projection (simple ou multiple)
    XDest:=Format('%.3f',[Lat]);
    YDest:=Format('%.3f',[Long]);
  end;
end;

function TDonnees.VersGeocentriqueNum(NomSource : string; XSource, YSource : real;
            IndiceSource : integer; var XGeo, YGeo, ZGeo : real) : boolean;
// même chose que TDonnees.VersGeocentrique mais travail avec des nombre au lieu de chaînes
// réalise une conversion depuis le système Source vers les coordonnées géocentriques (en WGS84) (paramètres numériques)
// NomSource est l'identifiants du système source
// X et Y sont les coordonnées
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource indique le numéro de fuseau
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource est inutilisé
// s'il s'agit d'un Datum, X indique la Latitude et Y la Longitude
// IndiceSource=0 pour une origine à Greenwich, =1 pour Paris
var
  I : integer;
  Echec : boolean;
  Lat, Long : real;
begin
  Echec:=true;
  I:=NumeroDatum(NomSource);
  if I<>-1 then
  begin // c'est un datum
    TDatum(LesDatum[I]).VersCartesien(XSource, YSource, (IndiceSource=1), XGeo,YGeo, ZGeo);
    Echec:=false;
  end
  else
  begin // c'est une projection (simple ou mutiple)
    I:=NumeroProjection(NomSource);
    if I<>-1 then
    begin // c'est une projection simple
      TProjection(LesProjection[I]).VersLatLong(XSource, YSource, Lat, Long);
      TProjection(LesProjection[I]).Datum.VersCartesien(Lat, Long, false, XGeo, YGeo, ZGeo);
      Echec:=false;
    end
    else
    begin // ça devrait être une projection multiple
      I:=NumeroMultiple(NomSource);
      if I<>-1 then
      begin
        TMultiple(LesMultiple[I]).VersLatLong(XSource, YSource, IndiceSource, Lat, Long);
        TMultiple(LesMultiple[I]).Datum.VersCartesien(Lat, Long, false, XGeo, YGeo, ZGeo);
        Echec:=false;
      end;
    end;
  end;
  result:=not Echec;
end;

function TDonnees.DepuisGeocentriqueNum(XGeo, YGeo, ZGeo : real; NomDest : string;
                        var IndiceDest : integer; var XDest, YDest : real) : boolean;
// même chose que TDonnees.DepuisGeocentrique mais travail avec des nombre au lieu de chaînes
// réalise une conversion depuis les coordonnées géocentriques (en WGS84) vers le système Dest (paramètres numériques)
// NomDest est l'identifiant du système destination
// X et Y sont les coordonnées
// s'il s'agit d'une projection, ce sont des mètres
// IndiceDestination indique le numéro de fuseau
// s'il s'agit d'une projection, ce sont des mètres
// IndiceDestination est indéfini et inutilisé
// s'il s'agit d'un Datum, X indique la Latitude et Y la Longitude
// IndiceSource=0 pour une origine à Greenwich, =1 pour Paris
// UniteAngle indique l'unité d'angle à utiliser pour la destination
var
  I : integer;
  Echec : boolean;
  Lat, Long : real;
begin
  Echec:=true;
  I:=NumeroDatum(NomDest);
  if I<>-1 then
  begin // c'est un datum
    TDatum(LesDatum[I]).VersLatLong(XGeo, YGeo, ZGeo, (IndiceDest=1), XDest, YDest);
    Echec:=false;
  end
  else
  begin // c'est une projection (simple ou mutiple)
    I:=NumeroProjection(NomDest);
    if I<>-1 then
    begin // c'est une projection simple
      TProjection(LesProjection[I]).Datum.VersLatLong(XGeo, YGeo, ZGeo, false, Lat, Long);
      TProjection(LesProjection[I]).VersGrille(Lat, Long, XDest, YDest);
      Echec:=false;
    end
    else
    begin // ça devrait être une projection multiple
      I:=NumeroMultiple(NomDest);
      if I<>-1 then
      begin
        TMultiple(LesMultiple[I]).Datum.VersLatLong(XGeo, YGeo, ZGeo, false, Lat, Long);
        TMultiple(LesMultiple[I]).VersGrille(Lat, Long, XDest, YDest, IndiceDest);
        Echec:=false;
      end;
    end;
  end;
  result:=not Echec;
end;


function TDonnees.Conversion(NomSource, NomDest, XSource, YSource : string;
                     IndiceSource : integer; var IndiceDest : integer;
                     var XDest, YDest : string; UniteAngle : TUniteAngle) : boolean;
// réalise une conversion entre les systèmes Source et Dest
// NomSource et NomDest sont les Nom identifiants les systèmes source et destination
// X et Y sont les coordonnées
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource et IndiceDestination indiquent les numéros de fuseau
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource et IndiceDestination sont indéfinis et inutilisés
// s'il s'agit d'un Datum, X indique la Latitude et Y la Longitude
// IndiceSource=0 pour une origine à Greenwich, =1 pour Paris
// UniteAngle indique l'unité d'angle à utiliser pour la destination
var
  Xcart, Ycart, Zcart : real;
begin
  result:=VersGeocentrique(NomSource, XSource, YSource, IndiceSource, Xcart, Ycart, Zcart) and
     DepuisGeocentrique(Xcart, Ycart, Zcart, NomDest, IndiceDest, XDest, YDest, UniteAngle);
end;

function TDonnees.ConversionNum(NomSource, NomDest : string; XSource, YSource : real;
                     IndiceSource : integer; var IndiceDest : integer;
                     var XDest, YDest : real) : boolean;
// même chose que TDonnees.Conversion mais travail avec des nombre au lieu de chaînes
// NomSource et NomDest sont les Nom identifiants les systèmes source et destination
// X et Y sont les coordonnées
// s'il s'agit d'une projection, ce sont des mètres
// s'il s'agit d'un datum, ce sont des radians
// IndiceSource et IndiceDestination indiquent les numéros de fuseau
// s'il s'agit d'une projection, ce sont des mètres
// IndiceSource et IndiceDestination sont indéfinis et inutilisés
// s'il s'agit d'un Datum, X indique la Latitude et Y la Longitude (en radian)
// IndiceSource=0 pour une origine à Greenwich, =1 pour Paris
// UniteAngle indique l'unité d'angle à utiliser pour la destination
var
  Xcart, Ycart, Zcart : real;
begin
  result:=VersGeocentriqueNum(NomSource, XSource, YSource, IndiceSource, Xcart, Ycart, Zcart) and
     DepuisGeocentriqueNum(Xcart, Ycart, Zcart, NomDest, IndiceDest, XDest, YDest);
end;

{
var
  Lat, Long, Xcart, Ycart, Zcart : real;
  I : integer;
  Echec : boolean;
begin
// conversion vers coordonnées cartésiennes
// depuis datum
  Echec:=true;
  try
    for I:=0 to LesDatum.Count-1 do
     with TDatum(LesDatum[I]) do
      if NomSource=Nom then
      begin
        VersCartesien(XSource, YSource, (IndiceSource=1), Xcart,Ycart, Zcart);
        Echec:=false;
      end;
  // ou depuis projection
    for I:=0 to LesProjection.Count-1 do
     with TProjection(LesProjection[I]) do
      if NomSource=Nom then
      begin
        VersLatLong(XSource, YSource, Lat, Long);
        Datum.VersCartesien(Lat, Long, false, Xcart, Ycart, Zcart);
        Echec:=false;
      end;
  // ou depuis projection multiple
    for I:=0 to LesMultiple.Count-1 do
     with TMultiple(LesMultiple[I]) do
      if NomSource=Nom then
      begin
        VersLatLong(XSource, YSource, IndiceSource, Lat, Long);
        Datum.VersCartesien(Lat, Long, false, Xcart, Ycart, Zcart);
        Echec:=false;
      end;
  finally
  end;
  if not Echec then
  try
    Echec:=true;
  // conversion inverse depuis les coordonnées cartésiennes
    for I:=0 to LesDatum.Count-1 do
     with TDatum(LesDatum[I]) do
      if NomDest=Nom then
      begin
        VersLatLong(Xcart, Ycart, Zcart, (IndiceDest=1), XDest, YDest);
        Echec:=false;
      end;
  // ou vers projection
    for I:=0 to LesProjection.Count-1 do
     with TProjection(LesProjection[I]) do
      if NomDest=Nom then
      begin
        Datum.VersLatLong(Xcart, Ycart, Zcart, false, Lat, Long);
        VersGrille(Lat, Long, XDest, YDest);
        Echec:=false;
      end;
  // ou vers projection multiple
    for I:=0 to LesMultiple.Count-1 do
     with TMultiple(LesMultiple[I]) do
      if NomDest=Nom then
      begin
        Datum.VersLatLong(Xcart, Ycart, Zcart, false, Lat, Long);
        VersGrille(Lat, Long, XDest, YDest, IndiceDest);
        Echec:=false;
      end;
  finally
  end;
  result:=not Echec;
end;
}

function TDonnees.Convergence(NomSource, NomDest, XSource, YSource : string;
                             IndiceSource : integer;
                             var Declinaison : string; UniteAngle : TUniteAngle) : boolean;
// calcul de la convergence au point XSource, YSource
// faute de formule exacte, le calcul est réalisé par essai autour du point choisi
// on se décale de 100m en direction du Nord et on calcule l'angle résultant
type
  Doublet = array[0..1] of real;
var
  Lat, Long, Xproj, Yproj, Xcart, Ycart, Zcart : Doublet;
//  XCardinaux, YCardinaux, ZCardinaux : doublet;
//  Nord, Est : real;
  I, J, IndiceDest : integer;
  Echec : boolean;

  function Angle_Doublet(X, Y : Doublet) : real;
  begin
    result:=-arctan((X[1]-X[0])/(Y[1]-Y[0]));
  end;

begin
// conversion vers coordonnées cartésiennes
// depuis datum
  Echec:=true;
  try
    for I:=0 to LesDatum.Count-1 do
     with TDatum(LesDatum[I]) do
      if NomSource=Nom then
      begin
        try
          Lat[0]:=StrToAngle(XSource);
          Lat[1]:=Lat[0]+1.5e-5;
        except
          raise EConvertError.Create('Latitude de départ incorrecte, exemple 2d24''17.036''''');
        end;
        try
          Long[0]:=StrToAngle(YSource);
          Long[1]:=Long[0];
        except
          raise EConvertError.Create('Longitude de départ incorrecte, exemple 2d24''17.036''''');
        end;
        for J:=0 to 1 do
          VersCartesien(Lat[J], Long[J], (IndiceSource=1), Xcart[J],Ycart[J], Zcart[J]);
        Echec:=false;
      end;
  // ou depuis projection
    for I:=0 to LesProjection.Count-1 do
     with TProjection(LesProjection[I]) do
      if NomSource=Nom then
      begin
        try
          Xproj[0]:=StrToFloat(XSource);
          Xproj[1]:=XProj[0];
        except
          raise EConvertError.Create('Easting de départ incorrecte, exemple 231500');
        end;
        try
          Yproj[0]:=StrToFloat(YSource);
          Yproj[1]:=Yproj[0]+100;
        except
          raise EConvertError.Create('Northing de départ incorrecte, exemple 231500');
        end;
        for J:=0 to 1 do
        begin
          VersLatLong(Xproj[J], Yproj[J], Lat[J], Long[J]);
          Datum.VersCartesien(Lat[J], Long[J], false, Xcart[J], Ycart[J], Zcart[J]);
        end;
        Echec:=false;
      end;
  // ou depuis projection multiple
    for I:=0 to LesMultiple.Count-1 do
     with TMultiple(LesMultiple[I]) do
      if NomSource=Nom then
      begin
        try
          Xproj[0]:=StrToFloat(XSource);
          Xproj[1]:=XProj[0];
        except
          raise EConvertError.Create('Easting de départ incorrecte, exemple 231500');
        end;
        try
          Yproj[0]:=StrToFloat(YSource);
          Yproj[1]:=Yproj[0]+100;
        except
          raise EConvertError.Create('Northing de départ incorrecte, exemple 231500');
        end;
        for J:=0 to 1 do
        begin
          VersLatLong(Xproj[J], Yproj[J], IndiceSource, Lat[J], Long[J]);
          Datum.VersCartesien(Lat[J], Long[J], false, Xcart[J], Ycart[J], Zcart[J]);
        end;
        Echec:=false;
      end;
  finally
  end;
  if not Echec then
  try
    Echec:=true;
  // conversion inverse depuis les coordonnées cartésiennes
    for I:=0 to LesDatum.Count-1 do
     with TDatum(LesDatum[I]) do
      if NomDest=Nom then
      begin
        for J:=0 to 1 do
          VersLatLong(Xcart[J], Ycart[J], Zcart[J], (IndiceDest=1), Lat[J], Long[J]);
        Declinaison:=AngleToStr(GetRelevement(Lat[0],Long[0],Lat[1],Long[1],Ellipsoide.e),UniteAngle);
        Echec:=false;
      end;
  // ou vers projection
    for I:=0 to LesProjection.Count-1 do
     with TProjection(LesProjection[I]) do
      if NomDest=Nom then
      begin
        for J:=0 to 1 do
        begin
          Datum.VersLatLong(Xcart[J], Ycart[J], Zcart[J], false, Lat[J], Long[J]);
          VersGrille(Lat[J], Long[J], Xproj[J], Yproj[J]);
        end;
        Declinaison:=AngleToStr(-Angle_Doublet(XProj,YProj),UniteAngle);
        Echec:=false;
      end;
  // ou vers projection multiple
    for I:=0 to LesMultiple.Count-1 do
     with TMultiple(LesMultiple[I]) do
      if NomDest=Nom then
      begin
        for J:=0 to 1 do
        begin
          Datum.VersLatLong(Xcart[J], Ycart[J], Zcart[J], false, Lat[J], Long[J]);
          VersGrille(Lat[J], Long[J], Xproj[J], Yproj[J], IndiceDest);
        end;
        Declinaison:=AngleToStr(-Angle_Doublet(XProj,YProj),UniteAngle);
        Echec:=false;
      end;
  finally
  end;
  result:=not Echec;
end;

function TDonnees.Alteration(NomSource, NomDest, XSource, YSource : string;
                             IndiceSource : integer;
                             var Facteur : real) : boolean;
// calcul de le facteur d'altération linéaire au point XSource, YSource
// faute de formule exacte, le calcul est réalisé par essai autour du point choisi
// on se décale de 100m en direction du Nord et on calcule l'altération correspondante
type
  Doublet = array[0..1] of real;
var
  Lat, Long, Xproj, Yproj, Xcart, Ycart, Zcart : Doublet;
  DistanceP2P : real;
  I, J, IndiceDest : integer;
  Echec : boolean;

  function Angle_Doublet(X, Y : Doublet) : real;
  begin
    result:=-arctan((X[1]-X[0])/(Y[1]-Y[0]));
  end;

begin
// conversion vers coordonnées cartésiennes
// depuis datum
  Echec:=true;
  try
    DistanceP2P:=100;
    for I:=0 to LesDatum.Count-1 do
     with TDatum(LesDatum[I]) do
      if NomSource=Nom then
      begin
        try
          Lat[0]:=StrToAngle(XSource);
          Lat[1]:=Lat[0]+1.5e-5;
        except
          raise EConvertError.Create('Latitude de départ incorrecte, exemple 2d24''17.036''''');
        end;
        try
          Long[0]:=StrToAngle(YSource);
          Long[1]:=Long[0];
        except
          raise EConvertError.Create('Longitude de départ incorrecte, exemple 2d24''17.036''''');
        end;
        for J:=0 to 1 do
          VersCartesien(Lat[J], Long[J], (IndiceSource=1), Xcart[J],Ycart[J], Zcart[J]);
        DistanceP2P:=sqrt(sqr(Xcart[1]-Xcart[0])+sqr(Ycart[1]-Ycart[0])+sqr(Zcart[1]-Zcart[0]));
        Echec:=false;
      end;
  // ou depuis projection
    for I:=0 to LesProjection.Count-1 do
     with TProjection(LesProjection[I]) do
      if NomSource=Nom then
      begin
        try
          Xproj[0]:=StrToFloat(XSource);
          Xproj[1]:=XProj[0];
        except
          raise EConvertError.Create('Easting de départ incorrecte, exemple 231500');
        end;
        try
          Yproj[0]:=StrToFloat(YSource);
          Yproj[1]:=Yproj[0]+100;
          DistanceP2P:=100;
        except
          raise EConvertError.Create('Northing de départ incorrecte, exemple 231500');
        end;
        for J:=0 to 1 do
        begin
          VersLatLong(Xproj[J], Yproj[J], Lat[J], Long[J]);
          Datum.VersCartesien(Lat[J], Long[J], false, Xcart[J], Ycart[J], Zcart[J]);
        end;
        Echec:=false;
      end;
  // ou depuis projection multiple
    for I:=0 to LesMultiple.Count-1 do
     with TMultiple(LesMultiple[I]) do
      if NomSource=Nom then
      begin
        try
          Xproj[0]:=StrToFloat(XSource);
          Xproj[1]:=XProj[0];
        except
          raise EConvertError.Create('Easting de départ incorrecte, exemple 231500');
        end;
        try
          Yproj[0]:=StrToFloat(YSource);
          Yproj[1]:=Yproj[0]+100;
          DistanceP2P:=100;
        except
          raise EConvertError.Create('Northing de départ incorrecte, exemple 231500');
        end;
        for J:=0 to 1 do
        begin
          VersLatLong(Xproj[J], Yproj[J], IndiceSource, Lat[J], Long[J]);
          Datum.VersCartesien(Lat[J], Long[J], false, Xcart[J], Ycart[J], Zcart[J]);
        end;
        Echec:=false;
      end;
  finally
  end;
  if not Echec then
  try
    Echec:=true;
  // conversion inverse depuis les coordonnées cartésiennes
    for I:=0 to LesDatum.Count-1 do
     with TDatum(LesDatum[I]) do
      if NomDest=Nom then
      begin
        for J:=0 to 1 do
          VersLatLong(Xcart[J], Ycart[J], Zcart[J], (IndiceDest=1), Lat[J], Long[J]);
        Facteur:=1; // ça n'a pas vraiment de sens
        Echec:=false;
      end;
  // ou vers projection
    for I:=0 to LesProjection.Count-1 do
     with TProjection(LesProjection[I]) do
      if NomDest=Nom then
      begin
        for J:=0 to 1 do
        begin
          Datum.VersLatLong(Xcart[J], Ycart[J], Zcart[J], false, Lat[J], Long[J]);
          VersGrille(Lat[J], Long[J], Xproj[J], Yproj[J]);
        end;
        Facteur:=sqrt(sqr(Xproj[1]-Xproj[0])+sqr(Yproj[1]-Yproj[0]))/DistanceP2P;
        Echec:=false;
      end;
  // ou vers projection multiple
    for I:=0 to LesMultiple.Count-1 do
     with TMultiple(LesMultiple[I]) do
      if NomDest=Nom then
      begin
        for J:=0 to 1 do
        begin
          Datum.VersLatLong(Xcart[J], Ycart[J], Zcart[J], false, Lat[J], Long[J]);
          VersGrille(Lat[J], Long[J], Xproj[J], Yproj[J], IndiceDest);
        end;
        Facteur:=sqrt(sqr(Xproj[1]-Xproj[0])+sqr(Yproj[1]-Yproj[0]))/DistanceP2P;
        Echec:=false;
      end;
  finally
  end;
  result:=not Echec;
end;

function TDonnees.UnEllipsoide(LeQuel : string) : TObject;
var
  I : integer;
begin
  I:=NumeroEllipsoide(Lequel);
  if I=-1 then
    result:=nil
  else
    result:=LesEllipsoide[I];
end;

function TDonnees.UnDatum(LeQuel : string) : TObject;
var
  I : integer;
begin
  I:=NumeroDatum(Lequel);
  if I=-1 then
    result:=nil
  else
    result:=LesDatum[I];
end;

function TDonnees.UneProjection(LeQuel : string) : TObject;
var
  I : integer;
begin
  I:=NumeroProjection(LeQuel);
  if I=-1 then
  begin
    I:=NumeroMultiple(Lequel);
    if I=-1 then
      result:=nil
    else
      result:=LesMultiple[I];
  end
  else
    result:=LesProjection[I];
end;

function TDonnees.RetireEllipsoide(LeQuel : string) : boolean;
// retire l'éllipsoïde demandé, vérifie les dépendances, renvoie False si il y en a
var
  Candidat : TEllipsoide;
  I : integer;
begin
  Candidat:=UnEllipsoide(LeQuel) as TEllipsoide;
  I:=0;
  while (I<LesDatum.Count) and (Candidat<>(LesDatum[I] as TDatum).Ellipsoide) do
    inc(I);
  result:=(I=LesDatum.Count);
  if result then
    LesEllipsoide.Remove(Candidat);
end;

function TDonnees.RetireDatum(LeQuel : string) : boolean;
// retire le datum demandé, vérifie les dépendances, renvoie False si il y en a
var
  Candidat : TDatum;
  I : integer;
begin
  Candidat:=UnDatum(LeQuel) as TDatum;
  I:=0;
  while (I<LesProjection.Count) and (Candidat<>(LesProjection[I] as TProjection).Datum) do
    inc(I);
  result:=(I=LesProjection.Count);
  if result then
  begin
    I:=0;
    while (I<LesMultiple.Count) and (Candidat<>(LesMultiple[I] as TMultiple).Datum) do
      inc(I);
    result:=(I=LesMultiple.Count);
    if result then
      LesDatum.Remove(Candidat);
  end;
end;


function TDonnees.LaDescription(LeQuel : string) : string;
// retourne la description associée
var
  I : integer;
begin
  result:='';
  for I:=0 to LesDatum.Count-1 do
    if LeQuel=(LesDatum[I] as TDatum).Nom then
      result:=(LesDatum[I] as TDatum).Description;
  for I:=0 to LesProjection.Count-1 do
    if LeQuel=(LesProjection[I] as TProjection).Nom then
      result:=(LesProjection[I] as TProjection).Description;
  for I:=0 to LesMultiple.Count-1 do
    if LeQuel=(LesMultiple[I] as TMultiple).Nom then
      result:=(LesMultiple[I] as TMultiple).Description;
end;

function TDonnees.Inconnu(Candidat : string) : boolean;
// indique si un nom est déjà utilisé
var
  I : integer;
begin
  result:=true;
  for I:=0 to LesDatum.Count-1 do
    if (LesDatum[I] as TDatum).Nom=Candidat then
      result:=false;
  for I:=0 to LesProjection.Count-1 do
    if (LesProjection[I] as TProjection).Nom=Candidat then
      result:=false;
  for I:=0 to LesMultiple.Count-1 do
    if (LesMultiple[I] as TMultiple).Nom=Candidat then
      result:=false;
end;

function TDonnees.NumeroDatum(LeQuel: string): integer;
begin
  result:=LesDatum.Count-1;
  while (result>=0) and (LeQuel<>(LesDatum[result] as TDatum).Nom) do
    dec(result);
end;

function TDonnees.NumeroEllipsoide(LeQuel: string): integer;
begin
  result:=LesEllipsoide.Count-1;
  while (result>=0) and (LeQuel<>(LesEllipsoide[result] as TEllipsoide).Nom) do
    dec(result);
end;

function TDonnees.NumeroMultiple(LeQuel: string): integer;
begin
  result:=LesMultiple.Count-1;
  while (result>=0) and (LeQuel<>(LesMultiple[result] as TMultiple).Nom) do
    dec(result);
end;

function TDonnees.NumeroProjection(LeQuel: string): integer;
begin
  result:=LesProjection.Count-1;
  while (result>=0) and (LeQuel<>(LesProjection[result] as TProjection).Nom) do
    dec(result);
end;

function TDonnees.GetNbDatum: integer;
begin
  result:=LesDatum.Count;
end;

function TDonnees.GetNbEllipsoide: integer;
begin
  result:=LesEllipsoide.Count;
end;

function TDonnees.GetNbMultiple: integer;
begin
  result:=LesMultiple.Count;
end;

function TDonnees.GetNbProjection: integer;
begin
  result:=LesProjection.Count;
end;

// liste les systèmes géodésiques et remplit le tableau des noms
// retourne le numéro de séparations ou -1 si échec
function TDonnees.ListerLesSystemes: integer;
var
  QListeSyst: TStringList;
  Sepa: Integer;
  I: Integer;
begin
  Sepa   := -1;
  Result := -1;
  QListeSyst := TStringList.Create;
  try
    QListeSyst.Clear;
    try
      SetLength(FListeDesNomsDeSystemes, 0);
      for I:=0 to LesDatum.Count-1 do QListeSyst.Add((LesDatum[I] as TDatum).Nom);
      Sepa := QListeSyst.Count-1;
      for I:=0 to LesProjection.Count-1 do QListeSyst.Add((LesProjection[I] as TProjection).Nom);
      inc(Sepa, (QListeSyst.Count-1) shl 8);
      for I:=0 to LesMultiple.Count-1 do QListeSyst.Add((LesMultiple[I] as TMultiple).Nom);
      // on initialise le tableau contenant les noms des systèmes
      FNbreDeSystemes := QListeSyst.Count;
      SetLength(FListeDesNomsDeSystemes, FNbreDeSystemes);
      for i := 0 to FNbreDeSystemes - 1 do
      begin
        FListeDesNomsDeSystemes[i] := QListeSyst.Strings[i];
      end;
      // passage ici: OK
      Result := Sepa;
    except
      // purge de la stringlist provisoire
      QListeSyst.Clear;
    end;
  finally
    QListeSyst.Free;
  end;
end;
// nombre de projections
function TDonnees.GetNbDeSystemes: integer;
begin
  Result := FNbreDeSystemes;
end;
// attraper le libellé d'une projection
function TDonnees.GetNomDeProjection(const Idx: integer): string;
begin
  Result := FListeDesNomsDeSystemes[Idx];
end;

end.
