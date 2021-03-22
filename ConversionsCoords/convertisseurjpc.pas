unit ConvertisseurJPC;
// shunte l'unité Donnee.pas
//
// Statut: Opérationnel. A valider de manière approfondie.
// DONE: Nettoyer le code de certaines unités, dont les fonctions appelant TStream
// DONE: Le symbole degré, qui posait de gros problèmes, est remplacé par 'd'
// DONE: Seuls les degrés décimaux sont utilisés bien que les sexagésimaux soient traités en interne
// 02/04/2014: Les sources du convertisseur partent dans un dossier séparé ./ConversionsCoords
// 02/04/2014: Possibilité de se passer du fichier de paramètres (régénéré en dur)
// 02/04/2014: Fonction GetNumeroFuseauUTM()
// 21/08/2014: Support complet des codes EPSG
// 22/08/2014: Support uniquement en codes EPSG.

// Conversions Lambert -> WGS84   : OK
// Conversions WGS84   -> Lambert : OK
// Conversions WGS84   -> UTM     : OK
// Conversions UTM     -> WGS84   : OK

// NOTA: Ajouter le chemin ./ConversionsCoords dans 'Autres fichiers unité' de 'Options du projet'

{$mode delphi}
interface

uses
  //StructuresDonnees, Common
   GHCD_Types, GeneralFunctions
  ,ConversionOutils
  ,createur
  ,ellipsoide
  ,datum
  ,projection
  ,multiple

  //,Common               in '../Common.pas'
  //,StructuresDonnees    in '../StructuresDonnees.pas'
  ,Classes
  ,Contnrs
  ,SysUtils;


type

  { TConversionSysteme }

  TConversionSysteme = class


  private
    // ready ?
    FIsReady: boolean;
    // liste des systèmes géodésiques
    FListeDesSystemes: array of TLabelSystemesCoordsEPSG;
    FNbreSystemes    : integer;

    fNomJeuDeDonnees: string;
    fLesEllipsoide : TObjectList;
    fLesDatum      : TObjectList;
    fLesProjection : TObjectList;
    fLesMultiple   : TObjectList;

    function GenererListeDesSystemes: boolean;
    function GetIndexSysteme(const Mode: byte; const qCodeEPSG: TCodeEPSG): integer;
    // conversions
    function ConversionNum(const IndiceSource : integer;
                            const EPSGSource, EPSGDest : integer;
                            const XSource, YSource : double;
                            var IndiceDest : integer;
                            var XDest, YDest : double) : boolean;


    function DepuisGeocentriqueNum(const XGeo, YGeo, ZGeo: double;
                                   const EPSGDest: integer;
                                   var IndiceDest: integer;
                                   var XDest, YDest: double): boolean;
    function VersGeocentriqueNum(const IndiceSource: integer;
                                 const EPSGSource  : integer;
                                 const XSource, YSource : double;
                                 var XGeo, YGeo, ZGeo   : double) : boolean;


     // ellipsoides
    function GetNbEllipsoides(): integer;
    function GetEllipsoide(const Idx: integer): TEllipsoide;
    function GetIdxEllipsoideByName(const qNom: string): integer;
    function GetIdxEllipsoideByCodeEPSG(const C: TCodeEPSG): integer;
    procedure AddEllipsoideByParameters(const qCodeEPSG: TCodeEPSG;
                                        const qNom, qDescription: string; const a, e: double);
     // datums
    function GetNbDatums(): integer;
    function GetDatum(const Idx: integer): TDatum;
    function GetIdxDatumByName(const qNom: string): integer;
    function GetIdxDatumByCodeEPSG(const C: TCodeEPSG): integer;

    procedure AddDatumByParameters(const qCodeEPSG: TCodeEPSG;
                                   const qNom, qDescription, qNomEllipsoide: string;
                                   const qDX, qDY, qDZ, qRotX, qRotY, qRotZ, qScale: double;
                                   const qGrilleCorrection: string);
     // projections
    function GetNbProjections(): integer;
    function GetProjection(const Idx: integer): TProjection;
    function GetIdxProjectionByName(const qNom: string): integer;
    function GetIdxProjectionByCodeEPSG(const C: TCodeEPSG): integer;
    procedure AddProjectionByParameters(const qCodeEPSG: TCodeEPSG; const qNatureProj, qNom, qDescription, qDatum: string; const qParametres: TGHStringArray);
    // multiples
    function GetNbMultiples(): integer;
    function GetMultiple(const Idx: integer): TMultiple;
    function GetIdxMultipleByCodeEPSG(const C: integer): integer;
    function GetIdxMultipleByName(const qNom: string): integer;

    procedure AddMultipleByParameters(const qCodeEPSG: integer; const qNatureMultiple, qNom, qDescription, qDatum: string; const qParametres: TGHStringArray);
    // tests
    function Tester(): boolean;
  public
    constructor Create;
    destructor Destroy;
    function IsReady(): boolean;

    //**************************************************************************
    // Procédures communes aux autres convertisseurs
    function  Initialiser(): boolean;
    procedure Finaliser();
    // attraper le nombre de projections
    function GetNbSystemes(): integer;
    // attraper une projection donnée
    function GetCodeEPSGNomSysteme(const Idx: integer): TLabelSystemesCoordsEPSG;
    function FindCodeEPSGNomSysteme(const C: integer): TLabelSystemesCoordsEPSG;

    // attrapper un numéro de fuseau UTM
    function GetNumeroFuseauUTM(const Latitude, Longitude: double): integer;
    // index de la proj correspondant à un code EPSG
    //function GetIndexSystemeByCodeEPSG(const C: integer): integer;
    // conversion d'un point par index source et cible (utilisé dans la calculette)
    function ConversionSyst1ToSyst2Index(const SystSrc, SystCible: integer; const MyPoint: TProjUV): TProjUV;
    // conversion d'un point par codes EPSG source et cible
    function ConversionSyst1ToSyst2EPSG(const SystSrc, SystCible: integer; const MyPoint: TProjUV): TProjUV;


    function GetEPSGSystemeFromCodeEPSG(const C: integer): TLabelSystemesCoordsEPSG;
    function GetIndexSystemeByCodeEPSG(const C: integer): integer;
    // reconstruction des grilles depuis données en dur
    procedure ReconstruireProjectionsDepuisDonneesEnDur;
    //**************************************************************************
    // Gestion des fichiers de paramétrage (non utilisé pour l'instant)
    // chargement du fichier de paramétrage
    procedure LoadFromFichier(const Filename: string); deprecated;
    // régénération du fichier projections
    function RegenererFichierProjections(const FP: string): boolean; deprecated;

  end;

implementation
const
  MD_DATUM = 0;
  MD_PROJECTION = 1;
  MD_MULTIPLE   = 2;

//const CODE_EPSG_DEBUT_LAMBERT_ZONES =  27570; // 2757x = avec les millions    27560 = sans les millions

const
  aWGS84: double = 6378137;
  eWGS84: double = 0.08181919106;
  //  bWGS84 : real = aWGS84*sqrt(1-sqr(eWGS84));
  bWGS84: double = 6335439;
const
  { Scaling value radiant to decimal degree }
  RAD_TO_DEG   =	57.29577951308232;
  { Scaling value decimal degree to radiant }
  DEG_TO_RAD   =	0.0174532925199432958;


{ TConvertisseurJPC }
function TConversionSysteme.GetIndexSysteme(const Mode: byte; const qCodeEPSG: TCodeEPSG): integer;
begin
  Result := -1;
  case Mode of
    MD_DATUM     : Result := GetIdxDatumByCodeEPSG(qCodeEPSG);
    MD_PROJECTION: Result := GetIdxProjectionByCodeEPSG(qCodeEPSG);
    MD_MULTIPLE  : Result := GetIdxMultipleByCodeEPSG(qCodeEPSG);
  end;
end;
procedure TConversionSysteme.Finaliser; // pour compatibilité
begin
  ;
end;

function TConversionSysteme.Initialiser: boolean;
var
  i: integer;
  ELL: TEllipsoide;
  DTM: TDatum;
  PRJ: TProjection;
  p: Integer;
  QNb: Integer;
  WU: String;
  MyMULT: TMultiple;
  FichierProjections: String;
begin
  DefaultFormatSettings.DecimalSeparator := '.';
  Result := False;
  //FichierProjections := GetGHTopoDirectory()  + 'A00_ProjectionsUsuelles.txt';
  //++ FichierProjections := GetGHTopoDirectory()  + 'A000_ProjectionsUsuelles.txt';
  //++ if (Not FileExists(FichierProjections)) then RegenererFichierProjections(FichierProjections);
  try
    // ajouter ellipsoides par défaut
    AddEllipsoideByParameters(CODE_EPSG_WGS84, 'GRS80', 'GRS 1980', aWGS84, eWGS84);
    AfficherMessage('-- 001');
    // ajouter datums par défaut
    AddDatumByParameters(CODE_EPSG_WGS84, 'WGS84', 'World Geodesic System', 'GRS80', 0, 0, 0, 0, 0, 0, 1, '');
    AfficherMessage('-- 002');

    ReconstruireProjectionsDepuisDonneesEnDur;  // reconstruction en dur
    AfficherMessage('-- 003');
    GenererListeDesSystemes;    // générer la liste des systèmes de coordonnées
    AfficherMessage('-- 004');
    Tester();                     // test avec un jeu de données
    Result := True;
  except
    AfficherMessage(Format('Erreur dans %s.Initialiser', [ClassName]));
  end;
end;


constructor TConversionSysteme.Create;
begin
  FIsReady        := false;
  fLesEllipsoide  := TObjectList.Create;
  fLesDatum       := TObjectList.Create;
  fLesProjection  := TObjectList.Create;
  fLesMultiple    := TObjectList.Create;
  fLesEllipsoide.Clear;
  fLesDatum.Clear;
  fLesProjection.Clear;
  fLesMultiple.Clear;
end;

destructor TConversionSysteme.Destroy;
begin
  fLesMultiple.Free;
  fLesProjection.Free;
  fLesDatum.Free;
  fLesEllipsoide.Free;
  FIsReady := false;
end;
function TConversionSysteme.IsReady: boolean;
begin
  Result := FIsReady;
end;
// ellipsoides
function TConversionSysteme.GetNbEllipsoides: integer;
begin
  Result := fLesEllipsoide.Count;
end;

function TConversionSysteme.GetEllipsoide(const Idx: integer): TEllipsoide;
begin
  Result := fLesEllipsoide.Items[Idx] as TEllipsoide;
end;

function TConversionSysteme.GetIdxEllipsoideByName(const qNom: string): integer;
var
  i: integer;
  EWE: TEllipsoide;
begin
  Result := -1;
  for i := 0 to GetNbEllipsoides - 1 do
  begin
    EWE := GetEllipsoide(i);
    if (LowerCase(EWE.Nom) = LowerCase(qNom)) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;
function TConversionSysteme.GetIdxEllipsoideByCodeEPSG(const C: TCodeEPSG): integer;
var
  i: integer;
  EWE: TEllipsoide;
begin
  Result := -1;
  for i := 0 to GetNbEllipsoides - 1 do
  begin
    EWE := GetEllipsoide(i);
    if (TCodeEPSG(EWE.CodeEPSG) = C) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TConversionSysteme.AddEllipsoideByParameters(const qCodeEPSG: TCodeEPSG;
                                                       const qNom, qDescription: string;
                                                       const a, e: double);
var
  EWE: TEllipsoide;
begin
  EWE := TEllipsoide.Create;
  EWE.SetParameters(qCodeEPSG, qNom, qDescription, a, e);
  fLesEllipsoide.Add(EWE);
end;

// datums
function TConversionSysteme.GetNbDatums: integer;
begin
  Result := fLesDatum.Count;
end;

function TConversionSysteme.GetDatum(const Idx: integer): TDatum;
begin
  Result := fLesDatum.Items[Idx] as TDatum;
end;

function TConversionSysteme.GetIdxDatumByName(const qNom: string): integer;
var
  i: Integer;
  EWE: TDatum;
begin
  Result := -1;
  for i := 0 to GetNbDatums - 1 do
  begin
    EWE := GetDatum(i);
    if (LowerCase(EWE.Nom) = LowerCase(qNom)) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;
function TConversionSysteme.GetIdxDatumByCodeEPSG(const C: TCodeEPSG): integer;
var
  i: Integer;
  EWE: TDatum;
begin
  Result := -1;
  for i := 0 to GetNbDatums - 1 do
  begin
    EWE := GetDatum(i);
    if (EWE.CodeEPSG = C) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

// Datum  NTF  Nouvelle  Triangulation  Francaise  Clarke80  -168  -60  320  *
procedure TConversionSysteme.AddDatumByParameters(const qCodeEPSG: TCodeEPSG;
                                                  const qNom, qDescription, qNomEllipsoide: string;
                                                  const qDX, qDY, qDZ, qRotX,
                                                        qRotY, qRotZ, qScale: double;
                                                  const qGrilleCorrection: string);
var
  MyDatum: TDatum;
  WU: integer;
begin
  MyDatum := TDatum.Create;

  try

    MyDatum.Nom := qNom;
    MyDatum.Description := qDescription;
    MyDatum.CodeEPSG    := qCodeEPSG;
    // transfo 7 paramètres
    MyDatum.DeltaX := qDX;
    MyDatum.DeltaY := qDY;
    MyDatum.DeltaZ := qDZ;
    MyDatum.RotX := qRotX;
    MyDatum.RotY := qRotY;
    MyDatum.RotZ := qRotZ;
    MyDatum.Scale := qScale;
    // Ellipsoide non trouvé => on sort en libérant le TDatum créé
    WU := GetIdxEllipsoideByName(qNomEllipsoide);
    if (WU = -1) then Raise ERangeError.CreateFmt('Ellipsoide %s inconnu dans le datum %s',  [qNomEllipsoide, qNom]);

    MyDatum.Ellipsoide := GetEllipsoide(WU);
    MyDatum.Valid := True;
    fLesDatum.Add(MyDatum);
  except
    MyDatum.Free;
    AfficherMessage(Format('Erreur dans %s.AddDatumByParameters (%d)', [ClassName, WU]));
  end;
end;

function TConversionSysteme.GetNbProjections: integer;
begin
  Result := fLesProjection.Count;
end;

function TConversionSysteme.GetProjection(const Idx: integer): TProjection;
begin
  Result := fLesProjection.Items[Idx] as TProjection;
end;

function TConversionSysteme.GetIdxProjectionByName(const qNom: string): integer;
var
  i: Integer;
  EWE: TProjection;
begin
  Result := -1;
  for i := 0 to GetNbProjections - 1 do
  begin
    EWE := GetProjection(i);
    if (LowerCase(EWE.Nom) = LowerCase(qNom)) then
    begin
      Result := i;
      Exit;
    end;
  end;

end;
function TConversionSysteme.GetIdxProjectionByCodeEPSG(const C: TCodeEPSG): integer;
var
  i: Integer;
  EWE: TProjection;
begin
  Result := -1;
  for i := 0 to GetNbProjections - 1 do
  begin
    EWE := GetProjection(i);
    if (EWE.CodeEPSG = C) then
    begin
      Result := i;
      Exit;
    end;
  end;

end;

procedure TConversionSysteme.AddProjectionByParameters(const qCodeEPSG: TCodeEPSG;
                                                       const qNatureProj, qNom, qDescription, qDatum: string;
                                                       const qParametres: TGHStringArray);
var
  MyProj: TProjection;
  WU: Integer;
  Compteur: Integer;
  i: Integer;
  EWE: String;
  q: Integer;
begin
  try
    MyProj := CreateProjection(qNatureProj);
    MyProj.Nom          := qNom;
    MyProj.Description  := qDescription;
    MyProj.CodeEPSG     := qCodeEPSG;
    WU := GetIdxDatumByName(qDatum);

    if (WU = -1) then Raise ERangeError.CreateFmt('Datum %s inconnu dans la projection %s',  [qDatum, qNom]);
    MyProj.Datum := GetDatum(WU);
    // décodage des paramètres
    Compteur := 0;
    for i := 0 to MyProj.Nb_ParamReal - 1    do MyProj.ParamReal[i]    := StrToFloatDef(qParametres[i], 0.00);
    Inc(Compteur, MyProj.Nb_ParamReal);
    for I := 0 to MyProj.Nb_ParamInteger - 1 do MyProj.ParamInteger[I] := StrToInt(qParametres[I + Compteur]);
    Inc(Compteur, MyProj.Nb_ParamInteger);
    for I := 0 to MyProj.Nb_ParamAngle - 1   do MyProj.ParamAngle[I]   := Trim(qParametres[I + Compteur])+'d';
    Inc(Compteur, MyProj.Nb_ParamAngle);
    for I := 0 to MyProj.Nb_ParamBoolean - 1 do MyProj.ParamBoolean[I] := (qParametres[I + Compteur] = '1');
    MyProj.Valid := True;
    fLesProjection.Add(MyProj);
  except
    MyProj.Free;
    AfficherMessage(Format('Erreur dans %s.AddProjectionByParameters', [ClassName]));
  end;
end;

// Multiples
function TConversionSysteme.GetNbMultiples: integer;
begin
  Result := fLesMultiple.Count;
end;

function TConversionSysteme.GetMultiple(const Idx: integer): TMultiple;
begin
  Result := fLesMultiple.Items[Idx] as TMultiple;
end;

function TConversionSysteme.GetIdxMultipleByName(const qNom: string): integer;
var
  i: Integer;
  EWE: TMultiple;
begin
  Result := -1;
  for i := 0 to GetNbMultiples - 1 do
  begin
    EWE := GetMultiple(i);
    if (LowerCase(EWE.Nom) = LowerCase(qNom)) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;
function TConversionSysteme.GetIdxMultipleByCodeEPSG(const C: integer): integer;
var
  i: Integer;
  EWE: TMultiple;
begin
  Result := -1;
  for i := 0 to GetNbMultiples - 1 do
  begin
    EWE := GetMultiple(i);
    if (EWE.CodeEPSG = C) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;


procedure TConversionSysteme.AddMultipleByParameters(const qCodeEPSG: integer;
                                                     const qNatureMultiple, qNom, qDescription, qDatum: string;
                                                     const qParametres: TGHStringArray);
var
  MyMultiple: TMultiple;
  WU: Integer;
  Compteur: Integer;
  i: Integer;
begin
  try
    MyMultiple := CreateMultiple(qNatureMultiple);
    //                  0                       0       1               2       3       4       5
    //Projection	Lambert_Conique_Secant=	LT93	Lambert-93	WGS84	700000	6600000	3	46.50	44	49
    MyMultiple.Nom          := qNom;
    MyMultiple.Description  := qDescription;
    MyMultiple.CodeEPSG     := qCodeEPSG;
    WU := GetIdxDatumByName(qDatum);

    if (WU = -1) then Raise ERangeError.CreateFmt('Datum %s inconnu dans la projection %s',  [qDatum, qNom]);
    MyMultiple.Datum := GetDatum(WU);
    // décodage des paramètres
    Compteur := 0;
    for i := 0 to MyMultiple.Nb_ParamReal - 1    do MyMultiple.ParamReal[i]    := StrToFloatDef(qParametres[i], 0.00);
    Inc(Compteur, MyMultiple.Nb_ParamReal);
    for I := 0 to MyMultiple.Nb_ParamInteger - 1 do MyMultiple.ParamInteger[I] := StrToInt(qParametres[I + Compteur]);
    Inc(Compteur, MyMultiple.Nb_ParamInteger);
    for I := 0 to MyMultiple.Nb_ParamAngle - 1   do MyMultiple.ParamAngle[I]   := Trim(qParametres[I + Compteur])+'d';
    Inc(Compteur, MyMultiple.Nb_ParamAngle);
    for I := 0 to MyMultiple.Nb_ParamBoolean - 1 do MyMultiple.ParamBoolean[I] := (qParametres[I + Compteur] = '1');
    MyMultiple.Valid := True;
    fLesMultiple.Add(MyMultiple);
  except
    MyMultiple.Free;
    AfficherMessage(Format('Erreur dans %s.MultipleByParameters', [ClassName]));
  end;
end;
//------------------------------------------------------------------------------

function TConversionSysteme.VersGeocentriqueNum(const IndiceSource: integer;
                                                const EPSGSource  : integer;
                                                const XSource, YSource : double;
                                                var XGeo, YGeo, ZGeo   : double) : boolean;
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
  Idx : integer;
  Echec : boolean;
  Lat, Long : real;
  MyDatum      : TDatum;
  MyProjection : TProjection;
  MyMultiple   : TMultiple;

begin
  Echec := true;
  Idx   := GetIndexSysteme(MD_DATUM, EPSGSource);
  if (Idx <> -1) then
  begin // c'est un datum
    MyDatum := GetDatum(Idx);
    MyDatum.VersCartesien(XSource, YSource, (IndiceSource=1), XGeo,YGeo, ZGeo);
    Echec:=false;
  end
  else
  begin // c'est une projection (simple ou multiple)
    Idx := GetIndexSysteme(MD_PROJECTION, EPSGSource);
    if (Idx <> -1) then
    begin // c'est une projection simple
      MyProjection := GetProjection(Idx);
      MyProjection.VersLatLong(XSource, YSource, Lat, Long);
      MyProjection.Datum.VersCartesien(Lat, Long, false, XGeo, YGeo, ZGeo);
      Echec:=false;
    end
    else
    begin // ça devrait être une projection multiple
      Idx := GetIndexSysteme(MD_MULTIPLE, EPSGSource);
      if (Idx <> -1) then
      begin
        MyMultiple := GetMultiple(Idx);
        MyMultiple.VersLatLong(XSource, YSource, IndiceSource, Lat, Long);
        MyMultiple.Datum.VersCartesien(Lat, Long, false, XGeo, YGeo, ZGeo);
        Echec:=false;
      end;
    end;
  end;
  result:=not Echec;
end;



function TConversionSysteme.DepuisGeocentriqueNum(const XGeo, YGeo, ZGeo: double;
                                                  const EPSGDest: integer;
                                                  var IndiceDest: integer;
                                                  var XDest, YDest: double): boolean;
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
  Idx : integer;
  Echec : boolean;
  Lat, Long : real;
  MyDatum      : TDatum;
  MyProjection : TProjection;
  MyMultiple   : TMultiple;
begin
  Echec:=true;
  Idx  := GetIndexSysteme(MD_DATUM, EPSGDest);
  if (Idx <> -1) then
  begin // c'est un datum
    MyDatum := GetDatum(Idx);
    MyDatum.VersLatLong(XGeo, YGeo, ZGeo, (IndiceDest=1), XDest, YDest);
    Echec:=false;
  end
  else
  begin // c'est une projection (simple ou mutiple)
    Idx := GetIndexSysteme(MD_PROJECTION, EPSGDest);
    if (Idx <> -1) then
    begin // c'est une projection simple
      MyProjection := GetProjection(Idx);
      MyProjection.Datum.VersLatLong(XGeo, YGeo, ZGeo, false, Lat, Long);
      MyProjection.VersGrille(Lat, Long, XDest, YDest);
      Echec:=false;
    end
    else
    begin // ça devrait être une projection multiple
      Idx := GetIndexSysteme(MD_MULTIPLE, EPSGDest);
      if (Idx <> -1) then
      begin
        MyMultiple := GetMultiple(Idx);
        MyMultiple.Datum.VersLatLong(XGeo, YGeo, ZGeo, false, Lat, Long);
        MyMultiple.VersGrille(Lat, Long, XDest, YDest, IndiceDest);
        Echec:=false;
      end;
    end;
  end;
  result:=not Echec;
end;



function TConversionSysteme.ConversionNum(const IndiceSource : integer;
                                          const EPSGSource, EPSGDest : integer;
                                          const XSource, YSource : double;
                                          var IndiceDest : integer;
                                          var XDest, YDest : double) : boolean;
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
  result:= VersGeocentriqueNum(IndiceSource, EPSGSource, XSource, YSource, Xcart, Ycart, Zcart)
           and
           DepuisGeocentriqueNum(Xcart, Ycart, Zcart, EPSGDest, IndiceDest, XDest, YDest);
end;

//******************************************************************************
function TConversionSysteme.GenererListeDesSystemes: boolean;
var
  LS: TStringList;
  i: Integer;
  EWE: TGHStringArray;
  WU: String;
  Nb: Integer;
  DDD: TDatum;
  PPP: TProjection;
  MMM: TMultiple;
begin
  AfficherMessage(Format('%s.GenererListeDesSystemes', [ClassName]));
  AfficherMessage('---------------------------------------------------');
  result := False;
  SetLength(FListeDesSystemes, 0);
  LS := TStringList.Create;
  try
    LS.Clear;
    // on ne charge pas les ellipsoides
    // ici, on ne crée pas de variables intermédiaires pour récupérer un simple nom
    Nb := GetNbDatums;
    // AfficherMessage(Format('-- Datums: %d', [Nb]));
    for i:= 0 to Nb - 1 do
    begin
      DDD := GetDatum(i);
      //// AfficherMessage(Format('%d: %d: %s', [i, DDD.CodeEPSG, DDD.Nom]));
      LS.Add(Format('%d: %s', [DDD.CodeEPSG, DDD.Nom]));
    end;
    // AfficherMessage('');
    Nb := GetNbProjections;
    // AfficherMessage(Format('-- Projs: %d', [Nb]));
    for i:= 0 to Nb - 1 do
    begin
      PPP := GetProjection(i);
      //// AfficherMessage(Format('%d: %d: %s', [i, PPP.CodeEPSG, PPP.Nom]));
      LS.Add(Format('%d: %s', [PPP.CodeEPSG, PPP.Nom]));

    end;
    // AfficherMessage('');
    Nb := GetNbMultiples;
    // AfficherMessage(Format('-- Multiples: %d', [Nb]));
    for i:= 0 to Nb - 1   do
    begin
      MMM := GetMultiple(i);
      // AfficherMessage(Format('%d: %d: %s', [i, MMM.CodeEPSG, MMM.Nom]));
      LS.Add(Format('%d: %s', [MMM.CodeEPSG, MMM.Nom]));

      // AfficherMessage(WU);
    end;
    // AfficherMessage(Format('-- Systemes: %d', [LS.Count]));

    // tableau des systèmes
    FNbreSystemes := LS.Count;
    SetLength(FListeDesSystemes, FNbreSystemes);
    // AfficherMessage(Format('-- Systemes: %d', [LS.Count]));
    for i:= 0 to FNbreSystemes - 1 do
    begin
      // AfficherMessage(' --- ' + LS.Strings[i]);
      EWE := split(LS.Strings[i], ':');
      FListeDesSystemes[i].CodeEPSG := StrToIntDef(Trim(EWE[0]), -1);
      FListeDesSystemes[i].Nom  := Trim(EWE[1]);
    end;
    // vidage
    LS.Clear;
    //--------------
    Result := True;
    // AfficherMessage('---------------------------------------------------');
    // AfficherMessage(Format('%s.GenererListeDesSystemes OK', [ClassName]));
    // AfficherMessage('---------------------------------------------------');
    Exit;
  finally
    LS.Free;
  end;
end;

function TConversionSysteme.GetNbSystemes(): integer;
begin
  Result := FNbreSystemes;
end;

function TConversionSysteme.GetCodeEPSGNomSysteme(const Idx: integer): TLabelSystemesCoordsEPSG;
begin
  Result := FListeDesSystemes[Idx];
end;

function TConversionSysteme.FindCodeEPSGNomSysteme(const C: integer): TLabelSystemesCoordsEPSG;
var
  EWE: TLabelSystemesCoordsEPSG;
  i: Integer;
begin
  Result.CodeEPSG := -1;
  Result.Nom  := 'Not found';
  for i := 0 to GetNbSystemes - 1 do
  begin
    EWE := GetCodeEPSGNomSysteme(i);
    if (EWE.CodeEPSG = C) then
    begin
      Result := EWE;
      exit;
    end;
  end;
end;


function TConversionSysteme.GetNumeroFuseauUTM(const Latitude, Longitude: double): integer;
var
  f: Integer;
begin
  result := -1;
  // UTM non défini au-delà de +/- 80°
  if (Abs(Latitude) > 80) then Exit;
  f := 1 + trunc((180 + Longitude) / 6);
  Result := f;

end;
(*
function TConversionSysteme.GetIndexSystemeByCodeEPSG(const C: integer): integer;
var
  i, Nb: Integer;
  EWE: TLabelSystemesCoordsEPSG;
begin
  Result := -1;
  Nb := GetNbSystemes;
  for i := 0 to Nb-1 do
  begin
    EWE := GetCodeEPSGNomSysteme(i);
    if (EWE.CodeEPSG = C) then
    begin
      Result := i;
      exit;
    end;
  end;
end;
//*)


function TConversionSysteme.ConversionSyst1ToSyst2Index(const SystSrc, SystCible: integer; const MyPoint: TProjUV): TProjUV;
var
  QSrc, QCible: TLabelSystemesCoordsEPSG;
begin
  QSrc   := GetCodeEPSGNomSysteme(SystSrc);
  QCible := GetCodeEPSGNomSysteme(SystCible);
  Result := ConversionSyst1ToSyst2EPSG(QSrc.CodeEPSG, QCible.CodeEPSG, MyPoint);
end;

function TConversionSysteme.ConversionSyst1ToSyst2EPSG(const SystSrc, SystCible: integer; const MyPoint: TProjUV): TProjUV;
var
  IndiceSource, IndiceDest : integer;
  EWE : TProjUV;
begin
  EWE.U := MyPoint.U;
  EWE.V := MyPoint.V;

  IndiceSource := 0;
  IndiceDest   := 0;
  // passage en radians si on est en WGS84
  if (SystSrc = CODE_EPSG_WGS84) then
  begin
    EWE.U := MyPoint.U * DEG_TO_RAD;
    EWE.V := MyPoint.V * DEG_TO_RAD;
  end;
  ConversionNum(IndiceSource,
                SystSrc,
                SystCible,
                EWE.U, EWE.V,
                IndiceDest,
                Result.U, Result.V);

  // passage en degrés
  if (SystCible = CODE_EPSG_WGS84) then
  begin
    Result.U := RAD_TO_DEG * Result.U;
    Result.V := RAD_TO_DEG * Result.V;
  end;
end;

procedure TConversionSysteme.ReconstruireProjectionsDepuisDonneesEnDur();
CONST
  DUMMY = 0.00;
var
  EWE: TGHStringArray;
  i: Integer;
  Nb: Integer;
  EEE: TEllipsoide;
  DDD: TDatum;
  MMM: TMultiple;
  PPP: TProjection;
  function SetParamsEWE(const P0, P1, P2, P3, P4, P5: double): TGHStringArray;
  var
    n: Integer;
  begin
    for n := Low(Result) to High(Result) do Result[n] := '';
    Result[0] := Format('%.10f', [P0]);
    Result[1] := Format('%.10f', [P1]);
    Result[2] := Format('%.10f', [P2]);
    Result[3] := Format('%.10f', [P3]);
    Result[4] := Format('%.10f', [P4]);
    Result[5] := Format('%.10f', [P5]);
    //Result[6] := Format('%.d', [QEPSG]);
  end;
begin
  AfficherMessage('ReconstruireProjectionsDepuisDonneesEnDur');
  // Ellipsoides
  //------------
  AddEllipsoideByParameters(0, 'Clarke80', 'Clarke 1880', 6378249.2, 0.0824832568);
  AddEllipsoideByParameters(0, 'Hayford09', 'Hayford 1909', 6378388.0, 0.0819918900);
  AddEllipsoideByParameters(0, 'Plessis', 'Plessis 1840', 6376523.0, 0.0804334508);
  // datums
  //------------
  AddDatumByParameters(0, 'NTF' , 'Nouvelle Triangulation Francaise', 'Clarke80'  ,  -168, -60 ,  320, 0, 0, 0, 1.00, '*');
  AddDatumByParameters(0, 'ED50', 'European Datum'                  , 'Hayford 09',  -84 , -97 , -117, 0, 0, 0, 1.00, '*');
  AddDatumByParameters(0, 'ATG' , 'Ancienne Triangulation Francaise', 'Plessis'   , 1118,   23 ,   66, 0, 0, 0, 1.00, '*');
  //AfficherMessage('-->002');
  // projections
  //------------

  // Projection      Lambert Conique Secant     LT93               Lambert-93           WGS84   700000  6600000  3  46.50  44  49 # NE PAS MODIFIER !
  EWE := SetParamsEWE(700000.00, 6600000.00, 3.00, 46.50, 44.00, 49.00);
  AddProjectionByParameters(CODE_EPSG_LAMBERT_93 , 'Lambert Conique Secant', 'LT93', 'Lambert-93', 'WGS84', EWE);

  // Projection  Lambert Conique Tangent    LT1                Lambert I Carto      NTF     0.99987734  600000  1200000  2.3372291667	49.5
  EWE := SetParamsEWE(0.99987734, 600000.00, 1200000.00,  2.3372291667, 49.50, DUMMY);
  AddProjectionByParameters(CODE_EPSG_LAMBERT_I  , 'Lambert Conique Tangent', 'LT1',             'Lambert I Carto', 'NTF', EWE);

  // Projection  Lambert Conique Tangent    LT2                Lambert II Carto     NTF     0.99987742  600000   200000  2.3372291667	46.8
  EWE := SetParamsEWE(0.99987742, 600000.00, 2200000.00, 2.3372291667, 46.80, DUMMY);
  AddProjectionByParameters(CODE_EPSG_LAMBERT_II , 'Lambert Conique Tangent', 'LT2'            , 'Lambert II Carto' , 'NTF', EWE);

  // Projection  Lambert Conique Tangent    LT3                Lambert III Carto    NTF     0.99987750  600000  3200000  2.3372291667	 44.1
  EWE := SetParamsEWE(0.99987750, 600000.00, 3200000.00, 2.3372291667, 44.10, DUMMY);
  AddProjectionByParameters(CODE_EPSG_LAMBERT_III, 'Lambert Conique Tangent', 'LT3'            , 'Lambert III Carto', 'NTF', EWE);

  // Projection  Lambert Conique Tangent    LT2+               Lambert II etendu    NTF     0.99987742  600000  2200000  2.3372291667
  EWE := SetParamsEWE(0.99987742, 600000.00, 2200000.00, 2.3372291667, 44.10, DUMMY);
  AddProjectionByParameters(CODE_EPSG_LAMBERT_II , 'Lambert Conique Tangent', 'LT2+'           , 'Lambert II etendu', 'NTF'  , EWE);

  // Projection  Lambert Conique Tangent    LT4                Lambert IV Corse     NTF     0.99994471  234.358  185861.369  2.3372291667  42.165
  EWE := SetParamsEWE(0.99994471, 234.358, 185861.369, 2.3372291667, 42.165, DUMMY);
  AddProjectionByParameters(CODE_EPSG_LAMBERT_IV , 'Lambert Conique Tangent', 'LT4'            , 'Lambert IV Corse' , 'NTF', EWE);
  //AfficherMessage('-->003');
  // Projections 9 zones CC  // début à EPSG:3942
  for i := 2 to 10 do
  begin
    // Projection	Lambert Conique Secant	CC50	Conique Conforme Zone 9	WGS84	1700000	9200000	3	50	49.25	50.75
    EWE := SetParamsEWE(1700000,
                        (i - 1) * 1000000.0 + 200000.0,
                        3.00,
                        40.0 + i,
                        40.25 + (i-1),
                        40.75 + i
                        );
    AddProjectionByParameters(CODE_EPSG_CC_ZONE_0 + i, 'Lambert Conique Secant', Format('CC%d', [40+i]), Format('Conique Conforme Zone %d', [i - 1]), 'WGS84', EWE);
  end;
  //AfficherMessage('-->004');
  // Projections Multiples dont UTM
  EWE := SetParamsEWE(0.9996, 500000.0, 0.0, 10000000.0, -177.0, 6.0);
  AddMultipleByParameters(32600, 'Transverse Mercator Multiple', 'UTM'     , 'Universal Transverse Mercator', 'WGS84', EWE);
  //AfficherMessage('-->005');
  // fuseaux UTM
  // Les numéros de fuseaux UTM vont de 2 à 60. Pas d'UTM01
  // Hémisphère Nord
  for i := 2 to 60 do
  begin
    EWE := SetParamsEWE(0.9996,
                        500000.0, // X0
                        1.0,
                        0.0,      // Y0: origine à l'équateur
                        -177.0 + 6.0 * (i),
                        6.0
                        );

    AddMultipleByParameters(CODE_EPSG_UTM_ZONE_0_NORTH + (i),
                            'Transverse Mercator Multiple',
                            Format('UTM%dN', [i]),
                            'Universal Transverse Mercator', 'WGS84',
                            EWE);
  end;

  // Hémisphère Sud -- DONE: Revoir les paramètres
  for i := 2 to 60 do
  begin
    EWE := SetParamsEWE(0.9996,
                        500000.0,   // X0
                        0.0,
                        10000000.0, // Y0 = 10.000.000 m à l'équateur
                        -177.0 + 6.0 * (i),
                        6.0
                        );

    AddMultipleByParameters(CODE_EPSG_UTM_ZONE_0_SOUTH + (i),
                            'Transverse Mercator Multiple',
                            Format('UTM%dS', [i]),
                            'Universal Transverse Mercator', 'WGS84',
                            EWE);
  end;
  // Projection Google
  //      +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs
  //AddProjectionByParameters(CODE_EPSG_GOOGLE , 'Lambert Conique Secant', 'LT93', 'Lambert-93', 'WGS84', EWE);
  // Contrôles: on liste
  {
  AfficherMessage('======================');
  Nb := GetNbEllipsoides;
  AfficherMessage(Format('%d ellipsoides', [Nb]));
  for i := 0 to Nb - 1 do
  begin
    EEE := GetEllipsoide(i);
    AfficherMessage(Format('%d: %d: %s', [i, EEE.CodeEPSG, EEE.Nom]));
  end;
  AfficherMessage('');

  Nb := GetNbDatums;
  AfficherMessage(Format('%d datums', [Nb]));
  for i := 0 to Nb - 1 do
  begin
    DDD := GetDatum(i);
    AfficherMessage(Format('%d: %d: %s', [i, DDD.CodeEPSG, DDD.Nom]));
  end;
  AfficherMessage('');
  Nb := GetNbMultiples;
  AfficherMessage(Format('%d multiples', [Nb]));
  for i := 0 to Nb - 1 do
  begin
    MMM := GetMultiple(i);
    AfficherMessage(Format('%d: %d: %s', [i, MMM.CodeEPSG, MMM.Nom]));
  end;

  AfficherMessage('');
  Nb := GetNbProjections;
  AfficherMessage(Format('%d projections', [Nb]));
  for i := 0 to Nb - 1 do
  begin
    PPP := GetProjection(i);
    AfficherMessage(Format('%d: %d: %s', [i, PPP.CodeEPSG, PPP.Nom]));
  end;

  AfficherMessage('');

  //*}

  AfficherMessage(Format('%d ellipsoides', [GetNbEllipsoides()]));
  AfficherMessage(Format('%d datums', [GetNbDatums()]));
  AfficherMessage(Format('%d multiples', [GetNbMultiples()]));
  AfficherMessage(Format('%d projections', [GetNbProjections()]));
  AfficherMessage('ReconstruireProjectionsDepuisDonneesEnDur OK');
end;
//******************************************************************************
// test avec un couple de coordonnées
function TConversionSysteme.Tester(): boolean;
var
  EWE_WGS84, EWE_LT93, EWE_UTM30, EWE_UTM31: TProjUV;
  WU: TProjUV;
  procedure miou(const S: string; const P: TProjUV);
  begin
    AfficherMessageErreur(Format(' -- %s: %.6f, %.6f', [S, P.U, P.V]));
  end;
  function miouXY(const S: string; const P: TProjUV; const QX, QY: double): double;
  var
     Delta: TProjUV;
  begin
    Delta.U := QX - P.U;
    Delta.V := QY - P.V;
    AfficherMessageErreur(Format(' -- %s: %.6f, %.6f (expected: %.2f, %.2f) (delta: %.2f, %.2f)', [S, P.U, P.V, QX, QY, Delta.U, Delta.V]));
  end;
begin
  //AfficherMessage(Format('%s.Tester', [ClassName]));
  Result := False;
  (*
  Valeurs attendues pour l’auto-test du convertisseur
  Point    Grottes de Tramebernède    X    Y
  Origine    LT3    403460    3089600
  Pivot    WGS84    43,08031927    -0,07713723
  Cible    LT93    449274,577866    6225000,21506
  Cible    UTM30 (WGS84)    737934,246355    4773881,2728
  Cible    UTM31 (WGS84)    249506,314178    4774330,84958
  //*)
  WU.U := 403460.00;   WU.V := 3089600.00;
  AfficherMessageErreur(Format('Fuseau UTM: %d', [GetNumeroFuseauUTM(EWE_WGS84.U, EWE_WGS84.V)]));
  AfficherMessageErreur('Avec EPSG');
  AfficherMessageErreur('*********');

  EWE_WGS84 := ConversionSyst1ToSyst2EPSG(CODE_EPSG_LAMBERT_III, CODE_EPSG_WGS84, WU);
  EWE_LT93  := ConversionSyst1ToSyst2EPSG(CODE_EPSG_LAMBERT_III, CODE_EPSG_LAMBERT_93 , WU);
  EWE_UTM30 := ConversionSyst1ToSyst2EPSG(CODE_EPSG_LAMBERT_III, CODE_EPSG_UTM_ZONE_0_NORTH , WU);

  //EWE_UTM31 := ConversionSyst1ToSyst2('UTM', 'LT3', EWE_UTM30);
  miouXY('Original: LT3', WU        , WU.U, WU.V);
  miou  ('Pivot: WGS84' , EWE_WGS84);
  miouXY('Cible: LT93'  , EWE_LT93  , 449274.577866, 6225000.21506);
  miouXY('Cible: UTM'   , EWE_UTM30 , 737934.246355, 4773881.2728 );
  Result := True;
end;
//******************************************************************************
procedure TConversionSysteme.LoadFromFichier(const Filename: string); deprecated;
var
  LS: TStringList;
  EWE, WU: TGHStringArray;
  i, n: integer;
begin
  //AfficherMessage('-- Charger depuis : ' + Filename);
  LS := TStringList.Create;
  try
    LS.Clear;
    LS.LoadFromFile(Filename);
    if (LS.Count = 0) then
      Exit;
    for i := 0 to LS.Count - 1 do
    begin
      EWE := Split(Trim(LS.Strings[i]), #9);
      case IndexOfString(EWE[0], ['ellipsoide', 'datum', 'projection', 'multiple']) of
        0:
        begin
          // # ELLIPSOIDES
          //Nom  Description  a  e
          //Ellipsoide  Clarke80  Clarke_1880  6378249.2  0.0824832568
          AddEllipsoideByParameters(-1,
            EWE[1],
            EWE[2],
            StrToFloatDef(EWE[3], -1.00),
            StrToFloatDef(EWE[4], -1.00));
        end;
        1:
        begin
          //# DATUMS
          //#   Nom  Description  Ellipsoide  TX  TY  TZ  RX  RY  RZ  Scale  Grille de correction
          //Datum  NTF  Nouvelle Triangulation Francaise  Clarke80  -168  -60  320  0  0  0  1.00  *
          AddDatumByParameters(-1, EWE[1], EWE[2], EWE[3],
            StrToFloatDef(EWE[4], 0.00),
            StrToFloatDef(EWE[5], 0.00),
            StrToFloatDef(EWE[6], 0.00),
            StrToFloatDef(EWE[7], 0.00),
            StrToFloatDef(EWE[8], 0.00),
            StrToFloatDef(EWE[9], 0.00),
            StrToFloatDef(EWE[10], 1.00),
            EWE[11]);
        end;
        2:
        begin
          for n := 0 to high(WU) do WU[n] := '';
          for n := 5 to High(EWE) do WU[n-5] := EWE[n];
          AddProjectionByParameters(-1, EWE[1], EWE[2], EWE[3], EWE[4], WU);
        end;
        3:
        begin
          for n := 0 to high(WU) do WU[n] := '';
          for n := 5 to High(EWE) do WU[n-5] := EWE[n];
          AddMultipleByParameters(-1, EWE[1], EWE[2], EWE[3], EWE[4], WU);
        end;
        else
        begin // ignorer
          ;
        end;
       end;
    end;
  finally
    LS.Free;
  end;
end;

// régénération du fichier projections
function TConversionSysteme.RegenererFichierProjections(const FP: string): boolean;
CONST
  DUMMY = 0;
var
  F: TextFile;
  i: Integer;
  procedure WrtLn(const S: string);
  begin
    WriteLn(F, S);
  end;

  procedure WrtCommentaires(const S: string);
  begin
    WrtLn('# ' + S);
  end;

  procedure WrtEnTetes(const A: array of string);
  var
    EWE: String;
    i: Integer;
  begin
    EWE := '';
    for i := Low(A) to High(A) do EWE := EWE + A[i] + #9;
    EWE := Trim(EWE);
    WriteLn(F, EWE);
  end;
  procedure WrtEllipoide(const QNom, QDesc: string; const Qa, Qe: double);
  var
    EWE: String;
  begin
    EWE := Format('Ellipsoide' + #9 + '%s' + #9 + '%s' + #9 + '%.2f' + #9 + '%.10f',
                  [QNom, QDesc, Qa, Qe]);
    WrtLn(EWE);
  end;
  procedure WrtDatum(const QNom, QDescription, QEllipsoide: string;
                     const QTX, QTY, QTZ, QRX, QRY, QRZ, QScale: double;
                     const QGrilleCorrection: string);
  var
    EWE: String;
  begin
    EWE := Format('Datum' + #9 + '%s' + #9 + '%s' + #9 + '%s' + #9 +
                  '%.2f' + #9 + '%.2f' + #9 + '%.2f' + #9 +
                  '%.2f' + #9 + '%.2f' + #9 + '%.2f' + #9 +
                  '%.2f' + #9 + '%s',
                 [QNom, QDescription, QEllipsoide,
                  QTX, QTY, QTZ,
                  QRX, QRY, QRZ,
                  QScale, QGrilleCorrection]);
    WrtLn(EWE);
  end;
  procedure WrtProjection(const QNom, QCode, QDesc, QDatum: string; const P0, P1, P2, P3, P4, P5: double; const CodeEPSG: integer);
  var
    EWE: String;
  begin
    EWE := Format('Projection' + #9 + '%s' + #9 + '%s' + #9 + '%s' + #9 +  '%s' + #9 +
                  '%.10f' + #9 + '%.10f' + #9 +
                  '%.10f' + #9 + '%.10f' + #9 +
                  '%.10f' + #9 + '%.10f' + #9 +
                  '%d',
                  [QNom, QCode, QDesc, QDatum,
                   P0, P1, P2, P3, P4, P5,
                   CodeEPSG
                  ]);
    WrtLn(EWE);
  end;
  procedure WrtProjectionCC(const QNom, QCode, QDesc, QDatum: string; const P0, P1, P2, P3, P4, P5: double; const CodeEPSG: integer);
  var
    EWE: String;
  begin
    EWE := Format('Projection' + #9 + '%s' + #9 + '%s' + #9 + '%s' + #9 +  '%s' + #9 +
                  '%.10f' + #9 + '%.10f' + #9 +
                  '%.10f' + #9 + '%.10f' + #9 +
                  '%.10f' + #9 + '%.10f' + #9 +
                  '%d',
                  [QNom, QCode, QDesc, QDatum,
                   P0, P1, P2, P3, P4, P5,
                   CodeEPSG
                  ]);
    WrtLn(EWE);
  end;
  procedure WrtProjectionLT93(const QNom, QCode, QDesc, QDatum: string; const P0, P1, P2, P3, P4, P5: double; const CodeEPSG: integer);
  var
    EWE: String;
  begin
    //Projection	Lambert Conique Secant	LT93	Lambert-93	WGS84
    (*
    700000
    6600000
    3
    46.50
    44.8166666667
    0	# NE PAS MODIFIER !
    //*)
    EWE := Format('Projection' + #9 + '%s' + #9 + '%s' + #9 + '%s' + #9 +  '%s' + #9 +
                  '%.10f' + #9 + '%.10f' + #9 +
                  '%.10f' + #9 + '%.10f' + #9 +
                  '%.10f' + #9 + '%.10f' + #9 +
                  '%d',
                  [QNom, QCode, QDesc, QDatum,
                   P0, P1, P2, P3, P4, P5,
                   CodeEPSG
                  ]);
    WrtLn(EWE);
  end;
  procedure WrtFuseauUTM(const TypeProj, Systeme, Description, Datum: string;
                         const P0, P1, P2, P3, P4, P5: double; const CodeEPSG: integer);
  var
    EWE: String;
  begin
    //Multiple	Transverse Mercator Multiple	UTM	Universal Transverse Mercator	WGS84	0.9996	500000	0	10000000	-177	6
    EWE := Format('Multiple' + #9 + '%s' + #9 + '%s' + #9 + '%s' + #9 +  '%s' + #9 +
                  '%.10f' + #9 + '%.10f' + #9 +
                  '%.10f' + #9 + '%.10f' + #9 +
                  '%.10f' + #9 + '%.10f' + #9 +
                  '%d',
                  [TypeProj, Systeme, Description, Datum,
                   P0, P1, P2, P3, P4, P5,
                   CodeEPSG
                  ]);
    WrtLn(EWE);
  end;
begin
  Result := False;
  AfficherMessage(Format('%s.RegenererFichierProjections: %s', [ClassName, FP]));
  AssignFile(F, FP);
  try
    ReWrite(F);
    WrtCommentaires('ELLIPSOIDES');
    WrtEnTetes(['#', 'Nom'   , 'Description' , 'a'      , 'e']);
    //Ellipsoide  Clarke80      Clarke 1880    6378249.2  0.0824832568
    WrtEllipoide('Clarke80'  , 'Clarke 1880' , 6378249.2, 0.0824832568);
    //Ellipsoide  Hayford09     Hayford 1909   6378388    0.08199189
    WrtEllipoide('Hayford09' , 'Hayford 1909', 6378388.0, 0.0819918900);
    //Ellipsoide  Plessis       Plessis 1840   6376523    0.0804334508
    WrtEllipoide('Plessis'   , 'Plessis 1840', 6376523.0, 0.0804334508);
    WrtLn('');
    WrtCommentaires('DATUMS');
    WrtEnTetes(['#', 'Nom'   , 'Description' , 'Ellipsoide', 'TX', 'TY', 'TZ', 'RX', 'RY', 'RZ', 'Scale', 'Grille de correction']);
    WrtDatum('NTF' , 'Nouvelle Triangulation Francaise', 'Clarke80'  ,  -168, -60 ,  320, 0, 0, 0, 1.00, '*'); // Bug fixé: Paramètre 04 (ancienne valeur: +60; valeur correcte: -60)
    WrtDatum('ED50', 'European Datum'                  , 'Hayford 09',  -84 , -97 , -117, 0, 0, 0, 1.00, '*');
    WrtDatum('ATG' , 'Ancienne Triangulation Francaise', 'Plessis'   , 1118,   23 ,   66, 0, 0, 0, 1.00, '*');
    WrtLn('');
    WrtCommentaires('PROJECTIONS');
    WrtEnTetes(['#', 'Nom'   , 'Code', 'Description', 'Datum' , 'P0', 'P1', 'P2', 'P3', 'P4', 'P5']);

    // Projection      Lambert Conique Secant     LT93               Lambert-93           WGS84   700000  6600000  3  46.50  44  49 # NE PAS MODIFIER !
    WrtProjectionLT93('Lambert Conique Secant ', 'LT93'           , 'Lambert-93'       , 'WGS84', 700000, 6600000, 3, 46.50, 44, 49, CODE_EPSG_LAMBERT_93);
    // Projection  Lambert Conique Tangent    LT1                Lambert I Carto      NTF     0.99987734  600000  1200000  2.3372291667	49.5
    WrtProjection('Lambert Conique Tangent', 'LT1'            , 'Lambert I Carto'  , 'NTF'  , 0.99987734, 600000, 1200000, 2.3372291667, 49.5, DUMMY, CODE_EPSG_LAMBERT_I);
    // Projection  Lambert Conique Tangent    LT2                Lambert II Carto     NTF     0.99987742  600000   200000  2.3372291667	46.8
    WrtProjection('Lambert Conique Tangent', 'LT2'            , 'Lambert II Carto' , 'NTF'  , 0.99987742, 600000, 2200000, 2.3372291667, 46.8, DUMMY, CODE_EPSG_LAMBERT_II);
    // Projection  Lambert Conique Tangent    LT3                Lambert III Carto    NTF     0.99987750  600000  3200000  2.3372291667	 44.1
    WrtProjection('Lambert Conique Tangent', 'LT3'            , 'Lambert III Carto', 'NTF'  , 0.99987750, 600000, 3200000, 2.3372291667, 44.1, DUMMY, CODE_EPSG_LAMBERT_III);
    // Projection  Lambert Conique Tangent    LT2+               Lambert II etendu    NTF     0.99987742  600000  2200000  2.3372291667
    WrtProjection('Lambert Conique Tangent', 'LT2+'           , 'Lambert II etendu', 'NTF'  , 0.99987742, 600000, 2200000, 2.3372291667, 44.1, DUMMY, CODE_EPSG_LAMBERT_II);
    // Projection  Lambert Conique Tangent    LT4                Lambert IV Corse     NTF     0.99994471  234.358  185861.369  2.3372291667  42.165
    WrtProjection('Lambert Conique Tangent', 'LT4'            , 'Lambert IV Corse' , 'NTF'  , 0.99994471, 234.358, 185861.369, 2.3372291667, 42.165, DUMMY, CODE_EPSG_LAMBERT_IV);
    // Projections 9 zones CC  // début à EPSG:3942
    for i := 2 to 10 do
    begin
      // Projection	Lambert Conique Secant	CC50	Conique Conforme Zone 9	WGS84	1700000	9200000	3	50	49.25	50.75
      WrtProjectionCC('Lambert Conique Secant', Format('CC%d', [40+i]), Format('Conique Conforme Zone %d', [i - 1]), 'WGS84',
                      1700000,
                      (i - 1) * 1000000 + 200000,
                      3,
                      40 + i,
                      40.25 + (i-1),
                      40.75 + i,
                      CODE_EPSG_CC_ZONE_0 + i);
    end;
    WrtLn('');
    WrtCommentaires('MULTIPLES');
    WrtEnTetes(['#', 'TypeProj'   , 'Systeme', 'Description', 'Datum' , 'P0', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6']);
    WrtFuseauUTM('Transverse Mercator Multiple', 'UTM'     , 'Universal Transverse Mercator'            , 'WGS84', 0.9996, 500000, 0, 10000000,   -177,  6, 32600);

    for i := 2 to 60 do
      WrtFuseauUTM('Transverse Mercator Multiple', Format('UTM%d', [i]), 'Universal Transverse Mercator', 'WGS84', 0.9996, 500000, 1, 0, -177 + 6 * (i - 1), 6, 32600 + (i-1));
//    WrtFuseauUTM('Transverse Mercator Multiple', 'UTM ED50', 'Universal Transverse Mercator'            , 'WGS84', 0.9996, 500000, 0, 10000000,   -177,  6, 32600);
  finally
    CloseFile(F);
  end;
end;

function TConversionSysteme.GetEPSGSystemeFromCodeEPSG(const C: integer): TLabelSystemesCoordsEPSG;
var
  EWE: TLabelSystemesCoordsEPSG;
  i: Integer;
begin
  Result.CodeEPSG := -1;
  Result.Nom  := 'Not found';
  for i := 0 to GetNbSystemes - 1 do
  begin
    EWE := GetCodeEPSGNomSysteme(i);
    if (EWE.CodeEPSG = C) then Exit(EWE);
  end;
end;
function TConversionSysteme.GetIndexSystemeByCodeEPSG(const C: integer): integer;
var
  i, Nb: Integer;
  EWE: TLabelSystemesCoordsEPSG;
begin
  Result := -1;
  Nb := GetNbSystemes;
  for i := 0 to Nb-1 do
  begin
    EWE := GetCodeEPSGNomSysteme(i);
    if (EWE.CodeEPSG = C) then
    begin
      Result := i;
      exit;
    end;
  end;
end;



end.
