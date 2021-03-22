unit datum;

// un datum s'appelle aussi un Système géodésique en français

interface

uses
  Classes,
  grille_ign, ellipsoide, donnee, type_quadri, outils, FileUtil;

const
  //AngleParisStr='2°20''14.025'''''; // angle du méridien de Paris par rapport à Greenwich
  AngleParisNum = 2.3372291667;

var
  AngleParis : real;

type

  TDatum = class
  private
    fEllipsoide : TEllipsoide; // elleipsoide du datum
    fDeltaX, fDeltaY, fDeltaZ : real; // décallage du centre de l'ellipsoïde par
                                      // rapport au centre de la terre (WGS84) en mètres
    fReduit : boolean; // transformation réduite (3 paramètres) ou complète (7 paramètres)
    fRotX, fRotY, fRotZ : real; // rotation décallage de l'ellipsoïde par
                                // rapport à WGS84 en secondes d'arc
    fScale : real; // facteur d'échelle en ppm pour passer de TRS80 à l'ellipsoïde
    fNom : string; // nom (abrégé) du datum
    fCodeEPSG: integer;
    fDescription : string; // description du datum
    fValid : boolean; // indique si les paramètres du datum sont bien définis
    fFileGrille : string; // nom du fichier contenant la grille
    fGrilleIgn : TGrilleIgn; // grille de conversion si elle existe
    fNoIgnGrille : boolean; // refus d'utiliser la grille IGN (NTF uniquement)
    procedure SetFileGrille (Value : string);
  public
    property Ellipsoide : TEllipsoide read fEllipsoide write fEllipsoide;
    property DeltaX : real read fDeltaX write fDeltaX;
    property DeltaY : real read fDeltaY write fDeltaY;
    property DeltaZ : real read fDeltaZ write fDeltaZ;
    property Reduit : boolean read fReduit write fReduit;
    property RotX : real read fRotX write fRotX;
    property RotY : real read fRotY write fRotY;
    property RotZ : real read fRotZ write fRotZ;
    property Scale : real read fScale write fScale;
    property Nom : string read fNom write fNom;
    property CodeEPSG: integer read fCodeEPSG write fCodeEPSG;
    property Description : string read fDescription write fDescription;
    property Valid : boolean read fValid write fValid;
    property FileGrille : string read fFileGrille write SetFileGrille;
    property NoIgnGrille : boolean read fNoIgnGrille write fNoIgnGrille;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream (Stream : TStream; LesDonnees : TDonnees);
    procedure SaveToStream (Stream : TStream);

    // dans les deux procedures suivantes, les coordonnées cartésiennes
    // sont relatives à WGS84
    procedure VersCartesien(Lat, Long : real; Paris : boolean; var X, Y, Z : real);
    procedure VersLatLong(X, Y, Z : real; Paris : boolean; var Lat, Long : real);
    procedure DepuisWGS84(LatWGS84, LongWGS84 : real; var Lat, Long : real);
    procedure VersWGS84(Lat, Long: real; var LatWGS84, LongWGS84 : real);
    procedure Assign(Autre : TDatum);
    // calcul le quadrillage suivant le datum actuel en utilisant les limites
    // fournies en WGS84
    procedure CalculQuadrillage(var ZoneQuadri: TZoneQuadri;
                                LatTopLeft, LongTopLeft, LatTopRight, LongTopRight,
                                LatBottomLeft, LongBottomLeft, LatBottomRight, LongBottomRight,
                                Pas: real; UniteAngle : TUniteAngle; Paris : boolean);
  end;

implementation

uses
  SysUtils, Forms,
  ign, relevement, arrondir;

constructor TDatum.Create;
begin
  inherited Create;
  fValid:=false;
  fReduit:=true;
  fFileGrille:='';
  fNoIgnGrille:=false;
end;

destructor TDatum.Destroy;
begin
  inherited Destroy;
  if fGrilleIgn<>nil then
    fGrilleIgn.Free;
end;

procedure TDatum.SetFileGrille (Value : string);
begin
  if Value<>fFileGrille then
  begin
    if fGrilleIgn<>nil then
    begin
      fGrilleIgn.Free;
      fGrilleIgn := nil;
    end;
    fFileGrille:=Value;
    if FileExists(fFileGrille) then
    begin
      fGrilleIgn:=TGrilleIgn.Create(fFileGrille);
      fNoIgnGrille := false;
    end  
    else
      fNoIgnGrille := (Nom='NTF');
  end;
end;

procedure TDatum.LoadFromStream (Stream : TStream; LesDonnees : TDonnees);
var
  Parametres : TstringList;
  I : integer;

  procedure LoadGrille(GrillePos : integer);
  var
    S : string;
  begin
    if Parametres.Count=GrillePos then
    begin
      if Parametres[GrillePos-1]='*' then
        NoIgnGrille:=true  // refus de la grille de correction
      else
        FileGrille:=Parametres[GrillePos-1]; // il y a une grille de correction
    end
    else
    begin
      if Nom='NTF' then
      begin // pour NTF, tentative de chargement de la grille par défaut
        S := AnsiToUtf8(ExtractFilePath(ParamStr(0)) + 'gr3df97a.txt');
        if FileExists(S) then
          FileGrille:=S;
      end;
    end;
  end;


begin
  Parametres:=TStringList.Create;
  fValid:=false;
  try
    DetailParam(FinLigne(Stream),Parametres);
    if not (Parametres.Count in [6,7,10,11]) then
      Raise ERangeError.Create('Nombre incorrecte de paramètre lors de la lecture d''un datum');
    Nom:=Parametres[0];
    Description:=Parametres[1];
    // recherche de l'ellipsoïde associé
    I:=0;
    while (I<LesDonnees.LesEllipsoide.Count) and
      ((LesDonnees.LesEllipsoide[I] as TEllipsoide).Nom<>Parametres[2]) do
      inc(I);
    if I>=LesDonnees.LesEllipsoide.Count then
      Raise ERangeError.CreateFmt('Ellipsoïde %s inconnu dans le datum %s',
      [Parametres[2],Parametres[0]]);
    Ellipsoide:=LesDonnees.LesEllipsoide[I] as TEllipsoide;
    DeltaX:=StrToFloat(Parametres[3]);
    DeltaY:=StrToFloat(Parametres[4]);
    DeltaZ:=StrToFloat(Parametres[5]);
    Reduit:=(Parametres.Count<=7);
    if Reduit then
      LoadGrille(7)
    else
    begin // transformation à 7 paramètres
      RotX:=StrToFloat(Parametres[6]);
      RotY:=StrToFloat(Parametres[7]);
      RotZ:=StrToFloat(Parametres[8]);
      Scale:=StrToFloat(Parametres[9]);
      LoadGrille(11);
    end;
    fValid:=true;
  finally
    Parametres.Free;
  end;
end;

procedure TDatum.SaveToStream (Stream : TStream);
var
  S : string;
begin
  S:=Format('Datum="%s" "%s" "%s" %.12g %.12g %.12g',
    [Nom, Description, Ellipsoide.Nom, DeltaX, DeltaY, DeltaZ]);
  if not Reduit then
    S:=S+Format(' %.12g %.12g %.12g %.8g',[RotX, RotY, RotZ, Scale]);
  if NoIgnGrille then
    S := S + ' *'
  else
    if FileGrille<>'' then
      S:=Format('%s "%s"',[S,FileGrille]);
  S:=S+#13#10;
  Stream.Write(S[1],length(S));
end;

procedure TDatum.Assign(Autre : TDatum);
begin
  Nom:=Autre.Nom;
  Description:=Autre.Description;
  Ellipsoide:=Autre.Ellipsoide;
  DeltaX:=Autre.DeltaX;
  DeltaY:=Autre.DeltaY;
  DeltaZ:=Autre.DeltaZ;
  Reduit:=Autre.Reduit;
  FileGrille:=Autre.FileGrille;
  if not Reduit then
  begin
    RotX:=Autre.RotX;
    RotY:=Autre.RotY;
    RotZ:=Autre.RotZ;
    Scale:=Autre.Scale;
  end;
end;


procedure TDatum.VersCartesien(Lat, Long : real; Paris : boolean; var X, Y, Z : real);
var
  h, LatWGS, LongWGS, TX, TY, TZ : real;
begin
  if Paris then
    Long:=Long+AngleParis;
  alg0009(Long, Lat, 0, Ellipsoide.a, Ellipsoide.e, X, Y, Z);
  if Reduit then
    alg0013(DeltaX, DeltaY, DeltaZ, 0, 0, 0, 0,
            X, Y, Z, X, Y, Z)
  else
    alg0013(DeltaX, DeltaY, DeltaZ, Scale/1e6,
            RotX/180/60/60*Pi, RotY/180/60/60*Pi, RotZ/180/60/60*Pi,
            X, Y, Z, X, Y, Z);
  if fGrilleIgn<>nil then // on a une grille de conversion
  begin
    alg0012(X, Y, Z, aWGS84, eWGS84, LongWGS, LatWGS, h);
    if fGrilleIgn.GetTranslation(LatWGS/Pi*180,LongWGS/Pi*180,TX,TY,TZ) then
    begin
      alg0009(Long, Lat, 0, Ellipsoide.a, Ellipsoide.e, X, Y, Z);
      alg0013(tX, TY, TZ, 0, 0, 0, 0, X, Y, Z, X, Y, Z);
    end;
  end;
end;

procedure TDatum.VersLatLong(X, Y, Z : real; Paris : boolean; var Lat, Long : real);
var
  h, LatWGS, LongWGS, TX, TY, TZ : real;
  UseGrid : boolean;
begin
  UseGrid:=false;
  if fGrilleIgn<>nil then // on a une grille de conversion
  begin
    alg0012(X, Y, Z, aWGS84, eWGS84, LongWGS, LatWGS, h);
    if fGrilleIgn.GetTranslation(LatWGS/Pi*180,LongWGS/Pi*180,TX,TY,TZ) then
    begin
      alg0013bis(TX, TY, TZ, 0, 0, 0, 0,X, Y, Z, X, Y, Z);
      UseGrid:=true;
    end;
  end;

  if not UseGrid then
    if Reduit then
      alg0013bis(DeltaX, DeltaY, DeltaZ, 0, 0, 0, 0,
              X, Y, Z, X, Y, Z)
    else
      alg0063(DeltaX, DeltaY, DeltaZ, Scale/1e6,
              RotX/180/60/60*Pi, RotY/180/60/60*Pi, RotZ/180/60/60*Pi,
              X, Y, Z, X, Y, Z);
  alg0012(X, Y, Z, Ellipsoide.a, Ellipsoide.e, Long, Lat, h);
  if Paris then
    Long:=Long-AngleParis;
end;

procedure TDatum.DepuisWGS84(LatWGS84, LongWGS84 : real; var Lat, Long : real);
var
  X, Y, Z : real;
begin
  alg0009(LongWGS84, LatWGS84, 0, aWGS84, eWGS84, X, Y, Z);
  VersLatLong(X, Y, Z, false, Lat, Long);
end;

procedure TDatum.VersWGS84(Lat, Long: real; var LatWGS84, LongWGS84 : real);
var
  X, Y, Z, h : real;
begin
  VersCartesien(Lat, Long,false, X, Y, Z);
  alg0012(X, Y, Z, aWGS84, eWGS84, LongWGS84, LatWGS84, h);
end;


procedure TDatum.CalculQuadrillage(var ZoneQuadri: TZoneQuadri;
  LatTopLeft, LongTopLeft, LatTopRight, LongTopRight, LatBottomLeft,
  LongBottomLeft, LatBottomRight, LongBottomRight,
  Pas: real; UniteAngle : TUniteAngle; Paris : boolean);
var
  Lat, Long : real;
  PasLat, PasLong : real;
  I, J, XMin, XMax, YMin, YMax : integer;
  LatMin, LatMax, LongMin, LongMax : real;

  function QuelPas(PasIndicatif, TailleRadian : real) : real;
  var
    Taille, Pas : real;

  begin
    case UniteAngle of
      uaGrade :
        begin
          Taille:=TailleRadian*Pi/200; // taille d'un grade
          Pas:=Arrondi125(PasIndicatif/Taille); // nombre de grade par pas
          result:=Pas/200*Pi; // nombre de radian par pas
        end;
      uaDegre :
        begin
          Taille:=TailleRadian*Pi/180; // taille d'un degré
          Pas:=Arrondi125(PasIndicatif/Taille); // nombre de degré par pas
          result:=Pas/180*Pi; // nombre de radian par pas
        end;
      uaDM :
      begin
          Taille:=TailleRadian*Pi/180; // taille d'un degré
          if PasIndicatif>Taille then
          begin // nombre entier de degrés
            Pas:=Arrondi125(PasIndicatif/Taille); // nombre de degré par pas
            result:=Pas/180*Pi; // nombre de radian par pas
          end
          else begin
            Taille:=Taille/60; // taille d'une minute
            if PasIndicatif>Taille then
            begin // nombre entier de minutes
              Pas:=Arrondi60(PasIndicatif/Taille);// nombre de minutes par pas
              result:=Pas/180/60*Pi;
            end
            else begin // fraction de minute
              Pas:=Arrondi125(PasIndicatif/Taille);// nombre de minutes par pas
              result:=Pas/180/60*Pi;
            end;
          end;
        end;
      uaDMS :
        begin
          Taille:=TailleRadian*Pi/180; // taille d'un degré
          if PasIndicatif>Taille then
          begin // nombre entier de degrés
            Pas:=Arrondi125(PasIndicatif/Taille); // nombre de degré par pas
            result:=Pas/180*Pi; // nombre de radian par pas
          end
          else begin
            Taille:=Taille/60; // taille d'une minute
            if PasIndicatif>Taille then
            begin // nombre entier de minutes
              Pas:=Arrondi60(PasIndicatif/Taille);// nombre de minutes par pas
              result:=Pas/180/60*Pi;
            end
            else begin // fraction de minute
              Taille:=Taille/60; // taille d'une seconde
              if PasIndicatif>Taille then
              begin // nombre entier de secondes
                Pas:=Arrondi60(PasIndicatif/Taille);// nombre de secondes par pas
                result:=Pas/180/60/60*Pi;
              end
              else begin // fraction de seconde
                Pas:=Arrondi125(PasIndicatif/Taille);// nombre de seconde par pas
                result:=Pas/180/60/60*Pi;
              end;
            end;
          end;
        end;
      else
        Raise EConvertError.Create('Unité d''angle inconnue');
    end;
  end;

begin
  try
    LimiteCercleCirconscrit(LatTopLeft, LongTopLeft,
                   LatTopRight, LongTopRight, LatBottomLeft, LongBottomLeft,
                   LatBottomRight, LongBottomRight, self,
                   LatMin, LatMax, LongMin, LongMax);

    PasLat:=QuelPas(Pas,(Ellipsoide.a+Ellipsoide.b)/2);
    PasLong:=QuelPas(Pas,(Ellipsoide.a+Ellipsoide.b)/2*cos((LatTopLeft+LatBottomRight)/2));

    if Paris then
    begin
      LongMin:=LongMin-AngleParis;
      LongMax:=LongMax-AngleParis;
    end;

    // quadrillage lui-même
    XMin:=Mytrunc(LongMin/PasLong);
    XMax:=Mytrunc(LongMax/PasLong)+1;
    YMin:=Mytrunc(LatMin/PasLat);
    if YMin<-(Pi/PasLat+1)/2 then
      inc(YMin); // il ne faut pas dépasser le pôle sud pour cause d'arrondi
    YMax:=Mytrunc(LatMax/PasLat)+1;
    if YMax>(Pi/PasLat+1)/2 then
      dec(YMax); // il ne faut pas dépasser le pôle nord pour cause d'arrondi
    SetLength(ZoneQuadri.LesMeridien,XMax-XMin+1);
    SetLength(ZoneQuadri.LesParallele,YMax-YMin+1);
    for I:=0 to XMax-XMin do
    begin
      SetLength(ZoneQuadri.LesMeridien[I].Lat,YMax-YMin+1);
      SetLength(ZoneQuadri.LesMeridien[I].Long,YMax-YMin+1);
      ZoneQuadri.LesMeridien[I].Nom:=AngleToStr((XMin+I)*PasLong,UniteAngle);
      for J:=0 to YMax-YMin do
      begin
        if Paris then
          VersWGS84((Ymin+J)*PasLat,(Xmin+I)*PasLong+AngleParis,Lat,Long)
        else
          VersWGS84((Ymin+J)*PasLat,(Xmin+I)*PasLong,Lat,Long);
        ZoneQuadri.LesMeridien[I].Lat[J]:=Lat;
        ZoneQuadri.LesMeridien[I].Long[J]:=Long;
      end;
    end;
    for J:=0 to YMax-YMin do
    begin
      SetLength(ZoneQuadri.LesParallele[J].Lat,XMax-XMin+1);
      SetLength(ZoneQuadri.LesParallele[J].Long,XMax-XMin+1);
      ZoneQuadri.LesParallele[J].Nom:=AngleToStr((YMin+J)*PasLat,UniteAngle);
      for I:=0 to XMax-XMin do
      begin
        if Paris then
          VersWGS84((Ymin+J)*PasLat,(Xmin+I)*PasLong+AngleParis,Lat,Long)
        else
          VersWGS84((Ymin+J)*PasLat,(Xmin+I)*PasLong,Lat,Long);
        ZoneQuadri.LesParallele[J].Lat[I]:=Lat;
        ZoneQuadri.LesParallele[J].Long[I]:=Long;
      end;
    end;
  except
    SetLength(ZoneQuadri.LesMeridien,0);
    SetLength(ZoneQuadri.LesParallele,0);
  end;
end;

begin
  defaultFormatSettings.DecimalSeparator:='.';
  AngleParis := AngleParisNum;//StrToAngle(AngleParisStr);
end.
