unit mercator_multi;

{$mode delphi}{$H+}
interface

uses
  multiple, transmercator, type_quadri;

type

// projection mercator avec des fuseaux
// un numéro de fuseau >0 correspond à l'hémisphère Nord
// un numéro de fuseau <0 correspond au fuseau de nombre opposé
// dans l'hémisphère Sud

  TMultipleMercator = class(TMultiple)
  private
    Nord, Sud : array of TTransverseMercator;
    fFuseauOrigine : string; // longitude du centre du fuseau numéro 1
    fFuseauOrigineReal : real;  // valeur en radian de la longitude précédente
    fPas : string; // pas entre un fuseau et le suivant, valeur négative si la numérotation augmente vers l'ouest
    fPasReal : real;  // valeur en radian du pas précédent
    fk0 : real; // facteur d'échelle au point d'origine
    fX0, fY0Nord, fY0Sud : real; // coordonnées en projection du point d'origine
    function GetFuseau(fuseau : integer) : TTransverseMercator;
  protected
    // real
    function GetNbReal : integer; override;
    function GetParamReal(index : integer) : real; override;
    procedure SetParamReal(index : integer; Value : real); override;
    function GetNom_ParamReal(index : integer) : string; override;
    // integer
    function GetNbInteger : integer; override;
    function GetParamInteger(index : integer) : Integer; override;
    procedure SetParamInteger(index : integer; Value : Integer); override;
    function GetNom_ParamInteger(index : integer) : string; override;
    // Angle
    function GetNbAngle : integer; override;
    function GetParamAngle(index : integer) : string; override;
    procedure SetParamAngle(index : integer; Value : string); override;
    function GetNom_ParamAngle(index : integer) : string; override;
    // Boolean
    function GetNbBoolean : integer; override;
    function GetParamBoolean(index : integer) : Boolean; override;
    procedure SetParamBoolean(index : integer; Value : Boolean); override;
    function GetNom_ParamBoolean(index : integer) : string; override;

    function GetNature : string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure VersGrille(Lat, Long : real; var X, Y : real; var fuseau : integer); override;
    procedure VersLatLong(X, Y : real; fuseau : integer; var Lat, Long : real); override;
    function NbZoneQuadrillage(LatTopLeft, LongTopLeft, LatTopRight, LongTopRight,
                               LatBottomLeft, LongBottomLeft, LatBottomRight, LongBottomRight : real
                               ) : integer; override; // indique le nombre de zones couvertes par la demande
    procedure CalculQuadrillage(var LesZones : array of TZoneQuadri;
                                LatTopLeft, LongTopLeft, LatTopRight, LongTopRight,
                                LatBottomLeft, LongBottomLeft, LatBottomRight, LongBottomRight,
                                Pas : real); override;
    function NbLimiteZoneQuadrillage(LatTopLeft, LongTopLeft, LatTopRight, LongTopRight,
                                     LatBottomLeft, LongBottomLeft, LatBottomRight, LongBottomRight : real
                                     ) : integer; override; // indique le nombre de limites entre zones
    procedure CalculLimiteZone(var LesLimites : array of TPolySegment;
                                LatTopLeft, LongTopLeft, LatTopRight, LongTopRight,
                                LatBottomLeft, LongBottomLeft, LatBottomRight, LongBottomRight,
                                Pas : real); override;
  end;

implementation

uses
  SysUtils,
  outils, math, ign, donnee, relevement, arrondir;

constructor TMultipleMercator.Create;
begin
  inherited Create;
end;

destructor TMultipleMercator.Destroy;
var
  I : integer;
begin
  for I:=1 to High(Nord) do
    (Nord[I] as TTransverseMercator).Free;
  for I:=1 to High(Sud) do
    (Sud[I] as TTransverseMercator).Free;
  inherited Destroy;
end;


function TMultipleMercator.GetNature : string;
begin
  result:=tmMercatorMultiple;
end;

function TMultipleMercator.GetNbReal : integer;
begin
  result:=4;
end;

function TMultipleMercator.GetParamReal(index : integer) : real;
begin
  case index of
    0 : result:=fk0;
    1 : result:=fX0;
    2 : result:=fY0Nord;
    3 : result:=fY0Sud;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TMultipleMercator.SetParamReal(index : integer; Value : real);
begin
  case Index of
    0 : fk0:=Value;
    1 : fX0:=Value;
    2 : fY0Nord:=Value;
    3 : fY0Sud:=Value;
  else
    begin
      fValid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;
end;

function TMultipleMercator.GetNom_ParamReal(index : integer) : string;
begin
  case index of
    0 : result:='k0';
    1 : result:='X0';
    2 : result:='Y0Nord';
    3 : result:='Y0Sud';
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

function TMultipleMercator.GetNbInteger : integer;
begin
  result:=0;
end;

function TMultipleMercator.GetParamInteger(index : integer) : Integer;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TMultipleMercator.SetParamInteger(index : integer; Value : Integer);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TMultipleMercator.GetNom_ParamInteger(index : integer) : string;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

function TMultipleMercator.GetNbAngle : integer;
begin
  result:=2;
end;

function TMultipleMercator.GetParamAngle(index : integer) : string;
begin
  case index of
    0 : result:=fFuseauOrigine;
    1 : result:=fPas;
  else
    raise ERangeError.Create('Demande de paramètre hors-limites');
  end;
end;

procedure TMultipleMercator.SetParamAngle(index : integer; Value : string);
begin
  case index of
    0 : begin
      try
        fFuseauOrigine:=Value;
        fFuseauOriginereal:=StrToAngle(Value);
      except
        fValid:=false;
        Raise;
      end;
    end;
    1 : begin
      try
        fPas:=Value;
        fPasreal:=StrToAngle(Value);
      except
        fValid:=false;
        Raise;
      end;
    end;
  else
    begin
      fValid:=false;
      raise ERangeError.Create('Ecriture de paramètre hors-limites');
    end;
  end;  
end;

function TMultipleMercator.GetNom_ParamAngle(index : integer) : string;
begin
  case index of
    0 : result:='LandaFuseau0';
    1 : result:='Pas';
  else
    begin
      result:='';
      raise ERangeError.Create('Demande de paramètre hors-limites');
    end;
  end;
end;

function TMultipleMercator.GetNbBoolean : integer;
begin
  result:=0;
end;

function TMultipleMercator.GetParamBoolean(index : integer) : Boolean;
begin
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;

procedure TMultipleMercator.SetParamBoolean(index : integer; Value : Boolean);
begin
  fValid:=false;
  raise ERangeError.Create('Ecriture de paramètre hors-limites');
end;

function TMultipleMercator.GetNom_ParamBoolean(index : integer) : string;
begin
  result:='';
  raise ERangeError.Create('Demande de paramètre hors-limites');
end;


function TMultipleMercator.GetFuseau(fuseau : integer) : TTransverseMercator;
begin
  if fuseau>0 then
  begin
    if fuseau>high(Nord) then
      SetLength(Nord,fuseau+1);
    if Nord[fuseau]=nil then
    begin
      result:=TTransverseMercator.Create;
      result.Datum:=Datum;
      result.ParamReal[0]:=fk0;
      result.ParamReal[1]:=fX0;
      result.ParamReal[2]:=fY0Nord;
      result.ParamAngle[0]:=AngleToStrDegreSexa((fuseau-1)*fPasReal+fFuseauOrigineReal);
      result.ParamAngle[1]:='0d';
      result.Valid:=true;
      Nord[fuseau]:=result;
    end;
    result:=Nord[fuseau];
  end
  else begin
    if -fuseau>high(Sud) then
      SetLength(Sud,(-fuseau)+1);
    if Sud[-fuseau]=nil then
    begin
      result:=TTransverseMercator.Create;
      result.Datum:=Datum;
      result.ParamReal[0]:=fk0;
      result.ParamReal[1]:=fX0;
      result.ParamReal[2]:=fY0Sud;
      result.ParamAngle[0]:=AngleToStrGrade((-fuseau-1)*fPasReal+fFuseauOrigineReal);
      result.ParamAngle[1]:='0d';
      result.Valid:=true;
      Sud[-fuseau]:=result;
    end;
    result:=Sud[-fuseau];
  end;
end;

procedure TMultipleMercator.VersGrille(Lat, Long : real; var X, Y : real; var fuseau : integer);
var
  Alpha : real;
begin
  // on ramène la longitude en valeurs positives
  Alpha:=Long-fFuseauOrigineReal+fPasReal/2;
  Alpha:=Alpha*sign(fPasReal);
  while Alpha<0 do
    Alpha:=Alpha+2*Pi;
  while Alpha>2*Pi do
    Alpha:=Alpha-2*Pi;
  fuseau:=trunc(Alpha/abs(fPasReal))+1;
  if Lat<0 then
    fuseau:=-fuseau;
  GetFuseau(fuseau).VersGrille(Lat,Long,X, Y);
end;

procedure TMultipleMercator.VersLatLong(X, Y : real; fuseau : integer; var Lat, Long : real);
begin
  GetFuseau(fuseau).VersLatLong(X,Y,Lat, Long);
end;

function TMultipleMercator.NbZoneQuadrillage(LatTopLeft, LongTopLeft,
  LatTopRight, LongTopRight, LatBottomLeft, LongBottomLeft, LatBottomRight,
  LongBottomRight: real): integer;
var
  LatMin, LatMax, LongMin, LongMax : real;
  NMin, NMax : integer;
begin
  try
    LimiteCercleCirconscrit(LatTopLeft, LongTopLeft,
                   LatTopRight, LongTopRight, LatBottomLeft, LongBottomLeft,
                   LatBottomRight, LongBottomRight, Datum,
                   LatMin, LatMax, LongMin, LongMax);
    NMin:=MyTrunc((LongMin-fFuseauOrigineReal)/fPasreal+0.5);
    NMax:=MyTrunc((LongMax-fFuseauOrigineReal)/fPasreal+0.5);
    result:=(NMax-NMin)+1;
    if LatMin*LatMax<0 then // on est à cheval sur l'équateur (non testé)
      result:=result*2;
  except
    result:=0;
  end;
end;

procedure TMultipleMercator.CalculQuadrillage(
  var LesZones: array of TZoneQuadri; LatTopLeft, LongTopLeft, LatTopRight,
  LongTopRight, LatBottomLeft, LongBottomLeft, LatBottomRight,
  LongBottomRight, Pas: real);
var
  LatMin, LatMax, LongMin, LongMax, Lat, Long, Lat1, Long1 : real;
  NMin, NMax, NZone, I, J, K,
  DernierDedans, DernierDehors, DeltaZone : integer;
  XMin, XMax, YMin, YMax, h, g : real;
  HMin, HMax, VMin, VMax : integer;
  Gauche, Droite, Haut, Bas : real;
  Lefuseau : TTransverseMercator;

  procedure FaitMoiLesParalleles(Delta : integer);
  var
    J, K : integer;
  begin
    for J:=0 to VMax-VMin do
    begin
      SetLength(LesZones[I-NMin+Delta].Lesparallele[J].Lat,HMax-HMin+1);
      SetLength(LesZones[I-NMin+Delta].Lesparallele[J].Long,HMax-HMin+1);
      LesZones[I-NMin+Delta].Lesparallele[J].Nom:=Format('%g',[(VMin+J)*Pas/1000]);
      DernierDedans:=-1; DernierDehors:=-1;
      for K:=0 to HMax-HMin do
      begin
        LeFuseau.VersLatLong((HMin+K)*Pas,(VMin+J)*Pas,Lat, Long);
        if Long<Gauche then
          DernierDehors:=K;
        if Long<=Droite then
          DernierDedans:=K;
        Datum.VersWGS84(Lat,Long,Lat,Long);
        LesZones[I-NMin+Delta].Lesparallele[J].Lat[K]:=Lat;
        LesZones[I-NMin+Delta].Lesparallele[J].Long[K]:=Long;
      end;
      // élimination des segments hors zone
      // à droite d'abord
      if DernierDedans=-1 then
      begin // on enlève tout (non testé)
        SetLength(LesZones[I-NMin+Delta].Lesparallele[J].Lat,0);
        SetLength(LesZones[I-NMin+Delta].Lesparallele[J].Long,0);
      end
      else if DernierDedans<HMax-Hmin then
      begin // on enlève une partie des points et on recalcule le dernier
        LeFuseau.VersLatLong((HMin+DernierDedans)*Pas,(VMin+J)*Pas,Lat, Long);
        LeFuseau.VersLatLong((HMin+DernierDedans+1)*Pas,(VMin+J)*Pas,Lat1, Long1);
        h:=(Droite-Long)/(Long1-Long);
        Lat1:=Lat+(Lat1-Lat)*h;
        Long1:=Droite;
        Datum.VersWGS84(Lat1,Long1,Lat1,Long1);
        LesZones[I-NMin+Delta].LesParallele[J].Lat[DernierDedans+1]:=Lat1;
        LesZones[I-NMin+Delta].LesParallele[J].Long[DernierDedans+1]:=Long1;
        SetLength(LesZones[I-NMin+Delta].LesParallele[J].Lat,DernierDedans+2);
        SetLength(LesZones[I-NMin+Delta].LesParallele[J].Long,DernierDedans+2);
      end;
      // à gauche ensuite
      if DernierDehors=HMax-HMin then
      begin // on enlève tout (non testé)
        SetLength(LesZones[I-NMin+Delta].Lesparallele[J].Lat,0);
        SetLength(LesZones[I-NMin+Delta].Lesparallele[J].Long,0);
      end
      else if DernierDehors<>-1 then
      begin // on enlève le début des points et on recalcule le premier qui reste
        LeFuseau.VersLatLong((HMin+DernierDehors)*Pas,(VMin+J)*Pas,Lat, Long);
        LeFuseau.VersLatLong((HMin+DernierDehors+1)*Pas,(VMin+J)*Pas,Lat1, Long1);
        h:=(Gauche-Long)/(Long1-Long);
        Lat1:=Lat+(Lat1-Lat)*h;
        Long1:=Gauche;
        Datum.VersWGS84(Lat1,Long1,Lat1,Long1);
        LesZones[I-NMin+Delta].LesParallele[J].Lat[DernierDehors]:=Lat1;
        LesZones[I-NMin+Delta].LesParallele[J].Long[DernierDehors]:=Long1;
        for K:=0 to high(LesZones[I-NMin+Delta].LesParallele[J].Lat)-DernierDehors do
        begin
          LesZones[I-NMin+Delta].LesParallele[J].Lat[K]:=LesZones[I-NMin+Delta].LesParallele[J].Lat[K+DernierDehors];
          LesZones[I-NMin+Delta].LesParallele[J].Long[K]:=LesZones[I-NMin+Delta].LesParallele[J].Long[K+DernierDehors];
        end;
        SetLength(LesZones[I-NMin+Delta].LesParallele[J].Lat,high(LesZones[I-NMin+Delta].LesParallele[J].Lat)-DernierDehors+1);
        SetLength(LesZones[I-NMin+Delta].LesParallele[J].Long,high(LesZones[I-NMin+Delta].LesParallele[J].Long)-DernierDehors+1);
      end;
    end;
  end;

begin
  inherited;
  try
    Pas:=Arrondi125(Pas);
    LimiteCercleCirconscrit(LatTopLeft, LongTopLeft,
                   LatTopRight, LongTopRight, LatBottomLeft, LongBottomLeft,
                   LatBottomRight, LongBottomRight, Datum,
                   LatMin, LatMax, LongMin, LongMax);
    NMin:=MyTrunc((LongMin-fFuseauOrigineReal)/fPasreal+0.5)+1;
    NMax:=MyTrunc((LongMax-fFuseauOrigineReal)/fPasreal+0.5)+1;
    // calcule du décalage de la numérotation des zones de l'hémisphère
    if LatMax*LatMin>=0 then
      DeltaZone:=0
    else
      DeltaZone:=NMax-NMin+1;

    for I:=NMin to NMax do
    begin
      NZone:=(I-1+60) mod 60 + 1;
      if NZone=NMin then
        Gauche:=LongMin
      else
        Gauche:=(NZone-1.5)*fPasReal+fFuseauOrigineReal;
      if NZone=NMax  then
        Droite:=LongMax
      else
        Droite:=(NZone-0.5)*fPasReal+fFuseauOrigineReal;
      // zones dans l'hémisphère nord
      if LatMax>0 then
      begin
        LeFuseau:= GetFuseau(NZone);
        Haut:=LatMax;
        if LatMin<0 then
          Bas:=0
        else
          Bas:=LatMin;
        LeFuseau.VersGrille(Bas,(NZone-1)*fPasReal+fFuseauOrigineReal,h, YMin); // on prend en bas au centre du fuseau
        // on prend le max des deux sommets
        LeFuseau.VersGrille(Haut,Gauche,h,YMax);
        LeFuseau.VersGrille(Haut,Droite,h,g);
        if g>YMax then
          YMax:=g;
        LeFuseau.VersGrille(Bas,Gauche,Xmin,h); // on prend l'angle inférieur gauche
        LeFuseau.VersGrille(Bas,Droite,XMax,h); // on prend l'angle inférieur droit
        HMin:=MyTrunc(XMin/Pas);
        HMax:=MyTrunc(XMax/Pas)+1;
        VMin:=MyTrunc(YMin/Pas);
        VMax:=MyTrunc(YMax/Pas)+1;
        //méridiens
        SetLength(LesZones[I-NMin].LesMeridien,HMax-HMin+1);
        SetLength(LesZones[I-NMin].LesParallele,VMax-VMin+1);
        for K:=0 to HMax-HMin do
        begin
          SetLength(LesZones[I-NMin].LesMeridien[K].Lat,VMax-VMin+1);
          SetLength(LesZones[I-NMin].LesMeridien[K].Long,VMax-VMin+1);
          LesZones[I-NMin].LesMeridien[K].Nom:=Format('%g',[(HMin+K)*Pas/1000]);
          DernierDedans:=-1;
          for J:=0 to VMax-VMin do
          begin
            LeFuseau.VersLatLong((HMin+K)*Pas,(VMin+J)*Pas,Lat, Long);
            if (Long>=Gauche) and (Long<=Droite) then
              DernierDedans:=J;
            Datum.VersWGS84(Lat,Long,Lat,Long);
            LesZones[I-NMin].LesMeridien[K].Lat[J]:=Lat;
            LesZones[I-NMin].LesMeridien[K].Long[J]:=Long;
          end;
          // élimination des segments hors zone
          if DernierDedans=-1 then
          begin // on enlève tout
            SetLength(LesZones[I-NMin].LesMeridien[K].Lat,0);
            SetLength(LesZones[I-NMin].LesMeridien[K].Long,0);
          end
          else if DernierDedans<VMax-Vmin then
          begin // on enlève une partie des points et on recalcule le dernier
            LeFuseau.VersLatLong((HMin+K)*Pas,(VMin+DernierDedans)*Pas,Lat, Long);
            LeFuseau.VersLatLong((HMin+K)*Pas,(VMin+DernierDedans+1)*Pas,Lat1, Long1);
            if Long1>Droite then
              g:=Droite
            else
              g:=Gauche;
            h:=(g-Long)/(Long1-Long);
            Lat1:=Lat+(Lat1-Lat)*h;
            Long1:=g;
            Datum.VersWGS84(Lat1,Long1,Lat1,Long1);
            LesZones[I-NMin].LesMeridien[K].Lat[DernierDedans+1]:=Lat1;
            LesZones[I-NMin].LesMeridien[K].Long[DernierDedans+1]:=Long1;
            SetLength(LesZones[I-NMin].LesMeridien[K].Lat,DernierDedans+2);
            SetLength(LesZones[I-NMin].LesMeridien[K].Long,DernierDedans+2);
          end;
        end;
        // parallèles
        FaitMoiLesParalleles(0);
      end;
      // zones dans l'hémisphère sud
(********************  Début de zone non testée **********************)
      if LatMin<0 then
      begin
        LeFuseau:= GetFuseau(-NZone);
        Bas:=LatMin;
        if LatMax>0 then
          Haut:=0
        else
          Haut:=LatMax;
        LeFuseau.VersGrille(Haut,(NZone-1)*fPasReal+fFuseauOrigineReal,h, YMax); // on prend en bas au centre du fuseau
        // on prend le min des deux sommets
        LeFuseau.VersGrille(Bas,Gauche,h,YMin);
        LeFuseau.VersGrille(Bas,Droite,h,g);
        if g<YMin then
          YMin:=g;
        LeFuseau.VersGrille(Haut,Gauche,Xmin,h); // on prend l'angle supérieur gauche
        LeFuseau.VersGrille(Haut,Droite,XMax,h); // on prend l'angle supérieur droit
        HMin:=MyTrunc(XMin/Pas);
        HMax:=MyTrunc(XMax/Pas)+1;
        VMin:=MyTrunc(YMin/Pas);
        VMax:=MyTrunc(YMax/Pas)+1;
        //méridiens
        SetLength(LesZones[I-NMin+DeltaZone].LesMeridien,HMax-HMin+1);
        SetLength(LesZones[I-NMin+DeltaZone].LesParallele,VMax-VMin+1);
        for K:=0 to HMax-HMin do
        begin
          SetLength(LesZones[I-NMin+DeltaZone].LesMeridien[K].Lat,VMax-VMin+1);
          SetLength(LesZones[I-NMin+DeltaZone].LesMeridien[K].Long,VMax-VMin+1);
          LesZones[I-NMin+DeltaZone].LesMeridien[K].Nom:=Format('%g',[(HMin+K)*Pas/1000]);
          DernierDedans:=VMax-VMin+1;
          for J:=VMax-VMin downto 0 do
          begin
            LeFuseau.VersLatLong((HMin+K)*Pas,(VMin+J)*Pas,Lat, Long);
            if (Long>=Gauche) and (Long<=Droite) then
              DernierDedans:=J;
            Datum.VersWGS84(Lat,Long,Lat,Long);
            LesZones[I-NMin+DeltaZone].LesMeridien[K].Lat[J]:=Lat;
            LesZones[I-NMin+DeltaZone].LesMeridien[K].Long[J]:=Long;
          end;
          // élimination des segments hors zone
          if DernierDedans=VMax-VMin+1 then
          begin // on enlève tout
            SetLength(LesZones[I-NMin+DeltaZone].LesMeridien[K].Lat,0);
            SetLength(LesZones[I-NMin+DeltaZone].LesMeridien[K].Long,0);
          end
          else if DernierDedans>0 then
          begin // on enlève une partie des points et on recalcule le dernier
            LeFuseau.VersLatLong((HMin+K)*Pas,(VMin+DernierDedans)*Pas,Lat, Long);
            LeFuseau.VersLatLong((HMin+K)*Pas,(VMin+DernierDedans-1)*Pas,Lat1, Long1);
            if Long1>Droite then
              g:=Droite
            else
              g:=Gauche;
            h:=(g-Long)/(Long1-Long);
            Lat1:=Lat+(Lat1-Lat)*h;
            Long1:=g;
            Datum.VersWGS84(Lat1,Long1,Lat1,Long1);
            LesZones[I-NMin+DeltaZone].LesMeridien[K].Lat[DernierDedans-1]:=Lat1;
            LesZones[I-NMin+DeltaZone].LesMeridien[K].Long[DernierDedans-1]:=Long1;
            for J:=0 to high(LesZones[I-NMin+DeltaZone].LesMeridien[K].Lat)-DernierDedans+1 do
            begin
              LesZones[I-NMin+DeltaZone].LesMeridien[K].Lat[J]:=LesZones[I-NMin+DeltaZone].LesMeridien[K].Lat[J+DernierDedans-1];
              LesZones[I-NMin+DeltaZone].LesMeridien[K].Long[J]:=LesZones[I-NMin+DeltaZone].LesMeridien[K].Long[J+DernierDedans-1];
            end;
            SetLength(LesZones[I-NMin+DeltaZone].LesMeridien[K].Lat,high(LesZones[I-NMin+DeltaZone].LesMeridien[K].Lat)-DernierDedans+2);
            SetLength(LesZones[I-NMin+DeltaZone].LesMeridien[K].Long,high(LesZones[I-NMin+DeltaZone].LesMeridien[K].Long)-DernierDedans+2);
          end;
        end;
        // parallèles
        FaitMoiLesParalleles(DeltaZone);
      end;
(********************  Fin de zone non testée **********************)
    end;
  except
  end;
end;

function TMultipleMercator.NbLimiteZoneQuadrillage(LatTopLeft, LongTopLeft,
  LatTopRight, LongTopRight, LatBottomLeft, LongBottomLeft, LatBottomRight,
  LongBottomRight: real): integer;
var
  LatMin, LatMax, LongMin, LongMax : real;
  NMin, NMax : integer;
begin
  try
    LimiteCercleCirconscrit(LatTopLeft, LongTopLeft,
                   LatTopRight, LongTopRight, LatBottomLeft, LongBottomLeft,
                   LatBottomRight, LongBottomRight, Datum,
                   LatMin, LatMax, LongMin, LongMax);
    NMin:=MyTrunc((LongMin-fFuseauOrigineReal)/fPasreal+0.5);
    NMax:=MyTrunc((LongMax-fFuseauOrigineReal)/fPasreal+0.5);
    if NMax-NMin+1=round(2*Pi/fPasReal) then
    begin // on fait tout le tour de la terre (non testé)
      result:=NMax-NMin+1;
      if LatMin*LatMax<0 then // on est à cheval sur l'équateur (non testé)
        result:=result*3;
    end
    else begin
      result:=(NMax-NMin);
      if LatMin*LatMax<0 then // on est à cheval sur l'équateur (non testé)
        result:=result*3+1;
    end;
  except
    result:=0;
  end;
end;

procedure TMultipleMercator.CalculLimiteZone(
  var LesLimites: array of TPolySegment; LatTopLeft, LongTopLeft,
  LatTopRight, LongTopRight, LatBottomLeft, LongBottomLeft, LatBottomRight,
  LongBottomRight, Pas: real);
var
  LatMin, LatMax, LongMin, LongMax : real;
  NMin, NMax, I : integer;

  procedure TraceEquateur(var PolySegment : TPolySegment; LongDebut, LongFin : real);
  var
    I, NbPoint : integer;
    Lat, Long : real;
  begin
    NbPoint:=round((LongFin-LongDebut)*Datum.Ellipsoide.a/Pas);
    SetLength(PolySegment.Lat,NBPoint+1);
    SetLength(PolySegment.Long,NBPoint+1);
    for I:=0 to NbPoint do
    begin
      Lat:=0; Long:=I/NbPoint*(LongFin-LongDebut)+LongDebut;
      Datum.VersWGS84(Lat,Long,Lat,Long);
      PolySegment.Lat[I]:=Lat;
      PolySegment.Long[I]:=Long;
    end;
  end;

  procedure TraceMeridien(var PolySegment : TPolySegment; LatDebut, LatFin, Longitude : real);
  var
    I, NbPoint : integer;
    Lat, Long : real;
  begin
    NbPoint:=round((LatFin-LatDebut)*Datum.Ellipsoide.a/Pas);
    SetLength(PolySegment.Lat,NBPoint+1);
    SetLength(PolySegment.Long,NBPoint+1);
    for I:=0 to NbPoint do
    begin
      Long:=Longitude; Lat:=I/NbPoint*(LatFin-LatDebut)+LatDebut;
      Datum.VersWGS84(Lat,Long,Lat,Long);
      PolySegment.Lat[I]:=Lat;
      PolySegment.Long[I]:=Long;
    end;
  end;

begin
  inherited;
  try
    LimiteCercleCirconscrit(LatTopLeft, LongTopLeft,
                   LatTopRight, LongTopRight, LatBottomLeft, LongBottomLeft,
                   LatBottomRight, LongBottomRight, Datum,
                   LatMin, LatMax, LongMin, LongMax);
    NMin:=MyTrunc((LongMin-fFuseauOrigineReal)/fPasreal+0.5);
    NMax:=MyTrunc((LongMax-fFuseauOrigineReal)/fPasreal+0.5);
    if NMax-NMin+1=round(2*Pi/fPasReal) then
    begin // on fait tout le tour de la terre (non testé)
      if LatMin*LatMax<0 then // on est à cheval sur l'équateur (non testé)
      begin
        for I:=0 to NMax-NMin+1 do
        begin
          TraceMeridien(LesLimites[3*I],0, LatMax, fFuseauOrigineReal+fPasReal*(I-0.5));
          TraceMeridien(LesLimites[3*I+1],LatMin, 0, fFuseauOrigineReal+fPasReal*(I-0.5));
          TraceEquateur(LesLimites[3*I+1],fFuseauOrigineReal+fPasReal*(I-0.5),fFuseauOrigineReal+fPasReal*(I+0.5));
        end;
      end
      else begin // on est d'un seul côté de l'équateur
        for I:=0 to NMax-NMin+1 do
          TraceMeridien(LesLimites[I],LatMin, LatMax, fFuseauOrigineReal+fPasReal*(I-0.5));
      end;
    end
    else begin
//      result:=(NMax-NMin);
      if LatMin*LatMax<0 then // on est à cheval sur l'équateur (non testé)
      begin
        for I:=NMin to NMax-1 do
        begin
          TraceMeridien(LesLimites[2*(I-NMin)],LatMin, 0, fFuseauOrigineReal+fPasReal*(I+0.5));
          TraceMeridien(LesLimites[2*(I-NMin)+1],0, LatMax, fFuseauOrigineReal+fPasReal*(I+0.5));
        end;
        for I:=NMin to NMax do
          TraceEquateur(LesLimites[(I-NMin)+(NMax-NMin)*2],fFuseauOrigineReal+fPasReal*(I-0.5), fFuseauOrigineReal+fPasReal*(I+0.5));
      end
      else begin
        for I:=NMin to NMax-1 do
          TraceMeridien(LesLimites[I-NMin],LatMin, LatMax, fFuseauOrigineReal+fPasReal*(I+0.5));
      end;
    end;
  except
  end;
end;

end.
