unit grille_ign;

interface

uses
  SysUtils;

type

  TGrilleIgn = class
  private
    FLatMin: real;
    FPasLat: real;
    FPasLong: real;
    FLongMax: real;
    FLongMin: real;
    FLatMax: real;
    FCoefX, FCoefY, FCoefZ : array of real;
    procedure SetLatMin(const Value: real);
    procedure SetLatMax(const Value: real);
    procedure SetLongMax(const Value: real);
    procedure SetLongMin(const Value: real);
    procedure SetPasLat(const Value: real);
    procedure SetPasLong(const Value: real);
  public
    property LatMin : real read FLatMin write SetLatMin;
    property LatMax : real read FLatMax write SetLatMax;
    property LongMin : real read FLongMin write SetLongMin;
    property LongMax : real read FLongMax write SetLongMax;
    property PasLat : real read FPasLat write SetPasLat;
    property PasLong : real read FPasLong write SetPasLong;
    constructor Create (FileName : TFileName);
    function GetTranslation(Lat, Long : real; var TX, TY, TZ : real) : boolean;
  end;

implementation

uses
  Math;

{ TGrilleIgn }

constructor TGrilleIgn.Create(FileName: TFileName);
type
// définition des différents types de grilles
  TGrille_Format = (gf_txt_france, gf_mnt_france, gf_mnt_nc);
// txt_france : 4 lignes d'entête, les limites de la grilles et son pas sont sur la seconde ligne après GR3D1
//              un point par ligne, commençant par un entier indéterminé constant,
//              Long, Lat, DX, DY, DZ, Erreur et se
//              terminant par un nombre entier variable avec éventuellement un L accolé devant
// mnt_france : 1 ligne d'entête contenant directement les limites et le pas de la grille
//              tous les points sont sur la second ligne, simplement séparés par des espaces
//              chaque point comporte uniquement DX, DY, DZ et Erreur
// mnt_nc (Nouvelle Calédonie) : 1 ligne d'entête contenant les limites de la grille et son pas
//              ainsi que des informations complémentaires, en particulier
//              les noms des systèmes géodésique de départ et d'arrivée
//              un point par ligne sans données supplémentaires
//              pour chaque point Long, Lat, DX, DY, DZ, Erreur

var
  F : TextFile;
  S : string;
  Separateur : char;
  NbPasLat, NbPasLong, I  : integer;
  Grille_Format : TGrille_Format;

  function Extrait(var S : string; Sep : char) : string;
  // supprime les espaces au début de la chaîne actuelle
  // renvoie le premier élément concret de la chaîne dans result
  // renvoie la chaîne S coupée du premier élément
  // Sep indique le séparateur entre les éléments
  var
    P : integer;
  begin
    while S[1]=Sep do
      delete(S,1,1);

    P:=Pos(Sep,S);
    if P=0 then
    begin // on est la fin de la chaîne
      result:=S;
      S:='';
    end
    else
    begin
      result:=copy(S,1,P-1);
      delete(S,1,P);
    end;
  end;

  procedure Param_Grille(var S : string);
  begin
    LongMin:=StrToFloat(Extrait(S,' '));
    LongMax:=StrToFloat(Extrait(S,' '));
    LatMin:=StrToFloat(Extrait(S,' '));
    LatMax:=StrToFloat(Extrait(S,' '));
    PasLong:=StrToFloat(Extrait(S,' '));
    PasLat:=StrToFloat(Extrait(S,' '));
  end;

  procedure Lit_Un_Point(var S : string; Sep : char);
  var
    Long, Lat, TX, TY, TZ : real;
    I : integer;
  begin
    Long:=StrToFloat(Extrait(S,Sep));
    Lat:=StrToFloat(Extrait(S,Sep));
    TX:=StrToFloat(Extrait(S,Sep));
    TY:=StrToFloat(Extrait(S,Sep));
    TZ:=StrToFloat(Extrait(S,Sep));
    I := round((Long-LongMin)/PasLong)*NbPasLat+round((Lat-LatMin)/PasLat);
    FCoefX[I]:=TX;
    FCoefY[I]:=TY;
    FCoefZ[I]:=TZ;
    Extrait(S,Sep); // élimination du coefficient de précision
  end;

begin
  inherited Create;
  AssignFile(f,FileName);
  Reset(F);
  try
    if UpperCase(ExtractFileExt(FileName))='.TXT' then
    begin
      Grille_Format:=gf_txt_france;
      readln(F,S);  //GR3D  on enlève la première ligne
      readln(F,S);  //GR3D1 on lit la deuxième ligne
      Extrait(S,' ');   //et on enlève le début
      Param_Grille(S);
      readln(F,S);  //GR3D2 on enlève la troisième ligne de l'entête
      readln(F,S);  //GR3D3 on enlève la dernière ligne de l'entête
    end
    else begin
      readln(F,S);  // lecture de la ligne d'entête
      Param_Grille(S);
      if S='' then
        Grille_Format:=gf_mnt_france
      else
        Grille_Format:=gf_mnt_nc;
    end;

    NbPasLong:=round((LongMax-LongMin)/PasLong)+1;
    NbPasLat:=round((LatMax-LatMin)/PasLat)+1;
    SetLength(FCoefX,NbPasLong*NbPasLat);
    SetLength(FCoefY,NbPasLong*NbPasLat);
    SetLength(FCoefZ,NbPasLong*NbPasLat);

    if Grille_Format=gf_mnt_france then
    begin // tout sur une ligne
      readln(F,S);
      if Pos(' ',S)<>0 then
        Separateur:=' '
      else
        Separateur:=#9;
      for I:=0 to NbPasLong*NbPasLat-1 do
      begin
        FCoefX[I]:=StrToFloat(Extrait(S,Separateur));
        FCoefY[I]:=StrToFloat(Extrait(S,Separateur));
        FCoefZ[I]:=StrToFloat(Extrait(S,Separateur));
        Extrait(S,Separateur); // élimination du coefficient de précision
      end;
    end
    else begin
      Separateur:=#0;
      while not(Eof(F)) do
      begin
        readln(F,S);
        if S<>'' then // il peut y avoir des lignes vide à ne pas traiter (gr3dnc03a.mnt)
        begin
          if Separateur=#0 then // on utilise la première ligne pour détecter le séparateur
            if Pos(' ',S)<>0 then
              Separateur:=' '
            else
              Separateur:=#9;
          if Grille_Format=gf_txt_france then
            Extrait(S,Separateur); // on élimine le coefficent devant la ligne
          Lit_Un_Point(S,Separateur);
        end;
      end;
    end;
 
  finally
    CloseFile(F);
  end;
end;

function TGrilleIgn.GetTranslation(Lat, Long : real; var TX, TY, TZ : real) : boolean;
var
  NbPasLat, I  : integer;
  IndiceLong, IndiceLat, A, B, C, D : real;
begin
  if (Long<LongMin) or (Long>LongMax) or (Lat<LatMin) or (Lat>LatMax) then
    result:=false
  else
  begin
    result:=true;
    try
      NbPasLat:=round((LatMax-LatMin)/PasLat)+1;
      IndiceLong:=(Long-LongMin)/PasLong;
      IndiceLat:=(Lat-LatMin)/PasLat;
      I:=trunc(IndiceLong)*NbPasLat+Trunc(IndiceLat);
      A:=(1-frac(IndiceLong))*(1-frac(IndiceLat));
      B:=frac(IndiceLong)*(1-frac(IndiceLat));
      C:=(1-frac(IndiceLong))*frac(IndiceLat);
      D:=frac(IndiceLong)*frac(IndiceLat);
      TX:=A*FCoefX[I]+B*FCoefX[I+NbPasLat]+C*FCoefX[I+1]+D*FCoefX[I+NbPasLat+1];
      TY:=A*FCoefY[I]+B*FCoefY[I+NbPasLat]+C*FCoefY[I+1]+D*FCoefY[I+NbPasLat+1];
      TZ:=A*FCoefZ[I]+B*FCoefZ[I+NbPasLat]+C*FCoefZ[I+1]+D*FCoefZ[I+NbPasLat+1];
    except
      result:=false;
    end;
  end;
end;

procedure TGrilleIgn.SetLatMax(const Value: real);
begin
  FLatMax := Value;
end;

procedure TGrilleIgn.SetLatMin(const Value: real);
begin
  FLatMin := Value;
end;

procedure TGrilleIgn.SetLongMax(const Value: real);
begin
  FLongMax := Value;
end;

procedure TGrilleIgn.SetLongMin(const Value: real);
begin
  FLongMin := Value;
end;

procedure TGrilleIgn.SetPasLat(const Value: real);
begin
  FPasLat := Value;
end;

procedure TGrilleIgn.SetPasLong(const Value: real);
begin
  FPasLong := Value;
end;

end.
