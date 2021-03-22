unit ConversionOutils;

// DONE: Tous les symboles '°', de codages différents (UTF8, ANSI, ...)
//       ont été remplacés par 'd'.

interface

uses
  SysUtils, Math, Classes;

const

  TextBoolean : array[boolean] of string=('0','1');

type

  TUniteAngle = (uaDegre, uaDMS, uaGrade, uaDM);

// conversion d'angle en radian vers du texte et réciproquement
// en grade, un angle s'exprime sous la forme '45.986244g'
// en degrés décimaux, un angle s'exprime sous la forme '48.986244d'
// en degrés sexadécimaux, un angle s'exprime sous la forme '45d31'28.568'''   ** DEPRECATED **
// en degrés, minutes décimales, un angle s'exprime sous la forme '45d31.658'' ** DEPRECATED **
// dans tous les cas, le séparateur décimal est le point
// il ne doit pas y avoir d'espace

function StrToAngle(S : string) : double;
function AngleToStr(Angle : real; Unite : TUniteAngle) : string;
function AngleToStrDegreDecimaux(Angle : real) : string;

function AngleToStrGrade(Angle : real) : string;
function AngleToStrDegreSexa(Angle : real) : string;
function AngleToStrDegreMinute(Angle : real) : string;
//*)

function UniteAngleToInt(Unite : TUniteAngle) : integer;
function IntToUniteAngle(Entier : integer) : TUniteAngle;

// extrait le texte jusqu'au signe = et positionne le flux à la suite
function DebutLigne(Flux : TStream) : string;
// extrait le texte juste à la fin de la ligne et positionne le flux au début
// de la ligne suivante
function FinLigne(Flux : TStream) : string;
// extrait les différentes parties de la chaîne S
procedure DetailParam(S : string; var ListeParam : TstringList);
// même chose mais pour une ligne de texte
//function GetDetailParams(const S : string): TStringArray;


implementation
//uses Common; // provisoire, pour AfficherMessage


function StrToAngle( S : string) : double;
var
  Dernier : char;
  Extrait : string;
  Pivot : integer;
  Negatif : boolean;
begin
  assert( defaultFormatSettings.DecimalSeparator='.');
  //AfficherMessage('-StrToAngle --' + S);
  if S='' then
    raise EConvertError.Create('Erreur de conversion d''angle, texte vide');
  Dernier:=S[length(S)];
  case Dernier of
    '0' :
    begin // angle nul ?
      if S='0' then
        result:=0
      else
        raise EConvertError.Create('Erreur de conversion d''angle, angle nul');
    end;
    'g' :
    begin // en grade
      delete(S,length(S),1);
      result:=GradToRad(StrToFloat(S));
    end;
    'd' : begin // en degrés décimaux
      delete(S,length(S),1);
      result:=DegToRad(StrToFloat(S));
    end;
    '''', 'm' :
    begin // en degrés sexadécimaux '45d31'28.568'''
                 // ou en degrés minutes
      Negatif:=(S[1]='-');
      if Negatif then
        delete(S,1,1);
      delete(S,length(S),1);
      Pivot:=pos('d',S);
      if Pivot=0 then
        raise EConvertError.Create('Erreur de conversion d''angle, l''unité des degrés (d) n''a pas été trouvée');
      Extrait:=copy(S,1,Pivot-1);
      result:=StrToInt(Extrait);
      delete(S,1,Pivot);
      Pivot:=pos('''',S);
      if Pivot=0 then
      begin // il n'y a que des minutes, pas de secondes
//        result:=result+StrToInt(S)/60;
        result:=result+StrToFloat(S)/60;
        result:=DegToRad(result);
      end
      else begin // on a minutes et secondes
        Extrait:=copy(S,1,Pivot-1);
        result:=result+StrToInt(Extrait)/60;
        delete(S,1,Pivot);
        if S[length(S)]<>'''' then
          raise EConvertError.Create('Erreur de conversion d''angle, l''unité des secondes '''' n''a pas été trouvée');
        delete(S,length(S),1);
        result:=result+StrToFloat(S)/3600;
        result:=DegToRad(result);
      end;
      if Negatif then
        result:=-result;
    end;
  else
    begin
      raise EConvertError.Create('Erreur de conversion d''angle, unité inconnue');

    end;
  end;
  //*)
end;

function AngleToStr(Angle : real; Unite : TUniteAngle) : string;
begin
  case Unite of
    uaDegre : result:=AngleToStrDegreDecimaux(Angle);
    uaDMS : result:=AngleToStrDegreSexa(Angle);
    uaGrade : result:=AngleToStrGrade(Angle);
    uaDM : result:=AngleToStrDegreMinute(Angle);
  else
    result:=AngleToStrDegreDecimaux(Angle);
  end;
end;

function AngleToStrGrade(Angle : real) : string;
begin

  assert( defaultFormatSettings.DecimalSeparator='.');
  result:=Format('%.8fg',[RadToGrad(Angle)]);
  //AfficherMessage('-- AngleToStrGrade ' + Result);
end;

function AngleToStrDegreDecimaux(Angle : real) : string;
begin
  assert( defaultFormatSettings.DecimalSeparator='.');
  result:=Format('%.8fd',[RadToDeg(Angle)]);
  //AfficherMessage('-- AngleToStrDegDec ' + Result);
end;

function AngleToStrDegreSexa(Angle : real) : string;
var
  Total : real;
  Degre, Minute, Seconde, MilliSeconde : integer;
  Negatif : boolean;
begin
  assert( defaultFormatSettings.DecimalSeparator='.');
  Total:=RadToDeg(Angle);
  Negatif:=(Total<0);
  if Negatif then
    Total:=-Total;
  MilliSeconde:=round(Total*3600*1000);
  Degre:=MilliSeconde div (3600*1000);
  MilliSeconde:=MilliSeconde-Degre*(3600*1000);
  Minute:=MilliSeconde div (60*1000);
  MilliSeconde:=MilliSeconde-Minute*(60*1000);
  Seconde:=MilliSeconde div 1000;
  MilliSeconde:=MilliSeconde-Seconde*1000;
  if MilliSeconde=0 then
  begin
    if Seconde=0 then
    begin
      if Minute=0 then
        result:=Format('%dd',[Degre])
      else
        result:=Format('%dd%.2d''',[Degre,Minute]);
    end
    else
      result:=Format('%dd%.2d''%.2d''''',[Degre,Minute,Seconde]);
  end
  else
    result:=Format('%dd%.2d''%.2d.%.3d''''',[Degre,Minute,Seconde,MilliSeconde]);
  if Negatif then
    result:='-'+result;
   //AfficherMessage('-- AngleToDegSexa ' + Result);
end;

function AngleToStrDegreMinute(Angle : real) : string;
var
  Total : real;
  Degre, Minute, MilliMinute : integer;
  Negatif : boolean;
begin
  assert( defaultFormatSettings.DecimalSeparator='.');
  Total:=RadToDeg(Angle);
  Negatif:=(Total<0);
  if Negatif then
    Total:=-Total;
  MilliMinute:=round(Total*60*1000);
  Degre:=MilliMinute div (60*1000);
  MilliMinute:=MilliMinute-Degre*(60*1000);
  Minute:=MilliMinute div 1000;
  MilliMinute:=MilliMinute-Minute*1000;
  if MilliMinute=0 then
  begin
    if Minute=0 then
      result:=Format('%dd',[Degre])
    else
      result:=Format('%dd%.2d''',[Degre,Minute]);
  end
  else
    result:=Format('%dd%.2d.%.3d''',[Degre,Minute,MilliMinute]);
  if Negatif then
    result:='-'+result;
   //AfficherMessage('-- AngleToDegMin ' + Result);
end;

function UniteAngleToInt(Unite : TUniteAngle) : integer;
begin
  case Unite of
    uaDegre : result:=0;
    uaDMS   : result:=1;
    uaDM    : result:=2;
    uaGrade : result:=3;
    //else Raise EConvertError.Create('Unité d''angle inconnue');
    else
      Result := 0;
  end;
end;


function IntToUniteAngle(Entier : integer) : TUniteAngle;
begin
  case Entier of
    0 : result:=uaDegre;
    1 : result:=uaDMS;
    2 : result:=uaDM;
    3 : result:=uaGrade;
    //else Raise EConvertError.Create('L''entier %d ne correspond à aucune unité d''angle'{,[Entier]});
    else Result := uaDegre;
  end;
end;

function DebutLigne(Flux : TStream) : string;
// extrait le texte jusqu'au signe =
var
  C : char;
begin
  result:='';
  if Flux.Position<Flux.Size then
  begin
    repeat
      Flux.Read(C,1);
      if (C<>'=') then
        result:=result+C;
    until (C='=') or (Flux.Position=Flux.Size);
    (*
    while (result<>'') and (result[length(result)] in [' ',#9]) do
      delete(result,length(result),1);
    //*)
    Result := Trim(Result);
  end;
end;

function FinLigne(Flux : TStream) : string;
// extrait le texte juste à la fin de la ligne
var
  C : char;
begin
  result:='';
  if Flux.Position<Flux.Size then
  begin
    // lecture jusqu'à la fin de la ligne
    repeat
      Flux.Read(C,1);
      if (C<>#13) and (C<>#10) then
        result:=result+C;
    until (C=#13) or (C=#10) or (Flux.Position=Flux.Size);
    // recherche du début de la ligne suivante
    if Flux.Position<>Flux.Size then
    repeat
      Flux.Read(C,1);
    until ((C<>#13) and (C<>#10)) or (Flux.Position=Flux.Size);
    if Flux.Position<>Flux.Size then
      Flux.Seek(-1,soFromCurrent);
  end;
end;

procedure DetailParam(S : string; var ListeParam : TstringList);
// extrait les différentes parties de la chaîne S
var
  Morceau : string;
begin
  ListeParam.Clear;
  while S<>'' do
  begin
    // recherche du début d'un paramètre
    while (S<>'') and ((S[1]=' ') or (S[1]=#9)) do
      delete(S,1,1);
    if S<>'' then
    begin
      if S[1]='"' then // paramètre texte
      begin
        delete(S,1,1);
        ListeParam.Add(Copy(S,1,pos('"',S)-1));
        delete(S,1,pos('"',S));
      end
      else begin // autre paramètre
        Morceau:='';
        while (S<>'') and (S[1]<>' ') and (S[1]<>#9) do
        begin
          Morceau:=Morceau+S[1];
          delete(S,1,1);
        end;
        ListeParam.Add(Morceau);
      end;
    end;
  end;
end;

end.
