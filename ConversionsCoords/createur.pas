unit createur;

// unité chargée de la creation des projections d'après leur type

interface

uses
  ancetre,
  projection, multiple,
  lambert, lambert_tan, lambert_sec, lambert_oblic,
  transmercator_brut, transmercator, utm, mtm, mercator_multi,
  rosenmund, laborde, laborde_courbure, laborde_equatoriale, laborde_bitan,
  laborde_oblic, hotine, hotine_centre,
  stereographic, stereo_bitan, stereo_courbure, stereo_equatoriale, stereo_polaire,
  newzealand, bonne, mercator_sphere, projlatlong;

type

  TProjectionClass = class of TProjection;

  TRelationProj = record
    Texte : string;
    Objet : TProjectionClass;
  end;

  TMultipleClass = class of TMultiple;

  TRelationMulti = record
    Texte : string;
    Objet : TMultipleClass;
  end;

const
  RelationProj : array[0..24] of TRelationProj =(
    (Texte : tpLambertConique; Objet : TLambert),
    (Texte : tpLambertConiqueTangent; Objet : TLambert_tan),
    (Texte : tpLambertConiqueSecant; Objet : TLambert_sec),
    (Texte : tpLambertConiqueOblique; Objet : TLambert_oblic),

    (Texte : tpTransverseMercatorBrut; Objet : TTransverseMercatorBrut),
    (Texte : tpTransverseMercator ; Objet : TTransverseMercator),
    (Texte : tpUTM ; Objet : TUTM),
    (Texte : tpMTM ; Objet : TMTM),

    (Texte : tpGaussRosenmund; Objet : TRosenmund),

    (Texte : tpGaussLaborde ; Objet : TLaborde),
    (Texte : tpGaussLabordeCourbure ; Objet : TLaborde_courbure),
    (Texte : tpGaussLabordeequatoriale ; Objet : TLaborde_equatoriale),
    (Texte : tpGaussLabordeBitangente ; Objet : TLaborde_Bitan),

    (Texte : tpGaussLabordeOblique ; Objet : TLabordeOblic),
    (Texte : tpHotine; Objet : THotine),
    (Texte : tpHotineCentre; Objet : THotineCentre),

    (Texte : tpStereographic ; Objet : TStereographic),
    (Texte : tpStereographicCourbure ; Objet : TStereo_courbure),
    (Texte : tpStereographicequatoriale ; Objet : TStereo_equatoriale),
    (Texte : tpStereographicBitangente ; Objet : TStereo_Bitan),
    (Texte : tpStereographicPolaire ; Objet : TStereo_Polaire),

    (Texte : tpNewZealandMapGrid; Objet : TNewZealand),
    (Texte : tpBonne; Objet : TBonne),
    (Texte : tpMercatorSphere; Objet : TMercator_sphere),
    (Texte : tpProjLatLong; Objet : TProj_LatLonj)
    );


  RelationMulti : array[0..0] of TRelationMulti =(
    (Texte : tmMercatorMultiple; Objet : TMultipleMercator)
    );


function CreateProjection(QuelType : string) : TProjection;

function CreateMultiple(QuelType : string) : TMultiple;

function Nb_Param_Projection(QuelType : string) : integer;

function Nom_Param_Projection(QuelType : string; Num_Param : integer) : string;

implementation

//uses ancetre;

function CreateProjection(QuelType : string) : TProjection;
var
  I : integer;
begin
  result:=nil;
  for I:=low(RelationProj) to high(RelationProj) do
    if QuelType=RelationProj[I].Texte then
      result:=(RelationProj[I].Objet).Create;
end;

function CreateMultiple(QuelType : string) : TMultiple;
var
  I : integer;
begin
  result:=nil;
  for I:=low(RelationMulti) to high(RelationMulti) do
    if QuelType=RelationMulti[I].Texte then
      result:=(RelationMulti[I].Objet).Create;
end;

function Nb_Param_Projection(QuelType : string) : integer;
var
  Projection : TAncetre;
begin
  try
    Projection:=CreateProjection(QuelType);
    if Projection=nil then
      Projection:=CreateMultiple(QuelType);
    if Projection=nil then
      result:=-1
    else begin
      with Projection do
        result:=Nb_ParamReal+Nb_ParamInteger+Nb_ParamAngle+Nb_ParamBoolean;
      Projection.Free;
    end;  
  except
    result:=-1;
  end;
end;

function Nom_Param_Projection(QuelType : string; Num_Param : integer) : string;
var
  Projection : TAncetre;
begin
  try
    Projection:=CreateProjection(QuelType);
    if Projection=nil then
      Projection:=CreateMultiple(QuelType);
    if Projection=nil then
      result:=''
    else begin
      with Projection do
      begin
        if (Num_Param>=0) and (Num_Param<Nb_ParamReal) then
          result:=Nom_ParamReal[Num_Param]
        else begin
          dec(Num_Param,Nb_ParamReal);
          if (Num_Param>=0) and (Num_Param<Nb_ParamInteger) then
            result:=Nom_ParamInteger[Num_Param]
          else begin
            dec(Num_Param,Nb_ParamInteger);
            if (Num_Param>=0) and (Num_Param<Nb_ParamAngle) then
              result:=Nom_ParamAngle[Num_Param]
            else begin
              dec(Num_Param,Nb_ParamAngle);
              if (Num_Param>=0) and (Num_Param<Nb_ParamBoolean) then
                result:=Nom_ParamBoolean[Num_Param]
              else
                result:='';
            end;
          end;
        end;
      end;
      Projection.Free;
    end;
  except
    result:='';
  end;
end;

end.
