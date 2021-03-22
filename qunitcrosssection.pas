unit QUnitCrossSection;

{$mode delphi}{$H+}

interface

uses
  GHCD_Types,
  UnitListesSimplesWithGeneriques,
  Classes, SysUtils;

type TCSArcCourbe = record
  P1   : TPoint2Df;
  TanP1: TPoint2Df;
  P2   : TPoint2Df;
  TanP2: TPoint2Df;
end;

type TCSCourbe = record
  IDStyle: integer;
  Arcs   : array of TCSArcCourbe;
end;

type TCSPolygone = record
  IDStyle: integer;
  Sommets: array of TPoint2Df;
end;

type TCSSimpleLigne = record
  IDStyle : integer;
  Extr1   : TPoint2Df;
  Extr2   : TPoint2Df;
end;
type TCSTexte = record
  IDStyle : integer;
  PosXY   : TPoint2Df;
  Text    : string;
end;



type

{ TCrossSection }

 TCrossSection = class
  private
    FBaseStation       : TBaseStation;
    FIDGroupe          : TIDGroupeEntites;
    FDecalageXY        : TPoint2Df; // décalage XY entre le point topo et le centre de la section transversale
    FListeCourbes      : TListeSimple<TCSCourbe>;
    FListePolygones    : TListeSimple<TCSPolygone>;
    FListeSimpleLignes : TListeSimple<TCSSimpleLigne>;
    FListeTextes       : TListeSimple<TCSTexte>;


  public
    function  Initialiser(const B: TBaseStation): boolean;
    procedure SetIDGroupe(const G: TIDGroupeEntites);
    procedure SetDecalageXY(const QX, QY: double);
    procedure Finaliser();
    function  Serialiser(out LS: TStringList): boolean;

    function  getNbCourbes(): integer;        inline;
    function  getNbPolygones(): integer;      inline;
    function  getNbSimplesLignes(): integer;  inline;
    function  getNbTextes(): Integer;         inline;

    procedure addCourbe(const Q: TCSCourbe)              ; inline;
    procedure addPolygone(const Q: TCSPolygone)          ; inline;
    procedure addSimpleLigne(const Q: TCSSimpleLigne)    ; inline;
    procedure addSimpleLigneByValues(const QIDStyle: integer; const QExtr1X, QExtr1Y, QExtr2X, QExtr2Y: double);
    procedure addTexte(const Q: TCSTexte)                ; inline;
    procedure addTexteByValues(const QIDStyle: integer; const QX, QY: double; const QText: string);


    function  getCourbe(const Idx: integer): TCSCourbe              ; inline;
    function  getPolygone(const Idx: integer): TCSPolygone          ; inline;
    function  getSimpleLigne(const Idx: integer): TCSSimpleLigne    ; inline;
    function  getTexte(const Idx: integer): TCSTexte                ; inline;

    procedure putCourbe(const Idx: integer; const Q: TCSCourbe)              ; inline;
    procedure putPolygone(const Idx: integer; const Q: TCSPolygone)          ; inline;
    procedure putSimpleLigne(const Idx: integer; const Q: TCSSimpleLigne)    ; inline;
    procedure putTexte(const Idx: integer; const Q: TCSTexte)                ; inline;

    procedure RemoveCourbe(const Idx: integer)             ; inline;
    procedure RemovePolygone(const Idx: integer)           ; inline;
    procedure RemoveSimpleLigne(const Idx: integer)        ; inline;
    procedure RemoveTexte(const Idx: integer)              ; inline;


end;

implementation

{ TCrossSection }

function TCrossSection.Initialiser(const B: TBaseStation): boolean;
begin
  FBaseStation       := B;
  FDecalageXY.X      := 0.00; FDecalageXY.Y      := 0.00;
  FListeCourbes      := TListeSimple<TCSCourbe>.Create;
  FListePolygones    := TListeSimple<TCSPolygone>.Create;
  FListeSimpleLignes := TListeSimple<TCSSimpleLigne>.Create;
  FListeTextes       := TListeSimple<TCSTexte>.Create;
  FListeCourbes.ClearListe();
  FListePolygones.ClearListe();
  FListeSimpleLignes.ClearListe();
  FListeTextes.ClearListe();
end;

procedure TCrossSection.SetIDGroupe(const G: TIDGroupeEntites);
begin
  FIDGroupe := G;
end;

procedure TCrossSection.SetDecalageXY(const QX, QY: double);
begin
  FDecalageXY.X := QX; FDecalageXY.Y := QY;
end;

procedure TCrossSection.Finaliser;
begin
  try
    FListeCourbes.ClearListe();
    FListePolygones.ClearListe();
    FListeSimpleLignes.ClearListe();
    FListeTextes.ClearListe();
  finally
    FreeAndNil(FListeCourbes);//FListeCourbes.Free;
    FreeAndNil(FListePolygones);//FListePolygones.Free;
    FreeAndNil(FListeSimpleLignes);//FListeSimpleLignes.Free;
    FreeAndNil(FListeTextes);//FListeTextes.Free;
  end;
end;

function TCrossSection.getNbCourbes: integer;
begin
  Result := FListeCourbes.GetNbElements();
end;

function TCrossSection.getNbPolygones: integer;
begin
  Result := FListePolygones.GetNbElements();
end;

function TCrossSection.getNbSimplesLignes: integer;
begin
  Result := FListeSimpleLignes.GetNbElements();
end;

function TCrossSection.getNbTextes: Integer;
begin
  Result := FListeTextes.GetNbElements();
end;

procedure TCrossSection.addCourbe(const Q: TCSCourbe);
begin
  FListeCourbes.AddElement(Q);
end;

procedure TCrossSection.addPolygone(const Q: TCSPolygone);
begin
  FListePolygones.AddElement(Q);
end;

procedure TCrossSection.addSimpleLigne(const Q: TCSSimpleLigne);
begin
  FListeSimpleLignes.AddElement(Q);
end;

procedure TCrossSection.addSimpleLigneByValues(const QIDStyle: integer; const QExtr1X, QExtr1Y, QExtr2X, QExtr2Y: double);
var
  EWE: TCSSimpleLigne;
begin
  EWE.IDStyle := QIDStyle;
  EWE.Extr1.X := QExtr1X;
  EWE.Extr1.Y := QExtr1Y;
  EWE.Extr2.X := QExtr2X;
  EWE.Extr2.Y := QExtr2Y;
  addSimpleLigne(EWE);
end;

procedure TCrossSection.addTexte(const Q: TCSTexte);
begin
  FListeTextes.AddElement(Q);
end;

procedure TCrossSection.addTexteByValues(const QIDStyle: integer; const QX, QY: double; const QText: string);
var
  EWE: TCSTexte;
begin
  EWE.IDStyle := QIDStyle;
  EWE.PosXY.X := QX; EWE.PosXY.Y := QY;
  EWE.Text    := QText;
  addTexte(EWE);
end;

function TCrossSection.getCourbe(const Idx: integer): TCSCourbe;
begin
  Result := FListeCourbes.GetElement(Idx);
end;

function TCrossSection.getPolygone(const Idx: integer): TCSPolygone;
begin
  Result := FListePolygones.GetElement(Idx);
end;

function TCrossSection.getSimpleLigne(const Idx: integer): TCSSimpleLigne;
begin
  Result := FListeSimpleLignes.GetElement(Idx);
end;

function TCrossSection.getTexte(const Idx: integer): TCSTexte;
begin
  Result := FListeTextes.GetElement(Idx);
end;

procedure TCrossSection.putCourbe(const Idx: integer; const Q: TCSCourbe);
begin
  FListeCourbes.PutElement(Idx, Q);
end;

procedure TCrossSection.putPolygone(const Idx: integer; const Q: TCSPolygone);
begin
  FListePolygones.PutElement(Idx, Q);
end;

procedure TCrossSection.putSimpleLigne(const Idx: integer; const Q: TCSSimpleLigne);
begin
  FListeSimpleLignes.PutElement(Idx, Q);
end;

procedure TCrossSection.putTexte(const Idx: integer; const Q: TCSTexte);
begin
  FListeTextes.PutElement(Idx, Q);
end;

procedure TCrossSection.RemoveCourbe(const Idx: integer);
begin
  FListeCourbes.RemoveElement(Idx);
end;

procedure TCrossSection.RemovePolygone(const Idx: integer);
begin
  FListePolygones.RemoveElement(Idx);
end;

procedure TCrossSection.RemoveSimpleLigne(const Idx: integer);
begin
  FListeSimpleLignes.RemoveElement(Idx);
end;

procedure TCrossSection.RemoveTexte(const Idx: integer);
begin
  FListeTextes.RemoveElement(Idx);
end;
//******************************************************************************
function TCrossSection.Serialiser(out LS: TStringList): boolean;
  procedure QCommentaire(const Indent: integer; const QS: string);
  begin
    LS.Add(StringOfChar(' ', Indent) + '#' + QS);
  end;
  function NbRealToStr(const V: double): string;
  begin
    Result := Format('%.3f', [V]);
  end;

  procedure QBeginSection(const Indent: integer; const QS: string; const Args: array of string);
  var
    EWE: String;
    n, i: Integer;
  begin
    EWE := StringOfChar(' ', Indent) +  QS;
    n := Length(Args);
    if (n > 0) then
    begin
      for i := 0 to n - 1 do EWE := EWE + #9 + Args[i];
    end;
    LS.Add(EWE);
  end;

  procedure QEndSection(const Indent: integer; const QS: string);
  begin
    LS.Add(StringOfChar(' ', Indent) +  'end' + QS);
  end;
  procedure QLnTexte(const Indent: integer; const T: TCSTexte);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', Indent) + #9 + 'CSTexte' + #9;
    WU := WU + Format('%d' + #9 + '%.3f' + #9 + '%.3f' + #9 +'%s',
                     [T.IDStyle, T.PosXY.X, T.PosXY.Y, T.Text]);
    LS.Add(WU);

  end;
  procedure QLnSimpleLigne(const Indent: integer; const L: TCSSimpleLigne);
  var
    WU: String;
  begin
    WU := StringOfChar(' ', Indent) + #9 + 'CSSimpleLigne' + #9;
    WU := WU + Format('%d' + #9 + '%.3f' + #9 + '%.3f' + #9 + '%.3f' + #9 + '%.3f',
                     [L.IDStyle, L.Extr1.X, L.Extr1.Y, L.Extr2.X, L.Extr2.Y]);
    LS.Add(WU);

  end;
var
  n, i: Integer;
begin
  LS.Clear;
  QCommentaire(2, 'Parameters: IDCrossSection, IDGroupe, IDBasePoint, OffsetX, OffsetY');
  QBeginSection(2, 'crosssection', [IntToStr(1), IntToStr(FIDGroupe), Inttostr(FBaseStation.IDStation), NbRealToStr(FDecalageXY.X), NbRealToStr(FDecalageXY.Y)]);
    QBeginSection(4, 'CSSimplesLignes', []);
      n := getNbSimplesLignes();
      if (n > 0) then
      begin
        for i := 0 to n - 1 do  QLnSimpleLigne(6, getSimpleLigne(i));
      end;
    QEndSection(4, 'CSSimplesLignes');
    QBeginSection(4, 'CSTextes', []);
      n := getNbTextes();
      if (n > 0) then
      begin
        for i := 0 to n - 1 do  QLnTexte(6, getTexte(i));
      end;
    QEndSection(4, 'CSTextes');
  QEndSection(2, 'crosssection');

end;

end.

