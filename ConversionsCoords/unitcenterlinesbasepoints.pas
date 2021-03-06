unit UnitCenterlinesBasePoints;
// Gestion des centerlines (section Basepoints) + MetaFiltre
// 17/11/2013: Opérationnel
// 02/12/2014: Seules les stations sont sélectionnables
// 10/06/2015: ID de terrain des stations pour TBaseStation
// 24/06/2015: Les antennes sont désormais métafiltrées (leur ID est égal à -ID de
//             leur station d'accrochage)
// 01/07/2015: Filtre Couleur
// 08/10/2015: Filtre Secteur + corrections
// 28/10/2015: Restauration de la version 09/10/2015 de l'unité
// 10/11/2015: Ajout du filtre STATION=<ID>; eg: STATION=1055.3

{$INCLUDE CompilationParameters.inc}
interface
uses
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GHCD_Types,
  UnitListesSimplesWithGeneriques,
  SysUtils, Classes, Math, Graphics,

  GeneralFunctions;
type

{ TCenterLines }

 TCenterLines = class(TListeSimple<TBaseStation>)
  private
    procedure ViderListe(); inline;
  public
    function  InitialiseCenterLines(): boolean;
    procedure FinaliseCenterLines();
    // filtre
    function MetaFiltrer(const MyFiltre: string): integer;
    // les classiques: ajout, modif, accès, comptage
    function  GetNbBasePoints(): integer; inline;
    function  GetBasePoint(const Idx: integer): TBaseStation; inline;
    procedure AddBasePoint(const BP: TBaseStation); inline;
    procedure PutBasepoint(const Idx: integer; const BP: TBaseStation); inline;
    // recherche
    function FindBasePointByIndex(const Idx: TIDBaseStation; out BP: TBaseStation): boolean;
    function FindBasePointByCle(const Cle: string; out BP: TBaseStation): boolean;
    function GetNearBasePointA(const X, Y: double; const OldBasePoint: TBaseStation; const StLocked: boolean): TBaseStation;
    // extractions
    function  ExtractBasePointFromLine(const MyLine: string): TBaseStation;
    // utilitaires
    procedure TrierListeBasePointsParIndex();
    function ImporterBasePoints(const FichierGCP: string): boolean;
    function ExporterBasePoints(const FichierGCP: string; const Tag: string): boolean;

end;

implementation

function  TCenterLines.ImporterBasePoints(const FichierGCP: string): boolean;
var
  fp   : TextFile;
  Line: string;
  NoLine: integer;
  i   : integer;
  BasePt: TBaseStation;
  Nb: Integer;
  MyLine: String;
  EWE: TBaseStation;
  function LireLigne: string;
  begin
    ReadLn(fp, Result);
    Result := Trim(Result);
    Inc(NoLine);
  end;
begin
  ViderListe();
  Result := False;
  AfficherMessage(ClassName, Format('ImportBasePoints(%s)',[FichierGCP]));
  AssignFile(fp, FichierGCP);
  NoLine := 0;
  try
    ReSet(fp);
    while (not EOF(fp)) do
    begin
      MyLine := LireLigne;
      if (MyLine = '') then Continue;
      if (Pos('#', MyLine) > 0) then Continue; // commentaires
      // lecture points de base
      if (MyLine = BASEPOINTMARKER) then
      begin
        AfficherMessage(Format('--> (%d) - ''%s'' section found',[NoLine, BASEPOINTMARKER]));
        while (True) do
        begin
          MyLine := LireLigne;
          if (MyLine = '') then Continue;
          if (Pos('#', MyLine) > 0) then Continue; // commentaires
          if (MyLine = ENDBASEPOINTMARKER) then Break;
          EWE := ExtractBasePointFromLine(MyLine);
          self.AddBasePoint(EWE);
        end;
      end; //if (Line = BASEPOINTMARKER) then begin
    end;
    // ne pas oublier de trier
    self.TrierListeBasePointsParIndex();
    Result := True;
  finally
    CloseFile(fp);
  end;
end;

function TCenterLines.ExporterBasePoints(const FichierGCP: string; const Tag: string): boolean;
var
  F:  TextFile;
  i, Nb: Integer;
  BP: TBaseStation;

begin
  AssignFile(F, FichierGCP);
  try
    ReWrite(f);
    WriteLn(F, Format('# File %s saved %s %s',[FichierGCP, DateToStr(Now), TimeToStr(Now)]));
    WriteLn(F, Format('# Centerlines of %s',[Tag]));
    WriteLn(F, Format('# Insert following command in .GCD file: %s %s',[INSTRUCTION_INCLUDE_GCP, ExtractFileName(FichierGCP)]));
    WriteLn(F, '');
    WriteLn(F, Format('# Producer: %s - %s', [rsGHCAVEDRAWEXENAME, GetGHCaveDrawVersionAndDateBuild()]));
    WriteLn(F, BASEPOINTMARKER);
    WriteLn(F, '  # Parameters: IDStation; Caption; GHTopo TypeStation; integer Colour; Start shot, End shot, Left and Right coordinates');
    Nb := self.GetNbBasePoints();
    AfficherMessage(Format('--> Saving %d basepoints', [Nb]));
    for i:=0 to Nb - 1 do
    begin
      BP := self.GetBasePoint(i);
      WriteLn(F, GenererLigneBasePoint(i, BP));
    end;
    WriteLn(F, ENDBASEPOINTMARKER);
  finally
    CloseFile(F);
  end;
end;


// Trier les visées par ZOrder
function SortBaseStations(Item1, Item2: Pointer): Integer;
var
  E1, E2: ^TBaseStation;
  bidon1: TIDBaseStation;
  bidon2: TIDBaseStation;
begin
  E1:=Item1;
  E2:=Item2;
  bidon1 := E1^.IDStation;
  bidon2 := E2^.IDStation;
  if (bidon1 < bidon2) then
    Result := -1
  else if (bidon1 = bidon2) then
    Result := 0
  else
    Result := 1;
end;

{ TCenterLines }

procedure TCenterLines.ViderListe;
begin
  self.ClearListe();
end;

function TCenterLines.InitialiseCenterLines: boolean;
begin
  AfficherMessage(self.ClassName, 'InitialiseCenterLines');
  Result := False;
  try
    ViderListe();
    Result := true;
  finally
  end;
end;

procedure TCenterLines.FinaliseCenterLines;
begin
  AfficherMessage(self.ClassName, 'FinaliseCenterLines');
  ViderListe();
end;
//******************************************************************************
function TCenterLines.GetNbBasePoints: integer;
begin
  Result := self.GetNbElements();
end;

function TCenterLines.GetBasePoint(const Idx: integer): TBaseStation;
begin
  Result := self.GetElement(Idx);
end;

procedure TCenterLines.AddBasepoint(const BP: TBaseStation);
begin
  self.AddElement(BP);
end;
procedure TCenterLines.PutBasepoint(const Idx: integer; const BP: TBaseStation);
begin
  self.PutElement(Idx, BP);
end;
//******************************************************************************
function TCenterLines.FindBasePointByIndex(const Idx: TIDBaseStation; out BP: TBaseStation): boolean;
var
  i: Integer;
  // recherche par méthode récursive dichotomique
  function FindDepth(const I1, I2: TIDBaseStation; const QIDX: TIDBaseStation):  TIDBaseStation;
  var
    C1: TBaseStation;
    PVT: integer;
  begin
    // coupure en deux => calcul index médian
    PVT := (I2+I1) div 2;
    // début > fin >> sortie directe avec erreur
    if (I1 > I2) then begin Result := -1; exit; end;//
    C1 := GetBasePoint(PVT); //GetBasePoint(PVT);
    // comparaison. Si vrai >> sortie avec numéro d'index
    if (C1.IDStation = QIDX) then
    begin
      Result := PVT;
      Exit;
    end;
    // sinon, recherche en profondeur avec un niveau supplémentaire
    if (QIDX < C1.IDStation) then
    begin
      Result := FindDepth(I1, PVT-1, QIDX);
      Exit;
    end;
    Result := FindDepth(PVT+1, I2, QIDX);
  end;
begin
  Result := false;
  i := FindDepth(0, GetNbBasePoints - 1, Idx);
  if (i >= 0) then
  begin
    BP     := GetBasePoint(i);
    Exit(True);
  end;
  //AfficherMessage('=== Recherche infructeuse - Balayage de la table');
  for i:=0 to GetNbBasePoints - 1 do
  begin
    BP := GetBasePoint(i);
    if (BP.IDStation = TIDBaseStation(Idx)) then Exit(True);
  end;
  // point non trouvé
end;

function TCenterLines.FindBasePointByCle(const Cle: string; out BP: TBaseStation): boolean;
var
  i, n: Integer;
  S, S1, S2: string;
  WU: SizeInt;
  qSer, qSt: Integer;
  Idx: TIDBaseStation;
  PP: SizeInt;
begin
  AfficherMessage(self.ClassName, Format('FindBasePointIdx(%s)', [Cle]));
  Result := false;
  S := Trim(Cle);
  // analyse de la clé
  // la clé contient un point --> recherche par série/station
  WU := Pos('.', S);
  if (WU > 0) then
  begin
    N := Length(S);
    S1 := Trim(Copy(S, 0, WU - 1));
    S2 := Trim(Copy(S, WU + 1, N));
    qSer := StrToIntDef(S1, 0);
    qSt  := StrToIntDef(S2, 0);
    Idx  := SetIDStationFromSerSt(qSer, qSt);
    Result := FindBasePointByIndex(Idx, BP);
    // /!\ Les antennes ont un ID négatif.
    if (BP.IDStation <> 0) then Exit;
  end;
  AfficherMessage('-- La clé de recherche est textuelle');
  for i:=0 to GetNbBasePoints - 1  do
  begin
    BP := GetBasePoint(i);
    PP := Pos(S, BP.IDTerrain);
    if (PP > 0) then Exit(True);
  end;
end;
// attraper l'élément point le plus proche d'un point (x, y)
function TCenterLines.GetNearBasePointA(const X, Y: double; const OldBasePoint: TBaseStation; const StLocked: boolean): TBaseStation;
var
  i, n: integer;
  r, Nearest: double;
  EWE: TBaseStation;
begin
  Result := OldBasePoint;  // valeur d'erreur = on ne bouge pas
  if (StLocked) then Exit; // station verrouillée = on sort
  try
    Nearest := INFINITE;
    n := 0;
    for i := 0 to GetNbBasePoints - 1 do
    begin
      EWE := GetBasePoint(i);
      if (Not EWE.Enabled) then Continue;   // zap des visées non affichées
      if (EWE.IDStation < 0) then Continue; // zap des visées en antenne
      r := (X - EWE.PosStation.X) ** 2 + (Y - EWE.PosStation.Y) ** 2;
      if (r < Nearest) then
      begin
        Nearest := r;
        n := i;
      end;
    end;
    Result := GetBasePoint(n);
  except
  end;
end;

//******************************************************************************
// définir station de base d'après les lignes de la section 'basepoints'
function TCenterLines.ExtractBasePointFromLine(const MyLine: string): TBaseStation;
var
  q: integer;
  AP: TGHStringArray;
  BP: TBaseStation;
begin
  AP := Split(MyLine, #9);
  //120400160	L3-16	0	10027212	119.56	215.88	-179.25	121.33	219.20	-180.62	120.44	219.67	-179.25	122.21	218.73	-178.75
  BP.IDStation   := StrToInt64Def(AP[0], -1);
  BP.IDTerrain   := Trim(AP[1]);
  BP.Type_Entite := StrToIntDef(AP[2], 0);
  BP.Couleur     := StrToIntDef(AP[3], 0);

  BP.PosExtr0.X  := ConvertirEnNombreReel(AP[4], 0.00);
  BP.PosExtr0.Y  := ConvertirEnNombreReel(AP[5], 0.00);
  BP.PosExtr0.Z  := ConvertirEnNombreReel(AP[6], 0.00);

  BP.PosStation.X  := ConvertirEnNombreReel(AP[7], 0.00);
  BP.PosStation.Y  := ConvertirEnNombreReel(AP[8], 0.00);
  BP.PosStation.Z  := ConvertirEnNombreReel(AP[9], 0.00);

  BP.PosPG.X   := ConvertirEnNombreReel(AP[10], 0.00);
  BP.PosPG.Y   := ConvertirEnNombreReel(AP[11], 0.00);
  BP.PosPG.Z   := ConvertirEnNombreReel(AP[12], 0.00);

  BP.PosPD.X   := ConvertirEnNombreReel(AP[13], 0.00);
  BP.PosPD.Y   := ConvertirEnNombreReel(AP[14], 0.00);
  BP.PosPD.Z   := ConvertirEnNombreReel(AP[15], 0.00);

  Result := BP;
end;
// trier la table par index
procedure TCenterLines.TrierListeBasePointsParIndex;
begin
  AfficherMessage(Format('%s.TrierListeBasePointsParIndex()', [ClassName]));
  Sort(SortBaseStations);
end;

//******************************************************************************
// Métafiltre - Retourne le nombre de valeurs retenues
function TCenterLines.MetaFiltrer(const MyFiltre: string): integer;
var
  NoFiltre: integer;
  i, m, n : integer;
  NbViseesRetenues: integer;
  LV              : Double;
  DevelVisees     : double;
  f1, st1, st2: string;
  ListeFiltres: array of TFiltre;
  MyBaseStation: TBaseStation;
  function DoDrawVisee(const F:TFiltre; const E:TBaseStation): boolean;
  var
    Long: double;
    s: string;
    d: TDateTime;
    p,q, j : integer;
    h, v: double;
    Ai, Bi : integer; // bornes entières
    Af, Bf : double;  // bornes réelles
    Ad, Bd : TDateTime; // bornes date
    NumeroSerie: integer;
    function MiouDouble(const V1, V2: double): Boolean;
    var
      s666: double;
    begin
      s666 := ConvertirEnNombreReel(F.Valeur, 0.00);
      case F.Operateur of
        1: Result := ((V1 <= S666) or (V2 <= S666));
        2: Result := ((V1  = S666) or (V2  = S666));
        3: Result := ((V1 >= S666) or (V2 >= S666));
      else
        Result:=False;
      end;
    end;
    function MiouInteger(const V1: Integer): Boolean;
    var
      pq: Integer;
    begin
      pq := StrToIntDef(F.Valeur, 0);
      Result := (V1 = pq);
      case F.Operateur of
       1: Result := (V1 <= pq);
       2: Result := (V1  = pq);
       3: Result := (V1 >= pq);
      else
       Result:=False;
      end;
    end;
    function MiouTColor(const V1: TColor): Boolean;
    var
      pq: Integer;
      RR, GG, BB: byte;
      QQ: String;
      CC: TColor;
    begin
      QQ := Trim(F.Valeur);
      // Conversion d'une valeur hexa: préfixer la chaîne d'un dollar
      RR := StrToIntDef('$' + Copy(QQ, 6, 2), 0);
      GG := StrToIntDef('$' + Copy(QQ, 4, 2), 0);
      BB := StrToIntDef('$' + Copy(QQ, 2, 2), 0);
      CC := RGBToColor(RR, GG, BB);
      Result := (V1 = CC);
    end;
    function ExistsPointTopo(const S: string): boolean;
    var
      WU: TGHShortStringArray;
      eSer, eSt: Integer;
      QAT: Int64;
      PP: SizeInt;
    begin
      Result := False;
      PP := Pos('.', S);
      if (PP > 0) then  // clé en ssss.pppp trouvée
      begin
        WU := ShortSplit(S, '.');
        eSer := StrToIntDef(WU[0], -1);
        eSt  := StrToIntDef(WU[1], -1);
        if ((eSer = -1) or (eSt = -1)) then Exit;
        QAT := eSer * MULTIPLICATEUR_SERIE + eSt * MULTIPLICATEUR_STATION;
        Result := (QAT = abs(E.IDStation));
      end
      else
      begin // sinon, recherche sur l'ID terrain
        PP := Pos(LowerCase(Trim(S)), LowerCase(E.IDTerrain));
        Result := (PP > 0);
      end;
    end;
  begin
    Result:=False;
    NumeroSerie := ExtractNumSerieFromTIDBaseStation(E.IDStation); //Abs(E.IDStation) div MULTIPLICATEUR_SERIE; // 100000;
    // -------
    case F.TypeFiltre of
      ftINVALID: Result := False;
      ftSCALAR: begin
        case F.Filter of
          -1       : Result:=False;
           kFLT_NIL: Result:=False;
           kFLT_ALL: Result:=True;
           kFLT_ID : Result:=(Trim(F.Valeur) = Trim(E.IDTerrain));
           kFLT_LONGUEUR  : ; // deprecated: pas utilisé
           kFLT_COULEUR   : Result := MiouTColor(E.Couleur); // deprecated: le rôle des couleurs a changé dans GHTopo
           kFLT_X         : Result := MiouDouble(E.PosExtr0.X, E.PosStation.X);
           kFLT_Y         : Result := MiouDouble(E.PosExtr0.Y, E.PosStation.Y);
           kFLT_Z         : Result := MiouDouble(E.PosExtr0.Z, E.PosStation.Z);
           kFLT_LARGEUR   : ; // deprecated: pas utilisé
           kFLT_HAUTEUR   : ; // deprecated: pas utilisé
           kFLT_SERIE     : Result := MiouInteger(NumeroSerie);
           kFLT_RESEAU    : ; // en attente
           kFLT_SECTEUR   : ; // en attente
           kFLT_CODE      : ; // deprecated: pas utilisé
           kFLT_EXPE      : ; // deprecated: pas utilisé
           kFLT_TYPEVISEE : ; // deprecated: pas utilisé
           kFLT_STATION   : Result := ExistsPointTopo(F.Valeur);
        else
          Result:=False;
        end; // case Filter ..
        //-----------------
      end; // ftScalar
    ftINTERVAL: begin
        case F.Filter of
          -1: Result:=False;
           kFLT_NIL     : ;
           kFLT_AZIMUT  : ; // deprecated: pas utilisé
           kFLT_PENTE   : ; // deprecated: pas utilisé
           kFLT_DATE    : ; // deprecated: pas utilisé
           kFLT_COULEUR : ; // rôle des couleurs changé dans GHTopo
           kFLT_X: // X
             begin // intervalle sur Z
               Af := ConvertirEnNombreReel(F.BorneInf, 0.00);
               Bf := ConvertirEnNombreReel(F.BorneSup, 0.00);
               V:=E.PosExtr0.X;
               Result:= ((V>Af) AND (V<Bf));
               V:=E.PosStation.X;
               Result:= Result or ((V>Af) AND (V<Bf));
             end;

           kFLT_Y:   //Y
              begin // intervalle sur Z
               Af := ConvertirEnNombreReel(F.BorneInf, 0.00);
               Bf := ConvertirEnNombreReel(F.BorneSup, 0.00);

               V:=E.PosExtr0.Y;
               Result:= ((V>Af) AND (V<Bf));
               V:=E.PosStation.Y;
               Result:= Result or ((V>Af) AND (V<Bf));
             end;
           kFLT_Z:
             begin // intervalle sur Z
               Af := ConvertirEnNombreReel(F.BorneInf, 0.00);
               Bf := ConvertirEnNombreReel(F.BorneSup, 0.00);

               V:=0.50 * (E.PosExtr0.Z + E.PosStation.Z);
               Result:= ((V>Af) AND (V<Bf));
             end;
           kFLT_SERIE: // filtre sur séries
             begin
               Ai := StrToIntDef(F.BorneInf,0);
               Bi := StrToIntDef(F.BorneSup,0);
               Result:= ((NumeroSerie >= Ai) AND (NumeroSerie <= Bi));
             end;
           kFLT_RESEAU : ; // deprecated: pas utilisé
           kFLT_SECTEUR: ; // deprecated: pas utilisé
           kFLT_CODE  : ; // deprecated: pas utilisé
           kFLT_EXPE  : ; // deprecated: pas utilisé
        else
          Result:=False;
        end; // case F.Filter
      end; // ftINTERVAL
    end;   // case TypeFiltre
  end;
  //------------------------------------
  function ChooseFilter(const s90: string):integer;
  begin
    Result:=IndexOfString(s90, [rsMETAFILTRE_NIL,         // 0
                                rsMETAFILTRE_ALL,         // 1
                                rsMETAFILTRE_ID,          // 2
                                rsMETAFILTRE_LENGTH,      // 3
                                rsMETAFILTRE_AZIMUTH,     // 4
                                rsMETAFILTRE_PENTE,       // 5
                                rsMETAFILTRE_DATE,        // 6
                                rsMETAFILTRE_COLOR,       // 7
                                rsMETAFILTRE_X,           // 8
                                rsMETAFILTRE_Y,           // 9
                                rsMETAFILTRE_Z,           // 10
                                rsMETAFILTRE_LARGEUR,     // 11
                                rsMETAFILTRE_HAUTEUR,     // 12
                                rsMETAFILTRE_DATES,       // 13
                                rsMETAFILTRE_COLORS,      // 14
                                rsMETAFILTRE_SERIE,       // 15
                                rsMETAFILTRE_RESEAU,      // 16
                                rsMETAFILTRE_SECTEUR,     // 17
                                rsMETAFILTRE_CODE,        // 18
                                rsMETAFILTRE_EXPE,        // 19
                                rsMETAFILTRE_TYPEVISEE,   // 20
                                rsMETAFILTRE_STATION
                                ]);
    AfficherMessageErreur(Format('%d: %s', [Result,s90]));
  end;
  //-----------------------
  function SetFiltre(const F44: string): TFiltre;
  var
    P: integer;
    Q: integer;
    F4: string;
    s1,s2: string;
    function GetConnector(var s: string): TConnectorFiltres;
    // Retourne le connecteur avec le filtre précédent
    // et supprime le premier caractère du filtre
    begin
      Result:=ftERROR;
      if (Length(s) = 0) then Exit;
      case S[1] of
        'U': begin  // (U)nion
               Result:=ftOR;
               system.Delete(S,1,1);
             end;
        'N': begin
               Result:=ftAND;
               system.Delete(S,1,1);
             end;   // I(N)tersection
      else
        Result:=ftERROR;
      end;
    end;

  begin
    //AfficherMessage(Format('  SetFiltre(%s)',[F44]));
    F4:=F44;
    Result.Caption:=F4;
    try
      (* rechercher le connecteur avec le filtre précédent *)
      (* si aucun connecteur trouvé, c'est le début de la liste des filtres *)
      case GetConnector(F4) of
        ftOR   : Result.ConnectedWithPrevious:=ftOR;
        ftAND  : Result.ConnectedWithPrevious:=ftAND;
        ftERROR: Result.ConnectedWithPrevious:=ftOR;
      end;
      //*)
      (* rechercher un point d'exclamation              *)
      (* et mettre le flag d'inversion à True si trouvé *)
      (* Le résultat du MétaFiltre sera basculé si True *)
      Q:=Pos('!', F4);
      Result.Basculed:=(q>0);
      if (Result.Basculed) then system.Delete(F4, q, 1);
      if (Pos('ALL', F4) > 0) then
      begin

        Result.Filter:=1;
        Result.Operateur:=0;
        Result.Valeur:='';
        Exit;
      end;
      if (Pos('NIL', F4) > 0) then
      begin
        Result.Filter:=0;
        Result.Operateur:=0;
        Result.Valeur:='';
        Exit;
      end;
      (* rechercher un ensemble de drapeaux
        Format: <Filtre>=[xxxxxxxxxxxxx] avec x = 0 ou 1
      ******************)
      Q:=Pos('[',F4);
      if (Q > 0) then
      begin
        // définir le type de filtre: Drapeaux
        // rechercher le type de filtre
        P:=Pos('=',F4);
        s1:=Trim(UpperCase(Copy(F4, 1, P-1)));
        Result.Filter:=ChooseFilter(s1);
        //AfficherMessage(Format('Type de filtre sur drapeaux: "%s" %d',[s1, Result.Filter]));
        // attraper la valeur du champ Drapeaux
        P:=Pos(']',F4);
        Result.Flags:=Copy(F4, Q+1, P - Q - 1);
        // protection: Compléter par des zéros
        Result.Flags:=Result.Flags + StringOfChar('0', 300);
        Result.TypeFiltre:=ftFLAGS;
        Exit;
      end;
      (* rechercher un spécificateur d'intervalle
      Formats acceptés:
      <Filtre>=(A, B);
      ******************)
      Q:=Pos('(', F4);
      //Q:=Q+Pos('..', F4);
      if (Q > 0) then
      begin
        //rechercher la donnée à filtrer
        P:=Pos('=',F4);
        s1:=Trim(UpperCase(Copy(F4, 1, P-1)));
        Result.Filter:=ChooseFilter(S1);
        // Extraction de la première valeur
        P:=Pos(',', F4);
        if (P = 0) then
        begin
          Result.Filter:=-1;
          Result.Operateur:=0;
          Result.Valeur:='';
          Exit;
        end;
        Result.BorneInf :=Copy(F4, Q+1, P - Q - 1);
        //AfficherMessage('Inférieur: '+Result.BorneInf);
        // Extraction de la deuxième valeur
        Q:=P; // sauvegarder position de la virgule
        //P:=  Pos(']', F4);
        P:=Pos(')', F4); // recherche de la parenthèse fermante
        if (P = 0) then
        begin
          Result.Filter:=-1;
          Result.Operateur:=0;
          Result.Valeur:='';
          Exit;
        end;
        Result.BorneSup:=Copy(F4, Q+1, P - Q - 1);
        // définir le type de filtre comme intervalle et quitter
        Result.TypeFiltre:=ftINTERVAL;
        Exit;
      end;
      // rechercher un signe égal
      P:=  Pos('=',F4);
      P:=P+Pos('<',F4);
      P:=P+Pos('>',F4);
      // pas de signe égal -> filtre erroné
      if (P = 0) then
      begin
        Result.Filter:=-1;
        Result.Operateur:=0;
        Result.Valeur:='';
        Exit;
      end;
      s1:=Trim(Copy(F4, 1, P-1));
      Result.Filter:=ChooseFilter(s1);
      //AfficherMessage(Format('Filter=%d',[Result.Filter]));
      Result.Operateur:=IndexOfString(F4[P],[' ','<','=','>']);
      s2:=Trim(copy(F4, P+1, Length(F4)));
      Result.Valeur:=s2;
      Result.TypeFiltre := ftSCALAR;
    except
      Result.TypeFiltre:=ftINVALID;
      Result.Filter:=-1;
      Result.Operateur:=0;
      Result.Valeur:='';
      Exit;
    end;
  end;
  //--------------------
  procedure RecenserFiltres;
  var
    l: integer;
    S1, S2, s3: string;
    P         : integer;
    function FoundSeparator(const S: string): integer;
    var
      q: integer;
    begin
      Result:=0;
      if (Length(S) = 0) then Exit;
      for q:=1 to Length(S) do
      begin
        if (S[q] in [';', '&', '|']) then
        begin
          Result:=q;
          Exit;
        end;
      end;
    end;
  begin
    //AfficherMessage(Format('  RecenserFiltres %s',[f1]));
    with TStringList.Create do
    begin
      try
        s2:='N'+f1; // connecteur de début
        Duplicates:=dupIgnore;
        Clear;
        repeat
          P:=FoundSeparator(s2);
          s1:=trim(Copy(s2,1,P-1));
          s2:=Trim(Copy(s2,P,Length(s2)));
          if (Length(s2) > 0) then
          begin
           case s2[1] of
            '&'     : s2[1]:='N'; // intersection (AND)
            '|',';' : s2[1]:='U'; // union (OR)
           else
           end;
          end;
          if (Length(S1) > 0) then Add(s1);
        until P=0;
        if (Length(S2) > 0) then Add(s2);
        // controle et définition des filtres
        SetLength(ListeFiltres,0);
        SetLength(ListeFiltres, Count);
        for l := 0 to Count - 1 do ListeFiltres[l] := SetFiltre(Strings[l]);
      finally
        Free;
      end;
    end;
  end;
  procedure ActiverDesactiverTout(const Q: boolean);
  var
    v: integer;
    EWE: TBaseStation;
  begin
    for v:= 0 to GetNbBasePoints - 1 do
    begin
      EWE := GetBasePoint(v);
      EWE.Enabled := Q;
      PutBasepoint(v, EWE);
    end;
  end;
begin
  AfficherMessage('MetaFiltre: '+ MyFiltre);
  f1 := Trim(UpperCase(MyFiltre));
  //AfficherMessage(Format('%s.MetaFiltre("%s")',[ClassName, Filtre]));
  Result := -1;
  ActiverDesactiverTout(False);
  if (Trim(MyFiltre) = '') then
  begin
    ActiverDesactiverTout(True);
    Result:=1;
    Exit;
  end;
  if (f1 = 'NIL') then
  begin
    Result:=1;
    Exit;
  end;
  RecenserFiltres;
  // application des filtres
  NbViseesRetenues := 0;
  DevelVisees      := 0.00;
  for i:= 0 to GetNbBasePoints - 1 do
  begin
    MyBaseStation := GetBasePoint(i);
    MyBaseStation.Enabled := True;
    for NoFiltre := 0 to High(ListeFiltres) do
    begin
      case ListeFiltres[NoFiltre].ConnectedWithPrevious of
        ftERROR, ftOR: MyBaseStation.Enabled := MyBaseStation.Enabled or  DoDrawVisee(ListeFiltres[NoFiltre], MyBaseStation);
        ftAND        : MyBaseStation.Enabled := MyBaseStation.Enabled and DoDrawVisee(ListeFiltres[NoFiltre], MyBaseStation);
      end;
      if (ListeFiltres[NoFiltre].Basculed) then MyBaseStation.Enabled := Not(MyBaseStation.Enabled);
    end;
    if (MyBaseStation.Enabled) then Inc(NbViseesRetenues);
    PutBasepoint(i, MyBaseStation);
  end;
  Result := NbViseesRetenues;
  SetLength(ListeFiltres,0);
end;

end.


