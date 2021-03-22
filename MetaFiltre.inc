(* Fichier Metafiltre.inc: MétaFiltre *)
{$NOTE Metafiltre.inc: Contient le code du MétaFiltre}
(**** NE PAS MODIFIER ****)
// NE PAS MODIFIER
function TBDDEntites.MetaFiltre(const Filtre: string; out DevelVisees: double): integer;
type TTableAUtiliser = (teuVISEES, teuANTENNES);
var
  f1: string;
  ListeFiltres: array of TFiltre;
  // La fonction ChooseMetaFiltre est déplacée dans Common.pas
  function DoDrawVisee(const F: TFiltre; const E: TBaseStation): boolean;
  var
    Long: double;
    s: string;
    d: TDateTime;
    p, q, j: integer;
    h, v: double;
    Ai, Bi: integer; // bornes entières
    Af, Bf: double;  // bornes réelles
    Ad, Bd: TDateTime; // bornes date
    Q666: TTypeDeVisee;
    EWE: TGHStringArray;
    QStartPt, QNumSerie, QEndPoint: LongInt;
  begin
    Result := False;
    // -------
    case F.TypeFiltre of
      ftINVALID: Result := False;
      ftSCALAR :
      begin
        case F.Filter of
          -1      : Result := False;
          kFLT_NIL: Result := False;
          kFLT_ALL: Result := True;
          kFLT_ID : Result := (Trim(F.Valeur) = Trim(E.IDTerrain));
          kFLT_LONGUEUR:
          begin // longueur des visées
            Long := Hypot3D(E.PosStation.X - E.PosExtr0.X,
                            E.PosStation.Y - E.PosExtr0.Y,
                            E.PosStation.Z - E.PosExtr0.Z);
            case F.Operateur of
              1: Result := (Long <= ConvertirEnNombreReel(F.Valeur, 0.00));
              3: Result := (Long >= ConvertirEnNombreReel(F.Valeur, 0.00));
            otherwise
                Result := False;
            end;
          end;
          kFLT_DATE:
          begin // filtres sur dates - DONE: Les resourcestrings du MetaFiltre doivent être en majuscules
            d := Trunc(StrToDate(F.Valeur));
            case F.Operateur of
              1: Result := (Trunc(E.DateLeve) <= d);
              2: Result := (Trunc(E.DateLeve)  = d);
              3: Result := (Trunc(E.DateLeve) >= d);
            otherwise
              Result := False;
            end;
          end;
          kFLT_COULEUR:  // TODO: Modifications à faire; inertée
          begin
            (*
            p := StrToIntDef(F.Valeur, 0);
            case F.Operateur of
              1: Result := (E.ColorEntite <= p);
              2: Result := (E.ColorEntite = p);
              3: Result := (E.ColorEntite >= p);
              else
                Result := False;
            end;
            //*)
          end;
          kFLT_X:
          begin
            d := ConvertirEnNombreReel(F.Valeur, 0.00);
            case F.Operateur of
              1: Result := ((E.PosExtr0.X <= d) or (E.PosStation.X <= d));
              2: Result := ((E.PosExtr0.X  = d) or (E.PosStation.Y  = d));
              3: Result := ((E.PosExtr0.X >= d) or (E.PosStation.Y >= d));
            otherwise
              Result := False;
            end;
          end;
          kFLT_Y:
          begin
            d := ConvertirEnNombreReel(F.Valeur, 0.00);
            case F.Operateur of
              1: Result := ((E.PosExtr0.Y <= d) or (E.PosStation.Y <= d));
              2: Result := ((E.PosExtr0.Y  = d) or (E.PosStation.Y  = d));
              3: Result := ((E.PosExtr0.Y >= d) or (E.PosStation.Y >= d));
            otherwise
              Result := False;
            end;
          end;
          kFLT_Z:
          begin // filtres sur profondeur
            d := ConvertirEnNombreReel(F.Valeur, 0.00);
            case F.Operateur of
              1: Result := ((E.PosExtr0.Z <= d) or (E.PosStation.Z <= d));
              2: Result := ((E.PosExtr0.Z  = d) or (E.PosStation.Z  = d));
              3: Result := ((E.PosExtr0.Z >= d) or (E.PosStation.Z >= d));
            otherwise
              Result := False;
            end;
          end;
          kFLT_LARGEUR: // filtres sur largeur
          begin
            d := ConvertirEnNombreReel(F.Valeur, 0.00);
            H := Hypot2D(E.PosOPD.X - E.PosOPG.X, E.PosOPD.Y - E.PosOPG.Y);
            V := Hypot2D(E.PosPD.X - E.PosPG.X, E.PosPD.Y - E.PosPG.Y);
            case F.Operateur of
              1: Result := ((V <= d) or (H <= d));
              2: Result := ((H = d) or (V = d));
              3: Result := ((H >= d) or (V >= d));
            otherwise
              Result := False;
            end;
          end;
          kFLT_HAUTEUR: // filtres sur hauteurs
          // TODO: Implanter ce filtre HAUTEURS
          begin
            d := ConvertirEnNombreReel(F.Valeur, 0.00);
            H := E.PosOPD.Z - E.PosOPG.Z;
            V := E.PosPD.Z - E.PosPG.Z;  //Z2PH - E.Z2PB;
            case F.Operateur of
              1: Result := ((V <= d) or (H <= d));
              2: Result := ((H = d) or (V = d));
              3: Result := ((H >= d) or (V >= d));
            otherwise
                Result := False;
            end;
          end;
          kFLT_SERIE: // filtre sur séries
          begin
            // TODO: Adapter ce code à GHCaveDraw
            //Le filtre sur séries peut avoir le format suivant:
            // a: SERIE = 122
            // b: SERIE = 122:23-34
            // d: SERIE = 122:12-

            p := StrToIntDef(F.Valeur, -1);
            if (p > 0) then
            begin
              case F.Operateur of
                1: Result := (abs(E.Entite_Serie) <= p);
                2: Result := (abs(E.Entite_Serie) = p);
                3: Result := (abs(E.Entite_Serie) >= p);
              otherwise
                result := false;
              end;
            end
            else
            begin
              EWE := ExtractStringsToTStringArray([':', '-'], [' '], F.Valeur);
              QNumSerie := StrToIntDef(EWE[0], 1);
              QStartPt  := StrToIntDef(EWE[1], 1);
              QEndPoint := StrToIntDef(EWE[2], 666);
              //AfficherMessageErreur(Format('%s: %s, %s, %s', [F.Valeur, EWE[0], EWE[1], EWE[2]]));
              Result    := (E.Entite_Serie = QNumSerie) AND
                           (IsInRange(E.Entite_Station, QStartPt, QEndPoint));
            end;
          end;
          kFLT_RESEAU: // filtre sur réseaux
          begin
            p := StrToIntDef(F.Valeur, 0);
            case F.Operateur of
              1: Result := (E.eReseau <= p);
              2: Result := (E.eReseau = p);
              3: Result := (E.eReseau >= p);
            otherwise
              Result := False;
            end;
          end;
          kFLT_SECTEUR: // filtre sur secteur
          begin
            p := StrToIntDef(F.Valeur, 0);
            case F.Operateur of
              1: Result := (E.eSecteur <= p);
              2: Result := (E.eSecteur = p);
              3: Result := (E.eSecteur >= p);
            otherwise
              Result := False;
            end;
          end;
          kFLT_CODE: // filtre sur codes
          begin
            p := StrToIntDef(F.Valeur, 0);
            case F.Operateur of
              1: Result := (E.eCode <= p);
              2: Result := (E.eCode = p);
              3: Result := (E.eCode >= p);
            otherwise
              Result := False;
            end;
          end;
          kFLT_EXPE: // filtre sur expés
          begin
            p := StrToIntDef(F.Valeur, 0);
            case F.Operateur of
              1: Result := (E.eExpe <= p);
              2: Result := (E.eExpe = p);
              3: Result := (E.eExpe >= p);
            otherwise
              Result := False;
            end;
          end;
          kFLT_TYPEVISEE: // filtre sur type de visée
          begin
            p := StrToIntDef(F.Valeur, 0);
            Q666 := GetTypeDeVisee(p);
            Result := (E.Type_Entite = Q666);
          end;
          kFLT_ENTRANCE_RATT:
          begin
            p := StrToIntDef(F.Valeur, 0);
            case F.Operateur of
              1: Result := (E.eEntrance <= p);
              2: Result := (E.eEntrance = p);
              3: Result := (E.eEntrance >= p);
            otherwise
              Result := False;
            end;
          end;
        otherwise
          Result := False;
        end; // case Filter ..
      end; // ftScalar
      ftINTERVAL:
      begin
        case F.Filter of
          -1: Result := False;
          kFLT_NIL: ;
          kFLT_AZIMUT:
          begin // intervalle sur les azimuts
            Af := ConvertirEnNombreReel(F.BorneInf, 0.00);
            Bf := ConvertirEnNombreReel(F.BorneSup, 0.00);
            V := GetAzimut(E.PosStation.X - E.PosExtr0.X,
                           E.PosStation.Y - E.PosExtr0.Y,
                           360.00);
            Result := ((V > Af) and (V < Bf)) or (((V + 180) > Af) and ((V + 180) < Bf));
          end;
          kFLT_PENTE:
          begin // intervalle sur les inclinaisons
            Af := ConvertirEnNombreReel(F.BorneInf, 0.00);
            Bf := ConvertirEnNombreReel(F.BorneSup, 0.00);
            Long := 1e-12 + Hypot3D(
              E.PosStation.X - E.PosExtr0.X,
              E.PosStation.Y - E.PosExtr0.Y,
              E.PosStation.Z - E.PosExtr0.Z);
            H := E.PosStation.Z - E.PosExtr0.Z;
            V := ArcSin(H / Long) * INVDEGREES;
            Result := ((V > Af) and (V < Bf));
          end;
          kFLT_DATE:
          begin // intervalle sur dates
            try
              Ad := StrToDate(F.BorneInf);
              Bd := StrToDate(F.BorneSup);
              Result := ((E.DateLeve >= Ad) and (E.DateLeve <= Bd));
            except
              Result := False;
            end;
          end;
          kFLT_COULEUR: ; // couleurs
          kFLT_X: // X
          begin
            Af := ConvertirEnNombreReel(F.BorneInf, 0.00);
            Bf := ConvertirEnNombreReel(F.BorneSup, 0.00);
            V := E.PosExtr0.X;
            Result := ((V > Af) and (V < Bf));
            V := E.PosStation.X;
            Result := Result or ((V > Af) and (V < Bf));
          end;

          kFLT_Y:
          begin
            Af := ConvertirEnNombreReel(F.BorneInf, 0.00);
            Bf := ConvertirEnNombreReel(F.BorneSup, 0.00);
            V := E.PosExtr0.Y;
            Result := ((V > Af) and (V < Bf));
            V := E.PosStation.Y;
            Result := Result or ((V > Af) and (V < Bf));
          end;
          kFLT_Z:
          begin // intervalle sur Z
            Af := ConvertirEnNombreReel(F.BorneInf, 0.00);
            Bf := ConvertirEnNombreReel(F.BorneSup, 0.00);
            V := 0.50 * (E.PosExtr0.Z + E.PosStation.Z);
            Result := ((V > Af) and (V < Bf));
          end;
          kFLT_SERIE: // filtre sur séries
          begin
            Ai := abs(StrToIntDef(F.BorneInf, 0));
            Bi := abs(StrToIntDef(F.BorneSup, 0));
            Result := ((E.Entite_Serie >= Ai) and (E.Entite_Serie <= Bi));
          end;
          kFLT_RESEAU: // filtres sur réseaux
          begin
            Ai := StrToIntDef(F.BorneInf, 0);
            Bi := StrToIntDef(F.BorneSup, 0);
            Result := ((E.eReseau >= Ai) and (E.eReseau <= Bi));
          end;
          kFLT_ENTRANCE_RATT: // filtres sur entrées de rattachement
          begin
            Ai := StrToIntDef(F.BorneInf, 0);
            Bi := StrToIntDef(F.BorneSup, 0);
            Result := ((E.eEntrance >= Ai) and (E.eEntrance <= Bi));
          end;
          kFLT_SECTEUR: // filtres sur secteur
          begin
            Ai := StrToIntDef(F.BorneInf, 0);
            Bi := StrToIntDef(F.BorneSup, 0);
            Result := ((E.eSecteur >= Ai) and (E.eSecteur <= Bi));
          end;
          kFLT_CODE: // filtres sur codes
          begin
            Ai := StrToIntDef(F.BorneInf, 0);
            Bi := StrToIntDef(F.BorneSup, 0);
            Result := ((E.eCode >= Ai) and (E.eCode <= Bi));
          end;
          kFLT_EXPE: // filtres sur expés
          begin
            Ai := StrToIntDef(F.BorneInf, 0);
            Bi := StrToIntDef(F.BorneSup, 0);
            Result := ((E.eExpe >= Ai) and (E.eExpe <= Bi));
          end;
        otherwise
          Result := False;
        end; // case F.Filter
      end; // ftINTERVAL
    end;   // case TypeFiltre
  end;

  function SetFiltre(const F44: string): TFiltre;
  var
    P: integer;
    Q: integer;
    F4: string;
    s1, s2: string;

    function GetConnector(var s: string): TConnectorFiltres;
      // Retourne le connecteur avec le filtre précédent
      // et supprime le premier caractère du filtre
    begin
      if (Length(s) = 0) then
      begin
        Result := ftERROR;
        Exit;
      end;
      case S[1] of
        'U':
        begin  // (U)nion
          Result := ftOR;
          system.Delete(S, 1, 1);
        end;
        'N':
        begin
          Result := ftAND;
          system.Delete(S, 1, 1);
        end;   // I(N)tersection
      else
          Result := ftERROR;
      end;
    end;
  begin
    F4 := F44;
    Result.Caption := F4;
    try
      (* rechercher le connecteur avec le filtre précédent *)
      (* si aucun connecteur trouvé, c'est le début de la liste des filtres *)
      case GetConnector(F4) of
        ftOR   : Result.ConnectedWithPrevious := ftOR;
        ftAND  : Result.ConnectedWithPrevious := ftAND;
        ftERROR: Result.ConnectedWithPrevious := ftOR;
      end;
      (* rechercher un point d'exclamation              *)
      (* et mettre le flag d'inversion à True si trouvé *)
      (* Le résultat du MétaFiltre sera basculé si True *)
      Q := Pos('!', F4);
      Result.Basculed := (q > 0);
      if (Result.Basculed) then
      begin
        System.Delete(F4, q, 1);
      end;
      if (Pos('ALL', F4) > 0) then
      begin
        Result.Filter := 1;
        Result.Operateur := 0;
        Result.Valeur := '';
        Exit;
      end;
      if (Pos('NIL', F4) > 0) then
      begin
        Result.Filter := 0;
        Result.Operateur := 0;
        Result.Valeur := '';
        Exit;
      end;

      (* rechercher un spécificateur d'intervalle
      Formats acceptés:
      <Filtre>=(A, B);
      ******************)
      Q := Pos('(', F4);
      if (Q > 0) then
      begin
        //rechercher la donnée à filtrer
        P := Pos('=', F4);
        s1 := Trim(UpperCase(Copy(F4, 1, P - 1)));
        Result.Filter := ChooseFilter(S1);
        // Extraction de la première valeur
        P := Pos(',', F4);
        if P = 0 then
        begin
          Result.Filter := -1;
          Result.Operateur := 0;
          Result.Valeur := '';
          Exit;
        end;
        Result.BorneInf := Copy(F4, Q + 1, P - Q - 1);
        //AfficherMessage('Inférieur: '+Result.BorneInf);
        // Extraction de la deuxième valeur
        Q := P; // sauvegarder position de la virgule
        P := Pos(')', F4); // recherche de la parenthèse fermante
        if P = 0 then
        begin
          Result.Filter := -1;
          Result.Operateur := 0;
          Result.Valeur := '';
          Exit;
        end;
        Result.BorneSup := Copy(F4, Q + 1, P - Q - 1);
        // définir le type de filtre comme intervalle et quitter
        Result.TypeFiltre := ftINTERVAL;
        Exit;
      end;
      // rechercher un signe égal
      P := Pos('=', F4);
      P := P + Pos('<', F4);
      P := P + Pos('>', F4);
      // pas de signe égal -> filtre erroné
      if (P = 0) then
      begin
        Result.Filter := -1;
        Result.Operateur := 0;
        Result.Valeur := '';
        Exit;
      end;
      s1 := Trim(Copy(F4, 1, P - 1));
      Result.Filter := ChooseFilter(s1);
      //AfficherMessage(Format('Filter=%d',[Result.Filter]));
      Result.Operateur := IndexOfString(F4[P], False, [' ', '<', '=', '>']);
      s2 := Trim(copy(F4, P + 1, Length(F4)));
      Result.Valeur := s2;
      Result.TypeFiltre := ftSCALAR;
    except
      Result.TypeFiltre := ftINVALID;
      Result.Filter := -1;
      Result.Operateur := 0;
      Result.Valeur := '';
      Exit;
    end;
  end;
  procedure RecenserFiltres;
  var
    l: integer;
    S1, S2, s3: string;
    P: integer;

    function FoundSeparator(const S: string): integer;
    var
      q: integer;
    begin
      Result := 0;
      if (Length(S) = 0) then Exit;
      for q := 1 to Length(S) do
      begin
        if (S[q] in [';', '&', '|', '+', '*']) then
        begin
          Result := q;
          Exit;
        end;
      end;
    end;
  begin
    //AfficherMessage(Format('  RecenserFiltres %s',[f1]));
    with TStringList.Create do
    begin
      try
        s2 := 'N' + f1; // connecteur de début
        Duplicates := dupIgnore;
        Clear;
        repeat
          P := FoundSeparator(s2);
          s1 := trim(Copy(s2, 1, P - 1));
          //s2:=Trim(Copy(s2,P+1,Length(s2)));
          s2 := Trim(Copy(s2, P, Length(s2)));
          if (Length(s2) > 0) then
          begin
            case s2[1] of
              '&', '*'      : s2[1] := 'N'; // intersection (AND) (produit)
              '|', ';', '+' : s2[1] := 'U'; // union (OR)         (somme)
            else
            end;
          end;
          if (Length(S1) > 0) then
            Add(s1);
        until P = 0;
        if (Length(S2) > 0) then Add(s2);
        // controle et définition des filtres
        SetLength(ListeFiltres, 0);
        SetLength(ListeFiltres, Count);
        for l := 0 to Count - 1 do ListeFiltres[l] := SetFiltre(Strings[l]);
      finally
        Free;
      end;
    end;
  end;

  procedure SelectAll(const WU: boolean);
  var
    v, qn: integer;
  begin
    FNbreViseesRetenues := 0;
    qn := GetNbEntitesVisees();
    if (qn > 0) then
    begin
      for v := LOW_INDEX to qn - 1 do SetEntiteViseeVisible(v, WU);
      FNbreViseesRetenues := IIF(WU, qn, 0);
    end;
    qn := GetNbEntitesAntennes();
    if (qn > 0) then
    begin
      for v := LOW_INDEX to qn - 1 do  SetEntiteAntenneVisible(v, WU);
    end;
 end;
  procedure Cuicui(const QMode: TTableAUtiliser);
  var
    Nb, i, NoFiltre: Integer;
    Q666: TBaseStation;
    LV: Double;
  begin
    FNbreViseesRetenues := 0;
    DevelVisees := 0.00;
    case QMode of
      teuVISEES  : Nb := GetNbEntitesVisees();
      teuANTENNES: Nb := GetNbEntitesAntennes();
    end;
    if (Nb = 0) then Exit;
    for i := LOW_INDEX to Nb - 1 do
    begin
      case QMode of
        teuVISEES   :
          begin
            Q666 := GetEntiteVisee(i);
            Q666.Enabled  := True;
            SetEntiteViseeVisible(i, True);
          end;
        teuANTENNES :
          begin
            Q666 := GetEntiteAntenne(i);
            Q666.Enabled  := True;
            SetEntiteAntenneVisible(i, True);
          end;
      end;
      for NoFiltre := 0 to High(ListeFiltres) do
      begin
        case ListeFiltres[NoFiltre].ConnectedWithPrevious of
          ftERROR, ftOR: Q666.Enabled := Q666.Enabled OR  DoDrawVisee(ListeFiltres[NoFiltre], Q666);
          ftAND        : Q666.Enabled := Q666.Enabled and DoDrawVisee(ListeFiltres[NoFiltre], Q666);
        end;
        if (ListeFiltres[NoFiltre].Basculed) then Q666.Enabled := not (Q666.Enabled);
        case QMode of
          teuVISEES    : PutEntiteVisee(i, Q666);
          teuANTENNES  : PutEntiteAntenne(i, Q666);
        end;
      end;
      // uniquement pour le mode Visees
      if ((QMode = teuVISEES) and (Q666.Enabled)) then
      begin
        FNbreViseesRetenues += 1;
        LV := Hypot3D(Q666.PosStation.X - Q666.PosExtr0.X, Q666.PosStation.Y - Q666.PosExtr0.Y, Q666.PosStation.Z - Q666.PosExtr0.Z);
        DevelVisees := DevelVisees + LV;
      end;
    end;
  end;

begin
  f1 := Trim(UpperCase(Filtre));
  AfficherMessage(Format('%s.MetaFiltre("%s")', [ClassName, Filtre]));
  Result := -1;
  if ((f1 = '') or (f1 = 'ALL') or (f1 = 'TOUT')) then
  begin
    SelectAll(True);
    Exit(0);
  end;
  try
    RecenserFiltres;
    SelectAll(False);
    // application des filtres
    Cuicui(teuVISEES);
    Cuicui(teuANTENNES);
    AfficherMessage(Format('%s.MetaFiltre("%s"): %d items, %.2f m', [ClassName, Filtre, FNbreViseesRetenues, DevelVisees]));
    Result := FNbreViseesRetenues;
    SetLength(ListeFiltres, 0);
  except
    Result := -1;
  end;
end;

