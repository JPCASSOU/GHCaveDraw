unit UnitAnnotations;

interface

uses
  SysUtils, Classes,
  Common, StructuresDonnees,
  Graphics;

type

{ TAnnotationList }

TAnnotationList = class(TList)

  public

    procedure Initialize;
    procedure Finalize;
    function  LoadFromFile(const MyFichierNot: string): integer;
    function  SaveToFile(const AnnFilename: string): integer;

    procedure AddAnnotation(const A: TAnnotation);
    function  GetAnnotation(const N: integer):TAnnotation;
    procedure InsertAnnotation(const A: TAnnotation; const N: integer);

    procedure DeleteAnnotation(const N: integer);
    procedure PutAnnotation(const A: TAnnotation; const N: integer);
    function  FindAnnotation(const S: string): integer;

    function GetNbAnnotations: integer;
end;


implementation

// colonnes du fichier annotations
const
  FA_COL_ID_ANN      = 0;
  // mode de positionnement: 0 = absolu; 1 = accroché à une station
  FA_COL_MODE_POS    = 1;
  FA_COL_BASEPT_SER  = 2;
  FA_COL_BASEPT_ST   = 3;
  // Point de base: centré, justifié, etc ...
  FA_COL_BASEPT_POS  = 4;
  // coordonnées du texte
  FA_COL_BASEPT_X    = 5;                   // position absolue du texte
  FA_COL_BASEPT_Y    = FA_COL_BASEPT_X + 1;
  FA_COL_BASEPT_Z    = FA_COL_BASEPT_X + 2;
  FA_COL_BASEPT_OF_X = FA_COL_BASEPT_Z + 1; // décalage
  FA_COL_BASEPT_OF_Y = FA_COL_BASEPT_Z + 2;
  FA_COL_BASEPT_OF_Z = FA_COL_BASEPT_Z + 3;
  // Caractéristiques du texte: couleur, limite, fonte, taille, caractères
  FA_COL_TXT_COLOR_R = FA_COL_BASEPT_OF_Z + 1;
  FA_COL_TXT_COLOR_G = FA_COL_BASEPT_OF_Z + 2;
  FA_COL_TXT_COLOR_B = FA_COL_BASEPT_OF_Z + 3;
  FA_COL_TXT_MAX_LG  = FA_COL_BASEPT_OF_Z + 4; // longueur max
  FA_COL_TXT_FONT    = FA_COL_BASEPT_OF_Z + 5; // nom de fonte
  FA_COL_TXT_SIZE    = FA_COL_BASEPT_OF_Z + 6; // taille du texte
  FA_COL_TXT_BOLD    = FA_COL_BASEPT_OF_Z + 7; // gras
  FA_COL_TXT_ITALIC  = FA_COL_BASEPT_OF_Z + 8; // italique
  FA_COL_TXT_UNDERLN = FA_COL_BASEPT_OF_Z + 9; // souligné
  // Affiché ?
  FA_COL_TXT_DISPLAY = FA_COL_TXT_UNDERLN + 1;
  // réservé
  FA_COL_TXT_RESERVE = FA_COL_TXT_UNDERLN + 2;
  // contenu du texte
  FA_COL_TXT_CAPTION = FA_COL_TXT_RESERVE + 1; // texte


//**************************************
// Méthodes de la classe TAnnotationList
//**************************************
(*
FichierNOT    : string;
  X             : Double;
  Y             : Double;
  Z             : Double;
  Color         : TColor;
  FontColor     : TColor; // Couleur de la fonte
  Caption       : String;
  FontName      : String;
  FontSize      : Byte;
  Accrochage    : Byte;
  FontBold      : boolean;
  FontItalic    : boolean;
  FontUnderline : boolean;
//*)
procedure TAnnotationList.Initialize;
begin
  AfficherMessage(Format('%s.Initialize',[ClassName]));
  self.Clear;
end;
procedure TAnnotationList.Finalize;
var
  i : integer;
begin
  try
    AfficherMessage(Format('%s.Finalize',[ClassName]));
    if (self.Count > 0) then
    begin
      for i:=0 to self.Count-1 do Dispose(Items[i]);
    end;
  finally
    self.Clear;
  end;
end;

procedure TAnnotationList.AddAnnotation(const A: TAnnotation);
var
  PA: ^TAnnotation;
begin
  New(PA);
  with Self do
  begin
    PA^:=A;
    Add(PA);
  end;
end;

function TAnnotationList.GetAnnotation(const N: integer):TAnnotation;
var
  Q: integer;
  PA: ^TAnnotation;
begin
  Q:=N;
  if Q<0 then Q:=0;
  if Q > self.Count - 1 then Q:= self.Count - 1;
  //New(PA);
  with Self do
  begin
    PA:=Items[Q];
    Result:=PA^;
  end;
end;

procedure TAnnotationList.PutAnnotation(const A: TAnnotation; const N: integer);
var
  PA: ^TAnnotation;
begin
  //New(PA);
  with Self do
  begin
    PA := self.Items[N]; // récupère le pointeur
    PA^ := A;
    Items[N] := PA;
  end;
end;
procedure TAnnotationList.InsertAnnotation(const A: TAnnotation; const N: integer);
var
  Q: integer;
  PA: ^TAnnotation;
begin
  AfficherMessage('InsertAnnotation');
  Q:=N;
  if Q> self.Count - 1 then Q := self.Count - 1;
  if Q<0 then Q:=0;
  New(PA);
  with self do begin
    PA^ := A;
    Insert(Q, PA);
  end;

end;
procedure TAnnotationList.DeleteAnnotation(const N: integer);
begin
  //PA:=self.Items[N];
  if not (self.Items[N]=nil) then
    Dispose(self.Items[N]);
  self.Delete(n);
end;


function TAnnotationList.LoadFromFile(const MyFichierNot: string): integer;
var
  pNOT : TextFile;
  Ligne: string;
  AN   : TAnnotation;
  ps   : integer;
  NoL  : integer;
  r,g,b: Byte;
  s, p : string;
  LA   : TStringArray;

begin
  AfficherMessage(Format('%s.LoadFromFile(%s)',[ClassName, MyFichierNot]));

  Result:=-1;
  with self do
  begin
    if Not FileExists(MyFichierNot) then
    begin

      // si fichier inexistant, ajouter une annotation
      // d'initialisation


      AN.ModePosition := 1;
      AN.BaseRefSer := 1;
      AN.BaseRefSt  := 0;
      AN.PosPtDeBase  := 1;
      AN.X:=10.00;
      AN.Y:=10.00;
      AN.Z:=10.00;
      AN.FontColor:=clBlue;
      AN.FontColor:=clBlack;
      AN.Caption:='Entrée principale';
      AN.FontName:=DEFAULT_FONT_NAME;
      AN.FontSize:=10;
      //AN.Accrochage:=0;
      AN.FontBold:=True;
      AN.FontUnderline:=True;
      AN.FontItalic:=False;
      AN.MaxLength :=-1;
      AN.displayed := True;
      AddAnnotation(AN);

      Result:=-1;
      exit;
    end;
    Clear;
    AssignFile(pNOT, MyFichierNot);
    try
      Reset(pNOT);
      NoL:=0;
      ReadLn(pNOT, Ligne); // ligne de titres
      ReadLn(pNOT, Ligne); // ligne vide
      while Not(Eof(pNOT)) do begin
       try
        Inc(NoL);
        ReadLn(pNOT, Ligne);
        //AfficherMessage(Ligne);
        LA:=Split(Ligne, TAB);
        AN.IDAnnotation    := StrToIntDef(LA[FA_COL_ID_ANN], 0);
        // mode de positionnement: 0 = absolu; 1 = accroché à une station
        AN.ModePosition    := StrToIntDef(LA[FA_COL_MODE_POS],0);
        AN.BaseRefSer      := StrToIntDef(LA[FA_COL_BASEPT_SER],1);
        AN.BaseRefSt       := StrToIntDef(LA[FA_COL_BASEPT_ST],0);
        // Position du texte par rapport au point de base
        AN.PosPtDeBase     := StrToIntDef(LA[FA_COL_BASEPT_POS],0);
        // coordonnées du texte
        AN.X               := ConvertirEnNombreReel(LA[FA_COL_BASEPT_X],0.00);
        AN.Y               := ConvertirEnNombreReel(LA[FA_COL_BASEPT_Y],0.00);
        AN.Z               := ConvertirEnNombreReel(LA[FA_COL_BASEPT_Z],0.00);
        AN.dx              := ConvertirEnNombreReel(LA[FA_COL_BASEPT_OF_X],0.00);
        AN.dy              := ConvertirEnNombreReel(LA[FA_COL_BASEPT_OF_Y],0.00);
        AN.dz              := ConvertirEnNombreReel(LA[FA_COL_BASEPT_OF_Z],0.00);
        // Caractéristiques du texte: couleur, limite, fonte, taille, caractères
        r   :=StrToIntDef(LA[FA_COL_TXT_COLOR_R],0);
        g   :=StrToIntDef(LA[FA_COL_TXT_COLOR_G],0);
        b   :=StrToIntDef(LA[FA_COL_TXT_COLOR_B],0);
        AN.FontColor:=RGB(r,g,b);
        // longueur max
        AN.MaxLength       := StrToIntDef(LA[FA_COL_TXT_MAX_LG], -1);
        // nom de fonte
        p := Trim(LA[FA_COL_TXT_FONT]);
        if p = NAME_FOR_DEFAULT_FONT then P := DEFAULT_FONT_NAME;
        AN.FontName := P;
        // caractères
        AN.FontSize      := StrToIntDef(LA[FA_COL_TXT_SIZE],8);
        AN.FontBold      := (StrToIntDef(LA[FA_COL_TXT_BOLD],0) = 1);
        AN.FontItalic    := (StrToIntDef(LA[FA_COL_TXT_ITALIC],0) = 1);
        AN.FontUnderline := (StrToIntDef(LA[FA_COL_TXT_UNDERLN],0) = 1);
        // texte affiché
        AN.Displayed     := (StrToIntDef(LA[FA_COL_TXT_DISPLAY],0) = 1);
        // champ réservé
        AN.Reserved      := StrToIntDef(LA[FA_COL_TXT_RESERVE], 0);
        // contenu du texte
        AN.Caption       := Trim(LA[FA_COL_TXT_CAPTION]);


        // ajoute l'annotation
        AddAnnotation(AN);
       except
         AfficherMessage(Format('Error in line %d: "%s"',[NoL,Ligne]));
         AfficherMessage('-------------------------------');
         for r:=0 to High(LA) do
           AfficherMessage(Format('LA[%d]="%s"',[r,LA[r]]));

         AfficherMessage('-------------------------------');

       end;
      end;
      Result:=Count;
      //ShowMessage('OK');
      // contrôle
      if self.Count = 0 then Exit;
      AfficherMessage('Liste des annotations');

      for NoL := 0 to self.Count -1 do begin
        AN := GetAnnotation(NoL);
        AfficherMessage(Format('%d - %s',[NoL, AN.Caption]));


      end;
      AfficherMessage('=====================');

    finally
        CloseFile(pNOT);
    end;
  end;
end;
function TAnnotationList.SaveToFile(const AnnFilename: string): integer;
var
  pNOT : TextFile;
  Ligne: string;
  AN   : TAnnotation;
  i    : integer;
  r,g,b: Byte;
begin
  AfficherMessage(Format('%s.SaveToFile(%s)',[ClassName, AnnFilename]));
  Result:=-1;
  with self do
  begin
    if (self.Count = 0) then
    begin
      Result:=-2;
      Exit;
    end;
    //ShowMessage('Sauvegarde '+FichierNOT);
    AssignFile(pNOT, AnnFilename);
    try
      ReWrite(pNOT);
      // titres des colonnes
      WriteLn(pNOT,
              'ID' + TAB +              // FA_COL_ID_ANN      = 0;
              'Position texte' + TAB +  // FA_COL_MODE_POS    = 1;
              'Serie' + TAB +           // FA_COL_BASEPT_SER  = 2;
              'Station' + TAB +         // FA_COL_BASEPT_ST   = 3;
              'Justif.' + TAB +         // FA_COL_BASEPT_POS  = 4; Point de base: centré, justifié, etc ...
              'X' + TAB +               // FA_COL_BASEPT_X    = 5; position absolue du texte
              'Y' + TAB +               // FA_COL_BASEPT_Y    = FA_COL_BASEPT_X + 1;
              'Z' + TAB +               // FA_COL_BASEPT_Z    = FA_COL_BASEPT_X + 2;
              'dx' + TAB +              // FA_COL_BASEPT_OF_X = FA_COL_BASEPT_Z + 1; // décalage
              'dy' + TAB +              // FA_COL_BASEPT_OF_Y = FA_COL_BASEPT_Z + 2;
              'dz' + TAB +              // FA_COL_BASEPT_OF_Z = FA_COL_BASEPT_Z + 3;
              'R' + TAB +               // FA_COL_TXT_COLOR_R = FA_COL_BASEPT_OF_Z + 1; // Caractéristiques du texte: couleur, limite, fonte, taille, caractères
              'G' + TAB +               // FA_COL_TXT_COLOR_G = FA_COL_BASEPT_OF_Z + 2;
              'B' + TAB +               // FA_COL_TXT_COLOR_B = FA_COL_BASEPT_OF_Z + 3;
              'Long. max' + TAB +       // FA_COL_TXT_MAX_LG  = FA_COL_BASEPT_OF_Z + 4; // longueur max
              'Fonte' + TAB +           // FA_COL_TXT_FONT    = FA_COL_BASEPT_OF_Z + 5; // nom de fonte
              'Taille' + TAB +          // FA_COL_TXT_SIZE    = FA_COL_BASEPT_OF_Z + 6; // taille du texte
              'Bold' + TAB +            // FA_COL_TXT_BOLD    = FA_COL_BASEPT_OF_Z + 7; // gras
              'Italic' + TAB +          // FA_COL_TXT_ITALIC  = FA_COL_BASEPT_OF_Z + 8; // italique
              'Underline' + TAB +       // FA_COL_TXT_UNDERLN = FA_COL_BASEPT_OF_Z + 9; // souligné
              'Affiche' + tab +         // Affiché
              'Reserve' + TAB +         // champ réservé
              'Texte'                  // FA_COL_TXT_CAPTION = FA_COL_BASEPT_OF_Z + 10; // texte
              );
      WriteLn(pNOT, '');
      for i:=0 to self.Count -1 do
        begin
            AN:=GetAnnotation(i);
            r:=Red(AN.FontColor);
            g:=Green((AN.FontColor);
            b:=Blue(AN.FontColor);

            //AN.X
            Ligne:=Format('%d' + TAB +              // FA_COL_ID_ANN      = 0;
                          '%d' + TAB +  // FA_COL_MODE_POS    = 1;
                          '%d' + TAB +           // FA_COL_BASEPT_SER  = 2;
                          '%d' + TAB +         // FA_COL_BASEPT_ST   = 3;
                          '%d' + TAB +         // FA_COL_BASEPT_POS  = 4; Point de base: centré, justifié, etc ...
                          '%.2f' + TAB +               // FA_COL_BASEPT_X    = 5; position absolue du texte
                          '%.2f' + TAB +               // FA_COL_BASEPT_Y    = FA_COL_BASEPT_X + 1;
                          '%.2f' + TAB +               // FA_COL_BASEPT_Z    = FA_COL_BASEPT_X + 2;
                          '%.2f' + TAB +              // FA_COL_BASEPT_OF_X = FA_COL_BASEPT_Z + 1; // décalage
                          '%.2f' + TAB +              // FA_COL_BASEPT_OF_Y = FA_COL_BASEPT_Z + 2;
                          '%.2f' + TAB +              // FA_COL_BASEPT_OF_Z = FA_COL_BASEPT_Z + 3;
                          '%d' + TAB +               // FA_COL_TXT_COLOR_R = FA_COL_BASEPT_OF_Z + 1; // Caractéristiques du texte: couleur, limite, fonte, taille, caractères
                          '%d' + TAB +               // FA_COL_TXT_COLOR_G = FA_COL_BASEPT_OF_Z + 2;
                          '%d' + TAB +               // FA_COL_TXT_COLOR_B = FA_COL_BASEPT_OF_Z + 3;
                          '%d' + TAB +       // FA_COL_TXT_MAX_LG  = FA_COL_BASEPT_OF_Z + 4; // longueur max
                          '%s' + TAB +           // FA_COL_TXT_FONT    = FA_COL_BASEPT_OF_Z + 5; // nom de fonte
                          '%d' + TAB +          // FA_COL_TXT_SIZE    = FA_COL_BASEPT_OF_Z + 6; // taille du texte
                          '%d' + TAB +            // FA_COL_TXT_BOLD    = FA_COL_BASEPT_OF_Z + 7; // gras
                          '%d' + TAB +          // FA_COL_TXT_ITALIC  = FA_COL_BASEPT_OF_Z + 8; // italique
                          '%d' + TAB +       // FA_COL_TXT_UNDERLN = FA_COL_BASEPT_OF_Z + 9; // souligné
                          '%d' + TAB +      // affiché
                          '%d' + TAB +      // réservé
                          '%s',                  // FA_COL_TXT_CAPTION = FA_COL_BASEPT_OF_Z + 10; // texte
                   [
                     i,
                     AN.ModePosition, AN.BaseRefSer, AN.BaseRefSt,
                     AN.PosPtDeBase,
                     AN.X, AN.Y, AN.Z,
                     AN.dx, AN.dy, AN.dz,
                     r, g, b,
                     AN.MaxLength,
                     AN.FontName,
                     AN.FontSize,
                     IIF(AN.FontBold   , 1,0),
                     IIF(AN.FontItalic , 1,0),
                     IIF(AN.FontUnderline, 1,0),
                     IIF(AN.Displayed, 1, 0),
                     AN.Reserved,
                     AN.Caption
                   ]);
            WriteLn(pNOT, Ligne);
        end;
      finally
        CloseFile(pNOT);
      end;
   end; // with self
end;

function TAnnotationList.FindAnnotation(const S: string): integer;
var
  AN: TAnnotation;
  i: integer;
  ss: string;
begin
  Result:=-1;
  ss:=Trim(s);
  if (self.Count =0) then Exit;
  for i:=0 to self.Count - 1 do begin
    AN:=self.GetAnnotation(i);
    if (Pos(ss, AN.Caption)>0) then begin
      Result:=i;
      Exit;
    end;
  end;
end;

function TAnnotationList.GetNbAnnotations: integer;
begin
  Result := self.Count - 1;
end;


end.

