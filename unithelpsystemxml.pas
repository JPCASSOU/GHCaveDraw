unit UnitHelpSystemXML;
{$INCLUDE CompilationParameters.inc}
{$ERROR Ce fichier n'a rien a faire ici}
// 24/12/2013: Version sans génération de fichier temporaire
// 25/04/2016: La liste des sections passe aux génériques
interface

uses
  StructuresDonnees,
  Common,
  {$IFDEF USE_GENERIC_TYPES}
  UnitListesSimplesWithGeneriques,
  {$ELSE}
  UnitListesSimplesWithoutGeneriques,
  {$ENDIF}
  SysUtils, Classes, FileUtil,
  DOM, XMLWrite, XMLRead;

// constantes pour la lecture du fichier
const
  BEGINSECTION = '<Section>';
  BEGININDEX   = '<Index>';
  BEGINTOPICS  = '<Topics>';
  BEGINTITLE   = '<Title>';
  BEGINTEXTE   = '<Texte>';


// objet HelpFile
type

{ THelpFileStructure }

 THelpFileStructure = class

  private
    //FListeSections: TListeSections;
    FCurrentIndex: integer;
    FListeDesSections: THelpListeDesSections;
    procedure ViderListeSections();
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSection(const S: THelpFileSection);
    function  GetSection(const I: integer):THelpFileSection;
    function SearchIndexSection(const Topix: string): integer;
    function LoadFromFile(const FichierAide: string): Boolean;

    function GetNbreSections(): integer;
end;


implementation

//-----------------------


//------------------------------------
// Classe THelpFileStructure
procedure THelpFileStructure.AddSection(const S: THelpFileSection);
begin
  FListeDesSections.AddElement(S);
end;
function THelpFileStructure.GetSection(const I: integer):THelpFileSection;
begin
  Result := FListeDesSections.GetElement(i);
end;


//------------------------------------
// Classe THelpFileStructure
constructor THelpFileStructure.Create;
begin
  inherited Create;
  AfficherMessage(Format('%s.Create',[ClassName]));
  FListeDesSections := THelpListeDesSections.Create; //<THelpFileSection>.Create;
  ViderListeSections();
end;
destructor THelpFileStructure.Destroy;
begin
  AfficherMessage(Format('%s.Destroy',[ClassName]));
  try
    ViderListeSections();
  finally
    FListeDesSections.Free;
  end;
  inherited Destroy;
end;
//-----------------------------
function THelpFileStructure.LoadFromFile(const FichierAide: string): Boolean;
var
  MyDocXML     : TXMLDocument;
  HelpFileRoot : TDOMElement;
  QListeDesSections: TDOMNodeList;
  i, j         : Integer;
  QIN          : TDOMNode;
  SD           : string;
  EWE          : TDOMNodeList;
  WU           : TDOMNode;
  iNode        : TDOMNode;
  CurrentNiveau: integer;
  procedure ProcessNode(Node: TDOMNode);
  var
    cNode: TDOMNode;
    s: string;
    k: String;
    kk: Integer;
    MySection: THelpFileSection;
    NAttr: TDOMNode;
  begin
    if Node = nil then Exit; // Stoppe si on a atteint une feuille

    // Ajoute un nœud à l'arbre s'il existe
    s := ''; k := '';
    if (Node.HasAttributes and (Node.Attributes.Length > 0)) then
    begin
      AfficherMessage(Format('Attributs de %s', [Node.NodeName]));
      for kk := 0 to Node.Attributes.Length - 1 do
      begin
        k := Node.Attributes[kk].NodeName;
        s := Node.Attributes[kk].NodeValue;
        AfficherMessage(Format('%d: %s - %s', [CurrentNiveau, k, s]));
      end;
    end;
    // C'est ici que les sections sont ajoutées
    if (Node.NodeName = 'Section') then
    begin
      for kk := 0 to Node.Attributes.Length - 1 do
      begin
        NAttr := Node.Attributes[kk];
        if (NAttr.NodeName = 'Index')  then MySection.Index   := StrToIntDef(NAttr.NodeValue, 0);
        if (NAttr.NodeName = 'Topics') then MySection.Topics  := NAttr.NodeValue;
        if (NAttr.NodeName = 'Title')  then MySection.Title   := NAttr.NodeValue;
      end;
      MySection.Texte := Node.TextContent;
      AfficherMessageErreur('Ajout de ' + MySection.Topics);
      AddSection(MySection);
    end;





    // Va au nœud enfant
    cNode := Node.ChildNodes.Item[0];
    CurrentNiveau := CurrentNiveau + 1;

    // Traite tous les nœuds enfants
    while cNode <> nil do
    begin
      ProcessNoDe(cNode);
      cNode := cNode.NextSibling;
    end;
    CurrentNiveau := CurrentNiveau - 1;
  end;
begin
  Result := false;
  AfficherMessage(Format('%s.LoadFromFile(%s)', [ClassName, FichierAide]));
  ViderListeSections();
  MyDocXML := TXMLDocument.Create;
  try
    try
      ReadXMLFile(MyDocXML, FichierAide);
      HelpFileRoot := MyDocXML.DocumentElement;
      // AfficherMessage(Format('%s: %s', [{$I %FILE%}, {$I %LINE%}]));
      AfficherMessage('-- ' + HelpFileRoot.TagName);
      // lecture des sections
      CurrentNiveau := 0;
      iNode := MyDocXML.DocumentElement.ChildNodes.Item[0];
      while iNode <> nil do
      begin
        ProcessNode(iNode); // Procédure récursive
        iNode := iNode.NextSibling;
      end;
      Result := True;
    except

    end;
  finally
    MyDocXML.Free;
  end;
end;

function THelpFileStructure.GetNbreSections: integer;
begin
  Result := FListeDesSections.GetNbElements();
end;

function THelpFileStructure.SearchIndexSection(const Topix: string): integer;
var
  i: integer;
  SS: THelpFileSection;
begin
  Result := 0;
  for i := 0 to FListeDesSections.GetNbElements() - 1 do
  begin
    SS := self.GetSection(i);
    if (lowercase(SS.Topics) = lowercase(Topix)) then
    begin
      Result := i;
      exit;
    end;
  end;
end;

procedure THelpFileStructure.ViderListeSections;
begin
  FListeDesSections.ClearListe();
end;

end.

