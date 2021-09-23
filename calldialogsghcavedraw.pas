unit CallDialogsGHCaveDraw;
{$INCLUDE CompilationParameters.inc}
interface
uses
  {$IFDEF LANGUAGE_FRENCH}
    UnitMessages_fr,
  {$ENDIF}
  GHCD_Types,
  GeneralFunctions,
  ConvertisseurJPC,
  UnitDocDessin,
  unitGenererFichePointTopo,
  SysUtils, Classes, Types, Graphics,
  Controls,
  Forms,
  stdctrls,
  ExtCtrls,
  PopupNotifier,
  Dialogs,
  extdlgs,
  Printers,      //, OSPrinters, // OSPrinters est INDISPENSABLE ici !!
  PrintersDlgs,
  Printer4Lazarus,
  FileUtil;
  // Question Oui-Non
  function QuestionOuiNon(const Msg: string): boolean;
  // choix de fichiers
  // choix d'une photo
  function ChoosePhotoFileName(var PhotoFileName: string): boolean;

  function DoDialogOpenFilename(const Filters: string; var QFileName: TStringDirectoryFileName): boolean;
  function DoDialogSaveFilename(const QFilters, QDefaultExt: string; var QFileName: TStringDirectoryFileName): boolean;
  function DoDialogSelectDossier(const QInitialDir: TStringDirectoryFileName; var QDirectorySelected: TStringDirectoryFileName): boolean;
  // paramétrage de la grille de dessin
  function ParametrerVue2D(var ParamsVue2D: TParamsVue2D): boolean;
  // choix d'une couleur
  function ChooseColor(const OldColor: TColor): TColor;

  // édition d'un objet symbole
  procedure  EditerSymboleObject(const MyDocuDessin: TDocumentDessin;
                                 const CurrentIdxSymbole: integer;
                                 const ModeEdition: TModeEdition;
                                 const IdxGroupeCourant:  TIDGroupeEntites;
                                 const TypeDeSymbole   : TNatureObjetSymbole;
                                 const OP: TSymbole;
                                 const AbsolutePos: TPoint3Df;
                                 const ProcRepercuter: TProcRefreshVues);

  // édition d'un objet texte
  procedure  EditerTextObject(const MyDocuDessin    : TDocumentDessin;
                              const CurrentIdxTexte : integer;
                              const ModeEdition     : TModeEdition;
                              const IdxGroupeCourant: TIDGroupeEntites;
                              const TypeDeTexte     : TNatureObjetTexte;
                              var   OT              : TTextObject;
                              const AbsolutePos     : TPoint3Df;
                              const ProcRepercute   : TProcRefreshVues);
  // édition d'un scrap
  function EditerScrap(const MyDocDessin:TDocumentDessin;
                       var   MyScrap: TScrap): boolean;


  // dialogue d'édition de groupe
  function EditerGroupe(var MyGroupe: TGroupeEntites; const ME: TModeEdition; const QFiltres: string; out DoSetThisGroupeAsCurrent: boolean): boolean;

  // dialogue d'édition de style d'objets
  function EditerStylesObjets(const MyDocDessin: TDocumentDessin; const PF: TProcRefreshStyles): boolean;

  // garnir une combobox avec les types de symboles
  procedure RemplirComboTypesSymboles(const CB: TComboBox; const Idx: integer);
  procedure RemplirComboTypesCourbes(const CB: TComboBox; const Idx: integer);
  procedure RemplirComboTypesSimplesLignes(const CB: TComboBox; const Idx: integer);
  procedure RemplirComboTypesPolygones(const CB: TComboBox; const Idx: integer);
  procedure RemplirComboTypesTextes(const CB: TComboBox; const Idx: integer);

  // boite A propos
  procedure DisplayHelpSystem(const MyDocDessin:TDocumentDessin;
                              const Topics: string;
                              const ReadOnly: boolean = false);
  // hiérarchie des objets (inutilisé)
  procedure DisplayHierarchieObjets(const MyDocDessin:TDocumentDessin);
  // supergroupe
  procedure DisplaySuperGroupes(const MyDocDessin: TDocumentDessin; var Idx: TIDSuperGroupe; out QTodo: byte);
  // objets d'un groupe
  procedure DisplayObjetsOfThisGroupe(const MyDocDessin:TDocumentDessin; const QIdx: TIDGroupeEntites);
  // saisie d'une valeur numérique isolée
  function  InputNumValue(const ACaption, APrompt: string; const ValMini, ValMax: double; var Value: double): boolean;
  // insertion / modification d'une image TImageObject
  function DoDialogTImageObject(const FD: TDocumentDessin;
                                var EWE: TImageObject;
                                const QInitialDir: string;
                                const ImgPosition: TPoint3Df;
                                const DoCreateImage: boolean): boolean;
  // éditeur de texte
  procedure DisplayEditeurTexte(const QText: TStringList; const QTitreDlg: string; const QM: TModeSelectionEntites); overload;

  // notifier un message d'erreur et afficher la console
  procedure NotifyError(const Msg: string);
  function  EditerSectionTransversale(const FD: TDocumentDessin; const BP: TBaseStation; const QOffsetX, QOffsetY: double): boolean;
  procedure DisplayDlgExportToTherion(const FD: TDocumentDessin);
  procedure DisplayDlgExportGIS(const FD: TDocumentDessin);

  function  SelectIndexGroupe(const FD: TDocumentDessin; const IdxGroupe: TIDGroupeEntites; out IdxSelected: TIDGroupeEntites): boolean;
  // création d'une fiche point topo avec un extrait de plan
  function  CreateFichePointTopo(const FD: TDocumentDessin;
                                 const QTextures: TTexturesPolygonObject;
                                 const Station: TBaseStation;
                                 const ParamsVueD: TParamsVue2D): boolean;

  // centre d'impression
  function DisplayPrintingCenter(const FD: TDocumentDessin; const TP: TTexturesPolygonObject; const FP: TParamsVue2D): boolean;
  function SelectionnerImprimante(var MyPrinter: TPrinter): boolean;
  // liste des points d'intérêt
  function DisplayPOI(const FD: TDocumentDessin): boolean;

  // sélection d'un système de coordonnées
  function SelectCoordinatesSystem(const FC: TConversionSysteme; out SC: TLabelSystemesCoordsEPSG): boolean;
  // choix d'une couleur AutoCAD
  function SelectCouleurAutocad(const OldIdx: integer): integer;
  // bac à sable
  function DisplaySandbox(const FD: TDocumentDessin): boolean;
  // toast
  procedure DisplayToast(AOwner: TWinControl; const Msg: string; const DureeInMS: integer = 1500);
implementation
uses
  DGCDummyUnit
 ,frmGroupes               // pour dialogue Groupe;
 ,frmJournal               // console
 ,frmTextObject            // pour EditerTextObject()
 ,frmSymbolObject          // pour EditerSymbolObject()
 ,dlgHelpSystem            // pour le système d'aide
 ,frmEditStylesObjet       // pour EditerStylesObjets()
 ,frmEditScraps            // pour EditerScrap()
 //,frmHierarchieObjets      // pour la hiérarchie d'objets (inutilisé)
 ,frmTImageObject          // pour DoDialogTImageObject()
 ,frmEditeurTexte          // pour l'éditeur de texte intégré
 ,dlgEditeurSectionTransversale // éditeur de sections
 ,frmExportTherion         // pour export vers Therion
 ,frmExportToSIG           // pour export GIS
 ,frmListeGroupes          // pour liste des groupes
 ,CentreImpressionExt      // pour centre d'impression
 ,frmParametrerVue2D       // pour les préférences TParamsVue2D
 ,frmSelectCoordsSystem
 ,frmListePOI              // pour la liste des POI
 ,frmObjetsOfGroupe        // pour les objets du groupe
 ,frmDisplaySuperGroupes   // gestion des super-groupes
 ,dlgAutocadColor          // palette AutoCAD
 ,frmhierarchieobjets
 ,frmSandbox               // bac à sable
 ;

procedure DisplayHierarchieObjets(const MyDocDessin:TDocumentDessin);
var
  DLG: TdlgHierarchieDesObjets;
begin
  DLG := TdlgHierarchieDesObjets.Create(Application);
  try
    if (DLG.Initialiser(MyDocDessin)) then DLG.ShowModal;
  finally
    DLG.Release;
  end;
end;
//*)

procedure DisplaySuperGroupes(const MyDocDessin: TDocumentDessin; var Idx: TIDSuperGroupe; out QTodo: byte);
var
  DLG: TdlgDisplaySuperGroupes;
begin
  QTodo := 0;
  DLG := TdlgDisplaySuperGroupes.Create(Application);
  try
    if (DLG.Initialiser(MyDocDessin)) then DLG.ShowModal;
    DLG.Finaliser();
  finally
    DLG.Release;
  end;
end;

procedure DisplayObjetsOfThisGroupe(const MyDocDessin: TDocumentDessin; const QIdx: TIDGroupeEntites);
var
  DLG: TdlgObjetsOfGroupe;
begin
  DLG := TdlgObjetsOfGroupe.Create(Application);
  try
    if (DLG.Initialiser(MyDocDessin, Qidx)) then DLG.ShowModal;
    DLG.Finaliser();
  finally
    DLG.Release;
  end;
end;

function InputNumValue(const ACaption, APrompt: string; const ValMini, ValMax: double; var Value: double): boolean;
var
  WU: String;
begin
  Result := false;
  WU := FloatToStr(Value);
  if (InputQuery(ACaption, APrompt, WU)) then
  begin
    Value  := ConvertirEnNombreReel(WU, 0.00);
    Result := IsInRange(Value, ValMini, ValMax);
  end;
end;

procedure DisplayHelpSystem(const MyDocDessin:TDocumentDessin; const Topics: string; const ReadOnly: boolean = false);
var
  s: string;
  TD: TfrmHelpSystem;
begin
  TD := TfrmHelpSystem.Create(Application);
  try
    {$IFDEF FRENCH_MESSAGES}   s:= GetGHTopoDirectory() + 'HelpFile_fr.txt'; {$ENDIF}
    {$IFDEF ENGLISH_MESSAGES}  s:= GetGHTopoDirectory() + 'HelpFile_en.txt'; {$ENDIF}
    {$IFDEF SPANISH_MESSAGES}  s:= GetGHTopoDirectory() + 'HelpFile_es.txt'; {$ENDIF}
    {$IFDEF ITALIAN_MESSAGES}  s:= GetGHTopoDirectory() + 'HelpFile_it.txt'; {$ENDIF}
    {$IFDEF DEUTSCH_MESSAGES}  s:= GetGHTopoDirectory() + 'HelpFile_de.txt'; {$ENDIF}
    if (TD.Initialiser(MyDocDessin, s, Topics, ReadOnly)) then
      TD.ShowModal
    else
      ShowMessage(Application.ExeName + ' - version: ' + GetGHCaveDrawVersion());
  finally
    FreeAndNil(TD);
  end;
end;


function EditerStylesObjets(const MyDocDessin: TDocumentDessin; const PF: TProcRefreshStyles): boolean;
var
  TD: TdlgEditStylesObjets;
begin
  Result := False;
  TD := TdlgEditStylesObjets.Create(Application);
  try
    TD.Initialise(MyDocDessin, PF);
    TD.ShowModal;
  finally
    FreeAndNil(TD);
  end;
end;

// Question Oui-Non
function QuestionOuiNon(const Msg: string): boolean;
begin
  Result := (MessageDlg(GetResourceString(Msg), mtConfirmation,[mbYES, mbNO], 0) = mrYES);
end;
// choix d'une photo
function ChoosePhotoFileName(var PhotoFileName: string): boolean;
var
  TD: TOpenPictureDialog;
begin
  Result := False;
  TD := TOpenPictureDialog.Create(Application);
  try
    TD.InitialDir := GetGHCaveDrawDirectory();
    if (TD.Execute) then
    begin
       PhotoFileName := ExtractFileName(TD.Filename);
       Result        := True;
    end;
  finally
    FreeAndNil(TD);
  end;
end;
// choix de fichiers
function DoDialogOpenFilename(const Filters: string; var QFileName: TStringDirectoryFileName): boolean;
var
  TD: TOpenDialog;
begin
  Result := False;
  TD := TOpenDialog.Create(Application);
  try
    TD.InitialDir   := ExtractFilePath(QFileName); // Paramstr(0));
    TD.FileName     := QFileName;
    TD.Filter       := Filters;
    if (TD.Execute) then
    begin
      QFileName := TD.FileName;
      Result := True;
    end;
  finally
    FreeAndNil(TD);
  end;
end;

function DoDialogSaveFilename(const QFilters, QDefaultExt: string; var QFileName: TStringDirectoryFileName): boolean;
var
  TD: TSaveDialog;
begin
  Result := False;
  TD := TSaveDialog.Create(Application);
  try
    TD.Options      := TD.Options + [ofOverWritePrompt];
    TD.InitialDir   := ExtractFilePath(QFileName);
    TD.FileName     := ExtractFileName(QFileName);
    TD.Filter       := QFilters;
    TD.DefaultExt   := QDefaultExt;
    if (TD.Execute) then
    begin
      QFileName := TD.FileName;
      Result := True;
    end;
  finally
    FreeAndNil(TD);
  end;
end;

// choix d'un dossier
function DoDialogSelectDossier(const QInitialDir: TStringDirectoryFileName; var QDirectorySelected: TStringDirectoryFileName): boolean;
var
  DS: TSelectDirectoryDialog;
begin
  Result := false;
  DS := TSelectDirectoryDialog.Create(Application);
  try
    DS.InitialDir := QInitialDir;
    if (DS.Execute) then
    begin
       QDirectorySelected := DS.FileName;
       result := true;
    end;
  finally
    FreeAndNil(DS);
  end;

end;

function ParametrerVue2D(var ParamsVue2D: TParamsVue2D): boolean;
var
  TD: TdlgParametrerVue2D;
  EWE: TParamsVue2D;
begin
  result := false;
  EWE := ParamsVue2D;
  TD := TdlgParametrerVue2D.Create(Application);
  try
    if (TD.Initialiser(EWE)) then
    begin
      TD.ShowModal;
      if (TD.ModalResult = mrOK) then
      begin
        ParamsVue2D := TD.GetParams2DFromForm();
        result := True;
      end;
    end;
  finally
    FreeAndNil(TD);
  end;
end;

// choix d'une couleur
function ChooseColor(const OldColor: TColor): TColor;
var
  TD: TColorDialog;
begin
  Result := OldColor;
  TD := TColorDialog.Create(Application);
  try
    TD.Color := OldColor;
    if (TD.Execute) then Result := TD.Color;
  finally
    FreeAndNil(TD);
  end;
end;
procedure EditerSymboleObject(const MyDocuDessin: TDocumentDessin;
                              const CurrentIdxSymbole: integer;
                              const ModeEdition: TModeEdition;
                              const IdxGroupeCourant: TIDGroupeEntites;
                              const TypeDeSymbole   : TNatureObjetSymbole;
                              const OP: TSymbole;
                              const AbsolutePos: TPoint3Df;
                              const ProcRepercuter: TProcRefreshVues);
var
  TD: TdlgSymbolObject;
begin
  TD := TdlgSymbolObject.Create(Application);
  try
    TD.Initialiser(MyDocuDessin,
                  CurrentIdxSymbole,
                  ModeEdition,
                  IdxGroupeCourant,
                  TypeDeSymbole,
                  OP,
                  AbsolutePos,
                  ProcRepercuter);
    TD.ShowModal;
  finally
    FreeAndNil(TD);
  end;
end;

procedure EditerTextObject(const MyDocuDessin: TDocumentDessin;
                           const CurrentIdxTexte: integer;
                           const ModeEdition: TModeEdition;
                           const IdxGroupeCourant: TIDGroupeEntites;
                           const TypeDeTexte     : TNatureObjetTexte;
                           var   OT: TTextObject;
                           const AbsolutePos: TPoint3Df;
                           const ProcRepercute   : TProcRefreshVues);
var
  TD: TdlgTextObject;
begin
  TD := TdlgTextObject.Create(Application);
  try
    TD.Initialiser(MyDocuDessin, CurrentIdxTexte, ModeEdition, IdxGroupeCourant, TypeDeTexte, OT, AbsolutePos, ProcRepercute);
    TD.ShowModal;
  finally
    FreeAndNil(TD);
  end;
end;
// dialogue d'édition de groupe
function EditerGroupe(var MyGroupe: TGroupeEntites; const ME: TModeEdition; const QFiltres: string; out DoSetThisGroupeAsCurrent: boolean): boolean;
var
  TD: TdlgGroupe;
begin
  Result := False;
  TD := TdlgGroupe.Create(Application);
  try
    TD.Initialise(MyGroupe, ME, QFiltres);
    TD.ShowModal;
    if (TD.ModalResult = mrOK) then
    begin
      MyGroupe := TD.GetGroupe();
      DoSetThisGroupeAsCurrent := TD.GetDoSetGrpAsCurrent();
      Result := True;
    end;
  finally
    FreeAndNil(TD);
  end;
end;



// garnir une combobox avec les types de symboles
procedure RemplirComboTypesSymboles(const CB: TComboBox; const Idx: integer);
var
  i: Integer;
begin
  CB.Clear;
  for i := 0 to High(ListeNatureSymboles) do
    CB.Items.Add(ListeNatureSymboles[i]);
  CB.DropDownCount := CB.Items.Count;
  CB.ItemIndex     := Idx;
end;

// garnir combobox avec types de courbes
procedure RemplirComboTypesCourbes(const CB: TComboBox; const Idx: integer);
var
  i: Integer;
begin
  CB.Clear;
  for i := 0 to High(ListeStylesCourbes) do
    CB.Items.Add(ListeStylesCourbes[i]);
  CB.DropDownCount := CB.Items.Count;
  CB.ItemIndex     := Idx;
end;
// garnir combobox avec types de polygones
procedure RemplirComboTypesPolygones(const CB: TComboBox; const Idx: integer);
var
  i: Integer;
begin
  CB.Clear;
  for i := 0 to High(ListeStylesPolygones) do
    CB.Items.Add(ListeStylesPolygones[i]);
  CB.DropDownCount := CB.Items.Count;
  CB.ItemIndex     := Idx;
end;
// garnir combobox avec types de lignes
procedure RemplirComboTypesSimplesLignes(const CB: TComboBox; const Idx: integer);
var
  i: Integer;
begin
  CB.Clear;
  for i := 0 to High(ListeStylesLignes) do
    CB.Items.Add(ListeStylesLignes[i]);
  CB.DropDownCount := CB.Items.Count;
  CB.ItemIndex     := Idx;
end;

// garnir combobox avec types de polygones
procedure RemplirComboTypesTextes(const CB: TComboBox; const Idx: integer);
var
  i: Integer;
begin
  CB.Clear;
  for i := 0 to High(ListeStylesTextes) do
    CB.Items.Add(ListeStylesTextes[i]);
  CB.DropDownCount := CB.Items.Count;
  CB.ItemIndex     := Idx;
end;


function EditerScrap(const MyDocDessin: TDocumentDessin; var MyScrap: TScrap): boolean;
var
  TD: TdlgEditScrap;
begin
  Result := False;
  TD := TdlgEditScrap.Create(Application);
  try
    TD.Initialise(MyDocDessin, MyScrap);
    TD.ShowModal;
    if (TD.ModalResult = mrOK) then
    begin
      MyScrap := TD.GetScrap();
      Result := True;
    end;
  finally
    FreeAndNil(TD);
  end;
end;

// ajout d'images
function DoDialogTImageObject(const FD: TDocumentDessin;
                              var EWE: TImageObject;
                              const QInitialDir: string;
                              const ImgPosition: TPoint3Df;
                              const DoCreateImage: boolean): boolean;
var
  P1: TPoint2Df;
  DLG: TdlgTImageObject;
begin
  Result := false;
  DLG := TdlgTImageObject.Create(Application);
  try
    DLG.Initialiser(FD, QInitialDir, DoCreateImage);
    P1.setFrom(ImgPosition.X, ImgPosition.Y);
    DLG.setPositionPtRefImage(P1, 50.00);
    DLG.ShowModal;
    if (DLG.ModalResult = mrOK) then
    begin
      EWE := DLG.getImageObject();
      Result := True;
    end;
    DLG.Finaliser();
  finally
    FreeAndNil(DLG);
  end;
end;
// affichage d'un petit éditeur de texte intégré

procedure DisplayEditeurTexte(const QText: TStringList; const QTitreDlg: string; const QM: TModeSelectionEntites); overload;
var
  DLG: TdlgEditeurTexte;
begin
  DLG := TdlgEditeurTexte.Create(Application);
  try
    if (DLG.Initialiser()) then
    begin
      DLG.SetText(QText);
      DLG.Caption := QTitreDlg;
      DLG.ShowModal;
    end;
  finally
    FreeAndNil(DLG);
  end;
end;
// affiche le message d'erreur et met les consoles en avant plan
procedure NotifyError(const Msg: string);
begin
  ShowMessage(Msg);
  dlgProcessing.SetFocus;
end;

function EditerSectionTransversale(const FD: TDocumentDessin; const BP: TBaseStation; const QOffsetX, QOffsetY: double): boolean;
var
  DLG: TfrmEditeurSections;
begin
  result := false;
  DLG := TfrmEditeurSections.Create(Application);
  try
    if (DLG.Initialiser(FD, BP, QOffsetX, QOffsetY)) then
    begin
      DLG.ShowModal;
      DLG.Finaliser();
      result := True;
    end;
  finally
    FreeAndNil(DLG);
  end;

end;

procedure DisplayDlgExportToTherion(const FD: TDocumentDessin);
var
  TD: TdlgExportToTherion;
begin
  TD := TdlgExportToTherion.Create(Application);
  try
    if (TD.Initialiser(FD)) then
    begin
      TD.ShowModal;
    end;
  finally
    FreeAndNil(TD);
  end;
end;

procedure DisplayDlgExportGIS(const FD: TDocumentDessin);
var
  TD: TdlgExportSIG;
begin
  TD := TdlgExportSIG.Create(Application);
  try
    if (TD.Initialiser(FD)) then
    begin
      TD.ShowModal;
    end;
  finally
    FreeAndNil(TD);
  end;
end;

function SelectIndexGroupe(const FD: TDocumentDessin;
                           const IdxGroupe: TIDGroupeEntites;
                           out   IdxSelected: TIDGroupeEntites): boolean;
var
  DG: TdlgListeGroupes;
begin
  Result := false;
  IdxSelected := -1;
  DG := TdlgListeGroupes.Create(Application);
  try
    if (DG.Initialiser(FD, IdxGroupe)) then
    begin
      DG.ShowModal;
      if (DG.ModalResult = mrOK) then
      begin
        IdxSelected := DG.GetSelectedIndex;
        Result := true;
      end;
    end;
  finally
    DG.Release;
  end;

end;

function CreateFichePointTopo(const FD: TDocumentDessin;
                              const QTextures: TTexturesPolygonObject;
                              const Station: TBaseStation;
                              const ParamsVueD: TParamsVue2D): boolean;
var
  LesFichesPointTopo: TFichesPointTopo;
begin
  Result := false;
  LesFichesPointTopo := TFichesPointTopo.Create;
  try
    if (LesFichesPointTopo.Initialiser(FD, QTextures, ParamsVueD)) then
    begin
      LesFichesPointTopo.AddStation(Station);
      LesFichesPointTopo.CreerLesFiches();
      Result := True;
    end;
    LesFichesPointTopo.Finaliser();
  finally
    FreeAndNil(LesFichesPointTopo);
  end;


end;

function DisplayPrintingCenter(const FD: TDocumentDessin;
                               const TP: TTexturesPolygonObject;
                               const FP: TParamsVue2D): boolean;
var
  TD: TfrmPrintingCenterExt;
begin
  // contexte pour la grille de dessin
  Result := false;
  TD := TfrmPrintingCenterExt.Create(Application);
  try
    if (TD.Initialiser(FD, TP, FP, ''))  then
    begin
      TD.ShowModal;
      Result := True;
    end;
  finally
    FreeAndNil(TD);
  end;
end;

function DisplayPOI(const FD: TDocumentDessin): boolean;
var
  TD: TdlgListePOI;
begin
  result := false;
  TD := TdlgListePOI.Create(Application);
  try
    if (TD.Initialiser(FD))  then
    begin
      TD.ShowModal;
      TD.Finaliser();
    end;
  finally
    FreeAndNil(TD);
  end;

end;

function SelectionnerImprimante(var MyPrinter: TPrinter): boolean;
var
  PC: TPrinterSetupDialog;
begin
  result := false;
  PC := TPrinterSetupDialog.Create(Application);
  try
    result := PC.Execute;
  finally
    FreeAndNil(PC);
  end;
end;

// sélection d'un système de coordonnées
function SelectCoordinatesSystem(const FC: TConversionSysteme; out SC: TLabelSystemesCoordsEPSG): boolean;
var
  TD: TdlgSelectionCoordsSystem;
begin
  result := false;
  TD :=  TdlgSelectionCoordsSystem.Create(Application);
  try
    if (TD.Initialiser(FC)) then
    begin
      TD.ShowModal;
      if (TD.ModalResult = mrOK) then
      begin
        SC := TD.GetSelectedEPSGSysCoords();
        result := true;
      end;
    end;
  finally
    FreeAndNil(TD);
  end;
end;

function SelectCouleurAutocad(const OldIdx: integer): integer;
var
  TD: TdlgSelectAutocadColor;
begin
  Result := OldIdx;
  TD := TdlgSelectAutocadColor.Create(Application);
  try
    TD.Initialiser(OldIdx);
    TD.ShowModal;
    if (TD.ModalResult = mrOK) then Result  := TD.GetIdxColor();
  finally
    TD.Release;
  end;
end;
// bac à sable
function DisplaySandbox(const FD: TDocumentDessin): boolean;
var
  TD: TdlgBacASable;
begin
  TD := TdlgBacASable.Create(Application);
  try
    TD.Initialiser(FD);
    TD.ShowModal;
  finally
    TD.Release;
  end;
end;

procedure DisplayToast(AOwner: TWinControl; const Msg: string; const DureeInMS: integer = 1500);
const
  TW = 400;
  TH =  60;
var
  MyToast : TPanel;
  MyParent: TComponent;
  t: TDateTime;
  DELAI   : TDateTime;
  R: TSize;
begin
  DELAI := DureeInMS / (1000.0 * 86400.0);

  MyToast := TPanel.Create(AOwner);
  //try

    MyToast.Parent      := AOwner;
    MyToast.Color       := clInfoBk;
    MyToast.Caption     := Msg;
    //MyToast.AutoSize    := True;

    MyToast.Width       := TW;
    MyToast.Height      := TH;
    MyToast.Left        := (AOwner.Width  - TW) div 2;
    MyToast.Top         := (AOwner.Height - TH) div 2;
    MyToast.Enabled     := True;
    MyToast.Show;
    t := Now();
    while (Now() < (t + DELAI)) do Application.ProcessMessages;
    //Application.ProcessMessages;
    MyToast.Hide;

    (*

    MyToast.AutoSize    := True;
    MyToast.Caption     := Msg;




    //*)
  //finally
  //  FreeAndNil(MyToast);
  //end;
end;

end.

