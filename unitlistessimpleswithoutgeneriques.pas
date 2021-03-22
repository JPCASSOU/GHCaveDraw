unit UnitListesSimplesWithoutGeneriques deprecated 'For use with Lazarus / FPC 2.6.0 under ARM Raspberry architectures';
// 11/08/2016: Version sans listes génériques
// Cette unité est une 'béquille', en attente de la dispo de FreePascal 3.x sur ARM
// et a été écrite pour wrapper le TListeSimple
// la version 2.6 de FPC ne gère pas ou très mal la généricité



{$INCLUDE CompilationParameters.inc}

interface

uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  Classes, SysUtils;
type TTableEntrees = class(TList)
  private
  public
    procedure AddElement(const E: TEntrance);
    procedure ClearListe;
    function GetElement(const Idx: integer): TEntrance;
    function GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TEntrance);
    function RemoveElement(const Idx: integer): boolean;

end;

type TTableReseaux = class(TList)
  private
  public
    procedure AddElement(const E: TReseau);
    procedure ClearListe;
    function GetElement(const Idx: integer): TReseau;
    function GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TReseau);
    function RemoveElement(const Idx: integer): boolean;
end;

type TTableSecteurs = class(TList)
  private
  public
    procedure AddElement(const E: TSecteur);
    procedure ClearListe;
    function GetElement(const Idx: integer): TSecteur;
    function GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TSecteur);
    function RemoveElement(const Idx: integer): boolean;
end;

type TTableCodes = class(TList)
  private
  public
    procedure AddElement(const E: TCode);
    procedure ClearListe;
    function GetElement(const Idx: integer): TCode;
    function GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TCode);
    function RemoveElement(const Idx: integer): boolean;
    function GetElementByIndex(const Idx: integer): TCode;
    function ExistsElement(const Idx: integer): boolean;
end;
type TTableExpes = class(TList)
  private
  public
    procedure AddElement(const E: TExpe);
    procedure ClearListe;
    function GetElement(const Idx: integer): TExpe;
    function GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TExpe);
    function RemoveElement(const Idx: integer): boolean;
    function GetElementByIndex(const Idx: integer): TExpe;
    function ExistsElement(const Idx: integer): boolean;
end;

type TTableViseesAntenne = class(TList)
  private
  public
    procedure AddElement(const E: TViseeAntenne);
    procedure ClearListe;
    function GetElement(const Idx: integer): TViseeAntenne;
    function GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TViseeAntenne);
    function RemoveElement(const Idx: integer): boolean;
    function GetElementByIndex(const Idx: integer): TViseeAntenne;
end;
type TTableIDTerrain = class(TList)
  private
  public
    procedure AddElement(const E: TEtiquetteTerrain);
    procedure ClearListe;
    function  GetElement(const Idx: integer): TEtiquetteTerrain;
    function  GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TEtiquetteTerrain);
    function RemoveElement(const Idx: integer): boolean;
end;

type  TMNTTableVertex    = class(TList)
  private
  public
    procedure AddElement(const E: TVertex);
    procedure ClearListe;
    function  GetElement(const Idx: integer): TVertex;
    function  GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TVertex);
    function  RemoveElement(const Idx: integer): boolean;

end;
type  TMNTTableTriangles = class(TList)
  private
  public
    procedure AddElement(const E: TMNTTriangleABC);
    procedure ClearListe;
    function  GetElement(const Idx: integer): TMNTTriangleABC;
    function  GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TMNTTriangleABC);
    function  RemoveElement(const Idx: integer): boolean;
end;

type THelpListeDesSections = class(TList) //eSimple<THelpFileSection>)
  private
  public
    procedure AddElement(const E: THelpFileSection);
    procedure ClearListe;
    function  GetElement(const Idx: integer): THelpFileSection;
    function  GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: THelpFileSection);
    function  RemoveElement(const Idx: integer): boolean;
end;





// pour le DistoX
type TListePoints3Df = class(TList)
  private
  public
    procedure AddElement(const E: TPoint3Df);
    procedure ClearListe;
    function  GetElement(const Idx: integer): TPoint3Df;
    function  GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TPoint3Df);
    function  RemoveElement(const Idx: integer): boolean;
end;
type TListeMesuresTopoDistoX = class(TList)
  private
  public
    procedure AddElement(const E: TMesureViseeDistoX);
    procedure ClearListe;
    function  GetElement(const Idx: integer): TMesureViseeDistoX;
    function  GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TMesureViseeDistoX);
    function  RemoveElement(const Idx: integer): boolean;
end;

type TListeSelectedSeries = class(TList)
  private
  public
    procedure AddElement(const E: Integer);
    procedure ClearListe;
    function  GetElement(const Idx: integer): Integer;
    function  GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: Integer);
    function  RemoveElement(const Idx: integer): boolean;
end;

type TListePointsTopo = class(TList) //eSimple<TUneVisee>)
  private
  public
    procedure AddElement(const E: TUneVisee);
    procedure ClearListe;
    function  GetElement(const Idx: integer): TUneVisee;
    function  GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TUneVisee);
    function  RemoveElement(const Idx: integer): boolean;

end;

type TListeJonctionsCoupe = class(TList)
  private
  public
    procedure AddElement(const E: TJonctionCoupeDeveloppee);
    procedure ClearListe;
    function  GetElement(const Idx: integer): TJonctionCoupeDeveloppee;
    function  GetNbElements: integer;
    procedure PutElement(const Idx: integer; const E: TJonctionCoupeDeveloppee);
    function  RemoveElement(const Idx: integer): boolean;
end;

type TTableJonctionsXYZ = class(TList)
  private
  public
    procedure AddElement(const E: TJonctionXYZ);
    procedure ClearListe;
    function  GetElement(const Idx: Integer): TJonctionXYZ;
    function  GetNbElements: Integer;
    procedure PutElement(const Idx: integer; const E: TJonctionXYZ);
    function  RemoveElement(const Idx: integer): boolean;
end;

type TTableBranchesXYZ = class(TList)
  private
  public
    procedure AddElement(const E: TBrancheXYZ);
    procedure ClearListe;
    function  GetElement(const Idx: Integer): TBrancheXYZ;
    function  GetNbElements: Integer;
    procedure PutElement(const Idx: integer; const E: TBrancheXYZ);
    function  RemoveElement(const Idx: integer): boolean;
end;





implementation

//******************************************************************************
{ TTableEntrees }
procedure TTableEntrees.ClearListe;
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TTableEntrees.GetNbElements: integer;
begin
  Result := self.Count;
end;

procedure TTableEntrees.AddElement(const E: TEntrance);
var
  pE: ^TEntrance;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TTableEntrees.GetElement(const Idx: integer): TEntrance;
var
  pE: ^TEntrance;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;



procedure TTableEntrees.PutElement(const Idx: integer; const E: TEntrance);
var
  pE: ^TEntrance;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TTableEntrees.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;


//******************************************************************************
{ TTableReseaux }
procedure TTableReseaux.ClearListe;
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TTableReseaux.GetNbElements: integer;
begin
  Result := self.Count;
end;

procedure TTableReseaux.AddElement(const E: TReseau);
var
  pE: ^TReseau;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TTableReseaux.GetElement(const Idx: integer): TReseau;
var
  pE: ^TReseau;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TTableReseaux.PutElement(const Idx: integer; const E: TReseau);
var
  pE: ^TReseau;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TTableReseaux.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;
//******************************************************************************
{ TTableSecteurs }
procedure TTableSecteurs.ClearListe;
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TTableSecteurs.GetNbElements: integer;
begin
  Result := self.Count;
end;

procedure TTableSecteurs.AddElement(const E: TSecteur);
var
  pE: ^TSecteur;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TTableSecteurs.GetElement(const Idx: integer): TSecteur;
var
  pE: ^TSecteur;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;



procedure TTableSecteurs.PutElement(const Idx: integer; const E: TSecteur);
var
  pE: ^TSecteur;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TTableSecteurs.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;
//******************************************************************************
{ TTableCodes }
procedure TTableCodes.ClearListe;
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TTableCodes.GetNbElements: integer;
begin
  Result := self.Count;
end;

procedure TTableCodes.AddElement(const E: TCode);
var
  pE: ^TCode;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TTableCodes.GetElement(const Idx: integer): TCode;
var
  pE: ^TCode;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;



procedure TTableCodes.PutElement(const Idx: integer; const E: TCode);
var
  pE: ^TCode;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TTableCodes.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;
function TTableCodes.GetElementByIndex(const Idx: integer): TCode;
var
  ii: integer;
  E : TCode;
begin
  // NE PAS SECURISER A CE NIVEAU
  for ii:=0 to self.Count-1 do
  begin
    E := self.GetElement(ii);
    if (E.IDCode = Idx) then Break;
  end;
  Result := E;
end;
function TTableCodes.ExistsElement(const Idx: integer): boolean;
var
  i: integer;
  EWE: TCode;
begin
  Result := False;
  for i:= 0 to self.Count - 1 do
  begin
    EWE := self.GetElement(i);
    if (EWE.IDCode = Idx) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;
//******************************************************************************
{ TTableExpes }
procedure TTableExpes.ClearListe;
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TTableExpes.GetNbElements: integer;
begin
  Result := self.Count;
end;

procedure TTableExpes.AddElement(const E: TExpe);
var
  pE: ^TExpe;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TTableExpes.GetElement(const Idx: integer): TExpe;
var
  pE: ^TExpe;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TTableExpes.PutElement(const Idx: integer; const E: TExpe);
var
  pE: ^TExpe;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TTableExpes.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;
function TTableExpes.GetElementByIndex(const Idx: integer): TExpe;
var
  ii: integer;
  E : TExpe;
begin
  // NE PAS SECURISER A CE NIVEAU
  for ii:=0 to self.Count-1 do
  begin
    E := self.GetElement(ii);
    if (E.IDExpe = Idx) then Break;
  end;
  Result := E;
end;

function TTableExpes.ExistsElement(const Idx: integer): boolean;
var
  i: integer;
  EWE: TExpe;
begin
  Result := False;
  for i:= 0 to self.GetNbElements - 1 do
  begin
    EWE := self.GetElement(i);
    if (EWE.IDExpe = Idx) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;
//******************************************************************************
{ TTableViseesAntenne }
procedure TTableViseesAntenne.ClearListe();
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TTableViseesAntenne.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TTableViseesAntenne.AddElement(const E: TViseeAntenne);
var
  pE: ^TViseeAntenne;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TTableViseesAntenne.GetElement(const Idx: integer): TViseeAntenne;
var
  pE: ^TViseeAntenne;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TTableViseesAntenne.PutElement(const Idx: integer; const E: TViseeAntenne);
var
  pE: ^TViseeAntenne;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TTableViseesAntenne.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;
function TTableViseesAntenne.GetElementByIndex(const Idx: integer): TViseeAntenne;
var
  ii: integer;
  E : TViseeAntenne;
begin
  Result := self.GetElement(0); // sécurisation
  if (not IsInRange(Idx, 0, self.Count - 1)) then Exit;
  for ii:=0 to self.Count-1 do
  begin
    E := self.GetElement(ii);
    if (E.IDViseeAntenne = Idx) then Break;
  end;
  Result := E;
end;

//******************************************************************************
{ TTableIDTerrain }
procedure TTableIDTerrain.ClearListe();
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TTableIDTerrain.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TTableIDTerrain.AddElement(const E: TEtiquetteTerrain);
var
  pE: ^TEtiquetteTerrain;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TTableIDTerrain.GetElement(const Idx: integer): TEtiquetteTerrain;
var
  pE: ^TEtiquetteTerrain;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TTableIDTerrain.PutElement(const Idx: integer; const E: TEtiquetteTerrain);
var
  pE: ^TEtiquetteTerrain;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TTableIDTerrain.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;

//******************************************************************************
{ TMNTTableVertex }
procedure TMNTTableVertex.ClearListe();
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TMNTTableVertex.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TMNTTableVertex.AddElement(const E: TVertex);
var
  pE: ^TVertex;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TMNTTableVertex.GetElement(const Idx: integer): TVertex;
var
  pE: ^TVertex;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TMNTTableVertex.PutElement(const Idx: integer; const E: TVertex);
var
  pE: ^TVertex;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TMNTTableVertex.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;

//******************************************************************************
{ TTableIDTerrain }
procedure TMNTTableTriangles.ClearListe();
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TMNTTableTriangles.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TMNTTableTriangles.AddElement(const E: TMNTTriangleABC);
var
  pE: ^TMNTTriangleABC;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TMNTTableTriangles.GetElement(const Idx: integer): TMNTTriangleABC;
var
  pE: ^TMNTTriangleABC;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TMNTTableTriangles.PutElement(const Idx: integer; const E: TMNTTriangleABC);
var
  pE: ^TMNTTriangleABC;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TMNTTableTriangles.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;
//******************************************************************************
{ THelpListeDesSections }
procedure THelpListeDesSections.ClearListe();
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function THelpListeDesSections.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure THelpListeDesSections.AddElement(const E: THelpFileSection);
var
  pE: ^THelpFileSection;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function THelpListeDesSections.GetElement(const Idx: integer): THelpFileSection;
var
  pE: ^THelpFileSection;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure THelpListeDesSections.PutElement(const Idx: integer; const E: THelpFileSection);
var
  pE: ^THelpFileSection;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function THelpListeDesSections.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;

//******************************************************************************
{         TListeDesJonctions }
procedure TListeJonctionsCoupe.ClearListe();
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TListeJonctionsCoupe.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TListeJonctionsCoupe.AddElement(const E: TJonctionCoupeDeveloppee);
var
  pE: ^TJonctionCoupeDeveloppee;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TListeJonctionsCoupe.GetElement(const Idx: integer): TJonctionCoupeDeveloppee;
var
  pE: ^TJonctionCoupeDeveloppee;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TListeJonctionsCoupe.PutElement(const Idx: integer; const E: TJonctionCoupeDeveloppee);
var
  pE: ^TJonctionCoupeDeveloppee;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TListeJonctionsCoupe.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;
//******************************************************************************
{         TTableJonctionsXYZ }
procedure TTableJonctionsXYZ.ClearListe();
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TTableJonctionsXYZ.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TTableJonctionsXYZ.AddElement(const E: TJonctionXYZ);
var
  pE: ^TJonctionXYZ;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TTableJonctionsXYZ.GetElement(const Idx: integer): TJonctionXYZ;
var
  pE: ^TJonctionXYZ;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TTableJonctionsXYZ.PutElement(const Idx: integer; const E: TJonctionXYZ);
var
  pE: ^TJonctionXYZ;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TTableJonctionsXYZ.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;
//******************************************************************************
//        TListePoints3Df
procedure TListePoints3Df.ClearListe();
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TListePoints3Df.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TListePoints3Df.AddElement(const E: TPoint3Df);
var
  pE: ^TPoint3Df;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TListePoints3Df.GetElement(const Idx: integer): TPoint3Df;
var
  pE: ^TPoint3Df;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TListePoints3Df.PutElement(const Idx: integer; const E: TPoint3Df);
var
  pE: ^TPoint3Df;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TListePoints3Df.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;



//******************************************************************************
//        TListeMesuresTopoDistoX
procedure TListeMesuresTopoDistoX.ClearListe();
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TListeMesuresTopoDistoX.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TListeMesuresTopoDistoX.AddElement(const E: TMesureViseeDistoX);
var
  pE: ^TMesureViseeDistoX;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TListeMesuresTopoDistoX.GetElement(const Idx: integer): TMesureViseeDistoX;
var
  pE: ^TMesureViseeDistoX;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TListeMesuresTopoDistoX.PutElement(const Idx: integer; const E: TMesureViseeDistoX);
var
  pE: ^TMesureViseeDistoX;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TListeMesuresTopoDistoX.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;
//******************************************************************************
//        TListeSelectedSeries
procedure TListeSelectedSeries.ClearListe();
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TListeSelectedSeries.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TListeSelectedSeries.AddElement(const E: Integer);
var
  pE: ^Integer;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TListeSelectedSeries.GetElement(const Idx: integer): Integer;
var
  pE: ^Integer;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TListeSelectedSeries.PutElement(const Idx: integer; const E: Integer);
var
  pE: ^Integer;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TListeSelectedSeries.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;
//******************************************************************************
//        TTableJonctions = class(TList) //eSimple<TJonction>)
procedure TTableBranchesXYZ.ClearListe();
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TTableBranchesXYZ.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TTableBranchesXYZ.AddElement(const E: TBrancheXYZ);
var
  pE: ^TBrancheXYZ;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TTableBranchesXYZ.GetElement(const Idx: integer): TBrancheXYZ;
var
  pE: ^TBrancheXYZ;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TTableBranchesXYZ.PutElement(const Idx: integer; const E: TBrancheXYZ);
var
  pE: ^TBrancheXYZ;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TTableBranchesXYZ.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;

//******************************************************************************
//        TListePointsTopo = class(TList) //eSimple<TUneVisee>)
procedure TListePointsTopo.ClearListe();
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

function TListePointsTopo.GetNbElements(): integer;
begin
  Result := self.Count;
end;

procedure TListePointsTopo.AddElement(const E: TUneVisee);
var
  pE: ^TUneVisee;
begin
 New(pE);
 pE^ := E;
 self.Add(pE);
end;

function TListePointsTopo.GetElement(const Idx: integer): TUneVisee;
var
  pE: ^TUneVisee;
begin
  pE:= self.Items[Idx];
  Result := pE^;
end;
procedure TListePointsTopo.PutElement(const Idx: integer; const E: TUneVisee);
var
  pE: ^TUneVisee;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TListePointsTopo.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;


end.

