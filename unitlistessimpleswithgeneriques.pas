unit UnitListesSimplesWithGeneriques;
// 04/06/2015: Version basée sur les génériques

//{$INCLUDE CompilationParameters.inc}

interface

uses
  //{$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  //GHCD_Types,
  Classes, SysUtils
  , contnrs
  ;


// liste générique (fonctionne très bien)
type

{ TListeSimple }

 TListeSimple<T> = class(TList)
  private
  public
    procedure ClearListe();
    function  GetNbElements(): integer;
    procedure AddElement(const E: T);  // et Rros Minet
    function  GetElement(const Idx: integer): T;
    procedure PutElement(const Idx: integer; const E: T);
    function  RemoveElement(const Idx: integer): boolean;
    procedure InsertElement(const Idx: integer; const E: T);
    procedure ExchangeElements(const Idx1, Idx2: integer);
    procedure RemoveLastElement();
    function  SortElements(ProcSort: TListSortCompare): boolean;
    // procedures de pile, qui est une liste simplifiée
    //procedure PushElement(const E: T); inline;
    //procedure PushElements(const AE: array of T);
    //function  PopElement(): T; // retire l'élément de la pile
end;

implementation
uses DGCDummyUnit;

{ TListeSimple<T> }

procedure TListeSimple<T>.ClearListe;
var
  i: Integer;
begin
  try
    if (self.Count > 0) then for i:= 0 to self.Count - 1 do Dispose(self.Items[i]);
  finally
    self.Clear;
  end;
end;

procedure TListeSimple<T>.RemoveLastElement();
var
  Nb : integer;
begin
  Nb := self.Count;
  if (Nb = 0) then Exit;
  Dispose(self.Items[Nb - 1]);
  self.Delete(Nb - 1);
end;

procedure TListeSimple<T>.ExchangeElements(const Idx1, Idx2: integer);
begin
  self.Exchange(Idx1, Idx2);
end;

function TListeSimple<T>.GetNbElements: integer;
begin
  Result := self.Count;
end;

procedure TListeSimple<T>.AddElement(const E: T);
var
  pE: ^T;
begin
  New(pE);
  pE^ := E;
  self.Add(pE);
end;

function TListeSimple<T>.GetElement(const Idx: integer): T;
var
  pE: ^T;
  QIdx: integer;
begin
  if (Idx > self.Count - 1) then QIdx := 0 else QIdx := Idx;
  pE:= self.Items[QIdx];
  Result := pE^;
end;



procedure TListeSimple<T>.PutElement(const Idx: integer; const E: T);
var
  pE: ^T;
begin
  pE := self.Items[Idx];
  pE^:= E;
  self.Items[Idx] := pE;
end;

function TListeSimple<T>.RemoveElement(const Idx: integer): boolean;
begin
  Result := False;
  try
    Dispose(self.Items[Idx]);
    self.Delete(Idx);
    Result := True;
  except
  end;
end;

function TListeSimple<T>.SortElements(ProcSort: TListSortCompare): boolean;
begin
  self.Sort(ProcSort);
end;

procedure TListeSimple<T>.InsertElement(const Idx: integer; const E: T);
var
  pT: ^T;
begin
  try
    New(pT);
    pT^ := E;
    self.Insert(Idx, pT);
  except
  end;
end;

end.
