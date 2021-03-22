unit unitSimplificationCourbe;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  GHCD_Types,
  GeneralFunctions,
  UnitListesSimplesWithGeneriques;
procedure SimplifyInt2D(var Tol2: double; const Orig: array of TPoint2Df;
                        var Marker: array of boolean; const j, k: integer);

function PolySimplifyInt2D(const Tol: double;
                           const Orig: array of TPoint2Df;
                           var   QSimple: TArrayPoints2Df): integer;

implementation
function VecMinInt2D(const A, B: TPoint2Df): TPoint2Df;
// Result = A - B
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

function DotProdInt2D(const A, B: TPoint2Df): Double;
// Dotproduct = A * B
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

function NormSquaredInt2D(const A: TPoint2Df): Double;
// Square of the norm |A|
begin
  Result := A.X * A.X + A.Y * A.Y;
end;

function DistSquaredInt2D(const A, B: TPoint2Df): Double;
// Square of the distance from A to B
begin
  Result := NormSquaredInt2D(VecMinInt2D(A, B));
end;
function SafeDivide(const a, b: double; const Default: double): double;
begin
  try
    Result := a / b;
  except
    Result := Default;
  end;
end;

procedure SimplifyInt2D(var Tol2: double; const Orig: array of TPoint2Df;
                        var Marker: array of boolean; const j, k: integer);
// Simplify polyline in OrigList between j and k. Marker[] will be set to True
// for each point that must be included
var
  i, MaxI: integer; // Index at maximum value
  MaxD2: double;    // Maximum value squared
  CU, CW, B: double;
  DV2: double;
  P0, P1, PB, U, W: TPoint2Df;
begin
  // Is there anything to simplify?
  if k <= j + 1 then exit;

  P0 := Orig[j];
  P1 := Orig[k];
  U  := VecMinInt2D(P1, P0); // Segment vector
  CU := DotProdInt2D(U, U); // Segment length squared
  MaxD2 := 0;
  MaxI  := 0;

  // Loop through points and detect the one furthest away
  for i := j + 1 to k - 1 do begin
    W  := VecMinInt2D(Orig[i], P0);
    CW := DotProdInt2D(W, U);

    // Distance of point Orig[i] from segment
    if (CW <= 0) then
    begin
      // Before segment
      DV2 := DistSquaredInt2D(Orig[i], P0)
    end
    else
    begin
      if (CW > CU) then
      begin
        // Past segment
        DV2 := DistSquaredInt2D(Orig[i], P1);
      end else
      begin
        // Fraction of the segment
        B := SafeDivide(CW, CU, 0.00);
        PB.X := P0.X + B * U.X;
        PB.Y := P0.Y + B * U.Y;
        DV2 := DistSquaredInt2D(Orig[i], PB);
      end;
    end;

    // test with current max distance squared
    if (DV2 > MaxD2) then
    begin
      // Orig[i] is a new max vertex
      MaxI  := i;
      MaxD2 := DV2;
    end;
  end;

  // If the furthest point is outside tolerance we must split
  if (MaxD2 > Tol2) then
  begin // error is worse than the tolerance
    // split the polyline at the farthest vertex from S
    Marker[MaxI] := True;  // mark Orig[maxi] for the simplified polyline
    // recursively simplify the two subpolylines at Orig[maxi]
    SimplifyInt2D(Tol2, Orig, Marker, j, MaxI); // polyline Orig[j] to Orig[maxi]
    SimplifyInt2D(Tol2, Orig, Marker, MaxI, k); // polyline Orig[maxi] to Orig[k]
  end;
end;


function PolySimplifyInt2D(const Tol: double;
                           const Orig: array of TPoint2Df;
                           var   QSimple: TArrayPoints2Df): integer;
var
  i, N: integer;
  Marker: array of boolean;
  Tol2: double;
  FTmpLst: TListeSimple<TPoint2Df>;
begin
  AfficherMessage(Format('PolySimplifyInt2D: Tol: %.2f, n=%d', [Tol, length(Orig)]));
  Result := -1;
  if (length(Orig) < 2) then exit;
  Tol2 := Tol * Tol;
  try
    // Create a marker array
    N := Length(Orig);
    SetLength(Marker, N);
    // Include first and last point
    Marker[0]     := True;
    Marker[N - 1] := True;
    // Exclude intermediate for now
    for i := 1 to N - 2 do Marker[i] := False;

    // Simplify
    SimplifyInt2D(Tol2, Orig, Marker, 0, N - 1);
    AfficherMessage(Format('Marqueurs -- N=%d', [N]));
    // Copy to resulting list
    FTmpLst:= TListeSimple<TPoint2Df>.Create;
    FTmpLst.ClearListe();
    for i := 0 to N - 1 do
    begin
      AfficherMessage(Format('%d: %s', [i, BoolToStr(Marker[i], 'Vrai', 'Faux')]));
      if (Marker[i]) then FTmpLst.AddElement(Orig[i]);
    end;
    N := FTmpLst.GetNbElements();
    SetLength(QSimple, N);
    for i := 0 to N - 1 do
    begin
      QSimple[i] := FTmpLst.GetElement(i);
      AfficherMessage(Format('%d: %.2f, %.2f', [i, QSimple[i].X, QSimple[i].Y]));
    end;
      FTmpLst.ClearListe();
    Result := N;
  finally
    FreeAndNil(FTmpLst);//       FTmpLst.Free;
  end;
end;

end.

