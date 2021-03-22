unit CalculMatriciel3x3;
// calcul matriciel de base pour éviter import d'une librairie
// et les gds parcours de boucles, allocations dynamiques et consorts
// NOTA: Ne pas utiliser le mode Delphi si on utilise la surcharge d'opérateurs

//{$INCLUDE CompilationParameters.inc}
{$mode objfpc} // pour surcharge d'opérateurs
{$ERROR Ce fichier n'a rien a faire ici}

interface
uses
  StructuresDonnees,
  math,
  Classes, SysUtils;
operator + (const A, B: TPoint3Df) R : TPoint3Df;
operator - (const A, B: TPoint3Df) R : TPoint3Df;
operator * (const A: TPoint3Df; const k: double) R : TPoint3Df;
operator ** (const A, B: TPoint3Df) R : double; // produit scalaire
operator / (const A, B: TPoint3Df) R : TPoint3Df; // produit vectoriel
operator >< (const A, B: TPoint3Df) R : TMatrix3x3; // produit tensoriel de Kronecker


operator + (const A, B: TMatrix3x3) R : TMatrix3x3;
operator - (const A, B: TMatrix3x3) R : TMatrix3x3;
operator * (const A: TMatrix3x3; const k: double) R : TMatrix3x3;
operator ** (const A, B: TMatrix3x3) R : TMatrix3x3; // produit matriciel

// matrices 2x2
function LoadIdentity2x2: TMatrix2x2;
function ProduitMat2x2(const A, B: TMatrix2x2): TMatrix2x2;
function DetMat2x2(const M: TMatrix2x2): double;
function InvMat2x2(const M: TMatrix2x2): TMatrix2x2;
// matrices 3x3
function DetMat3x3(const M: TMatrix3x3): double;
function InvMat3x3(const M: TMatrix3x3): TMatrix3x3;
function ProduitMat3x3(const A, B: TMatrix3x3): TMatrix3x3;
function DeltaKronecker(const i, j: integer): Extended;

function LoadIdentity3x3: TMatrix3x3;
function AddMatrix3x3(const A, B: TMatrix3x3): TMatrix3x3;
function SubstractMatrix3x3(const A, B: TMatrix3x3): TMatrix3x3;
function MultMatrix3x3ByScalar(const A: TMatrix3x3; const k: Extended): TMatrix3x3;
function MaxNormeMatrix3x3(const M: TMatrix3x3): Extended;
function makeMatrix3x3ByValues(const V11, V12, V13, V21, V22, V23, V31, V32, V33: double): TMatrix3x3;
function TransposeMatrix3x3(const A: TMatrix3x3): TMatrix3x3;
function MaxDiffMatrix(const A, B: TMatrix3x3): double;
function makeMatrix3x3Nulle: TMatrix3x3;
function IsSameMatrix(const A, B: TMatrix3x3; const EPS: double): boolean;
// vecteurs
function makeVecteurNul: TPoint3Df;
function makeVecteurByValues(const X, Y, Z: double): TPoint3Df;
// norme euclidienne
function NormeVecteur(const V: TPoint3Df): Extended;
// normalise le vecteur tq |V| = 1
function NormaliseVecteur(const V: TPoint3Df): TPoint3Df;

function AddVecteurs(const V: array of TPoint3Df): TPoint3Df; // fonction variadique
function AddTwoVecteurs(const V1, V2: TPoint3Df): TPoint3Df;

function SubstractVecteurs(const V1, V2: TPoint3Df): TPoint3Df;
function MultVectorByScalar(const V : TPoint3Df; const k: double): TPoint3Df;

function ProduitKroneckerVecteurs(const A, B: TPoint3Df): TMatrix3x3;  // produit tensoriel de deux vecteurs
function TripleProduitScalaire(const a, b, c: TPoint3Df): Extended;

function ProduitScalaire(const Vect1, Vect2: TPoint3Df): double;

function ProduitVectoriel(const Vect1, Vect2: TPoint3Df;
                          const Normalized: Boolean):TPoint3Df;
function TurnX(const V: TPoint3Df; const Angle: double): TPoint3Df;
function AngleBetweenVectors(const a, b: TPoint3Df): Extended;
function MultMat3x3ByVector(const M: TMatrix3x3; const V: TPoint3Df): TPoint3Df;
function MaxDiffVector(const a, b: TPoint3Df): double;
// Affichages
procedure AfficherVecteur(const Nom: string; const V: TPoint3Df);
procedure AfficherMatrix3x3(const Nom: string; const M: TMatrix3x3);


implementation
uses Common; // pour AfficherMessage

operator + (const A, B: TPoint3Df)R: TPoint3Df;
begin
  R.X := A.X + B.X;
  R.Y := A.Y + B.Y;
  R.Z := A.Z + B.Z;
end;

operator - (const A, B: TPoint3Df)R: TPoint3Df;
begin
  R.X := A.X - B.X;
  R.Y := A.Y - B.Y;
  R.Z := A.Z - B.Z;
end;

operator * (const A: TPoint3Df; const k: double)R: TPoint3Df;
begin
  R.X := A.X * k;
  R.Y := A.Y * k;
  R.Z := A.Z * k;
end;

operator ** (const A, B: TPoint3Df) R : double;
begin
  R := A.X * B.X + A.Y * B.Y + A.Z * B.Z;
end;

operator / (const A, B: TPoint3Df) R: TPoint3Df;
begin
  R := ProduitVectoriel(A, B, False);
end;

operator >< (const A, B: TPoint3Df) R: TMatrix3x3;
begin
  R := ProduitKroneckerVecteurs(A, B);
end;

//------------------------------------------------------------------------------
operator + (const A, B: TMatrix3x3): TMatrix3x3;    // produit matriciel
var
  i, j: Integer;
begin
  for i := 1 to 3 do
     for j := 1 to 3 do
       R[i, j] := A[i, j] + B[i, j];
end;

operator - (const A, B: TMatrix3x3) R: TMatrix3x3;
var
  i, j: Integer;
begin
  for i := 1 to 3 do
     for j := 1 to 3 do
       R[i, j] := A[i, j] - B[i, j];

end;

operator * (const A: TMatrix3x3; const k: double) R: TMatrix3x3;
var
  i, j: Integer;
begin
  for i := 1 to 3 do
     for j := 1 to 3 do
       R[i, j] := A[i, j] * k;
end;

operator ** (const A, B: TMatrix3x3): TMatrix3x3;
var
  i, j, k: integer;
begin
  for i := 1 to 3 do
     for j := 1 to 3 do
        begin
          R[i,j] := 0.00;
          for k := 1 to 3 do
             R[i,j] += A[i, k] * B[k, j];
        end;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// matrices 2x2
function LoadIdentity2x2: TMatrix2x2;
var i,j: integer;
begin
  for i := 1 to 2 do
     for j := 1 to 2 do
        Result[i, j] := DeltaKronecker(i, j);
end;
function ProduitMat2x2(const A, B: TMatrix2x2): TMatrix2x2;
var
  i, j, k: integer;
begin
  for i := 1 to 2 do
     for j := 1 to 2 do
        begin
          Result[i,j] := 0.00;
          for k := 1 to 2 do
             Result[i,j] += A[i, k] * B[k, j];
        end;
end;
function DetMat2x2(const M: TMatrix2x2): double;
begin
  result := M[1,1] * M[2,2] - M[1,2] * M[2,1]
end;
function InvMat2x2(const M: TMatrix2x2): TMatrix2x2;
var
  ted: Extended;
begin
  ted := 1 / DetMat2x2(M);
  Result[1,1] :=  M[2,2] * ted;        Result[2,1] := -M[1,2] * ted;
  Result[1,2] := -M[2,1] * ted;        Result[2,2] :=  M[2,2] * ted;
end;
// matrices 3x3
function DetMat3x3(const M: TMatrix3x3): double;
begin
  result :=  M[1, 1] * (M[3, 3] * M[2, 2] - M[3, 2] * M[2, 3]) +      // a11(a33a22-a32a23)
            -M[2, 1] * (M[3, 3] * M[1, 2] - M[3, 2] * M[1, 3]) +      //-a21(a33a12-a32a13)
             M[3, 1] * (M[2, 3] * M[1, 2] - M[2, 2] * M[1, 3]);       //+a31(a23a12-a22a13)

end;
function InvMat3x3(const M: TMatrix3x3): TMatrix3x3;
var
  i, j: integer;
  ted: Extended;
begin
  ted := 1 / DetMat3x3(M);
  Result[1,1] :=  (M[3,3] * M[2,2] - M[3,2] * M[2,3]);   //   a33a22-a32a23
  Result[2,1] := -(M[3,3] * M[2,1] - M[3,1] * M[2,3]);   // -(a33a21-a31a23)
  Result[3,1] :=  (M[3,2] * M[2,1] - M[3,1] * M[2,2]);   //   a32a21-a31a22

  Result[1,2] := -(M[3,3] * M[1,2] - M[3,2] * M[1,3]);   //  -(a33a12-a32a13)
  Result[2,2] :=  (M[3,3] * M[1,1] - M[3,1] * M[1,3]);   //    a33a11-a31a13
  Result[3,2] := -(M[3,2] * M[1,1] - M[3,1] * M[1,2]);   //  -(a32a11-a31a12)

  Result[1,3] :=  (M[2,3] * M[1,2] - M[2,2] * M[1,3]);   //        a23a12-a22a13  |
  Result[2,3] := -(M[2,3] * M[1,1] - M[2,1] * M[1,3]);   //        -(a23a11-a21a13) |
  Result[3,3] :=  (M[2,2] * M[1,1] - M[2,1] * M[1,2]);   //         a22a11-a21a12  |
  // division par ted
  for i := 1 to 3 do
    for j := 1 to 3 do
      Result[i,j] *= ted;

end;

function ProduitMat3x3(const A, B: TMatrix3x3): TMatrix3x3;
var
  i, j, k: integer;
begin
  for i := 1 to 3 do
     for j := 1 to 3 do
        begin
          Result[i,j] := 0.00;
          for k := 1 to 3 do
             Result[i,j] += A[i, k] * B[k, j];
        end;
end;
function DeltaKronecker(const i, j: integer): Extended;
begin
  if (i = j) then  result := 1.0 else result := 0.0;
end;


function LoadIdentity3x3: TMatrix3x3;
var i,j: integer;
begin
  for i := 1 to 3 do
     for j := 1 to 3 do
        Result[i, j] := DeltaKronecker(i, j);
end;
// somme de deux matrices
function AddMatrix3x3(const A, B: TMatrix3x3): TMatrix3x3;
var
  i, j: Integer;
begin
  for i := 1 to 3 do
     for j := 1 to 3 do
       Result[i, j] := A[i, j] + B[i, j];
end;
// différence  de deux matrices
function SubstractMatrix3x3(const A, B: TMatrix3x3): TMatrix3x3;
var
  i, j: Integer;
begin
  for i := 1 to 3 do
     for j := 1 to 3 do
       Result[i, j] := A[i, j] - B[i, j];
end;
// produit par un scalaire
function MultMatrix3x3ByScalar(const A: TMatrix3x3; const k: Extended): TMatrix3x3;
var
  i, j: Integer;
begin
  for i := 1 to 3 do
     for j := 1 to 3 do
       Result[i, j] := A[i, j] * k;
end;
// transposée
function TransposeMatrix3x3(const A: TMatrix3x3): TMatrix3x3;
var
  i, j: Integer;
begin
  for i := 1 to 3 do
     for j := 1 to 3 do
       Result[i, j] := A[j, i];
end;
function MultMat3x3ByVector(const M: TMatrix3x3; const V: TPoint3Df): TPoint3Df;
begin
  Result := makeVecteurByValues(M[1, 1] * V.X + M[1, 2] * V.Y + M[1, 3] * V.Z,
                                M[2, 1] * V.X + M[2, 2] * V.Y + M[2, 3] * V.Z,
                                M[3, 1] * V.X + M[3, 2] * V.Y + M[3, 3] * V.Z);
end;
// produit de Kronecker de deux vecteurs
function ProduitKroneckerVecteurs(const A, B: TPoint3Df): TMatrix3x3;
begin
  Result[1, 1] := A.X * B.X;    Result[1, 2] := A.X * B.Y;   Result[1, 3] := A.X * B.Z;
  Result[2, 1] := A.Y * B.X;    Result[2, 2] := A.Y * B.Y;   Result[2, 3] := A.Y * B.Z;
  Result[3, 1] := A.Z * B.X;    Result[3, 2] := A.Z * B.Y;   Result[3, 3] := A.Z * B.Z;
end;

// valeur du plus haut terme de la matrice M
function MaxNormeMatrix3x3(const M: TMatrix3x3): Extended;
var
  i, j: Integer;
begin
  Result := -MaxDouble;
  for i := 1 to 3 do
     for j := 1 to 3 do
       if (M[i, j] > Result) then Result := M[i, j];
end;
function IsSameMatrix(const A, B: TMatrix3x3; const EPS: double): boolean;
var
  i, j: Integer;
begin
  Result := True;
  for i := 1 to 3 do
     for j := 1 to 3 do
       Result := Result AND (SameValue(A[i,j], B[i,j], EPS));
end;


// construit matrice deuis valeurs
function makeMatrix3x3ByValues(const V11, V12, V13, V21, V22, V23, V31, V32, V33: double): TMatrix3x3;
begin
  Result[1,1] := V11; Result[1,2] := V12; Result[1,3] := V13;
  Result[2,1] := V21; Result[2,2] := V22; Result[2,3] := V23;
  Result[3,1] := V31; Result[3,2] := V32; Result[3,3] := V33;
end;
function makeMatrix3x3Nulle: TMatrix3x3;
var
  i, j: Integer;
begin
  for i := 1 to 3 do
     for j := 1 to 3 do
       Result[i, j] := 0.00;
end;

// rotations
function makeMatRotationX(const Angle: Extended): TMatrix3x3;
var
  cosa, sina: Extended;
begin
  sincos(Angle, sina, cosa);
  Result := makeMatrix3x3ByValues(1, 0   , 0,
                                  0, cosa, -sina,
                                  0, sina, cosa);

end;
function makeMatRotationY(const Angle: Extended): TMatrix3x3;
var
  cosa, sina: Extended;
begin
  sincos(Angle, sina, cosa);
  Result := makeMatrix3x3ByValues(cosa , 0, sina,
                                  0    , 1, 0,
                                  -sina, 0, cosa);

end;
function makeMatRotationZ(const Angle: Extended): TMatrix3x3;
var
  cosa, sina: Extended;
begin
  sincos(Angle, sina, cosa);
  Result := makeMatrix3x3ByValues(cosa, -sina, 0,
                                  sina,  cosa, 0,
                                  0   ,  0   , 1);

end;
//***********************************
// VECTEURS

// produit scalaire
function ProduitScalaire(const Vect1, Vect2: TPoint3Df): double;
begin
  Result := Vect1.X * Vect2.X + Vect1.Y * Vect2.Y + Vect1.Z * Vect2.Z;
end;
// produit vectoriel éventuellement normalisé
function ProduitVectoriel(const Vect1, Vect2: TPoint3Df;
                          const Normalized: Boolean):TPoint3Df;
var
  v: TPoint3Df;
  r: Extended;
begin
  v.X:=Vect1.Y*Vect2.Z-Vect1.Z*Vect2.Y;
  v.Y:=Vect1.Z*Vect2.X-Vect1.X*Vect2.Z;
  v.Z:=Vect1.X*Vect2.Y-Vect1.Y*Vect2.X;
  if Normalized then
  begin
    r:=sqrt(Sqr(v.x)+sqr(v.y)+sqr(v.z))+1e-12;
    v.X:=v.x/r;
    v.y:=v.y/r;
    v.z:=v.z/r;
  end;
  Result:=v;
end;
// rotation rapide
function TurnX(const V: TPoint3Df; const Angle: double): TPoint3Df;
var
  s, c: double;
begin
  sincos(Angle, s, c);
  Result.X := V.X;
  Result.Y := c * V.Y - s * V.Z;
  Result.Z := s * V.Y + c * V.Z;
end;


// vecteur nul
function makeVecteurNul: TPoint3Df;
begin
  Result.X := 0.00;
  Result.Y := 0.00;
  Result.Z := 0.00;
end;
function makeVecteurByValues(const X, Y, Z: double): TPoint3Df;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

// ajout de vecteurs
function AddVecteurs(const V: array of TPoint3Df): TPoint3Df; // fonction variadique
var
  i: Integer;
begin
  result := makeVecteurNul;
  for i := 0 to High(V) do
  begin
    Result.X += V[i].X;
    Result.Y += V[i].Y;
    Result.Z += V[i].Z;
  end;
end;

function AddTwoVecteurs(const V1, V2: TPoint3Df): TPoint3Df;
begin
  Result.X := V1.X + V2.X;
  Result.Y := V1.Y + V2.Y;
  Result.Z := V1.Z + V2.Z;
end;

// produit triple: (A x B) . C
function TripleProduitScalaire(const a, b, c: TPoint3Df): Extended;
var
  avb: TPoint3Df;
begin
  avb := ProduitVectoriel(a, b, False);
  Result := ProduitScalaire(avb, c);
end;
// norme euclidienne
function NormeVecteur(const V: TPoint3Df): Extended;
begin
  Result := sqrt(V.X*V.X + V.Y*V.Y + V.Z*V.Z);
end;
// normalise le vecteur tq |V| = 1
function NormaliseVecteur(const V: TPoint3Df): TPoint3Df;
var
  r: Extended;
begin

  r := NormeVecteur(V);
  Result.X := V.X / r;
  Result.Y := V.Y / r;
  Result.Z := V.Z / r;
end;
// angle de deux vecteurs (en radians)
function AngleBetweenVectors(const a, b: TPoint3Df): Extended;
var
  V1, V2: TPoint3Df;
begin
  V1 := NormaliseVecteur(a);
  V2 := NormaliseVecteur(b);
  Result := arccos(ProduitScalaire(V1, V2));
end;

//  Soustraction de deux vecteurs: V1 - V2
function SubstractVecteurs(const V1, V2: TPoint3Df): TPoint3Df;
begin
  result.X := V1.X - V2.X;
  result.Y := V1.Y - V2.Y;
  result.Z := V1.Z - V2.Z;
end;

function MultVectorByScalar(const V : TPoint3Df; const k: double): TPoint3Df;
begin
  Result.X := V.X * k;
  Result.Y := V.Y * k;
  Result.Z := V.Z * k;
end;
// Retourne la différence maxi terme à terme des vecteurs a et b
function MaxDiffVector(const a, b: TPoint3Df): double;
begin
  result := Max(abs(a.X - b.X),
                max(abs(a.Y - b.Y),
                    abs(a.Z - b.Z))
               );
end;

// Retourne la différence maxi terme à terme des matrices 3x3 a et b
function MaxDiffMatrix(const A, B: TMatrix3x3): double;
var
  qMax: double;
  EWE : double;
  i, j: Integer;
begin
  qMax := -1e100;
  for i := 1 to 3 do
    for j := 1 to 3 do
    begin
      EWE :=  abs(A[i, j] - B[i, j]);
      if (EWE > qMax) then qMax := EWE;
    end;
  Result := EWE;
end;
//------------------------------------------------------------------------------

// Affichages
procedure AfficherVecteur(const Nom: string; const V: TPoint3Df);
begin
  AfficherMessage(Format('Vecteur : %s = [%.8f, %.8f, %.8f]', [Nom, V.X, V.Y, V.Z]));
end;
procedure AfficherMatrix3x3(const Nom: string; const M: TMatrix3x3);
var
  i: Integer;
begin
  AfficherMessage(Format('Matrice : %s', [Nom]));
  for i := 1 to 3 do
    AfficherMessage(Format('%.8f, %.8f, %.8f', [M[i, 1], M[i, 2], M[i, 3]]));
end;
end.
