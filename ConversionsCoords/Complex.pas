unit Complex;

interface

type TComplex = record
  Re: double;
  Im: double;
end;

function VarComplexCreate(const R, I: double): TComplex;


implementation
function VarComplexCreate(const R, I: double): TComplex;
begin
  Result.Re := R;
  Result.Im := I;
end;

end.
