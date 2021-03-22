unit UnitStructuresDonneesEntites;

{$INCLUDE CompilationParameters.inc}

interface
// pour MetaFiltre
type
  TCompOperateur = integer;

type
  TConnectorFiltres = (ftERROR, ftAND, ftOR);

type
  TTypeFiltre = (ftINVALID, ftSCALAR, ftINTERVAL); //, ftFLAGS);

type
  TFiltre = record
    Caption: string;
    TypeFiltre: TTypeFiltre;
    Filter: integer;
    Operateur: TCompOperateur;
    Basculed: boolean;
    Valeur: string;
    BorneInf: string;
    BorneSup: string;
    Flags: string;
    ConnectedWithPrevious: TConnectorFiltres;
  end;

implementation

end.

