unit unitUtilsCPU;

{$INCLUDE CompilationParameters.inc}

interface

Uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils ;

    // Objet de gestion des affinités processeur.
 type

    { TCPU }

    TCPU = Class

    Protected
      FCPUMask  : Array[0..31] Of Boolean ;    // Mapping physique des processeurs autorisés pour le processus courant.
      FCPUMap   : Array[0..31] Of Cardinal ;   // Mapping logique des processeurs, contient un masque d'affinité.
      FCPUCount : Cardinal ;                   // Nombre de processeurs utilisables, conditionne la limite supérieure de CPUMap.

      // Accesseur "Count".
      Function GetCPUCount : Cardinal ;

    Public
      // Constructeur
      Constructor Create ;

      // Récupération du nombre de CPU / coeurs du système.
      // Les processeurs sont ensuite numérotés de 0 à (Count-1).
      Property Count : Cardinal Read GetCPUCount ;

      // Force le thread courant à basculer sur le processeur passé en paramètre.
      // Lève une exception en cas d'erreur.
      // Ne lève PAS d'erreur (mais ne fait rien non plus) en cas de numéro de CPU invalide.
      Procedure SwitchTo ( Const CPUIndex : Cardinal ) ;

    End;




implementation

{$IFDEF LINUX}

{ TCPU }

function TCPU.GetCPUCount: Cardinal;
begin

end;

constructor TCPU.Create;
begin

end;

procedure TCPU.SwitchTo(const CPUIndex: Cardinal);
begin

end;

{$ENDIF}
{$IFDEF MSWINDOWS}
{ TCPU }

constructor TCPU.Create;
begin
     // Initialisation des structures d'affinité processeur.
     FCPUCount:=0;
     FillChar(FCPUMask,SizeOf(FCPUMask),0);
     FillChar(FCPUMap,SizeOf(FCPUMap),0);
     // Calcul direct du nombre de CPU.
     GetCPUCount;
end;

Function TCPU.GetCPUCount : Cardinal ;
Var
   I, ProcessMask, SystemMask : Cardinal ;
Begin
     // Inutile de tout déclencher si on a déjà le nombre de CPU...
     If (FCPUCount=0) Then
        Begin
        // On va récupérer le nombre de CPU.
        FCPUCount:=0 ;
        // On compte les bits d'affinité.
        Win32Check(GetProcessAffinityMask(GetCurrentProcess,ProcessMask,SystemMask));
        // Bon, on a le masque : on va calculer sa taille.
        For I:=0 To 31 Do
            Begin
            // Explosage du masque binaire vers un tableau de booléens.
            FCPUMask[I]:=(ProcessMask And (1 Shl I))<>0 ;
            // On compte les CPU utilisables, et on crée la map d'assignation.
            // Le contenu de CPUMap, à l'indice donné, est donc le masque d'affinité à utiliser.
            If FCPUMask[I] Then
               Begin
               FCPUMap[FCPUCount]:=(1 Shl I) ;
               Inc(FCPUCount);
               End;
            End;
        End;
     // Le nombre de CPU est correct quel que soit le cas, on renvoie donc le résultat.
     Result:=FCPUCount ;
End;

Procedure TCPU.SwitchTo ( Const CPUIndex : Cardinal ) ;
Begin
     If (CPUIndex<FCPUCount) Then
        If (SetThreadAffinityMask(GetCurrentThread,FCPUMap[CPUIndex])=0) Then
           RaiseLastOSError ;
End;
{$ENDIF}

end.

