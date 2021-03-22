unit unitUtilsComposants;
// Opérations sur des contrôles (listbox, etc...)
// 06/02/2014: Remplacement de l'évaluateur d'expressions
// 25/09/2018: DrawPipistrelle est factorisé
// 11/10/2018: Pas mal de factorisations et corrections
{$INCLUDE CompilationParameters.inc}
{$ERROR Ce fichier n'a rien à faire ici}
interface
uses
  {$INCLUDE SelectionLangues.inc} // insère les unités en fonction de la langue
  StructuresDonnees,
  Common,
  math,
  StdCtrls, Grids, CheckLst,
  Classes, SysUtils, Graphics,
  Forms, Dialogs,
  ConvertisseurJPC,
