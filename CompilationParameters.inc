// GHCaveDraw Configuration file
// 08/07/2013
{$mode delphi}{$H+}
// langue
{$DEFINE LANGUAGE_FRENCH}
//------------------------------------------------------------------------------
// Plateformes:
//-------------
// Les variables MSWINDOWS, LINUX et MACOS sont prédéfinies
// Il est inutile de les respécifier


// Pour MacOSX
{$DEFINE MAC_OS_X}
{$UNDEF MAC_OS_X}

// Les IDStation sont des Int64 ou du texte
// Le mode ID littéral fonctionne bien mais le MétaFiltre est désactivé
{$DEFINE TIDBASEPOINT_AS_TEXT}
{$UNDEF  TIDBASEPOINT_AS_TEXT}



// config gestion des strings
{$codepage UTF8}

// DXF support
{$DEFINE DXF_SUPPORT}
{.$UNDEF DXF_SUPPORT}

// Outils OpenCaveMap
{$DEFINE OPENCAVEMAP}
{.$DEFINE OPENCAVEMAP}

{$DEFINE OPENCAVEMAP_ADMIN}
{$UNDEF OPENCAVEMAP_ADMIN}





