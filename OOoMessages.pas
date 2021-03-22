unit OOoMessages;

interface

const   { ces messages peuvent être traduits dans un autre langage }

  { OOoTools unit }
  OOo_serviceKO = 'Impossible de créer le service : %s';
  OOo_connectKO = 'Connexion OpenOffice impossible';
  OOo_structureKO = 'Nom de structure inconnu : %s';
  OOo_inspectionKO = 'Cet objet ne peut pas être inspecté';
  OOo_nbrArgsKO = 'Nombre d''arguments incorrect';
  OOo_notString = 'L''argument de rang %d (à partir de 0) devrait être un String';
  OOo_convertToURLKO = 'ConvertToURL impossible';
  OOo_convertFromURLKO = 'ConvertFromURL impossible';


  { OOoXray units }
  XrayMess10 = '- Propriétés -';
  XrayMess10T = '- Propriétés triées -';
  XrayMess13 = '- Remarques -';
  XrayMess20 = '- Méthodes -';
  XrayMess20T = '- Méthodes triées -';
  XrayMess21 = '- Arguments -';
  XrayMess22 = '- Type renvoyé -';
  XrayMess23 = '- Interface -';
  XrayMess30 = '- Services supportés, triés -';
  XrayMess31 = '- Services disponibles, triés -';
  XrayMess32 = '- Interfaces supportées, triées -';
  XrayMess40 = '*** objet sans nom ***';
  XrayMess61 = '???';
  XrayMess62 = 'Structure :  ';
  XrayMess70 = 'Xray impossible car la méthode nécessite des arguments';
  XrayMess71 = 'Cette méthode ne renvoie rien';
  XrayMess72 = 'Limitation du pont COM : %s est inaccessible par Xray';
  XrayMess74 = 'Cette propriété ne peut être lue, on peut seulement l''écrire !';
  XrayMess80 = 'Désolé, il n''y a pas de page dans le SDK pour ceci.';
  XrayMess81 = 'Désolé, cette pseudo-propriété est non documentée';
  XrayMess82 = 'Pseudo-propriété, affichage de : %s';
  XrayMess83 = 'Il existe plusieurs pages sur : %s';
  XrayMess84 = 'SDK address is incorrect.'#13'Please modify constant SDKaddr in OOoXray.pas';
  XrayMess85 = 'L''adresse du SDK est incorrecte.'#13'Veuillez modifier la constante SDKaddr dans OOoXray.pas';
  XrayMess86 = 'Cette propriété n''est pas documentée dans les services supportés';
  XrayMess87 = 'La documentation affichée provient d''autres services';
  XrayMess88 = 'Le contenu de cette fenêtre Xray est sauvé';
  XrayMvalue = 'Valeur = ';
  XrayMzeroString = 'Chaîne de longueur zéro';
  XrayMcolType = '- Type -';
  XrayMcolValue = '- Valeur -';


  { Unit1 unit }
  OOoMess001 = 'Connecté à OpenOffice';
  OOoMess002 = 'Déconnecté d''OpenOffice';


  { OOoExamples unit }
  OOoMess105 = 'Le document va fermer';
  OOoMess107 = 'La table n''est pas triée';
  OOoMess108 = 'La table est maintenant triée !';
  OOoMess109 = 'Veuillez remplir la zone ';
  OOoMess111 = 'Bonjour à tous';
  OOoMess112 = 'écrit avec ';
  OOoMess113 = 'OpenOffice.org ';



implementation

end.
