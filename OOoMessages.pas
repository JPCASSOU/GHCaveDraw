unit OOoMessages;

interface

const   { ces messages peuvent �tre traduits dans un autre langage }

  { OOoTools unit }
  OOo_serviceKO = 'Impossible de cr�er le service : %s';
  OOo_connectKO = 'Connexion OpenOffice impossible';
  OOo_structureKO = 'Nom de structure inconnu : %s';
  OOo_inspectionKO = 'Cet objet ne peut pas �tre inspect�';
  OOo_nbrArgsKO = 'Nombre d''arguments incorrect';
  OOo_notString = 'L''argument de rang %d (� partir de 0) devrait �tre un String';
  OOo_convertToURLKO = 'ConvertToURL impossible';
  OOo_convertFromURLKO = 'ConvertFromURL impossible';


  { OOoXray units }
  XrayMess10 = '- Propri�t�s -';
  XrayMess10T = '- Propri�t�s tri�es -';
  XrayMess13 = '- Remarques -';
  XrayMess20 = '- M�thodes -';
  XrayMess20T = '- M�thodes tri�es -';
  XrayMess21 = '- Arguments -';
  XrayMess22 = '- Type renvoy� -';
  XrayMess23 = '- Interface -';
  XrayMess30 = '- Services support�s, tri�s -';
  XrayMess31 = '- Services disponibles, tri�s -';
  XrayMess32 = '- Interfaces support�es, tri�es -';
  XrayMess40 = '*** objet sans nom ***';
  XrayMess61 = '???';
  XrayMess62 = 'Structure :  ';
  XrayMess70 = 'Xray impossible car la m�thode n�cessite des arguments';
  XrayMess71 = 'Cette m�thode ne renvoie rien';
  XrayMess72 = 'Limitation du pont COM : %s est inaccessible par Xray';
  XrayMess74 = 'Cette propri�t� ne peut �tre lue, on peut seulement l''�crire !';
  XrayMess80 = 'D�sol�, il n''y a pas de page dans le SDK pour ceci.';
  XrayMess81 = 'D�sol�, cette pseudo-propri�t� est non document�e';
  XrayMess82 = 'Pseudo-propri�t�, affichage de : %s';
  XrayMess83 = 'Il existe plusieurs pages sur : %s';
  XrayMess84 = 'SDK address is incorrect.'#13'Please modify constant SDKaddr in OOoXray.pas';
  XrayMess85 = 'L''adresse du SDK est incorrecte.'#13'Veuillez modifier la constante SDKaddr dans OOoXray.pas';
  XrayMess86 = 'Cette propri�t� n''est pas document�e dans les services support�s';
  XrayMess87 = 'La documentation affich�e provient d''autres services';
  XrayMess88 = 'Le contenu de cette fen�tre Xray est sauv�';
  XrayMvalue = 'Valeur = ';
  XrayMzeroString = 'Cha�ne de longueur z�ro';
  XrayMcolType = '- Type -';
  XrayMcolValue = '- Valeur -';


  { Unit1 unit }
  OOoMess001 = 'Connect� � OpenOffice';
  OOoMess002 = 'D�connect� d''OpenOffice';


  { OOoExamples unit }
  OOoMess105 = 'Le document va fermer';
  OOoMess107 = 'La table n''est pas tri�e';
  OOoMess108 = 'La table est maintenant tri�e !';
  OOoMess109 = 'Veuillez remplir la zone ';
  OOoMess111 = 'Bonjour � tous';
  OOoMess112 = '�crit avec ';
  OOoMess113 = 'OpenOffice.org ';



implementation

end.
