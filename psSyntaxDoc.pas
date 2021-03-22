{-----------------------------------------------------------------------------
  psSyntaxDoc.pas

  written by Precision software & consulting
            copyright ©  2008 - 2011
            Email : info@be-precision.com
            Web : http://www.be-precision.com

  Purpose: Documentation generator for RemObjects PascalScript engine.
           Supports various documentation modes, from a simple syntax overview,
           to the SynEdit code-completion templates and XML output.
           By using this generator, you can provide your developers
           and end-users with a full syntax description of your scripting solution,
           that is based on RemObjects PascalScript.

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  This unit can be freely used in any application. 
------------------------------------------------------------------------------}

unit psSyntaxDoc;

interface

uses
  Classes, SysUtils, uPSCompiler, uPSUtils;

type
  TPSSyntaxDocMode = (sdmIdentifiersOnly, sdmSimple, sdmAdvanced, sdmSynEdit, sdmXml);
  TPSSyntaxDocOption = (sdoIncludeAncestorElements, sdoIncludeChildElements, sdoShortDeclarationsOnly);
  TPSSyntaxDocOptions = set of TPSSyntaxDocOption;

function MakePSSyntaxDoc(aCompiler: TPSPascalCompiler; var SyntaxDoc:string;
  _SyntaxDocMode: TPSSyntaxDocMode = sdmSynEdit; _SyntaxDocOptions: TPSSyntaxDocOptions = [sdoIncludeAncestorElements, sdoIncludeChildElements]):Boolean;

implementation

type
  TPSSyntaxDocCompiler = class(TPSPascalCompiler);

function MakePSSyntaxDoc(aCompiler: TPSPascalCompiler; var SyntaxDoc:string;
  _SyntaxDocMode: TPSSyntaxDocMode = sdmSynEdit; _SyntaxDocOptions: TPSSyntaxDocOptions = [sdoIncludeAncestorElements, sdoIncludeChildElements]):Boolean;

type
  TPSSyntaxDocItemKind = (sdikKeyword, sdikConst, sdikType, sdikPtrVariable, sdikVariable, sdikProcedure, sdikFunction, sdikClass, sdikProperty,
      sdikConstructor, sdikFncMethod, sdikProcMethod, sdikAttribute, sdikField);
  PPSSyntaxDocItem = ^TPSSyntaxDocItem;
  TPSSyntaxDocItem = record
    Parent:PPSSyntaxDocItem;
    Ancestor:PPSSyntaxDocItem;
    Kind:TPSSyntaxDocItemKind;
    Identifier:string;
    Declaration:string;
    ShortDecl:string;
    _AncestorIdent:string;
  end;
  TPSSyntaxDocItemsArray = array of PPSSyntaxDocItem;

const
  _unspec = '...';
  _postfix = ';';
  _SyntaxDocItemKindPrefix:array[TPSSyntaxDocItemKind] of string = (
      'keyword', 'const', 'type', 'ptrvar', 'var', 'procedure' , 'function', 'class', 'property', 'constructor', 'methodfnc', 'methodproc', 'attribute', 'field');
  _keywords: array[0.. 49] of string = (
      'and','array','as','begin','case','chr','const','div','do','downto','else','end','except','export','finally','for','function',
      'goto','if','in','is','label','mod','nil','not','of','on','or','ord','out','packed','procedure','program','raise','record',
      'repeat','set','shl','shr','then','to','try','type','unit','until','uses','var','while','with','xor'
      );
      //'class','constructor','destructor','external','finalization','forward','implementation','inherited','initialization','interface',
      //'override','private','property','protected','public','published', 'virtual'
  _XmlGroupNames:array[TPSSyntaxDocItemKind] of string = (
      'keywords', 'consts', 'types', 'variables', 'objvariables', 'procedures' , 'functions', 'classes', '', '', '', '', 'attributes', '');
  _XmlItemKindPrefix:array[TPSSyntaxDocItemKind] of string = (
      'keyword', 'const', 'type', 'var', 'var', 'procedure' , 'function', 'class', 'property', 'constructor', 'function', 'procedure', 'attribute', 'field');

var
  Comp:TPSSyntaxDocCompiler;
  sdList:TStringList;
  SDI,SDICh,SDIA:PPSSyntaxDocItem;
  SDIAp:TPSSyntaxDocItemsArray;
  i,m,n:Integer;
  aname,tmpLn,tmpDoc:string;
  C:TPSConstant;
  TT:TPSType;
  TTR:TPSRecordFieldTypeDef;
  TTD:TPSParametersDecl;
  TTP:TPSParameterDecl;
  TRP:TPSRegProc;
  TC:TPSCompileTimeClass;
  TV:TPSVar;
  TA:TPSAttributeType;
  TPP:TPSProcedure;
  DI:TPSDelphiClassItem;
  xmlGroups:array[TPSSyntaxDocItemKind] of string;
  xmlIK:TPSSyntaxDocItemKind;
  xmlBool:Boolean;
  xmlDecl,xmlAnc:string;

  function _GetIdentName(const IdentName:string; const OrigName:string=''; CheckExclam:Boolean=False):string;
  begin
    if Length(OrigName)>0 then
      Result:=OrigName
    else
      Result:=IdentName;
    if CheckExclam and (Length(Result)>0) and (Result[1]='!') then
    begin
      if Result='!OPENARRAYOFCONST' then Result:='array of const'
      else if Result='!OPENARRAYOFU8' then Result:='array of Byte'
      else if Result='!OPENARRAYOFS8' then Result:='array of ShortInt'
      else if Result='!OPENARRAYOFU16' then Result:='array of SmallInt'
      else if Result='!OPENARRAYOFS16' then Result:='array of Word'
      else if Result='!OPENARRAYOFU32' then Result:='array of Cardinal'
      else if Result='!OPENARRAYOFS32' then Result:='array of Longint'
      else if Result='!OPENARRAYOFSINGLE' then Result:='array of Single'
      else if Result='!OPENARRAYOFDOUBLE' then Result:='array of Double'
      else if Result='!OPENARRAYOFEXTENDED' then Result:='array of Extended'
      else if Result='!OPENARRAYOFSTRING' then Result:='array of String'
      else if Result='!OPENARRAYOFPCHAR' then Result:={$IFDEF PS_PANSICHAR}'array of PAnsiChar'{$ELSE}'array of PChar'{$ENDIF}
      else if Result='!OPENARRAYOFVARIANT' then Result:='array of variant'
    {$IFNDEF PS_NOINT64}else if Result='!OPENARRAYOFS64' then Result:='array of Int64'{$ENDIF}
      else if Result='!OPENARRAYOFCHAR' then Result:='array of Char'
    {$IFNDEF PS_NOWIDESTRING}
      else if Result='!OPENARRAYOFWIDESTRING' then Result:='array of WideString'
      else if Result='!OPENARRAYOFUNICODESTRING' then Result:='array of UnicodeString'
      else if Result='!OPENARRAYOFWIDECHAR' then Result:='array of WideChar'
    {$ENDIF}
      else if Result='!OPENARRAYOFTOBJECT' then Result:='array of TObject'
      else if pos('!OPENARRAYOFRECORD_',Result)=1 then Result:='array of record ...';
    end;
  end;

  function _GetVariantValue(p: PIfRVariant):string;
  var
    du8: tbtu8;
    du16: tbtu16;
    wb:TBytes;
  begin
    case p.FType.BaseType of
      btType:Result:=p^.ttype.OriginalName;
      {$IFNDEF PS_NOWIDESTRING}
      btWideString:Result:=''''+tbtwidestring(p^.twidestring)+'''';
      btUnicodeString:Result:=''''+tbtUnicodeString(p^.twidestring)+'''';
      btWideChar:
        if (Ord(p^.twidechar)>=32) and (Ord(p^.twidechar)<=128) then
          Result:=''''+p^.twidechar+''''
        else
        begin
          wb:=WideBytesOf(p^.twidechar);
          Result:='#'+IntToStr(wb[0])+'#'+IntToStr(wb[1]);
        end;
      {$ENDIF}
      btSingle:Result:=FloatToStr(p^.tsingle);
      btDouble:Result:=FloatToStr(p^.tdouble);
      btExtended:Result:=FloatToStr(p^.textended);
      btCurrency:Result:=CurrToStr(p^.tcurrency);
      btChar:
        if (Ord(p^.tchar)>=32) and (Ord(p^.tchar)<=128) then
          Result:=''''+p^.tchar+''''
        else
          Result:='#'+IntToStr(BytesOf(p^.tchar)[0]);
      btSet:Result:=tbtString(p^.tstring);
      btString:Result:=''''+tbtString(p^.tstring)+'''';
      btenum:
          if TPSEnumType(p^.FType).HighValue <=256 then
          begin
            du8 := tbtu8(p^.tu32);
            Result:=IntToStr(du8);
          end
          else
          if TPSEnumType(p^.FType).HighValue <=65536 then
          begin
            du16 := tbtu16(p^.tu32);
            Result:=IntToStr(du16);
          end;
      bts8,btu8: Result:=IntToStr(p^.tu8);
      bts16,btu16: Result:=IntToStr(p^.tu16);
      bts32,btu32: Result:=IntToStr(p^.tu32);
      {$IFNDEF PS_NOINT64}
      bts64: Result:=IntToStr(p^.ts64);
      {$ENDIF}
    else
      Result:='';
    end;
  end;

  function _GetTypeDecl(T:TPSType;ShortDecl:Boolean=False):string;
  var
    j,k:Integer;
    efirst,elast:string;
    TP:TPSParameterDecl;
    TR:TPSRecordFieldTypeDef;
    CC:TPSConstant;
    BT:TPSType;
  begin
    Result:='';
    if T=nil then
      Exit;
    if T is TPSTypeLink then
    begin
      if not ShortDecl then
        Result := _GetIdentName(TPSTypeLink(T).LinkTypeNo.Name,TPSTypeLink(T).LinkTypeNo.OriginalName)
    end
    else
    if T is TPSUndefinedClassType then
    begin
      if ShortDecl then
        Result:='class'
      else
        Result:='class('+_unspec+')'
    end
    else
    if T is TPSVariantType then
    begin
      if not ShortDecl then
        Result:='Variant'
    end
    {$IFNDEF PS_NOINTERFACES}
    else
    if T is TPSInterfaceType then
    begin
      if ShortDecl or (TPSInterfaceType(T).Intf.InheritedFrom=nil) then
        Result:='interface'
      else
        Result:='interface(' + _GetIdentName(TPSInterfaceType(T).Intf.InheritedFrom.Name,
            TPSInterfaceType(T).Intf.InheritedFrom.aType.OriginalName) + ')'
        // Result:='interface(' + GUIDToString(TPSInterfaceType(T).Intf.Guid) + ')'
    end
    {$ENDIF}
    else
    if T is TPSEnumType then
    begin
      if ShortDecl then
      begin
        efirst:=''; elast:='';
        for j:=0 To Comp.FConstants.Count-1 do
        begin
          CC:=Comp.FConstants.Data[j];
          if CC.Value.FType=T then
          begin
            elast:=_GetIdentName(CC.Name,CC.OrgName);
            if Length(efirst)=0 then
              efirst:=elast;
          end;
        end;
        if Length(efirst)>0 then
          Result:= efirst + '..'+ elast;
      end
      else
      begin
        efirst:='';
        for j:=0 To Comp.FConstants.Count-1 do
        begin
          CC:=Comp.FConstants.Data[j];
          if CC.Value.FType=T then
            efirst:=efirst+', '+_GetIdentName(CC.Name, CC.OrgName);
        end;
        if Length(efirst)>0 then
        begin
          Delete(efirst,1,2);
          Result:='(' + efirst + ')';
        end;
      end;
    end
    else
    if T is TPSSetType then
    begin
      if not ShortDecl then
      begin
        if TPSSetType(T).SetType<>nil then
          Result:='set of ' +_GetIdentName(TPSSetType(T).SetType.Name,TPSSetType(T).SetType.OriginalName)
        else
          Result:='['+_unspec+']'
      end;
    end
    else
    if T is TPSArrayType then
    begin
      if not ShortDecl then
      begin
        if T is TPSStaticArrayType then
          efirst:='['+IntToStr(TPSStaticArrayType(T).StartOffset) + '..' + IntToStr(TPSStaticArrayType(T).StartOffset+TPSStaticArrayType(T).Length-1) + ']'
        else
          efirst:='';
        Result:='array' +efirst+ ' of ';
        if TPSArrayType(T).ArrayTypeNo<>nil then
        begin
          if Length(TPSArrayType(T).ArrayTypeNo.Name)>0 then
            Result:=Result+_GetIdentName(TPSArrayType(T).ArrayTypeNo.Name,TPSArrayType(T).ArrayTypeNo.OriginalName)
          else
            Result:=Result+_GetTypeDecl(TPSArrayType(T).ArrayTypeNo);
        end
        else
          Result:=Result + _unspec
      end;
    end
    else
    if T is TPSRecordType then
    begin
      Result:='record';
      if not ShortDecl then
      begin
        Result:=Result+' ';
        if TPSRecordType(T).RecValCount=0 then
          Result:=Result + _unspec+' '
        else
        begin
          k:=TPSRecordType(T).RecValCount;
          for j:=0 To k-1 do
          begin
            TR:=TPSRecordType(T).RecVal(j);
            Result:=Result+_GetIdentName(TR.FieldName,TR.FieldOrgName);
            if TR.aType<>nil then
            begin
              if Length(TR.aType.Name)>0 then
                Result:=Result + ': '+_GetIdentName(TR.aType.Name,TR.aType.OriginalName)
              else
                Result:=Result + ': '+_GetTypeDecl(TR.aType);
            end;
            Result:=Result + '; ';
          end;
        end;
        Result:=Result + 'end';
      end;
    end
    else
    if T is TPSProceduralType then
    begin
      if TPSProceduralType(T).ProcDef.Result=nil then
        Result:='procedure'
      else
        Result:='function';
      k:=TPSProceduralType(T).ProcDef.ParamCount;
      if k>0 then
        Result:=Result + '(';
      if not ShortDecl then
      begin
        for j:=0 To k-1 do
        begin
          TP:=TPSProceduralType(T).ProcDef.Params[j];
          case TP.Mode of
            pmOut:Result:=Result + 'out ';
            pmInOut:Result:=Result + 'var ';
          end;
          Result:=Result+_GetIdentName(TP.Name,TP.OrgName);
          if TP.aType<>nil then
          begin
            if Length(TP.aType.Name)>0 then
              Result:=Result + ': '+_GetIdentName(TP.aType.Name,TP.aType.OriginalName)
            else
              Result:=Result + ': '+_GetTypeDecl(TP.aType);
          end;
          if j<k-1 then
            Result:=Result + '; ';
        end;
      end;
      if k>0 then
        Result:=Result + ')';
      if TPSProceduralType(T).ProcDef.Result<>nil then
        Result:=Result + ': ' + _GetIdentName(TPSProceduralType(T).ProcDef.Result.Name,TPSProceduralType(T).ProcDef.Result.OriginalName);
    end
    else
    if T is TPSClassType then
    begin
      if not ShortDecl then
        Result:=_GetIdentName(TPSClassType(T).Name,TPSClassType(T).OriginalName)
    end
    else
    if T is TPSAttributeType then
    begin
      if not ShortDecl then
        Result:=_GetIdentName(TPSAttributeType(T).Name,TPSAttributeType(T).OriginalName)
    end
    else
    begin
      if not ShortDecl then
      begin
        case T.BaseType of
          btPointer:Result:='Pointer';
          {$IFDEF PS_PANSICHAR}
          btPChar:
            if T.Name='PWIDECHAR' then
              Result:='PWideChar'
            else
              Result:='PAnsiChar';
          {$ENDIF}        {$IFNDEF PS_NOWIDESTRING}
          btWideChar:Result:='WideChar';
        {$ENDIF}
        else
          begin
            BT:=Comp.FindBaseType(T.BaseType);
            if BT<>nil then
              Result:=_GetIdentName(BT.Name,BT.OriginalName);
            if Length(Result)=0 then
              Result:=_unspec;
          end;
        end;
        if AnsiSameText(T.Name,Result) then
          Result:='';
      end;
    end;
  end;

  function NewSDI(aKind:TPSSyntaxDocItemKind; const anIdent:string; const aDecl:string=''; const aShortDecl:string='';
      aParent:PPSSyntaxDocItem=nil; anAncestor:PPSSyntaxDocItem=nil):PPSSyntaxDocItem;
  begin
    new(Result);
    Result^.Parent:=aParent;
    Result^.Ancestor:=anAncestor;
    Result^.Kind:=aKind;
    Result^.Identifier:=anIdent;
    Result^.Declaration:=aDecl;
    Result^.ShortDecl:=aShortDecl;
  end;


  function FindSDI(sdik:TPSSyntaxDocItemKind; const anIdent:string; const aParents:TPSSyntaxDocItemsArray):PPSSyntaxDocItem;
  var
    x,xc:Integer;
    nap:Integer;
    xDI:PPSSyntaxDocItem;

    function _xDInA:Boolean;
    var
      nxi:Integer;
    begin
      Result:=False;
      if (nap=0) and (xDI.Parent=nil) then
        Result:=true
      else
      for nxi:=0 To nap-1 do
        if xDI.Parent=aParents[nxi] then
        begin
          Result:=True;
          break;
        end;
    end;

  begin
    Result:=nil;
    if sdList.Find(anIdent,xc) then
    begin
      nap:=Length(aParents);
      xDI:=PPSSyntaxDocItem(sdList.Objects[xc]);
      if (xDI.Kind=sdik) and _xDInA then
        Result:=xDI
      else
        for x:=xc+1 To sdList.Count-1 do
        begin
          xDI:=PPSSyntaxDocItem(sdList.Objects[x]);
          if not SameText(anIdent,xDI.Identifier) then
            break
          else
          if (xDI.Kind=sdik) and _xDInA then
          begin
            Result:=xDI;
            break;
          end;
        end;
    end;
  end;

  function _GetDeclaration(pSDI:PPSSyntaxDocItem; ShortDecl:Boolean=False; NoPrefix:Boolean=False):string;
  begin
    if (pSDI=nil) or ((Length(pSDI.ShortDecl)=0) and (Length(pSDI.Declaration)=0)) then
      Result:=''
    else
    begin
      if not NoPrefix then
        case pSDI.Kind of
          sdikConst:Result:=' = ';
          sdikFunction,sdikProcedure,sdikConstructor,sdikFncMethod,sdikProcMethod,sdikProperty:Result:=' ';
        else
          Result:=' : ';
        end
      else
        Result:='';
      if ShortDecl and (Length(pSDI.ShortDecl)>0) then
        Result:=Result+pSDI.ShortDecl
      else
        Result:=Result+pSDI.Declaration
    end;
  end;

begin
  Comp:=TPSSyntaxDocCompiler(aCompiler);
  SyntaxDoc:='';
  sdList:=TStringList.Create;
  SetLength(SDIAp,0);
  try
    sdList.CaseSensitive:=False;
    sdList.Duplicates:=dupAccept;
    sdList.Sorted:=True;
    // keywords
    for i:=0 To Length(_keywords)-1 do
    begin
      SDI:=NewSDI(sdikKeyword,_keywords[i]);
      sdList.AddObject(SDI.Identifier,TObject(SDI));
    end;

    // constants
    for i:=0 To Comp.FConstants.Count-1 do
    begin
      C:=Comp.FConstants.Data[i];
      if Length(C.Name)=0 then
        continue;
      SDI:=NewSDI(sdikConst,_GetIdentName(C.Name, C.OrgName),'',_GetVariantValue(C.Value));
      aname:=_GetIdentName(C.Value.Ftype.Name,C.Value.Ftype.OriginalName);
      if C.Value.FType.BaseType=btenum then
        SDI.Declaration:=aName + '(' + SDI.ShortDecl + ')'
      else
        SDI.Declaration:=SDI.ShortDecl;
      sdList.AddObject(SDI.Identifier,TObject(SDI));
    end;

    // types
    for i:=0 To Comp.FTypes.Count-1 do
    begin
      TT:=Comp.FTypes.Data[i];
      if (TT is TPSClassType) or (TT is TPSAttributeType) or (Length(TT.Name)=0) or (Copy(TT.Name,1,1)='!') then
        continue;

      SDI:=NewSDI(sdikType,_GetIdentName(TT.Name, TT.OriginalName),_GetTypeDecl(TT),_GetTypeDecl(TT,True));

      if TT is TPSTypeLink then
        SDI._AncestorIdent:=TPSTypeLink(TT).LinkTypeNo.Name;

      sdList.AddObject(SDI.Identifier,TObject(SDI));

      if TT is TPSRecordType then
      begin
        for m:=0 To TPSRecordType(TT).RecValCount-1 do
        begin
          TTR:=TPSRecordType(TT).RecVal(m);
          SDICh:=NewSDI(sdikField,_GetIdentName(TTR.FieldName,TTR.FieldOrgName),'','',SDI);
          if TTR.aType<>nil then
          begin
            if Length(TTR.aType.Name)>0 then
              SDICh.Declaration:=_GetIdentName(TTR.aType.Name,TTR.aType.OriginalName)
            else
            begin
              SDICh.Declaration:=_GetTypeDecl(TTR.aType);
              SDICh.ShortDecl:=_GetTypeDecl(TTR.aType,True);
            end;
          end;
          sdList.AddObject(SDICh.Identifier,TObject(SDICh));
        end;
      end;
    end;
    // type ancestors
    for i:=0 To sdList.Count-1 do
      if PPSSyntaxDocItem(sdList.Objects[i]).Kind=sdikType then
      begin
        SDI:=PPSSyntaxDocItem(sdList.Objects[i]);
        if Length(SDI._AncestorIdent)>0 then
          SDI.Ancestor:=FindSDI(sdikType,SDI._AncestorIdent,SDIAp);
      end;

    // variables
    for i:=0 To Comp.FVars.Count-1 do
    begin
      TV:=Comp.FVars.Data[i];
      if Length(TV.Name)=0 then
        continue;
      if TV.SaveAsPointer then
        SDI:=NewSDI(sdikPtrVariable,_GetIdentName(TV.Name, TV.OrgName))
      else
        SDI:=NewSDI(sdikVariable,_GetIdentName(TV.Name, TV.OrgName));
      if TV.aType<>nil then
      begin
        if Length(TV.aType.Name)>0 then
          SDI.Declaration:=_GetIdentName(TV.aType.Name,TV.aType.OriginalName)
        else
        begin
          SDI.Declaration:=_GetTypeDecl(TV.aType);
          SDI.ShortDecl:=_GetTypeDecl(TV.aType,True);
        end;
      end;
      sdList.AddObject(SDI.Identifier,TObject(SDI));
    end;

    // procedures and functions
    for i:=0 To Comp.FRegProcs.Count-1 do
    begin
      TRP:=TPSRegProc(Comp.FRegProcs.Data[i]);
      if (Length(TRP.Name)=0) or (TRP.Name[1]='!') then
        continue;

      TTD:=TRP.Decl;
      SDI:=NewSDI(sdikProcedure,_GetIdentName(TRP.Name,TRP.OrgName));
      if TTD.Result<>nil then
        SDI.Kind:=sdikFunction;

      if TTD.ParamCount>0 then
      begin
        SDI.Declaration:=SDI.Declaration+'(';
        for n:=0 To TTD.ParamCount-1 do
        begin
          if n>0 then
            SDI.Declaration:=SDI.Declaration + '; ';
          TTP:=TTD.Params[n];
          case TTP.Mode of
            pmOut:SDI.Declaration:=SDI.Declaration + 'out ';
            pmInOut:SDI.Declaration:=SDI.Declaration + 'var ';
          end;
          SDI.Declaration:=SDI.Declaration + _GetIdentName(TTP.Name,TTP.OrgName);
          if TTP.aType<>nil then
          begin
            if Length(TTP.aType.Name)>0 then
              SDI.Declaration:=SDI.Declaration + ': '+_GetIdentName(TTP.aType.Name,TTP.aType.OriginalName,True)
            else
              SDI.Declaration:=SDI.Declaration + ': '+_GetTypeDecl(TTP.aType);
          end;
        end;
        SDI.Declaration:=SDI.Declaration+')';
        SDI.ShortDecl:='()';
      end;

      if TTD.Result<>nil then
      begin
        if Length(TTD.Result.Name)>0 then
          aname:=': '+_GetIdentName(TTD.Result.Name,TTD.Result.OriginalName)
        else
          aname:= ': '+_GetTypeDecl(TTD.Result);
        SDI.Declaration:=SDI.Declaration + aname;
        SDI.ShortDecl:=SDI.ShortDecl + aname;
      end;

      sdList.AddObject(SDI.Identifier,TObject(SDI));
    end;

    // procedures and functions (script)
    for i:=0 To Comp.FProcs.Count-1 do
    begin
      TPP:=TPSProcedure(Comp.FProcs.Data[i]);
      if TPP is TPSInternalProcedure then
      begin
        if (Length(TPSInternalProcedure(TPP).Name)=0) or (TPSInternalProcedure(TPP).Name[1]='!') then
          continue;
        aname:=_GetIdentName(TPSInternalProcedure(TPP).Name,TPSInternalProcedure(TPP).OriginalName);
        TTD:=TPSInternalProcedure(TPP).Decl;
      end
      else
      begin
        if (Length(TPSExternalProcedure(TPP).RegProc.Name)=0) or (TPSExternalProcedure(TPP).RegProc.Name[1]='!') then
          continue;
        aname:=_GetIdentName(TPSExternalProcedure(TPP).RegProc.Name,TPSExternalProcedure(TPP).RegProc.OrgName);
        TTD:=TPSExternalProcedure(TPP).RegProc.Decl;
      end;

      SDI:=NewSDI(sdikProcedure,aname);
      if TTD.Result<>nil then
        SDI.Kind:=sdikFunction;

      if TTD.ParamCount>0 then
      begin
        SDI.Declaration:=SDI.Declaration+'(';
        for n:=0 To TTD.ParamCount-1 do
        begin
          if n>0 then
            SDI.Declaration:=SDI.Declaration + '; ';
          TTP:=TTD.Params[n];
          case TTP.Mode of
            pmOut:SDI.Declaration:=SDI.Declaration + 'out ';
            pmInOut:SDI.Declaration:=SDI.Declaration + 'var ';
          end;
          SDI.Declaration:=SDI.Declaration + _GetIdentName(TTP.Name,TTP.OrgName);
          if TTP.aType<>nil then
          begin
            if Length(TTP.aType.Name)>0 then
              SDI.Declaration:=SDI.Declaration + ': '+_GetIdentName(TTP.aType.Name,TTP.aType.OriginalName,True)
            else
              SDI.Declaration:=SDI.Declaration + ': '+_GetTypeDecl(TTP.aType);
          end;
        end;
        SDI.Declaration:=SDI.Declaration+')';
        SDI.ShortDecl:='()';
      end;

      if TTD.Result<>nil then
      begin
        if Length(TTD.Result.Name)>0 then
          aname:=': '+_GetIdentName(TTD.Result.Name,TTD.Result.OriginalName)
        else
          aname:= ': '+_GetTypeDecl(TTD.Result);
        SDI.Declaration:=SDI.Declaration + aname;
        SDI.ShortDecl:=SDI.ShortDecl + aname;
      end;

      sdList.AddObject(SDI.Identifier,TObject(SDI));
    end;

    // classes
    for i:=0 To Comp.FClasses.Count-1 do
    begin
      TC:=Comp.FClasses.Data[i];

      if (TC.aType=nil) or (Length(TC.aType.Name)=0) then
        continue;
      if Length(TC.aType.OriginalName)=0 then
        aname:=_GetIdentName(TC.aType.Name)
      else
        aname:=_GetIdentName(TC.aType.OriginalName);

      {!!! if Length(TC.FClassName)=0 then
        continue;
      if (TC.aType=nil) or (Length(TC.aType.OriginalName)=0) then
        aname:=_GetIdentName(TC.FClassName)
      else
        aname:=_GetIdentName(TC.aType.OriginalName);}

      SDI:=NewSDI(sdikClass,aname,'','');
      if (TC.ClassInheritsFrom<>nil) and (TC.ClassInheritsFrom.aType<>nil) then
      begin
        SDI._AncestorIdent:=TC.ClassInheritsFrom.aType.Name;
        SDI.ShortDecl:=_GetIdentName(TC.ClassInheritsFrom.aType.Name,TC.ClassInheritsFrom.aType.OriginalName);
        SDI.Declaration:='class('+SDI.ShortDecl+')';
      end
      else
        SDI.Declaration:='class';
      sdList.AddObject(SDI.Identifier,TObject(SDI));

      for m:=0 To TC.Count-1 do
      begin
        DI:=TC.Items[m];
        if Length(DI.Name)=0 then
          continue;
        SDICh:=NewSDI(sdikProperty, _GetIdentName(DI.Name,DI.OrgName), '', '', SDI);
        if DI is TPSDelphiClassItemConstructor then
          SDICh.Kind:=sdikConstructor
        else
        if DI is TPSDelphiClassItemMethod then
        begin
          if DI.Decl.Result<>nil then
            SDICh.Kind:=sdikFncMethod
          else
            SDICh.Kind:=sdikProcMethod;
        end;

        if DI.Decl.ParamCount>0 then
        begin
          if DI is TPSDelphiClassItemProperty then
            SDICh.Declaration:=SDICh.Declaration+'['
          else
            SDICh.Declaration:=SDICh.Declaration+'(';
          for n:=0 To DI.Decl.ParamCount-1 do
          begin
            if n>0 then
              SDICh.Declaration:=SDICh.Declaration + '; ';
            TTP:=DI.Decl.Params[n];
            case TTP.Mode of
              pmOut:SDICh.Declaration:=SDICh.Declaration + 'out ';
              pmInOut:SDICh.Declaration:=SDICh.Declaration + 'var ';
            end;
            if (n=0) and (DI is TPSDelphiClassItemProperty) then
              SDICh.Declaration:=SDICh.Declaration + 'Index'
            else
              SDICh.Declaration:=SDICh.Declaration + _GetIdentName(TTP.Name,TTP.OrgName);
            if TTP.aType<>nil then
            begin
              if Length(TTP.aType.Name)>0 then
                SDICh.Declaration:=SDICh.Declaration + ': '+_GetIdentName(TTP.aType.Name,TTP.aType.OriginalName,True)
              else
                SDICh.Declaration:=SDICh.Declaration + ': '+_GetTypeDecl(TTP.aType);
            end;
          end;
          if DI is TPSDelphiClassItemProperty then
          begin
            SDICh.Declaration:=SDICh.Declaration+']';
            SDICh.ShortDecl:='[]';
          end
          else
          begin
            SDICh.Declaration:=SDICh.Declaration+')';
            SDICh.ShortDecl:='()';
          end;
        end;

        if DI.Decl.Result<>nil then
        begin
          if Length(DI.Decl.Result.Name)>0 then
            aname:=': '+_GetIdentName(DI.Decl.Result.Name,DI.Decl.Result.OriginalName)
          else
            aname:= ': '+_GetTypeDecl(DI.Decl.Result);
          SDICh.Declaration:=SDICh.Declaration + aname;
          SDICh.ShortDecl:=SDICh.ShortDecl + aname;
        end;

        sdList.AddObject(SDICh.Identifier,TObject(SDICh));
      end;
    end;
    // class ancestors
    for i:=0 To sdList.Count-1 do
      if PPSSyntaxDocItem(sdList.Objects[i]).Kind=sdikClass then
      begin
        SDI:=PPSSyntaxDocItem(sdList.Objects[i]);
        if Length(SDI._AncestorIdent)>0 then
          SDI.Ancestor:=FindSDI(sdikClass,SDI._AncestorIdent,SDIAp);
      end;

    { TODO: better support for attribute types }
    // attribute types
    for i:=0 To Comp.FAttributeTypes.Count-1 do
    begin
      TA:=Comp.FAttributeTypes.Data[i];
      if Length(TA.Name)=0 then
        continue;

      SDI:=NewSDI(sdikAttribute,_GetIdentName(TA.Name,TA.OrgName),'class(TCustomAttribute)','');
      sdList.AddObject(SDI.Identifier,TObject(SDI));
    end;


    // build the resulting syntax documentation
    case _SyntaxDocMode of

      sdmIdentifiersOnly:
        begin
          for i:=0 To sdList.Count-1 do
          begin
            SDI:=PPSSyntaxDocItem(sdList.Objects[i]);
            if SDI.Parent=nil then
              SyntaxDoc:=SyntaxDoc + SDI.Identifier + #13#10
            else
            if sdoIncludeChildElements in _SyntaxDocOptions then
              SyntaxDoc:=SyntaxDoc + SDI.Parent.Identifier+'.'+SDI.Identifier+#13#10;
            // ancestor elements
            if (SDI.Parent=nil) and (SDI.Ancestor<>nil) and
              ([sdoIncludeAncestorElements,sdoIncludeChildElements] * _SyntaxDocOptions = [sdoIncludeAncestorElements,sdoIncludeChildElements]) then
            begin
              tmpDoc:='';
              SetLength(SDIAp,1); SDIAp[0]:=SDI;
              SDIA:=SDI.Ancestor;
              while SDIA<>nil do
              begin
                tmpLn:='';
                for n:=0 To sdList.Count-1 do
                begin
                  SDICh:=PPSSyntaxDocItem(sdList.Objects[n]);
                  if (SDICh.Parent=SDIA) and (FindSDI(SDICh.Kind,SDICh.Identifier,SDIAp)=nil) then
                    tmpLn:=tmpLn + SDI.Identifier + '.' + SDICh.Identifier + #13#10;
                end;
                if Length(tmpLn)>0 then
                  tmpDoc:=tmpDoc+tmpLn;
                SetLength(SDIAp,Length(SDIAp)+1); SDIAp[Length(SDIAp)-1]:=SDIA;
                SDIA:=SDIA.Ancestor;
              end;
              SyntaxDoc:=SyntaxDoc + tmpDoc;
            end;
          end;
        end;

      sdmSimple,sdmAdvanced:
        begin
          for i:=0 To sdList.Count-1 do
          begin
            SDI:=PPSSyntaxDocItem(sdList.Objects[i]);
            if (SDI.Parent=nil) or (sdoIncludeChildElements in _SyntaxDocOptions) then
            begin
              if _SyntaxDocMode=sdmAdvanced then
                SyntaxDoc:=SyntaxDoc + _SyntaxDocItemKindPrefix[SDI.Kind] + ' ';
              if SDI.Parent=nil then
                SyntaxDoc:=SyntaxDoc + SDI.Identifier + _GetDeclaration(SDI,sdoShortDeclarationsOnly in _SyntaxDocOptions)
              else
                SyntaxDoc:=SyntaxDoc + SDI.Parent.Identifier+'.'+SDI.Identifier + _GetDeclaration(SDI,sdoShortDeclarationsOnly in _SyntaxDocOptions);
              if (_SyntaxDocMode=sdmAdvanced) and (not (SDI.Kind in [sdikKeyword])) then
                SyntaxDoc:=SyntaxDoc + _postfix;
              SyntaxDoc:=SyntaxDoc + #13#10;

              // ancestor elements
              if (SDI.Parent=nil) and (SDI.Ancestor<>nil) and
                ([sdoIncludeAncestorElements,sdoIncludeChildElements] * _SyntaxDocOptions = [sdoIncludeAncestorElements,sdoIncludeChildElements]) then
              begin
                tmpDoc:='';
                SetLength(SDIAp,1); SDIAp[0]:=SDI;
                SDIA:=SDI.Ancestor;
                while SDIA<>nil do
                begin
                  tmpLn:='';
                  for n:=0 To sdList.Count-1 do
                  begin
                    SDICh:=PPSSyntaxDocItem(sdList.Objects[n]);
                    if (SDICh.Parent=SDIA) and (FindSDI(SDICh.Kind,SDICh.Identifier,SDIAp)=nil) then
                    begin
                      if _SyntaxDocMode=sdmAdvanced then
                        tmpLn:=tmpLn + _SyntaxDocItemKindPrefix[SDICh.Kind] + ' ';
                      tmpLn:=tmpLn + SDI.Identifier + '.' + SDICh.Identifier + _GetDeclaration(SDICh,sdoShortDeclarationsOnly in _SyntaxDocOptions);
                      if _SyntaxDocMode=sdmAdvanced then
                        tmpLn:=tmpLn + _postfix;
                      tmpLn:=tmpLn + #13#10;
                    end;
                  end;
                  if Length(tmpLn)>0 then
                    tmpDoc:=tmpDoc+tmpLn;
                  SetLength(SDIAp,Length(SDIAp)+1); SDIAp[Length(SDIAp)-1]:=SDIA;
                  SDIA:=SDIA.Ancestor;
                end;
                SyntaxDoc:=SyntaxDoc + tmpDoc;
              end;
            end;
          end;
        end;

      sdmSynEdit:
        begin
          SyntaxDoc:=SyntaxDoc+'[Keywords]'#13#10;
          for i:=0 To sdList.Count-1 do
            if PPSSyntaxDocItem(sdList.Objects[i]).Kind=sdikKeyword then
              SyntaxDoc:=SyntaxDoc + PPSSyntaxDocItem(sdList.Objects[i]).Identifier + #13#10;

          SyntaxDoc:=SyntaxDoc+#13#10'[InsertList]'#13#10;
          for i:=0 To sdList.Count-1 do
          begin
            SDI:=PPSSyntaxDocItem(sdList.Objects[i]);
            if (SDI.Kind=sdikKeyword) then
              continue;

            if (SDI.Parent=nil) or (sdoIncludeChildElements in _SyntaxDocOptions) then
            begin
              aname:=SDI.Identifier;
              case SDI.Kind of
                sdikConstructor,sdikFncMethod,sdikProcMethod,sdikFunction,sdikProcedure:
                    if Pos('(',SDI.ShortDecl)>0 then aname:=aname+'(|)';
                sdikProperty:
                    if Pos('[',SDI.ShortDecl)>0 then aname:=aname+'[|]';
              end;
              SyntaxDoc:=SyntaxDoc + aname + #13#10;

              // ancestor elements
              if (SDI.Parent=nil) and (SDI.Ancestor<>nil) and
                ([sdoIncludeAncestorElements,sdoIncludeChildElements] * _SyntaxDocOptions = [sdoIncludeAncestorElements,sdoIncludeChildElements]) then
              begin
                tmpDoc:='';
                SetLength(SDIAp,1); SDIAp[0]:=SDI;
                SDIA:=SDI.Ancestor;
                while SDIA<>nil do
                begin
                  tmpLn:='';
                  for n:=0 To sdList.Count-1 do
                  begin
                    SDICh:=PPSSyntaxDocItem(sdList.Objects[n]);
                    if (SDICh.Parent=SDIA) and (FindSDI(SDICh.Kind,SDICh.Identifier,SDIAp)=nil) then
                    begin
                      aname:=SDICh.Identifier;
                      case SDICh.Kind of
                        sdikConstructor,sdikFncMethod,sdikProcMethod:
                            if Pos('(',SDICh.ShortDecl)>0 then aname:=aname+'(|)';
                        sdikProperty:
                            if Pos('[',SDICh.ShortDecl)>0 then aname:=aname+'[|]';
                      end;
                      tmpLn:=tmpLn + aname + #13#10;
                    end;
                  end;
                  if Length(tmpLn)>0 then
                    tmpDoc:=tmpDoc+tmpLn;
                  SetLength(SDIAp,Length(SDIAp)+1); SDIAp[Length(SDIAp)-1]:=SDIA;
                  SDIA:=SDIA.Ancestor;
                end;
                SyntaxDoc:=SyntaxDoc + tmpDoc;
              end;

            end;
          end;

          SyntaxDoc:=SyntaxDoc+#13#10'[ItemList]'#13#10;
          for i:=0 To sdList.Count-1 do
          begin
            SDI:=PPSSyntaxDocItem(sdList.Objects[i]);
            if SDI.Kind=sdikKeyword then
              continue;

            if (SDI.Parent=nil) or (sdoIncludeChildElements in _SyntaxDocOptions) then
            begin
              SyntaxDoc:=SyntaxDoc + Format('\image{%d}\style{+B}',[Integer(SDI.Kind)]);
              if SDI.Parent<>nil then
                SyntaxDoc:=SyntaxDoc + SDI.Parent.Identifier+'.';
              SyntaxDoc:=SyntaxDoc + SDI.Identifier + '\style{-B}' + _GetDeclaration(SDI,sdoShortDeclarationsOnly in _SyntaxDocOptions) + _postfix + #13#10;

              // ancestor elements
              if (SDI.Parent=nil) and (SDI.Ancestor<>nil) and
                ([sdoIncludeAncestorElements,sdoIncludeChildElements] * _SyntaxDocOptions = [sdoIncludeAncestorElements,sdoIncludeChildElements]) then
              begin
                tmpDoc:='';
                SetLength(SDIAp,1); SDIAp[0]:=SDI;
                SDIA:=SDI.Ancestor;
                while SDIA<>nil do
                begin
                  tmpLn:='';
                  for n:=0 To sdList.Count-1 do
                  begin
                    SDICh:=PPSSyntaxDocItem(sdList.Objects[n]);
                    if (SDICh.Parent=SDIA) and (FindSDI(SDICh.Kind,SDICh.Identifier,SDIAp)=nil) then
                    begin
                      tmpLn:=tmpLn + Format('\image{%d}\style{+B}',[Integer(SDICh.Kind)]) +
                          SDI.Identifier + '.' + SDICh.Identifier + '\style{-B}' +
                          _GetDeclaration(SDICh,sdoShortDeclarationsOnly in _SyntaxDocOptions) + _postfix + #13#10;
                    end;
                  end;
                  if Length(tmpLn)>0 then
                    tmpDoc:=tmpDoc+tmpLn;
                  SetLength(SDIAp,Length(SDIAp)+1); SDIAp[Length(SDIAp)-1]:=SDIA;
                  SDIA:=SDIA.Ancestor;
                end;
                SyntaxDoc:=SyntaxDoc + tmpDoc;
              end;

            end;
          end;
        end;

      sdmXml:
        begin
          for xmlIK:=Low(TPSSyntaxDocItemKind) To High(TPSSyntaxDocItemKind) do
            XmlGroups[xmlIK]:='';

          SyntaxDoc:=SyntaxDoc+'<?xml version="1.0" encoding="utf-8" ?>'#13#10+
            '<ps-syntax-doc>'#13#10;

          for i:=0 To sdList.Count-1 do
          begin
            SDI:=PPSSyntaxDocItem(sdList.Objects[i]);

            if SDI.Parent=nil then
            begin
              XmlGroups[SDI.Kind]:=XmlGroups[SDI.Kind]+
                '    <'+_XmlItemKindPrefix[SDI.Kind]+' id="'+SDI.Identifier+'"';

              xmldecl:=_GetDeclaration(SDI,sdoShortDeclarationsOnly in _SyntaxDocOptions, True);
              if SDI.Ancestor<>nil then
                xmlAnc:=SDI.Ancestor.Identifier
              else
                xmlAnc:='';

              if (Length(xmldecl)>0) and (xmlDecl<>xmlAnc) then
              begin
                case SDI.Kind of
                  sdikClass:
                    begin
                      if xmlDecl<>'class('+xmlAnc+')' then
                        XmlGroups[SDI.Kind]:=XmlGroups[SDI.Kind]+' decl="'+xmldecl+'"';
                    end;
                  sdikType:
                    begin
                      if (SDI.Ancestor<>nil) or (Copy(xmlDecl,1,7)<>'record ') then
                        XmlGroups[SDI.Kind]:=XmlGroups[SDI.Kind]+' decl="'+xmldecl+'"'
                      else
                        XmlGroups[SDI.Kind]:=XmlGroups[SDI.Kind]+' decl="record"'
                    end;
                else
                  XmlGroups[SDI.Kind]:=XmlGroups[SDI.Kind]+' decl="'+xmldecl+'"';
                end;
              end;

              if Length(xmlAnc)>0 then
                XmlGroups[SDI.Kind]:=XmlGroups[SDI.Kind]+' ancestor="'+xmlAnc+'"';
              XmlGroups[SDI.Kind]:=XmlGroups[SDI.Kind]+'>';

              xmlBool:=False;
              if (sdoIncludeChildElements in _SyntaxDocOptions) and (not ((SDI.Kind in [sdikType]) and (SDI.Ancestor<>nil) and (xmlDecl=xmlAnc))) then
              begin
                for m:=0 To sdList.Count-1 do
                  if PPSSyntaxDocItem(sdList.Objects[m]).Parent=SDI then
                  begin
                    SDICh:=PPSSyntaxDocItem(sdList.Objects[m]);
                    XmlGroups[SDI.Kind]:=XmlGroups[SDI.Kind]+#13#10+
                      '      <'+_XmlItemKindPrefix[SDICh.Kind]+' id="'+SDICh.Identifier+'"';
                    xmldecl:=_GetDeclaration(SDICh,sdoShortDeclarationsOnly in _SyntaxDocOptions,True);
                    if Length(xmldecl)>0 then
                      XmlGroups[SDI.Kind]:=XmlGroups[SDI.Kind]+' decl="'+xmldecl+'" />'
                    else
                      XmlGroups[SDI.Kind]:=XmlGroups[SDI.Kind]+' />';
                    if not xmlBool then xmlBool:=True;
                  end;
              end;
              if xmlBool then
                XmlGroups[SDI.Kind]:=XmlGroups[SDI.Kind]+#13#10+
                  '    </'+_XmlItemKindPrefix[SDI.Kind]+'>'#13#10
              else
                XmlGroups[SDI.Kind]:=XmlGroups[SDI.Kind]+'</'+_XmlItemKindPrefix[SDI.Kind]+'>'#13#10;
            end;

          end;

          for xmlIK:=Low(TPSSyntaxDocItemKind) To High(TPSSyntaxDocItemKind) do
            if Length(_XmlGroupNames[xmlIK])>0 then
            begin
              if Length(XmlGroups[xmlIK])>0 then
                SyntaxDoc:=SyntaxDoc+
                  '  <'+_XmlGroupNames[xmlIK]+'>'#13#10+
                    XmlGroups[xmlIK]+
                  '  </'+_XmlGroupNames[xmlIK]+'>'#13#10
              else
                SyntaxDoc:=SyntaxDoc+
                  '  <'+_XmlGroupNames[xmlIK]+' />'#13#10;
            end;

          SyntaxDoc:=SyntaxDoc+'</ps-syntax-doc>';
        end;

    end;

    Result := True;

  finally
    for i:=0 To sdList.Count-1 do
    begin
      SDI:=PPSSyntaxDocItem(sdList.Objects[i]);
      finalize(SDI^);
      dispose(SDI);
    end;
    SetLength(SDIAp,0);
    sdList.Free;
  end;
end;

end.