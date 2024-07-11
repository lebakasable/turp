uses
  SysUtils;

type
  TLoc = record
    FilePath : AnsiString;
    Row, Col : LongWord;
  end;

function LocToStr(const Loc: TLoc): AnsiString;
begin
  LocToStr := Format('%s:%d:%d', [Loc.FilePath, Loc.Row, Loc.Col]);
end;

type
  PSymbol = ^TSymbol;
  TSymbol = record
    Name : AnsiString;
    Loc : TLoc;
  end;
  TSymbolArray = Array of TSymbol;

  TCharPredicate = function(const C: Char): Boolean;
  
  TLexer = record
    Source : AnsiString;
    FilePath : AnsiString;
    Pos : LongWord;
    Bol : LongWord;
    Row : LongWord;
    Peek : PSymbol;
  end;

  PStmt = ^TStmt;

  TStmtCase = record
    State : TSymbol;
    Read : TSymbol;
    Write : TSymbol;
    Step : TSymbol;
    Next : TSymbol;
  end;

  TStmtVar = record
    Symbol : TSymbol;
    Type_ : TSymbol;
    Body : PStmt;
  end;

  TStmtKind = (StmtCase, StmtVar);

  TStmt = record
    case Kind : TStmtKind of
    StmtCase: (Case_ : ^TStmtCase);
    StmtVar: (Var_ : ^TStmtVar)
  end;
  TStmtArray = Array of TStmt;

  PSet = ^TSet;
  TSet = TSymbolArray;

  TTypeMapEntry = record
    Name : AnsiString;
    Set_ : TSet;
  end;

  TTypeMap = record
    Entries : Array of TTypeMapEntry;
  end;

  TProgram = record
    Stmts : TStmtArray;
    Types : TTypeMap;
  end;

  PTriple = ^TTriple;
  TTriple = record
    Write, Step, Next : TSymbol
  end;

  TMachine = record
    State : TSymbol;
    Tape : TSymbolArray;
    TapeDefault : TSymbol;
    Head : LongWord;
    Halt : Boolean;
  end;

function CreateLexer(const Source, FilePath: AnsiString): TLexer;
begin
  CreateLexer.Source := Source;
  CreateLexer.FilePath := FilePath;
end;

function LexerLoc(const Lexer: TLexer): TLoc;
begin
  with LexerLoc do
  begin
    FilePath := Lexer.FilePath;
    Row := Lexer.Row + 1;
    Col := Lexer.Pos - Lexer.Bol + 1;
  end;
end;

procedure LexerAdvanceLoc(var Lexer: TLexer; const SkippedChar: Char);
begin
  Inc(Lexer.Pos);
  if SkippedChar = LineEnding then
  begin
    Lexer.Bol := Lexer.Pos;
    Inc(Lexer.Row);
  end;
end;

function LexerStripPrefix(var Lexer: TLexer; const Prefix: AnsiString): Boolean;
  function StripPrefix(const Prefix, Str: AnsiString): AnsiString;
  begin
    if Pos(Prefix, Str) = 1 then
      StripPrefix := Copy(Str, Length(Prefix) + 1, Length(Str) - Length(Prefix))
    else
      StripPrefix := Str;
  end;
var
  Source : AnsiString;
  C : Char;
begin
  Source := StripPrefix(Prefix, Lexer.Source);
  if Source <> Lexer.Source then
  begin
    for C in Prefix do
      LexerAdvanceLoc(Lexer, C);
    Lexer.Source := Source;
    LexerStripPrefix := True;
  end
  else
    LexerStripPrefix := False;
end;

function LexerStripWhile(var Lexer: TLexer; const Skip: TCharPredicate): AnsiString;
var
  EndPos : LongWord;
  C : Char;
begin
  EndPos := 1;
  for C in Lexer.Source do
  begin
    if Skip(C) then
    begin
      LexerAdvanceLoc(Lexer, C);
      Inc(EndPos);
    end
    else Break;
  end;

  LexerStripWhile := Copy(Lexer.Source, 1, EndPos - 1);
  Lexer.Source := Copy(Lexer.Source, EndPos, Length(Lexer.Source) - EndPos + 1);
end;

function IsSpace(const C: Char): Boolean;
begin
  IsSpace := C in [' ', #9, #10, #13];
end;

function IsNotSpace(const C: Char): Boolean;
begin
  IsNotSpace := not IsSpace(C);
end;

function LexerChopSymbol(var Lexer: TLexer): PSymbol;
var
  Loc : TLoc;

  Special : Array of AnsiString = ('(', ')', '{', '}', ':');
  Name : AnsiString;
begin
  LexerStripWhile(Lexer, @IsSpace);

  if Length(Lexer.Source) = 0 then
    Exit(nil);

  Loc := LexerLoc(Lexer);

  New(LexerChopSymbol);

  for Name in Special do
    if LexerStripPrefix(Lexer, Name) then
    begin
      LexerChopSymbol^.Name := Name;
      LexerChopSymbol^.Loc := Loc;
      Exit(LexerChopSymbol);
    end;

  LexerChopSymbol^.Name := LexerStripWhile(Lexer, @IsNotSpace);
  LexerChopSymbol^.Loc := Loc;
end;

function LexerNextSymbol(var Lexer: TLexer): PSymbol;
begin
  if Lexer.Peek <> nil then
  begin
    LexerNextSymbol := Lexer.Peek;
    Lexer.Peek := nil;
  end
  else
    LexerNextSymbol := LexerChopSymbol(Lexer);
end;

function LexerPeekSymbol(var Lexer: TLexer): PSymbol;
begin
  if Lexer.Peek = nil then
    Lexer.Peek := LexerChopSymbol(Lexer);
  LexerPeekSymbol := Lexer.Peek;
end;

procedure TypeMapInsert(var TypeMap: TTypeMap; const Name: AnsiString; const Set_: TSet);
var
  Entry : TTypeMapEntry;
begin
  Entry.Name := Name;
  Entry.Set_ := Set_;
  Insert(Entry, TypeMap.Entries, Length(TypeMap.Entries));
end;

function TypeMapGet(const TypeMap: TTypeMap; const Name: AnsiString): PSet;
var
  Entry : TTypeMapEntry;
begin
  TypeMapGet := Nil;
  for Entry in TypeMap.Entries do
    if Entry.Name = Name then
    begin
      New(TypeMapGet);
      TypeMapGet^ := Entry.Set_;
      Exit(TypeMapGet);
    end;
end;

function StmtSubstitute(const Stmt: TStmt; const Symbol, By: TSymbol): TStmt;
begin
  case Stmt.Kind of
    StmtCase:
    begin
      StmtSubstitute.Kind := StmtCase;
      New(StmtSubstitute.Case_);
      if Stmt.Case_^.State.Name = Symbol.Name then StmtSubstitute.Case_^.State := By
      else StmtSubstitute.Case_^.State := Stmt.Case_^.State;
      if Stmt.Case_^.Read.Name = Symbol.Name then StmtSubstitute.Case_^.Read := By
      else StmtSubstitute.Case_^.Read := Stmt.Case_^.Read;
      if Stmt.Case_^.Write.Name = Symbol.Name then StmtSubstitute.Case_^.Write := By
      else StmtSubstitute.Case_^.Write := Stmt.Case_^.Write;
      StmtSubstitute.Case_^.Step := Stmt.Case_^.Step;
      if Stmt.Case_^.Next.Name = Symbol.Name then StmtSubstitute.Case_^.Next := By
      else StmtSubstitute.Case_^.Next := Stmt.Case_^.Next;
    end;
    StmtVar:
    begin
      StmtSubstitute.Kind := StmtVar;
      New(StmtSubstitute.Var_);
      StmtSubstitute.Var_^.Symbol := Stmt.Var_^.Symbol;
      if Stmt.Var_^.Type_.Name = Stmt.Var_^.Symbol.Name then StmtSubstitute.Var_^.Type_ := By
      else StmtSubstitute.Var_^.Type_ := Stmt.Var_^.Type_;
      New(StmtSubstitute.Var_^.Body);
      StmtSubstitute.Var_^.Body^ := StmtSubstitute(StmtSubstitute.Var_^.Body^, Stmt.Var_^.Symbol, Stmt.Var_^.Type_);
    end;
  end;
end;

function StmtEntryState(const Stmt: TStmt; const Prog: TProgram): PSymbol;
var
  Set_ : PSet;
begin
  StmtEntryState := Nil;
  case Stmt.Kind of
    StmtCase:
    begin
      New(StmtEntryState);
      StmtEntryState^ := Stmt.Case_^.State;
    end;
    StmtVar:
    begin
      Set_ := TypeMapGet(Prog.Types, Stmt.Var_^.Type_.Name);
      if Set_ <> Nil then
        if Length(Set_^) > 0 then
          StmtEntryState := StmtEntryState(StmtSubstitute(Stmt.Var_^.Body^, Stmt.Var_^.Symbol, Set_^[0]), Prog)
      else
      begin
        WriteLn(LocToStr(Stmt.Var_^.Type_.Loc), ': ERROR: Unknown type ', Stmt.Var_^.Type_.Name);
        Halt(1);
      end;
    end;
  end;
end;

function StmtMatchState(const Stmt: TStmt; const Prog: TProgram; const State, Read: TSymbol): PTriple;
var
  Set_ : PSet;
  Symbol : TSymbol;
  Triple : PTriple;
begin
  StmtMatchState := Nil;
  case Stmt.Kind of
    StmtCase:
      if (Stmt.Case_^.State.Name = State.Name) and (Stmt.Case_^.Read.Name = Read.Name) then
      begin
        New(StmtMatchState);
        StmtMatchState^.Write := Stmt.Case_^.Write;
        StmtMatchState^.Step := Stmt.Case_^.Step;
        StmtMatchState^.Next := Stmt.Case_^.Next;
      end;
    StmtVar:
    begin
      Set_ := TypeMapGet(Prog.Types, Stmt.Var_^.Type_.Name);
      if Set_ <> Nil then
        for Symbol in Set_^ do
        begin
          Triple := StmtMatchState(StmtSubstitute(Stmt.Var_^.Body^, Stmt.Var_^.Symbol, Symbol), Prog, State, Read);
          if Triple <> Nil then
            Exit(Triple);
        end
      else
      begin
        WriteLn(LocToStr(Stmt.Var_^.Type_.Loc), ': ERROR: Unknown type ', Stmt.Var_^.Type_.Name);
        Halt(1);
      end;
    end;
  end;
end;

function ProgramEntryState(const Prog: TProgram): PSymbol;
var
  Stmt : TStmt;
  State : PSymbol;
begin
  ProgramEntryState := Nil;
  for Stmt in Prog.Stmts do
  begin
    State := StmtEntryState(Stmt, Prog);
    if State <> Nil then Exit(State);
  end;
end;

procedure MachineNext(var Machine: TMachine; const Prog: TProgram);
var
  Stmt : TStmt;
  Triple : PTriple;
begin
  for Stmt in Prog.Stmts do
  begin
    Triple := StmtMatchState(Stmt, Prog, Machine.State, Machine.Tape[Machine.Head]);
    if Triple <> Nil then
    begin
      Machine.Tape[Machine.Head] := Triple^.Write;
      case Triple^.Step.Name of
        '<-':
        begin
          if Machine.Head = 0 then
          begin
            WriteLn(StdErr, LocToStr(Triple^.Step.Loc), ': ERROR: Tape underflow');
            Halt(1);
          end;
          Dec(Machine.Head);
        end;
        '->':
        begin
          Inc(Machine.Head);
          if Machine.Head >= Length(Machine.Tape) then
            Insert(Machine.TapeDefault, Machine.Tape, Length(Machine.Tape));
        end;
      end;
      Machine.State := Triple^.Next;
      Machine.Halt := False;
      Exit;
    end;
  end;
end;

procedure MachinePrint(const Machine: TMachine);
var
  Buffer : AnsiString;
  Head : LongWord;
  I : LongWord;
begin
  Buffer := Machine.State.Name + ': ';
  for I := 0 to High(Machine.Tape) do
  begin
    if I = Machine.Head then
      Head := Length(Buffer);
    Buffer := Buffer + Machine.Tape[I].Name + ' ';
  end;
  WriteLn(Buffer);
  for I := 0 to Head do Write(' ');
  WriteLn('^');
end;

function ParseSymbol(var Lexer: TLexer): TSymbol;
var
  Symbol : PSymbol;
begin
  Symbol := LexerNextSymbol(Lexer);
  if Symbol <> nil then
    ParseSymbol := Symbol^
  else
  begin
    WriteLn(StdErr, LocToStr(LexerLoc(Lexer)), ': ERROR: Expected symbol but reached the end of the input');
    Halt(1);
  end;
end;

function ExpectSymbols(var Lexer: TLexer; const ExpectedNames: Array of AnsiString): TSymbol;
var
  Symbol : TSymbol;
  Buffer : AnsiString;
  I : LongWord;
begin
  Symbol := ParseSymbol(Lexer);
  for I := 0 to High(ExpectedNames) do
    if Symbol.Name = ExpectedNames[I] then
      Exit(Symbol);

  Buffer := '';
  for I := 0 to High(ExpectedNames) do
    if I = 0 then
      Buffer := Buffer + ExpectedNames[I]
    else if I = High(ExpectedNames) then
      Buffer := Buffer + ', or ' + ExpectedNames[I]
    else
      Buffer := Buffer + ', ' + ExpectedNames[I];

  WriteLn(StdErr, LocToStr(Symbol.Loc), ': ERROR: Expected ', Buffer, ' but got ', Symbol.Name);
  Halt(I);
end;

function ParseSet(var Lexer: TLexer): TSet;
var
  Set_ : TSet = ();
  Symbol : PSymbol;
begin
  ExpectSymbols(Lexer, ['{']);
  Symbol := LexerNextSymbol(Lexer);
  while Symbol <> Nil do
  begin
    if Symbol^.Name = '}' then Break;
    Insert(Symbol^, Set_, High(Set_));
    Symbol := LexerNextSymbol(Lexer);
  end;
  ParseSet := Set_;
end;

function ParseStmt(var Lexer: TLexer): TStmt;
var
  Key : TSymbol;
begin
  Key := ExpectSymbols(Lexer, ['case', 'var']);
  case Key.Name of
    'case':
    begin
      ParseStmt.Kind := StmtCase;
      New(ParseStmt.Case_);
      ParseStmt.Case_^.State := ParseSymbol(Lexer);
      ParseStmt.Case_^.Read := ParseSymbol(Lexer);
      ParseStmt.Case_^.Write := ParseSymbol(Lexer);
      ParseStmt.Case_^.Step := ExpectSymbols(Lexer, ['->', '<-']);
      ParseStmt.Case_^.Next := ParseSymbol(Lexer);
    end;
    'var':
    begin
      ParseStmt.Kind := StmtVar;
      New(ParseStmt.Var_);
      ParseStmt.Var_^.Symbol := ParseSymbol(Lexer);
      ExpectSymbols(Lexer, ['is']);
      ParseStmt.Var_^.Type_ := ParseSymbol(Lexer);
      New(ParseStmt.Var_^.Body);
      ParseStmt.Var_^.Body^ := ParseStmt(Lexer);
    end;
  end;
end;

function ParseProgram(var Lexer: TLexer): TProgram;
var
  Prog : TProgram = (Stmts: (); Types: (Entries: ()));
  Key : PSymbol;
  Symbol : TSymbol;
  Set_ : TSet;
begin
  Key := LexerPeekSymbol(Lexer);
  while Key <> Nil do
  begin
    case Key^.Name of
      'case', 'var': Insert(ParseStmt(Lexer), Prog.Stmts, Length(Prog.Stmts));
      'type':
      begin
        LexerNextSymbol(Lexer);
        Symbol := ParseSymbol(Lexer);
        if TypeMapGet(Prog.Types, Symbol.Name) <> nil then
        begin
          WriteLn(StdErr, LocToStr(Symbol.Loc), ': ERROR: Redefinition of type ', Symbol.Name);
          Halt(1);
        end;
        Set_ := ParseSet(Lexer);
        TypeMapInsert(Prog.Types, Symbol.Name, Set_);
      end;
    else
      WriteLn(StdErr, LocToStr(Key^.Loc), ': ERROR: Unknown keyword ', Key^.Name);
      Halt(1);
    end;
    Key := LexerPeekSymbol(Lexer);
  end;
  ParseProgram := Prog;
end;

function ParseTape(var Lexer: TLexer): TSymbolArray;
var
  Tape : TSymbolArray = ();
begin
  while LexerPeekSymbol(Lexer) <> nil do
    Insert(ParseSymbol(Lexer), Tape, Length(Tape));
  ParseTape := Tape;
end;

function SlurpFile(const FilePath: AnsiString): AnsiString;
var
  FileHandle : TextFile;
  Line : String;
begin
  Assign(FileHandle, FilePath);
  Reset(FileHandle);

  SlurpFile := '';
  while not Eof(FileHandle) do
  begin
    ReadLn(FileHandle, Line);
    SlurpFile := SlurpFile + Line + LineEnding;
  end;

  Close(FileHandle);

  if Length(SlurpFile) > 0 then
    SetLength(SlurpFile, Length(SlurpFile) - Length(LineEnding));
end;

procedure Usage(var Out: Text);
begin
  WriteLn(Out, 'Usage: turp <input.turp> <input.tape>');
end;

var
  TurpPath, TapePath : AnsiString;
  TurpSource, TapeSource : AnsiString;
  TurpLexer, TapeLexer : TLexer;

  Prog : TProgram;
  Tape : TSymbolArray;

  EntryState : PSymbol;
  TapeDefault : TSymbol;

  Machine : TMachine;
begin
  if ParamCount = 0 then
  begin
    Usage(StdErr);
    WriteLn(StdErr, 'ERROR: No input.turp is provided');
    Halt(1);
  end;
  TurpPath := ParamStr(1);

  if ParamCount = 1 then
  begin
    Usage(StdErr);
    WriteLn(StdErr, 'ERROR: No input.tape is provided');
    Halt(1);
  end;
  TapePath := ParamStr(2);

  TurpSource := SlurpFile(TurpPath);
  TurpLexer := CreateLexer(TurpSource, TurpPath);
  Prog := ParseProgram(TurpLexer);

  EntryState := ProgramEntryState(Prog);
  if EntryState = Nil then
  begin
    WriteLn(StdErr, 'ERROR: The turp file must at least contain one case');
    Halt(1);
  end;

  TapeSource := SlurpFile(TapePath);
  TapeLexer := CreateLexer(TapeSource, TapePath);
  Tape := ParseTape(TapeLexer);

  if Length(Tape) > 0 then
    TapeDefault := Tape[High(Tape)]
  else
  begin
    WriteLn(StdErr, 'ERROR: The tape file must at least contain one symbol');
    Halt(1);
  end;

  Machine.State := EntryState^;
  Machine.Tape := Tape;
  Machine.TapeDefault := TapeDefault;

  while not Machine.Halt do
  begin
    MachinePrint(Machine);
    Machine.Halt := True;
    MachineNext(Machine, Prog);
  end;
end.
