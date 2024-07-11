program Turp;

uses
  Lexer,
  Expr;

type
  PStmt = ^TStmt;

  TStmtCase = record
    State : TExpr;
    Read : TExpr;
    Write : TExpr;
    Step : TExpr;
    Next : TExpr;
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
    Write, Step, Next : TExpr;
  end;

  TMachine = record
    State : TExpr;
    Tape : TExprArray;
    TapeDefault : TExpr;
    Head : LongWord;
    Halt : Boolean;
  end;

function StmtToStr(const Stmt: TStmt): AnsiString;
begin
  case Stmt.Kind of
    StmtCase:
    with Stmt.Case_^ do
    begin
      StmtToStr := 'case ' + ExprToStr(State) + ' ' + ExprToStr(Read) + ' ' + ExprToStr(Write) + ' ' + ExprToStr(Step) + ' ' + ExprToStr(Next);
    end;
    StmtVar:
    with Stmt.Var_^ do
    begin
      StmtToStr := 'var ' + Symbol.Name + ' : ' + Type_.Name + ' ' + StmtToStr(Body^);
    end;
  end;
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
    with Stmt.Case_^ do
    begin
      StmtSubstitute.Kind := StmtCase;
      New(StmtSubstitute.Case_);
      StmtSubstitute.Case_^.State := ExprSubstitute(State, Symbol, By);
      StmtSubstitute.Case_^.Read := ExprSubstitute(Read, Symbol, By);
      StmtSubstitute.Case_^.Write := ExprSubstitute(Write, Symbol, By);
      StmtSubstitute.Case_^.Step := ExprSubstitute(Step, Symbol, By);
      StmtSubstitute.Case_^.Next := ExprSubstitute(Next, Symbol, By);
    end;
    StmtVar:
    begin
      StmtSubstitute.Kind := StmtVar;
      New(StmtSubstitute.Var_);
      StmtSubstitute.Var_^.Symbol := Stmt.Var_^.Symbol;
      StmtSubstitute.Var_^.Type_ := Stmt.Var_^.Type_;
      New(StmtSubstitute.Var_^.Body);
      StmtSubstitute.Var_^.Body^ := StmtSubstitute(Stmt.Var_^.Body^, Symbol, By);
    end;
  end;
end;

function StmtEntryState(const Stmt: TStmt; const Prog: TProgram): PExpr;
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

function StmtMatchState(const Stmt: TStmt; const Prog: TProgram; const State, Read: TExpr): PTriple;
var
  Set_ : PSet;
  Symbol : TSymbol;
  Triple : PTriple;
begin
  StmtMatchState := Nil;
  case Stmt.Kind of
    StmtCase:
      if ExprMatches(Stmt.Case_^.State, State) and ExprMatches(Stmt.Case_^.Read, Read) then
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

function ProgramEntryState(const Prog: TProgram): PExpr;
var
  Stmt : TStmt;
  State : PExpr;
begin
  ProgramEntryState := Nil;
  for Stmt in Prog.Stmts do
  begin
    State := StmtEntryState(Stmt, Prog);
    if State <> Nil then Exit(State);
  end;
end;

procedure MachinePrint(const Machine: TMachine);
var
  Buffer : AnsiString;
  Head : LongWord;
  I : LongWord;
begin
  Buffer := ExprToStr(Machine.State) + ': ';
  for I := 0 to High(Machine.Tape) do
  begin
    if I = Machine.Head then
      Head := Length(Buffer);
    Buffer := Buffer + ExprToStr(Machine.Tape[I]) + ' ';
  end;
  WriteLn(Buffer);
  for I := 0 to Head do Write(' ');
  WriteLn('^');
end;

procedure MachineNext(var Machine: TMachine; const Prog: TProgram);
var
  Stmt : TStmt;
  Triple : PTriple;
  Name : PSymbol;
begin
  for Stmt in Prog.Stmts do
  begin
    Triple := StmtMatchState(Stmt, Prog, Machine.State, Machine.Tape[Machine.Head]);
    if Triple <> Nil then
    begin
      Machine.Tape[Machine.Head] := Triple^.Write;
      Name := ExprAtomName(Triple^.Step);
      if Name <> Nil then
        case Name^.Name of
          '<-':
          begin
            if Machine.Head = 0 then
            begin
              WriteLn(StdErr, LocToStr(ExprLoc(Triple^.Step)), ': ERROR: Tape underflow');
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
          '.': begin end;
          '!': MachinePrint(Machine);
        else
          WriteLn(StdErr, LocToStr(ExprLoc(Triple^.Step)), ': ERROR: Step must be ->, <-, ! or .');
          Halt(1);
        end
      else
      begin
        WriteLn(StdErr, LocToStr(ExprLoc(Triple^.Step)), ': ERROR: Step must be an atom');
        Halt(1);
      end;
      Machine.State := Triple^.Next;
      Machine.Halt := False;
      Exit;
    end;
  end;
end;

function ParseSet(var Lexer: TLexer): TSet;
var
  Set_ : TSet = ();
  Symbol : PSymbol;
begin
  LexerExpectSymbols(Lexer, ['{']);
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
  Key := LexerExpectSymbols(Lexer, ['case', 'var']);
  case Key.Name of
    'case':
    begin
      ParseStmt.Kind := StmtCase;
      New(ParseStmt.Case_);
      ParseStmt.Case_^.State := ParseExpr(Lexer);
      ParseStmt.Case_^.Read := ParseExpr(Lexer);
      ParseStmt.Case_^.Write := ParseExpr(Lexer);
      ParseStmt.Case_^.Step := ParseExpr(Lexer);
      ParseStmt.Case_^.Next := ParseExpr(Lexer);
    end;
    'var':
    begin
      ParseStmt.Kind := StmtVar;
      New(ParseStmt.Var_);
      ParseStmt.Var_^.Symbol := LexerParseSymbol(Lexer);
      LexerExpectSymbols(Lexer, [':']);
      ParseStmt.Var_^.Type_ := LexerParseSymbol(Lexer);
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
        Symbol := LexerParseSymbol(Lexer);
        if TypeMapGet(Prog.Types, Symbol.Name) <> Nil then
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

function ParseTape(var Lexer: TLexer): TExprArray;
var
  Tape : TExprArray = ();
begin
  while LexerPeekSymbol(Lexer) <> Nil do
    Insert(ParseExpr(Lexer), Tape, Length(Tape));
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
  Tape : TExprArray;

  EntryState : PExpr;
  TapeDefault : TExpr;

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
