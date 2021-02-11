open ReludeParse.Parser;

let input = Node_fs.readFileAsUtf8Sync("input/Year2020_Day8.txt");

type instruction =
  | Acc(int)
  | Jmp(int)
  | Nop(int);

let instructionParser = {
  let opParser = (op, builder) => {
    (str(op) <* wsStr, anyInt)
    |> mapTuple2((_, arg) => builder(arg))
    <* eol;
  };

  many(
    opParser("nop", arg => Nop(arg))
    <|> opParser("acc", arg => Acc(arg))
    <|> opParser("jmp", arg => Jmp(arg)),
  )
  <#> (l => l->Belt.List.toArray);
};

let program = input->runParser(instructionParser)->Belt.Result.getExn;

type system = {
  isTerminated: bool,
  accumulator: int,
  instructionPointer: int,
  visited: Belt.Set.Int.t,
};

let initialSystem = {
  isTerminated: false,
  accumulator: 0,
  instructionPointer: 0,
  visited: Belt.Set.Int.empty,
};

let run = (program, system) => {
  let {isTerminated, accumulator, instructionPointer, visited} = system;

  let isTerminated' = ref(isTerminated);
  let accumulator' = ref(accumulator);
  let instructionPointer' = ref(instructionPointer + 1);
  let visited' = visited->Belt.Set.Int.add(instructionPointer);

  switch (program->Belt.Array.get(instructionPointer)) {
  | Some(op) =>
    switch (op) {
    | Acc(arg) => accumulator' := accumulator + arg
    | Jmp(arg) => instructionPointer' := instructionPointer + arg
    | Nop(_) => ()
    }
  | None => isTerminated' := true
  };

  {
    isTerminated: isTerminated'.contents,
    accumulator: accumulator'.contents,
    instructionPointer: instructionPointer'.contents,
    visited: visited',
  };
};

let rec until = (pred, f, x) => {
  let x' = f(x);
  switch (pred(x, x')) {
  | Some(_) => x
  | None => until(pred, f, x')
  };
};

let pred = (x, x') =>
  if (x.isTerminated) {
    Some(0);
  } else {
    x.visited->Belt.Set.Int.get(x'.instructionPointer);
  };

// p1
until(pred, run(program), initialSystem).accumulator->Util.clog;

// p2
let arrayImmutableSet = (arr, i, v) => {
  let arr' = arr->Belt.Array.copy;
  arr'->Belt.Array.setExn(i, v);
  arr';
};

let logWhenTerminated: system => unit =
  result => result.isTerminated ? result.accumulator->Util.clog : ();

// p2
Belt.Array.forEachWithIndex(program, (i, op) => {
  switch (op) {
  | Jmp(arg) =>
    let program' = program->arrayImmutableSet(i, Nop(arg));
    until(pred, run(program'), initialSystem)->logWhenTerminated;
  | Nop(arg) =>
    let program' = program->arrayImmutableSet(i, Jmp(arg));
    until(pred, run(program'), initialSystem)->logWhenTerminated;
  | _ => ()
  }
});
