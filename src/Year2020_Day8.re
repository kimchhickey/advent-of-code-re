open ReludeParse.Parser;

type instruction_t =
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
    opParser("nop", n => Nop(n))
    <|> opParser("acc", n => Acc(n))
    <|> opParser("jmp", n => Jmp(n)),
  )
  <#> (l => l->Belt.List.toArray);
};

type status_t =
  | Processing
  | Terminated
  | InfiniteLoop;

type system_t = {
  status: status_t,
  accumulator: int,
  instructionPointer: int,
  visited: Belt.Set.Int.t,
};

let exec = (program, system) => {
  let {status, accumulator, instructionPointer, visited} = system;

  let status' = ref(status);
  let accumulator' = ref(accumulator);
  let instructionPointer' = ref(instructionPointer + 1);
  let visited' = visited->Belt.Set.Int.add(instructionPointer);

  switch (program->Belt.Array.get(instructionPointer)) {
  | Some(op) =>
    switch (op) {
    | Acc(n) => accumulator' := accumulator + n
    | Jmp(n) => instructionPointer' := instructionPointer + n
    | Nop(_) => ()
    }
  | None => status' := Terminated
  };

  switch (visited->Belt.Set.Int.get(instructionPointer'.contents)) {
  | Some(_) => status' := InfiniteLoop
  | None => ()
  };

  {
    status: status'.contents,
    accumulator: accumulator'.contents,
    instructionPointer: instructionPointer'.contents,
    visited: visited',
  };
};

let rec doUntil = (pred, f, x) => pred(x) ? x : doUntil(pred, f, f(x));

module type P = {
  let pred: system_t => bool;
  let print: system_t => unit;
};

// main
let () = {
  let input = Node_fs.readFileAsUtf8Sync("input/Year2020_Day8.txt");
  let program = runParser(input, instructionParser)->Belt.Result.getExn;

  let initialSystem = {
    status: Processing,
    accumulator: 0,
    instructionPointer: 0,
    visited: Belt.Set.Int.empty,
  };

  // part 1
  module P1: P = {
    let pred = s => s.status == InfiniteLoop;
    let print = s => s.accumulator->Js.log;
  };
  doUntil(P1.pred, exec(program), initialSystem)->P1.print;

  // part 2
  module P2: P = {
    let pred = s => s.status == Terminated || s.status == InfiniteLoop;
    let print = s => s.status == Terminated ? s.accumulator->Util.clog : ();
  };

  let swapAt = (arr, i, op) => {
    let arr' = arr->Belt.Array.copy;
    switch (op) {
    | Jmp(n) => arr'->Belt.Array.setExn(i, Nop(n))
    | Nop(n) => arr'->Belt.Array.setExn(i, Jmp(n))
    | _ => ()
    };
    arr';
  };

  Belt.Array.forEachWithIndex(
    program,
    (i, op) => {
      let program' = program->swapAt(i, op);
      doUntil(P2.pred, exec(program'), initialSystem)->P2.print;
    },
  );
};
