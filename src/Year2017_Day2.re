let input = Node_fs.readFileAsUtf8Sync("input/Year2017_Day2.txt");

let parse = input => {
  input
  ->Js.String2.trim
  ->Js.String2.split("\n")
  ->Belt.Array.map(line => line->Js.String2.split("\t"))
  ->Belt.Array.map(line => line->Belt.Array.map(v => int_of_string(v)));
};

let data = input->parse;

// p1
let maxArr = data->Belt.Array.map(arr => Js.Math.maxMany_int(arr));
let minArr = data->Belt.Array.map(arr => Js.Math.minMany_int(arr));

Js.log(
  Belt.Array.zip(maxArr, minArr)
  ->Belt.Array.map(((max, min)) => max - min)
  ->Belt.Array.reduce(0, (+)),
);

// p2
let makeCombinations = arr =>
  arr
  ->Belt.Array.map(v1 => arr->Belt.Array.map(v2 => (v1, v2)))
  ->Belt.Array.concatMany;

let isNotSame = (a, b) => a !== b;
let isEvenlyDivided = (a, b) => a mod b === 0;

let findQuotient = arr =>
  arr->Belt.Array.keepMap(((a, b)) =>
    isNotSame(a, b) && isEvenlyDivided(a, b) ? Some(a / b) : None
  );

Js.log(
  data
  ->Belt.Array.map(arr => arr->makeCombinations)
  ->Belt.Array.map(arr => arr->findQuotient)
  ->Belt.Array.concatMany
  ->Belt.Array.reduce(0, (+)),
);
