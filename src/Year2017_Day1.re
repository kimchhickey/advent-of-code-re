let input = Node_fs.readFileAsUtf8Sync("input/Year2017_Day1.txt");

let parse = input => {
  input
  ->Js.String2.split("\n")
  ->Belt.Array.getExn(0)
  ->Js.String2.split("")
  ->Belt.Array.map(s => int_of_string(s));
};

let rotate = (arr, n) => {
  let front = Belt.Array.slice(arr, ~offset=0, ~len=n);
  let back = Belt.Array.sliceToEnd(arr, n);
  Belt.Array.concat(back, front);
};

let solve = (origin, rotated) =>
  Belt.Array.zip(origin, rotated)
  ->Belt.Array.keepMap(((a, b)) => a == b ? Some(a) : None)
  ->Belt.Array.reduce(0, (+));

// integer array: [1, 2, 3, ...]
let captcha = input->parse;

// p1
let rotated = captcha->rotate(1);
Js.log(solve(captcha, rotated));

// p2
let half = captcha->Belt.Array.length / 2;
let rotated = captcha->rotate(half);
Js.log(solve(captcha, rotated));
