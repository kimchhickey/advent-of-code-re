module IntNumber = T.IntNumber;
// Day 1
let input1 = ["1721", "979", "366", "299", "675", "1456"];
let target1 = "2020";
let answer1 =
  IntNumber.findFirstPair(
    input1->Belt.List.map(v => v->IntNumber.fromString),
    target1->IntNumber.fromString,
  );
Js.log(answer1); // { TAG: 0, _0: [ 1721, 299 ] }
