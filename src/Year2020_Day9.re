module FloatNumber = T.FloatNumber;

// Day 9
let input2 = [
  "16550340689748",
  "14642152670036",
  "14900640875552",
  "19928504528616",
  "16734505234514",
];
let target2 = "31192493359784";
let answer2 =
  FloatNumber.findFirstPair(
    input2->Belt.List.map(v => v->FloatNumber.fromString),
    target2->FloatNumber.fromString,
  );
Js.log(answer2); // { TAG: 0, _0: [ 16550340689748, 14642152670036 ] }
