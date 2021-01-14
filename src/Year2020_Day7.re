module P = ReludeParse.Parser;

let input = Node_fs.readFileAsUtf8Sync("input/Year2020_Day7.sample.txt");

let s = "light red bags contain 1 bright white bag, 2 muted yellow bags.";

// 1. contains를 기준으로 두개의 튜플로 나눠보자
Js.log(P.runParser(s, P.many(P.anyAlpha)));
